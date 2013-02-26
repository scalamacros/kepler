/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import scala.tools.nsc.Global
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.Settings
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.typechecker.Analyzer
import scala.collection.mutable.Stack

import scala.reflect.macros.util.Traces

abstract class JitCompiler extends Traces {
  import scala.collection.mutable.Set

  val analyzer:Analyzer

  def globalSettings: scala.tools.nsc.Settings = analyzer.globalSettings
  val g:analyzer.global.type = analyzer.global

  lazy val virtualDirectory = new VirtualDirectory("(memory)", None)  

  def units = g.currentRun.units

  private def ifDefinedInTree(sym:g.Symbol) = (! sym.isErroneous ) && (
    // TODO: sym.associatedFile can be null ORLY ????
    ( sym.associatedFile != null ) &&
      units.exists(unit => unit.source.file.canonicalPath == sym.associatedFile.canonicalPath)
    )

  def checkAnnotation(className:String,methName:String,context:analyzer.Context):Option[g.Symbol] = {

    val parts = className.split("[\\$\\.]") :+ methName   // className is something like "package.a1.a2.inno.A1$A12$"
    // Select here should work - we don't need types of calling params
    // becuase function can not be overloaded
    val methSelector = parts.tail.foldLeft[g.Tree](g.Ident(g.newTermName(parts.head)))((a,b)=>g.Select(a,g.newTermName(b)))
    val typer = analyzer.newTyper(context)

    val silentTyper = analyzer.newTyper(typer.context.makeSilent(reportAmbiguousErrors=false))

    val methodSym:g.Symbol = try {
      silentTyper.typed1(methSelector , EXPRmode, g.WildcardType).symbol
    }  catch {
      case ex: g.TypeError => macroLogExpand("TypeError in methSelector typing gona die error=" + ex)
        throw(ex)
      case ex: Exception   => macroLogExpand("Unknown !!! error in methSelector typing gona die=" + ex )
        throw(ex)
    }
    //inMacros = false
    if (methodSym==g.NoSymbol || methodSym.isErroneous || !ifDefinedInTree(methodSym) ) {
      None
    } else {
      Some(methodSym)
    }

  }


  private def createClosure(toCheck:List[g.Symbol],checkedSyms:jitInternalGlobal.SymbolSet,forbiddenSet:Set[g.Symbol]=Set()):jitInternalGlobal.SymbolSet = {
    val importer = jitInternalGlobal.mkImporter(g)
    val run = new jitInternalGlobal.Run

    jitInternalGlobal.globalPhase=run.typerPhase

    // init exporter before other imports 
    // order matters
    jitInternalGlobal.units
    
    def toModuleClass(s:jitInternalGlobal.Symbol) = if (s.isModuleClass) s.sourceModule else s
    // push whole symbol chain for each toCheck  - symbol
    // A1.A2.impl -> A1.sourceModule , A2.sourceModule , impl
    def jitToCheck  = toCheck.flatMap(s => { 
        val startSym = importer.importSymbol(s) 
        startSym.ownerChain.takeWhile(! _.isPackageClass ).map { toModuleClass(_) }
      }
    )
    
    def jitForbidenSet = forbiddenSet.map(s => { importer.importSymbol(s) })

    jitInternalGlobal.getLinkedIds(jitToCheck,Set(),jitForbidenSet)
  }

  lazy val jitClassLoader = {
    val classpath = g.classPath.asURLs
    val loader = ScalaClassLoader.fromURLs(classpath, this.getClass.getClassLoader)
    import scala.tools.nsc.interpreter._
    
    new AbstractFileClassLoader(virtualDirectory, loader) {
      override def loadClass(name:String,resolve:Boolean) = {
        findAbstractFile(classNameToPath(name)) match {
          case null => super.loadClass(name,resolve)
          case file => 
            val bytes = file.toByteArray
            defineClass(name, bytes, 0, bytes.length)
        }
      }
    }
  }

  lazy val jitInternalGlobal = new {
    val mainGlobal:Global = g
  } with JitInternalGlobal(g.settings,g.reporter)

  /** Performs macro compilation for className.methName :
    * if its implementation can be found in localAst
    * main
    * @param className
    * @param methName
    * @param context
    * @param macroDef : Symbol - macro caller . def macroDef = macro macro_impl
    *
    */
  def tryLocalExpansion(className:String,methName:String,context:analyzer.Context,macroDef:g.Symbol):JitResult = {
    //debugStartAst("!!!tryLocalExpansion StartAst ")

    checkAnnotation(className,methName,context) match {
      case Some(methodSym : g.Symbol) =>
        macroLogVerbose("!!!tryLocalExpansion methSym="+methodSym)
        val currentSettings = g.settings.copy()
        currentSettings.outputDirs setSingleOutput virtualDirectory

        val jg = new JitGlobal(currentSettings,g.reporter)
        val closure = createClosure(List(methodSym),Set(),Set(macroDef))
        jitInternalGlobal.compileClosure(closure,jg)
      case None =>
        macroLogExpand("!!!tryLocalExpansion. Expandy function not found in local")
        JitSkip()
    }
  }
}

import scala.reflect.internal.SymbolTable

abstract class JitInternalGlobal(currentSettings: Settings, reporter: Reporter) extends Global(currentSettings, reporter) with Traces {
      val mainGlobal:Global

      def globalSettings: scala.tools.nsc.Settings = currentSettings
      
      import scala.collection.mutable.Set

      val run = new Run
      globalPhase=run.namerPhase

      override def forInteractive = true

      lazy val importer = new JitImporter { val from:mainGlobal.type = mainGlobal }

      abstract class JitImporter extends StandardImporter {
        val global:Global = from.asInstanceOf[Global]
        protected lazy val completerMap = new Cache[Symbol, Tree]()
        override val rewriteWithLocals=true
        val debug = false

        @inline final def importerDebug(msg: => Any) { if (debug) println(msg) }

        override def doTypeImport(tpe: from.Type) = if (tpe.isInstanceOf[global.analyzer.ImportType]) {
            val t = tpe.asInstanceOf[global.analyzer.ImportType]
            analyzer.ImportType(importTree(t.expr.asInstanceOf[from.Tree]))
          } else {
            super.doTypeImport(tpe)
          }
  
        override def doImportTreeParts(tree:from.Tree,mytree:Tree):Unit = {
          super.doImportTreeParts(tree,mytree)
          attachCompleter(mytree,mytree.symbol,tree.symbol)
        }
        
        def attachCompleter(tree:Tree,sym:Symbol,fromSym:from.Symbol) = {
          importerDebug("!!!tryattachCompleter " +tree.summaryString +" " +sym + "#" + (if (sym==null ) "" else sym.id),fromSym + "#" + (if (fromSym==null ) "" else fromSym.id),fromSym != null && fromSym.isInitialized)
          if ( sym != null && sym != NoSymbol && ! fromSym.isInitialized)
            tree match {
              case _:MemberDef => completerMap weakGet sym match {
                case Some(result) => assert(false,"cant attach completer again " + showRaw(tree) + " " + sym + "#" + sym.id)
                case _ =>
                    importerDebug("!!!attachCompleter",sym + "#" + (if (sym==null ) "" else sym.id),fromSym + "#" + (if (fromSym==null ) "" else fromSym.id),fromSym != null && fromSym.isInitialized)
                    completerMap.weakUpdate(sym,tree)
              }
              // object O1 extends O2.C1 with O2.T1 with O3.T2[O4.C4] {
              // Select@7653(Ident@7650(O4), O4.C4)
              case _:Select => 
              // Annotation Tree Part jumps also here - do nothing
              // TODO: deny annotation parts make completers
              case _:Ident  => 
              case _:Apply  => 

              case _ => assert(false,"attachCompleter can't recognize " + showRaw(tree))
            }
        }
        override def mkSymCompleter(sym:from.Symbol) = {
          val mytypeParams = sym.typeParams map importSymbol
          new LazyPolyType(mytypeParams) with FlagAgnosticCompleter {
            override def complete(s: Symbol) {
              val result = sym.info match {
                case from.PolyType(_, res) => res
                case result => result
              }
              
              importerDebug("!!!mkSymCompleter.complete " + s.id + " " + s + " " + completerMap.weakGet(s))

              s setInfo GenPolyType(mytypeParams, importType(result))
              s setAnnotations (sym.annotations map importAnnotationInfo)

              def processTParams(tparams: List[TypeDef],typeParams:List[Symbol]) =
                ( tparams zip typeParams ) foreach { el => el._1.setSymbol(el._2) }

              def processVParams(vparamss: List[List[ValDef]],paramss:List[List[Symbol]]) =
                vparamss.flatten zip paramss.flatten foreach { case (valdef,sym) => valdef.setSymbol(sym) }

              def processTemplateBody(body:List[Tree]) = body.foreach { el =>
                el match {
                  case EmptyTree => // Do nothing Generated by 'object A2_inobj {}'
                  case mdef:MemberDef =>
                    val member = s.info.decl(mdef.name)
                    val from_member = sym.info.decl(exportName(mdef.name))
                    assert(member != NoSymbol && member != null,s"member $s.$mdef not found in imported tree")
                    assert(from_member != NoSymbol && from_member != null,s"member $s.$mdef not found in imported tree")
                    mdef.setSymbol(member)                    
                    // tparams processing 
                    mdef match {
                      case TypeDef(_,_,tparams,_)    =>
                        processTParams(tparams,member.typeParams)
                      case ClassDef(_,_,tparams,_)   =>
                        processTParams(tparams,member.typeParams)
                      case DefDef(_,_,tparams,vparamss,_,_) =>
                        processTParams(tparams,member.typeParams)
                        processVParams(vparamss,member.paramss)
                      case _:ModuleDef | _:PackageDef | _:ValDef =>
                      case _ => assert(false,"unsupported MdefChild " + mdef.getClass)
                    }

                    // It' also possible that from_member symbol already initialized by somebody 
                    // without template initialisation this means that we should post parse tree here
                    // and dont wait for completer
                    if (from_member.isInitialized) {
                      mdef match {
                        case ddef : DefDef => processRetType(ddef,member)
                        case _ => 
                      }
                    } else {
                      attachCompleter(el,member,from_member)
                    }
                    
                    
                  case _ => 
                    assert(false , "not implemented completer el = "  + showRaw(el))
                    
                }
                
              }

              def processRetType(ddef:DefDef,s:Symbol) = {
                s.info match {
                  case NullaryMethodType(resultType) =>
                    if (ddef.tpt.isEmpty) ddef.tpt.defineType(resultType)
                  case mt @ MethodType(params,resultType) =>
                    if (ddef.tpt.isEmpty) ddef.tpt.defineType(resultType)
                    processVParams(ddef.vparamss,mt.paramss)
                    
                  case PolyType(params,resultType) =>
                    if (ddef.tpt.isEmpty) ddef.tpt.defineType(resultType)

                  case _ => assert(false,"defdef with unknown type " + s.info.getClass)
                }

                
              }

              completerMap weakGet s match {
                case Some(result) =>
                  result match {
                    case ValDef(mods, name, tpt, rhs) => 
                    case ddef @ DefDef(_,name,tparams,vparams,tpt,_) => 
                      processRetType(ddef,s)
                    case ClassDef(_, _, _, Template(parents, self_val, body) ) => 
                      processTemplateBody(body)
                    case ModuleDef(_, _,  Template(parents, self_val, body) ) =>
                      processTemplateBody(body)
                    case _:TypeDef => 
                    case _ => 
                      assert(false,"not implemented "+ showRaw(result,printIds=true,printTypes=true) + " " + result.getClass)
                  }
                case _ => 
              }

            }
          }
        }
      }

      lazy val units:List[CompilationUnit] = mainGlobal.currentRun.units.map( unit => {
        val cu:CompilationUnit = new CompilationUnit(unit.source) 
        cu.body = importer.importTree(
          unit.preTypedBody
          // if ( preTypedBody.preTypedBody == g.EmptyTree) unit.body else preTypedBody.preTypedBody
        )
        cu
      }).toList

      type WalkPath = List[Int]
      case class SymbolPath(symbol:Symbol,unit:CompilationUnit,ast:Tree,path:WalkPath) {
        def shorten = SymbolPath(symbol,unit,ast,path.tail)
        override def toString:String = "@" + symbol.toString + "#" + symbol.id + " p= " + path
      }

      type SymbolSet = Set[SymbolPath]

    private abstract class PathWalker(wp:WalkPath,debug:Boolean=false) extends Traverser {
        var result: Option[Tree] = None

        private val path = new Stack[Int] ++ wp
        private var cIdx = path.pop

        protected def isLastStep = path.isEmpty && cIdx ==0
        def markNode(tree:Tree)
        def markUpperNode(tree: Tree)

        override def traverse(tree: Tree) =
          if (result.isEmpty) {
            if (debug)
              macroLogExpand("PathWalker.traverse cIdx="  +cIdx + " path" + path + " " + tree.summaryString +"@"+tree.getClass  )
            if ( cIdx == 0) {
              if ( path.isEmpty) {
                markNode(tree)
                result = Some(tree)
              } else {
                cIdx = path.pop
                markNode(tree)
                super.traverse(tree)
              }
            } else {
              cIdx -= 1
              if (debug)
                macroLogExpand("PathWalker.markUpperNode cIdx="  +cIdx + " path" + path + " " + tree.summaryString +"@"+tree.getClass  )
              markUpperNode(tree)
            }
          }
      }

      private class ContextCreator(unit:CompilationUnit,wp:WalkPath,debug:Boolean=false) extends PathWalker(wp,debug) {
        var context = analyzer.rootContext(unit, EmptyTree, false)
        def markNode(tree:Tree):Unit  = {

          assert( tree.symbol == NoSymbol || tree.symbol.isStatic , "ContextCreator Jumps in non static node: " + tree.symbol)
          
          tree match {
            case tree: Block                          =>
              assert(false,"Illegal")
            case tree @ ModuleDef(_, _, impl) =>
              val sym = tree.symbol
              val clazz = sym.moduleClass

              // Self part
              context = context.makeNewScope(tree, clazz)
              analyzer.newNamer(context).enterSelf(impl.self)

              // Body part
              //context = context.make(impl,clazz,clazz.info.decls)
              context = context.makeNewScope(impl,clazz)

            case tree: PackageDef                     =>
              context = context.make(tree,tree.symbol.moduleClass,tree.symbol.info.decls)

            case tree: Import                       =>
              assert(false,"Illegal")

            case tree: Template                     =>
              // Parsed already as ModuleDef children
            case _:TypeDef                          => 
              // Do Nothing
            case _:DefDef | _:ClassDef |  _:ValDef => // DefDef can be only last element in context maker - we can't and don't want to dive in function scope
              assert(isLastStep,"Illegal MemberDef in context creator: isLastStep=false")

            case _ => assert(false,"ContextCreator_markNode:ERROR??? tree=" + showRaw(tree))
          }
      }

      def markUpperNode(tree:Tree)  = tree match {
        case tree: Import =>
          context = context.makeNewImport(tree)
        case _ =>
      }
    }



    class ExternalsMarker(inputs:SymbolSet,startSym:Symbol,skippedSet:Set[Symbol]) extends Traverser {
      val externalSyms = Set[Symbol]()

      // Symbols.scala hasTransOwner extension
      def hasTransOwnerForModule(sym0:Symbol,sym: Symbol): Boolean = {
        var o = sym0
        while ((o ne sym) && ( o.companionSymbol ne sym ) && (o ne NoSymbol)) o = o.owner
        (o eq sym) || (o.companionSymbol eq sym)
      }
      /* If we export ModuleDef, owner of its DefDefs is not module symbol
       we should check companionSymbol in owners list.
       if startSym is object - don't allow all internal members to be exported
       otherwise dont check object scope
       1 ) if in inputs is object AAA { def A1 = 1 } and our startSym is some
        def BBB = { AAA.A1 } we should produce AAA.A1 as external symbol - it can be used later as
        info about needed symbols for packing object AAA
        we should pack later object AAA with all it's members despite of members that refer to the macros
        and if some member call the macros and exists as "external" - this means 100% recursive macro call
        and we should produce error
       2 ) if we our startSym is object AAA we don't need info about it's content thats why we produce only
       object itself as external symbol
      */
      
      def isExternal:Symbol => Boolean = {
        def inputsCheck(s:Symbol) = inputs.exists(v => s.hasTransOwner(v.symbol) ) || skippedSet.contains(s)
        def isExternal0(s:Symbol) = s!=null && s!=NoSymbol && ! (
            inputsCheck(s) ||
            externalSyms.exists(s.hasTransOwner(_)) ||
            s.hasTransOwner(startSym)
          )
        def isModuleExternal(s:Symbol) = s!=null && s!=NoSymbol && ! (
            inputsCheck(s) ||
            externalSyms.exists(s.hasTransOwner(_)) ||
            hasTransOwnerForModule(s,startSym)
          )
        startSym match {
          case _:ModuleClassSymbol => isModuleExternal
          case _ => isExternal0
        }
      }



      override def traverse(tree: Tree): Unit  = {
        def debugEnter(s:Symbol,t:Tree) = {
          macroLogExpand("!!!ExternalsMarker.EnterSym " +
            "sym="+
            s+" "+
            s.id+" "+(if (s.isStatic ) "[STATIC]" else "")+
            s.getClass+
            " owner="+
            s.owner+" "+
            s.owner.id+" "+
            s.owner.getClass +
            " tree="+showRaw(tree,printIds=true) +
            " isExternal="  + isExternal(s) +
            " ifDefinedInTree="  + ifDefinedInTree(s)
          )
        }
        def flyToStatic(s:Symbol) = if (s.isStatic) s else s.owner
        def toModuleClass(s:Symbol) = if (s.isModuleClass) s.sourceModule else s
        def tryToEnter(s:Symbol) = tryToEnter0(flyToStatic(toModuleClass(s)))
        def tryToEnter0(s:Symbol) = if ( isExternal(s) && ifDefinedInTree(s) ) {
          debugEnter(s,tree)
          externalSyms+=s
        }
        
        tree match {
          case _:Template | _:Apply | _:AppliedTypeTree => super.traverse(tree)
          case tpt:TypeTree => 
            if (tpt.original != null)
              traverse(tpt.original)
          case _ => tree.symbol match {
            case null | NoSymbol => super.traverse(tree)
            case s:Symbol => 
              tryToEnter(s)
              super.traverse(tree)
          }
        }
      }
    }

    private class PathFinder(sym:Symbol) extends Traverser {
      // Result is Tree , Path to this tree , isPrimary constructor (for defdef only 
      //  - optimisation purposes - we dont need to traverse tree again) 
      var result: Option[(Tree,WalkPath,Boolean)] = None
      private val path = new Stack[Int]
      private var cIdx = 0

      private var isPrimaryConstructor = false 

      def isGetterOf(s1:Symbol,s2:Symbol) = s2 != null && s2 != NoSymbol && s1 != null && s1 != NoSymbol && s2.getter(s2.enclClass) == s1

      override def traverse(tree: Tree) = tree match {
        case _ if (! result.isEmpty) =>
        case ddef:DefDef if tree.symbol == sym => 
            result = Some((tree,(cIdx :: path.toList).reverse,ddef.name == nme.CONSTRUCTOR && isPrimaryConstructor))        
        case _:ValDef if tree.symbol == sym || isGetterOf(sym,tree.symbol) => result = Some((tree,(cIdx :: path.toList).reverse,false))
        case _:ClassDef|_:TypeDef|_:ModuleDef if tree.symbol == sym => result = Some((tree,(cIdx :: path.toList).reverse,false))
        case _ =>
          tree match {
            case _:Template       => isPrimaryConstructor = true
            case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => isPrimaryConstructor=false
            case _ => 
          }
          path.push(cIdx)
          cIdx=0
          super.traverse(tree)
          if (result.isEmpty) cIdx=path.pop+1
      }
    }

    def getAst(symbol:Symbol):Option[SymbolPath] = {
      def isGetterOf(s1:Symbol,s2:Symbol) = s2 != null && s2 != NoSymbol && s1 != null && s1 != NoSymbol && s2.getter(s2.enclClass) == s1
      
      def traverseUnits(units:List[CompilationUnit]):Option[SymbolPath] = units match  {
        case u::last => traverseUnit(u) match {
          case None => traverseUnits(last)
          case ret @ Some(_)    => ret
        }
        case Nil => None
      }
      def mkPathTyper(u:CompilationUnit,path:WalkPath):analyzer.Typer  = {
        val cc = new ContextCreator(u,path,debug=false)
        cc.traverse(u.body)
        assert(cc.result.isDefined,"ContextCreator can not create context")
        analyzer.newTyper(cc.context.makeSilent(reportAmbiguousErrors=false))
      }
      def traverseUnit(u:CompilationUnit):Option[SymbolPath] = {
        val pf = new PathFinder(symbol)
        pf.traverse(u.body)
        pf.result match {
          case Some((ast:Tree,path:WalkPath,isPrimaryConstructor:Boolean)) =>
            ast match {
              case ModuleDef(_,_, impl) => 
                // ModuleDef as external element dont need full typing
                val tmpTyper = mkPathTyper(u,path)
                val typedAst = try {
                  tmpTyper.context.withMacrosDisabled {
                    tmpTyper.typedParentTypes( impl )
                  }
                } catch {
                  case ex:Throwable  =>
                    macroLogExpand(s"!!!getAst warning: JitTyper Exception(notfatal) " + ex)
                    throw ex
                }
                Option(SymbolPath(ast.symbol,u,ast,path))
              case _:DefDef if isPrimaryConstructor  => // primary constructor 
                macroLogExpand("!!!getAst Skip Constructor typing rawAst="+showRaw(ast) )
                // Skip Constructor typing 
                Option(SymbolPath(ast.symbol,u,ast,path))
              case _ => 
                val tmpTyper = mkPathTyper(u,path)

                macroLogExpand("!!!getAst rawAst="+showRaw(ast) )
                
                val typedAst = try {
                  tmpTyper.context.withMacrosDisabled {
                    tmpTyper.typed( ast )
                  }
                }
                catch {
                  case ex:Throwable  =>
                    // Ignore Typer Errors
                    macroLogExpand(s"!!!getAst warning: JitTyper Exception(notfatal) " + ex)
                    throw ex
                }
                def filterNonFatalErrors(err: analyzer.AbsTypeError):Boolean = err match {
                  case _:analyzer.SymbolTypeError | _:analyzer.NormalTypeError => 
                    // TODO: 
                    // 1 ) analyzer.NormalTypeError 
                    //    check t4.scala enter existing symbols problem 
                    // 2 ) analyzer.SymbolTypeError
                    //    [Type error at:funchain.scala,line-46,offset=737] self 
                    //    constructor arguments cannot reference unconstructed `this`
                    false
                      
                  case _ => true
                }
                val errorsToReport = tmpTyper.context.flushAndReturnBuffer()
                if (! errorsToReport.isEmpty ) {
                  val fatalErrors = errorsToReport.filter(filterNonFatalErrors(_))
                  if (! fatalErrors.isEmpty ) {
                    macroLogExpand("!!!macroLogExpand typer errors: error[0].class=" + fatalErrors.head.getClass + " fatalErrors=" + fatalErrors)
                    throw new JitCompilerException("getAst typer exception" + fatalErrors)
                  }
                } else if (typedAst exists (_.isErroneous)) {
                  // macro expand cancel dont generate errors in context
                  // only set typer.infer.setError
                  macroLogExpand(s"!!!getAst typer warning" )
                  throw new JitCompilerException("getAst typer inner exception" ) 
                }
                
                Option(SymbolPath(ast.symbol,u,typedAst,path))
            }
          case None => None
        }
      }
      macroLogExpand("!!!getAst: search symbol="+symbol + "#" + symbol.id)
      traverseUnits(units)
    }
    
    // TODO: Refactor
    private def ifDefinedInTree(sym:Symbol) = (! sym.isErroneous ) && (
    // TODO: sym.associatedFile can be null ORLY ????
    ( sym.associatedFile != null ) &&
      units.exists(unit => unit.source.file.canonicalPath == sym.associatedFile.canonicalPath)
    )

    /** Get clousure on toCheck Symbol list , with respect to forbiddenSet and checkedSyms
     * @param toCheck
     * @param checkedSyms
     * @param forbiddenSet
     * @param skippedSet - Just skip symbols ( getters to VAL )
     * @return SymbolSet - set of SymbolPath elements
     */
    def getLinkedIds(toCheck:List[Symbol],checkedSyms:SymbolSet,forbiddenSet:Set[Symbol]=Set(),skippedSet:Set[Symbol]=Set()):SymbolSet = {
      toCheck match {
        case symSearch :: xs  => {
          macroLogExpand("!!!getLinkedIds symSearch=" + symSearch +" forbiddenSet="+forbiddenSet)
          getAst(symSearch) match {                                  // case ModuleDef() =>
            case Some(sp @ SymbolPath(symFound,unit,ast @ ModuleDef(mods,name,Template(parents,selfTree,body)),path)) =>
              macroLogExpand("!!!getLinkedIds found ModuleDef ast="+showRaw(ast))
              
              var forbiddenSet0 = forbiddenSet
              var checkedSyms0 = checkedSyms + sp
              
              def getExternals(ast:Tree):List[Symbol] = {
                val em = new ExternalsMarker(checkedSyms,symFound,skippedSet)
                em.traverse(ast)
                if (em.externalSyms.exists { forbiddenSet contains _ } )
                  throw JitCompilerCycleFoundException("")
                em.externalSyms.toList
              }

              val localParents:List[Symbol] = parents flatMap { getExternals(_) } filter { ifDefinedInTree(_) }
              
              macroLogExpand(">>>getLinkedIds_ModuleDef: Already Checked " + checkedSyms + " Check now: " + sp + 
                " Parent Syms: " + localParents.map( s => s + "#" + s.id)
              )

              val skippedSet0:Set[Symbol] = skippedSet ++ localParents
              // Mark always needed symbols 
              // 1 ) ovelroaded - we dont know exactly who calls them
              // 2 ) constuctor
              // TODO: Don't ignore Symbol with cycle but generate exception - to receive 
              // error description on compilation time against "Symbol not found" or "False overriden usage"
              for ( decl <- body if decl.symbol.name == nme.CONSTRUCTOR || decl.symbol.isOverridingSymbol ) {
                try {
                  val ret = getLinkedIds(List(decl.symbol),checkedSyms0,forbiddenSet0,skippedSet0)
                  checkedSyms0 = ret
                } catch {
                  case _:JitCompilerCycleFoundException =>
                    forbiddenSet0+=decl.symbol
                }
              }

              macroLogExpand("<<<getLinkedIds_ModuleDef: Current external syms " + checkedSyms0 + " forbiddenSet = " + forbiddenSet0)
              getLinkedIds( localParents ::: xs ,checkedSyms0,forbiddenSet0,skippedSet)
            case Some(sp @ SymbolPath(symFound,unit,ast,path)) =>
              macroLogExpand("!!!getLinkedIds found typed ast="+showRaw(ast,printTypes=true))
              macroLogExpand(">>>ExternalsMarkerCall: Start Already Checked " + checkedSyms + " Check now: " + sp)
              // ValDef support - if we found some symbol as symSearch filter ValDef element (symFound ) 
              // with its getter (symSearch)
              val skippedSet0 = if (symFound == symSearch) skippedSet else skippedSet + symSearch
              val em = new ExternalsMarker(checkedSyms,symFound,skippedSet0)
              em.traverse(ast)
              if (em.externalSyms.exists { forbiddenSet contains _ } )
                throw JitCompilerCycleFoundException("")
              // fly over not static symbols
              
              val newToCheck = em.externalSyms.toList
              
              macroLogExpand("<<<ExternalsMarkerCall: Result New external syms " + newToCheck)
              getLinkedIds(newToCheck ::: xs ,checkedSyms + sp,forbiddenSet,skippedSet0)
            case None =>
              macroLogExpand("!!!getLinkedIds Warn: Cant resolve ast for sym:" + symSearch + " sourceFile:" +symSearch.sourceFile + " ifDefinedInTree:" + ifDefinedInTree(symSearch))
              throw JitCompilerNoSymbolFoundException("Cant Resolve Symbol: " + symSearch + " in current ast")
              // TODO: Fatal error !
              getLinkedIds( xs ,checkedSyms,forbiddenSet,skippedSet)
          }
        }
        case Nil      => checkedSyms
      }
    }


    class HardResetter(debug:Boolean) extends Transformer {
      override def transform(tree: Tree): Tree = {
          super.transform {
            if (debug)
              println(">>>super.transform " + showRaw(tree,printTypes=true))
            val ret = tree match {
              case tpt: TypeTree =>
                if (tpt.original != null)
                  transform(tpt.original)
                else if (tpt.tpe != null) {
                  val dupl = tpt.duplicate
                  dupl.tpe = null
                  dupl
                }
                else tree
              case TypeApply(fn, args) if args map transform exists (_.isEmpty) =>
                transform(fn)
              case This(_) if tree.symbol != null && tree.symbol.isPackageClass =>
                tree
              case EmptyTree =>
                tree
              case _ =>
                val dupl = tree.duplicate
                if (tree.hasSymbolField)
                  dupl.symbol = NoSymbol
                if (dupl.canHaveAttrs)
                  dupl.tpe = null
                dupl
            }
            if (debug)
              println("<<<super.transform " + showRaw(ret,printTypes=true))
            ret
          }
      }
    }

    
    // Same as resetAllAttrs but ignores wasEmpty flag
    // Thynthetic vars like val t =  xs.map(_.tree) -
    // ValDef(Modifiers(PARAM | SYNTHETIC), newTermName("x$1"), TypeTree[1](), EmptyTree)), Select(Ident(newTermName("x$1")), newTermName("tree"))
    // [1] should be also reseted
    def hardResetAllAttrs(tree:Tree,debug:Boolean=false):Tree = new HardResetter(debug).transform(tree)

    def compileClosure(closure:SymbolSet,compiler:JitGlobal):JitResult = {
      val run = new compiler.Run
      val macroImporter = compiler.mkImporter(this)
      val unitsToCompile = closure.groupBy(_.unit).map(gr => {
        val (u,els) = gr
        val pdef:PackageDef = packUnitElements(u,u.body,els.toList).asInstanceOf[PackageDef]

        
        def convertedPackage:compiler.PackageDef = macroImporter.importTree(hardResetAllAttrs(pdef)).asInstanceOf[compiler.PackageDef]

        // TODO: pdef.name.toString == "<empty>" - some better way ?
        def wrappedPackage = if (pdef.name.toString == "<empty>") convertedPackage.stats else List( convertedPackage )
        val cu = new compiler.CompilationUnit(NoSourceFile)
        cu.body = compiler.PackageDef(
          compiler.Ident(pdef.name.toString) , wrappedPackage 
        )
        
        cu
      }).toList
      val numErrors    = compiler.reporter.ERROR.count
      def hasNewErrors = compiler.reporter.ERROR.count > numErrors
      
      run.compileUnits(unitsToCompile, run.namerPhase)

      if (! hasNewErrors)
        JitSuccess()
      else
        JitError()
    }
  
  private def packUnitElementsList(unit:CompilationUnit,startTrees:List[Tree],els:List[SymbolPath],startTree:Tree):Iterable[Tree] = {
    val simPaths = els.groupBy(e => if (e.path.isEmpty) -1 else e.path.head )
    val maxIndex = simPaths.keys.max
    val needConstructor = startTree.isInstanceOf[Template]
    macroLogExpand(">>>packUnitElementsList class=" + startTree.getClass + " l="+startTrees.length+" trees="+startTrees.map(_.summaryString) + " els=" + els + " simPaths:" + simPaths)

    val ret = for ( (tree,i) <- startTrees.zipWithIndex ) yield {
        simPaths.get(i) match {
          case Some(el) => Some(packUnitElements(unit,tree,el.map(_.shorten )))
          case None => tree match {
            case _:Import if i < maxIndex => Some(tree)
            // nme.CONSTRUCTOR - is already here but for "input point" method objects are not in scope 
            case DefDef(_,nme.CONSTRUCTOR,_,_,_,_) if needConstructor => Some(tree)
            case _ => None
          }
        }
      }

    macroLogExpand("<<<packUnitElementsList" /*+ ret.flatten*/)
    ret.flatten
  }

  private def packUnitElements(unit:CompilationUnit,startTree:Tree,els:List[SymbolPath]):Tree = {
    macroLogExpand(">>>packUnitElements u="+unit + " tree="+startTree.summaryString +"@"+startTree.getClass + " els=" + els)
    assert(! els.isEmpty , "packUnitElements has empty input list")
    val pathEmpty  =els.head.path.isEmpty

    val ret = startTree  match {
        case PackageDef(pid,stats) =>
          PackageDef(pid,packUnitElementsList(unit,pid :: stats,els.map(_.shorten ),startTree).toList)
        case ModuleDef(mods,name,impl) =>
          ModuleDef(mods,name,packUnitElementsList(unit,mods.annotations :+ impl,els,startTree).head.asInstanceOf[Template])
        case Template(parents, selftree, body) =>
          val childs = ( if ( selftree == emptyValDef ) parents else  parents :+selftree ) ++ body
          Template(parents,selftree,packUnitElementsList(unit,childs,els,startTree).toList)
          //startTree
        case _:DefDef if pathEmpty=>
          startTree
        case _ =>
          macroLogExpand("Found not implemented " + startTree.summaryString)
          startTree
    }
    macroLogExpand("<<<packUnitElements " + ret)
    ret
  }

}

object JitGlobal {
}

class JitGlobal(currentSettings: Settings, reporter: Reporter)
  extends Global(currentSettings, reporter) {

}

abstract class JitResult
case class JitSuccess() extends JitResult
case class JitError() extends JitResult
case class JitSkip() extends JitResult

case class JitCompilerCycleFoundException(msg: String) extends Exception(msg)
case class JitCompilerNoSymbolFoundException(msg: String) extends Exception(msg)
case class JitCompilerException(msg: String) extends Exception(msg)
