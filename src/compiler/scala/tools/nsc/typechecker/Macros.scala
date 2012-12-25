package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.tools.nsc.util._
import scala.reflect.runtime.ReflectionUtils
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.reflect.internal.util.Statistics
import scala.reflect.macros.util._
import scala.util.control.ControlThrowable
import scala.reflect.macros.runtime.AbortMacroException

/**
 *  Code to deal with macros, namely with:
 *    * Compilation of macro definitions
 *    * Expansion of macro applications
 *
 *  Say we have in a class C:
 *
 *    def foo[T](xs: List[T]): T = macro fooBar
 *
 *  Then fooBar needs to point to a static method of the following form:
 *
 *    def fooBar[T: c.WeakTypeTag] // type tag annotation is optional
 *           (c: scala.reflect.macros.Context)
 *           (xs: c.Expr[List[T]])
 *           : c.Expr[T] = {
 *      ...
 *    }
 *
 *  Then, if foo is called in qual.foo[Int](elems), where qual: D,
 *  the macro application is expanded to a reflective invocation of fooBar with parameters:
 *
 *    (simpleMacroContext{ type PrefixType = D; val prefix = qual })
 *    (Expr(elems))
 *    (TypeTag(Int))
 */
trait Macros extends scala.tools.reflect.FastTrack with Traces {
  self: Analyzer =>

  import global._
  import definitions._
  import treeInfo.{isRepeatedParamType => _, _}
  import MacrosStats._
  def globalSettings = global.settings

  /** `MacroImplBinding` and its companion module are responsible for
   *  serialization/deserialization of macro def -> impl bindings.
   *
   *  The first officially released version of macros persisted these bindings across compilation runs
   *  using a neat trick. The right-hand side of a macro definition (which contains a reference to a macro impl)
   *  was typechecked and then put verbatim into an annotation on the macro definition.
   *
   *  This solution is very simple, but unfortunately it's also lacking. If we use it, then
   *  signatures of macro defs become transitively dependent on scala-reflect.jar
   *  (because they refer to macro impls, and macro impls refer to scala.reflect.macros.Context defined in scala-reflect.jar).
   *  More details can be found in comments to https://issues.scala-lang.org/browse/SI-5940.
   *
   *  Therefore we have to avoid putting macro impls into binding pickles and come up with our own serialization format.
   *  Situation is further complicated by the fact that it's not enough to just pickle macro impl's class name and method name,
   *  because macro expansion needs some knowledge about the shape of macro impl's signature (which we can't pickle).
   *  Hence we precompute necessary stuff (e.g. the layout of type parameters) when compiling macro defs.
   */

  /** Represents all the information that a macro definition needs to know about its implementation.
   *  Includes a path to load the implementation via Java reflection,
   *  and various accounting information necessary when composing an argument list for the reflective invocation.
   */
  private case class MacroImplBinding(
    // Java class name of the class that contains the macro implementation
    // is used to load the corresponding object with Java reflection
    val className: String,
    // method name of the macro implementation
    // `className` and `methName` are all we need to reflectively invoke a macro implementation
    // because macro implementations cannot be overloaded
    val methName: String,
    // flattens the macro impl's parameter lists having symbols replaced with metadata
    // currently metadata is an index of the type parameter corresponding to that type tag (if applicable)
    // f.ex. for: def impl[T: WeakTypeTag, U: WeakTypeTag, V](c: Context)(x: c.Expr[T]): (U, V) = ???
    // `signature` will be equal to List(-1, -1, 0, 1)
    val signature: List[Int],
    // type arguments part of a macro impl ref (the right-hand side of a macro definition)
    // these trees don't refer to a macro impl, so we can pickle them as is
    val targs: List[Tree])

  /** Macro def -> macro impl bindings are serialized into a `macroImpl` annotation
   *  with synthetic content that carries the payload described in `MacroImplBinding`.
   *
   *  For example, for a pair of macro definition and macro implementation:
   *    def impl(c: scala.reflect.macros.Context): c.Expr[Unit] = c.literalUnit;
   *    def foo: Unit = macro impl
   *
   *  We will have the following annotation added on the macro definition `foo`:
   *
   *    @scala.reflect.macros.internal.macroImpl(
   *      `macro`(
   *        "signature" = List(-1),
   *        "methodName" = "impl",
   *        "versionFormat" = 1,
   *        "className" = "Macros$"))
   */
  private object MacroImplBinding {
    val versionFormat = 1

    def pickleAtom(obj: Any): Tree =
      obj match {
        case list: List[_] => Apply(Ident(ListModule), list map pickleAtom)
        case s: String => Literal(Constant(s))
        case i: Int => Literal(Constant(i))
      }

    def unpickleAtom(tree: Tree): Any =
      tree match {
        case Apply(list @ Ident(_), args) if list.symbol == ListModule => args map unpickleAtom
        case Literal(Constant(s: String)) => s
        case Literal(Constant(i: Int)) => i
      }

    def pickle(macroImplRef: Tree): Tree = {
      val MacroImplReference(owner, macroImpl, targs) = macroImplRef
      val paramss = macroImpl.paramss

      // todo. refactor when fixing SI-5498
      def className: String = {
        def loop(sym: Symbol): String = sym match {
          case sym if sym.owner.isPackageClass =>
            val suffix = if (sym.isModuleClass) "$" else ""
            sym.fullName + suffix
          case sym =>
            val separator = if (sym.owner.isModuleClass) "" else "$"
            loop(sym.owner) + separator + sym.javaSimpleName.toString
        }

        loop(owner)
      }

      def signature: List[Int] = {
        val transformed = transformTypeTagEvidenceParams(paramss, (param, tparam) => tparam)
        transformed.flatten map (p => if (p.isTerm) -1 else p.paramPos)
      }

      val payload = List[(String, Any)](
        "versionFormat" -> versionFormat,
        "className"     -> className,
        "methodName"    -> macroImpl.name.toString,
        "signature"     -> signature
      )

      // the shape of the nucleus is chosen arbitrarily. it doesn't carry any payload.
      // it's only necessary as a stub `fun` for an Apply node that carries metadata in its `args`
      // so don't try to find a program element named "macro" that corresponds to the nucleus
      // I just named it "macro", because it's macro-related, but I could as well name it "foobar"
      val nucleus = Ident(newTermName("macro"))
      val wrapped = Apply(nucleus, payload map { case (k, v) => Assign(pickleAtom(k), pickleAtom(v)) })
      val pickle = gen.mkTypeApply(wrapped, targs map (_.duplicate))

      // assign NoType to all freshly created AST nodes
      // otherwise pickler will choke on tree.tpe being null
      // there's another gotcha
      // if you don't assign a ConstantType to a constant
      // then pickling will crash
      new Transformer {
        override def transform(tree: Tree) = {
          tree match {
            case Literal(const @ Constant(x)) if tree.tpe == null => tree setType ConstantType(const)
            case _ if tree.tpe == null => tree setType NoType
            case _ => ;
          }
          super.transform(tree)
        }
      }.transform(pickle)
    }

    def unpickle(pickle: Tree): MacroImplBinding = {
      val (wrapped, targs) =
        pickle match {
          case TypeApply(wrapped, targs) => (wrapped, targs)
          case wrapped => (wrapped, Nil)
        }
      val Apply(_, pickledPayload) = wrapped
      val payload = pickledPayload.map{ case Assign(k, v) => (unpickleAtom(k), unpickleAtom(v)) }.toMap

      val pickleVersionFormat = payload("versionFormat").asInstanceOf[Int]
      if (versionFormat != pickleVersionFormat) throw new Error("macro impl binding format mismatch: expected $versionFormat, actual $pickleVersionFormat")

      val className = payload("className").asInstanceOf[String]
      val methodName = payload("methodName").asInstanceOf[String]
      val signature = payload("signature").asInstanceOf[List[Int]]
      MacroImplBinding(className, methodName, signature, targs)
    }
  }

  private def bindMacroImpl(macroDef: Symbol, macroImplRef: Tree): Unit = {
    val pickle = MacroImplBinding.pickle(macroImplRef)
    macroDef withAnnotation AnnotationInfo(MacroImplAnnotation.tpe, List(pickle), Nil)
  }

  private def loadMacroImplBinding(macroDef: Symbol): MacroImplBinding = {
    val Some(AnnotationInfo(_, List(pickle), _)) = macroDef.getAnnotation(MacroImplAnnotation)
    MacroImplBinding.unpickle(pickle)
  }

  /** Transforms parameters lists of a macro impl.
   *  The `transform` function is invoked only for WeakTypeTag evidence parameters.
   *
   *  The transformer takes two arguments: a value parameter from the parameter list
   *  and a type parameter that is witnesses by the value parameter.
   *
   *  If the transformer returns a NoSymbol, the value parameter is not included from the result.
   *  If the transformer returns something else, this something else is included in the result instead of the value parameter.
   *
   *  Despite of being highly esoteric, this function significantly simplifies signature analysis.
   *  For example, it can be used to strip macroImpl.paramss from the evidences (necessary when checking def <-> impl correspondence)
   *  or to streamline creation of the list of macro arguments.
   */
  private def transformTypeTagEvidenceParams(paramss: List[List[Symbol]], transform: (Symbol, Symbol) => Symbol): List[List[Symbol]] = {
    if (paramss.isEmpty || paramss.last.isEmpty) return paramss // no implicit parameters in the signature => nothing to do
    if (paramss.head.isEmpty || !(paramss.head.head.tpe <:< MacroContextClass.tpe)) return paramss // no context parameter in the signature => nothing to do
    def transformTag(param: Symbol): Symbol = param.tpe.dealias match {
      case TypeRef(SingleType(SingleType(NoPrefix, c), universe), WeakTypeTagClass, targ :: Nil)
      if c == paramss.head.head && universe == MacroContextUniverse =>
        transform(param, targ.typeSymbol)
      case _ =>
        param
    }
    val transformed = paramss.last map transformTag filter (_ ne NoSymbol)
    if (transformed.isEmpty) paramss.init else paramss.init :+ transformed
  }

  def computeMacroDefTypeFromMacroImpl(macroDdef: DefDef, macroImpl: Symbol): Type = {
    // Step I. Transform c.Expr[T] to T
    var runtimeType = macroImpl.tpe.finalResultType.dealias match {
      case TypeRef(_, ExprClass, runtimeType :: Nil) => runtimeType
      case _ => AnyTpe // so that macro impls with rhs = ??? don't screw up our inference
    }

    // Step II. Transform type parameters of a macro implementation into type arguments in a macro definition's body
    runtimeType = runtimeType.substituteTypes(macroImpl.typeParams, loadMacroImplBinding(macroDdef.symbol).targs.map(_.tpe))

    // Step III. Transform c.prefix.value.XXX to this.XXX and implParam.value.YYY to defParam.YYY
    def unsigma(tpe: Type): Type =
      transformTypeTagEvidenceParams(macroImpl.paramss, (param, tparam) => NoSymbol) match {
        case (implCtxParam :: Nil) :: implParamss =>
          val implToDef = flatMap2(implParamss, macroDdef.vparamss)(map2(_, _)((_, _))).toMap
          object UnsigmaTypeMap extends TypeMap {
            def apply(tp: Type): Type = tp match {
              case TypeRef(pre, sym, args) =>
                val pre1 = pre match {
                  case SingleType(SingleType(SingleType(NoPrefix, c), prefix), value) if c == implCtxParam && prefix == MacroContextPrefix && value == ExprValue =>
                    ThisType(macroDdef.symbol.owner)
                  case SingleType(SingleType(NoPrefix, implParam), value) if value == ExprValue =>
                    implToDef get implParam map (defParam => SingleType(NoPrefix, defParam.symbol)) getOrElse pre
                  case _ =>
                    pre
                }
                val args1 = args map mapOver
                TypeRef(pre1, sym, args1)
              case _ =>
                mapOver(tp)
            }
          }

          UnsigmaTypeMap(tpe)
        case _ =>
          tpe
      }

    unsigma(runtimeType)
  }

  /** A reference macro implementation signature compatible with a given macro definition.
   *
   *  In the example above for the following macro def:
   *    def foo[T](xs: List[T]): T = macro fooBar
   *
   *  This function will return:
   *    (c: scala.reflect.macros.Context)(xs: c.Expr[List[T]]): c.Expr[T]
   *
   *  Note that type tag evidence parameters are not included into the result.
   *  Type tag context bounds for macro impl tparams are optional.
   *  Therefore compatibility checks ignore such parameters, and we don't need to bother about them here.
   *
   *  @param macroDef The macro definition symbol
   *  @param tparams  The type parameters of the macro definition
   *  @param vparamss The value parameters of the macro definition
   *  @param retTpe   The return type of the macro definition
   */
  private def macroImplSig(macroDef: Symbol, tparams: List[TypeDef], vparamss: List[List[ValDef]], retTpe: Type): (List[List[Symbol]], Type) = {
    // had to move method's body to an object because of the recursive dependencies between sigma and param
    object SigGenerator {
      def WeakTagClass   = getMember(MacroContextClass, tpnme.WeakTypeTag)
      def ExprClass      = getMember(MacroContextClass, tpnme.Expr)
      val cache          = scala.collection.mutable.Map[Symbol, Symbol]()
      val ctxParam       = makeParam(nme.macroContext, macroDef.pos, MacroContextClass.tpe, SYNTHETIC)
      val paramss        = List(ctxParam) :: mmap(vparamss)(param)
      val implReturnType =
        if (macroDef.isTermMacro) typeRef(singleType(NoPrefix, ctxParam), ExprClass, List(sigma(retTpe)))
        else if (macroDef.isTypeMacro) typeRef(singleType(NoPrefix, ctxParam), TreeType, List())
        else abort(s"unknown macro flavor: $macroDef")

      object SigmaTypeMap extends TypeMap {
        def mapPrefix(pre: Type) = pre match {
          case ThisType(sym) if sym == macroDef.owner =>
            singleType(singleType(singleType(NoPrefix, ctxParam), MacroContextPrefix), ExprValue)
          case SingleType(NoPrefix, sym) =>
            mfind(vparamss)(_.symbol == sym).fold(pre)(p => singleType(singleType(NoPrefix, param(p)), ExprValue))
          case _ =>
            mapOver(pre)
        }
        def apply(tp: Type): Type = tp match {
          case TypeRef(pre, sym, args) =>
            val pre1  = mapPrefix(pre)
            val args1 = mapOverArgs(args, sym.typeParams)
            if ((pre eq pre1) && (args eq args1)) tp
            else typeRef(pre1, sym, args1)
          case _ =>
            mapOver(tp)
        }
      }
      def sigma(tpe: Type): Type = SigmaTypeMap(tpe)

      def makeParam(name: Name, pos: Position, tpe: Type, flags: Long) =
        macroDef.newValueParameter(name.toTermName, pos, flags) setInfo tpe
      def implType(isType: Boolean, origTpe: Type): Type = {
        def tsym = if (isType) WeakTagClass else ExprClass
        def targ = origTpe.typeArgs.headOption getOrElse NoType

        if (isRepeatedParamType(origTpe))
          scalaRepeatedType(implType(isType, sigma(targ)))
        else
          typeRef(singleType(NoPrefix, ctxParam), tsym, List(sigma(origTpe)))
      }
      def param(tree: Tree): Symbol = (
        cache.getOrElseUpdate(tree.symbol, {
          val sym = tree.symbol
          makeParam(sym.name, sym.pos, implType(sym.isType, sym.tpe), sym getFlag SYNTHETIC)
        })
      )
    }

    import SigGenerator._
    macroTraceVerbose("generating macroImplSigs for: ")(macroDef)
    macroTraceVerbose("tparams are: ")(tparams)
    macroTraceVerbose("vparamss are: ")(vparamss)
    macroTraceVerbose("retTpe is: ")(retTpe)
    macroTraceVerbose("macroImplSig is: ")((paramss, implReturnType))
  }

  /** Verifies that the body of a macro def typechecks to a reference to a static public non-overloaded method,
   *  and that that method is signature-wise compatible with the given macro definition.
   *
   *  @return Typechecked rhs of the given macro definition if everything is okay.
   *          EmptyTree if an error occurs.
   */
  def typedMacroBody(typer: Typer, macroDdef: DefDef): Tree =
    try new MacroTyper(typer, macroDdef).typed
    catch { case MacroBodyTypecheckException => EmptyTree }

  class MacroTyper(val typer: Typer, val macroDdef: DefDef) extends MacroErrors {
    // Phase I: sanity checks
    val macroDef = macroDdef.symbol
    macroLogVerbose("typechecking macro def %s at %s".format(macroDef, macroDdef.pos))
    assert(macroDef.isMacro, macroDdef)
    if (fastTrack contains macroDef) MacroDefIsFastTrack()
    if (!typer.checkFeature(macroDdef.pos, MacrosFeature, immediate = true)) MacroFeatureNotEnabled()

    // we use typed1 instead of typed, because otherwise adapt is going to mess us up
    // if adapt sees <qualifier>.<method>, it will want to perform eta-expansion and will fail
    // unfortunately, this means that we have to manually trigger macro expansion
    // because it's adapt which is responsible for automatic expansion during typechecking
    def typecheckRhs(rhs: Tree): Tree = {
      try {
        // interestingly enough, just checking isErroneous doesn't cut it
        // e.g. a "type arguments [U] do not conform to method foo's type parameter bounds" error
        // doesn't manifest itself as an error in the resulting tree
        val prevNumErrors = reporter.ERROR.count
        var rhs1 = typer.typed1(rhs, EXPRmode, WildcardType)
        def rhsNeedsMacroExpansion = rhs1.symbol != null && rhs1.symbol.isMacro && !rhs1.symbol.isErroneous
        while (rhsNeedsMacroExpansion) {
          rhs1 = macroExpand1(typer, rhs1) match {
            case Success(expanded) =>
              try {
                val typechecked = typer.typed1(expanded, EXPRmode, WildcardType)
                macroLogVerbose("typechecked1:%n%s%n%s".format(typechecked, showRaw(typechecked)))
                typechecked
              } finally {
                popMacroContext()
              }
            case Fallback(fallback) =>
              typer.typed1(fallback, EXPRmode, WildcardType)
            case Deferred(deferred) =>
              deferred
            case Failure(failure) =>
              failure
          }
        }
        val typecheckedWithErrors = (rhs1 exists (_.isErroneous)) || reporter.ERROR.count != prevNumErrors
        if (typecheckedWithErrors) MacroDefUntypeableBodyError()
        rhs1
      } catch {
        case ex: TypeError =>
          typer.reportTypeError(context, rhs.pos, ex)
          MacroDefUntypeableBodyError()
      }
    }

    // Phase II: typecheck the right-hand side of the macro def
    val typed = typecheckRhs(macroDdef.rhs)
    typed match {
      case MacroImplReference(owner, meth, targs) =>
        if (!meth.isMethod) MacroDefInvalidBodyError()
        if (!meth.isPublic) MacroImplNotPublicError()
        if (meth.isOverloaded) MacroImplOverloadedError()
        if (!owner.isStaticOwner && !owner.moduleClass.isStaticOwner) MacroImplNotStaticError()
        if (meth.typeParams.length != targs.length) MacroImplWrongNumberOfTypeArgumentsError(typed)
        bindMacroImpl(macroDef, typed)
      case _ =>
        MacroDefInvalidBodyError()
    }

    // Phase III: check compatibility between the macro def and its macro impl
    // this check ignores type tag evidence parameters, because type tag context bounds are optional
    // aXXX (e.g. aparamss) => characteristics of the macro impl ("a" stands for "actual")
    // rXXX (e.g. rparamss) => characteristics of a reference macro impl signature synthesized from the macro def ("r" stands for "reference")
    val macroImpl = typed.symbol
    val aparamss = transformTypeTagEvidenceParams(macroImpl.paramss, (param, tparam) => NoSymbol)
    val aret = macroImpl.tpe.finalResultType
    val macroDefRet =
      if (!macroDdef.tpt.isEmpty) typer.typedType(macroDdef.tpt).tpe
      else computeMacroDefTypeFromMacroImpl(macroDdef, macroImpl)
    val (rparamss, rret) = macroImplSig(macroDef, macroDdef.tparams, macroDdef.vparamss, macroDefRet)

    val implicitParams = aparamss.flatten filter (_.isImplicit)
    if (implicitParams.nonEmpty) MacroImplNonTagImplicitParameters(implicitParams)
    if (aparamss.length != rparamss.length) MacroImplParamssMismatchError()

    val atparams = macroImpl.typeParams
    val atvars = atparams map freshVar
    def atpeToRtpe(atpe: Type) = atpe.substSym(aparamss.flatten, rparamss.flatten).instantiateTypeParams(atparams, atvars)

    try {
      map2(aparamss, rparamss)((aparams, rparams) => {
        if (aparams.length < rparams.length) MacroImplMissingParamsError(aparams, rparams)
        if (rparams.length < aparams.length) MacroImplExtraParamsError(aparams, rparams)
      })

      // cannot fuse these loops because if aparamss.flatten != rparamss.flatten
      // then `atpeToRtpe` is going to fail with an unsound substitution
      map2(aparamss.flatten, rparamss.flatten)((aparam, rparam) => {
        if (aparam.name != rparam.name && !rparam.isSynthetic) MacroImplParamNameMismatchError(aparam, rparam)
        if (isRepeated(aparam) ^ isRepeated(rparam)) MacroImplVarargMismatchError(aparam, rparam)
        val aparamtpe = aparam.tpe.dealias match {
          case RefinedType(List(tpe), Scope(sym)) if tpe == MacroContextClass.tpe && sym.allOverriddenSymbols.contains(MacroContextPrefixType) => tpe
          case tpe => tpe
        }
        checkMacroImplParamTypeMismatch(atpeToRtpe(aparamtpe), rparam)
      })

      checkMacroImplResultTypeMismatch(atpeToRtpe(aret), rret)

      val maxLubDepth = lubDepth(aparamss.flatten map (_.tpe)) max lubDepth(rparamss.flatten map (_.tpe))
      val atargs = solvedTypes(atvars, atparams, atparams map varianceInType(aret), upper = false, depth = maxLubDepth)
      val boundsOk = typer.silent(_.infer.checkBounds(macroDdef, NoPrefix, NoSymbol, atparams, atargs, ""))
      boundsOk match {
        case SilentResultValue(true) => // do nothing, success
        case SilentResultValue(false) | SilentTypeError(_) => MacroImplTargMismatchError(atargs, atparams)
      }
    } catch {
      case ex: NoInstance => MacroImplTparamInstantiationError(atparams, ex)
    }
  }

  /** Macro classloader that is used to resolve and run macro implementations.
   *  Loads classes from from -cp (aka the library classpath).
   *  Is also capable of detecting REPL and reusing its classloader.
   */
  lazy val macroClassloader: ClassLoader = {
    val classpath = global.classPath.asURLs
    macroLogVerbose("macro classloader: initializing from -cp: %s".format(classpath))
    val loader = ScalaClassLoader.fromURLs(classpath, self.getClass.getClassLoader)

    // a heuristic to detect the REPL
    if (global.settings.exposeEmptyPackage.value) {
      macroLogVerbose("macro classloader: initializing from a REPL classloader".format(global.classPath.asURLs))
      import scala.tools.nsc.interpreter._
      val virtualDirectory = global.settings.outputDirs.getSingleOutput.get
      new AbstractFileClassLoader(virtualDirectory, loader) {}
    } else {
      loader
    }
  }

  /** Produces a function that can be used to invoke macro implementation for a given macro definition:
   *    1) Looks up macro implementation symbol in this universe.
   *    2) Loads its enclosing class from the macro classloader.
   *    3) Loads the companion of that enclosing class from the macro classloader.
   *    4) Resolves macro implementation within the loaded companion.
   *
   *  @return Requested runtime if macro implementation can be loaded successfully from either of the mirrors,
   *          `null` otherwise.
   */
  type MacroRuntime = MacroArgs => Any
  private val macroRuntimesCache = perRunCaches.newWeakMap[Symbol, MacroRuntime]
  private def macroRuntime(macroDef: Symbol): MacroRuntime = {
    macroTraceVerbose("looking for macro implementation: ")(macroDef)
    if (fastTrack contains macroDef) {
      macroLogVerbose("macro expansion is serviced by a fast track")
      fastTrack(macroDef)
    } else {
      macroRuntimesCache.getOrElseUpdate(macroDef, {
        val binding = loadMacroImplBinding(macroDef)
        val className = binding.className
        val methName = binding.methName
        macroLogVerbose(s"resolved implementation as $className.$methName")

        // I don't use Scala reflection here, because it seems to interfere with JIT magic
        // whenever you instantiate a mirror (and not do anything with in, just instantiate), performance drops by 15-20%
        // I'm not sure what's the reason - for me it's pure voodoo
        // upd. my latest experiments show that everything's okay
        // it seems that in 2.10.1 we can easily switch to Scala reflection
        try {
          macroTraceVerbose("loading implementation class: ")(className)
          macroTraceVerbose("classloader is: ")(ReflectionUtils.show(macroClassloader))
          val implObj = ReflectionUtils.staticSingletonInstance(macroClassloader, className)
          // relies on the fact that macro impls cannot be overloaded
          // so every methName can resolve to at maximum one method
          val implMeths = implObj.getClass.getDeclaredMethods.find(_.getName == methName)
          val implMeth = implMeths getOrElse { throw new NoSuchMethodException(s"$className.$methName") }
          macroLogVerbose("successfully loaded macro impl as (%s, %s)".format(implObj, implMeth))
          args => implMeth.invoke(implObj, ((args.c +: args.others) map (_.asInstanceOf[AnyRef])): _*)
        } catch {
          case ex: Exception =>
            macroTraceVerbose(s"macro runtime failed to load: ")(ex.toString)
            macroDef setFlag IS_ERROR
            null
        }
      })
    }
  }

  private def macroContext(typer: Typer, prefixTree: Tree, expandeeTree: Tree): MacroContext = {
    new {
      val universe: self.global.type = self.global
      val callsiteTyper: universe.analyzer.Typer = typer.asInstanceOf[global.analyzer.Typer]
      val expandee = universe.analyzer.macroExpanderAttachment(expandeeTree).original orElse expandeeTree
      val macroRole = universe.analyzer.macroExpanderAttachment(expandeeTree).role
    } with UnaffiliatedMacroContext {
      val prefix = Expr[Nothing](prefixTree)(TypeTag.Nothing)
      override def toString = "MacroContext(%s@%s +%d)".format(expandee.symbol.name, expandee.pos, enclosingMacros.length - 1 /* exclude myself */)
    }
  }

  /** Calculate the arguments to pass to a macro implementation when expanding the provided tree.
   */
  case class MacroArgs(c: MacroContext, others: List[Any])

  private def macroArgs(typer: Typer, expandee: Tree): MacroArgs = {
    val macroDef   = expandee.symbol
    val prefixTree = expandee.collect{ case Select(qual, name) => qual }.headOption.getOrElse(EmptyTree)
    val context    = expandee.attachments.get[MacroRuntimeAttachment].flatMap(_.macroContext).getOrElse(macroContext(typer, prefixTree, expandee))
    var typeArgs   = List[Tree]()
    val exprArgs   = ListBuffer[List[Expr[_]]]()
    def collectMacroArgs(tree: Tree): Unit = tree match {
      case Apply(fn, args) =>
        // todo. infer precise typetag for this Expr, namely the declared type of the corresponding macro impl argument
        exprArgs.prepend(args map (arg => context.Expr[Nothing](arg)(TypeTag.Nothing)))
        collectMacroArgs(fn)
      case TypeApply(fn, args) =>
        typeArgs = args
        collectMacroArgs(fn)
      case _ =>
    }
    collectMacroArgs(expandee)

    val argcDoesntMatch = macroDef.paramss.length != exprArgs.length
    val nullaryArgsEmptyParams = exprArgs.isEmpty && macroDef.paramss == ListOfNil
    if (argcDoesntMatch && !nullaryArgsEmptyParams) { typer.TyperErrorGen.MacroPartialApplicationError(expandee) }

    val argss: List[List[Any]] = exprArgs.toList
    macroTraceVerbose("context: ")(context)
    macroTraceVerbose("argss: ")(argss)

    val preparedArgss: List[List[Any]] =
      if (fastTrack contains macroDef) {
        // Take a dry run of the fast track implementation
        if (fastTrack(macroDef) validate expandee) argss
        else typer.TyperErrorGen.MacroPartialApplicationError(expandee)
      }
      else {
        // if paramss have typetag context bounds, add an arglist to argss if necessary and instantiate the corresponding evidences
        // consider the following example:
        //
        //   class D[T] {
        //     class C[U] {
        //       def foo[V] = macro Impls.foo[T, U, V]
        //     }
        //   }
        //
        //   val outer1 = new D[Int]
        //   val outer2 = new outer1.C[String]
        //   outer2.foo[Boolean]
        //
        // then T and U need to be inferred from the lexical scope of the call using `asSeenFrom`
        // whereas V won't be resolved by asSeenFrom and need to be loaded directly from `expandee` which needs to contain a TypeApply node
        // also, macro implementation reference may contain a regular type as a type argument, then we pass it verbatim
        val binding = loadMacroImplBinding(macroDef)
        macroTraceVerbose("binding: ")(binding)
        val tags = binding.signature filter (_ != -1) map (paramPos => {
          val targ = binding.targs(paramPos).tpe.typeSymbol
          val tpe = if (targ.isTypeParameterOrSkolem) {
            if (targ.owner == macroDef) {
              // doesn't work when macro def is compiled separately from its usages
              // then targ is not a skolem and isn't equal to any of macroDef.typeParams
              // val argPos = targ.deSkolemize.paramPos
              val argPos = macroDef.typeParams.indexWhere(_.name == targ.name)
              typeArgs(argPos).tpe
            } else
              targ.tpe.asSeenFrom(
                if (prefixTree == EmptyTree) macroDef.owner.tpe else prefixTree.tpe,
                macroDef.owner)
          } else
            targ.tpe
          context.WeakTypeTag(tpe)
        })
        macroTraceVerbose("tags: ")(tags)

        // transforms argss taking into account varargness of paramss
        // note that typetag context bounds are only declared on macroImpls
        // so this optional arglist might not match macroDef's paramlist
        // nb! varargs can apply to any parameter section, not necessarily to the last one
        mapWithIndex(argss :+ tags)((as, i) => {
          val mapsToParamss = macroDef.paramss.indices contains i
          if (mapsToParamss) {
            val ps = macroDef.paramss(i)
            if (isVarArgsList(ps)) {
              val (normal, varargs) = as splitAt (ps.length - 1)
              normal :+ varargs // pack all varargs into a single List argument
            } else as
          } else as
        })
      }
    macroTraceVerbose("preparedArgss: ")(preparedArgss)
    MacroArgs(context, preparedArgss.flatten)
  }

  /** Keeps track of macros in-flight.
   *  See more informations in comments to `openMacros` in `scala.reflect.macros.Context`.
   */
  private var _openMacros = List[MacroContext]()
  def openMacros = _openMacros
  private def pushMacroContext(c: MacroContext) = _openMacros ::= c
  private def popMacroContext() = _openMacros = _openMacros.tail
  def enclosingMacroPosition = openMacros map (_.macroApplication.pos) find (_ ne NoPosition) getOrElse NoPosition

  /** Describes the role that the macro expandee is performing.
   */
  type MacroRole = String
  final def APPLY_ROLE: MacroRole = "APPLY_ROLE"
  final def TYPE_ROLE: MacroRole = "TYPE_ROLE"
  final def APPLIED_TYPE_ROLE: MacroRole = "APPLIED_TYPE_ROLE"
  final def PARENT_ROLE: MacroRole = "PARENT_ROLE"
  final def NEW_ROLE: MacroRole = "NEW_ROLE"
  final def ANNOTATION_ROLE: MacroRole = "ANNOTATION_ROLE"
  private val roleNames = Map(
    APPLY_ROLE -> "apply", TYPE_ROLE -> "type", APPLIED_TYPE_ROLE -> "applied type",
    PARENT_ROLE -> "parent", NEW_ROLE -> "new", ANNOTATION_ROLE -> "annotation")

  /** Performs macro expansion.
   *
   *  ========= Expandable trees =========
   *
   *  A term of one of the following shapes:
   *
   *    Ident(<term macro>)
   *    Select(<any qualifier>, <term macro>)
   *    TypeApply(<any of the above>, <targs>)
   *    Apply(...Apply(<any of the above>, <args1>)...<argsN>)
   *
   *  A type of one of the following shapes:
   *
   *    Ident(<term macro>)
   *    SelectFromTypeTree(<any qualifier>, <term macro>)
   *    AppliedTypeTree(<any of the above>, <targs>)
   *    DependentTypeTree(...DependentTypeTree(<any of the above>, <args1>)...<argsN>)
   *
   *  Unlike term macros, macro types expand differently depending on the role they play.
   *  There is a total of 5 different roles for macro types:
   *    1) Type tree, as in `def x: TM(2)(3) = ???`
   *    2) Applied type tree, as in `TM(2)(3)[Int]`
   *    3) Parent, as in `class C extends TM(2)(3)` or `new TM(2)(3){}`
   *    4) New, as in `new TM(2)(3)`
   *    5) Annotation, as in `@TM(2)(3) class C`
   *
   *  ========= Macro expansion =========
   *
   *  First of all `macroExpandXXX`:
   *    1) If necessary desugars the `expandee` to fit into `macroExpand1`
   *
   *  Then `macroExpand1`:
   *    2) Checks whether the expansion needs to be delayed (see `mustDelayMacroExpansion`)
   *    3) Loads macro implementation using `macroMirror`
   *    4) Synthesizes invocation arguments for the macro implementation
   *    5) Checks that the result is a tree or an expr bound to this universe
   *
   *  Finally `macroExpandXXX`:
   *    6) Validates the expansion against the white list of supported tree shapes
   *    7) Typechecks the result as required by the circumstances of the macro application
   *
   *  If -Ymacro-debug-lite is enabled, you will get basic notifications about macro expansion
   *  along with macro expansions logged in the form that can be copy/pasted verbatim into REPL.
   *
   *  If -Ymacro-debug-verbose is enabled, you will get detailed log of how exactly this function
   *  performs class loading and method resolution in order to load the macro implementation.
   *  The log will also include other non-trivial steps of macro expansion.
   *
   *  @return
   *    the expansion result                    if the expansion has been successful,
   *    the fallback tree                       if the expansion has been unsuccessful, but there is a fallback,
   *    the expandee unchanged                  if the expansion has been delayed,
   *    the expandee fully expanded             if the expansion has been delayed before and has been expanded now,
   *    the expandee with an error marker set   if the expansion has been cancelled due malformed arguments or implementation
   *    the expandee with an error marker set   if there has been an error
   */
  private abstract class MacroExpander[Result: ClassTag](val role: MacroRole, val typer: Typer, val expandee: Tree) {
    def allowExpandee(expandee: Tree): Boolean = true
    def allowExpanded(expanded: Tree): Boolean = true
    def allowedExpansions: String = "anything"
    def allowResult(result: Result): Boolean = true

    def onSuccess(expanded: Tree): Result
    def onFallback(expanded: Tree): Result
    def onSuppressed(expandee: Tree): Result = expandee match { case expandee: Result => expandee }
    def onDeferred(expanded: Tree): Result = expanded match { case expanded: Result => expanded }
    def onFailure(expanded: Tree): Result = { typer.infer.setError(expandee); expandee match { case expandee: Result => expandee } }

    def template: Option[Template] = None
    def apply(desugared: Tree): Result = {
      if (isMacroExpansionSuppressed(desugared)) onSuppressed(expandee)
      else expand(desugared)
    }

    protected def expand(desugared: Tree): Result = {
      def showDetailed(tree: Tree) = showRaw(tree, printIds = true, printTypes = true)
      def summary() = s"expander = $this, expandee = ${showDetailed(expandee)}, desugared = ${if (expandee == desugared) () else showDetailed(desugared)}"
      if (macroDebugVerbose) println(s"macroExpand: ${summary()}")
      assert(allowExpandee(expandee), summary())

      val start = if (Statistics.canEnable) Statistics.startTimer(macroExpandNanos) else null
      if (Statistics.canEnable) Statistics.incCounter(macroExpandCount)
      try {
        linkExpandeeAndDesugared(expandee, desugared, role, template)
        macroExpand1(typer, desugared) match {
          case Success(expanded) =>
            if (allowExpanded(expanded)) {
              // also see http://groups.google.com/group/scala-internals/browse_thread/thread/492560d941b315cc
              val expanded1 = try onSuccess(duplicateAndKeepPositions(expanded)) finally popMacroContext()
              if (!hasMacroExpansionAttachment(expanded1)) linkExpandeeAndExpanded(expandee, expanded1)
              if (allowResult(expanded1)) expanded1 else onFailure(expanded)
            } else {
              typer.TyperErrorGen.MacroInvalidExpansionError(expandee, roleNames(role), allowedExpansions)
              onFailure(expanded)
            }
          case Fallback(fallback) => onFallback(fallback)
          case Deferred(deferred) => onDeferred(deferred)
          case Failure(failure) => onFailure(failure)
        }
      } finally {
        if (Statistics.canEnable) Statistics.stopTimer(macroExpandNanos, start)
      }
    }
  }

  /** Expands a tree that carries a term, which happens to be a term macro.
   *  @see MacroExpander
   */
   private abstract class TermMacroExpander(role: MacroRole, typer: Typer, expandee: Tree, mode: Int, pt: Type)
                  extends MacroExpander[Tree](role, typer, expandee) {
      override def allowedExpansions: String = "term trees"
      override def allowExpandee(expandee: Tree) = expandee.isTerm
      override def onSuccess(expanded: Tree) = typer.typed(expanded, mode, pt)
      override def onFallback(fallback: Tree) = typer.typed(fallback, mode, pt)
   }

  /** Expands a term macro used in apply role as `M(2)(3)` in `val x = M(2)(3)`.
   *  @see MacroExpander
   */
  def macroExpandApply(typer: Typer, expandee: Tree, mode: Int, pt: Type) = {
    object expander extends TermMacroExpander(APPLY_ROLE, typer, expandee, mode, pt) {
      override def onSuccess(expanded: Tree) = {
        // prematurely annotate the tree with a macro expansion attachment
        // so that adapt called indirectly by typer.typed knows that it needs to apply the existential fixup
        linkExpandeeAndExpanded(expandee, expanded)
        var expectedTpe = expandee.tpe
        if (isNullaryInvocation(expandee)) expectedTpe = expectedTpe.finalResultType
        // `macroExpandApply` is called from `adapt`, where implicit conversions are disabled
        // therefore we need to re-enable the conversions back temporarily
        if (macroDebugVerbose) println(s"typecheck #1 (against expectedTpe = $expectedTpe): $expanded")
        val expanded1 = typer.context.withImplicitsEnabled(typer.typed(expanded, mode, expectedTpe))
        if (expanded1.isErrorTyped) {
          if (macroDebugVerbose) println(s"typecheck #1 has failed: ${typer.context.errBuffer}")
          expanded1
        } else {
          if (macroDebugVerbose) println(s"typecheck #2 (against pt = $pt): $expanded1")
          val expanded2 = typer.context.withImplicitsEnabled(super.onSuccess(expanded1))
          if (macroDebugVerbose && expanded2.isErrorTyped) println(s"typecheck #2 has failed: ${typer.context.errBuffer}")
          expanded2
        }
      }
    }
    expander(expandee)
  }

  /** Expands a tree that carries a type, which happens to be a macro type.
   *  @see MacroExpander
   */
  private abstract class MacroTypeExpander[Result: ClassTag](role: MacroRole, typer: Typer, original: Tree, val tpt: Tree, val targs: List[Tree], val argss: List[List[Tree]])
                 extends MacroExpander[Result](role, typer, original) {
    def isTypeLike: Boolean = false
    def isParentLike: Boolean = false
    def isTemplateLike: Boolean = false

    override def allowExpandee(expandee: Tree) =
      if (isTypeLike) expandee.isType
      else true // the shapes are too diverse to be easily validated

    override def allowedExpansions =
      if (isTypeLike) "Ident, Select, TypTrees except for TypeBoundsTree, and their Annotated versions"
      else if (isParentLike) "Apply, Ident, Select, class type TypTrees, and their Annotated versions"
      else if (isTemplateLike) "Template, Apply, Ident, Select, class type TypTrees, and their Annotated versions"
      else abort("the expander should either be type-like, parent-like or template-like")

    override def allowExpanded(expanded: Tree): Boolean = expanded match {
      case Template(_, _, _) => isTemplateLike
      case Apply(_, _) => isTemplateLike || isParentLike
      case Annotated(_, Apply(_, _)) => isTemplateLike || isParentLike
      case Ident(_) => true
      case Select(_, _) => true
      case Annotated(_, annottee) => allowExpanded(annottee)
      case TypeBoundsTree(_, _) => false
      case _: TypTree => true
      case _ => false
    }

    def allowResultTree(result: Tree): Boolean = {
      if (result.tpe == null || result.isErrorTyped) true
      else allowResultType(result.tpe)
    }

    def allowResultAnn(result: AnnotationInfo): Boolean = {
      if (result.isErroneous || result == ErroneousAnnotation) true
      else allowResultType(result.atp)
    }

    def allowResultType(result: Type): Boolean = {
      // it's incorrect to check only nextOverriddenSymbol, because by the virtue of mixin composition
      // we might end up overriding an unrelated symbol, e.g. as in macro-override-abstract-overrides-macro
      val pre = tpt match { case ref: RefTree => ref.qualifier }
      if (pre != EmptyTree) {
        pre.tpe.baseClasses.forall(base => {
          val overridden = tpt.symbol.macroType.overriddenSymbol(base)
          if (overridden.isAliasType && !overridden.isMacroType || overridden.isAbstractType) {
            val info = overridden.info.asSeenFrom(pre.tpe, overridden.owner)
            // TODO: unfortunately we also need to do some inference here, otherwise we're going to get errors like:
            // macro expansion C violates bounds  >: [U <: C]C <: [U <: C]C
            if (!(info.bounds containsType result)) {
              typer.TyperErrorGen.MacroTypeExpansionViolatesOverriddenBounds(expandee, result, overridden, info.bounds)
              false
            } else true
          } else true
        })
      } else true
    }

    override def onFallback(expanded: Tree): Result = abort("type macros aren't supposed to support fallback")

    override def onDeferred(expanded: Tree): Result = {
      // SI-5692 macro hasn't been expanded. working around...
      typer.TyperErrorGen.MacroTypeHasntBeenExpandedError(expanded)
      onFailure(expandee)
    }

    def prepare(desugared: Tree): Tree

    override def expand(expandee: Tree): Result = {
      assert(tpt.symbol.isMacroType, "core of the macro type application should have been pre-typechecked in advance")
      val macroName = nme.typeMacroName(tpt.symbol.name)
      val macroRef = tpt match {
        case Select(qual, _) => Select(qual, macroName)
        case Ident(_) => Ident(macroName)
      }
      val desugared = atPos(original.pos)(gen.mkApply(macroRef, targs, argss))
      val desugared1 = unsuppressMacroExpansion(prepare(suppressMacroExpansion(desugared)))
      if (desugared1.isErrorTyped) onFailure(desugared1)
      else super.expand(desugared1)
    }
  }

  /** Expands a macro type used in type role as `TM(2)(3)` in `def x: TM(2)(3) = ???`.
   *  @see MacroExpander
   */
  def macroExpandType(typer: Typer, expandee: Tree, mode: Int, pt: Type) = {
    val treeInfo.Applied(tpt, targs, argss) = expandee
    object expander extends MacroTypeExpander[Tree](TYPE_ROLE, typer, expandee, tpt, targs, argss) {
      override def isTypeLike = true
      override def prepare(desugared: Tree) = typer.typed(desugared, EXPRmode, WildcardType)
      override def onSuccess(expanded: Tree) = typer.typed(expanded, mode, pt)
      override def allowResult(result: Tree) = allowResultTree(result)
    }
    expander(expandee)
  }

  /** Expands a macro type used in applied type role as `TM(2)(3)` in `TM(2)(3)[Int]`.
   *  @see MacroExpander
   */
  def macroExpandAppliedType(typer: Typer, expandee: Tree, mode: Int) = {
    val treeInfo.Applied(tpt, targs, argss) = expandee
    object expander extends MacroTypeExpander[Tree](APPLIED_TYPE_ROLE, typer, expandee, tpt, targs, argss) {
      override def isTypeLike = true
      override def prepare(desugared: Tree) = typer.typed(desugared, EXPRmode, WildcardType)
      override def onSuccess(expanded: Tree) = typer.typed1(expanded, mode | FUNmode | TAPPmode, WildcardType)
      override def allowResult(result: Tree) = allowResultTree(result)
    }
    expander(expandee)
  }

  /** Expands a macro type used in parent role as `TM(2)(3)` in `class C extends TM(2)(3)` or `new TM(2)(3){}`.
   *  @see MacroExpander
   */
  def macroExpandParent(typer: Typer, original: Tree, tpt: Tree, targs: List[Tree], argss: List[List[Tree]], templ: Template, inMixinPosition: Boolean) = {
    object expander extends MacroTypeExpander[Tree](PARENT_ROLE, typer, original, tpt, targs, argss) {
      override def isTemplateLike = true
      override def template = Some(templ)
      override def prepare(desugared: Tree) = typer.typedPrimaryConstrBody(templ)(desugared)
      override def onSuccess(expanded: Tree) = {
        val expanded1 = expanded match {
          // AnyRef emitted here is just a dummy that let's the compiler know
          // that the namer needs to replace the template being typechecked
          case templ1 @ Template(_, _, _) => Ident(AnyRefClass) updateAttachment MacroExpansionAttachment(original, expanded)
          case _ => expanded
        }
        typer.typedParentType(expanded1, templ, inMixinPosition)
      }
      override def allowResult(result: Tree) = allowResultTree(result)
    }
    expander(original)
  }

  /** Expands a macro type used in new role as `TM(2)(3)` in `new TM(2)(3)`.
   *  Contrast this role with parent role as in `new TM(2)(3){}`. In the former case, we get a New node,
   *  while in the latter case we get a Template for an anonymous class + a trivial New node.
   *  @see MacroExpander
   */
  def macroExpandNew(typer: Typer, original: Tree, tpt: Tree, targs: List[Tree], argss: List[List[Tree]], mode: Int) = {
    // TODO: at the moment factories for New transform Nil arglists to ListOfNil
    // back then I tried to play with this, but apparently something in compiler's guts depends on this hack and won't give up
    // therefore we need to undo that hack here or otherwise nullary type macros won't be usable in new role
    val argss1 = if (argss == ListOfNil) Nil else argss
    object expander extends MacroTypeExpander[Tree](NEW_ROLE, typer, original, tpt, targs, argss1) {
      // TODO: make type macros expanding in new role to behave template-like
      // it kind of makes sense to e.g. let `new TM` expand into `new C { def x = 2 }`
      override def isParentLike = true
      override def prepare(desugared: Tree) = typer.typed(desugared, EXPRmode, WildcardType)
      override def onSuccess(expanded: Tree) = typer.typed(repackApplyAsNew(expanded), mode, WildcardType)
      override def allowResult(result: Tree) = allowResultTree(result)
    }
    expander(original)
  }

  /** Expands a macro type used in annotation role as `TM(2)(3)` in `@TM(2)(3) class C`.
   *  @see MacroExpander
   */
  def macroExpandAnnotation(typer: Typer, original: Tree, tpt: Tree, targs: List[Tree], argss: List[List[Tree]], mode: Int, selfsym: Symbol, annClass: Symbol, requireJava: Boolean) = {
    // TODO: see an explanation of the situation in comments to `macroExpandNew`
    val argss1 = if (argss == ListOfNil) Nil else argss
    object expander extends MacroTypeExpander[AnnotationInfo](ANNOTATION_ROLE, typer, original, tpt, targs, argss1) {
      override def isParentLike = true
      override def prepare(desugared: Tree) = typer.typed(desugared, EXPRmode, WildcardType)
      override def onSuccess(expanded: Tree) = typer.typedAnnotation(repackApplyAsNew(expanded), mode, selfsym, annClass, requireJava)
      override def onFailure(expanded: Tree) = { typer.infer.setError(original); ErroneousAnnotation }
      override def allowResult(result: AnnotationInfo) = allowResultAnn(result)
    }
    expander(original)
  }

  /** Captures statuses of macro expansions performed by `macroExpand1'.
   */
  private sealed abstract class MacroStatus
  private case class Success(expanded: Tree) extends MacroStatus
  private case class Fallback(fallback: Tree) extends MacroStatus { currentRun.seenMacroExpansionsFallingBack = true }
  private case class Deferred(deferred: Tree) extends MacroStatus
  private case class Failure(failure: Tree) extends MacroStatus
  private def Delay(expanded: Tree) = Deferred(expanded)
  private def Skip(expanded: Tree) = Deferred(expanded)
  private def Cancel(expandee: Tree) = Failure(expandee)

  /** Does the same as `macroExpand`, but without typechecking the expansion
   *  Meant for internal use within the macro infrastructure, don't use it elsewhere.
   */
  private def macroExpand1(typer: Typer, expandee: Tree): MacroStatus = {
    // verbose printing might cause recursive macro expansions, so I'm shutting it down here
    withInfoLevel(nodePrinters.InfoLevel.Quiet) {
      if (expandee.symbol.isErroneous || (expandee exists (_.isErroneous))) {
        val reason = if (expandee.symbol.isErroneous) "not found or incompatible macro implementation" else "erroneous arguments"
        macroTraceVerbose("cancelled macro expansion because of %s: ".format(reason))(expandee)
        Cancel(typer.infer.setError(expandee))
      }
      else try {
        val runtime = macroRuntime(expandee.symbol)
        if (runtime != null) macroExpandWithRuntime(typer, expandee, runtime)
        else macroExpandWithoutRuntime(typer, expandee)
      } catch {
        case typer.TyperErrorGen.MacroExpansionException => Failure(expandee)
      }
    }
  }

  /** Expands a macro when a runtime (i.e. the macro implementation) can be successfully loaded
   *  Meant for internal use within the macro infrastructure, don't use it elsewhere.
   */
  private def macroExpandWithRuntime(typer: Typer, expandee: Tree, runtime: MacroRuntime): MacroStatus = {
    val wasDelayed  = isDelayed(expandee)
    val undetparams = calculateUndetparams(expandee)
    val nowDelayed  = !typer.context.macrosEnabled || undetparams.nonEmpty

    (wasDelayed, nowDelayed) match {
      case (true, true) => Delay(expandee)
      case (true, false) => Skip(macroExpandAll(typer, expandee))
      case (false, true) =>
        macroLogLite("macro expansion is delayed: %s".format(expandee))
        delayed += expandee -> undetparams
        expandee updateAttachment MacroRuntimeAttachment(delayed = true, typerContext = typer.context, macroContext = Some(macroArgs(typer, expandee).c))
        Delay(expandee)
      case (false, false) =>
        import typer.TyperErrorGen._
        macroLogLite("performing macro expansion %s at %s".format(expandee, expandee.pos))
        val args = macroArgs(typer, expandee)
        try {
          val numErrors    = reporter.ERROR.count
          def hasNewErrors = reporter.ERROR.count > numErrors
          val expanded = { pushMacroContext(args.c); runtime(args) }
          if (hasNewErrors) MacroGeneratedTypeError(expandee)
          def validateResultingTree(expanded: Tree) = {
            macroLogVerbose("original:")
            macroLogLite("" + expanded + "\n" + showRaw(expanded))
            val freeSyms = expanded.freeTerms ++ expanded.freeTypes
            freeSyms foreach (sym => MacroFreeSymbolError(expandee, sym))
            Success(atPos(enclosingMacroPosition.focus)(expanded))
          }
          expanded match {
            case expanded: Expr[_] if expandee.symbol.isTermMacro => validateResultingTree(expanded.tree)
            case expanded: Tree if expandee.symbol.isTypeMacro => validateResultingTree(expanded)
            case _ => MacroExpansionHasInvalidTypeError(expandee, expanded)
          }
        } catch {
          case ex: Throwable =>
            popMacroContext()
            val realex = ReflectionUtils.unwrapThrowable(ex)
            realex match {
              case ex: AbortMacroException => MacroGeneratedAbort(expandee, ex)
              case ex: ControlThrowable => throw ex
              case ex: TypeError => MacroGeneratedTypeError(expandee, ex)
              case _ => MacroGeneratedException(expandee, realex)
            }
        } finally {
          expandee.removeAttachment[MacroRuntimeAttachment]
        }
    }
  }

  /** Expands a macro when a runtime (i.e. the macro implementation) cannot be loaded
   *  Meant for internal use within the macro infrastructure, don't use it elsewhere.
   */
  private def macroExpandWithoutRuntime(typer: Typer, expandee: Tree): MacroStatus = {
    import typer.TyperErrorGen._
    val fallbackSym = expandee.symbol.nextOverriddenSymbol orElse MacroImplementationNotFoundError(expandee)
    macroTraceLite("falling back to: ")(fallbackSym)

    def mkFallbackTree(tree: Tree): Tree = {
      tree match {
        case Select(qual, name) => Select(qual, name) setPos tree.pos setSymbol fallbackSym
        case Apply(fn, args) => Apply(mkFallbackTree(fn), args) setPos tree.pos
        case TypeApply(fn, args) => TypeApply(mkFallbackTree(fn), args) setPos tree.pos
      }
    }
    Fallback(mkFallbackTree(expandee))
  }

  /** Without any restrictions on macro expansion, macro applications will expand at will,
   *  and when type inference is involved, expansions will end up using yet uninferred type params.
   *
   *  For some macros this might be ok (thanks to TreeTypeSubstituter that replaces
   *  the occurrences of undetparams with their inferred values), but in general case this won't work.
   *  E.g. for reification simple substitution is not enough - we actually need to re-reify inferred types.
   *
   *  Luckily, there exists a very simple way to fix the problem: delay macro expansion until everything is inferred.
   *  Here are the exact rules. Macro application gets delayed if any of its subtrees contain:
   *    1) type vars (tpe.isInstanceOf[TypeVar]) // [Eugene] this check is disabled right now, because TypeVars seem to be created from undetparams anyways
   *    2) undetparams (sym.isTypeParameter && !sym.isSkolem)
   */
  var hasPendingMacroExpansions = false
  private val delayed = perRunCaches.newWeakMap[Tree, scala.collection.mutable.Set[Int]]
  private def isDelayed(expandee: Tree) = delayed contains expandee
  private def calculateUndetparams(expandee: Tree): scala.collection.mutable.Set[Int] =
    delayed.get(expandee).getOrElse {
      val calculated = scala.collection.mutable.Set[Symbol]()
      expandee foreach (sub => {
        def traverse(sym: Symbol) = if (sym != null && (undetparams contains sym.id)) calculated += sym
        if (sub.symbol != null) traverse(sub.symbol)
        if (sub.tpe != null) sub.tpe foreach (sub => traverse(sub.typeSymbol))
      })
      macroLogVerbose("calculateUndetparams: %s".format(calculated))
      calculated map (_.id)
    }
  private val undetparams = perRunCaches.newSet[Int]
  def notifyUndetparamsAdded(newUndets: List[Symbol]): Unit = {
    undetparams ++= newUndets map (_.id)
    if (macroDebugVerbose) newUndets foreach (sym => println("undetParam added: %s".format(sym)))
  }
  def notifyUndetparamsInferred(undetNoMore: List[Symbol], inferreds: List[Type]): Unit = {
    undetparams --= undetNoMore map (_.id)
    if (macroDebugVerbose) (undetNoMore zip inferreds) foreach { case (sym, tpe) => println("undetParam inferred: %s as %s".format(sym, tpe))}
    if (!delayed.isEmpty)
      delayed.toList foreach {
        case (expandee, undetparams) if !undetparams.isEmpty =>
          undetparams --= undetNoMore map (_.id)
          if (undetparams.isEmpty) {
            hasPendingMacroExpansions = true
            macroTraceVerbose("macro expansion is pending: ")(expandee)
          }
        case _ =>
          // do nothing
      }
  }

  /** Performs macro expansion on all subtrees of a given tree.
   *  Innermost macros are expanded first, outermost macros are expanded last.
   *  See the documentation for `macroExpand` for more information.
   */
  def macroExpandAll(typer: Typer, expandee: Tree): Tree =
    new Transformer {
      override def transform(tree: Tree) = super.transform(tree match {
        // todo. expansion should work from the inside out
        case tree if (delayed contains tree) && calculateUndetparams(tree).isEmpty && !tree.isErroneous =>
          val context = tree.attachments.get[MacroRuntimeAttachment].get.typerContext
          delayed -= tree
          context.implicitsEnabled = typer.context.implicitsEnabled
          context.enrichmentEnabled = typer.context.enrichmentEnabled
          context.macrosEnabled = typer.context.macrosEnabled
          macroExpandApply(newTyper(context), tree, EXPRmode, WildcardType)
        case _ =>
          tree
      })
    }.transform(expandee)
}

object MacrosStats {
  import scala.reflect.internal.TypesStats.typerNanos
  val macroExpandCount    = Statistics.newCounter ("#macro expansions", "typer")
  val macroExpandNanos    = Statistics.newSubTimer("time spent in macroExpand", typerNanos)
}
