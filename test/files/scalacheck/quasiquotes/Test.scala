import org.scalacheck._

object Test extends Properties("quasiquotes") {
  include(ConstructionProps)
  include(DeconstructionProps)
}