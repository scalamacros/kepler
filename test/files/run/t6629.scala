object Test extends App {

import scala.reflect.runtime.{universe=>ru}
println(ru.reify{type X = List[T] forSome {type T}})

}