package mcmc

import scala.language.implicitConversions
import monocle.{Iso, Lens}
import monocle.function.Wrapped
import monocle.function.all.wrapped
import shapeless.tag
import shapeless.tag.@@

object implicits {

  implicit def untaggedLens[A, B, T](lens: Lens[A, B @@ T]): Lens[A, B] = lens ^<-> wrapped

  implicit def tagWrapped[A, T]: Wrapped[A @@ T, A] = new Wrapped[A @@ T, A] {
    val wrapped: Iso[@@[A, T], A] = Iso[@@[A, T], A](identity)(tag.apply[T].apply[A])
  }

}
