package mcmc

import monocle.{Iso, Lens}
import monocle.function.{At, Wrapped}
import monocle.function.all.wrapped
import shapeless.tag
import shapeless.tag.@@

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.language.implicitConversions

object implicits {

  implicit def untaggedLens[A, B, T](lens: Lens[A, B @@ T]): Lens[A, B] = lens ^<-> wrapped

  implicit def tagWrapped[A, T]: Wrapped[A @@ T, A] = new Wrapped[A @@ T, A] {
    val wrapped: Iso[@@[A, T], A] = Iso[@@[A, T], A](identity)(tag.apply[T].apply[A])
  }

  implicit def seqAt[A, S <: SeqLike[A, S]](implicit bf: CanBuildFrom[S, A, S]): At[S, Int, A] = (i: Int) =>
    Lens[S, A](_(i))(x => _.updated[A, S](i, x))

}
