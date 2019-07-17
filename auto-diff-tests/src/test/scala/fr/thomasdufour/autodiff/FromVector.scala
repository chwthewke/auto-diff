package fr.thomasdufour.autodiff

import cats.data.Chain
import cats.data.NonEmptyVector
import com.github.ghik.silencer.silent
import scala.reflect.ClassTag

trait FromVector[A, C[_]] extends FromNonEmptyVector[A, C] {
  def fromVector( vec: Vector[A] ): C[A]

  override def fromNev( nel: NonEmptyVector[A] ): C[A] = fromVector( nel.toVector )
}

object FromVector extends FromVectorVersionSpecific {
  implicit def chainFromVector[A]: FromVector[A, Chain] =
    new FromVector[A, Chain] {
      override def fromVector( vec: Vector[A] ): Chain[A] = Chain.fromSeq( vec )
    }

  implicit def listFromVector[A]: FromVector[A, List] =
    new FromVector[A, List] {
      override def fromVector( vec: Vector[A] ): List[A] = vec.toList
    }

  @silent( "deprecated" )
  implicit def streamFromVector[A]: FromVector[A, Stream] =
    new FromVector[A, Stream] {
      override def fromVector( vec: Vector[A] ): Stream[A] = vec.toStream
    }

  implicit def vectorFromVector[A]: FromVector[A, Vector] =
    new FromVector[A, Vector] {
      override def fromVector( vec: Vector[A] ): Vector[A] = vec
    }

  implicit def iterableFromVector[A]: FromVector[A, Iterable] =
    new FromVector[A, Iterable] {
      override def fromVector( vec: Vector[A] ): Iterable[A] = vec
    }

  implicit def arrayFromVector[A: ClassTag]: FromVector[A, Array] =
    new FromVector[A, Array] {
      override def fromVector( vec: Vector[A] ): Array[A] = vec.toArray
    }
}
