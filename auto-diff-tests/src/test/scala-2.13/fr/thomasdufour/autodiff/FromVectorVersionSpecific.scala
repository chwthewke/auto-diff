package fr.thomasdufour.autodiff

import scala.collection.immutable.Queue

trait FromVectorVersionSpecific {

  implicit def queueFromVector[A]: FromVector[A, Queue] =
    new FromVector[A, Queue] {
      override def fromVector( vec: Vector[A] ): Queue[A] = vec.to( Queue )
    }

}
