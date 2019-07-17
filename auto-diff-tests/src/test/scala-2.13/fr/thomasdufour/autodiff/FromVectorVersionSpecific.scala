package fr.thomasdufour.autodiff

import scala.collection.immutable.ArraySeq
import scala.collection.immutable.Queue
import scala.reflect.ClassTag

trait FromVectorVersionSpecific {

  implicit def queueFromVector[A]: FromVector[A, Queue] =
    new FromVector[A, Queue] {
      override def fromVector( vec: Vector[A] ): Queue[A] = vec.to( Queue )
    }

  implicit def lazyListFromVector[A]: FromVector[A, LazyList] =
    new FromVector[A, LazyList] {
      override def fromVector( vec: Vector[A] ): LazyList[A] = vec.to( LazyList )
    }

  implicit def arraySeqFromVector[A]( implicit C: ClassTag[A] ): FromVector[A, ArraySeq] =
    new FromVector[A, ArraySeq] {
      override def fromVector( vec: Vector[A] ): ArraySeq[A] = ArraySeq.from( vec )
    }

}
