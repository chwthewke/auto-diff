package fr.thomasdufour.autodiff

trait DiffOps {

  implicit class StringDiffExt( val self: String ) {
    def !==( other: String ): DifferenceTree = V( self, other )
  }

  implicit class DifferenceOps( val self: Option[Difference] ) {
    def tree: DifferenceTree = DifferenceTree.fromDifferenceOption( self )
  }

}

object DiffOps extends DiffOps
