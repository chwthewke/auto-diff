# Diff resolution hierarchy

Diff instances 

- Diff (A -> Difference)
  - DefaultDiff
    - Eq/Show
    - LabelledGeneric
      - Fields Hlist

Goals

- Automatic derivation for:
  - types with Show/Eq
  - case classes
  - sealed trait hierarchies
  - enumeratum Enums
  - collections w/ in- or out-of-order matching
