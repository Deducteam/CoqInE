Definition x := Prop.

Module F.
  Module G. Definition y := x. End G.
End F.

Module H := F.

Definition z := H.G.y.
