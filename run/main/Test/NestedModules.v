Module A.

Definition x := Prop.

Definition y := Set.

Module B.

Definition x := Set.

Definition y := Prop.

End B.

Definition z := B.x.

End A.

Definition x := A.x.

Definition y := A.B.y.

Definition z := A.z.

