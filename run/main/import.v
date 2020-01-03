Module Type SIG.    Parameter  T :  Set. Parameter  x :  T.    End SIG.
Module M.           Definition T := nat.   Definition x := 0.    End M.
Module N : SIG with Definition T := nat := M.

Module Two (X Y: SIG).
  Definition T := (X.T * Y.T)%type.
  Definition x := (X.x, Y.x).
End Two.
Module Q := Two M N.
