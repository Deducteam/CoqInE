(* This script tests the Dedukti plugin by exporting test files and parts
  of the Coq standard library. *)

Require Coqine.

Dedukti Enable Debug.
Dedukti Set Debug "debug.out".
Dedukti Set Destination "out".

Dedukti Set Encoding "default".



Require Coq.Init.Notations
        Coq.Init.Logic
        Coq.Init.Datatypes
        Coq.Init.Wf
        Coq.Init.Logic_Type.

Require Debuglib.

Dedukti Export All.
