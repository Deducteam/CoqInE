
(*   Entry point script for the Dedukti plugin.   *)

Declare ML Module "coqine_plugin".

(* Useful for more informative debug messages *)
Set Printing Universes.

(* Output folder *)
Dedukti Set Destination "out".

(* Enables debugging in the debug.out file *)
Dedukti Enable Debug.
Dedukti Set Debug "debug.out".

Dedukti Add Debug "Coq.Init.Datatype".

(* Import some modules to translate *)
Require Import Test.Fixpoints.

(* Import config file setting the encoding *)
Load config.

(* Exporting all imported modules *)
Dedukti Export All.
