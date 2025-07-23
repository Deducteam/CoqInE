Require Coqine.

Set Printing Universes.

Dedukti Enable Debug.
Dedukti Enable Verbose.

Dedukti Set Destination "out".
Dedukti Set Debug "debug.out".
Dedukti Debug Lib "Top.import".

Require Import import.

Load config.

Dedukti Export All.
