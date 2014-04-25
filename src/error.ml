(** Error and warning utilities **)

open Pp

let error message =
  Util.errorlabstrm "Coqine" message

let not_supported feature =
  error (str feature ++ str " not supported")

let warning message =
  Pp.warn message

