(** Error and warning utilities **)

open Pp

let error message =
  CErrors.errorlabstrm "Coqine" message

let not_supported feature =
  error (str feature ++ str " not supported")

let warning message =
  Feedback.msg_warning message
