(** Error and warning utilities **)

open Pp

let error message =
  raise (CErrors.make_anomaly ?label:(Some "Coqine") message)

let not_supported feature =
  error (str feature ++ str " not supported")

let warning message =
  Feedback.msg_warning message
