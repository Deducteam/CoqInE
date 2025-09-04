(** Error and warning utilities **)

open Pp

let error message =
  raise (CErrors.make_anomaly ?label:(Some "Coqine") message)

let not_supported feature =
  error (str feature ++ str " not supported")

let warn msg =
  Debug.debug "Warning: %s" msg;
  Feedback.msg_warning (str msg)

let warning fmt = Format.kasprintf warn fmt
