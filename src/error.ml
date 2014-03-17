(** Error and warning utilities **)

let not_supported feature =
  failwith (Printf.sprintf "%s not supported" feature)

let error message =
  Util.errorlabstrm "Yolk" message

