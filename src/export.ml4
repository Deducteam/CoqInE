let export qid filename =
  ()

VERNAC COMMAND EXTEND Coqine
| [ "Dedukti" "Export" global(qid) string(filename) ] -> [ export qid filename ]
END

