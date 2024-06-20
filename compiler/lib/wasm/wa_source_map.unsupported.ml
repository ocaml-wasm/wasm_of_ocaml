type t

let fail () = failwith "Sourcemap support not available"

let load ?tmp_buf:_ _ = fail ()

let parse ?tmp_buf:_ _ = fail ()

let write _ _ = fail ()

let is_empty _ = fail ()

type resize_data =
  { mutable i : int
  ; mutable pos : int array
  ; mutable delta : int array
  }

let resize _ _ = fail ()

let concatenate _ = fail ()

let iter_sources _ _ = fail ()

let insert_source_contents ~rewrite_path:_ _ _ = fail ()
