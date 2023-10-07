let ascii = true

open Js_of_ocaml

let measure f n =
  let t = Unix.gettimeofday () in
  f n;
  Unix.gettimeofday () -. t

let rec iteration_counts f n0 =
  let t0 = measure f n0 in
  if t0 < 0.01 then iteration_counts f (2 * n0) else truncate (float n0 /. t0)

let measure f =
  let n = iteration_counts f 100 in
  let t = measure f n in
  t /. float n

let res = ref (Js.string "")

let init1 l =
  if ascii
  then String.init l (fun _ -> Char.chr (Random.int 127))
  else "é" ^ String.init (max 0 (l - 2)) (fun _ -> Char.chr (Random.int 127))

let perform1 s n =
  for i = 0 to n do
    res := Js.string s
  done

let res = ref ""

let init2 l =
  let s = init1 l in
  let s' = Js.string s in
  assert (Js.to_string s' = s);
  s'

let perform2 s n =
  for i = 0 to n do
    res := Js.to_string s
  done

let bench init perform =
  for i = 0 to 20 * 4 do
    let l = truncate (2. ** (float i /. 4.)) in
    let s = init l in
    Format.printf "%d %g@." l (measure (fun n -> perform s n))
  done

let () =
  match Sys.argv.(1) with
  | "to_js" -> bench init1 perform1
  | "from_js" -> bench init2 perform2
  | _ -> assert false

(*
let () = for i= 0 to 30000 do res := (Js.string s) done
buffer: 4.3s
stringref: 2.0s

buffer: 2s
stringref: 0.386

buffer: 4.9
stringref: 2.3
*)

(*
let s' = Js.string s
let () = assert (s = Js.to_string s')

let res = ref ""

let () = for i = 0 to 30000 do res:= (Js.to_string s') done
buffer: 2.2s
stringref: 1.7s

buffer: 2.5s
stringref: 3.8s

buffer: 1s
stringref: 1.6s
*)
