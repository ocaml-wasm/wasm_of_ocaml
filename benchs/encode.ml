open Js_of_ocaml

let a = String.make 65536 (Char.chr 97)

let s = ref a

let () =
let a = Js.string a in
let t = Unix.gettimeofday () in
for i = 1 to 15000 do
  s:= (Js.to_string a)
done;
Firebug.console##log_2 (Js.string "ASCII") (Js.float (Unix.gettimeofday () -. t))


let () =
let a = Js.string ("\195\169" ^ String.sub a 2 (String.length a - 2)) in
let t = Unix.gettimeofday () in
for i = 1 to 15000 do
  s:= (Js.to_string a)
done;
Firebug.console##log_2 (Js.string "Latin1") (Js.float (Unix.gettimeofday () -. t))

let () =
let a = Js.string ("\229\133\182" ^ String.sub a 3 (String.length a - 3)) in
let t = Unix.gettimeofday () in
for i = 1 to 15000 do
  s := (Js.to_string a)
done;
Firebug.console##log_2 (Js.string "Unicode") (Js.float (Unix.gettimeofday () -. t))
