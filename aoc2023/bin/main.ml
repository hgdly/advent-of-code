let usage = "usage: aoc2023 -j <jour> -d <données>";;

let input_file = ref "";;
let day = ref 0;;
let anon_fun filename = input_file := filename;;

let read_whole_input_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let speclist = [
  ("-j", Arg.Set_int day, "day");
  ("-d", Arg.Set_string input_file, "données");
];;

let () =
  let () = Arg.parse speclist anon_fun usage in
  let () = if !input_file = "" then (Printf.printf "Données manquantes\n"; exit 1) in
  let () = if !day = 0 then (Printf.printf "Jour invalide\n"; exit 1) in
  let (p1, p2) = match !day with
    | 1 -> Aoc2023.Day_1.(p1, p2)
    | 2 -> Aoc2023.Day_2.(p1, p2)
    | 3 -> Aoc2023.Day_3.(p1, p2)
    | 4 -> Aoc2023.Day_4.(p1, p2)
    | 5 -> Aoc2023.Day_5.(p1, p2)
    | 6 -> Aoc2023.Day_6.(p1, p2)
    | _ -> raise (Invalid_argument "Jour invalide") in
  let input = read_whole_input_file !input_file in
  let () = Printf.printf "%d " !day in
  let () = Printf.printf "%d " (p1 input) in
  let () = Printf.printf "%d\n" (p2 input) in
  ();;
