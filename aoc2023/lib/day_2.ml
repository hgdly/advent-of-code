open Utils

type set = {red : int; green : int; blue : int}
type game = {id : int; sets : set list}

let zero_set = {red = 0; green = 0; blue = 0}
let onset f acc {red; green; blue} = {red = f acc.red red; green = f acc.green green; blue = f acc.blue blue}
let maxset = List.fold_left (onset max) zero_set

let maxR = 12
let maxG = 13
let maxB = 14

let parseSet l =
  let set_temp = {red = 0; green = 0; blue = 0} in
  let rec aux l acc = match l with
  | h :: t -> let c = implodeDigits (getDigitsInt h) in
              let acc = 
                match stringToChar (getFirstStrN (removeDigits h) 1) with
                | 'r' -> {acc with red = c}
                | 'g' -> {acc with green = c}
                | 'b' -> {acc with blue = c}
                | _ -> acc
              in aux t acc
  | [] -> acc
  in aux l set_temp

let parseSets s =
  let s = removeWhitespaces s in
  let l = splitStrChar s (stringToChar ";") in
  let l = List.fold_left (fun acc x -> [splitStrChar x (stringToChar ",")] @ acc) [] l in
  List.map parseSet l

let parse s =
  let s = truncateStrPrefix s "Game " in
  let l = splitStrChar s (stringToChar ":") in
  let id = implodeDigits (getDigitsInt (getFirst l)) in
  let s = getLast l in
  let sets = parseSets s in
  {id = id; sets = sets}

let isGameValid g =
  let s = maxset g.sets in s.red <= maxR && s.green <= maxG && s.blue <= maxB

let powerSet s_l =
  let s = maxset s_l in
  s.red * s.green * s.blue

let res l f =
  let games = List.map parse l in
  List.fold_left f 0 games

let res_p1 l = res l (fun acc x -> if isGameValid x then acc + x.id else acc)

let res_p2 l = res l (fun acc x -> acc + (powerSet x.sets))

let p1 input =
  input |> inputLinesToStrList |> res_p1

let p2 input = 
  input |> inputLinesToStrList |> res_p2
