open Utils

type time_rec = (int * int)
type time_rec_list = time_rec list

let getTime (td: time_rec) = fst td
let getRec (td: time_rec) = snd td

let filter_rec r = fun x -> (snd x) > r

let get_best (td: time_rec) =
  let rec aux h t r acc =
    if h = t then acc
    else
      let res = h * (t - h) in
      let acc = if res > r then acc + 1 else acc in
      aux (h + 1) t r acc
  in aux 0 (getTime td) (getRec td) 0


let parse s: time_rec_list =
  let s = removeLetterInStr s in
  let s = removeSymbolInStr s in
  let l = inputLinesToStrList s in
  let l = List.map strIntListToIntList l in
  twoByTwo (getFirst l) (getLast l)

let timeRecListToTimeRec (trl: time_rec_list): time_rec =
  let t = concatInt (List.map getTime trl) in
  let r = concatInt (List.map getRec trl) in
  (t, r)

let res_p1 (trl: time_rec_list) =
  mulList (List.map get_best trl)

let res_p2 (tr: time_rec) =
  get_best tr

let p1 input =
  input |> parse |> res_p1

let p2 input =
  input |> parse |> timeRecListToTimeRec |> res_p2