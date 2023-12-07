open Utils

type card = {id: int; wNum: int list; cNum: int list}

let compute c = 
  let l = intersection c.wNum c.cNum in
  let n = List.length l in
  if n = 0 then 0
  else pow 2 (n - 1)

let parse s =
  let s = truncateStrPrefix s "Game " in
  let l = splitStrChar s (stringToChar ":") in
  let id = implodeDigits (getDigitsInt (getFirst l)) in
  let s = getLast l in
  let l = splitStrChar s (stringToChar "|") in
  {id = id; wNum = strIntListToIntList (getFirst l); cNum = strIntListToIntList (getLast l)}

let p1 input =
  input |> inputLinesToStrList |>  List.map parse |> List.map compute |> sumList

let p2 input =
  let _ = input in 0