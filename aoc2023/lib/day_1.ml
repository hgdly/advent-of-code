open Utils;;

let rec getDigitsStrInt s =
  if s = "" then []
  else let s = String.lowercase_ascii s in
  let nb = 
    if isDigit (getFirst (explode s)) then
      charToInt (getFirst (explode s))
    else if startWith s "one" then 1
    else if startWith s "two" then 2
    else if startWith s "three" then 3
    else if startWith s "four" then 4
    else if startWith s "five" then 5
    else if startWith s "six" then 6
    else if startWith s "seven" then 7
    else if startWith s "eight" then 8
    else if startWith s "nine" then 9
    else 0
  in (if nb = 0 then [] else [nb]) @ getDigitsStrInt (truncateStrN s 1)
;;

let res s f =
  let intsList = List.fold_left (fun acc x -> let digits = f x in if List.length digits = 0 then acc else [getFirst digits; getLast digits] :: acc) [] s in
  let implodeDigitsList = List.fold_left (fun acc x -> implodeDigits x :: acc) [] intsList in
  List.fold_left (fun acc x -> acc + x) 0 implodeDigitsList

let res_p1 s = res s getDigitsInt;;
;;

let res_p2 s = res s getDigitsStrInt;;

let p1 input =
  input |> inputLinesToStrList |> res_p1;;
;;
let p2 input =
  input |> inputLinesToStrList |> res_p2;;
;;
