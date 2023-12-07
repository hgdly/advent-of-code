let getFirst (l : 'a list) : 'a =
  match l with
  | [] -> raise (Failure "getFirst")
  | h :: _ -> h
;;

let rec getLast (l : 'a list) : 'a =
  match l with
  | [] -> raise (Failure "getLast")
  | [h] -> h
  | _h :: t -> getLast t
;;

let isDigit (c : char) : bool =
  let code = Char.code c in
  code >= 48 && code <= 57
;;

let isLetter (c : char) : bool =
  let code = Char.code c in
  (code >= 65 && code <= 90) || (code >= 97 && code <= 122)
;;

let isAlpha (c : char) : bool =
  isDigit c || isLetter c
;;

let isSymbol (c : char) : bool =
  (c = '+' || c = '-' || c = '*' || c = '/' || c = '%' || c = '^' || c = '!' || c = '=' || c = '<' || c = '>' || c = '&' || c = '|' || c = '~' || c = '$' || c = '@' || c = '?' || c = ':' || c = '.' || c = ',' || c = ';' || c = '(' || c = ')' || c = '[' || c = ']' || c = '{' || c = '}' || c = '"' || c = '\'')

let charToInt (c : char) : int =
  Char.code c - 48
;;

let stringToInt = int_of_string;;

let stringToChar (s : string) : char =
  if String.length s <> 1 then
    raise (Failure "stringToChar")
  else
    String.get s 0
  ;;

let explode (s : string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []
;;

let implode (l : char list) : string =
  String.concat "" (List.map (String.make 1) l)
;;

let explodeInt (n: int): int list =
  let rec aux (n: int) (l: int list): int list =
    if n < 10 then
      n :: l
    else
      aux (n / 10) ((n mod 10) :: l)
  in aux n []  

let implodeDigits (l : int list) : int =
  List.fold_left (fun acc x -> acc * 10 + x) 0 l
;;

let concatInt (l : int list) : int = List.map explodeInt l |> List.flatten |> implodeDigits;;

let inputLinesToStrList (input : string) : string list =
  let lines = String.split_on_char '\n' input in
  List.filter (fun s -> s <> "") lines 
;;

let getDigitsInt (s : string) : int list =
  List.fold_right (fun c acc ->
    if isDigit c then
      charToInt c :: acc
    else
      acc
  ) (explode s) []
;;

let startWith (s : string) (prefix : string) : bool =
  let prefixLen = String.length prefix in
  if String.length s < prefixLen then
    false
  else
    let sPrefix = String.sub s 0 prefixLen in
    sPrefix = prefix
;;

let truncateStrPrefix (s : string) (prefix : string) : string =
  let prefixLen = String.length prefix in
  if String.length s < prefixLen then
    raise (Failure "truncate")
  else
    String.sub s prefixLen (String.length s - prefixLen)
;;

(* remove len char of string *)
let truncateStrN (s: string) (len : int) : string =
  if String.length s < len then
    raise (Failure "truncate")
  else
    String.sub s len (String.length s - len)
;;

let getFirstStrN (s: string) (len : int) : string =
  if String.length s < len then
    raise (Failure "getFirstStr")
  else
    String.sub s 0 len

let splitStrChar (s : string) (c : char) : string list = String.split_on_char c s;;

let removeWhitespaces (s : string) : string =
  let l = explode s in
  let rec aux (l : char list) : char list =
    match l with
    | [] -> []
    | h :: t ->
      if h = ' ' || h = '\t' || h = '\n' then
        aux t
      else
        h :: aux t
  in implode (aux l)
;;

let removeDigits (s : string) : string =
  let l = explode s in
  let rec aux (l : char list) : char list =
    match l with
    | [] -> []
    | h :: t ->
      if isDigit h then
        aux t
      else
        h :: aux t
  in implode (aux l)
;;

let removeEmptyStrList (l : string list) : string list =
  List.filter (fun s -> s <> "") l

let strIntListToIntList (s : string) : int list =
  let l = splitStrChar s ' ' in
  let l = removeEmptyStrList l in
  List.map stringToInt l

let intersection (l1 : 'a list) (l2 : 'a list) : 'a list =
  List.filter (fun x -> List.mem x l2) l1

let twoByTwo (l1 : 'a list) (l2 : 'a list) : ('a * 'a) list =
  List.map2 (fun x y -> (x, y)) l1 l2

let removeLetterInStr (s: string) : string =
  let l = explode s in
  let rec aux (l : char list) : char list =
    match l with
    | [] -> []
    | h :: t ->
      if isLetter h then
        aux t
      else
        h :: aux t
  in implode (aux l)

let removeSymbolInStr (s: string) : string =
  let l = explode s in
  let rec aux (l : char list) : char list =
    match l with
    | [] -> []
    | h :: t ->
      if isSymbol h then
        aux t
      else
        h :: aux t
  in implode (aux l)

let rec pow (x : int) (n : int) : int =
  if n = 0 then
    1
  else
    x * pow x (n - 1)

let sumList (l: int list): int =
  List.fold_left (+) 0 l

let mulList (l: int list): int =
  List.fold_left ( * ) 1 l

let reverse = List.rev

let list0N (n: int): int list =
  let rec aux i =
    if i <= n then i :: aux (i + 1) else []
  in aux 0

let listN0 (n: int): int list = reverse (list0N n)

let printIntList (l: int list) = List.iter (fun x -> print_int x; print_string " ") l
