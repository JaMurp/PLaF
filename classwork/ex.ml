

(* Examples of recursion on numbers *)

let rec fact n =
  match n with
  | 0 -> 1
  | m when m>0 -> m * fact (m-1)
  | _ -> failwith "fact: negative input"

(** [fact' n] returns the factorial of [n].
    Precondition: [n] is positive *) 
let rec fact' n =
  match n with
  | 0 -> 1
  | m -> m * fact' (m-1)

let rec mys (e:'a) (n:int) : 'a list =
  match n with
  | 0 -> []
  | m -> e :: mys e (m-1)

let rec mys' : 'a -> int -> 'a list =
  fun e n ->
  match n with
  | 0 -> []
  | m -> e :: mys' e (m-1)

(* Examples of recursion on lists *)

let rec size : 'a list -> int =
  fun l ->
  match l with
  | [] -> 0
  | h::t -> 1 + size t

let rec sum : int list -> int =
  fun l ->
  match l with
  | [] -> 0
  | h::t -> h + sum t

(* Exercises on lists *)
              
let rec mem e l =
  match l with
  | [] -> false
  | h::t -> h=e || mem e t





let rec has_duplicates l =
  match l with
  | [] -> false
  | h::t -> mem h t || has_duplicates t

(** [last l] removes the last element of [l].
    It fails if [l] is empty *)
let rec last l =
  failwith "implement"

let rec rev l =
  failwith "implement"

let rec concat l1 l2 =
  failwith "implement"

(** [take n l] returns a list with the first [n] elements of [l].
     Eg. take 0 l => []
     Eg. take 1 [1;2;3] => [1]
     Eg. take 100 [1;2;3] => [1;2;3]
*)
let rec take n l =
  failwith "implement"

let rec drop n l =
  failwith "implement"

(** [rad l] returns a list where adjacent duplicates have been removed *)
let rec rad l =
  failwith "implement"


