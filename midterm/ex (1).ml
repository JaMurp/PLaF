(* Examples of recursion on numbers *)

(** [fact n] returns the factorial of [n], handling negative input with a custom error. *)
let rec fact n =
  match n with
  | 0-> 1
  | m when m > 0->
    m * fact(m-1)
  |_ ->
    failwith"negative number"


(** [fact' n] returns the factorial of [n]. Precondition: [n] is positive. *)
let rec fact' n =
  match n with
  |0->1
  |m ->
    m*fact(m-1)

(** [mys e n] creates a list of length [n] where each element is [e]. *)
let rec mys (e:'a) (n:int) : 'a list =
  match n with
  |0-> []
  |m -> 
    e :: mys e (n-1)

(** [mys' e n] creates a list of length [n] where each element is [e]. *)
let rec mys' : 'a -> int -> 'a list =
  fun e n ->
  failwith "implement"

(* Examples of recursion on lists *)

(** [size l] returns the number of elements in list [l]. *)
let rec size : 'a list -> int =
  fun l ->
    match l with
    |[] -> 0
    |head :: tail ->
      1 + size(tail)

(** [sum l] returns the sum of all integer elements in list [l]. *)
let rec sum : int list -> int =
  fun l ->
    match l with
    |[]-> 0
    |head::tail ->
      head + sum(tail)

(* Exercises on lists *)
              
(** [mem e l] checks if element [e] is present in list [l]. *)
let rec mem e l =
  match l with
  |[]->false
  |head :: tail->
    if head=e then 
      true
    else
    mem e tail


(** [has_duplicates l] checks if list [l] contains any duplicate elements. *)
let rec has_duplicates l =
  match l with
  |[] -> false
  |h::t -> (mem h t) || has_duplicates t


(** [last l] returns the last element of list [l]. It fails if [l] is empty. *)
let rec last l =
  match l with
  |[] -> failwith"empty"
  |[x] -> x
  |head :: tail ->
    last tail

(** [rev l] returns a list that is the reverse of list [l]. *)
let rec rev l =
  match l with
  |[] -> []
  |head :: tail ->
    rev tail @ [head]


(** [concat l1 l2] concatenates lists [l1] and [l2]. *)
let concat l1 l2 =
  l1 @ l2

(** [take n l] returns a list with the first [n] elements of list [l]. *)
let rec take n l =
  match l,n with 
  |[],_ -> []
  |_,0-> []
  |head :: tail,m ->
    head :: take (m-1) tail

(** [drop n l] returns a list with the elements of list [l] after dropping the first [n] elements. *)
let rec drop n l =
  match n,l with 
  |_,[] -> []
  |0,l -> l
  |m, head:: tail ->
    drop (m-1) tail

(** [rad l] returns a list where adjacent duplicates in list [l] have been removed. *)
let rec rad l =
  match l with
  |[] -> []
  |[x] -> [x]
  |head :: mid :: tail->
    if head=mid then
      rad tail
    else
      head :: rad (mid::tail)
      
