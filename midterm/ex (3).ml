(* Recursion on numbers *)

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

let rec repeat (e:'a) (n:int) : 'a list =
  match n with
  | 0 -> []
  | m -> e :: repeat e (m-1)

let rec repeat' : 'a -> int -> 'a list =
  fun e n -> 
  match n with
  | 0 -> []
  | m -> e :: repeat' e (m-1)


(* Recursion on lists *)

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

let rec mem e l =
  match l with
  | [] -> false
  | h::t -> (e=h) || mem e t
         
let rec has_duplicates l =
  match l with
  | [] -> false
  | h::t -> (mem h t) || has_duplicates t
   
(** [last l] returns the last element in [l].
    It fails if [l] is empty *)
let rec last l =
  match l with
  | [] -> failwith "last: empty list"
  | [h] -> h
  | h1::h2::t -> last (h2::t)
          

let rec rev l =
  failwith "implement"

let rec concat l1 l2 =
  failwith "implement"

(** [take n l] returns a list with the first [n] elements of [l].
    Eg. take 0 [1;2;3] => []
    Eg. take 1 [1;2;3] => [1]
    Eg. take 100 [1;2;3] =>[1;2;3] *)
let rec take n l =
  failwith  "implement"

(** [drop n l] drops the first [n] elements of [l].
    Eg. drop 0 [1;2;3] => [1;2;3]
    Eg. drop 1 [1;2;3] => [2;3]
    Eg. drop 100 [1;2;3] => [] *)
let rec drop n l =
  failwith "implement"

(** [rad l] removes adjacent duplicates in [l]
    Eg. rad [1;2;2;2;3;3;2] => [1;2;3;2] *)
let rec rad l =
  failwith "implement"

(* Useful higher-order function schemes *)

(* motivating and defining map *)
let succ i = i+1
let upper c = Char.uppercase_ascii c
let isz i = i=0
            
let rec succl : int list -> int list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> succ h :: succl t

let rec upperl : char list -> char list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> upper h :: upperl t
              
let rec is_zerol : int list -> bool list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> isz h :: is_zerol t
          
let rec map : ('a -> 'b) -> 'a list -> 'b list =
  fun f l ->
  match l with
  | [] -> []
  | h::t -> f h :: map f t
              
let succl' l = map succ l
let upperl' l = map upper l
let is_zerol' l = map isz l

(* motivating and defining filter *)
    
let is_pos i = i>0
let is_upper c = c = Char.uppercase_ascii c
let is_ne l = l<>[]
                 
let rec fgtz : int list -> int list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if is_pos h
    then h:: fgtz t
    else fgtz t

let rec fu : char list -> char list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if is_upper h
    then h :: fu t
    else fu t

let rec fne : 'a list list -> 'a list list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if is_ne h
    then h :: fne t
    else fne t


let rec filter : ('a -> bool) -> 'a list -> 'a list =
  fun p l ->
  match l with
  | [] -> []
  | h::t ->
    if p h
    then h :: filter p t
    else filter p t
                 
let fgtz' l = filter is_pos l
let fu' l = filter is_upper l
let fne' l = filter is_ne l 


let rec suml  : int list -> int =
  fun l ->
  match l with
  | [] -> 0
  | h::t -> h + suml t

let rec andl : bool list -> bool =
  fun l ->
  match l with
  | [] -> true
  | h::t -> h && andl t

let rec concat : 'a list list -> 'a list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> h @ concat t 
     
let rec foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b =
  fun f a l  ->
  match l with
  | [] -> a
  | h::t -> f h (foldr f a t)

let suml' l = foldr (+) 0 l 
let andl' l = foldr (&&) true l
let concat' l = foldr (@) [] l

(* 
foldr f a [x;y;z]
=>
f x (f y (f z a))
*)

(* foldl f a [x;y;z]
=>
   f (f (f a x) y) z
*)

let rec foldl f a l =
  match l with
  | [] -> a
  | h::t -> foldl f (f a h) t

(* Variant types 
   Aka algebraic data types 
*)

type dow = Mon | Tue | Wed | Thu | Fri | Sat | Sun

let next d =
  match d with
  | Mon -> Tue
  | Tue -> Wed
  | Wed -> Thu
  | Thu -> Fri
  | Fri -> Sat
  | Sat -> Sun
  | Sun -> Mon

let is_weekend d =
  match d with
  | Sat | Sun -> true
  | _ -> false
  
type flavor = Cho |  Van | Str
type ic = Cone of flavor | Cup of (flavor*flavor) | Bucket of flavor list

let ic1 = Cone(Cho)
let ic2 = Cup(Cho,Str)
let ic3 = Bucket [Van;Str;Van]

let cost ic =
  match ic with
  | Cone(_) -> 1
  | Cup(_,_) -> 2
  | Bucket(_) -> 5

let has_same_flavor ic fl =
  match ic with
  | Cone(fl1) when fl1=fl -> true
  | Cup(fl1,fl2) when fl1=fl && fl2=fl -> true
  | Bucket l -> List.for_all ((=)fl) l
  | _ -> false
  
let is_boring ic =
   has_same_flavor ic Van || has_same_flavor ic Cho

type 'a option = None | Ok of 'a
                 
let rec find k d =
  match d with
  | [] -> None
  | (k',v)::t ->
    if k=k'
    then Ok v
    else find k t 

type ('a,'b) either = Left of 'a | Right of 'b
        
(* Recursive variant types *)

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt

(*

    33
   /  \
  12  77
     /  \
    44  102
*)

let t1: int bt =
  Node(33,
       Node(12,Empty,Empty),
       Node(77,
            Node(44,Empty,Empty),
            Node(102,Empty,Empty)))

(** [size t] returns the number of nodes in tree [t]. *)
let rec size t =
  match t with
  |Empty -> 0
  |Node(node,left,right )->
    1 + size left + size right


(** [sum t] calculates the sum of all values stored in the nodes of tree [t]. *)
let rec sum t =
  match t with
  |Empty -> 0
  |Node (node, left, right) ->
    node + sum left + sum right

(** [mirror t] produces a mirror image of tree [t], swapping left and right subtrees at every node. *)
let rec mirror t =
  match t with
  |Empty -> Empty
  |Node(node, lf, rt)->
    Node(node, mirror rt, mirror lf)


(** [map f t] applies function [f] to each value in the nodes of tree [t], producing a new tree with the results. *)
let rec map f t =
  match t with
  |Empty -> Empty
  |Node(node, lf, right)->
    Node(f node, map f lf, map f right)


(** [no_of_leaves t] counts the number of leaf nodes (nodes with no children) in tree [t]. *)
let rec no_of_leaves t =
  match t with 
  |Empty -> 0
  |Node(node, Empty, Empty) -> 1
  |Node(node, lf,rt)->
    no_of_leaves lf + no_of_leaves rt

(** [pre t] generates a list of node values in preorder traversal of tree [t]. *)
let rec pre t =
  

(** [ino t] generates a list of node values in inorder traversal of tree [t]. *)
let rec ino t =
  failwith "implement"

(** [pos t] generates a list of node values in postorder traversal of tree [t]. *)
let rec pos t =
  failwith "implement"


(*

    33
   /  \
  12  77
     /  \
    44  102
*)
