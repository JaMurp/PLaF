(* Quiz 1 - 31 January 2024

   Student name 1: James Murphy
   Student name 2: Tyler Speedy
*)


(* Notes: 
    a. You may add helper functions.
    b. "let rec" allows your function to be recursive, but it doesn't
    have to be. 
*)

(* Sample Tree *)
let ex = [(12, 7); (12, 43); (7, 4); (43, 33); (43,77)]

(*
      12
      /\ 
     /  \  
    7   43
   /    /\ 
  /    /  \  
 4    33  77
*)

(** [sub l1 l2] returns the list resulting from subtracting every 
    element in [l2] from [l1].
    Eg. sub [1;2] [] => [1; 2]
    Eg. sub [1;2;1] [1] => [2]
    Eg. sub [1;2] [2;3;1] => []
*)
let rec sub_helper num l =
  match l with 
  |[] -> []
  |[x] -> 
    if x = num then 
      []
    else 
      [x]
  |head :: tail -> 
    if head = num then 
      sub_helper num tail
    else
      head :: sub_helper num tail 



let rec sub l1 l2 =
  match l2 with 
  |[] -> l1
  | head :: tail -> sub (sub_helper head l1) tail
      

    
(** [outgoing_nodes t n] returns the list of nodes outgoing from node
 ** [n] in the tree [t]. You may assume the node [n] exists in the
 ** tree [t] .
 ** Eg. outgoing_nodes ex 12 => [7; 43]
*)
let rec outgoing_nodes t (n:int) =
  match t with 
  |[] -> []
  |(x, y) :: tail ->
    if x = n then 
      y :: outgoing_nodes tail n
    else
      outgoing_nodes tail n

    
(**  [nodes t] returns the list of nodes of the tree without
    duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [12; 7; 4; 33; 43; 77]

*)   
let rec remove_dup_help n l =
  match l with 
  |[] -> n :: []
  | head :: tail ->
    if head=n then
      remove_dup_help n tail
    else
      head :: remove_dup_help n tail

let rec remove_dup l =
  match l with
  |[] -> []
  | head:: tail ->
    remove_dup_help head tail

let rec nodes_list t =
  match t with 
  |[] -> []
  |(x, y) :: tail ->
    x :: y :: nodes_list tail

let rec nodes t = 
  remove_dup (nodes_list t)


  



    
(** [leaves t] returns the leaves of the tree [t]
   Eg. leaves ex =>  [4; 33; 77]
*)
let rec first_element x t =
  match t with
  | [] -> false
  | (a, _) :: tail -> a = x || first_element x tail

let rec append acc x t =
  if first_element x t then acc else x :: acc

let rec leaves_ acc t =
  match t with
  | [] -> acc
  | (_, b) :: tail -> leaves_ (append acc b t) tail

let leaves t = 
  leaves_ [] t


(* 
   Returns the root of a tree
   Eg. root ex =>  [12]
*)
let rec root t =
  failwith"TODO"

(* 
   Returns the boolean indicating if the tree is a binary tree.
   Eg. is_binary ex =>  true
*)
let rec is_binary t =
  failwith "complete"

(** [subtree t n] returns the subtree rooted at node [n]. (extra-credit)
 **  Eg. subtree ex 12 => [(43, 33); (43, 77); (7, 4); (12, 7); (12, 43)]
         subtree ex 43 -=> [(43, 33); (43, 77)]
         subtree ex 7 => [(7, 4)]
         subtree ex 4 => []
*)
let rec subtree t (n:int) =
  failwith "complete"

                               

