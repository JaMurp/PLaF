(* Quiz 1 - 1 Feb 2023 

   Student name 1: 
   Student name 2:
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
let rec sub_helper n l =
  match l with
  |[] -> []
  |head :: tail ->
    if head=n then
      sub_helper n tail
    else
      head :: sub_helper n tail

let rec sub l1 l2 =
  match l2 with 
  |[] -> l1 
  |head :: tail ->
    sub (sub_helper head l1) tail
    
(** [outgoing_nodes t n] returns the list of nodes outgoing from node
 ** [n] in the tree [t]. You may assume the node [n] exists in the
 ** tree [t] .
 ** Eg. outgoing_nodes ex 12 => [7; 43]
*)
let rec outgoing_nodes g n =
  match g with
  |[] -> []
  |(x,y) :: tail ->
      if x=n then 
        y :: outgoing_nodes tail n
      else 
        outgoing_nodes tail n


(**  [nodes t] returns the list of nodes of the tree without
    duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [12; 7; 4; 33; 43; 77]

*)

let rec remove_dup n l =
  match l with
  |[] -> []
  |head :: tail ->
    if head=n then 
      remove_dup n tail
    else
      head :: remove_dup n tail

let rec node_helper g =
  match g with 
  |[] -> []
  |(x,y) :: tail ->
    x :: y :: node_helper tail

let rec node_helper_2 l =
  match l with
  |[] -> []
  |head :: tail->
    head :: node_helper_2 (remove_dup head tail)

let nodes g =
   node_helper_2 (node_helper g)
    


(** [leaves t] returns the leaves of the tree [t]
   Eg. leaves ex =>  [4; 33; 77]
*)
let rec not_leaves_is_binary l =
  match l with
  |[]-> []
  |(x,y) :: tail ->
    x :: not_leaves_is_binary tail

let rec could_be_leaves l =
  match l with 
  |[]->[]
  |(x,y) :: tail ->
    y :: could_be_leaves tail
    
let leaves t =
  sub (could_be_leaves t) (not_leaves_is_binary t)


  
(* (* 
   Returns the root of a tree
   Eg. root ex =>  [12]
*)
let rec root t =
  failwith"TDOD" *)

(* 
   Returns the boolean indicating if the tree is a binary tree.
   Eg. is_binary ex =>  true
*)
let rec is_binary t =
  failwith"TDOD"



(** [subtree t n] returns the subtree rooted at node [n].
 **  Eg. subtree ex 12 => [(43, 33); (43, 77); (7, 4); (12, 7); (12, 43)]
         subtree ex 43 -=> [(43, 33); (43, 77)]
         subtree ex 7 => [(7, 4)]
         subtree ex 4 => []
*)
let rec subtree t (n:int) =
  failwith"todo"
  

                               

