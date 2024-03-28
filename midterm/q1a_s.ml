(* Quiz 1 - 31 January 2024

   Student name 1: 
   Student name 2:

*)


(* Notes: 
    a. You may add helper functions.
    b. "let rec" allows your function to be recursive, but it doesn't
    have to be. 
*)

(* Sample Directed Graph *)

let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]


(*
  1 <------ 3
  |      //||
  |     /   | 
  |    /    | 
 \/  /     \/
  2        4
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

(** [outgoing_nodes g n] returns the list of all the nodes that are
    immediate neighbors of [n] in [g].
    Eg. outgoing_nodes ex 3 => [1,4] 
*)
let rec outgoing_nodes g n =
  match g with
  |[] -> []
  |(x,y) :: tail ->
      if x=n then 
        y :: outgoing_nodes tail n
      else 
        outgoing_nodes tail n


(** [nodes g] returns the list of nodes of the graph [g] without duplicates. 
   The order of the nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
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

 
(** [degree g] returns the degree of [g]. The degree of a graph is 
    the maximum number of outgoing edges that any node has. 
    Eg. degree ex => 2
*)

let rec degree_helper_2 l =
  

let rec degree_helper g =
  match g with
  |[]->[]
  |(x,y) :: tail ->
    x :: degree_helper tail


let rec degree g =
  failwith"todo"

(** [remove g n] removes node [n] and all the edges involving [n], from
   the graph [g].
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
  match g with
  |[]-> []
  |(x,y):: tail ->
    if x=n || y=n then
      remove tail n
    else
      (x,y) :: remove tail n

  
(** [reachable g n] returns the list of all the reachable nodes from
   node [n] in [g]. (Extra-credit)
   Eg. reachable ex 3 => [1,4,2,3] 
*)

let rec reachable g n =
  failwith"todfo"

    
