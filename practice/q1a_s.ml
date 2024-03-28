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
let rec sub l1 l2 =
  match l1 with
  | [] -> []
  | h::t ->
    if List.mem h l2
    then sub t l2
    else h:: sub t l2

let sub' l1 l2 =
  List.filter  (fun e -> not (List.mem e l2)) l1

(** [outgoing_nodes g n] returns the list of all the nodes that are
    immediate neighbors of [n] in [g].
    Eg. outgoing_nodes ex 3 => [1,4] 
*)
let rec outgoing_nodes g n =
   List.map snd @@ List.filter (fun (src,tgt) -> src=n)  g

let rec outgoing_nodes' g n =
  match g with
  | [] -> []
  | (src,tgt)::t ->
    if src=n
    then tgt :: outgoing_nodes' t n
    else outgoing_nodes' t n

let rec outgoing_nodes'' g n =
  match g with
  | [] -> []
  | h::t ->
    if fst h=n
    then snd h :: outgoing_nodes'' t n
    else outgoing_nodes'' t n


(** [nodes g] returns the list of nodes of the graph [g] without duplicates. 
   The order of the nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)
let rec rem_dups l =
  match l with
  | [] -> []
  | h::t ->
    if List.mem h t
    then rem_dups t
    else h::rem_dups t
           
let rec nodes g =
    rem_dups @@ List.flatten @@ List.map (fun (src,tgt) -> [src;tgt]) g

let rec nodes g =
  match g with
  | [] -> []
  | (src,tgt)::t ->
    let r = nodes t
    in (match List.mem src r, List.mem tgt r with
        | true,true -> r
        | false,true -> src::r
        | true,false -> tgt::r
        | _,_ -> src::tgt::r)
      
(** [degree g] returns the degree of [g]. The degree of a graph is 
    the maximum number of outgoing edges that any node has. 
    Eg. degree ex => 2
*)
let rec degree g =
    List.fold_left max (-1) @@ List.map (fun n -> List.length (outgoing_nodes g n)) (nodes g)

(** [remove g n] removes node [n] and all the edges involving [n], from
   the graph [g].
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
   List.filter (fun (src,tgt) -> src<>n && tgt<>n) g
  
(** [reachable g n] returns the list of all the reachable nodes from
   node [n] in [g]. (Extra-credit)
   Eg. reachable ex 3 => [1,4,2,3] 
*)

let rec reachable g n = 
  let rec helper current visited =
      match current with
      | [] -> visited
      | h::t ->
        let og = outgoing_nodes g h
        in helper (t @ sub og visited) (h::visited)
  in helper [n] []
   
    
