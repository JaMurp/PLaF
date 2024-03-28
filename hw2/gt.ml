(*James Murphy*)
(*I pledge my Honor that I have abided by the Stevens Honor System*)

type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])



let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37,
                     [Node (14, [])]);
               Node (48, []);
               Node (103, [])])
       ])

       let x : int gt =
        Node (33,
              [Node (12,[]);
               Node (77,[])
              ])

let rec height t =
  match t with
  |Node(_,[]) -> 1
  |Node(_, l) ->
    1 + height_help l

and height_help l =
  match l with
  |[] -> 0
  |head:: tail -> 
    max (height head) (height_help tail)

let rec size t =
  match t with
  |Node(_,[])-> 1
  |Node(_, l)->
    1 + size_help l

and size_help l =
    match l with
    |[] -> 0
    | head:: tail ->
      size head + size_help tail


let rec paths_helper path node =
    match node with
    | Node (_, []) ->[List.rev path] 
    | Node (_, children) ->
      List.flatten(List.mapi (fun i child -> paths_helper (i:: path) child) children)
      
let paths_to_leaves t =
    paths_helper [] t
      
let rec help l =
  match l with 
  |[] -> 0
  |head :: tail ->
    1 + help tail

let rec is_perfect_help l =
  match l with 
  |[] -> true
  |[x] -> true
  |head :: mid :: tail ->
    if (help head) = (help mid) then 
      is_perfect_help (mid :: tail)
    else
      false

let rec is_leaf_perfect t =
  is_perfect_help (paths_to_leaves t)


(* 33, 12, 77, 37, 14, 48, 103 *)
let rec preorder t =
  match t with
  |Node(value,[]) -> [value]
  |Node(value, l)->
    value :: List.flatten (List.map preorder l)

let rec mirror t =
  match t with
  |Node(node,[])-> Node(node, [])
  |Node(node,l)->
    Node(node, (List.map mirror (List.rev l)))

let rec map f t =
  match t with 
  |Node(node,[]) -> Node(f node, [])
  |Node(node, l) -> 
    Node(f node, (List.map (map f) l))


let rec fold f t =
  match t with 
  |Node(node, l)->
    f node (List.map (fold f) l)

let sum t =
  fold (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t
        
let mem t e = 
  fold (fun i rs -> i=e || List.exists (fun i -> i) rs) t


let rec mirror' t =
  match t with 
  |Node(node, l) ->
    fold (fun node l -> Node(node, (List.rev l))) t

let max_in_list l =
  match l with
  |[] -> 0
  |head:: tail -> 
    List.fold_left max head tail
    
let degree_help l =
  match l with
  |Node(node, l) -> 
    List.length l
    
let rec degree t =
  match t with
  | Node(node, []) -> 0
  | Node(node , l) ->
    let child = List.map degree l in
    let x = List.length l in
    max x (max_in_list child)
    

