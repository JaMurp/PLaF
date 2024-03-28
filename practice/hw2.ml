(* practice hw assigment for quiz *)

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
  |head :: tail ->
    max (height head) (height_help tail)


let rec size t =
  match t with
  |Node(_, [])-> 1
  |Node(_, l) ->
    size_helper l


and size_helper l =
  match l with
  |[] -> 0
  |head :: tail ->
    1 + size head + size_helper tail




let rec paths_to_leaves t =
  match t with 
  |Node(node, []) ->


  
and paths_help arr l =
  match l with
  |[] -> []
  |head :: tail ->
    paths_help_2 head 

and paths_help_2 l =





