
(* type 'a option = None | Some of 'a *)
                    
type 'a node = { mutable data:'a;
                 mutable next: ('a node) option}

type 'a ll = { mutable head: ('a node) option;
               mutable size: int}

let ll1 : 'a ll =
  { head = None;
    size = 0 }

let ll2: int ll =
  { head = Some { data=5; next=None};
    size = 1}

let addFirst e ll =
  ll.head <- Some { data = e; next = ll.head };
  ll.size <- ll.size+1

let string_of_ll f ll =
  let rec helper no =
    match no with
    | None -> ""
    | Some n ->
       f (n.data) ^ "," ^ helper (n.next)
  in "["^helper (ll.head)^"]"

  
let mem e ll =
  let rec helper e no =
    match no with
    | None-> false 
    | Some n ->
      if n.data=e then
        true
      else
        helper e (n.next)
  in helper e (ll.head)


let addLast e ll =
  failwith "implement"
    
let get_at i ll =
  failwith "implement"

let add_at i e ll =
  failwith "implement"

let remove_first ll =
  failwith "implement"

let remove_last ll =
  failwith "implement"

let remove_at i ll =
  failwith "implement"

let rev ll =
  failwith "implement"
