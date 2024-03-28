let rec last l =
  match l with 
  | [] -> None
  | [x] -> Some x
  | head :: tail -> last tail


let rec kth n l =
  match l with
  | [] -> None
  | head :: tail ->
    if n = 1 then 
      Some head
    else
      kth (n - 1) tail



let rec length l =
  match l with 
  |[] -> 0
  | head :: tail -> 1 + length tail



let rec reverse l =
  match l with 
  | [] -> []
  |head :: tail -> reverse tail @ [head]



  let rec compress l = 
    match l with 
    | a :: (b :: tail) -> 
      if a = b then 
        compress (b :: tail)
      else 
        a :: compress (b :: tail)
    | [x] -> [x]
    | [] -> []



    let rec aux current acc list = 
      match list with
      | [] -> []  (* Empty list case *)
      | [x] -> (x :: current) :: acc  (* Single element case *)
      | a :: (b :: t) ->  (* List with two or more elements *)
          if a = b then 
            aux (a :: current) acc (b::t)
          else 
            aux [] ((a :: current) :: acc) (b :: t)
    


  let pack list =
    List.rev (aux [] [] list)            



  let rec encoding_helper num arr l =
    match l with 
    |[] -> []
    |[x] -> (num + 1, x) :: arr
    |h :: mid :: tail -> 
      if h = mid then 
        encoding_helper (num + 1) arr (mid :: tail)
      else
        encoding_helper 0 ((num + 1, h) :: arr) (mid :: tail)

  let encoding l =
    List.rev (encoding_helper 0 [] l)



  let rec remove num l =
    match l with 
    |[] -> []
    |head :: tail ->
      if num = 0 then 
        tail
      else
        head :: remove (num - 1) tail

  let rec insert x num l =
    match l with 
    |[] -> []
    |head :: tail ->
      if num = 0 then
        x :: tail
      else 
        head :: insert x (num - 1) l
    



  let rec range_helper num1 num2 l =
    if num1 = num2 then 
      num1 :: l
    else 
      num1 :: range_helper (num1 + 1) num2 l

  let range num1 num2 =
    range_helper num1 num2 []

     
      
  


