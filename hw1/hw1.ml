(*James Murphy*)
(*"I pledge my honor that I have abided by the Stevens Honor System"*)

type program = int list

let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program = [ 0 ; 2 ; 2 ; 3 ; 3 ; 5 ; 5 ; 4 ; 3 ; 5 ; 4 ; 3 ; 3 ; 5 ; 5 ; 1 ]

let rec map f l = 
  match l with
  | [] -> []
  | h::t -> f h :: map f t


let rec mirror_image_helper x =
  if x = 0 then
    0
  else if x = 1 then
    1
  else if x = 2 then 
    4
  else if x = 3 then 
    5
  else if x = 4 then
    2
  else
    3

let mirror_image l = 
  map mirror_image_helper l


let rec rotate_90_letter_helper x =
  if x = 0 then
    0
  else if x = 1 then
    1
  else if x = 2 then 
    3
  else if x = 3 then 
    4
  else if x = 4 then
    5
  else
    2

let rec rotate_90_letter l =
  map rotate_90_letter_helper l


let rec rotate_90_word lsts =
  match lsts with
  | [] -> []
  | lst :: rest -> (rotate_90_letter lst) :: (rotate_90_word rest)

let rec repeat n x =
  if n <= 0 then 
    []
  else 
    x :: repeat (n-1) x 

let rec lst_of_lst_to_lst l =
  match l with 
  | [] -> []
  | head :: tail -> head @ lst_of_lst_to_lst tail


let rec pantograph_helper n x =
  if n <= 0 then
    []
  else 
    if x = 0 then 
      x :: pantograph_helper 0 x
    else if x = 1 then 
      x :: pantograph_helper 0 x
    else
      x :: pantograph_helper (n-1) x


let rec pantograph n l =
  lst_of_lst_to_lst (map (pantograph_helper n) l)


let rec pantograph_helper_nm num x arr =
  if num = 1 then 
    x :: arr
  else if x = 0 then 
    0 :: arr
  else if x = 1 then
    1 :: arr 
  else
    pantograph_helper_nm (num - 1) x (x :: arr)


let rec pantograph_nm n l =
  match l with 
  |[] -> []
  |[x] -> pantograph_helper_nm n x []
  |head :: tail -> pantograph_helper_nm n head [] @ pantograph_nm n tail


let pantograph_f n l = 
  List.fold_left (fun acc x -> 
  if x == 0 || x == 1
  then acc @ [x]
  else acc @ repeat n x) [] l;;
  

let coverage_helper t n =
  match t with
  |(x,y) ->
    if n = 0 then 
      (x,y)
    else if n = 1 then
      (x,y)
    else if n =2 then
      (x,y+1)
    else if n = 3 then 
      (x+1, y)
    else if n = 4 then
      (x, y - 1)
    else 
      (x-1, y)


let rec coverage t l =
  match l with
  |[] -> [t]
  | head::tail -> t :: coverage (coverage_helper t head) tail


let rec compress_helper num arr l =
  match l with 
  |[] -> []
  |[x] -> (x, num + 1):: arr
  |h :: mid :: tail ->
    if h = mid then
      compress_helper (num + 1) arr (mid :: tail)
    else
      compress_helper 0 ((h, num + 1) :: arr) (mid :: tail)


let compress l =
  List.rev (compress_helper 0 [] l) 



let rec uncompress_helper (x,y) arr =
  if y = 0 then 
    arr
  else 
    uncompress_helper (x, y - 1) (x::arr)

let rec uncompress l =
  match l with
  |[] ->[]
  |[t] -> uncompress_helper t []
  |head :: tail -> uncompress_helper head [] @ uncompress tail


let uncompress_m l =
  lst_of_lst_to_lst (map (fun t -> uncompress_helper t []) l)


let uncompress_f l =
  List.fold_left (fun acc (x, num) -> acc @ (repeat num x)) [] l




let rec optimize_helper l =
  match l with 
  |[] -> []
  |head :: tail ->
    if head = 0 then 
      optimize_helper tail
    else if head = 1 then 
      head ::  optimize tail
    else
      head :: optimize_helper tail
  
let rec optimize l =
  match l with 
  |[] -> []
  |head :: tail ->
    if head = 0 then 
      head :: optimize_helper tail
    else if head = 1 then 
      optimize tail
    else 
      head :: optimize tail











