let rec take n lst =
  match lst with
  | [] -> []
  | hd::tl -> if n = 0 then [] else hd:: take (n-1) tl

let rec take2 n lst =
    if n <= 0 then [] else
    match lst with
    | [] -> []
    | hd::tl -> hd :: take2 (n-1) tl
  
let rec drop n lst =
  if n <= 0 then lst else 
  match lst with
  | [] -> []
  | _hd::tl -> drop (n-1) tl 


let take_tail n lst = 
  let rec aux n lst acc = 
    if n <= 0 then List.rev acc else
    match lst with
    | [] -> List.rev acc
    | hd::tl -> aux (n-1) tl (hd::acc)
  in aux n lst []

  
let drop_tail n lst =
  let rec aux n lst =
    if n <= 0 then lst else 
      match lst with 
      | [] -> lst
      | hd::tl -> aux (n-1) tl 
    in aux n lst

let max_true lst =
let m,_ = List.fold_left(fun (count,cur) x -> if x then (count,cur+1) else (max count cur, 0)) (0,0) lst 
  in m


type 'a tree = 
  | Leaf
  | Node of 'a tree * 'a * 'a tree



let rec is_tree el tree = 
  match tree with
  | Leaf -> false
  | Node (l,x,r) -> el = x || is_tree el l || is_tree el r 


let rec depth tree acc =
  match tree with
  | Leaf -> acc
  | Node (l,x,r) -> depth l (acc +1) || depth r (acc+1)
  