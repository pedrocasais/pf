let soma lst =
  let rec aux lst acc =
    if lst = [] then acc else aux (List.tl lst) (acc + List.hd lst)
  in aux lst 0 

let rec soma2 lst =
  match lst with
  | [] -> 0
  | hd :: tl -> hd + soma2 (List.tl lst)

let conta_pares lst =
  let rec aux lst acc =
    if lst = [] then acc else 
      if List.hd lst mod 2 = 0 then aux (List.tl lst) (acc + 1)
      else aux (List.tl lst) acc
    in aux lst 0

let rec conta_pares2 lst =
  match lst with
  | [] -> 0
  | hd::tl -> if hd mod 2 = 0 then 1 + conta_pares2 tl 
  else conta_pares2 tl

let palindroma lst =
  let rev = List.rev lst in
  let rec aux lst rev = 
    match lst,rev with
  | [],[] -> true
  | hd :: tl, hd2::tl2 -> hd = hd2 && aux tl tl2 
  | _ -> assert false 
  in aux lst rev

let remove_duplicate_sorted lst =
  if List.length lst < 2 then lst
  else
    let rec aux lst current flag =
      match lst with
      | [] -> []
      | [e] -> if e <> current then [e] else []
      | hd::tl -> if flag then hd::(aux tl hd false) else
         if hd = current then aux tl current false else hd::(aux tl hd false)
    in aux lst (List.hd lst) true

let rec remove_duplicate lst seen =
  match lst with
  | [] -> seen
  | hd :: tl -> if List.mem hd seen then remove_duplicate tl seen else remove_duplicate tl (hd::seen)


let a = soma [1;5;3]
let _ = print_int a; print_newline ()

let sum_list lst = List.fold_left(+) 0 lst

let b = conta_pares [1;-6;3;17;4;80;-18]
let _ = print_int b; print_newline ()

let c = conta_pares [1;-6;3;17;4;80;-18]
let _ = print_int c; print_newline ()



let palindroma_lst lst = List.for_all2(fun x y -> x = y) lst (List.rev lst)