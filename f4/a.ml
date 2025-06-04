let lista_antes = ['a';'9';'T';'%';'z';'-']


let rec to_Upper lst=
match lst with
| [] -> []
| hd :: tl -> if hd >= 'a' && hd <= 'z' then
  Char.chr (Char.code hd -32) :: to_Upper tl
else hd :: to_Upper tl


let rec to_Upper_ter lst acc =
match lst with
| [] -> acc
| hd :: tl -> if hd >= 'a' && hd <= 'z' then
  let c = Char.chr (Char.code hd -32) in
  to_Upper_ter tl (acc @ [c])    
else to_Upper_ter tl (acc @ [hd]) 



let to_Upper_opt lst =
  let rec aux lst acc = 
  match lst with
  | [] -> acc
  | hd :: tl -> if hd >= 'a' && hd <= 'z' then
    let c = Char.chr (Char.code hd -32) in
    aux tl (acc @ [c])    
  else aux tl (acc @ [hd])
in aux lst []

let fancy_Upper lst =
  List.map(function | 'a'..'z' as c -> Char.chr (Char.code c -32) | c -> c) lst


let rec is_sorted lst cmp =
  match lst with
  | [] -> true
  | [e] -> true
  | e1::e2::tl -> cmp e1 e2 <= 0  && is_sorted tl cmp


let is_sorted_uneficient lst cmp =
  let tmp = List.sort cmp lst in
  List.for_all2 (fun e1 e2 -> e1 = e2) lst tmp


let count_pairs l =
  List.fold_left (fun x acc-> if x mod 2 = 0 then acc +1 else acc) 0 l

let palindroma l =
  List.for_all2 (fun x y -> x = y) l (List.rev l)


let remove_dups l =
  List.fold_left(fun acc a -> if List.mem a acc then acc else acc @ [a]) [] l


let count l =
  List.fold_left (fun acc a -> if a then acc + 1 else acc ) 0 l 
  (*   List.length(List.filter (fun x -> x) l)   *) 