let ht = Hashtbl.create 1000

let fact n =
let rec aux n acc =
  if n <= 1 then acc else aux (n-1) (acc*n)
in aux n 1

let comb n k =
  (fact n / (fact k*fact(n-k)))

let rec bell n =
  if n = 0 then 1 else 
  let rec aux n k acc= 
    match Hashtbl.find_opt ht n with
    | Some x-> x
    | None ->
      if k = n then acc
      else 
      let res2 = aux n (k-1) (acc +  comb n k * bell k) in
      Hashtbl.add ht n res2; res2;
    in aux n 0 0

let rec bell_z n = 
  if n = Z.zero then Z.one 
  else
    let rec aux n k acc =
      if k = n then acc else 
        aux n Z.(k - Z.one) Z.(acc + (comb (n-Z.one) k) * bell_z k)
      in aux n Z.zero Z.zero


let memo_rec f =
  let h = Hashtbl.create 16 in
  let rec g x =
    try Hashtbl.find h x
    with Not_found->
      let y = f g x in
      Hashtbl.add h x y;
      y
    in
    g

    let () =
      let n = 10 in
      Printf.printf "%s\n" (bell n |> string_of_int)