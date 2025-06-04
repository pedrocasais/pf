let ht = Hashtbl.create 1000

let fact n =
let rec aux n acc =
  if n <= Z.one then acc else aux Z.(n-Z.one) Z.(acc*n)
in aux n Z.one

let comb n k =
  let nume = fact n in
  let deno = Z.(fact k * fact Z.(n-k)) in
  Z.(nume / deno)


let rec bell n =
  if n = Z.zero then Z.one else 
  let rec aux n k acc= 
    match Hashtbl.find_opt ht n with
    | Some x-> x
    | None ->
      if k = n then acc
      else 
      let res2 = aux n Z.(k-Z.one) Z.(acc +  comb n k * bell k) in
      Hashtbl.add ht n res2; res2;
    in aux n Z.zero Z.zero

let rec bell_z n = 
  if n = Z.zero then Z.one 
  else
    let rec aux n k acc =
      if k = n then acc else 
        aux n Z.(k - Z.one) Z.(acc + (comb (n- Z.one ) k) * bell_z k)
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



    let bell_r = 
      memo_rec (fun bell_2 n ->  if n = Z.zero then Z.one 
      else
        let rec aux n k acc =
          if k = n then acc else 
            aux n Z.(k - Z.one) Z.(acc + (comb (n- Z.one ) k) * bell_2 k)
          in aux n Z.zero Z.zero
    )

    let () =
      let n = Z.of_int 10 in
      Printf.printf "%s\n"  (bell_r n |> string_of_int)