let count  = ref 0

let rec fib1 n =  
  let () =
    incr count;
    Printf.printf "%d\n" !count in
    if n = 0 then 1 else if n = 1 then 1 else fib1 (n-2) + fib1(n-1)

let htable = Hashtbl.create 3000

let fib2 n =
    Hashtbl.add htable 0 0;
    Hashtbl.add htable 1 1;
    let rec aux n = 
    match Hashtbl.find_opt htable n with
    | Some x  -> x
    | None -> 
      let a = aux(n-1) in
      let b = aux(n-2) in
      let res = a+b in
      Hashtbl.add htable n res; res in
    aux n

let ht = Hashtbl.create 5000 


let fib_z n =
  let rec aux i =
    Hashtbl.add ht 0 Z.zero;
    Hashtbl.add ht 1 Z.one in
    match Hashtbl.find_opt ht n with
    | Some x -> x
    | None ->
      let a = aux (Z.sub i (Z.of_int 2)) in 
      let b = aux (Z.sub i Z.one) in
      let res = Z.add a b in
      Hashtbl.add ht i res; res in
      aux (Z.of_int n) 


let () = 
  let n = 100 in
  Printf.printf "R -> %d\n" (fib2 n)
