

let ht = Hashtbl.create 1000

let  soma n  = 
  let () = Hashtbl.add ht 0 0 in
  
  let rec aux n = 
    match Hashtbl.find_opt ht n with 
    | Some x -> x
    | None -> 
      let a = aux (n-1) in
      Hashtbl.add ht n (a+n); a+n;
    in aux n
