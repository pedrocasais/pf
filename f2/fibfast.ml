

let fibfast n = 
  let rec aux n acc acc2 =
    if n <= 1 then acc2 else aux (n-1) (acc2) (acc2 + acc)  
  in aux n 1 1



let b = fibfast 50
let _ = print_int b; print_newline ()