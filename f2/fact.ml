let rec fact n = 
  if n <= 1 then 1 else n * fact(n-1) 

let fact_opt n =
  let rec aux n acc =
    if n <= 1 then acc else aux(n-1) (acc * n)
  in aux n 1 
 
let a = fact 5 
let _ = print_int a; print_newline ()

let b = fact_opt 5 
let _ = print_int b; print_newline ()