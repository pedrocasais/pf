let rec power x n = 
  if n <= 0 then 1 else x * power x (n-1)

let rec soma n i =
  if i <= 0 then 1 else (power n i) + soma n (i - 1) 

let soma_opt n i = 
  let rec aux n i acc =
    if i <= 0 then acc else aux n (i - 1) (acc + (power n i))
  in aux n i 1

let a = soma 3 3
let _ = print_int a; print_newline()

let b = soma_opt 3 3
let _ = print_int b; print_newline()
