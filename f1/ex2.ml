let rec power x n = 
  if n <= 0 then 1 else x * power x (n-1)

let rec p_tr x n acc = 
  if n <= 0 then acc else p_tr x (n-1) (x*acc)

let power_opt x n = 
  let rec aux x n acc = 
    if n <= 0 then acc else aux x (n-1) (x*acc)
  in aux x n 1

let () = 
  let read = read_line() in
  let a = power (int_of_string read) 2 in
  print_int a; print_newline ()