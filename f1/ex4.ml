let rec factorial x = 
  if x <= 1 then 1 else factorial (x-1) + x


let factorial_opt x = 
  let rec aux x acc = 
    if x <= 1 then acc else aux (x-1) (acc + x)  
  in aux x 1

let () = 
  let read = read_line() in
  let a = factorial (int_of_string read) in
  let b = factorial_opt (int_of_string read) in  
  print_int a; print_newline ();
  print_int b; print_newline ()
