let rec fib x = 
  if x < 2 then 1 else fib (x-1) + fib(x-2)

let fib_opt x = 
  let rec aux x a b =
    if x = 0 then a 
    else if x = 1 then b 
    else aux (x-1) (b) (a+b)
  in aux x 1 1


let () = 
  let read = read_line() in
  let a = fib (int_of_string read) in
  let b = fib_opt (int_of_string read) in  
  print_int a; print_newline ();
  print_int b; print_newline ()
  