let rec sum a = 
  if a = 0 then 0 
  else (a mod 10) + sum (a/10)

let sum_opt a =
  let rec aux a acc =
    if a = 0 then acc
    else  aux (a/10) (acc + a mod 10) 
  in aux a 0

let () = 
  let read = read_line() in
  let a = sum (int_of_string read) in
  let b = sum_opt (int_of_string read) in  
  print_int a; print_newline ();
  print_int b; print_newline ()
