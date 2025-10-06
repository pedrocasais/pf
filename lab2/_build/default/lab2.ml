
  (* recursiva normal *)
  let rec tribonacci n =
    if n < 2 then 0
    else (
      if n = 2 then 1 else
      (* completar *)
      tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)
    )

(*

*)
  (* recursiva terminal *)
  (*
    a = 0
    b = 0
    c = 1
  *)
  let tribonacci_terminal n =
    let rec aux n a b c =
      if n < 3 then c
      else (* completar *)
      aux (n - 1) b c (a + b + c)
    in
    if n < 2 then 0 else (* completar *)
    aux n 0 0 1



(* função principal *)
let _ =
  let linha = read_line () in
  (*print_endline linha ;*)
  (* completar *)
  let a = tribonacci (int_of_string linha) in
  print_int a ; print_newline () ;
  let b = tribonacci_terminal (int_of_string linha) in
  print_int b ; print_newline 
  ()
