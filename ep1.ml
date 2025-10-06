
(*
  check patern of the string 

  params
    s -> string
    pos -> position of the string (starts at 0) 
    fams -> number of families (starts at 0) 
    tam -> size of string
  
  return
    true if pattern is acepted else false 
*)
let check s = 
  let rec aux pos fams tam =
    (* check if its end of string *)
    if pos + 1 <= tam then  
      (* check if fams < 0 (it means theres a M before H)*)
      if fams < 0 then false else
      match s.[pos] with
      | 'H' -> aux (pos +1) (fams +1) tam
      | 'C' -> aux (pos +1) fams tam
      | 'A' -> 
        let ant = s.[pos - 1] in
        let next = s.[pos +1 ] in  
        if ant = 'C' && next = 'C' then aux (pos +1) fams tam else false
      | 'M' -> aux (pos +1) (fams - 1) tam
      | _ -> false
    else 
      if fams = 0 then true else false
  in aux 0 0 (String.length s)


(* stacks *)


(* string -> stack 
  push element of string to stack until string is empty
  params ->  string
  returns -> stack*)
let toStack s =
  let rec aux pos stack= 
    if pos + 1 <= String.length s then 
      let () = Stack.push s.[pos] stack in (aux (pos+1)) stack
  in 
  let ss = Stack.create () in
  aux 0 ss;ss


(* String -> Stack  *)
let toStack_opt s = String.fold_left(fun acc x -> Stack.push x acc;acc) (Stack.create ()) s


(* print stack *)
let printStack s= Stack.iter(fun x -> print_char x) s ;s

(*
  check patern of the stack 

  params
    stack -> stack with chars 
    fams -> number of families (starts at 0) 
    ant -> last char seen (starts empty ' ')
  
  return
    true if pattern is acepted else false 
*)
let check2 stack = 
  let rec aux fams ant = 
    (*stack is reversed so if fams > 0 it means a there a M before H*)
    if fams > 0 then false else
    match Stack.pop_opt stack with
      | None -> if fams = 0 then true else false 
      | Some e -> 
        match e with
        | 'H' -> aux (fams + 1) 'H'    
        | 'C' -> if ant = ' ' then false else aux fams 'C'
        | 'A' -> 
          (* check if next value of stack and previous = 'C' -> true  *)
          if ant  = 'C' && Stack.top stack = 'C' then aux fams 'A'else false
        | 'M' -> aux (fams - 1) 'M'
        | _ -> false
        in 
        aux 0 ' '



(*
  check patern of the stack 

  params
    stack -> stack with chars 
    fams -> number of families (starts at 0) 
    ss -> stack to add elements
  
  return
    true if pattern is acepted else false 
*)
let check3 stack = 
  let rec aux fams ss = 
    (*stack is reversed so if fams > 0 it means a there a M before H*)
    if fams > 0 then false else
    match Stack.pop_opt stack with
      | None -> if fams = 0 then true else false 
      | Some e -> 
        match e with
        | 'H' ->(Stack.push e ss); aux fams ss
        | 'C' -> 
           (Stack.push e ss); aux fams ss
        | 'A' -> 
          (* check if next value of stack and previous on ss = 'C' -> true  *)
          if Stack.top ss = 'C' && Stack.top stack = 'C' then let () = (Stack.push e ss) in aux fams ss else false
        | 'M' ->(Stack.push e ss); aux fams ss
        | _ -> false
        in 
        let ss = (Stack.create ()) in 
        aux 0 ss


let () =  
  (*let string2 = "HHMCM" in*)
  let read = read_line() in
  (*print_string read;*)
  
  (*if read.[0] <> 'H' || read.[String.length read -1] <> 'M' then print_string "NO" else
   if check read = true then print_string "YES" else print_string "NO";
  print_newline
  *)
  if read.[0] <> 'H' || read.[String.length read -1] <> 'M' then print_endline "NO" else
  if check3 ( toStack_opt read) then print_endline "YES" else print_endline "NO";

()

(*    HMHMHMHHHMMMHMMHMHHHMM   *)

