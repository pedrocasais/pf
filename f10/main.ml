module MyStack = struct

  type 'a stack =
  | Empty
  | Entry of 'a * 'a stack


  let empty = Empty

  let push s el = 
    Entry(el,s)
    
  let pop s = function 
    | Empty -> failwith "Stack empty"
    | Entry(_s,e) -> e

  let top s = function
    | Empty -> failwith "Stack empty"
    | Entry(e,_s)-> e  
end

module ListStack = struct
    type 'a stack = 'a list

    let empty = []

    let push e s =
      e::s


    let pop = function
    | [] -> failwith "list empty"
    | _e::s -> s 
    let pop_ret = function
    | [] -> None
    | e::s -> Some(e,s) 

    let pop_ret_opt = function
    | [] -> None
    | e::s -> Some(e,s) 


    let top = function
    | [] -> failwith "list is empty"
    | e::_s -> e

end




let prcessa2 lst = 
  let rec aux s acc = function
  | [] -> 
    (match  Stack.top_opt s with
    | None -> List.rev acc
    | Some e-> List.rev @@ (string_of_int (Stack.length s)^e ) :: acc )
  | hd::tl -> 
    match Stack.top_opt s with
    | None -> let () = Stack.push hd s in aux s acc tl
    | Some e -> if e <> hd then
        let acc' = (string_of_int (Stack.length s) ^ e) :: acc in
        let () = Stack.clear s; Stack.push hd s in
        aux s acc' tl
      else let () = Stack.push hd s in aux s acc tl
    in aux (Stack.create ()) [] lst
   






(*
let processa p p1 = 
  let rec aux stack = function
    | [] -> stack
    | hd::tl -> if hd = '#' then 
      match Stack.pop_opt stack with
      | None -> assert false
      | Some _e ->  aux stack tl 
      else
        let () = Stack.push hd stack in
      in 
      let
        

    | Some(e),Some(e1) ->
      match MyStack.pop stack with
      let a =  MyStack.top stack in
      let b = MyStack.top s2 in
      if e = e1 then aux el el1 (MyStack.push e)  else false 
    | _ -> false
    in 
    aux p p1 (MyStack.empty)



*)


let valida l = 
  let rec aux stack  a b=
  match ListStack.pop stack  with
  | [] -> if a = b then true else false
  | hd::tl -> if hd = '(' then aux tl (a+1) b else if hd = ')' then aux tl a (b+1) else false 
  in aux l 0 0


let inverter s1=
  let s = String.fold_left(fun acc x -> ListStack.push x acc) ListStack.empty s1 in
  let rec aux acc stack= 
  match ListStack.pop stack with
    | [] -> acc
    | hd::tl -> aux (acc^(Char.escaped hd)) tl
in (aux "" s)
    
  let palindrome s = 
    let rec to_stack n acc =
      if n = 0 then acc 
      else to_stack (n/10) (ListStack.push (n mod 10) acc)
    in 
    let s1 = to_stack s ListStack.empty in

    let rec check n st acc =
      match ListStack.pop_ret_opt st with
      | None -> acc && n= 0
      | Some(hd,tl) -> check (n/10) tl (acc && hd = n mod 10)
    in check s s1 true



