

  let del stack = 
    let rec aux s s2 = 
    match Stack.pop_opt s with 
      | None -> s2      
      | Some hd -> 
        if hd = '#' then
          let _ = Stack.pop_opt s2 in
          aux s s2
        else 
          let () = Stack.push hd s2 in  aux s s2
      in
    let rec a l1 s = match l1 with | [] -> s | hd :: tl -> let () = Stack.push hd s in a tl s
    in aux Stack.create() Stack.create()   
