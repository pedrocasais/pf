let mul a = a * 10

let () = 
        let read = read_line() in
        let a = mul (int_of_string read) in
        print_int a; print_newline ()