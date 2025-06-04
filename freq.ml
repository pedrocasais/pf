open OUnit2;;

let rec soma = function [] -> 0 | x ::xs -> x + soma xs


let testes = 
  "testes"
  >:::[
      ("vazio" >:: fun _ -> assert_equal 1 (soma [])); 
      ("um" >:: fun _ -> assert_equal 1 (soma [1])); 
    ("dois" >:: fun _ -> assert_equal 3 (soma [1;2])); 
  ]

let _ = run_test_tt_main testes
