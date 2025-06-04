
let rec sum n k f =
  if k = n -2 then 0,0
  else
    let a,count1 = f k in
    let b,count2 = f (n-k-1) in
    let sumRest,sumCount = sum n (k-1) f in
    a + b + sumRest,count1 + count2 + sumCount
let rec s1 n = 
    if n = 0 then 1,1 
    else if n  = 1 then 2,1 
    else
      let a,count = s1 (n-1) in 
      let sumResult, sumCount = sum n 1 s1 in
      3 * a + sumResult, count + sumCount + 1 

let rec ex2 n =
  if n = 0 then 1 
  else if n= 1 then 2 
  else ((6*n)-3 * ex2 (n-1) - (n-2)* ex2 (n-2)) / (n+1)


let () = 
let n = 6 in
Printf.printf "%d\n" (ex2 n)


