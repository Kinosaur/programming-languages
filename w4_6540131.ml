(* 
Name: Kaung Khant Lin
Student ID: 6540131
Course Code: CSX3004
*)

(*
1. isodd : int -> bool
*)
fun isodd n = n mod 2 = 1;

(*
2. cube : int -> int
*)
fun cube n = n * n * n;

(*
3. cuber : real -> real
*)
fun cuber (x: real) = x * x * x;

(*
4. fourth : 'a list -> 'a
*)
fun fourth (_ :: _ :: _ :: x :: _) = x;

(*
5. min3 : int * int * int -> int
*)
fun min3 (a, b, c) =
  if a <= b andalso a <= c then a
  else if b <= c then b
  else c;

(*
6. remove2 : 'a * 'b * 'c -> 'a * 'c
*)
fun remove2 (a, _, c) = (a, c);

(*
7. thirdch : string -> char
*)
fun thirdch s = case explode s of _ :: _ :: c :: _ => c;

(*
8. rotate : 'a list * int -> 'a list
*)
fun rotate (lst, 0) = lst
  | rotate ([], _) = []
  | rotate (x::xs, n) = rotate (xs @ [x], n - 1);

(*
9. max : int list -> int
*)
fun maxhelper ([], currentMax) = currentMax
  | maxhelper (x::xs, currentMax) = 
      if x > currentMax then maxhelper(xs, x)
      else maxhelper(xs, currentMax);

fun max (x::xs) = maxhelper(xs, x);

(*
10. select : 'a list * ('a -> bool) -> 'a list
*)
fun select ([], _) = []
  | select (x::xs, f) =
      if f x then x :: select(xs, f)
      else select(xs, f);
