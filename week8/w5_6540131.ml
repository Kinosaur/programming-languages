(* 
Name: Kaung Khant Lin
Student ID: 6540131
Course Code: CSX3004
*)

fun exist (e, L) =
  case L of
      [] => false
    | x::xs => if e = x then true else exist (e, xs);

fun greaterthan (L, e) =
  case L of
      [] => []
    | x::xs => if x > e then x :: greaterthan(xs, e)
               else greaterthan(xs, e);

fun repeats L =
  case L of
      [] => false
    | [_] => false
    | x::y::xs => if x = y then true else repeats (y::xs);

fun quicksort [] = []
  | quicksort (x::xs) =
    let
      val (left, right) = List.partition (fn y => y < x) xs
      (* left: elements < x, right: elements >= x *)
    in
      quicksort left @ [x] @ quicksort right
    end;

fun ismember (_, []) = false
  | ismember (x, y::ys) = x = y orelse ismember (x, ys);

fun union ([], ys) = ys
  | union (x::xs, ys) =
      if List.exists (fn y => x = y) ys
      then union(xs, ys)
      else x :: union(xs, ys);

fun intersection ([], _) = []
  | intersection (x::xs, ys) =
      if List.exists (fn y => x = y) ys
      then x :: intersection(xs, ys)
      else intersection(xs, ys);

fun powerset xs =
  foldr (fn (x, acc) => acc @ (map (fn s => x::s) acc)) [[]] xs;