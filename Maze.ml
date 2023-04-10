(* Maze module body *)
(* LAP (AMD 2023) *)

(* 
Student 1: Guilherme Simoes nº62909
Student 2: Nadim Mussá Daud nº63529

Comment:

?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????

*)

(*
0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
	100 columns
*)


(* COMPILATION - How Mooshak builds this module:
		ocamlc -c Maze.mli Maze.ml
*)



(* AUXILIARY GENERAL FUNCTIONS - you can add more *)

(* Sorted lists with no repetitions *)
(* precondition for all the list arguments:
		isCanonical l && isCanonical l1 && isCanonical l2 *)
(* postcondition for all the list results: isCanonical result *)

let rec removeDups z = (* pre: z sorted *)
	match z with
	| [] -> []
	| [x] -> [x]
	| x::y::xs -> (if x = y then [] else [x])@ removeDups (y::xs)
;;

let canonize z = (* sort and remove duplicates *)
	removeDups (List.sort compare z)
;;

let isCanonical z = (* check if sorted and with no duplicates *)
	z = (canonize z)
;;

let belongs v l =
	List.mem v l
;;

let length =
	List.length
;;

let filter =
	List.filter
;;

let exists =
	List.exists
;;

let for_all =
	List.for_all
;;

let partition =
	List.partition
;;

let contained l1 l2 =
	for_all (fun x -> belongs x l2) l1
;;

let union l1 l2 =
	canonize (l1@l2)

let inter l1 l2 =
	filter (fun x -> belongs x l2) l1
;;

let diff l1 l2 =
	filter (fun a -> not (belongs a l2)) l1
;;

let map f l =
	canonize (List.map f l)
;;

let merge l =
	canonize (List.flatten l)
;;

let flatMap f l =
	merge (List.map f l)
;;

let showi l =
	let li = List.map string_of_int l in
	let body = String.concat "," li in
		Printf.printf "[%s]\n" body
;;

let showp l =
	let li = List.map (fun (a,b) -> Printf.sprintf "(%d,%d)" a b) l in
	let body = String.concat "," li in
		Printf.printf "[%s]\n" body
;;

let rec range a b =
  if a > b then []
  else a :: range (a + 1) b;;

let rec linePassages a b =
  if a > b then []
  else (a, a+1):: linePassages(a+1) b;;

(* TYPES & CONSTANTS *)

type room = int
type rooms = room list

type path = room list
type island = room list

type passage = room * room
type passages = passage list

type maze = {
    rooms: rooms;
	entrances: rooms;
    exits: rooms;
    passages: passages
}

let _NO_PATH = []


(* SOME EXAMPLES - you can add more *)

let myMaze = {
    rooms = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];
	entrances = [1;4;11];
    exits = [6;12];
    passages = [(1,2);(1,4);(1,5);(2,5);(3,6);(4,5);(4,7);
				(5,6);(5,8);(6,9);(7,8);(8,9);(10,11);(11,12)]
};;

let loopMaze = {
    rooms = [1;2;3;4];
	entrances = [1];
    exits = [4];
    passages = [(1,2);(2,3);(3,4);(4,1)]
};;

(* FUNCTION isValid *)

(* falta verificar se as rooms contem todas as passagens *)
let isValid m = (* pre: m not empty*)
	if (length (inter m.exits m.entrances) = 0 && 
    contained m.rooms (union m.entrances m.exits)) then true 
  else false
;;

(* FUNCTION makeLineMaze *)

let makeLineMaze a b =  {
  rooms = range a b;
  entrances = [a];
  exits = [b];
  passages = linePassages a b;
}
	
;;

(* FUNCTION combine *)

(* falta a verificacao do maze *)
let combine m1 m2 = {
	rooms = canonize (union m1.rooms m2.rooms);
  entrances = canonize (union m1.entrances m2.entrances);
  exits = canonize (union m1.exits m2.exits);
  passages = canonize (union m1.passages m2.passages);
}
;;

(* FUNCTION next   SE R NA POSIÇÃO x REGISTAR O NUMERO n*)

let rec nextB l r =
	match l with
	|[] -> []
	|(x,n)::xs -> if x=r then [n]::nextB xs r
;;

let next m r = 
	match m.passages with
	|[] -> []
	|x::xs -> nextB m.passages r 
							
;;

(* FUNCTION next2 *)

let next2 m rs = 
	match m.passages with
	|[] -> []
	|x::xs -> match rs in
						 |[] -> []
						 |x::xs -> [next m x]::[next m xs]
;;

(* FUNCTION prev *)

let rec nextC l r =
	match l with
	|[] -> []
	|(n,x)::xs -> if r=x then [n]::nextC xs r
;;

let prev m r = 
	match m.passages with
	|[] -> []
	|x::xs -> nextC m.passages r 
;;

(* FUNCTION adjacent *)

let adjacent m r = 
	match m.passages with
	|[] -> []
	|x::xs -> prev m r::next m r
;;

(* FUNCTION reachable *)

let reachable m = 
	next2 m m.entrances
;;

(* FUNCTION solitary   DIFF BETWEEN M.ROOMS AND M.PASSAGES*)
let rec belongs room passages =
	match passages with
	|[] -> []
	|x::xs -> room=x || belongs room xs
;;

(* EVITAR REPETIÇAO NO "IF-THEN-ELSE" *)
let solitary m = 
	match m.rooms with
	|[] -> []
	|x::xs -> if belongs x m.passages
						then solitary xs m.passages
						else x::solitary xs m.passages
;;

(* FUNCTION islands *)
(*Incompleto*)

let rec allAdjacents m =
	match m.rooms with
	|[] -> []
	|x::xs -> (adjacent m x)::(allAdjacents xs)
;;
(*unir todas listas com elementos em comum *)
let islands m = 
	match allAdjacents m with
	|[] -> []
	|x::xs -> if inter x xs <> []
						then union x xs
						else x::islands xs
;;

(* FUNCTION shortest *)
(*Incompleto*)
let shortest m = 
	match m.entrances with
	|[] -> []
	|x::xs -> match m.passages in
						|[] -> []
						|(y,n)::ys -> if x=y then 
;;

(* FUNCTION paths *)
(*Incompleto*)

(*let rec paths2 v l
	match l with
	|[] -> []
	|(x,n)::xs -> if v=x 
								then n::paths2 n xs
								else paths2 v xs
;;*)

let paths m = 
	match m.entrances with
	|[] -> []
	|x::xs -> x::paths2 xs m.passages	
;;


(* FUNCTION hasLoop *)


let hasLoop m = 
	false
;;

(* FUNCTION shortest2 *)

let shortest2 m = 
	_NO_PATH
;; 
