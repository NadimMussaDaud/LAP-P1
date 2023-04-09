(* Maze module body *)
(* LAP (AMD 2023) *)

(* 
Student 1: Guilherme Simoes nº62909
Student 2: ????? mandatory to fill

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

let combine m1 m2 = 
	loopMaze
;;

(* FUNCTION next *)

let next m r = 
	[]
;;

(* FUNCTION next2 *)

let next2 m r = 
	[]
;;

(* FUNCTION prev *)

let prev m r = 
	[]
;;

(* FUNCTION adjacent *)

let adjacent m r = 
	[]
;;

(* FUNCTION reachable *)

let reachable m = 
	[]
;;

(* FUNCTION solitary *)

let solitary m = 
	[]
;;

(* FUNCTION islands *)

let islands m = 
	[]
;;

(* FUNCTION shortest *)

let shortest m = 
	_NO_PATH
;;

(* FUNCTION paths *)

let paths m = 
	[]
;;

(* FUNCTION hasLoop *)

let hasLoop m = 
	false
;;

(* FUNCTION shortest2 *)

let shortest2 m = 
	_NO_PATH
;; 
