// http://fsharp.net에서 F#에 대해 자세히 알아봅니다.
//3.1
let rec map f (l:'a list) =
 if (l.IsEmpty) then [] else f(l.Head) :: map f l.Tail;;

let max (n:int) m = if n>m then n else m;;

let rec maxl (n:int) l = 
 match l with
 | [] -> n
 | h::t -> if h > n then maxl h t else maxl n t;;

let maxlist l =
 if l = [] then failwith "null list"
 else maxl l.Head l.Tail;;

//3.2
let rec mem m l =
 match l with
 | [] -> false
 | h::t -> if m=h then true else mem m t;;

let rec reduce l =
 match l with
 | [] -> []
 | h::t -> if (mem h t) then reduce t else h::reduce t;;

 //3.3
 let sum a b c = (if c then (a = b) else not(a = b));;

 let carry a b c = (if c then (a || b) else (a = b) );;

 let rec pow n = if n=0 then 1 else 2 * pow (n-1) ;;

 let bval l = 
  let rec bvall l1 x n = 
    match l1 with
    | [] -> 0
    | h::t -> if h=true then pow n + bvall t x n+1 else  bvall t x n+1
  bvall l (l.Length-1) 0

//  match l with
//  | [] -> 0
//  | h::t -> if l.Item(l.Length-1) = true then pow (l.Length-1) + bval t else bval t;;
// let bval l = 


 let evenp n = (n/2)*2 ;;

 let rec rep n = 
  if n>0 then if (n = evenp n) then false :: rep (n/2) else true :: rep (n/2)
  else [];;

let rec addc cin (l1:'a list) (l2:'a list) = 
 if (carry cin l1.Head l2.Head) then sum cin l1.Head l2.Head :: addc true l1.Tail l2.Tail
 else  sum cin l1.Head l2.Head :: addc false l1.Tail l2.Tail;;

let add x y = addc false x y;;
 


//let plus a b =