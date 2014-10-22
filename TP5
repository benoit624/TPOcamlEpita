(* Truth table forget_c*)
let rec append l1 l2 = match l1 with
  |[] -> l2
  |h::t -> h::append t l2;;
type 'a bundle =
    | Empty
    | Item of 'a * 'a bundle;;

(* 0.1*)
let empty_bundle ()=
    Empty;;
empty_bundle ();;
(* 0.2*)
let is_empty = function
    |Empty->true;
    |_->false;;

is_empty Empty;;
is_empty (Item ('a', Empty));;

(* 0.3*)
let cons bundle=function
    |a->Item(a,bundle);;
cons Empty 4;;
cons (Item (4, Empty)) 6;;
(* 0.4*)
let head =function
    |Item(a,_)->a;
    |_->failwith "Head failed: empty bundle" ;;
(*head Empty;;*)
head (Item (5, Empty));;
(* 0.5*)
let tail =function
    |Item(_,a)->a;
    |_->failwith "Head failed: empty bundle" ;;
    tail (Item ('a', (Item ('r', Empty))));;

    type boolean =
| True | False
| Var of string
| Not of boolean
| And of boolean * boolean
| Or of boolean * boolean ;;

Or (Or (Or (Or (Or (Var "f", Var "e"), Var "d"), Var "c"), Var "b"), Var "a");;
(* 1.1*)
let rec value strin= function
    |[]->failwith ("Unbound Var: "^strin);
    |(a,boolean)::t when a=strin ->boolean;
    |_::t-> value strin t;;
value "a" [("b", False); ("a", True)];;
(*value "c" [("b", False); ("a", True)];;*)
let rec extract = function
    |Var a|Not Var a->[a];
    |Or(a,b)|And(a,b)->append (extract a) (extract b);
    |_->failwith "je ne gere pas les true et false";;
extract (Or( (And (Var "a", Var "b")), (And (Var "c", Not (Var "e")))));;

let generate t= 
    let rec init = function
        |[]->[]
        |h::t->0::init t in init t;;
generate ["a"];;
generate ["a";"b"];;
