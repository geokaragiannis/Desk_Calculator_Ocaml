#use "bigint.ml";;
open Bigint;;
let foo = bigint_of_string "100";;
let bar = bigint_of_string "100";;
string_of_bigint (pow foo bar);;

let regs = Hashtbl.create 256;;
(*there is an add function which may seem more appropriate but 
ocaml hash tables can have multiple values hashed on the same key.
Hashtbl.replace adds the <key, value> if the key doesnt already exist
and replaces the old value with the new value if the key does exist.*)
Hashtbl.replace regs 'z' (bigint_of_string "25");;
Hashtbl.replace regs 'a' (bigint_of_string "_10");;
string_of_bigint (mul (Hashtbl.find regs 'z') (Hashtbl.find regs 'a'));;
