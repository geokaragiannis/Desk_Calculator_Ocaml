#use "bigint.ml";;
open Bigint;;
let foo = bigint_of_string "100";;
let bar = bigint_of_string "10000";;
string_of_bigint (pow foo bar);;
