#use "bigint.ml";;
open Bigint;;
let foo = bigint_of_string "_325";;
let bar = bigint_of_string "_325";;
string_of_bigint (sub foo bar);;
