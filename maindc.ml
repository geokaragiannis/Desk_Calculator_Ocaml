(* $Id: maindc.ml,v 1.5 2017-04-07 13:24:41-07 - - $ *)

include Scanner
include Bigint

open Bigint
open Printf
open Scanner

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop
let strlen = String.length
let strsub = String.sub

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

let regs : (int, Bigint.bigint) Hashtbl.t = Hashtbl.create 256
(* First arg is the key (int) and the second the value (Bigint) *)
let add_reg = Hashtbl.replace regs
(* Takes an int as an arg and returns a Bigint *)
let get_reg =  Hashtbl.find regs

(*
 * Print helper function that prints a number digit by digit.
 * Prints at most 70 digit per line, followed by a backslash.
 *)
let rec print_number_helper str count =
   if (strlen str) = 0 then printf "\n%!"
   else if count = 69 then
      begin
         printf "\\\n";
         print_number_helper str 0   
      end
   else 
      begin
         printf "%c" (String.get str 0);
         print_number_helper (strsub str 1 ((strlen str)-1)) (count + 1)
      end
   
let print_number number=print_number_helper (string_of_bigint number) 0

let print_stackempty () = printf "dc: stack empty\n%!"

let executereg (thestack: stack_t) (oper: char) (reg: int) =
    try match oper with
        | 's' -> let stk = (pop thestack) in add_reg reg stk
        | 'l' -> push (get_reg reg) thestack
        | _   -> printf "0%o 0%o is unimplemented\n%!" (ord oper) reg
    with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) =
    try let right = pop thestack
        in  try let left = pop thestack
                in  push (oper left right) thestack
            with Stack.Empty -> (print_stackempty ();
                                 push right thestack)
    with Stack.Empty -> print_stackempty ()

let execute (thestack: stack_t) (oper: char) =
    try match oper with
        | '+'  -> executebinop thestack Bigint.add
        | '-'  -> executebinop thestack Bigint.sub
        | '*'  -> executebinop thestack Bigint.mul
        | '/'  -> executebinop thestack Bigint.div
        | '%'  -> executebinop thestack Bigint.rem
        | '^'  -> executebinop thestack Bigint.pow
        | 'c'  -> Stack.clear thestack
        | 'd'  -> push (Stack.top thestack) thestack
        | 'f'  -> Stack.iter print_number thestack
        | 'l'  -> failwith "operator l scanned with no register"
        | 'p'  -> print_number (Stack.top thestack)
        | 'q'  -> raise End_of_file
        | 's'  -> failwith "operator s scanned with no register"
        | '\n' -> ()
        | ' '  -> ()
        | _    -> printf "0%o is unimplemented\n%!" (ord oper)
    with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
    let scanbuf = Lexing.from_channel inputchannel in
    let rec toploop () = 
        try  let nexttoken = Scanner.scanner scanbuf
             in  (match nexttoken with
                 | Number number       -> push number thestack
                 | Regoper (oper, reg) -> executereg thestack oper reg
                 | Operator oper       -> execute thestack oper
                 );
             toploop ()
        with End_of_file -> exit 0;
    in  toploop ()

let readfiles () =
    let thestack : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1 
         then try  let thefile = open_in Sys.argv.(1)
                   in  toploop thestack thefile
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(0) message;
                   exit 1));
        toploop thestack stdin)

let interact () =
    let thestack : bigint Stack.t = Stack.create ()
    in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()

