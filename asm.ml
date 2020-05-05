(*
Modified By: Christopher J. Kelly
Due Date:    4/8/2020
Assignment:  #6
Course:      Compilers II
Professor:   Schwesinger
Filename:    asm.ml
Purpose:     Translates commands to asm. mov, add, sub, mul
*)

open Printf

type reg =
  | RAX
  | RBX
  | RSP
  | RDI
  | RSI
  | R10
  | R11
  | R12

type arg =
  | Const of int
  | HexConst64 of int64
  | Reg of reg
  | PReg of string
  | MemAddr of reg
  | RegOffset of int * reg

type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg
  | ITupIndex of arg * arg * arg

  | ICmp of arg * arg
  | IJump of string
  | ILabel of string
  | ICall of string
  | IPush of arg

  | IJLT of string
  | IJGT of string
  | IJEQ of string
  | IJNEQ of string
  | IJGTE of string
  | IJLTE of string

  | ISar of arg* arg
  | IShl of arg* arg
  | IOr of arg * arg
  | IAnd of arg * arg

  | IComm of string
  | IRet


let r_to_asm (r : reg) : string =
  match r with
  | RAX -> "%rax"
  | RBX -> "%rbx"
  | RSP -> "%rsp"
  | RDI -> "%rdi"
  | RSI -> "%rsi"
  | R10 -> "%r10"
  | R11 -> "%r11"
  | R12 -> "%r12"

let p_to_asm (p : string) : string =
  match p with
  | "P0"  -> "%rdi"
  | "P1"  -> "%rsi"
  | "P2"  -> "%rdx"
  | "P3"  -> "%rcx"
  | "P4"  -> "%r8"
  | "P5"  -> "%r9"
  | _     -> failwith "Not a valid parameter register"

let arg_to_asm (a : arg) : string =
  match a with
  | Const(n) -> sprintf "$%d" n
  | HexConst64(n) -> sprintf "$0x%LX" n
  | Reg(r) -> r_to_asm r
  | PReg(p) -> p_to_asm p
  | MemAddr(r) -> sprintf "(%s)" (r_to_asm r)
  | RegOffset(n, r) ->
     if n >= 0 then
       sprintf "%d(%s)" n (r_to_asm r)
     else
       sprintf "-%d(%s)" (-1 * n) (r_to_asm r)

let i_to_asm(i : instruction) : string =
  match i with
  | IMov(src, dst) -> sprintf  "  movq %s, %s" (arg_to_asm src) (arg_to_asm dst)
  | IAdd(src, dst) -> sprintf  "  addq %s, %s" (arg_to_asm src) (arg_to_asm dst)
  | ISub(src, dst) -> sprintf  "  subq %s, %s" (arg_to_asm src) (arg_to_asm dst)
  | IMul(src, dst) -> sprintf  "  imulq %s, %s" (arg_to_asm src) (arg_to_asm dst)
  | ICmp(src, dst) -> sprintf  "  cmp %s, %s" (arg_to_asm src) (arg_to_asm dst)
  | IJump(name) -> sprintf     "  jmp %s" name
  | ILabel(name) -> sprintf    "%s:" name
  | ICall(name) -> sprintf     "  call %s" name
  | IPush(arg) -> sprintf      "  pushq %s" (arg_to_asm arg)
  | IJLT(name) -> sprintf      "  jl %s" name
  | IJGT(name) -> sprintf      "  jg %s" name
  | IJEQ(name) -> sprintf      "  je %s" name
  | IJNEQ(name) -> sprintf     "  jne %s" name
  | IJGTE(name) -> sprintf     "  jge %s" name
  | IJLTE(name) -> sprintf     "  jle %s" name
  | ISar(src, dest) -> sprintf "  sarq %s, %s" (arg_to_asm src) (arg_to_asm dest)
  | IShl(src, dest) -> sprintf "  shlq %s, %s" (arg_to_asm src) (arg_to_asm dest)
  | IOr(src, dest) ->  sprintf "  orq %s, %s" (arg_to_asm src) (arg_to_asm dest)
  | IAnd(src, dest) -> sprintf "  andq %s, %s" (arg_to_asm src) (arg_to_asm dest)
  | IComm(c) -> sprintf "/* %s */" c
  (* offset 8 to skip past size of tuple, then multiply index by 8 bytes *)
  | ITupIndex(loc, i, dest) -> sprintf "  movq 8(%s, %s, 8), %s" (arg_to_asm loc) (arg_to_asm i) (arg_to_asm dest)
  | IRet -> "  retq"

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (i_to_asm i)) "" is
