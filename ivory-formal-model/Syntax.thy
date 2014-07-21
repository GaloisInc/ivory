(* The syntax of Ivory terms and types *)

theory Syntax
imports Heaps
begin

section {* Syntax *}

subsection {* Types *}

(* These are things which can appear in the heap and can be loaded/stored
   from references*)
(* These classify heap values, which are only primitive types for the moment *)
datatype 'r area = Stored "'r wtype"
and      'r wtype = BoolT | NatT | UnitT | RefT 'r "'r area"

(* Some abbreviations for a bit of flexitility in case things change *)
abbreviation 
  NAT :: "'r wtype"
where
  "NAT \<equiv> NatT"

abbreviation 
  BOOL :: "'r wtype"
where
  "BOOL \<equiv> BoolT"

abbreviation 
  UNIT :: "'r wtype"
where
  "UNIT \<equiv> UnitT"

(* The type of functions: the first type is the return type, the list is that of the arguments *)
datatype 'r funtype = FunT "'r wtype" "'r wtype list"

subsection {* Expressions and statements *}

(* Binary operators and comparators *)
datatype binop = add | sub | mult
datatype cmpop = lt | eq

(* Expressions, essentialy standard *)
datatype 'var expr = 
  Var 'var
  | Nat nat
  | Bool bool
  | Unit
  | BinCmp cmpop "'var expr" "'var expr"
  | BinOp  binop "'var expr" "'var expr"

(* Not control flow, but impure in that they mutate the heap *)
datatype 'var impureexp = 
  Pure       "'var expr"
  | NewRef   "'var expr" 
  | ReadRef  "'var expr"
  | WriteRef "'var expr" "'var expr"

datatype ('var, 'fun) stmt = 
  Skip (* Do nothing *)
  | Return "'var expr" (* Return from an expression *)
  | Bind 'var "'var impureexp" "('var, 'fun) stmt" (* Bind the value of the inpure expression in the variable in the stmt *)
  | If "'var expr" "('var, 'fun) stmt" "('var, 'fun) stmt" (* Conditionals *)
  | For 'var "'var expr" "'var expr" "'var expr" "('var, 'fun) stmt" (* Looping *)
  | Seq "('var, 'fun) stmt" "('var, 'fun) stmt" (infixr ";;" 90) (* Statement composition *)
  | Call 'var 'fun "'var expr list" "('var, 'fun) stmt" (* Function calls, binding the result as the variable the stmt *)

fun
  is_terminal :: "('var, 'fun) stmt \<Rightarrow> bool"
where
  "is_terminal (Return e) = True"
| "is_terminal _          = False"

lemmas is_terminalE [consumes 1, case_names Return Skip]
  = is_terminal.elims(2)

subsubsection {* Values *}

(* As with the types above *)
datatype wvalue = NatV nat | BoolV bool | UnitV | RefV ridx roff
datatype hvalue = StoredV wvalue

(* Functions *)
datatype ('var, 'fun) func = Func "'var list" "('var, 'fun) stmt"

(* F: maps function names to their definitions; doesn't change dynamically *)
type_synonym ('var, 'fun) funs = "'fun \<Rightarrow> ('var, 'fun) func option"

(* R: maps region offsets to their contents *)  
type_synonym region = "roff \<Rightarrow> hvalue option"

(* H: maps region ids to regions *)
type_synonym heap = "region list"

(* st: stores map variables to their values *)
type_synonym 'var store = "'var \<Rightarrow> wvalue option"

(* The semantics are an abstract machine, including a stack representing both
   function return continuations and sequence continuations *)
datatype 'var frame_class = ReturnFrame 'var | SeqFrame

fun
  isReturnFrame :: "'var frame_class \<Rightarrow> bool"
where
  "isReturnFrame (ReturnFrame _) = True"
| "isReturnFrame SeqFrame        = False"

(* A frame then contains a continuation in the form of a store, a stmt, and, 
   in the case of a function frame, a variable for the result *)
type_synonym ('var, 'fun) stack_frame = "'var store \<times> ('var, 'fun) stmt \<times> 'var frame_class"

(* and a stack is just a list of frames *)
type_synonym ('var, 'fun) stack = "('var, 'fun) stack_frame list"

(* A state is then a store mapping (immutable) variables to values, a heap, and a stack *)
record ('var, 'fun) state = 
  store :: "'var store" 
  heap :: heap 
  stack :: "('var, 'fun) stack"

end