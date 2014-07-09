(* The model of the type system *)

theory TypeSystem
imports Syntax
begin

section {* Well formed programs *}

subsection {* Preliminaries *}

(* \<Sigma>: region  *)
type_synonym regionT = "roff \<Rightarrow> area option"

(* \<Theta> *)
type_synonym heapT = "regionT list" 

(* \<Psi> *)
type_synonym ('fun, 'r) funsT = "'fun \<Rightarrow> 'r funtype option"

(* \<Gamma> *)
type_synonym ('var, 'r) storeT = "'var \<Rightarrow> 'r wtype option"

(* \<Delta>: Maps region variables to their corresponding runtime regions *)
type_synonym 'r region_env = "'r \<Rightarrow> ridx option"


subsection {* Region type variable substitutions *}

(* \<theta>: maps region variables to region variables *)
type_synonym 'r tsubst = "'r \<Rightarrow> 'r"

fun 
  tsubst :: "'r tsubst \<Rightarrow> 'r wtype \<Rightarrow> 'r wtype"
where
  "tsubst \<theta> (RefT \<rho> \<tau>) = RefT (\<theta> \<rho>) \<tau>"
| "tsubst \<theta> \<tau>          = \<tau>"

fun 
  tfrees :: "'r wtype \<Rightarrow> 'r set"
where
  "tfrees (RefT \<rho> \<tau>) = {\<rho>}"
| "tfrees \<tau>          = {}"

definition
  tfrees_set :: "'r wtype set \<Rightarrow> 'r set"
where
  "tfrees_set ts = \<Union>(tfrees ` ts)"

subsection {* Expressions *}

inductive
  WfE :: "('var, 'r) storeT \<Rightarrow>'var expr \<Rightarrow> 'r wtype \<Rightarrow> bool" ("_ \<turnstile> _ : _" [49, 49, 49] 50) 
where
  wfVar:      "\<Gamma> v = Some \<tau> \<Longrightarrow> \<Gamma> \<turnstile> Var v : \<tau>"
| wfNat:      "\<Gamma> \<turnstile> Nat n : NAT"
| wfBool:     "\<Gamma> \<turnstile> Bool b : BOOL"
| wfUnit:     "\<Gamma> \<turnstile> Unit : UNIT"
| wfBinCmp:   "\<lbrakk> \<Gamma> \<turnstile> e\<^sub>1 : NAT; \<Gamma> \<turnstile> e\<^sub>2 : NAT \<rbrakk> \<Longrightarrow> \<Gamma> \<turnstile> BinCmp bop e\<^sub>1 e\<^sub>2 : BOOL"
| wfBinOp:    "\<lbrakk> \<Gamma> \<turnstile> e\<^sub>1 : NAT; \<Gamma> \<turnstile> e\<^sub>2 : NAT \<rbrakk> \<Longrightarrow> \<Gamma> \<turnstile> BinOp bop e\<^sub>1 e\<^sub>2 : NAT"

subsection {* Reference Initializers *}

inductive
  WfInit :: "('var, 'r) storeT \<Rightarrow> 'var init \<Rightarrow> area \<Rightarrow> bool" ("_ \<turnstile>0 _ : _" [49,49,49]50)
where
  wfIStored: "\<Gamma> \<turnstile> e : Prim \<tau> \<Longrightarrow> \<Gamma> \<turnstile>0 IStored e : Stored \<tau>"
| wfIArray:  "\<lbrakk> (\<forall> e \<in> set es. \<Gamma> \<turnstile>0 e : \<alpha>) ; length es = n \<rbrakk> \<Longrightarrow> \<Gamma> \<turnstile>0 IArray es : Array n \<alpha>"
(*
| wfIStruct: "list_all2 (WfInit \<Gamma>) es \<alpha>s \<Longrightarrow> \<Gamma> \<turnstile>0 IStruct es : Struct \<alpha>s"
*)
  | wfIStruct: "\<lbrakk> (\<forall> (e,\<alpha>) \<in> set (zip es \<alpha>s). \<Gamma> \<turnstile>0 e : \<alpha>) ; length es = length \<alpha>s \<rbrakk> \<Longrightarrow> \<Gamma> \<turnstile>0 IStruct es : Struct \<alpha>s"

subsection {* Impure expressions *}

inductive
  WfImpureExpr :: "('var, 'r) storeT \<Rightarrow> 'r \<Rightarrow> 'var impureexp  \<Rightarrow> 'r wtype \<Rightarrow> bool" ("_, _ \<turnstile>I _ : _" [49, 49, 49] 50) 
where
  wfPure:     "\<lbrakk> \<Gamma> \<turnstile> e : \<tau> \<rbrakk> \<Longrightarrow> \<Gamma>, \<rho> \<turnstile>I Pure e : \<tau>"
| wfNewRef:   "\<lbrakk> \<Gamma> \<turnstile>0 i : \<alpha>  \<rbrakk> \<Longrightarrow> \<Gamma>, \<rho> \<turnstile>I NewRef i : RefT \<rho> \<alpha>"
| wfReadRef:  "\<lbrakk> \<Gamma> \<turnstile> e : RefT \<gamma> (Stored \<tau>) \<rbrakk> \<Longrightarrow> \<Gamma>, \<rho> \<turnstile>I ReadRef e : Prim \<tau>"
  (* Solve for e2 first *)
| wfWriteRef: "\<lbrakk> \<Gamma> \<turnstile> e\<^sub>2 : Prim \<tau>;  \<Gamma> \<turnstile> e\<^sub>1 : RefT \<gamma> (Stored \<tau>) \<rbrakk>  \<Longrightarrow> \<Gamma>, \<rho> \<turnstile>I WriteRef e\<^sub>1 e\<^sub>2 : UNIT"

subsection {* Statements *}

inductive
  WfStmt :: "('var, 'r) storeT \<Rightarrow> ('fun, 'r) funsT \<Rightarrow> 'r \<Rightarrow> ('var, 'fun) stmt \<Rightarrow> 'r wtype \<Rightarrow> bool \<Rightarrow> bool" ("_, _, _ \<turnstile> _ : _, _" [49, 49, 49, 49] 50) 
where
  wfSkip:   "\<Gamma>, \<Psi>, \<rho> \<turnstile> Skip : \<tau>, False" 
| wfReturn: "\<Gamma> \<turnstile> e : \<tau> \<Longrightarrow> \<Gamma>, \<Psi>, \<rho> \<turnstile> Return e : \<tau>, b"
| wfBind:   "\<lbrakk> \<Gamma>, \<rho> \<turnstile>I e : \<tau>'; \<Gamma>(v \<mapsto> \<tau>'), \<Psi>, \<rho> \<turnstile> s : \<tau>, b \<rbrakk> \<Longrightarrow> \<Gamma>, \<Psi>, \<rho> \<turnstile> Bind v e s : \<tau>, b"
| wfIf:     "\<lbrakk> \<Gamma> \<turnstile> e : BOOL; \<Gamma>, \<Psi>, \<rho> \<turnstile> s\<^sub>1 : \<tau>, b ; \<Gamma>, \<Psi>, \<rho> \<turnstile> s\<^sub>2 : \<tau>, b \<rbrakk> \<Longrightarrow> \<Gamma>, \<Psi>, \<rho> \<turnstile> If e s\<^sub>1 s\<^sub>2 : \<tau>, b"
| wfWhile:  "\<lbrakk> \<Gamma> \<turnstile> e\<^sub>I : \<tau>'; \<Gamma>(v \<mapsto> \<tau>') \<turnstile> e\<^sub>B : BOOL; \<Gamma>(v \<mapsto> \<tau>') \<turnstile> e\<^sub>S : \<tau>'; \<Gamma>(v \<mapsto> \<tau>'), \<Psi>, \<rho> \<turnstile> s : \<tau>, b \<rbrakk> \<Longrightarrow> \<Gamma>, \<Psi>, \<rho> \<turnstile> For v e\<^sub>I e\<^sub>B e\<^sub>S s : \<tau>, False"
| wfSeq:    "\<lbrakk> \<Gamma>, \<Psi>, \<rho> \<turnstile> s\<^sub>1 : \<tau>, False; \<Gamma>, \<Psi>, \<rho> \<turnstile> s\<^sub>2 : \<tau>, b \<rbrakk> \<Longrightarrow> \<Gamma>, \<Psi>, \<rho> \<turnstile> s\<^sub>1 ;; s\<^sub>2 : \<tau>, b"
| wfCall:   "\<lbrakk> \<Psi> f = Some (FunT \<sigma> ts);
               list_all2 (\<lambda>e \<tau>. \<Gamma> \<turnstile> e : tsubst \<theta> \<tau>) es ts;
               \<Gamma>(x \<mapsto> tsubst \<theta> \<sigma>), \<Psi>, \<rho> \<turnstile> s : \<tau>, b \<rbrakk> \<Longrightarrow> \<Gamma>, \<Psi>, \<rho> \<turnstile> (Call x f es s) : \<tau>, b"

inductive_cases wfReturnE: "\<Gamma>, \<Psi>, \<rho> \<turnstile> Return e : \<tau>, b"

subsection {* Functions *}
(*
    tfrees \<tau> \<subseteq> tfrees_set (set ts) is derivable for well-typed programs,
but it would require a rather complicated proof over all the functions.  The
syntactic check is easier.


    Note that we want to be able to pick the region as being free in a larger
set than tfrees_set (set ts).
*)

inductive 
  WfFunc :: "('fun, 'r) funsT \<Rightarrow> ('var, 'fun) func \<Rightarrow> 'r funtype \<Rightarrow> bool"
where
  wfFunc: "\<lbrakk> length args = length ts;
             \<rho> \<notin> tfrees_set (set ts); [args [\<mapsto>] ts], \<Psi>, \<rho> \<turnstile> s : \<tau>, True;
             tfrees \<tau> \<subseteq> tfrees_set (set ts) \<rbrakk> (* no polymorphic return *)
             \<Longrightarrow> WfFunc \<Psi> (Func args s) (FunT \<tau> ts)"

section {* Well-formed values and programs *}

inductive
  WfPrimValue :: "prim_value \<Rightarrow> prim \<Rightarrow> bool"
where
  wfNatV:  "WfPrimValue (NatV n) NatT"
| wfBoolV: "WfPrimValue (BoolV b) BoolT"
| wfUnitV: "WfPrimValue UnitV UnitT"

inductive_cases WfPNatVE: "WfPrimValue v NatT"
inductive_cases WfPBoolVE: "WfPrimValue v BoolT"

inductive 
  WfWValue :: "'r region_env \<Rightarrow> heapT \<Rightarrow> wvalue \<Rightarrow> 'r wtype \<Rightarrow> bool"
where
  wfPrimV: "WfPrimValue v \<tau> \<Longrightarrow> WfWValue \<Delta> \<Theta> (PrimV v) (Prim \<tau>)"
| wfRefV:  "\<lbrakk> \<Delta> \<rho> = Some region; lookup_heap \<Theta> region off = Some \<tau> \<rbrakk> \<Longrightarrow> WfWValue \<Delta> \<Theta> (RefV region off) (RefT \<rho> \<tau>)"

inductive_cases WfNatVE: "WfWValue \<Delta> \<Theta> v NAT"
inductive_cases WfBoolVE: "WfWValue \<Delta> \<Theta> v BOOL"
inductive_cases WfRefVE: "WfWValue \<Delta> \<Theta> v (RefT \<rho> \<tau>)"

inductive 
  WfHValue :: "hvalue \<Rightarrow> area \<Rightarrow> bool"
where
  wfStoredV: "WfPrimValue v \<tau> \<Longrightarrow> WfHValue (StoredV v) (Stored \<tau>)"

inductive 
  WfHeap :: "heap \<Rightarrow> heapT \<Rightarrow> bool"
where
  wfHeapNil: "WfHeap [] []"
| wfHeapCons:"\<lbrakk> WfHeap H \<Theta>; submap_st \<Sigma> R WfHValue; finite (dom R) \<rbrakk> \<Longrightarrow> WfHeap (H @ [R]) (\<Theta> @ [\<Sigma>])"

inductive 
  WfFuns :: "('var, 'fun) funs \<Rightarrow> ('fun, 'r) funsT \<Rightarrow> bool"
where
  WfFuns: "submap_st \<Psi> F (WfFunc \<Psi>) \<Longrightarrow> WfFuns F \<Psi>"

inductive 
  WfStore :: "'r region_env \<Rightarrow> heapT \<Rightarrow> 'var store \<Rightarrow> ('var, 'r) storeT \<Rightarrow> bool"
where
  WfStore: "submap_st \<Gamma> G (WfWValue \<Delta> \<Theta>) \<Longrightarrow> WfStore \<Delta> \<Theta> G \<Gamma>"

(* We also know that \<forall>k \<in> ran (\<Delta> |` (- {\<rho>}). k < n *)
inductive
  WfFrees :: "'r region_env \<Rightarrow> ('var, 'r) storeT \<Rightarrow> 'r \<Rightarrow> nat \<Rightarrow> bool"
where
  WfFrees: "\<lbrakk> \<Delta> \<rho> = Some n; \<forall>k \<in> ran \<Delta>. k \<le> n; tfrees_set (ran \<Gamma>) \<subseteq> dom \<Delta>; finite (dom \<Delta>)\<rbrakk> \<Longrightarrow> WfFrees \<Delta> \<Gamma> \<rho> n"

inductive_cases WfFreesE [elim?]: "WfFrees \<Delta> \<Gamma> \<rho> n"

inductive 
  WfStack :: "('fun, 'r) funsT \<Rightarrow> 'r region_env \<Rightarrow> heapT \<Rightarrow> ('var, 'fun) stack \<Rightarrow> 'r wtype \<Rightarrow> bool \<Rightarrow> 'r \<Rightarrow> bool"
where
  wfStackNil:  "WfStack \<Psi> \<Delta> [\<Sigma>] [] NAT True \<rho>" (* The initial state expectss a nat back and has \<Sigma>, the global heap *)
| wfStackFun: "\<lbrakk> WfStack \<Psi> \<Delta>' \<Theta> st \<tau>' b' \<gamma>; WfStore \<Delta>' \<Theta> store' \<Gamma>; \<Gamma>(x \<mapsto> \<tau>), \<Psi>, \<gamma> \<turnstile> cont : \<tau>', b'; 
                 WfFrees \<Delta>' (\<Gamma>(x \<mapsto> \<tau>)) \<gamma> (length \<Theta> - 1); \<Delta>' \<subseteq>\<^sub>m \<Delta> \<rbrakk>
                \<Longrightarrow> WfStack \<Psi> \<Delta> (\<Theta> @ [\<Sigma>]) ((store', cont, ReturnFrame x) # st) \<tau> True \<rho>"
| wfStackSeq: "\<lbrakk> WfStack \<Psi> \<Delta> \<Theta> st \<tau> b' \<rho>; WfStore \<Delta> \<Theta> store' \<Gamma>; \<Gamma>, \<Psi>, \<rho> \<turnstile> cont : \<tau>, b'; tfrees_set (ran \<Gamma>) \<subseteq> dom \<Delta> \<rbrakk>
                \<Longrightarrow> WfStack \<Psi> \<Delta> \<Theta> ((store', cont, SeqFrame) # st) \<tau> b \<rho>"


inductive_cases wfStackFalseE: "WfStack \<Psi> \<Delta> \<Theta> st \<tau> False \<rho>"

inductive_cases WfStackConsE: "WfStack \<Psi> \<Delta> \<Theta> (s#st) \<tau> b \<rho>"
inductive_cases WfStackSeqE: "WfStack \<Psi> \<Delta> \<Theta> ((st, s, SeqFrame) # st') \<tau> b \<rho>"

declare One_nat_def Un_insert_right [simp del]
inductive_cases WfStackFunE: "WfStack \<Psi> \<Delta> \<Theta> ((st, s, ReturnFrame x) # st') \<tau> b \<rho>"
declare One_nat_def Un_insert_right [simp]

inductive 
  WfState :: "('var, 'fun) state \<Rightarrow> ('var, 'r) storeT \<Rightarrow> ('fun, 'r) funsT \<Rightarrow> 'r wtype \<Rightarrow> bool \<Rightarrow> 'r \<Rightarrow> bool"
where
  WfState: "\<lbrakk> WfStore \<Delta> \<Theta> (store S) \<Gamma>; WfHeap (heap S) \<Theta>; WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> b \<rho>; WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta> - 1) \<rbrakk>
            \<Longrightarrow> WfState S \<Gamma> \<Psi> \<tau> b \<rho>"

declare One_nat_def [simp del]
inductive_cases WfStateE [elim]: "WfState S \<Gamma> \<Psi> \<tau> b \<rho>"
declare One_nat_def [simp]

inductive 
  WfProgram :: "('fun, 'r) funsT \<Rightarrow> ('var, 'fun) state \<Rightarrow> ('var, 'fun) stmt \<Rightarrow> bool"
where
  wfProgramI: " \<lbrakk> WfState S \<Gamma> \<Psi> \<tau> b \<rho>; \<Gamma>, \<Psi>, \<rho> \<turnstile> s : \<tau>, b \<rbrakk> \<Longrightarrow> WfProgram \<Psi> S s"


end