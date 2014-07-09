(* An abstract machine for the semantics of Ivory *)

theory Semantics
imports Syntax
begin

section {* Semantics *}

subsection {* Expressions *}

(* We use a natural semantics for the evaluation of expressions *)

fun 
  cmpopV :: "cmpop \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> bool"
where
  "cmpopV lt e\<^sub>1 e\<^sub>2 = (e\<^sub>1 < e\<^sub>2)"
| "cmpopV eq e\<^sub>1 e\<^sub>2 = (e\<^sub>1 = e\<^sub>2)"

fun 
  binopV :: "binop \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat"
where
  "binopV add  e\<^sub>1 e\<^sub>2 = (e\<^sub>1 + e\<^sub>2)"
| "binopV sub  e\<^sub>1 e\<^sub>2 = (e\<^sub>1 - e\<^sub>2)"
| "binopV mult e\<^sub>1 e\<^sub>2 = (e\<^sub>1 * e\<^sub>2)"

(* Expression evaluation is partial because not all variables are mapped by the environment *)
fun
  ExpV :: "'var store \<Rightarrow> 'var expr \<Rightarrow> wvalue option"
where
  ExpVar: "ExpV G (Var x) = G x"
| ExpNat:  "ExpV G (Nat n) = Some (PrimV (NatV n))"
| ExpBool: "ExpV G (Bool b) = Some (PrimV (BoolV b))"
| ExpUnit: "ExpV G Unit = Some (PrimV UnitV)"
| ExpBinCmp: "ExpV G (BinCmp bop e\<^sub>1 e\<^sub>2) = (case (ExpV G e\<^sub>1, ExpV G e\<^sub>2) of 
                                           (Some (PrimV (NatV v\<^sub>1)), Some (PrimV (NatV v\<^sub>2))) \<Rightarrow> Some (PrimV (BoolV (cmpopV bop v\<^sub>1 v\<^sub>2)))
                                           | _ \<Rightarrow> None)"
| ExpBinOp: "ExpV G (BinOp bop e\<^sub>1 e\<^sub>2) = (case (ExpV G e\<^sub>1, ExpV G e\<^sub>2) of 
                                           (Some (PrimV (NatV v\<^sub>1)), Some (PrimV (NatV v\<^sub>2))) \<Rightarrow> Some (PrimV (NatV (binopV bop v\<^sub>1 v\<^sub>2)))
                                           | _ \<Rightarrow> None)"

abbreviation 
  expv_some :: "'var store \<Rightarrow> 'var expr \<Rightarrow> wvalue \<Rightarrow> bool" ("_ \<Turnstile> _ \<down> _")
where 
  "G \<Turnstile> e \<down> v \<equiv> ExpV G e = Some v"

fun
  wvalue_to_hvalue :: "wvalue \<Rightarrow> hvalue option"
where
  "wvalue_to_hvalue (PrimV v) = Some (StoredV v)"
| "wvalue_to_hvalue _         = None"

fun
  hvalue_to_wvalue :: "hvalue \<Rightarrow> wvalue option"
where
  "hvalue_to_wvalue (StoredV v) = Some (PrimV v)"

(* Impure expression evaluation is partial because both (pure) expression evaluation and some heap operations are. *)
fun
  ImpureExpV :: "'var store \<Rightarrow> heap \<Rightarrow> 'var impureexp \<Rightarrow> (heap \<times> wvalue) option"
where
  ExpPure: "ImpureExpV G H (Pure e) = Option.map (\<lambda>v. (H, v)) (ExpV G e)"
  (* We just choose a p here, we can probably do better; at least this is deterministic wrt equality as opposed to SOME *)
| ExpNewRef: "ImpureExpV G H (NewRef e) = (let region = length H - 1 in 
                                           let off = fresh_in_heap H region in
                                           Option.bind (ExpV G e) 
                                           (\<lambda>wv. Option.bind (wvalue_to_hvalue wv)
                                                 (\<lambda>v. Option.bind (update_heap H region off v)
                                                      (\<lambda>H'. Some (H', RefV region off)))))"  
| ExpReadRef: "ImpureExpV G H (ReadRef e) = (case ExpV G e of Some (RefV region off) \<Rightarrow> Option.bind (lookup_heap H region off)
                                                                                        (\<lambda>hv. Option.bind (hvalue_to_wvalue hv)
                                                                                              (\<lambda>v. Some (H,v)))
                                                             | _ \<Rightarrow> None)"
| ExpWriteRef: "ImpureExpV G H (WriteRef e\<^sub>1 e\<^sub>2) = (case (ExpV G e\<^sub>1, ExpV G e\<^sub>2) of 
                                                        (Some (RefV region off), Some wv) \<Rightarrow> Option.bind (wvalue_to_hvalue wv) 
                                                                                             (\<lambda>v. Option.bind (update_heap H region off v)
                                                                                                  (\<lambda>H'. Some (H', PrimV UnitV)))
                                                       | _                                \<Rightarrow> None)"

abbreviation 
  impure_expv_some :: "'var store \<Rightarrow> heap \<Rightarrow> 'var impureexp \<Rightarrow> heap \<Rightarrow> wvalue \<Rightarrow> bool"  ("_ \<Turnstile> _, _ \<Down> _, _" [49, 49, 49, 49, 49] 50)
where 
  "G \<Turnstile> H, e \<Down> H', v \<equiv> ImpureExpV G H e = Some (H', v)"

subsection {* Statement evaluation *}

(* The semantics may return either a Normal continuation, a Finished value --- the latter happens only 
   when a return occurs in an empty stack *)
datatype ('var, 'fun) StepResult = Normal "('var, 'fun) state \<times> ('var, 'fun) stmt" | Finished wvalue

(* The semantics of statements is modelled as an abstract machine,
   including stack frames for function and sequence continuations.  An
   operational semantics is possible, but such a semantics make
   preservation difficult to prove.  In particular, the intaraction
   between function calls, the store environment, and the sequence
   typing rule (which has the same store environment for both
   statements in the sequence).
*)

inductive
  Step :: "('var, 'fun) funs \<Rightarrow> ('var, 'fun) state \<times> ('var, 'fun) stmt \<Rightarrow> ('var, 'fun) StepResult \<Rightarrow> bool" ("_ \<Turnstile> _ \<rhd> _" [49, 49, 49] 50)
where
  StepBind:      "\<lbrakk> store S \<Turnstile> heap S, e \<Down> H', v \<rbrakk> \<Longrightarrow> F \<Turnstile> (S, Bind x e s) \<rhd> Normal (S\<lparr> store := (store S)(x \<mapsto> v), heap := H' \<rparr>, s)"
| StepIf:        "\<lbrakk> store S \<Turnstile> e \<down> PrimV (BoolV b) \<rbrakk> \<Longrightarrow> F \<Turnstile> (S, If e s\<^sub>1 s\<^sub>2) \<rhd> Normal (S, if b then s\<^sub>1 else s\<^sub>2)"
  (* Note that we replace e\<^sub>I by e\<^sub>S in the unfolded loop. *)
| StepFor:       "\<lbrakk> store S \<Turnstile> e\<^sub>I \<down> v \<rbrakk> \<Longrightarrow> F \<Turnstile> (S, For x e\<^sub>I e\<^sub>B e\<^sub>S s) \<rhd> Normal (S\<lparr> store := (store S)(x \<mapsto> v) \<rparr>, If e\<^sub>B (s ;; For x e\<^sub>S e\<^sub>B e\<^sub>S s) Skip)"
| StepSeq:       "F \<Turnstile> (S, s\<^sub>1 ;; s\<^sub>2) \<rhd> Normal (S \<lparr> stack := (store S, s\<^sub>2, SeqFrame) # stack S \<rparr>, s\<^sub>1)"
| StepSkip:      "stack S = (st', cont, SeqFrame) # stack' \<Longrightarrow> F \<Turnstile> (S, Skip) \<rhd> Normal (S \<lparr> store := st', stack := stack' \<rparr>, cont)"
| StepReturnSeq: "stack S = (st', cont, SeqFrame) # stack' \<Longrightarrow> F \<Turnstile> (S, Return e) \<rhd> Normal (S \<lparr> stack := stack' \<rparr>, Return e)"
| StepReturnFun: "\<lbrakk> stack S = (store', cont, ReturnFrame x) # stack'; store S \<Turnstile> e \<down> v \<rbrakk>
                  \<Longrightarrow> F \<Turnstile> (S, Return e) \<rhd> Normal (\<lparr> store = store'(x \<mapsto> v), heap = pop_heap (heap S), stack = stack' \<rparr>, cont)"
| StepCall:      "\<lbrakk> F f = Some (Func as body); length as = length es; pre_vs = map (ExpV (store S)) es; \<forall>v \<in> set pre_vs. v \<noteq> None \<rbrakk>
                 \<Longrightarrow> F \<Turnstile> (S, Call x f es s) \<rhd> Normal (\<lparr> store = [as [\<mapsto>] map the pre_vs], heap = push_heap (heap S), stack = (store S, s, ReturnFrame x) # (stack S) \<rparr>, body)"
| StepReturnFin: "\<lbrakk> stack S = []; store S \<Turnstile> e \<down> v \<rbrakk> \<Longrightarrow> F \<Turnstile> (S, Return e) \<rhd> Finished v"

inductive_cases StepSkipE: "F \<Turnstile> (S, Skip) \<rhd> Normal (S', s')"
inductive_cases StepReturnE [consumes 1, case_names SeqFrame ReturnFrame Finish]: "F \<Turnstile> (S, Return e) \<rhd> R"
(* inductive_cases WfStackConsE': "WfStack \<Psi> \<Delta> ((s, cont, x)#st) \<tau>" *)
inductive_cases StepCallE: "F \<Turnstile> (S, Call x args body s) \<rhd> R"
inductive_cases StepSeqE: "F \<Turnstile> (S, s\<^sub>1 ;; s\<^sub>2) \<rhd> R"

inductive
  StepN :: "('var, 'fun) funs \<Rightarrow> nat \<Rightarrow> ('var, 'fun) state \<times> ('var, 'fun) stmt \<Rightarrow> ('var, 'fun) StepResult \<Rightarrow> bool" ("_, _ \<Turnstile> _ \<rhd>\<^sup>* _" [49, 49, 49, 49] 50)
where
  Step1: "F, 0 \<Turnstile> S \<rhd>\<^sup>* Normal S"
| StepN: "\<lbrakk> F, n \<Turnstile> S \<rhd>\<^sup>* Normal S'; F \<Turnstile> S' \<rhd> S'' \<rbrakk> \<Longrightarrow> F, Suc n \<Turnstile> S \<rhd>\<^sup>* S''"


lemma StepN_add_head:
  assumes s1: "F \<Turnstile> S \<rhd> Normal S'"
  and     sn: "F, n \<Turnstile> S' \<rhd>\<^sup>* S''"
  shows   "F, Suc n \<Turnstile> S \<rhd>\<^sup>* S''"
  using sn s1
  by induction (auto intro: StepN.intros)

end

