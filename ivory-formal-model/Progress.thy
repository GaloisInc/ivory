(* The proof of the progress property *)

theory Progress
imports EvalSafe
begin

lemma Progress: 
  assumes wff: "WfFuns F \<Psi>"
  and    wfst: "WfState S \<Gamma> \<Psi> \<tau> b \<rho>"
  and     wfs: "\<Gamma>, \<Psi>, \<rho> \<turnstile> s : \<tau>, b"
  shows "\<exists>R. F \<Turnstile> (S, s) \<rhd> R"
  using wfs wfst wff 
proof (induct arbitrary: S)
  case (wfSkip \<Gamma> \<Psi> \<rho> \<tau> S)
  from `WfState S \<Gamma> \<Psi> \<tau> False \<rho>` 
  obtain \<Delta> \<Theta> where "WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> False" ..
  then obtain store' cont st where "stack S = (store', cont, SeqFrame) # st"
    by (rule wfStackFalseE)
  show ?case by (rule exI StepSkip)+ fact
next
  case (wfReturn \<Gamma> e \<tau> \<Psi> \<rho> b S)
  from `\<Gamma> \<turnstile> e : \<tau>` `WfState S \<Gamma> \<Psi> \<tau> b \<rho>`
  obtain v where e_to_v: "store S \<Turnstile> e \<down> v" 
    by (auto elim: Expr_safeE elim!: WfState.cases)

  show ?case
  proof (cases "stack S = []")
    case True thus ?thesis using e_to_v by (auto intro: Step.intros)
  next
    case False
    then obtain st cont fclass stack'
      where stackS: "stack S = (st, cont, fclass) # stack'"
      by (fastforce simp: neq_Nil_conv)

    thus ?thesis using e_to_v
      by (cases fclass) (auto intro: Step.intros)
  qed
next
  case (wfBind \<Gamma> \<rho> e \<tau>' v \<Psi> s \<tau> b S)

  from `\<Gamma>, \<rho> \<turnstile>I e : \<tau>'` `WfState S \<Gamma> \<Psi> \<tau> b \<rho>`
  obtain H' \<Theta>' \<Delta> v' where eval: "store S \<Turnstile> heap S, e \<Down> H', v'" 
    and wfh': "WfHeap \<Delta> H' \<Theta>'" 
    and wfs': "WfStore \<Delta> \<Theta>' (store S) \<Gamma>"
    and wfwv': "WfWValue \<Delta> \<Theta>' v' \<tau>'"
    by (auto elim!: ImpureExpr_safe_stateE)

  from wfs' wfwv' have wfs_upd: "WfStore \<Delta> \<Theta>' (store S(x \<mapsto> v')) (\<Gamma>(x \<mapsto> \<tau>'))" 
    by (rule WfStore_upd)
        
  with eval show ?case 
    by (auto intro: exI Step.intros)  
next
  case (wfIf \<Gamma> e \<Psi> \<rho> s\<^sub>1 \<tau> b s\<^sub>2 S)
  from `\<Gamma> \<turnstile> e : BOOL` `WfState S \<Gamma> \<Psi> \<tau> b \<rho>`
  obtain bv where e_to_v: "store S \<Turnstile> e \<down> BoolV bv" 
    by (auto elim!: Expr_safeE WfBoolVE elim!: WfState.cases)

  thus ?case 
    by (auto intro: exI Step.intros)      
next 
  case (wfWhile \<Gamma> e\<^sub>I \<tau>' v e\<^sub>B e\<^sub>S \<Psi> \<rho> s \<tau> b S)

  from `\<Gamma> \<turnstile> e\<^sub>I : \<tau>'` `WfState S \<Gamma> \<Psi> \<tau> False \<rho>`
  obtain v where e_to_v: "store S \<Turnstile> e\<^sub>I \<down> v"
    by (auto elim: Expr_safeE elim!: WfState.cases)

  thus ?case by (auto intro: exI Step.intros)
next
  case (wfSeq \<Gamma> \<Psi> \<rho> s\<^sub>1 \<tau> s\<^sub>2 b S)
  show ?case by rule rule
next
  case (wfCall \<Psi> f \<sigma> ts \<Gamma> \<theta> es x \<rho> s \<tau> b S)

  from `\<Psi> f = Some (FunT \<sigma> ts)` `WfFuns F \<Psi>`
  obtain fbody where "F f = Some fbody" "WfFunc \<Psi> fbody (FunT \<sigma> ts)"
    by (auto elim!: WfFuns.cases submap_stE)
  then obtain as body where 
    things: "F f = Some (Func as body)"
    "length as = length ts"
    by (cases fbody, auto elim!: WfFunc.cases)
      
  show ?case
  proof (rule exI, rule StepCall [OF _ _ refl])
    show "F f = Some (Func as body)" by fact
  next
    have "list_all2 (\<lambda>e \<tau>. \<Gamma> \<turnstile> e : tsubst \<theta> \<tau>) es ts" by fact
    hence "length es = length ts" ..
    with things show "length as = length es" by simp
  next
    show "\<forall>v \<in> set (map (ExpV (store S)) es). v \<noteq> None"
    proof      
      fix v
      assume "v \<in> set (map (ExpV (store S)) es)"
      then obtain e where ein: "e \<in> set es" and ev: "ExpV (store S) e = v"
        by clarsimp

      have "list_all2 (\<lambda>e \<tau>. \<Gamma> \<turnstile> e : tsubst \<theta> \<tau>) es ts" by fact      
      then obtain t where "\<Gamma> \<turnstile> e : tsubst \<theta> t" using ein
        by (rule list_all2_ballE1)
      
      moreover have "WfState S \<Gamma> \<Psi> \<tau> b \<rho>" by fact
      ultimately show "v \<noteq> None" using ev
        by (auto elim: Expr_safeE elim!: WfState.cases)
    qed
  qed
qed

end
