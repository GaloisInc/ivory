(* The top-level proofs of soundness *)

theory Soundness
imports Progress Preservation
begin

lemma Soundness:
  fixes \<Psi> :: "('fun, 'r :: {infinite}) funsT"
  assumes wfp: "WfProgram \<Psi> S s"
  and     wff: "WfFuns F \<Psi>"
  shows  "(\<exists>m nv. m \<le> n \<and> F, m \<Turnstile> (S, s) \<rhd>\<^sup>* Finished (PrimV (NatV nv)) )
         \<or> (\<exists>S' s'. F, n \<Turnstile> (S, s) \<rhd>\<^sup>* Normal (S', s') \<and> WfProgram \<Psi> S' s')"
  using wfp 
proof (induction n arbitrary: S s)
  case 0 

  show ?case
  proof (intro disjI2 exI conjI)
    show "F, 0 \<Turnstile> (S, s) \<rhd>\<^sup>* Normal (S, s)" ..
  qed fact
next
  case (Suc n)

  from `WfProgram \<Psi> S s` obtain \<Gamma> b \<tau> \<rho> where 
    "WfState S \<Gamma> \<Psi> \<tau> b \<rho>" and "\<Gamma>, \<Psi>, \<rho> \<turnstile> s : \<tau>, b"
    by (auto elim!: WfProgram.cases)

  with `WfFuns F \<Psi>` obtain R where "F \<Turnstile> (S, s) \<rhd> R"
    by (auto dest!: Progress)

  show ?case
  proof (cases R)
    case (Finished v)
    with `F \<Turnstile> (S, s) \<rhd> R` have "F \<Turnstile> (S, s) \<rhd> Finished v" by simp
    then obtain e where "s = Return e" "stack S = []" "store S \<Turnstile> e \<down> v"
      by cases simp_all

    from `s = Return e` `\<Gamma>, \<Psi>, \<rho> \<turnstile> s : \<tau>, b` have "\<Gamma> \<turnstile> e : \<tau>" 
      by (auto elim: WfStmt.cases)

    from `WfState S \<Gamma> \<Psi> \<tau> b \<rho>` `stack S = []`
    obtain \<Theta> \<Delta> where "WfStore \<Delta> \<Theta> (store S) \<Gamma>" 
      and "\<tau> = NAT" and "b = True" 
      by (auto elim: WfStack.cases elim!: WfStateE)

    from `\<Gamma> \<turnstile> e : \<tau>` `WfStore \<Delta> \<Theta> (store S) \<Gamma>` `store S \<Turnstile> e \<down> v` `\<tau> = NAT`
    obtain nv where "v = PrimV (NatV nv)"
      by (auto elim!: Expr_safeE WfNatVE WfPNatVE)
    with `F \<Turnstile> (S, s) \<rhd> Finished v` have "F \<Turnstile> (S, s) \<rhd> Finished (PrimV (NatV nv))"
      by simp
    
    show ?thesis
    proof (intro exI disjI1 conjI)
      show "Suc 0 \<le> Suc n"
        by simp

      from `F \<Turnstile> (S, s) \<rhd> Finished (PrimV (NatV nv))`
      show "F, Suc 0 \<Turnstile> (S, s) \<rhd>\<^sup>* Finished (PrimV (NatV nv))"
        by (auto intro: StepN.intros)
    qed
  next
    case (Normal Ss)
    with `F \<Turnstile> (S, s) \<rhd> R` obtain S' s' where "F \<Turnstile> (S, s) \<rhd> Normal (S', s')" by (cases Ss, auto)

    with `WfFuns F \<Psi>` `WfState S \<Gamma> \<Psi> \<tau> b \<rho>` `\<Gamma>, \<Psi>, \<rho> \<turnstile> s : \<tau>, b` 
    have "WfProgram \<Psi> S' s'"
      by (auto dest!: Preservation intro: WfProgram.intros)

    hence "(\<exists>m nv. m \<le> n \<and> F, m \<Turnstile> (S', s') \<rhd>\<^sup>* Finished (PrimV (NatV nv))) \<or>
      (\<exists>S'' s''. F, n \<Turnstile> (S', s') \<rhd>\<^sup>* Normal (S'', s'') \<and> WfProgram \<Psi> S'' s'')"
      by (rule Suc.IH)
    
    thus ?thesis
    proof (elim disjE conjE exE)
      fix m nv
      assume "m \<le> n" "F, m \<Turnstile> (S', s') \<rhd>\<^sup>* Finished (PrimV (NatV nv))"

      show ?thesis
      proof (intro exI disjI1 conjI)
        from `m \<le> n` show "Suc m \<le> Suc n" ..

        from `F \<Turnstile> (S, s) \<rhd> Normal (S', s')`
             `F, m \<Turnstile> (S', s') \<rhd>\<^sup>* Finished (PrimV (NatV nv))`
        show "F, Suc m \<Turnstile> (S, s) \<rhd>\<^sup>* Finished (PrimV (NatV nv))" 
          by (rule StepN_add_head)
      qed
    next
      fix S'' s'' 
      assume "F, n \<Turnstile> (S', s') \<rhd>\<^sup>* Normal (S'', s'')" "WfProgram \<Psi> S'' s''"
      show ?thesis
      proof (intro exI disjI2 conjI)
        from `F \<Turnstile> (S, s) \<rhd> Normal (S', s')`
             `F, n \<Turnstile> (S', s') \<rhd>\<^sup>* Normal (S'', s'')`
        show "F, Suc n \<Turnstile> (S, s) \<rhd>\<^sup>* Normal (S'', s'')"
          by (rule StepN_add_head)
      qed fact
    qed
  qed
qed

lemma Initial_program:
  fixes \<Psi> :: "('fun, 'r :: {infinite}) funsT"
  defines "S \<equiv> (\<lparr> store = Map.empty, heap = [Map.empty], stack = [] \<rparr>)"
  defines "(\<gamma>::'r) \<equiv> fresh {}"
  assumes wff: "WfFuns F \<Psi>"
  and     main: "\<Psi> main = Some (FunT NAT [])"
  shows   "WfProgram \<Psi> S (Call x main [] (Return (Var x)))"
proof
  from main
  show "Map.empty, \<Psi>, \<gamma> \<turnstile> Call x main [] (Return (Var x)) : NAT, True"
    by (auto intro!: WfStmt.intros WfE.intros)
  
  show "WfState S Map.empty \<Psi> NAT True \<gamma>"
  proof
    let ?\<Delta> = "[\<gamma> \<mapsto> 0]"
    let ?\<Theta> = "[Map.empty]"

    show "WfStore ?\<Delta> ?\<Theta> (store S) Map.empty"
      by rule simp
    
    have "WfHeap ([] @ [Map.empty]) ([] @ [Map.empty])"
      apply (rule WfHeap.intros) 
      apply rule
      apply simp_all
      done
    thus "WfHeap (heap S) ?\<Theta>" unfolding S_def by simp

    show "WfStack \<Psi> ?\<Delta> ?\<Theta> (stack S) NAT True \<gamma>"
      unfolding S_def by simp (rule wfStackNil)
    
    show "WfFrees ?\<Delta> Map.empty \<gamma> (length [Map.empty] - 1)"
      by rule (simp_all add: tfrees_set_def)
  qed
qed

lemma Initial_program_result:
  fixes \<Psi> :: "('fun, 'r :: {infinite}) funsT"
  and main :: 'fun and x :: 'var
  defines "S \<equiv> (\<lparr> store = Map.empty, heap = [Map.empty], stack = [] \<rparr>)"
  defines "s \<equiv> Call x main [] (Return (Var x))"
  assumes wff: "WfFuns F \<Psi>"
  and     main: "\<Psi> main = Some (FunT NAT [])"
  obtains (Terminates) n nv where "F, n \<Turnstile> (S, s) \<rhd>\<^sup>* Finished (PrimV (NatV nv))"
         | (Diverges) "\<forall>n. (\<exists>S' s'. F, n \<Turnstile> (S, s) \<rhd>\<^sup>* Normal (S', s') \<and> WfProgram \<Psi> S' s')"
proof -
  from wff main have "WfProgram \<Psi> S s"
    unfolding S_def s_def
    by (rule Initial_program)

  have "\<forall>n. (\<exists>n nv. F, n \<Turnstile> (S, s) \<rhd>\<^sup>* Finished (PrimV (NatV nv))) 
            \<or> (\<exists>S' s'. F, n \<Turnstile> (S, s) \<rhd>\<^sup>* Normal (S', s') \<and> WfProgram \<Psi> S' s')"
    (is "\<forall>n. ?FINISHES n \<or> ?DIVERGES n")
  proof
    fix n
    
    from `WfProgram \<Psi> S s` wff
    have "(\<exists>m nv. m \<le> n \<and> F, m \<Turnstile> (S, s) \<rhd>\<^sup>* Finished (PrimV (NatV nv)) )
          \<or> (\<exists>S' s'. F, n \<Turnstile> (S, s) \<rhd>\<^sup>* Normal (S', s') \<and> WfProgram \<Psi> S' s')"
      by (rule Soundness)
    
    thus "?FINISHES n \<or> ?DIVERGES n"
    proof (elim exE disjE conjE)
      fix m nv
      assume "m \<le> n" "F, m \<Turnstile> (S, s) \<rhd>\<^sup>* Finished (PrimV (NatV nv))"
      hence "?FINISHES n" by auto
      thus ?thesis ..
    next
      fix S' s' 
      assume "F, n \<Turnstile> (S, s) \<rhd>\<^sup>* Normal (S', s')" "WfProgram \<Psi> S' s'"
      hence "?DIVERGES n" by auto
      thus ?thesis ..
    qed
  qed
  thus ?thesis
    by (auto intro: Terminates Diverges)
qed

end