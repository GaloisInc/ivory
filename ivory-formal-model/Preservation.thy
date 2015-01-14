(* The proof of the preservation property *)

theory Preservation
imports EvalSafe
begin

lemma ImpureExp_length:
  assumes eval: "st \<Turnstile> H, e \<Down> H', v"
  shows "length H' = length H"
  using eval
  by (induction rule: ImpureExpV.induct)
     (auto simp add: Let_def option_bind_Some_iff update_heap_length split: option.splits wvalue.splits)

(*
   and frees: "WfFrees \<Delta> \<Gamma> \<Theta>" "WfFrees \<Delta>' (\<Gamma>'(x \<mapsto> \<tau>)) \<gamma> (length (butlast \<Theta>) - 1)"
*)

(* MOVE: Lib *)
lemma bij_betw_remove:
  assumes bij: "bij_betw f A B"
  and     xin: "x \<in> A"
  shows   "bij_betw f (A - {x}) (B - {f x})"
  using bij xin unfolding bij_betw_def
  by (clarsimp simp add: inj_on_diff inj_on_image_set_diff)

lemma bij_betw_in:
  assumes bij: "bij_betw f A B"
  and     xin: "x \<in> A"
  shows   "f x \<in> B"
  using bij xin unfolding bij_betw_def by auto


lemma WfValue_return:
  fixes \<Delta> \<Theta>
  defines "\<rho> \<equiv> region_for \<Delta> (length (butlast \<Theta>))"
  assumes wfwv: "WfWValue \<Delta> \<Theta> v \<tau>"
  and      wff: "WfFrees \<Delta> \<Gamma> \<Theta>"
  and     notin: "\<rho> \<notin> tfrees \<tau>"
  and         d: "\<Delta>' = \<Delta> |` (- { \<rho> })"
  shows  "WfWValue \<Delta>' (butlast \<Theta>) v \<tau>" 
  using wfwv wff notin d unfolding \<rho>_def
proof induction
  case (wfRefV \<Delta> \<rho>' region \<Theta> off \<tau>)
 
  let ?n = "length (butlast \<Theta>)"
  let ?\<rho> = "region_for \<Delta> ?n"
 
  have wff: "WfFrees \<Delta> \<Gamma> \<Theta>" by fact

  show ?case
  proof    
    show "\<Delta>' \<rho>' = Some region" using wfRefV by clarsimp
    
    from wff have "bij_betw \<Delta> (dom \<Delta>) (Some ` { 0 .. length \<Theta> - Suc 0 })" ..
    hence "bij_betw \<Delta> (dom \<Delta> - {?\<rho>}) (Some ` { 0 .. ?n } - {\<Delta> ?\<rho>})"
      by (clarsimp dest!: bij_betw_remove [OF _ region_for_in_dom [OF wff order_refl]])

    moreover have "\<Delta>' = \<Delta> |` (- { ?\<rho> })" by fact
    moreover have "\<Delta>' = \<Delta> |` (- { ?\<rho> })" by fact
    with `?\<rho> \<notin> tfrees (RefT \<rho>' \<tau>)`  `\<Delta>' \<rho>' = Some region`
    have "\<rho>' \<in> dom \<Delta> - {?\<rho>}" 
      by (clarsimp simp: region_for_is_same [OF wff])
    
    ultimately have "region < length (butlast \<Theta>)"
      using `\<Delta>' \<rho>' = Some region`
      apply -
      apply (drule (1) bij_betw_in)
      apply (clarsimp simp: region_for_is_same [OF wff])
      done
    
    thus "lookup_heap (butlast \<Theta>) region off = Some \<tau>"
      using `lookup_heap \<Theta> region off = Some \<tau>`
      by (simp add: lookup_heap_Some_iff nth_butlast)
  qed
qed rule+


lemma subseteq_negate:
  "(X \<subseteq> - {y}) = (y \<notin> X)" by auto

lemma Collect_minus:
  "{x. P x} - A = {x. x \<notin> A \<and> P x}" by auto

lemma ran_map_upd:
  "ran (m(x \<mapsto> y)) = ran (m |` (- {x})) \<union> {y}"
  unfolding ran_def restrict_map_def by auto

lemma tfrees_set_insert [simp]:
  "tfrees_set (insert \<tau> \<Theta>) = tfrees_set \<Theta> \<union> tfrees \<tau>"
  unfolding tfrees_set_def by auto

lemma tfrees_set_fun_upd:
  "tfrees_set (ran (\<Gamma>(x \<mapsto> \<tau>))) = tfrees_set (ran (\<Gamma> |` (- {x}))) \<union> tfrees \<tau>"
  by (simp add: ran_map_upd)

lemma region_for_restrict:
  assumes wffr: "WfFrees \<Delta> \<Gamma> \<Theta>"
  and   notin: "\<forall>\<rho> \<in> S. \<Delta> \<rho> \<noteq> Some n"
  and      nv: "n \<le> length \<Theta> - 1"
  shows "region_for (\<Delta> |` (- S)) n = region_for \<Delta> n"
  unfolding region_for_def
proof (rule the1_equality)
  
  let ?m = "length \<Theta> - Suc 0"
  from wffr have bb: "bij_betw \<Delta> (dom \<Delta>) (Some ` {0 .. ?m})" ..

  have ex1: "\<exists>!x. x \<in> dom \<Delta> \<and> \<Delta> x = Some n" 
  proof (rule bij_betw_inv)
    show "Some n \<in> Some ` {0 .. ?m}" using nv by simp
    from wffr show "finite (dom \<Delta>)" ..
  qed fact
  
  with notin have "\<exists>!x. x \<in> dom (\<Delta> |` (- S)) \<and> (\<Delta> |` (- S)) x = Some n" 
    by (auto simp: restrict_out cong: conj_cong)
  thus "\<exists>!x. (\<Delta> |` (- S)) x = Some n" by fastforce  

  from ex1 have "\<exists>!x. \<Delta> x = Some n" by auto
  hence dn: "\<Delta> (THE \<rho>. \<Delta> \<rho> = Some n) = Some n" by (rule theI')
  hence "(THE \<rho>. \<Delta> \<rho> = Some n) \<in> - S" using notin by clarsimp    
  thus "(\<Delta> |` (- S)) (THE \<rho>. \<Delta> \<rho> = Some n) = Some n" using dn by simp
qed

lemma WfStack_heap_not_empty':
  assumes wfs: "WfStack \<Psi> \<Delta> \<Theta> st \<tau> b"
  shows "length \<Theta> > 0"
  using wfs
  by induction auto

lemma bij_betw_restrict:
  shows "bij_betw (m |` A) A (Some ` B) = bij_betw m A (Some ` B)"
  unfolding bij_betw_def 
  by (clarsimp simp: image_def cong: inj_on_cong)

lemma restrict_dom_restrict:
  "(m |` A) = (m |` (dom m \<inter> A))"
  unfolding restrict_map_def by auto

lemma bij_betw_restrict':
  assumes cv: "dom m \<inter> C = A"
  shows "bij_betw (m |` C) A (Some ` B) = bij_betw m A (Some ` B)"
  using cv
  apply clarsimp
  apply (subst restrict_dom_restrict)
  apply (rule bij_betw_restrict)
  done

lemma diff_le_is_0:
  "x \<le> x - Suc y \<Longrightarrow> x = 0" by simp

lemma region_for_update:
  assumes wff:  "WfFrees \<Delta> \<Gamma> \<Theta>"
  and   ltheta: "length \<Theta> > l" (* don't care what l is *)
  and     xnin: "x \<notin> dom \<Delta>"
  shows "region_for (\<Delta>(x \<mapsto> length \<Theta>)) (length \<Theta>) = x"
  unfolding region_for_def
proof (rule the1_equality)
  have "length \<Theta> \<notin> ran \<Delta>"
  proof
    assume "length \<Theta> \<in> ran \<Delta>"
    then obtain y where "\<Delta> y = Some (length \<Theta>)"
      by (clarsimp simp: ran_def dom_def)

    moreover from wff have "bij_betw \<Delta> (dom \<Delta>) (Some ` { 0 .. length \<Theta> - Suc 0 })" ..

    ultimately show False using ltheta
      by (auto dest!: bij_betw_in [OF _ domI] diff_le_is_0)
  qed

  thus "\<exists>!\<rho>. (\<Delta>(x \<mapsto> length \<Theta>)) \<rho> = Some (length \<Theta>)"
    by (clarsimp simp: ran_def)
qed simp
  
(* . *)

lemma restrict_dom [simp]:
  "m |` (dom m) = m"
  unfolding restrict_map_def
  by (auto simp: dom_def)

lemma restrict_map_complement:
  assumes xnin: "x \<notin> dom m"
  shows "(m |` (- {x})) = m"
  using xnin
  apply -
  apply (subst restrict_dom_restrict)
  apply (simp add: Diff_eq [symmetric])
  done

lemma bij_betw_fun_upd:
  assumes bij: "bij_betw f A B"
  and     vals: "x \<notin> A" "y \<notin> B"
  shows   "bij_betw (f(x := y)) (A \<union> {x}) (B \<union> {y})"
  using bij vals
  unfolding bij_betw_def
  by (auto intro: inj_on_fun_updI split: split_if_asm)


lemma ran_restrict_subset:
  "ran (m |` S) \<subseteq> ran m"
  unfolding ran_def
  by (auto simp: restrict_map_def)

(* We need an infinite set of fresh names here *)
  
lemma Preservation: 
  fixes \<tau> :: "'r :: {infinite} wtype"
  assumes wff: "WfFuns F \<Psi>"
  and    wfst: "WfState S \<Gamma> \<Psi> \<tau> b \<rho>"
  and     wfs: "\<Gamma>, \<Psi>, \<rho> \<turnstile> s : \<tau>, b"
  and    step: "F \<Turnstile> (S, s) \<rhd> Normal (S', s')"
  shows  "\<exists>\<Gamma>' \<tau>' b' \<gamma>. WfState S' \<Gamma>' \<Psi> \<tau>' b' \<gamma> \<and> \<Gamma>', \<Psi>, \<gamma> \<turnstile> s' : \<tau>', b'"
  using wfs step wfst wff
proof (induction arbitrary: S s')
  case (wfSkip \<Gamma> \<Psi> \<rho> \<tau> S s')

  from `F \<Turnstile> (S, Skip) \<rhd> Normal (S', s')` 
  obtain store' stack' where
    Sv': "S' = S \<lparr> store := store', stack := stack' \<rparr>"
    and stackv: "stack S = (store', s', SeqFrame) # stack' " 
    by (rule StepSkipE)

  with `WfState S \<Gamma> \<Psi> \<tau> False \<rho>` 
  obtain \<Theta> \<Delta> \<Gamma>' b' where
     "WfHeap \<Delta> (heap S) \<Theta>" 
     "WfStack \<Psi> \<Delta> \<Theta> stack' \<tau> b'"
     "WfStore \<Delta> \<Theta> store' \<Gamma>'"
     "tfrees_set (ran \<Gamma>') \<subseteq> dom \<Delta>"
     "WfFrees \<Delta> \<Gamma> \<Theta>"
     "\<Gamma>', \<Psi>, region_for \<Delta> (length \<Theta> - 1) \<turnstile> s' : \<tau>, b'"
    by (auto elim!: WfStateE WfStackSeqE)
  moreover from `tfrees_set (ran \<Gamma>') \<subseteq> dom \<Delta>` `WfFrees \<Delta> \<Gamma> \<Theta>`
  have "WfFrees \<Delta> \<Gamma>' \<Theta>"
    by (auto elim!: WfFreesE intro!: WfFrees)

  ultimately show ?case using Sv'
    by (auto intro: WfState intro!: exI)
next
  case (wfReturn \<Gamma> e \<tau> \<Psi> \<rho> b S s')

  from `F \<Turnstile> (S, Return e) \<rhd> Normal (S', s')`
  show ?case
  proof (cases rule: StepReturnE)
    case (SeqFrame st cont stack')
    with wfReturn show ?thesis
      by (clarsimp elim!: WfStateE WfStackSeqE)
         (auto intro: WfState intro!: WfStmt.intros exI)
  next
    case (ReturnFrame store' cont x stack' v)

    hence Sv': "S' = \<lparr>store = store'(x \<mapsto> v), heap = pop_heap (heap S), stack = stack'\<rparr>"
      and sv': "s' = cont" by simp_all

    from `WfState S \<Gamma> \<Psi> \<tau> b \<rho>` 
    obtain \<Theta> \<Delta> where "WfStore \<Delta> \<Theta> (store S) \<Gamma>" 
      and wfh: "WfHeap \<Delta> (heap S) \<Theta>"
      and "WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> b"
      and wff: "WfFrees \<Delta> \<Gamma> \<Theta>"      
      ..

    let ?\<gamma> = "region_for \<Delta> (length (butlast \<Theta>) - 1)"

    from `WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> b` `stack S = (store', cont, ReturnFrame x) # stack'`
    obtain \<tau>' b' \<Delta>' \<Gamma>' where "WfStack \<Psi> \<Delta>' (butlast \<Theta>) stack' \<tau>' b'"
      "WfStore \<Delta>' (butlast \<Theta>) store' \<Gamma>'" 
      "\<Gamma>'(x \<mapsto> \<tau>), \<Psi>, ?\<gamma> \<turnstile> cont : \<tau>', b'"
      "region_for \<Delta> (length (butlast \<Theta>)) \<notin> tfrees_set (ran (\<Gamma>'(x \<mapsto> \<tau>)))"
      "\<Delta>' = \<Delta> |` (- { region_for \<Delta> (length (butlast \<Theta>)) })"
      by (auto elim!: WfStackFunE simp: subseteq_negate)

    (* have delta': "\<Delta>' = \<Delta> |` (- {\<rho>})" by fact *)
    (* hence "\<Delta>' \<subseteq>\<^sub>m \<Delta>" by (simp add: restrict_map_le) *)

    show ?thesis
    proof (intro exI conjI)
      show "WfState S' (\<Gamma>'(x \<mapsto> \<tau>)) \<Psi> \<tau>' b' ?\<gamma>" unfolding Sv'
      proof (rule, simp_all del: One_nat_def)
        from `WfStore \<Delta>' (butlast \<Theta>) store' \<Gamma>'`
        show "WfStore \<Delta>' (butlast \<Theta>) (store'(x \<mapsto> v)) (\<Gamma>'(x \<mapsto> \<tau>))"
        proof (rule WfStore_upd) 
          from `WfStore \<Delta> \<Theta> (store S) \<Gamma>` `store S \<Turnstile> e \<down> v` `\<Gamma> \<turnstile> e : \<tau>` 
          have "WfWValue \<Delta> \<Theta> v \<tau>" by (auto elim: Expr_safeE)
          
          thus "WfWValue \<Delta>' (butlast \<Theta>) v \<tau>" 
          proof (rule WfValue_return)
            from `region_for \<Delta> (length (butlast \<Theta>)) \<notin> tfrees_set (ran (\<Gamma>'(x \<mapsto> \<tau>)))`
            show "region_for \<Delta> (length (butlast \<Theta>)) \<notin> tfrees \<tau>"
              by (simp add: tfrees_set_fun_upd)
          qed fact+
        qed

        from `WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> b` `WfHeap \<Delta> (heap S) \<Theta>`
        have "heap S \<noteq> []" by (rule WfStack_heap_not_empty)
        with `WfHeap \<Delta> (heap S) \<Theta>` `WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> b`
        show "WfHeap \<Delta>' (pop_heap (heap S)) (butlast \<Theta>)" (* This answers: Why can we remove the region and retain safety? *)
          sorry
(*
          
          apply -
          apply (clarsimp simp: pop_heap_def)
          apply (erule WfHeap.cases)
          apply simp
          apply (clarsimp simp: pop_heap_def)
          apply (erule WfStack.cases)
          apply clarsimp
          apply (erule WfHeap.cases, simp_all)[1]
          apply rule

          apply clarsimp
          apply (drule WfHeap_renv_restrict)
          apply (erule WfHeap_renv_mono)
          apply (rule restrict_mono)
          apply (simp add: subseteq_negate)
          

          defer
          apply clarsimp
          
 
          
          apply (erule WfHeap_renv_mono)
          apply (erule WfStack.cases)
          apply clarsimp
          apply (erule WfHeap_renv_mono)
          
          apply (auto simp: pop_heap_def elim!: WfHeap.cases)n
*)
        from `WfStack \<Psi> \<Delta>' (butlast \<Theta>) stack' \<tau>' b'` 
        have "length \<Theta> > 1" 
          apply -
          apply (drule WfStack_heap_not_empty')
          apply simp
          done

        moreover have "\<Delta>' = \<Delta> |` (- { region_for \<Delta> (length (butlast \<Theta>)) })" by fact
        ultimately show "region_for \<Delta> (length \<Theta> - 2) = region_for \<Delta>' (length (butlast \<Theta>) - 1)"
          by (clarsimp simp: region_for_restrict [OF wff] region_for_is_same [OF wff]  numeral_2_eq_2 )

        let ?\<rho> = "region_for \<Delta> (length (butlast \<Theta>))"
        let ?n = "length (butlast \<Theta>)"
        
        show "WfFrees \<Delta>' (\<Gamma>'(x \<mapsto> \<tau>)) (butlast \<Theta>)" 
        proof (rule WfFrees [OF _ _ _ _ refl])
          from wff have "bij_betw \<Delta> (dom \<Delta>) (Some ` { 0 .. length \<Theta> - Suc 0 })" ..
          hence "bij_betw \<Delta> (dom \<Delta> - {?\<rho>}) (Some ` { 0 .. ?n } - {\<Delta> ?\<rho>})"
            by (clarsimp dest!: bij_betw_remove [OF _ region_for_in_dom [OF wff order_refl]])
          moreover have "\<Delta>' = \<Delta> |` (- { ?\<rho> })" by fact
          moreover have "Some ` {0..length \<Theta> - Suc 0} \<inter> - {\<Delta> (region_for \<Delta> (length \<Theta> - Suc 0))} = (Some ` {0..length \<Theta> - Suc (Suc 0)})"
            using `length \<Theta> > 1`
            by (auto simp add: region_for_is_same [OF wff] Diff_eq [symmetric] image_def Collect_minus cong: conj_cong bex_cong)
          ultimately show "bij_betw \<Delta>' (dom \<Delta>') (Some ` {0..length (butlast \<Theta>) - 1})"
            by (simp add: bij_betw_restrict' Diff_eq)

          show "heap_frees (butlast \<Theta>) \<subseteq> dom \<Delta>'" sorry
          show "tfrees_set (ran (\<Gamma>'(x \<mapsto> \<tau>))) \<subseteq> dom \<Delta>'" sorry
          show "finite (dom \<Delta>')" sorry
        qed 
          
      qed fact

      from `\<Gamma>'(x \<mapsto> \<tau>), \<Psi>, ?\<gamma> \<turnstile> cont : \<tau>', b'` sv' show "\<Gamma>'(x \<mapsto> \<tau>), \<Psi>, ?\<gamma> \<turnstile> s' : \<tau>', b'" by simp
    qed 
  next
    case Finish
    thus ?thesis using wfReturn by simp
  qed
next
  case (wfBind \<Gamma> \<rho> e \<tau>' x \<Psi> s \<tau> b S s')

  from `F \<Turnstile> (S, Bind x e s) \<rhd> Normal (S', s')`
  obtain H' v' where
    eval: "store S \<Turnstile> heap S, e \<Down> H', v'"
    and Sv': "S' = S\<lparr>store := store S(x \<mapsto> v'), heap := H'\<rparr>"
    and ss': "s' = s"
    by cases
  
  show ?case
  proof (intro exI conjI)
    from `s' = s` show "\<Gamma>(x \<mapsto> \<tau>'), \<Psi>, \<rho> \<turnstile> s' : \<tau>, b" 
      by (rule ssubst) fact

    from `\<Gamma>, \<rho> \<turnstile>I e : \<tau>'` `WfState S \<Gamma> \<Psi> \<tau> b \<rho>` 
    obtain H'' \<Theta>' \<Delta> v'' where eval': "store S \<Turnstile> heap S, e \<Down> H'', v''" 
      and wfh': "WfHeap \<Delta> H'' \<Theta>'" 
      and wfs': "WfStore \<Delta> \<Theta>' (store S) \<Gamma>"
      and wfst': "WfStack \<Psi> \<Delta> \<Theta>' (stack S) \<tau> b"
      and wfwv': "WfWValue \<Delta> \<Theta>' v'' \<tau>'"
      and "WfFrees \<Delta> \<Gamma> \<Theta>'"
      and rhov: "\<rho> = region_for \<Delta> (length \<Theta>' - 1)"
      by (fastforce elim!: ImpureExpr_safe_stateE)

    moreover from `WfState S \<Gamma> \<Psi> \<tau> b \<rho>`
    have "heap S \<noteq> []" 
      by (rule WfStateE) (erule (1) WfStack_heap_not_empty)

    moreover
    from `WfFrees \<Delta> \<Gamma> \<Theta>'`
    have "WfFrees \<Delta> (\<Gamma>(x \<mapsto> \<tau>')) \<Theta>'"
    proof (rule WfFrees_upd_storeT)
      from `\<Gamma>, \<rho> \<turnstile>I e : \<tau>'`
      have "tfrees \<tau>' \<subseteq> tfrees_set (ran \<Gamma>) \<union> { \<rho> }"
        by (rule ImpureExpr_tfrees)
      with `\<rho> = region_for \<Delta> (length \<Theta>' - 1)` 
      show "tfrees \<tau>' \<subseteq> tfrees_set (ran \<Gamma>) \<union> { region_for \<Delta> (length \<Theta>' - 1) }"
        by simp
    qed 
    ultimately show "WfState S' (\<Gamma>(x \<mapsto> \<tau>')) \<Psi> \<tau> b \<rho>" 
      using wfst' Sv' eval eval' 
      by (auto intro!: WfStore_upd WfState dest!: ImpureExp_length)
  qed      
next
  case (wfIf \<Gamma> e \<Psi> \<rho> s\<^sub>1 \<tau> b s\<^sub>2 S s')

  from `F \<Turnstile> (S, stmt.If e s\<^sub>1 s\<^sub>2) \<rhd> Normal (S', s')`
  obtain bl where
    "store S \<Turnstile> e \<down> BoolV bl"
    and ss: "S' = S" "s' = (if bl then s\<^sub>1 else s\<^sub>2)"
    by cases

  show ?case
  proof (intro exI conjI)
    from wfIf.hyps ss show "\<Gamma>, \<Psi>, \<rho> \<turnstile> s' : \<tau>, b"
      by simp
    from `S'= S` show "WfState S' \<Gamma> \<Psi> \<tau> b \<rho>" by (rule ssubst) fact
  qed
next
  case (wfWhile \<Gamma> e\<^sub>I \<tau>' x e\<^sub>B e\<^sub>S \<Psi> \<rho> s \<tau> b S s')

  from `F \<Turnstile> (S, For x e\<^sub>I e\<^sub>B e\<^sub>S s) \<rhd> Normal (S', s')`
  obtain v where
    eval: "store S \<Turnstile> e\<^sub>I \<down> v"
    and ss: "S' = S\<lparr>store := store S(x \<mapsto> v)\<rparr>"
    "s' = stmt.If e\<^sub>B (s ;; For x e\<^sub>S e\<^sub>B e\<^sub>S s) Skip"
    by cases

  from `WfState S \<Gamma> \<Psi> \<tau> False \<rho>` 
  obtain \<Theta> \<Delta> where wfs: "WfStore \<Delta> \<Theta> (store S) \<Gamma>" 
    "WfHeap \<Delta> (heap S) \<Theta>" 
    "WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> False"
    "WfFrees \<Delta> \<Gamma> \<Theta>"
    "\<rho> = region_for \<Delta> (length \<Theta> - 1)"
    ..

  show ?case
  proof (intro conjI exI)
    show "\<Gamma>(x \<mapsto> \<tau>'), \<Psi>, \<rho> \<turnstile> s' : \<tau>, False" using ss wfWhile.hyps
      by (auto intro!: WfStmt.intros elim: WfStmt_weaken_returns)

    show "WfState S' (\<Gamma>(x \<mapsto> \<tau>')) \<Psi> \<tau> False \<rho>" 
    proof
      from `\<Gamma> \<turnstile> e\<^sub>I : \<tau>'` have "tfrees \<tau>' \<subseteq> tfrees_set (ran \<Gamma>)"
        by (rule Expr_tfrees)
      
      hence "tfrees \<tau>' \<subseteq> tfrees_set (ran \<Gamma>) \<union> {region_for \<Delta> (length \<Theta> - 1)}" by auto

      with `WfFrees \<Delta> \<Gamma> \<Theta>`
      show "WfFrees \<Delta> (\<Gamma>(x \<mapsto> \<tau>')) \<Theta>"
        by (rule WfFrees_upd_storeT)

      from wfs ss show "WfHeap \<Delta> (heap S') \<Theta>" 
        and "WfStack \<Psi> \<Delta> \<Theta> (stack S') \<tau> False" by simp_all
      
      from `WfStore \<Delta> \<Theta> (store S) \<Gamma>` `\<Gamma> \<turnstile> e\<^sub>I : \<tau>'` ss eval
      show "WfStore \<Delta> \<Theta> (store S') (\<Gamma>(x \<mapsto> \<tau>'))"
        by (auto intro!: WfStore_upd elim!: Expr_safeE)
    qed fact
  qed
next      
  case (wfSeq \<Gamma> \<Psi> \<rho> s\<^sub>1 \<tau> s\<^sub>2 b S s')

  with `F \<Turnstile> (S, s\<^sub>1 ;; s\<^sub>2) \<rhd> Normal (S', s')`
  have Sv': "S' = S\<lparr>stack := (store S, s\<^sub>2, SeqFrame) # stack S\<rparr>"
    and ss: "s' = s\<^sub>1"
    by (auto elim: StepSeqE)

  show ?case
  proof (intro exI conjI)
    from `WfState S \<Gamma> \<Psi> \<tau> b \<rho>` `\<Gamma>, \<Psi>, \<rho> \<turnstile> s\<^sub>2 : \<tau>, b`
    show "WfState S' \<Gamma> \<Psi> \<tau> False \<rho>" using Sv'
      by (auto elim!: WfStateE WfStackFunE elim!: WfFreesE 
        intro!: WfState WfStack.intros WfFrees elim: WfStmt_weaken_returns)
    show "\<Gamma>, \<Psi>, \<rho> \<turnstile> s' : \<tau>, False" using ss by (rule ssubst) fact
  qed
next
  case (wfCall \<Psi> f \<sigma> ts \<Gamma> \<theta> es x \<rho> s \<tau> b S s')

  from `F \<Turnstile> (S, Call x f es s) \<rhd> Normal (S', s')` 
  obtain args body where
    Sv': "S' = 
    \<lparr>store = [args [\<mapsto>] map (the \<circ> ExpV (store S)) es],
    heap = push_heap (heap S),
    stack = (store S, s, ReturnFrame x) # stack S\<rparr>"
    and Ff: "F f = Some (Func args body)"
    and largs1: "length args = length es"
    and all_eval: "\<forall>e\<in>set es. \<exists>y. store S \<Turnstile> e \<down> y"
    and ss: "s' = body"
    by (rule StepCallE) auto

  from `WfState S \<Gamma> \<Psi> \<tau> b \<rho>` Sv' ss
  obtain \<Theta> \<Delta> where
    wfs: "WfStore \<Delta> \<Theta> (store S) \<Gamma>"
    "WfHeap \<Delta> (heap S) \<Theta>"
    "WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> b"
    "WfFrees \<Delta> \<Gamma> \<Theta>"
    "\<rho> = region_for \<Delta> (length \<Theta> - 1)"
    by (auto elim!: WfStateE)
  
  let ?\<rho> = "region_for \<Delta> (length \<Theta> - 1)"

  from `WfFuns F \<Psi>` `F f = Some (Func args body)` `\<Psi> f = Some (FunT \<sigma> ts)`
  obtain \<gamma> where largs2: "length args = length ts"
    and "\<gamma> \<in> - tfrees_set (set ts)" 
    "[args [\<mapsto>] ts], \<Psi>, \<gamma> \<turnstile> body : \<sigma>, True"
    "tfrees \<sigma> \<subseteq> tfrees_set (set ts)"
    by (auto elim!: WfFuns.cases WfFunc.cases submap_stE)

  show ?case 
  proof (intro exI conjI)
    let ?\<gamma> = "fresh (dom \<Delta>)" (* We can always pick a new region var *)

    (* IMPORTANT: We don't care about the non-ts parts of \<theta> *)
    def pi_def: \<pi> \<equiv> "\<lambda>\<rho>. if \<rho> = \<gamma> then ?\<gamma> else if \<rho> \<in> tfrees_set (set ts) then \<theta> \<rho> else \<rho>"
    let ?\<Gamma> = "[args [\<mapsto>] map (tsubst \<pi>) ts]"
    (* FIXME: make this an actual variable *)
    let ?vs = "map (the \<circ> ExpV (store S)) es"
    let ?G = "[args [\<mapsto>] ?vs]"
    let ?\<Delta> = "(\<Delta>(?\<gamma> \<mapsto> length \<Theta>))"

    from `WfFrees \<Delta> \<Gamma> \<Theta>` have "finite (dom \<Delta>)"..
    hence "?\<gamma> \<notin> dom \<Delta>"
      by (rule fresh_not_in)

    have pi_gamma: "\<pi> \<gamma> = ?\<gamma>" unfolding pi_def by simp

    from `list_all2 (\<lambda>e \<tau>. \<Gamma> \<turnstile> e : tsubst \<theta> \<tau>) es ts` 
    have "list_all2 (\<lambda>e \<tau>. \<Gamma> \<turnstile> e : tsubst \<pi> \<tau>) es ts"
    proof (cases rule: list_all2_weaken)
      case (P i)
      thus "\<Gamma> \<turnstile> (es ! i) : tsubst \<pi> (ts ! i)" using `\<gamma> \<in> - tfrees_set (set ts)`
        unfolding pi_def
        apply -
        apply (clarsimp simp: all_set_conv_all_nth tfrees_set_conv_bex in_set_conv_nth split: split_if_asm split_if)
        apply (subst tsubst_cong [where \<theta>' = \<theta>])
        apply (auto simp:  tfrees_set_conv_bex cong: tsubst_cong)
        done
    qed

    from `[args [\<mapsto>] ts], \<Psi>, \<gamma> \<turnstile> body : \<sigma>, True`
    have "?\<Gamma>, \<Psi>, \<pi> \<gamma> \<turnstile> s' : tsubst \<pi> \<sigma>, True" using ss
      by (simp add: option_map_map_upds [symmetric]) (erule WfStmt_tsubst)
    thus "?\<Gamma>, \<Psi>, ?\<gamma> \<turnstile> s' : tsubst \<pi> \<sigma>, True" using pi_gamma by simp

    have delta_subm: "\<Delta> \<subseteq>\<^sub>m ?\<Delta>" using `?\<gamma> \<notin> dom \<Delta>`
      unfolding map_le_def by simp

    show "WfState S' ?\<Gamma> \<Psi> (tsubst \<pi> \<sigma>) True ?\<gamma>"
      unfolding Sv'
    proof (rule, simp_all)
      have "WfStore ?\<Delta> \<Theta> ?G ?\<Gamma>"
      proof (rule, rule submap_st_list_all2I)

        show "list_all2 (WfWValue ?\<Delta> \<Theta>) ?vs (map (tsubst \<pi>) ts)"
        proof (rule list_all2_all_nthI)
          show "length ?vs = length (map (tsubst \<pi>) ts)" using largs1 largs2 by simp
        next
          fix n
          assume "n < length (map (the \<circ> ExpV (store S)) es)"
          hence lts: "n < length es" "n < length ts" using largs1 largs2 by auto

          from `list_all2 (\<lambda>e \<tau>. \<Gamma> \<turnstile> e : tsubst \<pi> \<tau>) es ts` `n < length es`
          have "\<Gamma> \<turnstile> es ! n : tsubst \<pi> (ts ! n)" by (rule list_all2_nthD)
          with `WfStore \<Delta> \<Theta> (store S) \<Gamma>` obtain v where "store S \<Turnstile> es ! n \<down> v" "WfWValue \<Delta> \<Theta> v (tsubst \<pi> (ts ! n))" 
            by (auto elim!: Expr_safeE )
          thus "WfWValue ?\<Delta> \<Theta> (?vs ! n) (map (tsubst \<pi>) ts ! n)" 
            using lts delta_subm
            apply (simp add: nth_map )
            apply (erule (1) WfWValue_renv_mono)
            done
        qed

        show "length args = length (map (tsubst \<pi>) ts)" using largs2 by simp
        show "length args = length ?vs" using largs1 by simp
      qed 
      thus "WfStore ?\<Delta> (push_heap \<Theta>) ?G ?\<Gamma>" by (rule WfStore_push_heap)

      from `WfHeap \<Delta> (heap S) \<Theta>`
      have "WfHeap ?\<Delta> (heap S) \<Theta>"
        by (rule WfHeap_renv_mono) fact
      thus "WfHeap ?\<Delta> (push_heap (heap S)) (push_heap \<Theta>)"
        unfolding push_heap_def 
        by (rule wfHeapCons) simp_all
        
      have ts_delta: "\<pi> ` tfrees_set (set ts) \<subseteq> dom \<Delta>"
      proof -
        from `list_all2 (\<lambda>e \<tau>. \<Gamma> \<turnstile> e : tsubst \<pi> \<tau>) es ts`
        have "tfrees_set (set (map (tsubst \<pi>) ts)) \<subseteq> tfrees_set (ran \<Gamma>)"
          by (rule all_WfE_into_tfrees_set)
        moreover
        from `WfFrees \<Delta> \<Gamma> \<Theta>`  have "tfrees_set (ran \<Gamma>) \<subseteq> dom \<Delta>" ..
        ultimately show ?thesis
          by (simp add: tfrees_set_tsubst)
      qed

      from `WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> b`
      have "0 < length \<Theta>" by (rule WfStack_heap_not_empty')
      hence "length \<Theta> - Suc 0 < length \<Theta>" by simp

      show "WfFrees ?\<Delta> ?\<Gamma> (push_heap \<Theta>)"
      proof 
        show "tfrees_set (ran ?\<Gamma>) \<subseteq> dom ?\<Delta>" using ts_delta
          apply simp
          apply (rule order_trans)
          apply (rule tfrees_set_mono)
          apply (rule ran_map_upds)
          apply (auto simp add: tfrees_set_tsubst)
          done

        have "heap_frees (push_heap \<Theta>) = heap_frees \<Theta>"
          unfolding heap_frees_def push_heap_def
          by clarsimp
        also from `WfFrees \<Delta> \<Gamma> \<Theta>` have "... \<subseteq> dom \<Delta>" ..
        also have "... \<subseteq> dom (\<Delta>(fresh (dom \<Delta>) \<mapsto> length \<Theta>))"
          by clarsimp
        finally show "heap_frees (push_heap \<Theta>) \<subseteq> dom ?\<Delta>" .
               
        from `WfFrees \<Delta> \<Gamma> \<Theta>` have "bij_betw \<Delta> (dom \<Delta>) (Some ` {0..length \<Theta> - Suc 0})" ..
        with `length \<Theta> - Suc 0 < length \<Theta>`
        have "bij_betw ?\<Delta> (dom \<Delta> \<union> {fresh (dom \<Delta>)}) (Some ` {0..length \<Theta> - Suc 0} \<union> {Some (length \<Theta>)})" using `finite (dom \<Delta>)`
          apply -
          apply (erule (1) bij_betw_fun_upd [OF _ fresh_not_in])
          apply clarsimp
          done
        moreover have "(Some ` {0..length \<Theta> - Suc 0} \<union> {Some (length \<Theta>)}) = Some ` {0 .. length \<Theta>}"
          by auto
        ultimately show "bij_betw ?\<Delta> (dom ?\<Delta>) (Some ` {0..length (push_heap \<Theta>) - 1})"
          by simp
        
        from `finite (dom \<Delta>)` show "finite (dom ?\<Delta>)" by simp
        
        show "length (push_heap \<Theta>) - 1 = length (push_heap \<Theta>) - 1" ..
      qed
      
      from `length \<Theta> - Suc 0 < length \<Theta>` `WfFrees \<Delta> \<Gamma> \<Theta>` `?\<gamma> \<notin> dom \<Delta>` `finite (dom \<Delta>)`
      have "region_for ?\<Delta> (length \<Theta>) = ?\<gamma>"
        by (simp add: region_for_update [OF _ _ fresh_not_in])
      
      show "WfStack \<Psi> ?\<Delta> (push_heap \<Theta>) ((store S, s, ReturnFrame x) # stack S) (tsubst \<pi> \<sigma>) True"
        unfolding push_heap_def
      proof (rule wfStackFun)
        let ?\<Delta>' = "\<Delta>(fresh (dom \<Delta>) \<mapsto> length \<Theta>) |`
                     (- {region_for (\<Delta>(fresh (dom \<Delta>) \<mapsto> length \<Theta>)) (length \<Theta>)})"

        from `length \<Theta> - Suc 0 < length \<Theta>` `?\<gamma> \<notin> dom \<Delta>`
          `\<rho> = region_for \<Delta> (length \<Theta> - 1)` `WfFrees ?\<Delta> ?\<Gamma> (push_heap \<Theta>)`
        have "region_for (\<Delta>(fresh (dom \<Delta>) \<mapsto> length \<Theta>)) (length \<Theta> - 1) = \<rho>"
          apply -
          apply simp
          apply (subst region_for_restrict [where S = "{fresh (dom \<Delta>)}", symmetric])
             apply assumption
            apply clarsimp
           apply clarsimp
          apply (simp add: restrict_map_complement)
          done

        with `\<Gamma>(x \<mapsto> tsubst \<theta> \<sigma>), \<Psi>, \<rho> \<turnstile> s : \<tau>, b` 
             `tfrees \<sigma> \<subseteq> tfrees_set (set ts)`
             `\<gamma> \<in> - tfrees_set (set ts)`
        show "\<Gamma>(x \<mapsto> tsubst \<pi> \<sigma>), \<Psi>, region_for ?\<Delta> (length \<Theta> - 1) \<turnstile> s : \<tau>, b"
          unfolding pi_def
          apply (subst tsubst_cong [where \<theta>' = \<theta>])
          apply (auto simp:  tfrees_set_conv_bex)
          done

              
        from `WfFrees \<Delta> \<Gamma> \<Theta>` have "finite (dom \<Delta>)"..
        hence "?\<gamma> \<notin> dom \<Delta>"
          by (rule fresh_not_in)
        hence "\<Delta>(?\<gamma> := None) = \<Delta>" by auto
        with `region_for ?\<Delta> (length \<Theta>) = ?\<gamma>`
        show "\<Delta> = ?\<Delta>'" by (simp add: restrict_complement_singleton_eq )

        from `WfFrees \<Delta> \<Gamma> \<Theta>` have "tfrees_set (ran \<Gamma>) \<subseteq> dom \<Delta>" ..
        with `tfrees \<sigma> \<subseteq> tfrees_set (set ts)` ts_delta
        show "tfrees_set (ran (\<Gamma>(x \<mapsto> tsubst \<pi> \<sigma>))) \<subseteq> dom \<Delta>"
          apply (simp add: tfrees_set_fun_upd tfrees_tsubst)
          apply rule
           apply (erule subset_trans [OF tfrees_set_mono, rotated])
           apply (rule ran_restrict_subset)
          apply auto
        done           
      qed fact+

      from `region_for ?\<Delta> (length \<Theta>) = ?\<gamma>`
      show "?\<gamma> = region_for ?\<Delta> (length (push_heap \<Theta>) - Suc 0)"
        by simp
      
    qed 
  qed
qed

end