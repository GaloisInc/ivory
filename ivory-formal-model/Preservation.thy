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

lemma WfValue_return:
  assumes wfwv: "WfWValue \<Delta> \<Theta> v \<tau>"
  and     frees: "WfFrees \<Delta>  \<Gamma> \<rho> (length \<Theta> - 1)" "WfFrees \<Delta>' (\<Gamma>'(x \<mapsto> \<tau>)) \<gamma> (length (butlast \<Theta>) - 1)" "\<Delta>' \<subseteq>\<^sub>m \<Delta>"
  and       len: "length \<Theta> > 1"
  shows   "WfWValue \<Delta>' (butlast \<Theta>) v \<tau>" 
  using wfwv frees len
proof induction
  case wfPrimV show ?case by rule fact
next
  case (wfRefV \<Delta> \<rho>' region \<Theta> off \<tau>)
 
  show ?case
  proof
    have "\<rho>' \<in> tfrees_set (ran (\<Gamma>'(x \<mapsto> RefT \<rho>' \<tau>)))"
      unfolding tfrees_set_def ran_def by auto
    also from `WfFrees \<Delta>' (\<Gamma>'(x \<mapsto> RefT \<rho>' \<tau>)) \<gamma> (length (butlast \<Theta>) - 1)` 
    have "... \<subseteq> dom \<Delta>'" by (auto elim!: WfFreesE)
    finally show "\<Delta>' \<rho>' = Some region"
      using `\<Delta>' \<subseteq>\<^sub>m \<Delta>` `\<Delta> \<rho>' = Some region`
      by (auto dest: map_leD)

    from `WfFrees \<Delta>' (\<Gamma>'(x \<mapsto> RefT \<rho>' \<tau>)) \<gamma> (length (butlast \<Theta>) - 1)` 
    have "\<forall>k \<in> ran \<Delta>'. k \<le> length (butlast \<Theta>) - 1" ..
    
    with `\<Delta>' \<rho>' = Some region` `length \<Theta> > 1`
    have "region < length (butlast \<Theta>)"
      by (auto simp: ran_def dest!: set_mp)
    
    thus "lookup_heap (butlast \<Theta>) region off = Some \<tau>"
      using `lookup_heap \<Theta> region off = Some \<tau>`
      by (simp add: lookup_heap_Some_iff nth_butlast)
  qed
qed


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
     "WfHeap (heap S) \<Theta>" 
     "WfStack \<Psi> \<Delta> \<Theta> stack' \<tau> b' \<rho>"
     "WfStore \<Delta> \<Theta> store' \<Gamma>'"
     "tfrees_set (ran \<Gamma>') \<subseteq> dom \<Delta>"
     "WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta> - 1)"
     "\<Gamma>', \<Psi>, \<rho> \<turnstile> s' : \<tau>, b'"
    by (auto elim!: WfStateE WfStackSeqE)
  moreover from `tfrees_set (ran \<Gamma>') \<subseteq> dom \<Delta>` `WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta> - 1)`
  have "WfFrees \<Delta> \<Gamma>' \<rho> (length \<Theta> - 1)"
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
      "WfHeap (heap S) \<Theta>" 
      "WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> b \<rho>"
      "WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta> - 1)"      
      ..

    from `WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> b \<rho>` `stack S = (store', cont, ReturnFrame x) # stack'`
    obtain \<tau>' b' \<Delta>' \<Gamma>' \<gamma> where "WfStack \<Psi> \<Delta>' (butlast \<Theta>) stack' \<tau>' b' \<gamma>"
      "WfStore \<Delta>' (butlast \<Theta>) store' \<Gamma>'" "\<Gamma>'(x \<mapsto> \<tau>), \<Psi>, \<gamma> \<turnstile> cont : \<tau>', b'" 
      "WfFrees \<Delta>' (\<Gamma>'(x \<mapsto> \<tau>)) \<gamma> (length (butlast \<Theta>) - 1)" "\<Delta>' \<subseteq>\<^sub>m \<Delta>"
      by (auto elim!: WfStackFunE)

    show ?thesis
    proof (intro exI conjI)
      show "WfState S' (\<Gamma>'(x \<mapsto> \<tau>)) \<Psi> \<tau>' b' \<gamma>" unfolding Sv'
      proof (rule, simp_all del: One_nat_def)
        from `WfStore \<Delta>' (butlast \<Theta>) store' \<Gamma>'`
        show "WfStore \<Delta>' (butlast \<Theta>) (store'(x \<mapsto> v)) (\<Gamma>'(x \<mapsto> \<tau>))"
        proof (rule WfStore_upd)
          from `WfStore \<Delta> \<Theta> (store S) \<Gamma>` `store S \<Turnstile> e \<down> v` `\<Gamma> \<turnstile> e : \<tau>` 
          have "WfWValue \<Delta> \<Theta> v \<tau>" by (auto elim: Expr_safeE)
          
          thus "WfWValue \<Delta>' (butlast \<Theta>) v \<tau>" 
          proof (rule WfValue_return)
            from `WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> b \<rho>` 
                 `stack S = (store', cont, ReturnFrame x) # stack'`
            show "1 < length \<Theta>" 
              by (auto dest: WfStack_heap_length)
          qed fact+
        qed

        from `WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> b \<rho>` `WfHeap (heap S) \<Theta>`
        have "heap S \<noteq> []" by (rule WfStack_heap_not_empty)
        with `WfHeap (heap S) \<Theta>` show "WfHeap (pop_heap (heap S)) (butlast \<Theta>)"
          by (auto simp: pop_heap_def elim: WfHeap.cases)
      qed fact+
        
      from `\<Gamma>'(x \<mapsto> \<tau>), \<Psi>, \<gamma> \<turnstile> cont : \<tau>', b'` sv' show "\<Gamma>'(x \<mapsto> \<tau>), \<Psi>, \<gamma> \<turnstile> s' : \<tau>', b'" by simp
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
      and wfh': "WfHeap H'' \<Theta>'" 
      and wfs': "WfStore \<Delta> \<Theta>' (store S) \<Gamma>"
      and wfst': "WfStack \<Psi> \<Delta> \<Theta>' (stack S) \<tau> b \<rho>"
      and wfwv': "WfWValue \<Delta> \<Theta>' v'' \<tau>'"
      and "WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta>' - 1)"
      by (fastforce elim!: ImpureExpr_safe_stateE)

    moreover from `WfState S \<Gamma> \<Psi> \<tau> b \<rho>`
    have "heap S \<noteq> []" 
      by (rule WfStateE) (erule (1) WfStack_heap_not_empty)

    moreover
    from `WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta>' - 1)`
    have "WfFrees \<Delta> (\<Gamma>(x \<mapsto> \<tau>')) \<rho> (length \<Theta>' - 1)"
    proof (rule WfFrees_upd_storeT)
      from `\<Gamma>, \<rho> \<turnstile>I e : \<tau>'` 
      show "tfrees \<tau>' \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}"
        by (rule ImpureExpr_tfrees)
    qed 
    ultimately show "WfState S' (\<Gamma>(x \<mapsto> \<tau>')) \<Psi> \<tau> b \<rho>" 
      using wfst' Sv' eval eval' 
      by (auto intro!: WfStore_upd WfState dest!: ImpureExp_length)
  qed      
next
  case (wfIf \<Gamma> e \<Psi> \<rho> s\<^sub>1 \<tau> b s\<^sub>2 S s')

  from `F \<Turnstile> (S, stmt.If e s\<^sub>1 s\<^sub>2) \<rhd> Normal (S', s')`
  obtain bl where
    "store S \<Turnstile> e \<down> PrimV (BoolV bl)"
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
    "WfHeap (heap S) \<Theta>" 
    "WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> False \<rho>"
    "WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta> - 1)"      
    ..

  show ?case
  proof (intro conjI exI)
    show "\<Gamma>(x \<mapsto> \<tau>'), \<Psi>, \<rho> \<turnstile> s' : \<tau>, False" using ss wfWhile.hyps
      by (auto intro!: WfStmt.intros elim: WfStmt_weaken_returns)

    show "WfState S' (\<Gamma>(x \<mapsto> \<tau>')) \<Psi> \<tau> False \<rho>" 
    proof
      from `\<Gamma> \<turnstile> e\<^sub>I : \<tau>'` have "tfrees \<tau>' \<subseteq> tfrees_set (ran \<Gamma>)"
        by (rule Expr_tfrees)
      
      hence "tfrees \<tau>' \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}" by auto
      with `WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta> - 1)`
      show "WfFrees \<Delta> (\<Gamma>(x \<mapsto> \<tau>')) \<rho> (length \<Theta> - 1)"
        by (rule WfFrees_upd_storeT)

      from wfs ss show "WfHeap (heap S') \<Theta>" 
        and "WfStack \<Psi> \<Delta> \<Theta> (stack S') \<tau> False \<rho>" by simp_all
      
      from `WfStore \<Delta> \<Theta> (store S) \<Gamma>` `\<Gamma> \<turnstile> e\<^sub>I : \<tau>'` ss eval
      show "WfStore \<Delta> \<Theta> (store S') (\<Gamma>(x \<mapsto> \<tau>'))"
        by (auto intro!: WfStore_upd elim!: Expr_safeE)
    qed 
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
      by (auto elim!: WfStateE WfStackFunE elim: WfFreesE  intro!: WfState WfStack.intros elim: WfStmt_weaken_returns)
    
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
    "WfHeap (heap S) \<Theta>"
    "WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau> b \<rho>"
    "WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta> - 1)"
    by (auto elim!: WfStateE)

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

    from `WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta> - 1)` have "finite (dom \<Delta>)"..
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

      from `WfHeap (heap S) \<Theta>`
      show "WfHeap (push_heap (heap S)) (push_heap \<Theta>)"
        unfolding push_heap_def 
        by (rule wfHeapCons) simp_all
        
      have ts_delta: "\<pi> ` tfrees_set (set ts) \<subseteq> dom \<Delta>"
      proof -
        from `list_all2 (\<lambda>e \<tau>. \<Gamma> \<turnstile> e : tsubst \<pi> \<tau>) es ts`
        have "tfrees_set (set (map (tsubst \<pi>) ts)) \<subseteq> tfrees_set (ran \<Gamma>)"
          by (rule all_WfE_into_tfrees_set)
        moreover
        from `WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta> - 1)`  have "tfrees_set (ran \<Gamma>) \<subseteq> dom \<Delta>" ..
        ultimately show ?thesis
          by (simp add: tfrees_set_tsubst)
      qed
      
      show "WfStack \<Psi> ?\<Delta> (push_heap \<Theta>) ((store S, s, ReturnFrame x) # stack S) (tsubst \<pi> \<sigma>) True ?\<gamma>"
        unfolding push_heap_def
      proof (rule wfStackFun)
        from `WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta> - 1)`
        show "WfFrees \<Delta> (\<Gamma>(x \<mapsto> tsubst \<pi> \<sigma>)) \<rho> (length \<Theta> - 1)"
        proof (rule WfFrees_upd_storeT)
          from `tfrees \<sigma> \<subseteq> tfrees_set (set ts)` 
          have "tfrees (tsubst \<pi> \<sigma>) \<subseteq> \<pi> ` tfrees_set (set ts)" by (auto simp: tfrees_tsubst)
          also have "...\<subseteq> tfrees_set (ran \<Gamma>)" 
            using `list_all2 (\<lambda>e \<tau>. \<Gamma> \<turnstile> e : tsubst \<pi> \<tau>) es ts` (* CLAG *)
            by (auto dest!: all_WfE_into_tfrees_set simp add: tfrees_set_tsubst)
          finally show "tfrees (tsubst \<pi> \<sigma>) \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}"
            by auto
        qed

        from `\<Gamma>(x \<mapsto> tsubst \<theta> \<sigma>), \<Psi>, \<rho> \<turnstile> s : \<tau>, b` 
             `tfrees \<sigma> \<subseteq> tfrees_set (set ts)`
             `\<gamma> \<in> - tfrees_set (set ts)`
        show "\<Gamma>(x \<mapsto> tsubst \<pi> \<sigma>), \<Psi>, \<rho> \<turnstile> s : \<tau>, b"
          unfolding pi_def
          by (subst tsubst_cong [where \<theta>' = \<theta>]) (auto simp:  tfrees_set_conv_bex)
      qed fact+

      show "WfFrees ?\<Delta> ?\<Gamma> ?\<gamma> (length (push_heap \<Theta>) - Suc 0)"
      proof 
        show "tfrees_set (ran ?\<Gamma>) \<subseteq> dom ?\<Delta>" using ts_delta
          apply simp
          apply (rule order_trans)
          apply (rule tfrees_set_mono)
          apply (rule ran_map_upds)
          apply (auto simp add: tfrees_set_tsubst)
          done

        show "?\<Delta> ?\<gamma> = Some (length (push_heap \<Theta>) - Suc 0)" by simp

        from `finite (dom \<Delta>)` show "finite (dom ?\<Delta>)" by simp

        from `WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta> - 1)` have "\<forall>k \<in> ran \<Delta>. k \<le> length \<Theta> - 1" ..
        thus "\<forall>k \<in> ran ?\<Delta>. k \<le> length (push_heap \<Theta>) - Suc 0" using `?\<gamma> \<notin> dom \<Delta>`
          by (auto simp: domIff)
      qed
    qed
  qed
qed

end