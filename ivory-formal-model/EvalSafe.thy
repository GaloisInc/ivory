(* Proofs about the safety of expression evaluation *)

theory EvalSafe
imports Semantics TypeSystemProps
begin

lemma Expr_safe:
  assumes wfe: "\<Gamma> \<turnstile> e : \<tau>"
  and     wfg: "WfStore \<Delta> \<Theta> G \<Gamma>"
  shows "\<exists>v. G \<Turnstile> e \<down> v \<and> WfWValue \<Delta> \<Theta> v \<tau>"
  using wfe wfg
proof induct
  case (wfVar \<Gamma> x \<tau>)
  have "WfStore \<Delta> \<Theta> G \<Gamma>" and "\<Gamma> x = Some \<tau>" by fact+
  then obtain v where "G x = Some v" and "WfWValue \<Delta> \<Theta> v \<tau>" 
    by (auto elim!: submap_stE WfStore.cases)
  thus ?case by simp
next
  case (wfBinCmp \<Gamma> e\<^sub>1 e\<^sub>2 bop)
  thus ?case
    by (clarsimp elim!: WfNatVE intro!: WfWValue.intros)
next
  case (wfBinOp \<Gamma> e\<^sub>1 e\<^sub>2 bop)
  thus ?case
    by (clarsimp elim!: WfNatVE intro!: WfWValue.intros)
qed (auto intro: WfWValue.intros)

lemma Expr_safeE:
  assumes wfe: "\<Gamma> \<turnstile> e : \<tau>"
  and     wfg: "WfStore \<Delta> \<Theta> G \<Gamma>"
  and     rl:  "\<And>v. \<lbrakk> G \<Turnstile> e \<down> v; WfWValue \<Delta> \<Theta> v \<tau> \<rbrakk> \<Longrightarrow> R"
  shows R
  using wfe wfg by (auto dest!: Expr_safe intro: rl)


lemma heap_frees_nil [simp]:
  "heap_frees [] = {}"
  unfolding heap_frees_def afrees_set_def by simp

lemma heap_frees_take:
  "heap_frees (take n \<Theta>) \<subseteq> heap_frees \<Theta>"
  unfolding heap_frees_def afrees_set_def 
  by (intro image_mono Union_mono set_take_subset)

lemma heap_frees_singleton [simp]:
  "heap_frees [\<Sigma>] = afrees_set (ran \<Sigma>)" unfolding heap_frees_def by simp

lemma afrees_set_mono:
  assumes ss: "S \<subseteq> S'"
  shows   "afrees_set S \<subseteq> afrees_set S'"
  using ss unfolding afrees_set_def
  by auto

lemma afrees_set_insert [simp]:
  "afrees_set (insert \<rho> S) = afrees \<rho> \<union> afrees_set S"
  unfolding afrees_set_def by simp

lemma afrees_set_empty [simp]:
  "afrees_set {} = {}"
  unfolding afrees_set_def by simp

lemma afrees_set_Un [simp]:
  "afrees_set (S \<union> S') = afrees_set S \<union> afrees_set S'"
  unfolding afrees_set_def by simp
  
lemma afrees_set_heap_frees_nth:
  assumes nl: "n < length \<Theta>"
  shows "afrees_set (ran (\<Theta> ! n)) \<subseteq> heap_frees \<Theta>"
  using nl unfolding heap_frees_def afrees_set_def
  by fastforce

lemma ImpureExpr_safeE:
  notes subheap_refl [intro]
  fixes \<tau> :: "'r wtype"
  assumes wfe: "\<Gamma>, \<rho> \<turnstile>I e : \<tau>"
  and   wfs: "WfStore \<Delta> \<Theta> st \<Gamma>" "WfHeap \<Delta> H \<Theta>" "H \<noteq> []" "WfFrees \<Delta> \<Gamma> \<rho> \<Theta>"
  obtains H' \<Theta>' v where "st \<Turnstile> H, e \<Down> H', v" "WfHeap \<Delta> H' \<Theta>'" "WfWValue \<Delta> \<Theta>' v \<tau>" "subheap \<Theta> \<Theta>'" "heap_frees \<Theta>' \<subseteq> heap_frees \<Theta> \<union> tfrees \<tau>" (* or \<subseteq> dom \<Delta>? *)
  using wfe wfs
proof (induction)
  case (wfPure \<Gamma> e \<tau> \<rho>)
  note that = wfPure.prems(1)

  have "\<Gamma> \<turnstile> e : \<tau>" by fact+
  then obtain v where "st \<Turnstile> e \<down> v" "WfWValue \<Delta> \<Theta> v \<tau>" using `WfStore \<Delta> \<Theta> st \<Gamma>`
    by (auto elim!: Expr_safeE )

  thus ?case using `WfHeap \<Delta> H \<Theta>`
    by (auto intro!: that)
next    
  case (wfNewRef \<Gamma> e \<tau> \<rho>)
  note that = wfNewRef.prems(1)

  from `\<Gamma> \<turnstile> e : \<tau>` `WfStore \<Delta> \<Theta> st \<Gamma>` obtain v where "st \<Turnstile> e \<down> v" "WfWValue \<Delta> \<Theta> v \<tau>" 
    by (auto elim!: Expr_safeE)

  show ?case
  proof (rule that)
    let ?region  = "(length H - 1)"
    let ?off     = "fresh_in_heap H ?region"
    let ?H'      = "take ?region H @ [(H ! ?region)(?off \<mapsto> StoredV v)]"
    let ?\<Theta>'      = "take ?region \<Theta> @ [(\<Theta> ! ?region)(?off \<mapsto> Stored \<tau>)]"

    from `WfHeap \<Delta> H \<Theta>` have fin: "finite (dom (lookup_heap H ?region))" 
      by (auto elim: WfHeap.cases) 

    with `WfHeap \<Delta> H \<Theta>` 
    have "?off \<notin> dom (lookup_heap \<Theta> ?region)"
      by (rule contra_subsetD [OF WfHeap_dom' fresh_in_heap_fresh])

    from `st \<Turnstile> e \<down> v` `H \<noteq> []`
    show "st \<Turnstile> H, NewRef e \<Down> ?H', RefV ?region ?off"
      by (clarsimp simp: Let_def update_heap_def)

    from `WfHeap \<Delta> H \<Theta>` have "length \<Theta> = length H" by (rule WfHeap_length [symmetric])
    with `H \<noteq> []` have "update_heap \<Theta> ?region ?off (Stored \<tau>) = Some ?\<Theta>'"
      by (auto simp add: update_heap_def diff_Suc_less)

    from `WfWValue \<Delta> \<Theta> v \<tau>` have "WfHValue \<Delta> \<Theta> (StoredV v) (Stored \<tau>)" ..
    with `WfHeap \<Delta> H \<Theta>` show "WfHeap \<Delta> ?H' ?\<Theta>'"
    proof (rule WfHeap_upd [OF _ _ refl])
      from `H \<noteq> []` show "update_heap H ?region ?off (StoredV v) = Some ?H'"
        by (simp add: update_heap_def)
      from fin show "?off \<notin> dom (lookup_heap H ?region)"
        by (rule fresh_in_heap_fresh)
    qed fact

    have notin: "?off \<notin> dom (lookup_heap \<Theta> ?region)" by fact
    show "subheap \<Theta> ?\<Theta>'" using `length \<Theta> = length H` `H \<noteq> []` 
      using subheap_take_drop [OF notin refl] by simp
    
    show "WfWValue \<Delta> ?\<Theta>' (RefV ?region ?off) (RefT \<rho> (Stored \<tau>))"
    proof      
      from `length \<Theta> = length H` 
      show "lookup_heap ?\<Theta>' ?region ?off = Some (Stored \<tau>)"
        by (auto simp: lookup_heap_Some_iff nth_append min_absorb2 )
          
      from `WfFrees \<Delta> \<Gamma> \<rho> \<Theta>` `length \<Theta> = length H` 
      show "\<Delta> \<rho> = Some ?region" by (clarsimp elim!: WfFreesE)
    qed
    
    from `length \<Theta> = length H` `H \<noteq> []` 
    show "heap_frees ?\<Theta>' \<subseteq> heap_frees \<Theta> \<union> tfrees (RefT \<rho> (Stored \<tau>))"
      apply (simp add: heap_frees_append heap_frees_take heap_frees_singleton del: Un_insert_left)
      apply rule
       apply (rule order_trans)
       apply (rule heap_frees_take)
       apply clarsimp
      apply (rule order_trans)
      apply (rule afrees_set_mono)
      apply (rule ran_map_upd_subset)
      apply (clarsimp simp del: Un_insert_right Un_insert_left)
      apply rule 
       apply (rule order_trans [OF afrees_set_heap_frees_nth ])
        apply simp
       apply clarsimp
      apply clarsimp
     done
  qed
next
  case (wfReadRef \<Gamma> e \<gamma> \<tau> \<rho>)
  note that = wfReadRef.prems(1)

  from `\<Gamma> \<turnstile> e : RefT \<gamma> (Stored \<tau>)` `WfStore \<Delta> \<Theta> st \<Gamma>`
  obtain v where "st \<Turnstile> e \<down> v" "WfWValue \<Delta> \<Theta> v (RefT \<gamma> (Stored \<tau>))"
    by (erule Expr_safeE)
  
  moreover from `WfWValue \<Delta> \<Theta> v (RefT \<gamma> (Stored \<tau>))` 
  obtain region off where "v = RefV region off" 
    "lookup_heap \<Theta> region off = Some (Stored \<tau>)" by (rule WfRefVE)

  from `WfHeap \<Delta> H \<Theta>` `lookup_heap \<Theta> region off = Some (Stored \<tau>)`
  obtain v' where "lookup_heap H region off = Some (StoredV v')" and "WfWValue \<Delta> (take (Suc region) \<Theta>) v' \<tau>" 
    by (auto elim!: WfHeap_inversionE WfHValue.cases)

  show ?case
  proof (rule that)
    from `st  \<Turnstile> e \<down> v` `v = RefV region off` `lookup_heap H region off = Some (StoredV v')`
    show "st \<Turnstile> H, ReadRef e \<Down> H, v'" by simp

    from `WfWValue \<Delta> (take (Suc region) \<Theta>) v' \<tau>` 
    have "WfWValue \<Delta> (take (Suc region) \<Theta> @ drop (Suc region) \<Theta>) v' \<tau>"
      by (rule WfWValue_heap_monotone)
    thus "WfWValue \<Delta> \<Theta> v' \<tau>" by simp

    show "heap_frees \<Theta> \<subseteq> heap_frees \<Theta> \<union> tfrees \<tau>" by simp
  qed fact+
next
  case (wfWriteRef \<Gamma> e\<^sub>2 \<tau> e\<^sub>1 \<gamma> \<rho>)
  note that = wfWriteRef.prems(1)

  from `\<Gamma> \<turnstile> e\<^sub>1 : RefT \<gamma> (Stored \<tau>)` `WfStore \<Delta> \<Theta> st \<Gamma>`
  obtain v where "st \<Turnstile> e\<^sub>1 \<down> v" "WfWValue \<Delta> \<Theta> v (RefT \<gamma> (Stored \<tau>))"
    by (erule Expr_safeE)
  
  from `WfWValue \<Delta> \<Theta> v (RefT \<gamma> (Stored \<tau>))` 
  obtain region off where "v = RefV region off" 
    "lookup_heap \<Theta> region off = Some (Stored \<tau>)" by (rule WfRefVE)

  from `\<Gamma> \<turnstile> e\<^sub>2 : \<tau>` `WfStore \<Delta> \<Theta> st \<Gamma>`
  obtain v' where "st \<Turnstile> e\<^sub>2 \<down> v'" "WfWValue \<Delta> \<Theta> v' \<tau>"
    by (auto elim!: Expr_safeE)
  
  from `WfHeap \<Delta> H \<Theta>` `lookup_heap \<Theta> region off = Some (Stored \<tau>)`
  obtain hv where "lookup_heap H region off = Some (StoredV hv)"
    by (auto elim!: WfHeap_inversionE WfHValue.cases)
  then obtain H' where "update_heap H region off (StoredV v') = Some H'"
    by (rule lookup_heap_into_update_heap_same)

  show ?case
  proof (rule that)
    from `st \<Turnstile> e\<^sub>1 \<down> v` `v = RefV region off` `st \<Turnstile> e\<^sub>2 \<down> v'`
      `update_heap H region off (StoredV v') = Some H'`
    show "st \<Turnstile> H, WriteRef e\<^sub>1 e\<^sub>2 \<Down> H', UnitV"
      by clarsimp
    
    show "WfWValue \<Delta> \<Theta> UnitV UnitT" by (intro WfWValue.intros)

    from `WfHeap \<Delta> H \<Theta>` `WfWValue \<Delta> \<Theta> v' \<tau>`
      `update_heap H region off (StoredV v') = Some H'`
      `lookup_heap \<Theta> region off = Some (Stored \<tau>)`
    show "WfHeap \<Delta> H' \<Theta>"
      by (rule WfHeap_upd_same_type [OF _ wfStoredV])

    show "heap_frees \<Theta> \<subseteq> heap_frees \<Theta> \<union> tfrees UNIT" by simp
  qed rule
qed


(* Combines WfStateE and ImpureExpr_safeE *)
lemma ImpureExpr_safe_stateE:
  notes subheap_refl [intro]
  assumes wfe: "\<Gamma>, \<rho> \<turnstile>I e : \<tau>"
  and     wfs: "WfState S \<Gamma> \<Psi> \<tau>' b \<rho>"
  obtains H' \<Theta>' \<Delta> v where "store S \<Turnstile> heap S, e \<Down> H', v" "WfHeap \<Delta> H' \<Theta>'" 
  "WfStore \<Delta> \<Theta>' (store S) \<Gamma>" "WfWValue \<Delta> \<Theta>' v \<tau>" "WfStack \<Psi> \<Delta> \<Theta>' (stack S) \<tau>' b \<rho>" 
  "WfFrees \<Delta> \<Gamma> \<rho> \<Theta>'" "heap_frees \<Theta>' \<subseteq> heap_frees \<Theta> \<union> tfrees \<tau>"
proof -
  from wfs obtain \<Theta> \<Delta> where
    "WfStore \<Delta> \<Theta> (store S) \<Gamma>"
    "WfHeap \<Delta> (heap S) \<Theta>" 
    "WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau>' b \<rho>"
    "WfFrees \<Delta> \<Gamma> \<rho> \<Theta>"
    ..
  moreover
  from `WfStack \<Psi> \<Delta> \<Theta> (stack S) \<tau>' b \<rho>` `WfHeap \<Delta> (heap S) \<Theta>` have "heap S \<noteq> []"
    by (rule WfStack_heap_not_empty)

  ultimately obtain H' \<Theta>' v where "store S \<Turnstile> heap S, e \<Down> H', v" "WfHeap \<Delta> H' \<Theta>'" 
    "WfWValue \<Delta> \<Theta>' v \<tau>" "subheap \<Theta> \<Theta>'" "heap_frees \<Theta>' \<subseteq> heap_frees \<Theta> \<union> tfrees \<tau>"
    using wfe
    by (auto elim!: ImpureExpr_safeE dest: WfHeap_length)

  from `subheap \<Theta> \<Theta>'` have "length \<Theta> = length \<Theta>'" by (rule subheap_lengthD)

  show ?thesis
  proof (rule that)
    show "WfStore \<Delta> \<Theta>' (store S) \<Gamma>" by (rule WfStore_heap_mono) fact+
    show "WfStack \<Psi> \<Delta> \<Theta>' (stack S) \<tau>' b \<rho>" by (rule WfStack_mono) fact+

    from `length \<Theta> = length \<Theta>'` show "WfFrees \<Delta> \<Gamma> \<rho> (length \<Theta>' - 1)" 
      by (rule subst) fact
  qed fact+
qed

end