(* Properties of type system judgements *)

theory TypeSystemProps
imports TypeSystem
begin

section {* Type system properties *}

subsection {* General properties *}

lemma WfHeap_length:
  assumes wfh: "WfHeap H \<Theta>"
  shows "length H = length \<Theta>"
  using wfh
  by induct auto

lemma WfHeap_dom:
  assumes wfh: "WfHeap H \<Theta>"
  and   nv: "n < length H"
  shows "dom (\<Theta> ! n) \<subseteq> dom (H ! n)"
  using wfh nv WfHeap_length [OF wfh]
  by induct (auto simp: nth_append not_less dest!: submap_st_dom)

lemma WfHeap_dom':
  assumes wfh: "WfHeap H \<Theta>"
  shows "dom (lookup_heap \<Theta> n) \<subseteq> dom (lookup_heap H n)"
  using wfh WfHeap_length [OF wfh]
  by induct (auto simp: nth_append not_less lookup_heap_def dest!: submap_st_dom split: split_if_asm)

lemma WfStack_heap_length:
  assumes wfst: "WfStack \<Psi> \<Delta> \<Theta> st \<tau> b \<rho>"
  shows   "length \<Theta> = Suc (length (filter (\<lambda>(_, _, f). isReturnFrame f) st))"
  using wfst 
  by induct auto

lemma WfStack_heap_not_empty:
  assumes wfst: "WfStack \<Psi> \<Delta> \<Theta> st \<tau> b \<rho>"
  and      wfh: "WfHeap H \<Theta>"
  shows "H \<noteq> []"
  using wfst wfh
  by (auto dest!: WfStack_heap_length WfHeap_length)


lemma WfFrees_domD:
  assumes wfe: "WfFrees \<Delta> \<Gamma> \<rho> n"
  shows "\<rho> \<in> dom \<Delta>"
  using wfe
  by (rule WfFreesE, auto)

lemma WfStore_lift_weak:
  assumes wfst: "WfStore \<Delta> \<Theta> st \<Gamma>"
  and   rl: "\<And>v \<tau>. WfWValue \<Delta> \<Theta> v \<tau> \<Longrightarrow> WfWValue \<Delta>' \<Theta>' v \<tau>"
  shows "WfStore \<Delta>' \<Theta>' st \<Gamma>"
  using wfst
  apply (clarsimp elim!: WfStore.cases intro!: WfStore.intros)
  apply (erule submap_st_weaken)
  apply (erule rl)
  done

lemma WfHeap_inversionE:
  assumes wfh: "WfHeap H \<Theta>"
  and     lup: "lookup_heap \<Theta> region off = Some \<tau>"
  obtains v where "lookup_heap H region off = Some v" and "WfHValue v \<tau>"
  using wfh lup
proof (induction arbitrary: thesis)
  case wfHeapNil thus ?case by simp
next
  case (wfHeapCons H \<Theta> \<Sigma> R )

  note heap_len = WfHeap_length [OF wfHeapCons.hyps(1)]

  show ?case
  proof (cases "region = length H")
    case True
    thus ?thesis using wfHeapCons.prems wfHeapCons.hyps heap_len
      by (auto simp add: lookup_heap_Some_iff nth_append elim!: submap_stE) 
  next
    case False
    with wfHeapCons.prems heap_len have "lookup_heap \<Theta> region off = Some \<tau>"
      by (clarsimp simp: lookup_heap_Some_iff nth_append )

    then obtain v where "lookup_heap H region off = Some v" and
      "WfHValue v \<tau>" by (rule wfHeapCons.IH [rotated])

    show ?thesis
    proof (rule wfHeapCons.prems(1))
      from `lookup_heap H region off = Some v`
      show "lookup_heap (H @ [R]) region off = Some v"
        by (simp add: lookup_heap_Some_iff nth_append)
    qed fact
  qed
qed

subsection {* Returns tag weakening *}

lemma WfStmt_weaken_returns:
  assumes wfs: "\<Gamma>, \<Psi>, \<rho> \<turnstile> s : \<tau>, b"
  and  brl: "b' \<longrightarrow> b"
  shows "\<Gamma>, \<Psi>, \<rho> \<turnstile> s : \<tau>, b'"
  using wfs brl
  by (induct arbitrary: b') (auto intro: WfStmt.intros)

lemma WfStack_weaken_returns:
  assumes wfst: "WfStack \<Psi> \<Delta> \<Theta> st \<tau> b' \<rho>"
  and  brl: "b' \<longrightarrow> b"
  shows "WfStack \<Psi> \<Delta> \<Theta> st \<tau> b \<rho>"
  using wfst brl
  by induct (auto intro!: WfStack.intros elim: WfStmt_weaken_returns)

subsection {* Type substitution and free type variables *}

lemma tsubst_twice:
  "tsubst \<theta> (tsubst \<theta>' \<tau>) = tsubst (\<theta> \<circ> \<theta>') \<tau>"
  by (induct \<tau>) simp_all

lemma tfrees_tsubst:
  "tfrees (tsubst \<theta> \<tau>) = \<theta> ` tfrees \<tau>"
  by (cases \<tau>, simp_all)

lemma tfrees_set_tsubst:
  "tfrees_set (tsubst \<theta> ` S) = \<theta> ` tfrees_set S"
  unfolding tfrees_set_def 
  by (auto simp: tfrees_tsubst)

(* Only true if we terminate, which is a whole pain to show *)
lemma tfrees_set_Un:
  "tfrees_set (S \<union> S') = tfrees_set S \<union> tfrees_set S'"
  unfolding tfrees_set_def by simp

lemma tfrees_set_singleton [simp]:
  "tfrees_set {\<tau>} = tfrees \<tau>"
  unfolding tfrees_set_def by simp

lemma tsubst_cong:
  "\<lbrakk>(\<And>x. x \<in> tfrees \<tau> \<Longrightarrow> \<theta> x = \<theta>' x); \<tau> = \<tau>' \<rbrakk> \<Longrightarrow> tsubst \<theta> \<tau> = tsubst \<theta>' \<tau>'"
  by (induct \<tau>) auto

lemma tfrees_set_conv_bex:
  "(x \<in> tfrees_set S) = (\<exists>\<tau> \<in> S. x \<in> tfrees \<tau>)"
  unfolding tfrees_set_def by auto
subsection {* Type judgements and free variables *}

lemma Expr_tfrees:
  assumes wf: "\<Gamma> \<turnstile> e : \<tau>"
  shows  "tfrees \<tau> \<subseteq> tfrees_set (ran \<Gamma>)"
  using wf
  by induction (auto simp: tfrees_set_def ran_def)

lemma ImpureExpr_tfrees:
  assumes wf: "\<Gamma>, \<rho> \<turnstile>I e : \<tau>"
  shows  "tfrees \<tau> \<subseteq> (tfrees_set (ran \<Gamma>) \<union> {\<rho>})"
  using wf
  by (induction) (auto dest: Expr_tfrees)

lemma tfrees_update_storeT:
  assumes "\<Gamma> \<turnstile> e : \<tau>"
  shows   "tfrees_set (ran (\<Gamma>(x \<mapsto> \<tau>))) \<subseteq> tfrees_set (ran \<Gamma>)" 
proof -
  from `\<Gamma> \<turnstile> e : \<tau>` have t_sub: "tfrees \<tau> \<subseteq> tfrees_set (ran \<Gamma>)" by (rule Expr_tfrees)

  have "tfrees_set (ran (\<Gamma>(x \<mapsto> \<tau>))) \<subseteq> tfrees_set (ran \<Gamma> \<union> {\<tau>})"
    by (auto simp: tfrees_set_def dest!: set_mp [OF ran_map_upd_subset])
  also have "... = tfrees_set (ran \<Gamma>)" using t_sub
    by (auto simp: tfrees_set_def)
  finally show ?thesis .
qed

lemma tfrees_update_storeT':
  assumes "\<Gamma>, \<rho> \<turnstile>I e : \<tau>"
  shows   "tfrees_set (ran (\<Gamma>(x \<mapsto> \<tau>))) \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}" 
proof -
  from `\<Gamma>, \<rho> \<turnstile>I e : \<tau>` have t_sub: "tfrees \<tau> \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}" by (rule ImpureExpr_tfrees)

  have "tfrees_set (ran (\<Gamma>(x \<mapsto> \<tau>))) \<subseteq> tfrees_set (ran \<Gamma> \<union> {\<tau>})"
    by (auto simp: tfrees_set_def dest!: set_mp [OF ran_map_upd_subset])
  also have "... \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}" using t_sub
    by (auto simp: tfrees_set_def)
  finally show ?thesis .
qed

lemma all_WfE_into_tfrees_set:
  assumes lall: "list_all2 (\<lambda>e \<tau>. \<Gamma> \<turnstile> e : tsubst \<theta> \<tau>) es ts"
  shows "tfrees_set (set (map (tsubst \<theta>) ts)) \<subseteq> tfrees_set (ran \<Gamma>)"
proof -
  {
    fix i
    assume "i < length ts"
    with lall have "\<Gamma> \<turnstile> es ! i : tsubst \<theta> (ts ! i)" 
      by (rule list_all2_nthD2)
    hence "tfrees (tsubst \<theta> (ts ! i)) \<subseteq> tfrees_set (ran \<Gamma>)"
      by (rule Expr_tfrees)
  } thus ?thesis unfolding tfrees_set_def
    by (fastforce simp: list_all2_conv_all_nth in_set_conv_nth)
qed

(* Not used but still maybe interesting 
lemma WfStmt_ret_tfree_subset0:
  assumes wfs: "\<Gamma>, \<Psi>, \<rho> \<turnstile> s : \<tau>, b"
  and    wff: "WfFuns F \<Psi>"
  and     b: "b"
  shows   "tfrees \<tau> \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}"
  using wfs b wff
proof induction
  case (wfBind \<Gamma> \<rho> e \<tau>' v \<Psi> s \<tau> b)

  from `\<Gamma>, \<rho> \<turnstile>I e : \<tau>'`
  have t_sub: "tfrees \<tau>' \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}" by (rule ImpureExpr_tfrees)
  
  have "tfrees \<tau> \<subseteq> tfrees_set (ran (\<Gamma>(v \<mapsto> \<tau>'))) \<union> {\<rho>}" by (rule wfBind.IH) fact+
  also have "... \<subseteq> tfrees_set (ran \<Gamma> \<union> {\<tau>'}) \<union> {\<rho>}"
    by (auto simp: tfrees_set_def dest!: set_mp [OF ran_map_upd_subset])
  also have "... \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}" using t_sub
    by (auto simp: tfrees_set_def)
  finally show ?case .
next
  case (wfCall \<Psi> f \<sigma> ts \<Gamma> \<theta> es x \<rho> s \<tau> b)

  have sigma_sub: "tfrees (tsubst \<theta> \<sigma>) \<subseteq> tfrees_set (ran \<Gamma>)" 
  proof -
    from `\<Psi> f = Some (FunT \<sigma> ts)` `WfFuns F \<Psi>`
    have "tfrees \<sigma> \<subseteq> tfrees_set (set ts)"
      by (auto elim!: WfFuns.cases submap_stE WfFunc.cases)
    hence "tfrees (tsubst \<theta> \<sigma>) \<subseteq> tfrees_set (set (map (tsubst \<theta>) ts))"
      by (simp add: tfrees_tsubst tfrees_set_tsubst image_mono)
    also have "... \<subseteq> tfrees_set (ran \<Gamma>)"
      by (rule all_WfE_into_tfrees_set) fact
    finally show ?thesis .
  qed

  have "tfrees \<tau> \<subseteq> tfrees_set (ran (\<Gamma>(x \<mapsto> tsubst \<theta> \<sigma>))) \<union> {\<rho>}" 
    by (rule wfCall.IH) fact+
  also have "... \<subseteq> tfrees_set (ran \<Gamma> \<union> {tsubst \<theta> \<sigma>}) \<union> {\<rho>}"
    by (auto simp: tfrees_set_def dest!: set_mp [OF ran_map_upd_subset])
  also have "... \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}" using sigma_sub
    by (auto simp add: tfrees_set_Un tfrees_tsubst simp del: Un_insert_right)
  finally show ?case .
qed (auto dest: Expr_tfrees)
*)  

lemma tfrees_set_mono:
  assumes ss: "S \<subseteq> S'"
  shows   "tfrees_set S \<subseteq> tfrees_set S'"
  using ss
  unfolding tfrees_set_def
  by auto

subsection {* Type judgements and substitution *}

lemma WfExpr_tsubst:
  assumes wf: "\<Gamma> \<turnstile> e : \<tau>"
  shows  "(Option.map (tsubst \<theta>) \<circ> \<Gamma>) \<turnstile> e : tsubst \<theta> \<tau>"
  using wf
  by induction (auto intro: WfE.intros)

lemma WfInit_tsubst:
  notes o_apply [simp del]
  assumes wf: "\<Gamma> \<turnstile>0 i : \<alpha>"
  shows   "(Option.map (tsubst \<theta>) \<circ> \<Gamma>) \<turnstile>0 i : \<alpha>"
  using wf
  by induction (auto intro!: WfInit.intros dest: WfExpr_tsubst)

lemma WfImpureExpr_tsubst:
  notes o_apply [simp del]
  assumes wf: "\<Gamma>, \<rho> \<turnstile>I e : \<tau>"
  shows  "(Option.map (tsubst \<theta>) \<circ> \<Gamma>), (\<theta> \<rho>) \<turnstile>I e : tsubst \<theta> \<tau>"
  using wf
  by induction (auto intro!: WfImpureExpr.intros dest: WfExpr_tsubst WfInit_tsubst)

lemmas WfExpr_tsubst_Prim = WfExpr_tsubst [where \<tau> = "Prim \<tau>", simplified, standard]

lemma WfStmt_tsubst:
  notes o_apply [simp del]
  assumes wfs: "\<Gamma>, \<Psi>, \<rho> \<turnstile> e : \<tau>, b"
  shows  "(Option.map (tsubst \<theta>) \<circ> \<Gamma>), \<Psi>, \<theta> \<rho> \<turnstile> e : tsubst \<theta> \<tau>, b"
  using wfs 
proof (induction )
  note [simp del] =  option_map_o_map_upd fun_upd_apply
  case (wfBind \<Gamma> \<rho> e \<tau>' v \<Psi> s \<tau> b)

  from `\<Gamma>, \<rho> \<turnstile>I e : \<tau>'`
  have t_sub: "tfrees \<tau>' \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}" by (rule ImpureExpr_tfrees)
  
  show ?case
  proof 
    have "Option.map (tsubst \<theta>) \<circ> \<Gamma>(v \<mapsto> \<tau>'), \<Psi>, \<theta> \<rho> \<turnstile> s : tsubst \<theta> \<tau>, b"
      by (rule wfBind.IH) 
    thus "(Option.map (tsubst \<theta>) \<circ> \<Gamma>)(v \<mapsto> tsubst \<theta> \<tau>'), \<Psi>, \<theta> \<rho> \<turnstile> s : tsubst \<theta> \<tau>, b"
      by (simp add: option_map_o_map_upd)

    from `\<Gamma>, \<rho> \<turnstile>I e : \<tau>'`
    show "Option.map (tsubst \<theta>) \<circ> \<Gamma>, \<theta> \<rho> \<turnstile>I e : tsubst \<theta> \<tau>'"
      by (rule WfImpureExpr_tsubst)
  qed
next
  case (wfWhile \<Gamma> e\<^sub>I \<tau>' v e\<^sub>B e\<^sub>S \<Psi> \<rho> s \<tau> b)
  note [simp del] =  option_map_o_map_upd fun_upd_apply
  
  show ?case using wfWhile.hyps
  proof (intro WfStmt.intros)
    let ?\<Gamma> = "(Option.map (tsubst \<theta>) \<circ> \<Gamma>)"

    from `\<Gamma> \<turnstile> e\<^sub>I : \<tau>'` 
    have t_sub: "tfrees \<tau>' \<subseteq> tfrees_set (ran \<Gamma>)" by (rule Expr_tfrees)

    have "Option.map (tsubst \<theta>) \<circ> \<Gamma>(v \<mapsto> \<tau>'), \<Psi>, \<theta> \<rho> \<turnstile> s : tsubst \<theta> \<tau>, b"
      by (rule wfWhile.IH)
    thus "?\<Gamma>(v \<mapsto> tsubst \<theta> \<tau>'), \<Psi>, \<theta> \<rho> \<turnstile> s : tsubst \<theta> \<tau>, b" 
      by (simp add: option_map_o_map_upd)
  qed (auto simp: option_map_o_map_upd [symmetric] 
           intro: WfExpr_tsubst_Prim WfExpr_tsubst)
next
  case (wfCall \<Psi> f \<sigma> ts \<Gamma> \<theta>' es x \<rho> s \<tau> b)

  note [simp del] = option_map_o_map_upd fun_upd_apply

  let ?both = "(\<theta> \<circ> \<theta>')"

  from `\<Psi> f = Some (FunT \<sigma> ts)`
  show ?case
  proof
    from `list_all2 (\<lambda>e \<tau>. \<Gamma> \<turnstile> e : tsubst \<theta>' \<tau>) es ts`
    show "list_all2 (\<lambda>e \<tau>. Option.map (tsubst \<theta>) \<circ> \<Gamma> \<turnstile> e : tsubst ?both \<tau>) es ts"
    proof 
      fix e' \<tau>'
      assume "\<Gamma> \<turnstile> e' : tsubst \<theta>' \<tau>'"
      thus "Option.map (tsubst \<theta>) \<circ> \<Gamma> \<turnstile> e' : tsubst ?both \<tau>'"
        by - (drule WfExpr_tsubst, simp add: tsubst_twice)
    qed 
    have "Option.map (tsubst \<theta>) \<circ> \<Gamma>(x \<mapsto> tsubst \<theta>' \<sigma>), \<Psi>, \<theta> \<rho> \<turnstile> s : tsubst \<theta> \<tau>, b"
      by (rule wfCall.IH)
    thus "(Option.map (tsubst \<theta>) \<circ> \<Gamma>)(x \<mapsto> tsubst (\<theta> \<circ> \<theta>') \<sigma>), \<Psi>, \<theta> \<rho> \<turnstile> s : tsubst \<theta> \<tau>, b"
      by (simp add: option_map_o_map_upd tsubst_twice)
  qed
qed (auto simp add: tsubst_twice 
             intro: WfStmt.intros
              dest: WfExpr_tsubst WfImpureExpr_tsubst
                    WfExpr_tsubst_Prim)


subsection {* Type judgements and store (type) updates *}

lemma WfStore_upd:
  assumes wfst: "WfStore \<Delta> \<Theta> G \<Gamma>"
  and     wfwv: "WfWValue \<Delta> \<Theta> v \<tau>"
  shows   "WfStore \<Delta> \<Theta> (G(x \<mapsto> v)) (\<Gamma>(x \<mapsto> \<tau>))"
  using wfst wfwv
  by (auto elim!: WfStore.cases  submap_st_update intro!: WfStore)

lemma WfFrees_upd_storeT:
  assumes wffr: "WfFrees \<Delta> \<Gamma> \<rho> n"
  and     t_sub:  "tfrees \<tau> \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}"
  shows "WfFrees \<Delta> (\<Gamma>(x \<mapsto> \<tau>)) \<rho> n"
  using wffr
proof (rule WfFreesE, intro WfFrees)
  assume "\<Delta> \<rho> = Some n" 
    "\<forall>k \<in> ran \<Delta>. k \<le> n" 
    "tfrees_set (ran \<Gamma>) \<subseteq> dom \<Delta>" "finite (dom \<Delta>)"

  thus "\<Delta> \<rho> = Some n" and "\<forall>k \<in> ran \<Delta>. k \<le> n" 
    and "finite (dom \<Delta>)" by simp_all
  
  have "tfrees_set (ran (\<Gamma>(x \<mapsto> \<tau>))) \<subseteq> tfrees_set (ran \<Gamma> \<union> {\<tau>})"
    by (auto simp: tfrees_set_def dest!: set_mp [OF ran_map_upd_subset])
  also have "... \<subseteq> tfrees_set (ran \<Gamma>) \<union> {\<rho>}" using t_sub
    by (auto simp: tfrees_set_def)
  also have "... \<subseteq> (dom \<Delta>)" using
    `tfrees_set (ran \<Gamma>) \<subseteq> dom \<Delta>``\<Delta> \<rho> = Some n`
    by (simp add: domI)
  finally show  "tfrees_set (ran (\<Gamma>(x \<mapsto> \<tau>))) \<subseteq> dom \<Delta>" .
qed 


subsection {* Type judgements and heap (type) updates *}

lemma WfWValue_region_extend:
  assumes wfwv: "WfWValue \<Delta> (\<Theta> @ [\<Sigma>]) v \<tau>'"
  and    notin: "x \<notin> dom \<Sigma>"
  shows  "WfWValue \<Delta> (\<Theta> @ [\<Sigma>(x \<mapsto> \<tau>)]) v \<tau>'"
  using wfwv notin
  by cases (auto simp: lookup_heap_Some_iff nth_append not_less intro!: WfWValue.intros split: split_if_asm)

lemma WfWValue_heap_monotone:
  assumes wfwv: "WfWValue \<Delta> \<Theta> v \<tau>'"
  shows  "WfWValue \<Delta> (\<Theta> @ \<Theta>') v \<tau>'"
  using wfwv 
  by cases (auto intro!: WfWValue.intros simp: lookup_heap_Some_iff nth_append)

lemma WfWValue_heap_mono:
  assumes wfst:  "WfWValue \<Delta> \<Theta> v \<tau>"
  and      sub:  "subheap \<Theta> \<Theta>'"
  shows "WfWValue \<Delta> \<Theta>' v \<tau>"
  using wfst sub
proof induction
  case (wfRefV \<Delta> \<rho> region \<Theta> off \<tau>)
  show ?case
  proof
    from `subheap \<Theta> \<Theta>'` `lookup_heap \<Theta> region off = Some \<tau>`
    show "lookup_heap \<Theta>' region off = Some \<tau>" by (rule subheap_lookup_heapD)
  qed fact
qed (auto intro: WfWValue.intros)

lemma WfStore_heap_mono:
  assumes wfst:  "WfStore \<Delta> \<Theta> G \<Gamma>"
  and      sub:  "subheap \<Theta> \<Theta>'"
  shows "WfStore \<Delta> \<Theta>' G \<Gamma>"
proof (rule, rule submap_st_weaken)
  from wfst show "submap_st \<Gamma> G (WfWValue \<Delta> \<Theta>)" by (auto elim: WfStore.cases)
next
  fix mv nv 
  assume wfwv: "WfWValue \<Delta> \<Theta> mv nv"
  thus "WfWValue \<Delta> \<Theta>' mv nv" using sub
    by (rule WfWValue_heap_mono)
qed

lemma WfStack_mono:
  assumes wfst: "WfStack \<Psi> \<Delta> \<Theta> st \<tau> b \<rho>"
  and   sub: "subheap \<Theta> \<Theta>'"
  shows "WfStack \<Psi> \<Delta> \<Theta>' st \<tau> b \<rho>"
  using wfst sub
proof (induction arbitrary: \<Theta>')
  case wfStackNil thus ?case by (clarsimp simp: subheap_singleton intro!: WfStack.intros)
next
  case (wfStackFun \<Psi> \<Delta>' \<Theta> st \<tau>' b' \<gamma> store' \<Gamma> x \<tau> cont \<Delta> \<Sigma> \<rho> \<Theta>')
                  
  from `subheap (\<Theta> @ [\<Sigma>]) \<Theta>'` obtain \<Sigma>'
    where "\<Theta>' = butlast \<Theta>' @ [\<Sigma>']" and "subheap \<Theta> (butlast \<Theta>')"
    unfolding subheap_def 
    by (clarsimp simp add: list_all2_append1 butlast_append list_all2_Cons1
                     cong: rev_conj_cong)

  moreover have "WfStack \<Psi> \<Delta> (butlast \<Theta>' @ [\<Sigma>']) ((store', cont, ReturnFrame x) # st) \<tau> True \<rho>"
  proof 
    from `subheap \<Theta> (butlast \<Theta>')` show "WfStack \<Psi> \<Delta>' (butlast \<Theta>') st \<tau>' b' \<gamma>" 
      by (rule wfStackFun.IH)

    from `WfStore \<Delta>' \<Theta> store' \<Gamma>` `subheap \<Theta> (butlast \<Theta>')`
    show "WfStore \<Delta>' (butlast \<Theta>') store' \<Gamma>" by (rule WfStore_heap_mono)

    from `\<Theta>' = butlast \<Theta>' @ [\<Sigma>']` `WfFrees \<Delta>' (\<Gamma>(x \<mapsto> \<tau>)) \<gamma> (length \<Theta> - 1)` `subheap \<Theta> (butlast \<Theta>')`
    show "WfFrees \<Delta>' (\<Gamma>(x \<mapsto> \<tau>)) \<gamma> (length (butlast \<Theta>') - 1)"
      by (clarsimp dest!: subheap_lengthD)
  qed fact+
  ultimately show ?case by simp
next
  case (wfStackSeq \<Psi> \<Delta> \<Theta> st \<tau> b' \<rho> store' \<Gamma> cont b \<Theta>')

  show ?case
  proof
    from `subheap \<Theta> \<Theta>'`show "WfStack \<Psi> \<Delta> \<Theta>' st \<tau> b' \<rho>" by (rule wfStackSeq.IH)
    from `WfStore \<Delta> \<Theta> store' \<Gamma>` `subheap \<Theta> \<Theta>'`
    show "WfStore \<Delta> \<Theta>' store' \<Gamma>" by (rule WfStore_heap_mono)
  qed fact+
qed      

lemma WfWValue_push_heap:
  assumes wfst:  "WfWValue \<Delta> \<Theta> v \<tau>"
  shows "WfWValue \<Delta> (push_heap \<Theta>) v \<tau>"
  using wfst
  by induction (auto intro!: WfWValue.intros simp: lookup_heap_Some_iff push_heap_def nth_append)

lemma WfStore_push_heap:
  assumes wfst:  "WfStore \<Delta> \<Theta> G \<Gamma>"
  shows "WfStore \<Delta> (push_heap \<Theta>) G \<Gamma>"
proof (rule, rule submap_st_weaken)
  from wfst show "submap_st \<Gamma> G (WfWValue \<Delta> \<Theta>)" by (auto elim: WfStore.cases)
next
  fix mv nv 
  assume wfwv: "WfWValue \<Delta> \<Theta> mv nv"
  thus "WfWValue \<Delta> (push_heap \<Theta>) mv nv" 
    by (rule WfWValue_push_heap)
qed

lemma WfStore_upd_heapT:
  assumes wfst:  "WfStore \<Delta> \<Theta> G \<Gamma>"
  and     new_T: "update_heap \<Theta> n x \<tau> = Some \<Theta>'"
  and  x_not_in: "x \<notin> dom (lookup_heap \<Theta> n)"
  shows "WfStore \<Delta> \<Theta>' G \<Gamma>"
proof (rule, rule submap_st_weaken)
  from wfst show "submap_st \<Gamma> G (WfWValue \<Delta> \<Theta>)" 
    by (auto elim: WfStore.cases)
next
  fix mv nv 
  assume wfwv: "WfWValue \<Delta> \<Theta> mv nv"

  from wfwv 
  show "WfWValue \<Delta> \<Theta>' mv nv" 
  proof cases
    case (wfRefV \<rho> region off \<tau>)

    hence "lookup_heap \<Theta>' region off = Some \<tau>" using x_not_in new_T 
      by (cases "n = region")
         (fastforce simp: lookup_heap_Some_iff nth_append update_heap_def not_less min_absorb2
                   split: split_if_asm )+
    thus ?thesis using wfRefV
      by (auto simp add: lookup_heap_Some_iff intro!: WfWValue.intros)
  qed (auto intro: WfWValue.intros)
qed

(* x is usually fresh_in_heap *)
lemma WfHeap_upd:
  assumes wfh: "WfHeap H \<Theta>"
  and     wfwv: "WfHValue v \<tau>"
  and       nv: "n = length H - 1"
  and    new_H: "update_heap H n x v = Some H'"
  and    new_T: "update_heap \<Theta> n x \<tau> = Some \<Theta>'"
  and    notin: "x \<notin> dom (lookup_heap H n)"
  shows   "WfHeap H' \<Theta>'"
  using wfh wfwv notin WfHeap_dom' [where n = n, OF wfh] nv new_H new_T
proof induction
  case wfHeapNil thus ?case by simp
next
  case (wfHeapCons H \<Theta> \<Sigma> R)
  
  note heap_len = WfHeap_length [OF wfHeapCons.hyps(1)]

  have "x \<notin> dom R" using wfHeapCons.prems by (simp add: lookup_heap_def)
  hence "x \<notin> dom \<Sigma>" using wfHeapCons.prems heap_len by auto
  with `WfHeap H \<Theta>` `submap_st \<Sigma> R WfHValue` `finite (dom R)`
  show ?case using wfHeapCons.prems heap_len 
    unfolding update_heap_def
    by (auto simp add: update_heap_def nth_append 
      intro!: WfHeap.intros submap_st_update 
      elim!: submap_st_weaken WfWValue_region_extend)
qed

lemma WfHeap_upd_same_type:
  assumes wfh: "WfHeap H \<Theta>"
  and     wfwv: "WfHValue v \<tau>"
  and    new_H: "update_heap H n x v = Some H'"
  and      lup: "lookup_heap \<Theta> n x = Some \<tau>"
  shows   "WfHeap H' \<Theta>"
  using wfh wfwv new_H lup
proof (induction arbitrary: H')
  case wfHeapNil thus ?case by simp
next
  case (wfHeapCons H \<Theta> \<Sigma> R H')
  
  note heap_len = WfHeap_length [OF wfHeapCons.hyps(1)]

  show ?case
  proof (cases "n = length \<Theta>")
    case True thus ?thesis using wfHeapCons.prems wfHeapCons.hyps heap_len 
      by (auto simp add: update_heap_def nth_append submap_st_def
                 intro!:  WfHeap.intros)
  next
    case False
    with `lookup_heap (\<Theta> @ [\<Sigma>]) n x = Some \<tau>` have "n < length \<Theta>" 
      by (simp add: lookup_heap_Some_iff)
    hence "n < length H" using heap_len by simp

    from `update_heap (H @ [R]) n x v = Some H'`
    have "H' = (butlast H') @ [R]"
      unfolding update_heap_def using False heap_len
      by (auto simp: nth_append butlast_snoc not_less butlast_append
              split: split_if_asm)
    moreover have "WfHeap (butlast H' @ [R]) (\<Theta> @ [\<Sigma>])"
    proof
      have "WfHValue v \<tau>" by fact 
      moreover 
      from `update_heap (H @ [R]) n x v = Some H'` `H' = (butlast H') @ [R]`
      have "update_heap (H @ [R]) n x v = Some (butlast H' @ [R])" by simp
      hence "update_heap H n x v = Some (butlast H')" using `n < length H`
        by (rule update_heap_shrink)
      moreover 
      from `lookup_heap (\<Theta> @ [\<Sigma>]) n x = Some \<tau>` `n < length \<Theta>`
      have "lookup_heap \<Theta> n x = Some \<tau>" 
        by (simp add: lookup_heap_Some_iff nth_append)
      
      ultimately show "WfHeap (butlast H') \<Theta>"
        by (rule wfHeapCons.IH)
    qed fact+
    ultimately show ?thesis by simp
  qed      
qed

subsection {* Region environment updates *}

lemma WfWValue_renv_mono:
  assumes wfwv: "WfWValue \<Delta> \<Theta> v \<tau>"
  and     sub: "\<Delta> \<subseteq>\<^sub>m \<Delta>'"
  shows  "WfWValue \<Delta>' \<Theta> v \<tau>"
  using wfwv sub 
  by induct (auto intro!: WfWValue.intros 
                   dest!: map_leD)


lemma WfStack_renv_mono:
  notes fun_upd_apply [simp del]
  assumes wfst: "WfStack \<Psi> \<Delta> \<Theta> st \<tau> b \<rho>"
  and     sub: "\<Delta> \<subseteq>\<^sub>m \<Delta>'"
  shows   "WfStack \<Psi> \<Delta>' \<Theta> st \<tau> b \<rho>"
  using wfst sub
proof induction
  case wfStackNil show ?case ..
next
  case wfStackFun
  thus ?case by (auto intro!: WfStack.intros elim: map_le_trans)
next
  case (wfStackSeq \<Psi> \<Delta> \<Theta> st \<tau> b' \<rho> store' \<Gamma> cont b)

  show ?case
  proof 
    from `WfStore \<Delta> \<Theta> store' \<Gamma>` `\<Delta> \<subseteq>\<^sub>m \<Delta>'`
    show "WfStore \<Delta>' \<Theta> store' \<Gamma>" 
      by (auto intro!: map_le_map_upd_right elim!: WfStore_lift_weak WfWValue_renv_mono )
    show "WfStack \<Psi> \<Delta>' \<Theta> st \<tau> b' \<rho>" by (rule wfStackSeq.IH) fact

    from `tfrees_set (ran \<Gamma>) \<subseteq> dom \<Delta>`
    show "tfrees_set (ran \<Gamma>) \<subseteq> dom \<Delta>'"
    proof (rule order_trans) 
      from `\<Delta> \<subseteq>\<^sub>m \<Delta>'` show "dom \<Delta> \<subseteq> dom \<Delta>'" by (rule map_le_implies_dom_le)
    qed
  qed fact+      
qed

  



end
