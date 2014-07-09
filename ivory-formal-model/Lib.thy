(* Lemmas which are not Ivory specific, and could/should be in the standard library *)

theory Lib 
imports Main
begin

class infinite =
  fixes fresh :: "'a set \<Rightarrow> 'a"
  assumes fresh_not_in: "finite S \<Longrightarrow> fresh S \<notin> S"

instantiation nat :: "{infinite}"
begin

definition
  nat_fresh_def: "fresh S = (if S = {} then 0 else Suc (Max S))"

instance
proof
  fix S :: "nat set"
  assume "finite S"
  show "fresh S \<notin> S"
  proof
    assume "fresh S \<in> S"
    with `finite S` have "Suc (Max S) \<le> Max S"
      unfolding nat_fresh_def
      by (auto dest!: Max_ge split: split_if_asm)

    thus False by simp
  qed 
qed
end


definition 
  submap_st :: "('var \<Rightarrow> 'c option) \<Rightarrow> ('var \<Rightarrow> 'fun option) \<Rightarrow> ('fun \<Rightarrow> 'c \<Rightarrow> bool) \<Rightarrow> bool"
where
  "submap_st m1 m2 P \<equiv> \<forall>x \<in> dom m1. x \<in> dom m2 \<and> P (the (m2 x)) (the (m1 x))"

fun
  inits :: "'a list \<Rightarrow> 'a list list"
where
  "inits []       = [[]]"
| "inits (x # xs) = [] # map (op # x) (inits xs)"

lemma submap_st_empty [simp]:
  "submap_st Map.empty m P"
  unfolding submap_st_def by simp

lemma submap_stE [elim?]:
  assumes major: "submap_st m n P"
  and mx: "m x = Some v"
  and rl: "\<And>v'. \<lbrakk> n x = Some v'; P v' v\<rbrakk> \<Longrightarrow> R"
  shows R
  using major mx rl
  unfolding submap_st_def by fastforce

lemma submap_st_update:
  assumes st: "submap_st m m' P"
  and    pvs: "P v' v"
  shows "submap_st (m(x \<mapsto> v)) (m'(x \<mapsto> v')) P"
  using st pvs unfolding submap_st_def by auto

lemma submap_st_weaken:
  assumes st: "submap_st m m' P"
  and     rl: "\<And>x v v'. \<lbrakk> m x = Some v; m' x = Some v'; P v' v \<rbrakk> \<Longrightarrow> P' v' v"
  shows "submap_st m m' P'"
  using st rl
  unfolding submap_st_def 
  by (auto dest: bspec)

lemma submap_st_dom:
  assumes st: "submap_st m m' P"
  shows "dom m \<subseteq> dom m'"
  using st unfolding submap_st_def by auto

lemma submap_stI:
  fixes as and k
  assumes dom: "dom M' \<subseteq> dom M"
  and     rl: "\<And>x v v'. \<lbrakk> M' x = Some v'; M x = Some v \<rbrakk> \<Longrightarrow> P v v'"
  shows   "submap_st M' M P"
  using dom unfolding submap_st_def 
  by (auto dest!: set_mp elim!: rl)

lemma map_leD:
  assumes mle: "M \<subseteq>\<^sub>m M'"
  and     mx: "M x = Some y"
  shows   "M' x = Some y"
  using mle mx unfolding map_le_def by (auto simp: dom_def)

lemma map_le_map_upd_right:
  assumes map_le: "M \<subseteq>\<^sub>m M'"
  and     notin: "x \<notin> dom M'"
  shows   "M \<subseteq>\<^sub>m M'(x \<mapsto> y)"
  using map_le notin unfolding map_le_def
  by (auto simp: dom_def)

lemma ran_map_upd_subset:
  "ran (M(x \<mapsto> v)) \<subseteq> ran M \<union> {v}"
  unfolding ran_def by auto

lemma ran_map_upds:
  "ran [as [\<mapsto>] bs] \<subseteq> set bs"
  unfolding ran_def map_upds_def
  by (clarsimp dest!: map_of_SomeD simp: set_zip)

lemma option_map_map_upds:
  "Option.map f \<circ> [as [\<mapsto>] bs] = [as [\<mapsto>] map f bs]"
  unfolding map_upds_def 
  by (simp add: map_of_map [symmetric] rev_map [symmetric] zip_map2)

lemma map_of_apply:
  "\<lbrakk>k \<in> set (map fst xs); P (Some (hd (map snd (filter (\<lambda>x. fst x = k) xs)))) \<rbrakk>
  \<Longrightarrow> P (map_of xs k)"
  by (induct xs) (auto split: split_if_asm)

lemma hd_filter_conv_nth:
  assumes non_empty: "filter P xs \<noteq> []"
  shows "R (hd (filter P xs)) = (\<exists>i < length xs. R (xs ! i) \<and> P (xs ! i) \<and> (\<forall>j < i. \<not> P (xs ! j)))" (is "?LHS xs = ?RHS xs")
  using non_empty
proof (induction xs arbitrary:)
  case Nil thus ?case by simp
next
  case (Cons y ys)
  show ?case
  proof (cases "P y")
    case True
    thus ?thesis by fastforce
  next
    case False
    hence "?LHS (y # ys) = ?LHS ys" by simp
    also have "... = ?RHS ys" 
      using False Cons.prems by (auto intro!: Cons.IH)
    also have "... = ?RHS (y # ys)"
    proof 
      assume "?RHS ys"
      then obtain i where "i < length ys \<and> R (ys ! i) \<and> P (ys ! i) \<and> (\<forall>j<i. \<not> P (ys ! j))" ..
      thus "?RHS (y # ys)" using False
        by - (rule exI [where x = "Suc i"], clarsimp simp: less_Suc_eq_0_disj)
    next
      assume "?RHS (y # ys)"
      with False obtain i where "i < length ys \<and> R (ys ! i) \<and> P (ys ! i) \<and> (\<forall>j<i. \<not> P (ys ! j))" 
        by (fastforce simp: nth.simps gr0_conv_Suc split: nat.splits)
      thus "?RHS ys" ..
    qed
    finally show ?thesis .
  qed
qed

lemma map_of_apply_nth:
  assumes key_in_list: "k \<in> set (map fst xs)"
  and     rl: "\<And>i. \<lbrakk>i < length xs; fst (xs ! i) = k; (\<forall>j < i. fst (xs ! j) \<noteq> k) \<rbrakk> \<Longrightarrow> P (Some (snd (xs ! i)))"
  shows   "P (map_of xs k)"
  using key_in_list
proof (rule map_of_apply)
  let ?i = "LEAST i. fst (xs ! i) = k"
  
  from key_in_list obtain i where
    ivs: "i < length xs" "fst (xs ! i) = k"
    by (clarsimp simp: in_set_conv_nth split_def)

  hence "[x\<leftarrow>xs . fst x = k] \<noteq> []"
    by (fastforce simp add: filter_empty_conv)

  moreover have "?i < length xs" using ivs
    by (auto elim!: order_le_less_trans [rotated] intro!: Least_le)
  moreover from `fst (xs ! i) = k` have "fst (xs ! ?i) = k" by (rule LeastI)
  moreover have "\<forall>j < ?i. fst (xs ! j) \<noteq> k" by (clarsimp dest!: not_less_Least)  
  
  from `[x\<leftarrow>xs . fst x = k] \<noteq> []` have "hd [x\<leftarrow>xs . fst x = k] = (xs ! ?i)"
    by (rule iffD2 [OF hd_filter_conv_nth])
       (rule exI [where x = "?i"], intro conjI, simp_all, fact+)
  
  moreover have "P (Some (snd (xs ! ?i)))" by (rule rl, fact+)
  ultimately show "P (Some (hd (map snd [x\<leftarrow>xs . fst x = k])))" 
    by (simp add: hd_map)
qed

lemma map_of_apply_nth_LEAST [consumes 1, case_names LEAST]:
  fixes xs and k
  defines "l_i \<equiv> LEAST i. fst (xs ! i) = k"
  assumes key_in_list: "k \<in> set (map fst xs)"
  and     rl: "\<lbrakk>l_i < length xs; fst (xs ! l_i) = k \<rbrakk> \<Longrightarrow> P (Some (snd (xs ! l_i)))"
  shows   "P (map_of xs k)"
  using key_in_list
proof (rule map_of_apply)
  from key_in_list obtain i where
    ivs: "i < length xs" "fst (xs ! i) = k"
    by (clarsimp simp: in_set_conv_nth split_def)

  hence "[x\<leftarrow>xs . fst x = k] \<noteq> []"
    by (fastforce simp add: filter_empty_conv)

  moreover have "l_i < length xs" using ivs unfolding l_i_def
    by (auto simp add: l_i_def intro: order_le_less_trans [rotated] intro!: Least_le)
  moreover have "fst (xs ! i) = k" by fact
  hence "fst (xs ! l_i) = k" 
    unfolding l_i_def by (rule LeastI)
  moreover from `l_i < length xs` 
  have "\<forall>j < l_i. fst (xs ! j) \<noteq> k" 
    unfolding l_i_def 
    by (clarsimp dest!: not_less_Least)    
  from `[x\<leftarrow>xs . fst x = k] \<noteq> []` have "hd [x\<leftarrow>xs . fst x = k] = (xs ! l_i)"
    by (rule iffD2 [OF hd_filter_conv_nth])
       (rule exI [where x = "l_i"], intro conjI, simp_all, fact+)

  moreover have "P (Some (snd (xs ! l_i)))" by (rule rl) fact+
  
  ultimately show "P (Some (hd (map snd [x\<leftarrow>xs . fst x = k])))" 
    by (simp add: hd_map)
qed    

lemma map_of_apply_nth_LEAST' [consumes 1, case_names LEAST]:
  fixes as and k
  defines "l_i \<equiv> LEAST i. as ! i = k"
  assumes key_in_list: "k \<in> set as"
  and    lens: "length as = length bs"
  and     rl: "\<lbrakk>l_i < length as; as ! l_i = k \<rbrakk> \<Longrightarrow> P (Some (bs ! l_i))"
  shows   "P (map_of (zip as bs) k)"
proof (rule map_of_apply_nth)
  from key_in_list lens show "k \<in> set (map fst (zip as bs))" by clarsimp 
next
  fix i
  assume ilt: "i < length (zip as bs)" and "fst (zip as bs ! i) = k"
    "\<forall>j<i. fst (zip as bs ! j) \<noteq> k"
  hence as_v: "as ! i = k" "\<forall>j<i. as ! j \<noteq> k"
    by auto
  hence l_i_v: "l_i = i" unfolding l_i_def
    by (auto intro!: Least_equality simp: linorder_not_less [symmetric])
  hence "P (Some (bs ! l_i))" using ilt lens as_v
    by (intro rl, simp_all)
  thus "P (Some (snd (zip as bs ! i)))" using lens ilt l_i_v by simp
qed    

lemma option_bind_Some_iff:
  "(Option.bind m f = Some v) = (\<exists>v'. m = Some v' \<and> f v' = Some v)"
  by (cases m) simp_all

lemma list_all2_weaken [consumes 1, case_names P]:
  assumes lall: "list_all2 P xs ys"
  and     rl: "\<And>i. \<lbrakk> i < length xs; length ys = length xs; P (xs ! i) (ys ! i) \<rbrakk> \<Longrightarrow> Q (xs ! i) (ys ! i)"
  shows "list_all2 Q xs ys"
  using lall unfolding list_all2_conv_all_nth
  by (auto intro: rl)


lemma submap_st_list_all2I:
  fixes as and k
  assumes lens: "length as = length bs" "length as = length cs"
  and     lall: "list_all2 P cs bs"
  shows   "submap_st [as [\<mapsto>] bs] [as [\<mapsto>] cs] P"
  unfolding submap_st_def
proof (intro ballI conjI)
  fix x
  assume xin: "x \<in> dom [as [\<mapsto>] bs]"
  thus "x \<in> dom [as [\<mapsto>] cs]" using lens by simp
  
  let ?i = "LEAST i. rev as ! i = x"
  from lall have "list_all2 P (rev cs) (rev bs)" by simp
  hence "?i < length (rev cs) \<Longrightarrow> P (rev cs ! ?i) (rev bs ! ?i)" 
    by (rule list_all2_nthD)
  thus "P (the ([as [\<mapsto>] cs] x)) (the ([as [\<mapsto>] bs] x))" using lens xin
    by (clarsimp simp: map_upds_def dom_map_of_zip zip_rev [symmetric]) (fastforce intro: map_of_apply_nth_LEAST')
qed

lemma list_all2_ballE1:
  assumes asms: "list_all2 P as bs" "a \<in> set as"
  and rl: "\<And>b. \<lbrakk> b \<in> set bs; P a b \<rbrakk> \<Longrightarrow> R"
  shows R 
  using asms rl
  by (fastforce simp: list_all2_conv_all_nth in_set_conv_nth)

end