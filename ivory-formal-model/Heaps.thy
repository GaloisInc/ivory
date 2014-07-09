(* Definitions and lemmas for manipulating Ivory heaps and heap types *)

theory Heaps
imports Lib
begin

(*
  Heaps in Ivory are represented by a stack of regions, where a region
  is just a map (key \<Rightarrow> value option).  We abstract out this by
  introducing lookup_heap and update_heap: lookup_heap returns Some x
  iff the given region and key exist, otherwise it gives None.
  Similarly, update_heap returns None if given an invalid region

  Note that heaps are extended from the end as this allows indexes
  into the heap to persist across extesions.
*)

(* Region indexes *)
type_synonym ridx = nat

(* Region offsets *)
type_synonym roff = nat

definition
  lookup_heap :: "(('a \<Rightarrow> 'b option) list) \<Rightarrow> ridx \<Rightarrow> 'a \<Rightarrow> 'b option"
where
  "lookup_heap H region = (if region < length H then (H ! region) else Map.empty)"

definition
  update_heap :: "(('a \<Rightarrow> 'b option) list) \<Rightarrow> ridx \<Rightarrow> 'a \<Rightarrow> 'b \<Rightarrow> ('a \<Rightarrow> 'b option) list option"
where
  "update_heap H region = (if region < length H then (\<lambda>off v. Some (take region H @ [ (H ! region)(off \<mapsto> v) ] @ drop (region + 1) H)) else (\<lambda>_ _. None))"

definition
  push_heap :: "(('a \<Rightarrow> 'b option) list) \<Rightarrow> (('a \<Rightarrow> 'b option) list)"
where
  "push_heap H = H @ [Map.empty]"

definition
  pop_heap :: "(('a \<Rightarrow> 'b option) list) \<Rightarrow> (('a \<Rightarrow> 'b option) list)"
where
  "pop_heap H = butlast H"

definition
  fresh_in_heap :: "(('a :: infinite \<Rightarrow> 'b option) list) \<Rightarrow> ridx \<Rightarrow> 'a"
where
  "fresh_in_heap H n = (fresh (dom (H ! n)))"

definition
  subheap :: "('a \<Rightarrow> 'b option) list \<Rightarrow> ('a \<Rightarrow> 'b option) list \<Rightarrow> bool"
where
  "subheap = list_all2 map_le"

lemma pop_push_heap: "pop_heap (push_heap H) = H"
  by (simp add: pop_heap_def push_heap_def)

lemma lookup_heap_Some_iff:
  "(lookup_heap H region off = Some v) = (region < length H \<and> (H ! region) off = Some v)"
  unfolding lookup_heap_def by simp

lemma lookup_heap_empty [simp]:
  "lookup_heap [] n = (\<lambda>_. None)" unfolding lookup_heap_def by simp

lemma lookup_heap_length_append [simp]:
  "lookup_heap (H @ [R]) (length H) = R"
  unfolding lookup_heap_def by simp

lemma update_heap_empty [simp]:
  "update_heap [] n = (\<lambda>_ _. None)" unfolding update_heap_def by simp

lemma update_heap_idem:
  shows "(update_heap H region off v = Some H) = (lookup_heap H region off = Some v)"
  apply (simp add: lookup_heap_Some_iff)
  apply (clarsimp simp add: update_heap_def  list_eq_iff_nth_eq)
  apply rule
   apply (clarsimp simp add: min_absorb1 min_absorb2 Suc_pred )
   apply (drule spec, drule (1) mp)
   apply (clarsimp simp: nth_append fun_upd_idem_iff)
  apply (clarsimp simp add: min_absorb1 min_absorb2 )
  apply rule
   apply (rule Suc_pred)
   apply arith
  apply (auto simp: le_imp_diff_is_add nth_append nth.simps min_absorb1 min_absorb2 not_less add_ac split: nat.splits)
  done

lemma update_heap_into_lookup_heap:
  assumes upd: "update_heap H region off v = Some H'"
  shows "lookup_heap H' region' off' = (if region' = region \<and> off' = off then Some v else lookup_heap H region' off')"
  using upd unfolding update_heap_def
  by (auto simp: lookup_heap_def min_absorb2 min_absorb1 not_less nth_append nth.simps
     Suc_pred le_imp_diff_is_add add_ac
     split: split_if_asm nat.splits)

lemma lookup_heap_into_update_heap_same:
  assumes lup: "lookup_heap H region off = Some v"
  obtains H' where "update_heap H region off v' = Some H'"
  using lup unfolding update_heap_def
  by (auto simp: lookup_heap_def min_absorb2 min_absorb1 not_less nth_append nth.simps
     Suc_pred le_imp_diff_is_add add_ac
     split: split_if_asm nat.splits) (* clag *)

lemma update_heap_Some_iff:
  "(update_heap H region off v = Some H')
   = (region < length H \<and> H' = take region H @ [ (H ! region)(off \<mapsto> v) ] @ drop (region + 1) H)"
  unfolding update_heap_def by auto

lemma update_heap_mono:
  assumes upd: "update_heap H region off v = Some H'"
  shows  "update_heap (H @ G) region off v = Some (H' @ G)"
  using upd unfolding update_heap_def
  by (auto simp: nth_append split: split_if_asm)

lemma update_heap_shrink:
  assumes upd: "update_heap (H @ G) region off v = Some (H' @ G)"
  and     region: "region < length H"
  shows   "update_heap H region off v = Some H'"
  using upd region unfolding update_heap_def
  by (clarsimp simp: not_less nth_append split: split_if_asm)

lemma update_heap_length:
  "update_heap H n x v = Some H' \<Longrightarrow> length H' = length H"
  by (auto simp: update_heap_def split: split_if_asm)

lemma length_pop_heap [simp]:
  "H \<noteq> [] \<Longrightarrow> length (pop_heap H) = length H - 1"
  unfolding pop_heap_def by simp

lemma length_push_heap [simp]:
  "length (push_heap H) = Suc (length H)" 
  unfolding push_heap_def by simp

lemma length_pop_heap_le:
  "length (pop_heap H) \<le> length H"
  unfolding pop_heap_def by simp

lemma fresh_in_heap_fresh:
  assumes finite: "finite (dom (lookup_heap H n))"
  shows "fresh_in_heap H n \<notin> dom (lookup_heap H n)"
proof
  let ?R = "lookup_heap H n"
  assume "fresh_in_heap H n \<in> dom ?R"
  hence "fresh (dom ?R) \<in> dom ?R"
    by (auto split: split_if_asm simp: fresh_in_heap_def lookup_heap_def)
  moreover from finite have "fresh (dom ?R) \<notin> dom ?R" by (rule fresh_not_in)
  ultimately show False by simp
qed  

lemma subheap_singleton:
  "subheap [R] H = (\<exists>R'. H = [R'] \<and> R \<subseteq>\<^sub>m R')"
  unfolding subheap_def
  by (cases H) auto

lemma subheap_lengthD:
  "subheap H H' \<Longrightarrow> length H = length H'" unfolding subheap_def by (rule list_all2_lengthD)

lemma subheap_lookup_heapD:
  assumes sh: "subheap H H'"
  and    lup: "lookup_heap H region off = Some v"
  shows   "lookup_heap H' region off = Some v"
  using sh lup unfolding subheap_def
  by (auto simp: lookup_heap_Some_iff list_all2_conv_all_nth intro: map_leD)


lemma subheap_mono_left:
  assumes sh: "subheap H H'"
  shows   "subheap (G @ H) (G @ H')"
  using sh unfolding subheap_def
  by (auto simp: list_all2_conv_all_nth nth_append)

lemma subheap_mono_right:
  assumes sh: "subheap H H'"
  shows   "subheap (H @ G) (H' @ G)"
  using sh unfolding subheap_def
  by (auto simp: list_all2_conv_all_nth nth_append)

lemma subheap_refl:
  shows   "subheap H H"
  unfolding subheap_def
  by (auto simp: list_all2_conv_all_nth )

lemma subheap_trans [trans]:
  assumes sh: "subheap H H'"
  and    sh': "subheap H' H''"
  shows   "subheap H H''"
  using sh sh' unfolding subheap_def
  by (auto simp: list_all2_conv_all_nth nth_append elim!: map_le_trans dest!: spec)

lemma subheap_take_drop:
  assumes xd: "x \<notin> dom (lookup_heap H n)"
  and     mn: "m = Suc n"
  and     nv: "n < length H"
  shows "subheap H (take n H @ [ (H ! n)(x \<mapsto> y) ] @ drop m H)" (is "subheap H ?H'")
  using mn nv xd unfolding subheap_def
  by (auto intro!: nth_equalityI simp: lookup_heap_Some_iff list_all2_conv_all_nth le_imp_diff_is_add nth_append nth.simps min_absorb1 min_absorb2 not_less add_ac map_le_def split: nat.splits)


end
