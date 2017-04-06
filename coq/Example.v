Require Import Semantics6.
Require Import List.
Require Import Ascii.
Require Import String.

Definition es := Apply (Delta (Add_operation (Sym "T" "__libc_start_main@@GLIBC_2.0") (Delta nil)::Add_operation (Sym "T" "__stack_chk_fail@@GLIBC_2.4") (Delta nil)::nil), List ((Sym "T" "__libc_start_main@@GLIBC_2.0")::(Sym "T" "__stack_chk_fail@@GLIBC_2.4")::nil)).

Definition tx := TypedExpression (Delta (Add_operation (Sym "T" "__libc_start_main@@GLIBC_2.0") (Delta nil)::Add_operation (Sym "T" "__stack_chk_fail@@GLIBC_2.4") (Delta nil)::nil),Type_Delta ("T"::"T"::nil)).

Definition ty := SafeTable (List ((Sym "T" "__libc_start_main@@GLIBC_2.0")::(Sym "T" "__stack_chk_fail@@GLIBC_2.4")::nil),Type_Delta ("T"::"T"::nil)).

Lemma delta : 
  Apply  (Delta ((Add_operation (Sym "t" "adsds") (Delta nil))::nil), List ((Sym "t" "adsds")::nil)) -> 
  TypedExpression (Delta ((Add_operation (Sym "t" "adsds") (Delta nil))::nil), Type_Delta (("t")::nil)) -> 
  SafeTable (List ((Sym "t" "adsds")::nil), Type_Delta ("t"::nil)). 
Proof. 
  intros H1 H2.  
  assert (Resolved ("t"::nil))
    by (solve_consistency).
  eapply TypeLemma; eauto.
Qed.  

