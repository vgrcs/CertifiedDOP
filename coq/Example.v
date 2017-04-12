Require Import CoqDeltaRPM.
Require Import List.
Require Import Ascii.
Require Import String.

Definition es := Apply 
                   (Delta (Add_operation (Sym "T" "__libc_start_main@@GLIBC_2.0") (Delta nil)::
                           Add_operation (Sym "T" "__stack_chk_fail@@GLIBC_2.4") (Delta nil)::nil), 
                    List ( ((Sym "T" "__libc_start_main@@GLIBC_2.0")::nil) ++
                           ((Sym "T" "__stack_chk_fail@@GLIBC_2.4")::nil) ++ nil)).

Definition tx := TypedExpression 
                   (Delta (Add_operation (Sym "T" "__libc_start_main@@GLIBC_2.0") (Delta nil)::
                           Add_operation (Sym "T" "__stack_chk_fail@@GLIBC_2.4") (Delta nil)::nil),
                    Type_Delta (("T"::nil) ++ ("T"::nil) ++ nil)).

Definition ty := SafeTable 
                   (List ((Sym "T" "__libc_start_main@@GLIBC_2.0")::
                          (Sym "T" "__stack_chk_fail@@GLIBC_2.4")::nil),
                    Type_Delta ("T"::"T"::nil)).

Lemma example: ty. 
Proof. 
  assert es. unfold es; solve_apply.
  assert tx. unfold tx; solve_typed_expr. 
  assert (Resolved ("T"::"T"::nil)) by solve_consistency.
  eapply TypeLemma; eauto.
Qed.  
