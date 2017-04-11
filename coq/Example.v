Require Import CoqDeltaRPM.
Require Import List.
Require Import Ascii.
Require Import String.

Definition es := Apply (Delta (Add_operation (Sym "T" "__libc_start_main@@GLIBC_2.0") (Delta nil)::Add_operation (Sym "T" "__stack_chk_fail@@GLIBC_2.4") (Delta nil)::nil), List ((Sym "T" "__libc_start_main@@GLIBC_2.0")::(Sym "T" "__stack_chk_fail@@GLIBC_2.4")::nil)).

Definition tx := TypedExpression (Delta (Add_operation (Sym "T" "__libc_start_main@@GLIBC_2.0") (Delta nil)::Add_operation (Sym "T" "__stack_chk_fail@@GLIBC_2.4") (Delta nil)::nil),Type_Delta ("T"::"T"::nil)).

Definition ty := SafeTable (List ((Sym "T" "__libc_start_main@@GLIBC_2.0")::(Sym "T" "__stack_chk_fail@@GLIBC_2.4")::nil),Type_Delta ("T"::"T"::nil)).

Ltac solve_apply :=
  repeat match goal with
          | [ |- Apply (_,_)] => try (replace ((Sym "t" "adsds")::nil) 
                                      with (((Sym "t" "adsds")::nil) ++ nil) 
                                       by (rewrite app_nil_r; reflexivity));
                                repeat (apply APPLY_delta || apply ADD_object || apply EMPTY)
         end.

Ltac solve_tc :=
  repeat match goal with
          | [ |- TypedExpression (_,_)] => try (replace (("t")::nil) 
                                                with ((("t")::nil) ++ nil) 
                                                 by (rewrite app_nil_r; reflexivity));
                                          repeat (apply TE_Delta || apply TE_Symbols_Add || 
                                                  apply TE_Empty || apply TE_Symbol)
         end.

Lemma delta: 
  SafeTable (List ((Sym "t" "adsds")::nil), Type_Delta ("t"::nil)). 
Proof. 
  assert (Apply  (Delta ((Add_operation (Sym "t" "adsds") (Delta nil))::nil), List ((Sym "t" "adsds")::nil)))
    by solve_apply.

  assert (TypedExpression (Delta ((Add_operation (Sym "t" "adsds") (Delta nil))::nil), Type_Delta (("t")::nil)))
    by solve_tc.

  assert (Resolved ("t"::nil))
    by solve_consistency.

  eapply TypeLemma; eauto.
Qed.  
