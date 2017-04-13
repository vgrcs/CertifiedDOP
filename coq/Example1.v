Add LoadPath "usr/local/lib/pcc" .
Require Import CoqDeltaRPM.
Require Import List.
Require Import Ascii.
Require Import String.
Lemma delta :
Apply (Delta ((Add_operation (Sym "?" "__libc_start_main@@GLIBC_2.0") (Delta nil))::(Add_operation (Sym "?" "__stack_chk_fail@@GLIBC_2.4") (Delta nil))::(Add_operation (Sym "?" "system@@GLIBC_2.0") (Delta nil))::nil), List ((Sym "?" "__libc_start_main@@GLIBC_2.0"::nil)++(Sym "?" "__stack_chk_fail@@GLIBC_2.4"::nil)++(Sym "?" "system@@GLIBC_2.0"::nil) ++ nil)) -> 
TypedExpression (Delta ((Add_operation (Sym "?" "__libc_start_main@@GLIBC_2.0") (Delta nil))::(Add_operation (Sym "?" "__stack_chk_fail@@GLIBC_2.4") (Delta nil))::(Add_operation (Sym "?" "system@@GLIBC_2.0") (Delta nil))::nil),Type_Delta (("?"::nil)++("?"::nil)++("?"::nil) ++ nil)) -> 
SafeTable (List ((Sym "?" "__libc_start_main@@GLIBC_2.0"::nil)++(Sym "?" "__stack_chk_fail@@GLIBC_2.4"::nil)++(Sym "?" "system@@GLIBC_2.0"::nil) ++ nil),Type_Delta ("?"::"?"::"?"::nil)).
Proof.
  intros H1 H2.
  assert (Resolved ("?"::"?"::"?"::nil))
    by (solve_consistency).
  eauto using TypeLemma.
Qed.