Require Import Coq.Program.Equality.
Require Import Ascii.
Require Import String.
Require Import List.
Require Import Coq.Init.Tactics.
Require Import Coq.ZArith.Znat.
Require Import Coq.Arith.Peano_dec. 
Require Import Coq.Strings.String.

Open Scope string_scope.
Open Scope list_scope.

Open Scope char_scope.

Inductive Symbol := 
  | Sym : ascii -> string -> Symbol.

Inductive Table :=
  | List : list Symbol -> Table.

Definition sym_dec : forall a b : Symbol, {a = b} + {a <> b}.
Proof.
  intros a b. 
  induction a,b; try (solve [ right; intro; inversion H]).
  destruct (ascii_dec a  a0);  destruct (string_dec s  s0);  subst;
  ((right; contradict n; now inversion n) || (left; reflexivity)).
Qed.  

Definition value_dec : forall a b : Table, {a = b} + {a <> b}.
Proof.
  intros a b.
  induction a, b. 
  destruct (list_eq_dec sym_dec l l0); subst;
  [left; reflexivity | right; contradict n; inversion n; reflexivity].
Qed.  

Inductive Expr := 
  | Add_operation   : Symbol -> Expr -> Expr
  | Rem_operation   : Symbol -> Expr -> Expr
  | Delta           : list Expr -> Expr.


Inductive Apply : Expr * Table -> Type :=
  | ADD_object      
    : forall core sym l,
        Apply (core, List l) ->
        Apply (Add_operation sym core, List (sym::l))
  | REMOVE_object   
    : forall core sym l l',
        Apply (core, List (l++sym::l')) ->
        Apply (Rem_operation sym core, List (l++l'))
  | APPLY_delta 
    : forall expr actions v1 v2,
        Apply (expr, List v1) -> 
        Apply (Delta actions, List v2) -> 
        Apply (Delta (expr::actions), List (v1 ++ v2)).

Inductive DialectType :=
  | Type_Base    : ascii -> DialectType
  | Type_Delta   : list ascii -> DialectType.


Inductive TypedSymbol : (Symbol * DialectType) -> Type :=
  | TE_Symbol : 
      forall f n,
        TypedSymbol (Sym f n, Type_Base f).

Inductive Resolved : (list ascii) -> Prop :=
  | DS : forall t, ~ In "U" t -> Resolved t.

Reserved Notation "e ':' t" (at level 50, left associativity).
Inductive TypedExpression : (Expr * DialectType) -> Type :=
  | TE_Symbols_Add   : 
      forall core op s syms,
        TypedSymbol (op, Type_Base s) ->
        (Delta core) : (Type_Delta syms) ->
        (Add_operation op (Delta core)) : (Type_Delta (s::syms))
  | TE_Symbols_Rem : 
      forall core op s syms,
        Resolved (s::syms) ->
        TypedSymbol (op, Type_Base s) ->
        (Delta core) : (Type_Delta syms) ->
        (Rem_operation op (Delta core)) : (Type_Delta (remove ascii_dec s syms))
  | TE_Delta  : 
      forall expr actions syms_a syms_b,
        expr : (Type_Delta syms_a) -> 
        (Delta actions) : (Type_Delta syms_b) ->
        (Delta (expr::actions)) : (Type_Delta (syms_a ++ syms_b))
where "e ':' n" := (TypedExpression (e, n)) : type_scope.

Inductive SafeTable : (Table * DialectType) -> Type :=
  | TC_Delta_Rem   : forall s syms (sym : Symbol) l l',
                       TypedSymbol (sym, Type_Base s) ->
                       SafeTable (List (l++sym::l'), Type_Delta syms) ->
                       SafeTable (List (l++l'), Type_Delta (remove ascii_dec s syms))
  | TC_Delta_App   : forall l1 s1 l2 s2,
                       SafeTable (List l1, Type_Delta s1) ->
                       SafeTable (List l2, Type_Delta s2) ->
                       SafeTable (List (l1 ++ l2), Type_Delta (s1 ++ s2))
  | TC_Delta_Add   : forall s syms sym l,
                       TypedSymbol (sym, Type_Base s) ->
                       SafeTable (List l, Type_Delta syms) ->
                       SafeTable (List (sym::l), Type_Delta (s::syms)).

Lemma TypeLemma :
  forall expr v t,
  Apply (expr, v) -> 
  expr : (Type_Delta t) -> 
  Resolved t ->  
  SafeTable (v, Type_Delta t).
Proof.
  intros expr v t HEv HTy HWf.
  generalize dependent t.
  dependent induction HEv;
  intros t HExp HDef;
  inversion HExp; subst. 
  - constructor; auto.
    eapply IHHEv; eauto.
    constructor.
    inversion HDef; subst.
    intro. apply H.
    apply in_cons.
    assumption.
  - econstructor; eauto. 
    eapply IHHEv; eauto.
    constructor.
    inversion H2; subst.
    intro. apply H.
    apply in_cons.
    assumption.
  - econstructor; [eapply IHHEv1 | eapply IHHEv2 ]; eauto;
    inversion HDef; subst;
    constructor; intro; apply H; 
    apply in_or_app; [ left | right ]; auto.
Qed.

Ltac solve_consistency :=
  repeat match goal with
          | [ |- Resolved _] => constructor
          | [ |-  ~ In _ _] =>  apply not_in_cons; intuition
          | [ H : _ = _  |- False ] => discriminate H
          | [ H : In _ _ |- False ] => apply in_inv in H; intuition            
         end.