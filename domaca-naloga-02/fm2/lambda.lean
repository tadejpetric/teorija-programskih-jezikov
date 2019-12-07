-- SYNTAX DEFINITIONS

inductive ty : Type
| unit : ty
| bool : ty
| arrow : ty -> ty -> ty
| prod : ty -> ty -> ty
| list : ty -> ty


def ident := string


inductive tm : Type
| var : ident -> tm
| unit : tm
| true : tm
| false : tm
| app : tm -> tm -> tm
| lam : ident -> tm -> tm
| if_then_else : tm -> tm -> tm -> tm
| pair : tm -> tm -> tm
| fst : tm -> tm
| snd : tm -> tm
| nil : tm
| cons : tm -> tm -> tm
| list_match : tm -> tm -> ident -> ident -> tm -> tm


def subst : ident -> tm -> tm -> tm
| x e (tm.var y) :=
    if x = y then e else tm.var y
| x e tm.unit :=
    tm.unit
| x e tm.true :=
    tm.true
| x e tm.false :=
    tm.false
| x e (tm.app e1 e2) :=
    tm.app (subst x e e1) (subst x e e2)
| x e (tm.lam y e') :=
    if x = y then tm.lam y e' else tm.lam y (subst x e e')
| x e (tm.if_then_else e' e1 e2) :=
    tm.if_then_else (subst x e e') (subst x e e1) (subst x e e2)
| x e (tm.pair e1 e2) :=
    tm.pair (subst x e e1) (subst x e e2)
| x e (tm.fst e1) :=
    tm.fst (subst x e e1)
| x e (tm.snd e1) :=
    tm.snd (subst x e e1)
| x e tm.nil :=
    tm.nil
| x e (tm.cons e1 e2) :=
    tm.cons (subst x e e1) (subst x e e2)
| x e (tm.list_match e0 e1 y ys e2) :=
    if (x = y) || (x = ys) then
      (tm.list_match (subst x e e0) (subst x e e1) y ys e2)
    else
      (tm.list_match (subst x e e0) (subst x e e1) y ys (subst x e e2)) 


-- OPERATIONAL SEMANTICS

inductive value : tm -> Prop
| unit : value tm.unit
| true : value tm.true
| false : value tm.false
| lam {x e} : value (tm.lam x e)
| pair {e1 e2} : value e1 -> value e2 -> value (tm.pair e1 e2)
| nil : value tm.nil
| cons {e es} : value e -> value es -> value (tm.cons e es)


inductive step : tm -> tm -> Prop
| app1 {e1 e1' e2} :
    step e1 e1' ->
    step (tm.app e1 e2) (tm.app e1' e2)
| app2 {v1 e2 e2'} :
    value v1 ->
    step e2 e2' ->
    step (tm.app v1 e2) (tm.app v1 e2')
| app_beta {x e1 v2} :
    value v2 ->
    step (tm.app (tm.lam x e1) v2) (subst x v2 e1)
| if_then_else {e e' e1 e2} :
    step e e' ->
    step (tm.if_then_else e e1 e2) (tm.if_then_else e' e1 e2)
| if_true {e1 e2} :
    step (tm.if_then_else tm.true e1 e2) e1
| if_false {e1 e2} :
    step (tm.if_then_else tm.false e1 e2) e2
| pair1 {e1 e1' e2} :
    step e1 e1' ->
    step (tm.pair e1 e2) (tm.pair e1' e2)
| pair2 {v1 e2 e2'} :
    value v1 ->
    step e2 e2' ->
    step (tm.pair v1 e2) (tm.pair v1 e2')
| fst_step {e e'} :
    step e e' ->
    step (tm.fst e) (tm.fst e')
| fst_beta {v1 v2} :
    value (tm.pair v1 v2) ->
    step (tm.fst (tm.pair v1 v2)) v1
| snd_step {e e'} :
    step e e' ->
    step (tm.snd e) (tm.snd e')
| snd_beta {v1 v2} :
    value (tm.pair v1 v2) ->
    step (tm.snd (tm.pair v1 v2)) v2
| cons1 {e e' es} :
    step e e' ->
    step (tm.cons e es) (tm.cons e' es)
| cons2 {v es es'} :
    value v ->
    step es es' ->
    step (tm.cons v es) (tm.cons v es')
| list_match_step {e e' e1 x xs e2} :
    step e e' ->
    step (tm.list_match e e1 x xs e2) (tm.list_match e' e1 x xs e2)
| list_match_nil {e1 x xs e2} :
    step (tm.list_match (tm.nil) e1 x xs e2) e1
| list_match_cons {v vs e1 x xs e2} :
    step 
      (tm.list_match (tm.cons v vs) e1 x xs e2) 
      (subst x v (subst xs vs e2))


-- TYPE SYSTEM

inductive ctx : Type
| nil : ctx
| cons : ctx -> ident -> ty -> ctx


inductive lookup : ident -> ctx -> ty -> Prop
| here {Γ x A} :
    lookup x (ctx.cons Γ x A) A
| there {x y A B Γ} :
    x ≠ y ->
    lookup x Γ A ->
    lookup x (ctx.cons Γ y B) A


inductive of : ctx -> tm -> ty -> Prop
| var {x Γ A} :
    lookup x Γ A ->
    of Γ (tm.var x) A
| unit {Γ} :
    of Γ tm.unit ty.unit
| true {Γ} :
    of Γ tm.true ty.bool
| false {Γ} :
    of Γ tm.false ty.bool
| app {Γ e1 e2 A B} :
    of Γ e1 (ty.arrow A B) ->
    of Γ e2 A ->
    of Γ (tm.app e1 e2) B
| lam {Γ x e A B} :
    of (ctx.cons Γ x A) e B ->
    of Γ (tm.lam x e) (ty.arrow A B)
| if_then_else {Γ e e1 e2 A} :
    of Γ e ty.bool ->
    of Γ e1 A ->
    of Γ e2 A ->
    of Γ (tm.if_then_else e e1 e2) A
| pair {Γ e1 e2 A B} :
    of Γ e1 A ->
    of Γ e2 B ->
    of Γ (tm.pair e1 e2) (ty.prod A B)
| fst {Γ e A B} :
    of Γ e (ty.prod A B) ->
    of Γ (tm.fst e) A
| snd {Γ e A B} :
    of Γ e (ty.prod A B) ->
    of Γ (tm.snd e) B
| nil {Γ A} :
    of Γ tm.nil (ty.list A)
| cons {Γ e es A} :
    of Γ e A ->
    of Γ es (ty.list A) ->
    of Γ (tm.cons e es) (ty.list A)
| list_match {Γ e e1 x xs e2 A B} :
    of Γ e (ty.list A) ->
    of Γ e1 B ->
    of (ctx.cons (ctx.cons Γ x A) xs (ty.list A)) e2 B ->
    of Γ (tm.list_match e e1 x xs e2) B


-- AUXILIARY LEMMAS

lemma substitution {Γ x A e e' A'} :
    of Γ e A
    -> of (ctx.cons Γ x A) e' A'
    -> of Γ (subst x e e') A'
:=
begin
  intros H,
  generalize ctx_cons : (ctx.cons Γ x A) = Γ',
  intros H',
  induction H',
  repeat {simp},
  case of.var {
      rewrite <- ctx_cons at H'_a,
      unfold subst,
      cases H'_a,
      case lookup.here {
        simp,
        assumption
      },
      case lookup.there {
        by_cases (x = H'_x),
          have H := (ne.symm H'_a_a), contradiction,
          simp [h], apply of.var, assumption
      }
  },
  case of.unit {
      apply of.unit
  },
  case of.true {
      apply of.true
  },
  case of.false {
      apply of.false
  },
  case of.app {
      apply of.app,
      apply H'_ih_a ctx_cons,
      apply H'_ih_a_1 ctx_cons
  },
  case of.if_then_else {
      apply of.if_then_else,
      apply H'_ih_a ctx_cons,
      apply H'_ih_a_1 ctx_cons,
      apply H'_ih_a_2 ctx_cons
  },
  case of.lam {
      unfold subst,
      by_cases (x = H'_x),
        simp [h], apply of.lam, sorry,
        simp [h], apply of.lam, sorry
  },
  case of.pair {
      unfold subst,
      apply of.pair,
        apply H'_ih_a ctx_cons,
        apply H'_ih_a_1 ctx_cons,
  },
  case of.fst {
      unfold subst,
      apply of.fst,
        apply H'_ih ctx_cons,
  },
  case of.snd {
      unfold subst,
      apply of.snd,
        apply H'_ih ctx_cons,
  },
  case of.nil {
      unfold subst, apply of.nil,
  },
  case of.cons {
      unfold subst,
      apply of.cons,
        apply H'_ih_a ctx_cons,
        apply H'_ih_a_1 ctx_cons,
  },
  case of.list_match {
    unfold subst,
    by_cases (x = H'_x),
      subst h, 
        simp,
        apply of.list_match, 
        apply H'_ih_a ctx_cons,
        apply H'_ih_a_1 ctx_cons,
        sorry,
      by_cases (x = H'_xs),
        subst h, 
          simp, 
          apply of.list_match,
          apply H'_ih_a ctx_cons,
          apply H'_ih_a_1 ctx_cons,
          sorry,
        dedup, 
          simp [h], simp [h_1], 
          apply of.list_match,
          apply H'_ih_a ctx_cons,
          apply H'_ih_a_1 ctx_cons,
          sorry,
  }
end


lemma weakening {Γ e A x B} :
    of Γ e A -> of (ctx.cons Γ x B) e A
:=
begin
    sorry
end


-- SAFETY THEOREMS

theorem preservation {e e'} :
    step e e' ->
    forall {Γ A}, of Γ e A ->
    of Γ e' A
:=
begin
    intros Hstep,
    induction Hstep,
    repeat {intros Γ A Hof},
    case step.app_beta {
        cases Hof,
        cases Hof_a,
        apply substitution Hof_a_1 Hof_a_a
    },
    case step.app1 {
        cases Hof,
        apply of.app,
        apply Hstep_ih Hof_a,
        apply Hof_a_1
    },
    case step.app2 {
        cases Hof,
        apply of.app,
        apply Hof_a,
        apply Hstep_ih Hof_a_1
    },
    case step.if_then_else {
        cases Hof,
        apply of.if_then_else,
        apply Hstep_ih Hof_a,
        apply Hof_a_1,
        apply Hof_a_2
    },
    case step.if_true {
        cases Hof,
        apply Hof_a_1
    },
    case step.if_false {
        cases Hof,
        apply Hof_a_2
    },
    repeat {sorry},
end


theorem progress {e A} :
    of ctx.nil e A ->
    (value e) ∨ (exists e', step e e')
:=
begin
    generalize empty : ctx.nil = Γ,
    intros H,
    induction H,
    case of.var {
        rewrite ←empty at H_a,
        cases H_a
    },
    case of.unit {
        left,
        exact value.unit
    },
    case of.app {
        cases H_ih_a empty,
        case or.inl {
            cases H_a,
            case of.var
                {rw ←empty at H_a_a, cases H_a_a},
            case of.app
                {cases h},
            case of.lam {
                cases H_ih_a_1 empty,
                right,
                existsi (subst H_a_x H_e2 H_a_e),
                apply step.app_beta,
                assumption,
                right,
                cases h_1,
                existsi (tm.app (tm.lam H_a_x H_a_e) h_1_w),
                eapply step.app2,
                exact value.lam,
                assumption
            },
            -- everything else is impossible (cases on faulty assumption)
            repeat {cases h},
        },
        case or.inr {
            cases h with e H_step,
            right,
            existsi (tm.app e H_e2),
            apply step.app1,
            assumption
        }
    },
    case of.lam {
        left,
        exact value.lam
    },
    case of.true {
        left,
        exact value.true
    },
    case of.false {
        left,
        exact value.false
    },
    case of.if_then_else {
        cases H_ih_a empty,
        case or.inl {
            cases H_a,
            case of.var {
                rw ←empty at H_a_a,
                cases H_a_a
            },
            case of.true {
                right,
                existsi H_e1,
                exact step.if_true
            },
            case of.false {
                right,
                existsi H_e2,
                exact step.if_false
            },
            repeat {cases h},
        },
        case or.inr {
            cases h,
            right,
            existsi (tm.if_then_else h_w H_e1 H_e2),
            exact (step.if_then_else h_h),
        }
  },
  repeat {sorry},
end
