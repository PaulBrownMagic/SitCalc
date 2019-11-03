:- object(sitcalc,
    imports(situation_query),
    implements(situation_protocol)).

    :- info([ version is 1.1
            , author is 'Paul Brown'
            , date is 2019/11/2
            , comment is 'A situation is defined by its history of actions.'
            ]).

    empty(s0).

    do(A, S1, S2) :-
        ^^isa(A, action),
        A::do(S1, S2).

    :- meta_predicate(holds_(*, *)).
    holds_(F, S) :-
        % Is a Fluent Case
        ^^isa(F, fluent),
        F::holds(S).
    holds_(Ob::Pred, S) :-
        is_obj_fluent(Ob::Pred),
        call(Ob::Pred, S).
    holds_(F, _) :-
        % Is not a Fluent, treat as term
        nonvar(F),
        \+ ^^isa(F, fluent),
        \+ is_obj_fluent(F),
        call(F).

    is_obj_fluent(Ob::Pred) :-
        current_object(Ob),
        Ob::current_predicate(fluent/1),
        Ob::fluent(Func/Ar),
        SAr is Ar - 1,
        functor(Pred, Func, SAr).


    :- public(poss/2).
    :- mode(poss(+object, +list), zero_or_one).
    :- mode(poss(-object, +list), zero_or_more).
    :- info(poss/2,
        [ comment is 'True iff. Action is an action and it is possible in the situation.'
        , argnames is ['Action', 'Situation']
        ]).
    poss(A, S) :-
        ^^isa(A, action),
        A::poss(S).

    :- public(prior/2).
    :- mode(prior(?list, +list), zero_or_more).
    :- info(prior/2,
       [ comment is 'Prior situations to the current one (transitive).'
       , argnames is ['Situation', 'Prior']
       ]).
    prior(do(_, S), P) :-
        S = P ; prior(S, P).

:- end_object.
