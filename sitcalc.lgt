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
        conforms_to_protocol(A, action_protocol),
        A::do(S1, S2).

    :- meta_predicate(holds_(*, *)).
    holds_(F, S) :-
        % Is a Fluent Case
        conforms_to_protocol(F, fluent_protocol),
        F::holds(S).
    holds_(Ob::Pred, S) :-
        is_obj_fluent(Ob::Pred),
        call(Ob::Pred, S).
    holds_(F, _) :-
        % Is not a Fluent, treat as term
        nonvar(F),
        \+ conforms_to_protocol(F, fluent_protocol),
        \+ is_obj_fluent(F),
        catch(call(F), error(existence_error(procedure, _), _), fail).

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
        conforms_to_protocol(A, action_protocol),
        A::poss(S).

    :- public(prior/2).
    :- mode(prior(?term, +term), zero_or_more).
    :- info(prior/2,
       [ comment is 'Prior situations to the current one (transitive).'
       , argnames is ['Situation', 'Prior']
       ]).
    prior(do(_, S), P) :-
        S = P ; prior(S, P).

    :- public(member/2).
    :- mode(member(?term, +term), zero_or_more).
    :- info(member/2,
        [ comment is 'Action is a member, or done in, the Situation term.'
        , argnames is ['Action', 'Situation']
        ]).
    member(Action, do(Action, _)).
    member(Action, do(_, Prior)) :-
        member(Action, Prior).

    :- public(memberchk/2).
    :- mode(memberchk(+term, +term), zero_or_one).
    :- info(memberchk/2,
        [ comment is 'Check if the Action is in the Situation term.'
        , argnames is ['Action', 'Situation']
        ]).
    memberchk(Action, do(Action, _)) :- !.
    memberchk(Action, do(_, Prior)) :-
        memberchk(Action, Prior).

    :- public(situation_list/2).
    :- mode(situation_list(?term, ?list), zero_or_one).
    :- info(situation_list/2,
        [ comment is 'Convert between the Situation term and a list representation.'
        , argnames is ['Situation', 'List']
        ]).
    situation_list(s0, []).
    situation_list(do(A, S), [A|L]) :-
        situation_list(S, L).

:- end_object.
