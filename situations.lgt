:- op(800, xfy, and).
:- op(850, xfy, or).
:- op(870, xfy, implies).
:- op(880, xfy, equivalentTo).
:- op(600, fy, not).


:- object(action,
    implements(action_protocol),
    imports(proto_hierarchy)).

    :- info([ version is 1.1
            , author is 'Paul Brown'
            , date is 2019/11/2
            , comment is 'An action to be extended by domain actions.'
            ]).

    do(S, do(Self, S)) :-
        ::poss(S),
        self(Self).
:- end_object.


:- object(situation,
    implements(situation_protocol)).

    :- info([ version is 1.1
            , author is 'Paul Brown'
            , date is 2019/11/2
            , comment is 'A situation is defined by its history of actions.'
            ]).

    empty(s0).

    holds(Q, S) :-
        ( var(Q) ; \+ compound_query(Q) ),
        holds_(Q, S).
    holds(Q, S) :-
        nonvar(Q),
        compound_query(Q),
        query(Q, S).

    do(A, S1, S2) :-
        implements_protocol(A, action),
        A::do(S1, S2).

    :- public(prior/2).
    :- mode(prior(?list, +list), zero_or_more).
    :- info(prior/2,
       [ comment is 'Prior situations to the current one (transitive).'
       , argnames is ['Situation', 'Prior']
       ]).
    prior(do(_, S), P) :-
        S = P ; prior(S, P).

   :- private(holds_/2).
   :- meta_predicate(holds_(*, *)).
   :- mode(holds_(?object, +list), zero_or_more).
   :- mode(holds_(+term, +list), zero_or_more).
   :- info(holds_/2,
        [ comment is 'Helper for holds/1, distinguishes from fluents that hold and Logtalk/Prolog terms.'
        , argnames is ['FluentOrTerm', 'Situation']
        ]).
   holds_(F, S) :-
       % Is a Fluent Case
       fluent::descendant(F),
       F::holds(S).
   holds_(Ob::Pred, S) :-
       is_obj_fluent(Ob::Pred),
       call(Ob::Pred, S).
   holds_(F, _) :-
       % Is not a Fluent, treat as term
       nonvar(F),
       \+ fluent::descendant(F),
       \+ is_obj_fluent(F),
       call(F).

   is_obj_fluent(Ob::Pred) :-
       current_object(Ob),
       Ob::current_predicate(fluent/1),
       functor(Pred, Func, Ar),
       NAr is Ar + 1,
       Ob::fluent(Func/NAr).


   :- public(poss/2).
   :- mode(poss(+object, +list), zero_or_one).
   :- mode(poss(-object, +list), zero_or_more).
   :- info(poss/2,
       [ comment is 'True iff. Action is an action and it is possible in the situation.'
       , argnames is ['Action', 'Situation']
       ]).
   poss(A, S) :-
       action::descendant(A),
       A::poss(S).


   % Transform holds query into single fluent subgoals and see if they hold
   :- meta_predicate(query(*, *)).
   query(P and Q, S) :- nonvar(P), nonvar(Q),
       query(P, S), query(Q, S).
   query(P or Q, S) :- nonvar(P), nonvar(Q),
       query(P, S); query(Q, S).
   query(P implies Q, S) :- nonvar(P), nonvar(Q),
       query(not P or Q, S).
   query(P equivalentTo Q, S) :- nonvar(P), nonvar(Q),
       query((P implies Q) and (Q implies P), S).
   query(not (not P), S) :- nonvar(P),
       query(P, S).
   query(not (P and Q), S) :- nonvar(P), nonvar(Q),
       query(not P or not Q, S).
   query(not (P or Q), S) :- nonvar(P), nonvar(Q),
       query(not P and not Q, S).
   query(not (P implies Q), S) :- nonvar(P), nonvar(Q),
       query(not (not P or Q), S).
   query(not (P equivalentTo Q), S) :- nonvar(P), nonvar(Q),
       query(not ((P implies Q) and (Q implies P)), S).
   query(not P, S) :- nonvar(P), \+ compound_query(P),
       \+ holds_(P, S).
   query(P, S) :- \+ compound_query(P),
       holds_(P, S).

   % test if arg is a query term that requires transformation
   compound_query(not _).
   compound_query(_ and _).
   compound_query(_ or _).
   compound_query(_ implies _).
   compound_query(_ equivalentTo _).

:- end_object.
