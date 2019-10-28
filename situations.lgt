:- op(800, xfy, and).
:- op(850, xfy, or).
:- op(870, xfy, implies).
:- op(880, xfy, equivalentTo).
:- op(200, fy, not).


:- object(action,
    imports(proto_hierarchy)).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/2
            , comment is 'An action to be extended by domain actions.'
            ]).

    :- public(poss/1).
   :- mode(poss(-list), zero_or_more).
   :- mode(poss(+list), zero_or_one).
   :- info(poss/1,
       [ comment is 'True if the action is possible in the situation.'
       , argnames is ['Situation']
       ]).

    :- public(do/2).
    :- mode(do(+list, ?list), zero_or_one).
    :- mode(do(-list, +list), zero_or_one).
    :- mode(do(-list, -list), zero_or_more).
    :- info(do/2,
        [ comment is 'True if doing the action in S1 results in S2.'
        , argnames is ['S1', 'S2']
        ]).
    do(S, [Self|S]) :-
        ::poss(S),
        self(Self).
:- end_object.


:- object(situation,
    extends(list)).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/2
            , comment is 'A situation is defined by its history of actions.'
            ]).

    :- public(prior/2).
    :- mode(prior(?list, +list), zero_or_more).
    :- info(prior/2,
       [ comment is 'Prior situations to the current one (transitive).'
       , argnames is ['Situation', 'Prior']
       ]).
    prior([_|S], P) :-
        S = P ; prior(S, P).

   :- public(holds/2).
   :- meta_predicate(holds(*, *)).
   :- mode(holds(?object, +list), zero_or_more).
   :- mode(holds(+term, +list), zero_or_more).
   :- info(holds/2,
       [ comment is 'What fluents hold in the situation. Can also be provided with a logical query term provided all fluents are nonvar: `situation::holds(power(X) and position(Y)).`'
       , argnames is ['Holding', 'Situation']
       ]).
   holds(Q, S) :-
       ( var(Q) ; \+ compound_query(Q) ),
       holds_(Q, S).
   holds(Q, S) :-
       nonvar(Q),
       compound_query(Q),
       query(Q, S).

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
   holds_(F, _) :-
       % Is not a Fluent, treat as term
       nonvar(F),
       \+ fluent::descendant(F),
       call(F).

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
