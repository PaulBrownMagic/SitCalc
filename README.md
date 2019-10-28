# SitCalc


SitCalc is a framework for managing state in an application without
mutation based on situation calculus.

![Workflow Status](https://github.com/PaulBrownMagic/SitCalc/workflows/Workflow/badge.svg)
[Code Coverage Report](https://paulbrownmagic.github.io/SitCalc/coverage_report.html)

In this library a situation is like a list of actions, but the syntax is
a little more verbose. The more recent actions come first:

```logtalk
do(get_married, do(meet_spouse, do(..., do(learn_to_walk, do(be_born, s0))...)))
```

Where `s0` denotes the situation in which nothing has happened
yet. This list is what is passed around your application.

To change this list, you *should* only do so through actions, which
extend the action class:

```logtalk
:- object(drop(_Item_),
    extends(action)).

    poss(Situation) :-
        holding(_Item_)::holds(Situation).

:- end_object.
```

This describes a drop action that is only possible when holding the
item. All actions must have possible conditions, even if they're just
always possible: `poss(_).`

An action is done like so:

```logtalk

?- Sit = do(pick_up(ball), s0), drop(ball)::do(Sit, NextSit).
```

The values that change in the application are called fluents (because
this is situation calculus). We need to define in what situations they
hold with what values. Note, in Logtalk the objects are identified by
their functors.

```logtalk
:- object(holding(_Item_),
    extends(fluent)).

	% Initial situation:
	holds(s0) :- _Item_ == pen.

	% Actions change what's held
	holds(do(A, _)) :-
	    A = pick_up(_Item_).

	% Or we were already holding it and we're not dropping it now
	holds(do(A, S)) :-
	   holds(S),
	   A \= drop(_Item_). % not holding it if we're dropping it

:- end_object.
```

Declaring these takes a little getting used to, but quickly becomes
quite repetitive. We can query them like so:

```logtalk
?- holding(What)::holds(s0).
What = pen.

?- holding(What)::holds(do(pick_up(ball), s0)).
What = ball ;
What = pen.

?- holding(What)::holds(do(drop(pen), do(pick_up(ball), s0))).
What = ball.
```

Finally, the situation object has a couple of utility predicates:

```logtalk
?- situation::poss(A, s0).
A = pick_up(ball).

?- situation::prior(do(pick_up(ball), do(drop(pen), s0)), P).
P = do(drop(pen), s0) ;
P = s0.

?- situation::holds(holding(pen) and holding(ball), do(pick_up(ball), s0)).
true.
```

The `holds/2` predicate on the situation object is quite different, it
also contains query composition operators: `and`, `or`, `not`,
`implies`, and `equivalentTo`. These are transformed via the revised
Lloyd Topper transformations from Reiter, and then the individual
fluents or other goals are called. This is useful when defining `poss/1`
for actions:

```logtalk
:- object(boil_kettle,
    extends(action)).

	poss(S) :-
	    situation::holds(power(kettle, on) and not kettle_water(empty), S).

:- end_object.
```

As we're in Prolog, `not` has the same meaning as `\+`.

Finally, to persist a situation between sessions, persist the list of
actions and reload. Long sequences of actions will become slow to query,
choose the applications you use this for wisely. To aid with this, we
apply tabling to the holds predicate if the Logtalk backend supports
this feature. Should you require a state-handling solution that clobbers
fluents, you may find
[STRIPState](https://github.com/PaulBrownMagic/STRIPState) more useful.
