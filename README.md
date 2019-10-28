# SitCalc

SitCalc is a framework for managing state in an application without
mutation based on situation calculus.

In this library a situation is a list, with the more recent actions
coming first:

```
[get_married, meet_spouse, ..., learn_to_walk, be_born]
```

Where the empty list denotes the situation in which nothing has happened
yet. This list is what is passed around your application.

To change this list, you *should* only do so through actions, which
extend the action class:

```
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

```
?- drop(ball)::do([pick_up(ball)], NextSit]).
```

The values that change in the application are called fluents (because
this is situation calculus). We need to define in what situations they
hold with what values. Note, in Logtalk the objects are identified by
their functors.

```
:- object(holding(_Item_),
    extends(fluent)).

	% Initial situation:
	holds([]) :- _Item_ == pen.

	% Actions change what's held
	holds([A|_]) :-
	    A = pick_up(_Item_).

	% Or we were already holding it and we're not dropping it now
	holds([A|S]) :-
	   A \= pick_up(_Item_), % case already dealt with
	   A \= drop(_Item_), % not holding it if we're dropping it
	   holds(S). % Prior situation

:- end_object.
```

Declaring these takes a little getting used to, but quickly becomes
quite repetitive. We can query them like so:

```
?- holding(What)::holds([]).
What = pen.

?- holding(What)::holds([pick_up(ball)]).
What = ball ;
What = pen.

?- holding(What)::holds([drop(pen), pick_up(ball)]).
What = ball.
```

Finally, the situation object has a couple of utility predicates:

```
?- situation::poss(A, []).
A = pick_up(ball).

?- situation::prior([pick_up(ball), drop(pen)], P).
P = [drop(pen)] ;
P = [].

?- situation::holds(holding(pen) and holding(ball), [pick_up(ball)]).
true.
```

The `holds/2` predicate on the situation object is quite different, it
also contains query composition operators: `and`, `or`, `not`,
`implies`, and `equivalentTo`. These are transformed via the revised
Lloyd Topper transformations from Reiter, and then the individual
fluents or other goals are called. This is useful when defining `poss/1`
for actions:

```
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
this feature.
