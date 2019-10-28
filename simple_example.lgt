/*
In this example we have a door and a light to work with.

Example queries:

```

?- situation::holds(door_position(Pos) and power(light, Power), []).
Pos = closed,
Power = off ;
false.

?- situation::poss(A, s0).
A = turn_on(light) ;
A = open_door ;
false.

?- open_door::do(s0, S1).
S1 = do(open_door, s0).

?- turn_on(light)::do($(S1), S2).
S2 = do(turn_on(light), do(open_door, s0)),
S1 = do(open_door, s0).

?- situation::prior($(S2), S).
S = do(open_door, s0),
S2 = do(turn_on(light), do(open_door, s0)) ;
S = s0,
S2 = do(turn_on(light), do(open_door, s0)) ;
false.

?- situation::holds(F, $(S2)), F::holds($(S1)).
F = door_position(open),
S2 = do(turn_on(light), do(open_door, s0)),
S1 = do(open_door, s0) ;
false.
```
*/

:- object(power(_Dev_, _V_), extends(fluent)).
    holds(s0) :-
        _Dev_ = light, _V_ = off.
    holds(do(A, _)) :-
        A = turn_on(_Dev_), _V_ = on.
    holds(do(A, _)) :-
        A = turn_off(_Dev_), _V_ = off.
    holds(do(A, S)) :-
        A \= turn_on(_Dev_),
        A \= turn_off(_Dev_),
        holds(S).
:- end_object.

:- object(door_position(_Pos_), extends(fluent)).
    holds(s0) :-
        _Pos_ = closed.
    holds(do(A, _)) :-
        A = close_door, _Pos_ = closed.
    holds(do(A, _)) :-
        A = open_door, _Pos_ = open.
    holds(do(A, S)) :-
        A \= close_door,
        A \= open_door,
        holds(S).
:- end_object.

:- object(turn_on(_Dev_), extends(action)).
    poss(S) :-
        power(_Dev_, off)::holds(S).
:- end_object.

:- object(turn_off(_Dev_), extends(action)).
   poss(S) :-
       power(_Dev_, on)::holds(S).
:- end_object.

:- object(open_door, extends(action)).
    poss(S) :-
        door_position(closed)::holds(S).
:- end_object.

:- object(close_door, extends(action)).
    poss(S) :-
        door_position(open)::holds(S).
:- end_object.
