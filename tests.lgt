:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paul Brown',
		date is 2020/10/28,
		comment is 'Unit tests for tictactoe.'
	]).

    cover(fluent).
    cover(action).
    cover(situations).

    % Test fluents
    test(holds_in_s0, true(P == closed)) :-
        door_position(P)::holds(s0).
    test(fails_in_s0, fail) :-
        door_position(open)::holds(s0).

    test(holds_on_action, true([D, V] == [light, on])) :-
        power(D, V)::holds(do(turn_on(light), s0)).
    test(actions_clobber, fail) :-
        power(light, on)::holds(do(turn_off(light), do(turn_on(light), s0))).

    test(irrelevant_actions_skip, true(P == open)) :-
        door_position(P)::holds(do(a, do(b, do(c, do(d, do(open_door, s0)))))).

    % Test actions
    test(poss_in_s0, true) :-
        open_door::poss(s0),
        turn_on(light)::poss(s0).
    test(poss_after_actions, true) :-
        S = do(open_door, do(turn_on(light), s0)),
        close_door::poss(S),
        turn_off(light)::poss(S).

    test(doing_action, true(S == do(open_door, s0))) :-
        open_door::do(s0, S).
    test(cant_do_action, fail) :-
        close_door::do(s0, _).
    test(action_to_front, true(S == do(close_door, do(open_door, s0)))) :-
        close_door::do(do(open_door, s0), S).

    % Test situations
    test(prior_transitive, true) :-
        ^^assertion(situation::prior(do(a, do(b, do(c, s0))), do(b, do(c, s0)))),
        ^^assertion(situation::prior(do(a, do(b, do(c, s0))), do(c, s0))),
        ^^assertion(situation::prior(do(a, do(b, do(c, s0))), s0)).

    test(plain_holds, true(P == closed)) :-
        situation::holds(door_position(P), s0).
    test(query_holds, true([P, V] == [closed, off])) :-
        situation::holds(door_position(P) and power(light, V), s0).
    test(term_holds, true(X = 3)) :-
        situation::holds(X is 1 + 2, s0).
    test(term_holds_builtin, true(Ls = [a, b, c])) :-
        findall(X, situation::holds(list::member(X, [a, b, c]), s0), Ls).
    test(term_holds_other_object, true) :-
        situation::holds(open_door::poss(s0), s0).
    test(term_holds_backend, true(C = 97)) :-
        situation::holds({atom_char(a, C)}, s0).

    test(sit_poss, true) :-
        situation::poss(open_door, s0).

:- end_object.
