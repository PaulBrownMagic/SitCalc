:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paul Brown',
		date is 2020/10/22,
		comment is 'Unit tests for tictactoe.'
	]).

    cover(fluent).
    cover(action).
    cover(situations).

    % Test fluents
    test(holds_in_s0, true(P == closed)) :-
        door_position(P)::holds([]).
    test(fails_in_s0, fail) :-
        door_position(open)::holds([]).

    test(holds_on_action, true([D, V] == [light, on])) :-
        power(D, V)::holds([turn_on(light)]).
    test(actions_clobber, fail) :-
        power(light, on)::holds([turn_off(light), turn_on(light)]).

    test(irrelevant_actions_skip, true(P == open)) :-
        door_position(P)::holds([a, b, c, d, open_door]).

    % Test actions
    test(poss_in_s0, true) :-
        open_door::poss([]),
        turn_on(light)::poss([]).
    test(poss_after_actions, true) :-
        S = [open_door, turn_on(light)],
        close_door::poss(S),
        turn_off(light)::poss(S).

    test(doing_action, true(S == [open_door])) :-
        open_door::do([], S).
    test(cant_do_action, fail) :-
        close_door::do([], _).
    test(action_to_front, true(S == [close_door, open_door])) :-
        close_door::do([open_door], S).

    % Test situations
    test(prior_transitive, true) :-
        situation::prior([a, b, c], [b, c]),
        situation::prior([a, b, c], [c]),
        situation::prior([a, b, c], []).

    test(plain_holds, true(P == closed)) :-
        situation::holds(door_position(P), []).
    test(query_holds, true([P, V] == [closed, off])) :-
        situation::holds(door_position(P) and power(light, V), []).
    test(term_holds, true(X = 3)) :-
        situation::holds(X is 1 + 2, []).

    test(sit_poss, true) :-
        situation::poss(open_door, []).


:- end_object.
