:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.3,
		author is 'Paul Brown',
		date is 2020/11/3,
		comment is 'Unit tests for SitCalc.'
	]).

    cover(fluent).
    cover(action).
    cover(sitcalc).

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
        ^^assertion(sitcalc::prior(do(a, do(b, do(c, s0))), do(b, do(c, s0)))),
        ^^assertion(sitcalc::prior(do(a, do(b, do(c, s0))), do(c, s0))),
        ^^assertion(sitcalc::prior(do(a, do(b, do(c, s0))), s0)).

    test(plain_holds, true(P == closed)) :-
        sitcalc::holds(door_position(P), s0).
    test(query_holds, true([P, V] == [closed, off])) :-
        sitcalc::holds(door_position(P) and power(light, V), s0).
    test(term_holds, true(X = 3)) :-
        sitcalc::holds(X is 1 + 2, s0).
    test(term_holds_builtin, true(Ls = [a, b, c])) :-
        findall(X, sitcalc::holds(list::member(X, [a, b, c]), s0), Ls).
    test(term_holds_other_object, true) :-
        sitcalc::holds(open_door::poss(s0), s0).
    test(term_holds_backend, true(C = 97)) :-
        sitcalc::holds({char_code(a, C)}, s0).

    test(sit_poss, true) :-
        sitcalc::poss(open_door, s0).

    test(member_action, true(A == foo)) :-
        sitcalc::member(A, do(foo, s0)).
    test(no_member, fail) :-
        sitcalc::member(_, s0).
    test(member_order, true(As == [foo, bar, baz])) :-
        findall(A, sitcalc::member(A, do(foo, do(bar, do(baz, s0)))), As).

    test(memberchk_first, true) :-
        sitcalc::memberchk(foo, do(foo, do(bar, do(baz, s0)))).
    test(memberchk_deep, true) :-
        sitcalc::memberchk(baz, do(foo, do(bar, do(baz, s0)))).
    test(memberchk_not_member, fail) :-
        sitcalc::memberchk(nosuchaction, do(foo, do(bar, do(baz, s0)))).

    test(situation_list_forwards, true(L == [foo, bar, baz])) :-
        sitcalc::situation_list(do(foo, do(bar, do(baz, s0))), L).
    test(situation_list_backwards, true(S == do(foo, do(bar, do(baz, s0))))) :-
        sitcalc::situation_list(S, [foo, bar, baz]).
    test(situation_list_empty, true) :-
        sitcalc::situation_list(s0, []).

    test(sit_length_forwards, true(L == 3)) :-
        sitcalc::length(do(foo, do(bar, do(baz, s0))), L).
    test(sit_length_backwards, true(S = do(foo, do(bar, do(baz, s0))))) :-
        sitcalc::length(S, 3), S = do(A, do(B, do(C, s0))), var(A), var(B), var(C).

    test(executable_ground, true) :-
        sitcalc::executable(do(open_door, do(turn_on(light), s0))).
    test(executable_var_first, true(S == s0)) :-
        sitcalc::executable(S).
:- end_object.
