:- category(action,
    implements(action_protocol)).

    :- info([ version is 1:1:0
            , author is 'Paul Brown'
            , date is 2019-11-02
            , comment is 'An action to be extended by domain actions.'
            ]).

    do(S, do(Self, S)) :-
        self(Self),
        ::poss(S).

:- end_category.
