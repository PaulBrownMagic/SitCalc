:- category(fluent,
    implements(fluent_protocol)).

    :- info([ version is 1:1:0
            , author is 'Paul Brown'
            , date is 2019-11-02
            , comment is 'A fluent is a prototype. It is identified by the relationship that can hold in some situations. The value(s) this relationship holds between in a situation, if any, depend upon the situation'
            ]).

    :- if(current_logtalk_flag(tabling, supported)).
        :- table(holds/1).
    :- endif.

:- end_category.
