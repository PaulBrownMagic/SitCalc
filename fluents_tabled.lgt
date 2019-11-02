:- object(fluent,
    implements(fluent_protocol),
    imports(proto_hierarchy)).

    :- info([ version is 1.1
            , author is 'Paul Brown'
            , date is 2019/11/2
            , comment is 'A fluent is a prototype. It is identified by the relationship that can hold in some situations. The value(s) this relationship holds between in a situation, if any, depend upon the situation'
            ]).

   :- table(holds/1).

:- end_object.
