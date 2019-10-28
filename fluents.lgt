:- object(fluent,
    imports(proto_hierarchy)).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/2
            , comment is 'A fluent is a prototype. It is identified by the relationship that can hold in some situations. The value(s) this relationship holds between in a situation, if any, depend upon the situation'
            ]).

   :- public(holds/1).
   :- mode(holds(-list), zero_or_more).
   :- mode(holds(+list), zero_or_one).
   :- info(holds/1,
       [ comment is 'True if the fluent holds in the situation.'
       , argnames is ['Situation']
       ]).
:- end_object.
