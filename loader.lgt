:- if(current_logtalk_flag(tabling, supported)).

	:- if((
		current_logtalk_flag(prolog_dialect, swi),
		current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
		Major =< 7, Minor =< 7, Patch =< 13
		)).
		:- use_module(library(tabling)).
	:- endif.
	:- initialization(logtalk::print_message(comment, core, ['Loading sitcalc with tabling supported'])).
:- endif.

:- initialization((
	logtalk_load([
				   meta(loader)
				 , hierarchies(loader)
				 , types(loader)
				 , situations(loader)
				 ]),
	logtalk_load([
				   fluent
				 , action
				 , sitcalc
				 ],
				 [
				   optimize(on)
				 ])
)).
