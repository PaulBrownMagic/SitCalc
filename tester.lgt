:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load([ meta(loader)
                 , hierarchies(loader)
                 , types(loader)
                 , situations(loader)
                 ]),
	logtalk_load(lgtunit(loader)),
	logtalk_load([fluent, action, sitcalc], [source_data(on), debug(on)]),
	logtalk_load(simple_example),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
