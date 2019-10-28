:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load([types(loader), meta(loader), hierarchies(loader)]),
	logtalk_load(lgtunit(loader)),
	logtalk_load([situations], [source_data(on), debug(on)]),
	logtalk_load(simple_example),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
