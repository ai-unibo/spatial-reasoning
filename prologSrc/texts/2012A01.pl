numbers_diagram([
		constr(I/variable(I)->(var_value(I,X),X in 1..9)),
		constr(all_different),
		constr(L/line(L)->(line_vals(L,LX),sum(LX,#=,20)))
	],
	constr(R,write_vars(R))
).