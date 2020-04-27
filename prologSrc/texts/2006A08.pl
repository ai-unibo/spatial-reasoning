numbers_diagram([
		constr(I/variable(I)->(var_value(I,X),X in 1\/3..7)),
		constr(all_different),
		constr(L/(line(L),length(L,3))->(line_vals(L,LX),sum(LX,#=,12)))
	],
	constr(R,write_vars(R))
).
