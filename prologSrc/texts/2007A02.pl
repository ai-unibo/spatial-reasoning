numbers_diagram([
		%constr(I/variable(I)->(var_value(I,X),X in 1..9)),% little help in case the image definition is low
		constr(L/(line(L),length(L,3))->(line_vals(L,LX),sum(LX,#=,15)))
	],
	constr(R,write_vars(R))
).
