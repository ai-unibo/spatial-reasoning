numbers_diagram([
		constr(I/(variable(I),var_shape(I,circle))->(var_value(I,X),X in 3..8)),
		constr(L/(line(L),length(L,3))->(line_vals(L,LX),sum(LX,#=,18)))
	],
	constr(R,(vars_by_position([bottom,left],[I|_]),var_value(I,R)))
).
