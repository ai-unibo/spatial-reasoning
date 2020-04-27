numbers_diagram([
		constr(I/variable(I)->(var_value(I,X),X in 2..5)),
		constr(all_different),
		constr((row(R),line_vals(R,RX),col(C),line_vals(C,CX),sum(RX,#=,S),sum(CX,#=,S)))
	],
	constr(R,write_vars(R))
).