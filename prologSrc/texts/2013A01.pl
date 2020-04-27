numbers_diagram([
		constr(I/variable(I)->(var_value(I,X),X in 3..6)),
		constr(all_different),
		constr((A,B)/adjacent(A,B)->(var_value(A,X),var_value(B,Y),abs(X-Y)#=1#\/abs(X-Y)#=2))
	],
	constr(R,write_vars(R))
).