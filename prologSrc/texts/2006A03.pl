diagram([
		constr(I/variable(I)->(var_value(I,X),count_vars(N),X in 1..N)),
		constr((I1,I2)/(variable(I1),variable(I2),I1\=I2,adjacent(I1,I2))->(var_value(I1,X1),var_value(I2,X2),X1#\=X2)),
		constr(minimize_max)
	],
	constr(R,(vals(LX),max_list(LX,R)))
).