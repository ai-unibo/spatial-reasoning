%:-module(diagram,[read_diagram/2,box/4,var_polygon/3,box_polygon/2,var_shape/3,box_shape/2,var_color_rgb/3,box_color_rgb/2,var_color/3,box_color/2,var_value/3,box_value/2,var_empty/2,var_not_empty/2,box_not_empty/1,boxes/2,count_vars/2,full_empty_vars/3,vars_by_position/3,line_elements/2,line_orientation/2,lines/2,rows/2,cols/2,adjacent/4,same_line/4,line_vars/3]).
:-module(diagram,[read_diagram/2,box/4,diagram/1,variable/1,vals/1,var_polygon/2,var_shape/2,var_color_rgb/2,var_color/2,var_value/2,var_initially_empty/1,boxes/2,count_vars/1,count_vars/2,full_empty_vars/3,vars_by_position/2,line_elements/2,line_orientation/2,lines/1,rows/1,cols/1,line/1,row/1,col/1,adjacent/2,adjacent/3,same_line/2,same_line/3,line_vals/2,write_vars/1,all_different/0,minimize_max/0,maximize_min/0]).
:-use_module('color.pl').
:-use_module('debug.pl').
:-use_module('geometry.pl').
:-use_module('image.pl').
:-use_module('shapes.pl').
:-use_module('solver.pl').
:-use_module('text.pl').
:-use_module(library(apply)).
:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(pairs)).
:-use_module(library(yall)).

read_text_diagram(I,Diag):-
	read_diagram(I,true,Diag).
read_diagram(I,Diag):-
	read_diagram(I,false,Diag).
read_diagram(I,WithText,diag(LBox,LLines)):-
	find_polygons(I,[threshold(medium)],LP),
	externals(LP,LPExt),
	%(	call(WithText)
	%->	writeln("Searching for text (it may take some seconds) ...")
	%;	true
	%),	
	writeln("Searching for text (it may take some seconds) ..."),
	convlist(create_box(I,WithText),LPExt,LBox/*Ext*/),
	writeln("Reading done."),
	%include(true_box,LBoxExt,LBox),
	only_on_debug((maplist(box_polygon,LBox,LPB),show(LPB),maplist(writeln,LBox),nl)),	%%%only for debug
	
	maplist(box_polygon,LBox,LPBox),
	maplist(baricenter,LPBox,LB),
	
	findall(c(P1,P2),(member(P1,LPBox),member(P2,LPBox),P1\=P2,touch(P1,P2)),LCP),
	maplist(adjacent_to_couple(LPBox,LB),LCP,LAdjCouplesDup),
	list_to_set(LAdjCouplesDup,LAdjCouples),
	unify_lines(LAdjCouples,LB,LAdjLines),
	
	find_polygons(I,[all,threshold(medium)],LPA),
	tolerance(T),
	exclude({LPBox}/[P]>>(member(PE,LPBox),contains(PE,P)),LPA,LPAExt),
	set_tolerance(6),
	
	polygons_to_segments(LPAExt,LSeg),
	set_tolerance(T),
	exclude({LPBox}/[seg(P1,P2)]>>(member(poly(_,LV),LPBox),member_equals(P1,LV,_),member_equals(P2,LV,_)),LSeg,LSegLink),
	convlist(segment_to_couple(LPBox,LB),LSegLink,LLinkCouplesDup),
	list_to_set_equals(LLinkCouplesDup,LLinkCouples),
	unify_lines(LLinkCouples,LB,LLinkLines),
	append(LAdjLines,LLinkLines,LLines),
	%show(LSegLink),					%%%only for debug
	%sleep(10),							%%%only for debug
	!.

create_box(I,ReadText,P,Box):-
	show(P),							%%%only for debug
	tolerance(T),
	set_tolerance(1),
	clean_poly(P,PCl),
	set_tolerance(T),
	(true_box(box(P,_,_));true_box(box(PCl,_,_))),
	subimg(I,P,SubI,_),
	main_color_rgb(SubI,ColRGB),
	(	call(ReadText)
	->	read_single_number(SubI,Txt,_,_) %,writeln(Txt)
	;	Txt=""
	),
	release_img(SubI),
	box(Box,P,ColRGB,Txt).

clean_poly(Poly,PolyCl):-
	edges(Poly,LE),
	clean_edges(LE,LECl),
	edges(PolyCl,LECl).

clean_edges([S],[S]):-!.
clean_edges([seg(P1,P2),seg(P2,P3)|T],LCl):-
	aligned_ord(P1,P2,P3),
	!,
	clean_edges([seg(P1,P3)|T],LCl).
clean_edges([seg(P1,P2),seg(P2,P3)|T],[seg(P1,P2)|TCl]):-
	clean_edges([seg(P2,P3)|T],TCl).

true_box(Box):-	%the definition of this predicate is delicate and not sure
	box_shape(Box,circle).
true_box(Box):-
	box_shape(Box,rectangle).
true_box(box(P,_,_)):-
	equilateral(P).

touch(Poly1,Poly2):-
	touch_(Poly1,Poly2).
touch(Poly1,Poly2):-
	touch_(Poly2,Poly1).
touch_(Poly,poly(_,LV)):-
	edges(Poly,E),
	member(seg(P1,P2),E),
	member(P,LV),
	aligned_ord(P1,P,P2).

adjacent_to_couple(LP,LBaric,c(P1,P2),Line):-
	nth1(I1,LP,P1),
	nth1(I2,LP,P2),
	make_couple(I1,I2,LBaric,Line),
	!.

segment_to_couple(LPoly,LBaric,seg(P1,P2),Line):-
	nth1(I1,LPoly,poly(_,LV1)),
	member_equals(P1,LV1,_),
	nth1(I2,LPoly,poly(_,LV2)),
	member_equals(P2,LV2,_),
	make_couple(I1,I2,LBaric,Line),
	!.

make_couple(I1,I2,LBaric,line([A,B],Dir)):-
	nth1(I1,LBaric,B1),
	nth1(I2,LBaric,B2),
	(	ordered(B1,B2)
	->	A=I1,
		B=I2
	;	B=I1,
		A=I2
	),
	link_orientation(B1,B2,Dir).

ordered(p(X1,Y1),p(X2,Y2)):-
	tolerance(E),
	(	abs(X1-X2)>E
	->	X1<X2
	;	Y1<Y2
	).

link_orientation(p(_,Y1),p(_,Y2),horizontal):-
	tolerance(E),
	abs(Y1-Y2)=<E.
link_orientation(p(X1,_),p(X2,_),vertical):-
	tolerance(E),
	abs(X1-X2)=<E.
link_orientation(_,_,oblique).

geometry:equals(line(L1,_),line(L1,_)).
geometry:equals(line(L1,_),line(L2,_)):-
	reverse(L1,L2).

unify_lines([],_,[]):-!.
unify_lines([L],_,[L]):-!.
unify_lines([line([B,C|T],Dir)|TC],LBaric,LL):-
	append([line([B,C|T],Dir)|TC1],[line([A,B],Dir)|TC2],[line([B,C|T],Dir)|TC]),
	baricenters_aligned(LBaric,A,B,C,Dir),
	!,
	append(TC1,TC2,TC_),
	unify_lines([line([A,B,C|T],Dir)|TC_],LBaric,LL).
unify_lines([line([A|T],Dir)|TC],LBaric,LL):-
	last(T,B),
	append([line([A|T],Dir)|TC1],[line([B,C],Dir)|TC2],[line([A|T],Dir)|TC]),
	baricenters_aligned(LBaric,A,B,C,Dir),
	!,
	append([A|T],[C],L1),
	append(TC1,TC2,TC_),
	unify_lines([line(L1,Dir)|TC_],LBaric,LL).
unify_lines([L1|TC],LBaric,[L1|TL]):-
	unify_lines(TC,LBaric,TL).

baricenters_aligned(LB,A,B,C,oblique):-
	!,
	nth1(A,LB,PA),
	nth1(B,LB,PB),
	nth1(C,LB,PC),
	aligned_ord(PA,PB,PC).
baricenters_aligned(_,_,_,_,_).

box(box(Poly,Col,Txt),Poly,Col,Txt).

var_polygon(I,Poly):-
	diagram(Diag),
	var_polygon(Diag,I,Poly).
var_polygon(diag(LBox,_),I,Poly):-
	nth1(I,LBox,Box),
	box_polygon(Box,Poly).

box_polygon(box(Poly,_,_),Poly).

var_shape(I,Shape):-
	diagram(Diag),
	var_shape(Diag,I,Shape).
var_shape(diag(LBox,_),I,Shape):-
	nth1(I,LBox,Box),
	box_shape(Box,Shape).

box_shape(box(poly(N,LV),_,_),Shape):-
	polygon_type(Shape,N),
	check_polygon_type(poly(N,LV),Shape),
	!.
box_shape(box(_,_,_),unknown).

var_color_rgb(I,Col):-
	diagram(Diag),
	var_color_rgb(Diag,I,Col).
var_color_rgb(diag(LBox,_),I,Col):-
	nth1(I,LBox,Box),
	box_color_rgb(Box,Col).

box_color_rgb(box(_,Col,_),Col).

var_color(I,Col):-
	diagram(Diag),
	var_color(Diag,I,Col).
var_color(diag(LBox,_),I,Col):-
	nth1(I,LBox,Box),
	box_color(Box,Col).

box_color(box(_,RGB,_),Col):-
	color_rgb(Col,RGB).

var_value(diag(LBox,_),I,N):-	%N can remain unbound
	nth1(I,LBox,Box),
	box_value(Box,N).

box_value(box(_,_,Txt),N):-
	number_string(N,Txt),
	!.
box_value(box(_,_,_),N):-
	var(N).

var_initially_empty(I):-
	diagram(Diag),
	\+var_not_empty(Diag,I).

var_empty(Diag,I):-
	\+var_not_empty(Diag,I).

var_not_empty(diag(LBox,_),I):-
	nth1(I,LBox,Box),
	box_not_empty(Box).

box_not_empty(box(_,_,Txt)):-
	number_string(_,Txt).

boxes(diag(LBox,_),LBox).

count_vars(N):-
	diagram(diag(LBox,_)),
	length(LBox,N).
count_vars(diag(LBox,_),N):-
	length(LBox,N).

full_empty_vars(Diag,F,E):-
	count_vars(Diag,N),
	numlist(1,N,V),
	partition(var_not_empty(Diag),V,F,E).

line_elements(line(LE,_),LE).

line_orientation(line(_,O),O).

lines(LL):-
	diagram(Diag),
	lines(Diag,LL).
lines(diag(_,LLines),LL):-
	maplist(line_elements,LLines,LL).

rows(LL):-
	diagram(Diag),
	rows(Diag,LL).
rows(diag(_,LLines),LR):-
	include([L]>>line_orientation(L,horizontal),LLines,LRows),
	maplist(line_elements,LRows,LR).

cols(LL):-
	diagram(Diag),
	cols(Diag,LL).
cols(diag(_,LLines),LC):-
	include([L]>>line_orientation(L,vertical),LLines,LCols),
	maplist(line_elements,LCols,LC).

adjacent(X1,X2):-
	diagram(Diag),
	adjacent(Diag,X1,X2,_).
adjacent(X1,X2,Dir):-
	diagram(Diag),
	adjacent(Diag,X1,X2,Dir).
adjacent(diag(_,LLines),X1,X2,Dir):-
	member(line(L,Dir),LLines),
	(	append(_,[X1,X2|_],L)
	;	append(_,[X2,X1|_],L)
	).

same_line(X1,X2):-
	diagram(Diag),
	same_line(Diag,X1,X2,_).
same_line(X1,X2,Dir):-
	diagram(Diag),
	same_line(Diag,X1,X2,Dir).
same_line(diag(_,LLines),X1,X2,Dir):-
	member(line(L,Dir),LLines),
	member(X1,L),
	member(X2,L).

line_vals(LI,LX):-
	vars(Vars),
	maplist({Vars}/[I,X]>>nth1(I,Vars,X),LI,LX).

vars_by_position(LPos,LV):-
	count_vars(N),
	numlist(1,N,LI),
	maplist(var_baricenter_coords,LI,LBX,LBY),
	%pairs_keys_values(LP,LI,LB),
	vars_by_position(LBX,LBY,LPos,LI,LV).
	%pairs_keys(LPF,LV).
/*vars_by_position(_,[],LV,LV):-!.
vars_by_position(_,_,[],[]):-!.
vars_by_position(diag(LBox,LLines),[top|TPos],LV,LVR):-
	include({LBox,LLines}/[I]>>(
		member(line([I|_],vertical),LLines)
	;	member(line([I1|TI],oblique),LLines),
		last(TI,I2),
		var_baricenter_coords(diag(LBox,LLines),I1,_,Y1),
		var_baricenter_coords(diag(LBox,LLines),I2,_,Y2),
		(	Y1<Y2
		->	I=I1
		;	I=I2
		)
	),LV,LVFilt),
	maplist(var_baricenter_coords(diag(LBox,LLines)),LVFilt,_,LY),
	pairs_keys_values(LP,LY,LVFilt),
	keysort(LP,LPSort),		%sort according to direction
	pairs_values(LPSort,LVFiltSort),
	vars_by_position(diag(LBox,LLines),TPos,LVFiltSort,LVR).
vars_by_position(diag(LBox,LLines),[bottom|TPos],LV,LVR):-
	include({LBox,LLines}/[I]>>(
		member(line([_|T],vertical),LLines),
		last(T,I)
	;	member(line([I1|TI],oblique),LLines),
		last(TI,I2),
		var_baricenter_coords(diag(LBox,LLines),I1,_,Y1),
		var_baricenter_coords(diag(LBox,LLines),I2,_,Y2),
		(	Y1>Y2
		->	I=I1
		;	I=I2
		)
	),LV,LVFilt),
	maplist(var_baricenter_coords(diag(LBox,LLines)),LVFilt,_,LY),
	pairs_keys_values(LP,LY,LVFilt),
	keysort(LP,LPSortRev),		%sort according to direction
	reverse(LPSortRev,LPSort),
	pairs_values(LPSort,LVFiltSort),
	vars_by_position(diag(LBox,LLines),TPos,LVFiltSort,LVR).
vars_by_position(diag(LBox,LLines),[left|TPos],LV,LVR):-
	include({LBox,LLines}/[I]>>(
		member(line([I|_],horizontal),LLines)
	;	member(line([I1|TI],oblique),LLines),
		last(TI,I2),
		var_baricenter_coords(diag(LBox,LLines),I1,X1,_),
		var_baricenter_coords(diag(LBox,LLines),I2,X2,_),
		(	X1<X2
		->	I=I1
		;	I=I2
		)
	),LV,LVFilt),
	maplist(var_baricenter_coords(diag(LBox,LLines)),LVFilt,LX,_),
	pairs_keys_values(LP,LX,LVFilt),
	keysort(LP,LPSort),		%sort according to direction
	pairs_values(LPSort,LVFiltSort),
	vars_by_position(diag(LBox,LLines),TPos,LVFiltSort,LVR).
vars_by_position(diag(LBox,LLines),[right|TPos],LV,LVR):-
	include({LBox,LLines}/[I]>>(
		member(line([_|T],horizontal),LLines),
		last(T,I)
	;	member(line([I1|TI],oblique),LLines),
		last(TI,I2),
		var_baricenter_coords(diag(LBox,LLines),I1,X1,_),
		var_baricenter_coords(diag(LBox,LLines),I2,X2,_),
		(	X1>X2
		->	I=I1
		;	I=I2
		)
	),LV,LVFilt),
	maplist(var_baricenter_coords(diag(LBox,LLines)),LVFilt,LX,_),
	pairs_keys_values(LP,LX,LVFilt),
	keysort(LP,LPSortRev),		%sort according to direction
	reverse(LPSortRev,LPSort),
	pairs_values(LPSort,LVFiltSort),
	vars_by_position(diag(LBox,LLines),TPos,LVFiltSort,LVR).
*/
vars_by_position(_,_,[],LV,LV):-!.
vars_by_position(_,_,_,[],[]):-!.
vars_by_position(LBX,LBY,[top|TPos],LV,LVR):-
	min_list(LBY,YMin),
	tolerance(T),
	YMax is YMin+T,
	include({LBY,YMax}/[I]>>(nth1(I,LBY,Y),Y=<YMax),LV,LVFilt),
	maplist({LBY}/[I]>>nth1(I,LBY,Y),LVFilt,LYFilt),
	pairs_keys_values(LP,LYFilt,LVFilt),
	keysort(LP,LPSort),		%sort according to direction
	pairs_values(LPSort,LVFiltSort),
	vars_by_position(LBX,LBY,TPos,LVFiltSort,LVR).
vars_by_position(LBX,LBY,[bottom|TPos],LV,LVR):-
	max_list(LBY,YMax),
	tolerance(T),
	YMin is YMax-T,
	include({LBY,YMin}/[I]>>(nth1(I,LBY,Y),Y>=YMin),LV,LVFilt),
	maplist({LBY}/[I]>>nth1(I,LBY,Y),LVFilt,LYFilt),
	pairs_keys_values(LP,LYFilt,LVFilt),
	keysort(LP,LPSortRev),		%sort according to direction
	reverse(LPSortRev,LPSort),
	pairs_values(LPSort,LVFiltSort),
	vars_by_position(LBX,LBY,TPos,LVFiltSort,LVR).
vars_by_position(LBX,LBY,[left|TPos],LV,LVR):-
	min_list(LBX,XMin),
	tolerance(T),
	XMax is XMin+T,
	include({LBX,XMax}/[I]>>(nth1(I,LBX,X),X=<XMax),LV,LVFilt),
	maplist({LBX}/[I]>>nth1(I,LBX,X),LVFilt,LXFilt),
	pairs_keys_values(LP,LXFilt,LVFilt),
	keysort(LP,LPSort),		%sort according to direction
	pairs_values(LPSort,LVFiltSort),
	vars_by_position(LBX,LBY,TPos,LVFiltSort,LVR).
vars_by_position(LBX,LBY,[right|TPos],LV,LVR):-
	max_list(LBX,XMax),
	tolerance(T),
	XMin is XMax-T,
	include({LBX,XMin}/[I]>>(nth1(I,LBX,X),X>=XMin),LV,LVFilt),
	maplist({LBX}/[I]>>nth1(I,LBX,X),LVFilt,LXFilt),
	pairs_keys_values(LP,LXFilt,LVFilt),
	keysort(LP,LPSortRev),		%sort according to direction
	reverse(LPSortRev,LPSort),
	pairs_values(LPSort,LVFiltSort),
	vars_by_position(LBX,LBY,TPos,LVFiltSort,LVR).

var_baricenter_coords(I,X,Y):-
	var_polygon(I,P),
	baricenter(P,p(X,Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solver:solve(diagram(LConstr,Result),Img,Res):-
	read_diagram(Img,Diag),
	solve_(Diag,LConstr,Result,Res).
solver:solve(numbers_diagram(LConstr,Result),Img,Res):-
	read_text_diagram(Img,Diag),
	solve_(Diag,LConstr,Result,Res).

solve_(Diag,LConstr,Result,R):-
	only_on_debug((writeln("diagram:"),writeln(Diag))),	%%%only for debug
	count_vars(Diag,N),
	length(Vars,N),
	set_diagram(Diag),
	set_vars(Vars),
	full_empty_vars(Diag,LFull,_),
	assign_box_val(Diag,Vars,LFull),
	maplist(invoke_constraint,LConstr),
	only_on_debug((maplist(fd_dom,Vars,VarsD),writeln(variables(VarsD)))),	%%%only for debug
	label(Vars),
	invoke_constraint(Result,R),
	only_on_debug(writeln(variables(Vars))).	%%%only for debug
	%writeln(result(R)).

set_diagram(D):-
	b_setval(diagram,D).

diagram(D):-
	b_getval(diagram,D).

set_vars(V):-
	b_setval(vars,V).

vars(V):-
	b_getval(vars,V).

vals(V):-
	b_getval(vars,V).

variable(I):-
	vars(LX),
	length(LX,N),
	numlist(1,N,L),
	member(I,L),
	var_initially_empty(I).

line(L):-
	lines(LL),
	member(L,LL).

row(L):-
	rows(LL),
	member(L,LL).

col(L):-
	cols(LL),
	member(L,LL).

var_value(X,V):-
	vars(LX),
	nth1(X,LX,V).

assign_box_val(_,_,[]).
assign_box_val(Diag,Vars,[I|TI]):-
	var_value(Diag,I,N),
	nth1(I,Vars,N),
	assign_box_val(Diag,Vars,TI).

assign_constraints_on_vars(Diag,Vars,LVarConstr):-
	length(Vars,N),
	numlist(1,N,LI),
	assign_constraints_on_vars(Diag,Vars,LI,LVarConstr).
	
assign_constraints_on_vars(_,[],[],_).
assign_constraints_on_vars(Diag,[X|TVars],[I|TI],LVarConstr):-
	(	var_not_empty(Diag,I)
	->	var_value(Diag,I,X)
	;	assign_constraints_on_var(Diag,X,I,LVarConstr)
	),
	assign_constraints_on_vars(Diag,TVars,TI,LVarConstr).

assign_constraints_on_var(_,_,_,[]).
assign_constraints_on_var(Diag,X,I,[dom(Check,Constr)|TVarConstr]):-
	(	invoke_constraint(Check,Diag,I)
	->	invoke_constraint(Constr,X)
	;	true
	),
	assign_constraints_on_var(Diag,X,I,TVarConstr).

assign_constraints_on_lines(_,_,[]).
assign_constraints_on_lines(Diag,Vars,[lin(Dir,Check,Constr)|TLineConstr]):-
	(	member(Dir,[lines,cols,rows])
	->	true
	;	throw("Syntax error in problem definition")
	),
	call(Dir,Diag,LLI),
	assign_constraint_on_lines(LLI,Vars,Check,Constr),
	assign_constraints_on_lines(Diag,Vars,TLineConstr).

assign_constraint_on_lines([],_,_,_).
assign_constraint_on_lines([LI|TLI],Vars,Check,Constr):-
	(	invoke_constraint(Check,LI)
	->	maplist({Vars}/[I,X]>>nth1(I,Vars,X),LI,LX),
		invoke_constraint(Constr,LX)
	;	true
	),
	assign_constraint_on_lines(TLI,Vars,Check,Constr).

write_vars(I2):-
	diagram(Diag),
	full_empty_vars(Diag,_,LXE),
	img(I),
	copy_img(I,I2),
	maplist(write_var(Diag,I2),LXE),
	show_img(I2).

write_var(Diag,Img,I):-
	var_polygon(Diag,I,Poly),
	var_value(I,N),
	write_text(Img,N,Poly).

all_different:-
	vals(LX),
	all_distinct(LX).

minimize_max:-
	vals(LX),
	maplist(fd_inf,LX,LInf),
	maplist(fd_sup,LX,LSup),
	min_list(LInf,Inf),
	max_list(LSup,Sup),
	Max in Inf..Sup,
	maplist(#>=(Max),LX),
	once(labeling([min(Max)],[Max|LX])).

maximize_min:-
	vals(LX),
	maplist(fd_inf,LX,LInf),
	maplist(fd_sup,LX,LSup),
	min_list(LInf,Inf),
	max_list(LSup,Sup),
	Min in Inf..Sup,
	maplist(#=<(Min),LX),
	once(labeling([max(Min)],[Min|LX])).

demo:-
	%load_img('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2016A01.png', I),
	%load_img('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2014A06.png', I),
	%load_img('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2015A07.png', I),
	load_img('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2017A01.png', I),
	%load_img('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2017A05.png', I),
	%load_img('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2008A08.png', I),
	%load_img('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2009A04.png', I),
	set_img(I),							%%%only for debug
	%find_polygons(I,LP),
	%writeln(LP),nl,
	read_diagram(I,diag(LB,LL)),
	maplist(box_polygon,LB,LPoly),
	maplist(box_shape,LB,LS),
	maplist(box_color,LB,LC),
	maplist(box_value,LB,LT),
	show(LPoly),
	maplist([A,B,C]>>writeln(var(A,B,C)),LS,LC,LT),nl,
	maplist(writeln,LL),nl,
	%read_text(I,LTB),
	%writeln(LTB),nl,
	%release_img(I).
	true.
