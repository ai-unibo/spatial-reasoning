:-module(geometry,[tolerance/1,set_tolerance/1,point/1,x_coord/2,y_coord/2,coords/3,aligned/3,aligned_ord/3,linear_interpolation/3,linear_interpolation/4,segment/1,segment_length/2,midpoint/2,intersects/2,intersects/3,polygon/1,n_vertices/2,vertices/2,edges/2,polygon_type/2,check_polygon_type/2,equilateral/1,baricenter/2,contains/2,externals/2,rect_bounds/2,rect_bounds/5,equals/2,member_equals/3,is_set_equals/1,list_to_set_equals/2,translate/3,symmetry_x/3,symmetry_y/3,symmetry_diag/2]).
:-use_module(library(apply)).
:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-dynamic tolerance/1.
:-multifile equals/2.
:-discontiguous intersects/2.

tolerance(10).

set_tolerance(V):-
	number(V),
	V>=0,
	retract(tolerance(_)),
	assert(tolerance(V)).

%:-op(700, xfx, =#=).
eq(A,A):-
	var(A),
	!.
eq(A,B):-
	(	var(A)
	->	A is B
	;	var(B)
	->	B is A
	;	A=:=B
	).

point(p(_,_)).

points([]).
points([P|T]):-
	point(P),
	points(T).

x_coord(p(X,_),X).

y_coord(p(_,Y),Y).

coords(p(X,Y),X,Y).

equals(p(X1,Y1),p(X1,Y1),_):-!.
equals(p(X,Y1),p(X,Y2),E):-!,
	abs(Y1-Y2)=<E.
equals(p(X1,Y),p(X2,Y),E):-!,
	abs(X1-X2)=<E.
equals(p(X1,Y1),p(X2,Y2),E):-
	abs(X1-X2)=<E,
	abs(Y1-Y2)=<E.
equals(p(X1,Y1),p(X2,Y2)):-
	tolerance(E),
	equals(p(X1,Y1),p(X2,Y2),E).

aligned(P1,P2,P3):-
	tolerance(T),
	aligned(P1,P2,P3,T).
aligned_ex(P1,P2,P3):-
	aligned(P1,P2,P3,0.5).
aligned(p(X1,_),p(X2,_),p(X3,Y3),T):-	%to generate a point between P1 and P2, use instead linear_interpolation/3
	X1=:=X2,
	!,
	equals(p(X3,Y3),p(X1,Y3),T).
aligned(p(_,Y1),p(_,Y2),p(X3,Y3),T):-
	Y1=:=Y2,
	!,
	equals(p(X3,Y3),p(X3,Y1),T).
aligned(p(X1,Y1),p(X2,Y2),p(X3,Y3),T):-
	number(X3),
	M is (Y1-Y2)/(X1-X2),
	(	abs(M)=<1
	;	var(Y3)
	),
	!,
	Y3A is Y2+M*(X3-X2),
	equals(p(X3,Y3),p(X3,Y3A),T).
aligned(p(X1,Y1),p(X2,Y2),p(X3,Y3),T):-
	X3A is X2+(X2-X1)/(Y2-Y1)*(Y3-Y2),
	equals(p(X3,Y3),p(X3A,Y3),T).

aligned_ord(P1,P2,P3):-
	tolerance(T),
	aligned_ord(P1,P2,P3,T).
aligned_ord_ex(P1,P2,P3):-
	aligned_ord(P1,P2,P3,0.5).
aligned_ord(P1,P2,P3,T):-
	aligned(P1,P2,P3,T),
	ordered(P1,P2,P3),
	!.
aligned_ord(P1,P2,_,T):-
	equals(P1,P2,T).
aligned_ord(_,P2,P3,T):-
	equals(P3,P2,T).

ordered(p(X1,Y1),p(X2,Y2),p(X3,Y3)):-	%only if aligned(P1,P2,P3)
	(	abs(X1-X2)>abs(Y1-Y2)
	->	sign(X1-X2)=:=sign(X2-X3)
	;	sign(Y1-Y2)=:=sign(Y2-Y3)
	).

linear_interpolation(p(X1,Y1),p(X2,Y2),K,p(X3,Y3)):-
	X3i is X1+K*(X2-X1),
	Y3i is Y1+K*(Y2-Y1),
	equals(p(X3,Y3),p(X3i,Y3i),0.5).
linear_interpolation(P1,_,P1).
linear_interpolation(_,P2,P2).
linear_interpolation(p(X1,Y1),p(X2,Y2),p(XK,YK)):-
	var(XK),
	var(YK),
	!,
	LX is abs(X1-X2),
	LY is abs(Y1-Y2),
	LMax is max(LX,LY),
	N is round(LMax),
	N>0,
	between(1,N,I),
	XK is X1+I/N*(X2-X1),
	YK is Y1+I/N*(Y2-Y1).
linear_interpolation(P1,P2,PK):-
	aligned_ex(P1,P2,PK),
	ordered(P1,PK,P2).

segment(seg(P1,P2)):-
	point(P1),
	point(P2).

segment_length(seg(p(X,Y1),p(X,Y2)),L):-
	!,
	eq(L,abs(Y1-Y2)).
segment_length(seg(p(X1,Y),p(X2,Y)),L):-
	!,
	eq(L,abs(X1-X2)).
segment_length(seg(p(X1,Y1),p(X2,Y2)),L):-
	eq(L,sqrt((X1-X2)^2+(Y1-Y2)^2)).

distance(P1,P2,D):-
	segment_length(seg(P1,P2),D).

/*midpoint(seg(p(X1,Y1),p(X2,Y2)),p(XM,YM)):-
	(integer(XM);var(XM)),
	(integer(YM);var(YM)),
	(integer(X1);var(X1)),
	(integer(Y1);var(Y1)),
	(integer(X2);var(X2)),
	(integer(Y2);var(Y2)),
	X1-XM#=XM-X2,
	Y1-YM#=YM-Y2.*/
midpoint(seg(p(X1,Y1),p(X2,Y2)),p(XM,YM)):-
	eq(XM,(X1+X2)/2),
	eq(YM,(Y1+Y2)/2).

intersects(seg(P11,P12),seg(P21,P22),PX):-
	segment_length(seg(P11,P12),L1),
	segment_length(seg(P21,P22),L2),
	(	L1<L2
	->	linear_interpolation(P11,P12,PX),
		aligned_ord_ex(P21,PX,P22)
	;	linear_interpolation(P21,P22,PX),
		aligned_ord_ex(P11,PX,P12)
	),
	!.
intersects(S1,S2):-
	intersects(S1,S2,_).

equals(seg(P11,P12),seg(P21,P22)):-
	equals(P11,P21),
	equals(P12,P22).
equals(seg(P11,P12),seg(P21,P22)):-
	equals(P11,P22),
	equals(P12,P21).

polygon(poly(N,LV)):-
	length(LV,N),
	N>2,
	points(LV),
	%is_set_equals(LV).	%with too tolerance it may fail
	edges(poly(N,LV),E),
	maplist(segment_length,E,L),
	min_list(L,LMin),
	LMin>=0.5.

n_vertices(poly(N,_),N).

vertices(poly(_,LV),LV).

edges(poly(NV,[V1|TV]),[seg(LastV,V1)|LE]):-
	edges_l([V1|TV],LE,LastV),
	length([V1|TV],NV).	%in case seg->poly

edges_l([P1,P2],[seg(P1,P2)],P2):-!.
edges_l([P1,P2|TV],[seg(P1,P2)|TE],Last):-
	edges_l([P2|TV],TE,Last).

equals(poly(N,[P11|TP1]),poly(N,LP2)):-
	member_equals(P11,LP2,PR21),
	append(LP2Start,[PR21|LP2End],LP2),
	append(LP2End,LP2Start,LP2Reordered),
	same_polygon_p(TP1,LP2Reordered).

same_polygon_p([P1],[P2]):-
	!,
	equals(P1,P2).
same_polygon_p([P1|T1],[P2|T2]):-
	equals(P1,P2),
	same_polygon_p(T1,T2).

%polygon_type(Name,NVert)
polygon_type(circle,_).
polygon_type(triangle,3).
polygon_type(square,4).
polygon_type(rectangle,4).
polygon_type(pentagon,5).
polygon_type(hexagon,6).
polygon_type(heptagon,7).

polygon_costraints(circle,X,(baricenter(X,P),X=poly(N,LV),N>=8,maplist(distance(P),LV,LD),max_list(LD,LMax),min_list(LD,LMin),tolerance(E),LMax-LMin=<min(E,LMax/2))):-!.
polygon_costraints(square,X,equilateral(X)):-!.	%right angles constraint omitted
polygon_costraints(Name,_,true):-
	polygon_type(Name,_).

check_polygon_type(P,Name):-
	polygon(P),
	polygon_type(Name,NVert),
	polygon_costraints(Name,P,ConstraintsOnP),
	n_vertices(P,NVert),
	call(ConstraintsOnP).

equilateral(LE):-
	LE=[seg(_,_)|_],
	maplist(segment_length,LE,LL),
	min_list(LL,LMin),
	max_list(LL,LMax),
	tolerance(E),
	LMax-LMin=<E*2*sqrt(2).
equilateral(P):-
	P=poly(_,_),
	edges(P,LE),
	equilateral(LE).

baricenter(poly(N,LV),p(XM,YM)):-
	maplist(coords,LV,LX,LY),
	sum_list(LX,XTot),
	sum_list(LY,YTot),
	eq(XM,XTot/N),
	eq(YM,YTot/N).

rect_bounds(Poly,poly(4,[p(XMin,YMin),p(XMin,YMax),p(XMax,YMax),p(XMax,YMin)])):-	%circumscribed rectangle, vertex list starts from the top-left corner
	rect_bounds(Poly,XMin,YMin,XMax,YMax).
rect_bounds(poly(_,LV),XMin,YMin,XMax,YMax):-
	aggregate_all(r(min(X),min(Y),max(X),max(Y)),(member(p(X,Y),LV)),r(XMin,YMin,XMax,YMax)).
	/*maplist(coords,LV,LX,LY),
	min_list(LX,XMin),
	min_list(LY,YMin),
	max_list(LX,XMax),
	max_list(LY,YMax).*/

contains(poly(N,LV),p(X,Y)):-
	maplist(x_coord,LV,LX),
	max_list(LX,XMax),
	min_list(LX,XMin),
	XMax>X,
	XMin<X,
	edges(poly(N,LV),E),
	include(ray_casting_intersects(p(X,Y)),E,LI),
	%include(intersects(seg(p(X,Y),p(XMax,Y))),E,LI),
	length(LI,NI),
	NI mod 2=:=1.
	/*include(aligned_ps(p(X,Y)),E,LA),
	length(LA,NA),
	(NI-NA) mod 2=:=1.*/
contains(poly(N,LV),p(X,Y)):-
	edges(poly(N,LV),E),
	member(seg(P1,P2),E),
	aligned_ord_ex(P1,p(X,Y),P2),
	!.
contains(poly(N,LV),seg(P1,P2)):-		%problems with concave polygons
	contains(poly(N,LV),P1),
	contains(poly(N,LV),P2).
contains(poly(N1,LV1),poly(_,LV2)):-	%problems with concave polygons
	forall(member(P,LV2),contains(poly(N1,LV1),P)).

ray_casting_intersects(p(X,Y),seg(p(X1,Y1),p(X2,Y2))):-	%use only with horizontal lines
	max(X1,X2)>=X,
	max(Y1,Y2)>=Y,
	min(Y1,Y2)<Y,	%not equal to avoid parallel lines or aligned vertices, without excluding spikes
	%abs(Y-Y1)>0.5,
	(	min(X1,X2)>=X
	;	linear_interpolation(p(X1,Y1),p(X2,Y2),p(XK,Y)),
		XK>=X
	).

aligned_ps(P,seg(P1,P2)):-
	aligned_ex(P,P1,P2).

contains_any(Poly,[P|_]):-
	contains(Poly,P).
contains_any(Poly,[_|T]):-
	contains_any(Poly,T).

intersects(poly(N,LV),seg(P1,P2)):-
	contains_any(poly(N,LV),[P1,P2]).
intersects(seg(P1,P2),poly(N,LV)):-
	contains_any(poly(N,LV),[P1,P2]).
intersects(poly(N1,LV1),poly(_,LV2)):-
	contains_any(poly(N1,LV1),LV2).

externals(L,LExt):-
	externals(L,L,LExt).
externals(_,[],[]):-!.
externals(L,[E|T],TExt):-
	member(E1,L),
	E1\=E,
	contains(E1,E),
	!,
	externals(L,T,TExt).
externals(L,[E|T],[E|TExt]):-
	externals(L,T,TExt).
	
member_equals(Eq,[H|_],H):-
	equals(Eq,H).
member_equals(Eq,[_|T],E):-
	member_equals(Eq,T,E).

is_set_equals([]):-!.
is_set_equals([H|T]):-
	member_equals(H,T,_),
	!,fail.
is_set_equals([_|T]):-
	is_set_equals(T).

list_to_set_equals([],[]).
list_to_set_equals([E|L],S):-
	member_equals(E,L,_),
	!,
	list_to_set_equals(L,S).
list_to_set_equals([E|L],[E|S]):-
	list_to_set_equals(L,S).

translate(p(A,B),p(X,Y),p(XT,YT)):-
	XT #= X+A,
	YT #= Y+B.
translate(p(A,B),seg(P1,P2),seg(P1T,P2T)):-
	translate(p(A,B),P1,P1T),
	translate(p(A,B),P2,P2T).
translate(p(A,B),poly(N,LV),poly(N,LVT)):-
	maplist(translate(p(A,B)),LV,LVT).

symmetry_x(X0,p(X,Y),p(XT,Y)):-
	XT #= X2-X,
	(	var(X0)
	->	X0 is X2/2
	;	X2 is round(X0*2)
	).
symmetry_x(X,seg(P1,P2),seg(P1T,P2T)):-
	symmetry_x(X,P1,P1T),
	symmetry_x(X,P2,P2T).
symmetry_x(X,poly(N,LV),poly(N,LVT)):-
	maplist(symmetry_x(X),LV,LVT).

symmetry_y(Y0,p(X,Y),p(X,YT)):-
	YT #= Y2-Y,
	(	var(Y0)
	->	Y0 is Y2/2
	;	Y2 is round(Y0*2)
	).
symmetry_y(Y,seg(P1,P2),seg(P1T,P2T)):-
	symmetry_y(Y,P1,P1T),
	symmetry_y(Y,P2,P2T).
symmetry_y(Y,poly(N,LV),poly(N,LVT)):-
	maplist(symmetry_y(Y),LV,LVT).

symmetry_diag(p(X,Y),p(Y,X)).
symmetry_diag(seg(P1,P2),seg(P1T,P2T)):-
	symmetry_diag(P1,P1T),
	symmetry_diag(P2,P2T).
symmetry_diag(poly(N,LV),poly(N,LVT)):-
	maplist(symmetry_diag,LV,LVT).

