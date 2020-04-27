:-module(count,[solve/3,min_area/1,grid_area/2]).
:-use_module('debug.pl').
:-use_module('geometry.pl').
:-use_module('image.pl').
:-use_module('shapes.pl').
:-use_module('solver.pl').
:-use_module(library(apply)).
:-use_module(library(lists)).
:-use_module(library(yall)).

find_segments(I,LSeg):-
	find_polygons(I,[border(1)],LP),
	maplist(area,LP,LA),	%\
	min_list(LA,AMin),		% | for area filters
	b_setval(min_area,AMin),%/
	tolerance(T),
	set_tolerance(10),
	polygons_to_segments(LP,LSeg),
	set_tolerance(T).

count(LSeg,Figure,N):-
	find_all(LSeg,Figure,L),
	length(L,N).

find_all(LSeg,Figure,PFilt):-
	polygon_type(Figure,NVert),
	tolerance(T),
	set_tolerance(8),	%maybe we can repeat search with different tolerance levels
	%findall(X,(construct_polygon(LSeg,NVert,X),check_polygon_type(X,Figure),writeln(X)),PFiltDup),	%%%only for debug
	findall(X,(construct_polygon(LSeg,NVert,X),check_polygon_type(X,Figure)),PFiltDup),
	set_tolerance(T),
	maplist(front_face,PFiltDup,PFiltDup1),	%it should not be needed, but it is needed
	list_to_set_equals(PFiltDup1,PFilt).
	%nl,writeln('results:'),			%%%only for debug
	%maplist(writeln,PFilt),			%%%only for debug
	%maplist(show_and_wait,PFilt).		%%%only for debug

construct_polygon(LS,NVert,P):-
	NSeg1 is NVert-1,
	construct_polyline(LS,NSeg1,Poly1,Points1NoLast,PLast),
	%writeln([Points1NoLast,PLast]),	%%%only for debug
	add_segment(LS,Poly1,Points1NoLast,PLast,true,Poly,_),
	edges(PMaybeRev,Poly),
	front_face(PMaybeRev,P).

construct_polyline(LSeg,1,[seg(P1,P2)],[P1],P2):-
	!,
	member(seg(P1,P2),LSeg).
construct_polyline(LSeg,NSeg,Poly,PointsNoLast,PLast):-
	NSeg1 is NSeg-1,
	construct_polyline(LSeg,NSeg1,Poly1,Points1NoLast,PLast),
	%writeln([Points1NoLast,PLast]),	%%%only for debug
	add_segment(LSeg,Poly1,Points1NoLast,PLast,false,Poly,PointsNoLast).

add_segment(LSeg,[seg(P2,P3)|TPoly],TPointsNoLast,PLast,Close,Poly,PointsNoLast):-
	possible_edge(seg(P1,P2),LSeg),
	\+member(P1,TPointsNoLast),
	show([seg(P1,P2),seg(P2,P3)|TPoly]),	%%%only for debug
	add_segment_switch(LSeg,P1,P2,P3,TPoly,TPointsNoLast,PLast,Close,Poly,PointsNoLast).

add_segment_switch(LSeg,P1,P2,P3,TPoly,TPointsNoLast,PLast,Close,Poly,PointsNoLast):-
	%aligned_ord(P1,P2,P3),
	aligned(P1,P2,P3),
	!,
	P1\=PLast,
	add_segment(LSeg,[seg(P1,P3)|TPoly],[P1|TPointsNoLast],PLast,Close,Poly,PointsNoLast).
add_segment_switch(_,P1,P2,P3,TPoly,TPointsNoLast,PLast,Close,[seg(P1,P2),seg(P2,P3)|TPoly],[P1|TPointsNoLast]):-
	\+call(Close),
	!,
	P1\=PLast.
add_segment_switch(LSeg,P1,P2,P3,TPoly,TPointsNoLast,PLast,_,Poly,PointsNoLast):-
	aligned_ord(PLast,P1,P2),
	!,
	close_segment(LSeg,[seg(P1,P2),seg(P2,P3)|TPoly],TPointsNoLast,PLast,Poly,PointsNoLast),
	!.	%only 1 possible closing segment can exist (delete close_segment backtrack points)

close_segment(_,[seg(PLast,P2)|TPoly],_,PLast,[seg(PLast,P2)|TPoly],_):-
	!,
	show([seg(PLast,P2)|TPoly]),		%%%only for debug
	append(_,[seg(PN1,PLast)],TPoly),
	\+aligned(PN1,PLast,P2).
close_segment(LSeg,[seg(P2,P3)|TPoly],TPointsNoLast,PLast,Poly,PointsNoLast):-
	possible_edge(seg(P1,P2),LSeg),
	\+member(P1,TPointsNoLast),
	%aligned_ord(P1,P2,P3),
	aligned(P1,P2,P3),
	close_segment(LSeg,[seg(P1,P3)|TPoly],[P1|TPointsNoLast],PLast,Poly,PointsNoLast).

possible_edge(seg(P1,P2),L):-
	member(seg(P1,P2),L).
possible_edge(seg(P1,P2),L):-
	member(seg(P2,P1),L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solver:solve(count_polygons(Shape),Img,N):-
	find_segments(Img,LSeg),
	count(LSeg,Shape,N).
	%writeln(result(N)).
solver:solve(count_polygons(Shape,constr(X,Filter)),Img,N):-
	find_segments(Img,LSeg),
	find_all(LSeg,Shape,L),
	include([X]>>Filter,L,LF),
	length(LF,N).
	%writeln(result(N)).

min_area(A):-
	b_getval(min_area,A).

grid_area(P,N):-
	min_area(AMin),
	area(P,A),
	N is truncate(A/AMin). %round(A/AMin).

demo:-
	%draw_grid_triangle(I),
	load_img('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2005A04.png',I),
	release_all_img,					%%%only for debug
	assert(img(I)),						%%%only for debug
	find_segments(I,LSeg),
	count(LSeg,triangle,N),
	writeln(result(N)),
	release_img(I).
