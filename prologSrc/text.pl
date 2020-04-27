:-module(text,[text_box/1,text_box/4,text/2,bounds/2,read_text/2,read_single_text/2,read_single_number/2]).
:-reexport('primitives.pl',[read_text/4,read_single_text/4,read_single_number/4,write_text/3]).
:-use_module('geometry.pl').
:-use_module('primitives.pl').
:-use_module(library(apply)).

text_box(txt(T,R,S)):-
	string(T),
	integer(S),
	check_polygon_type(R,rectangle).
text_box(txt(T,R,S),T,R,S).	%"constructor"

text(txt(T,_,_),T).

bounds(txt(_,R,_),R).

font_size(txt(_,_,S),S).

read_text(I,LTB):-
	read_text(I,LT,LR,LS),
	maplist(text_box,LTB,LT,LR,LS).
	
read_single_text(I,TB):-
	read_single_text(I,T,R,S),
	term_string(T,Txt),
	text_box(TB,Txt,R,S).

read_single_number(I,TB):-
	read_single_number(I,N,R,S),
	text_box(TB,N,R,S).
