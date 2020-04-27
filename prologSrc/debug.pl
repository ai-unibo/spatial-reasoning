:-module(debug,[debug_enabled/0,only_on_debug/1,only_on_web/1,show/1,show/2,show_and_wait/1,show_and_wait/2]).
:-reexport('primitives.pl',[show_img/1,show_objects/1,draw_rect/1,draw_grid/1,draw_grid_triangle/1,draw_circles/1]).
:-use_module('color.pl').
:-use_module('image.pl').
:-use_module('primitives.pl').
:-use_module(library(apply)).
:-use_module(library(lists)).
:-meta_predicate only_on_debug(0).

web_enabled:-
	current_prolog_flag(web,State),
	call(State).

only_on_web(Goal):-
		web_enabled
	->	call(Goal)
	;	true.


debug_enabled:-
	current_prolog_flag(debug,State),
	call(State).

only_on_debug(Goal):-
		debug_enabled
	->	call(Goal)
	;	true.

show(_,_):-
	\+debug_enabled,
	!.
show(I,L):-
	is_list(L),
	!,
	copy_img(I,I2),
	draw_objects_(I2,L,rgb(255,0,0),2),
	show_img(I2),
	release_img(I2).
show(I,P):-
	show(I,[P]).
show(_):-
	\+debug_enabled,
	!.
show(X):-
	img(I),
	show(I,X).

show_and_wait(_,_):-
	\+debug_enabled,
	!.
show_and_wait(I,P):-
	show(I,P),
	sleep(2).
show_and_wait(_):-
	\+debug_enabled,
	!.
show_and_wait(X):-
	img(I),
	show_and_wait(I,X).
