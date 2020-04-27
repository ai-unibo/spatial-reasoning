:-module(image,[draw_objects/2,draw_objects/3,draw_objects/4,cancel_objects/2,set_img/1,release_all_img/0,img/1]).
:-reexport('primitives.pl',[load_img/2,write_img/2,release_img/1,subimg/4,copy_img/2]).
:-use_module('color.pl').
:-use_module('primitives.pl').
:-dynamic(img/1).

draw_objects(I,L):-
	draw_objects_(I,L,rgb(0,0,0),1).

draw_objects(I,L,C):-
	draw_objects(I,L,C,1).

draw_objects(I,L,rgb(R,G,B),S):-
	!,
	draw_objects_(I,L,rgb(R,G,B),S).
draw_objects(I,L,C,S):-
	color_rgb(C,RGB),
	draw_objects_(I,L,RGB,S).

cancel_objects(I,L):-
	draw_objects_(I,L,rgb(255,255,255),3).

set_img(I):-
	release_all_img,
	asserta(img(I)).

release_all_img:-
	retract(img(I)),
	release_img(I),
	release_all_img,
	!.
release_all_img.
