:-module(color,[color/1,color_rgb/2,get_color/3,main_color/2]).
:-reexport('primitives.pl',[get_color_rgb/3,main_color_rgb/2]).
:-use_module(['primitives.pl']).
:-use_module(library(apply)).
:-use_module(library(lists)).

base_color(C):-
	base_color(C,_).
base_color(white,		rgb(255,255,255)).
base_color(black,		rgb(0,0,0)).
base_color(light_gray,	rgb(200,200,200)).
base_color(dark_gray,	rgb(100,100,100)).
base_color(red,			rgb(255,0,0)).
base_color(orange,		rgb(255,102,0)).
base_color(brown,		rgb(153,51,0)).
base_color(yellow,		rgb(255,255,0)).
base_color(light_green,	rgb(0,255,0)).
base_color(dark_green,	rgb(0,128,0)).
base_color(light_blue,	rgb(0,255,255)).
base_color(dark_blue,	rgb(0,0,255)).
base_color(purple,		rgb(255,0,255)).
base_color(violet,		rgb(128,0,128)).

color(C):-
	color(C,_).
color(gray,light_gray).
color(gray,dark_gray).
color(green,light_green).
color(green,dark_green).
%color(blue,light_blue).
%color(blue,dark_blue).
color(C,C):-
	base_color(C).

get_base_color(C,RGB):-
	base_color(C,RGB),
	!.
get_base_color(C,rgb(R,G,B)):-
	setof(base_color(X,XRGB),base_color(X,XRGB),LBC),
	maplist(color_distance(base_color(_,rgb(R,G,B))),LBC,LD),
	min_list(LD,DMin),
	nth0(I,LD,DMin),
	nth0(I,LBC,base_color(C,_)).

color_distance(base_color(_,rgb(R1,G1,B1)),base_color(_,rgb(R2,G2,B2)),D):-
	D is sqrt((R2-R1)^2+(G2-G1)^2+(B2-B1)^2).

color_rgb(N,RGB):-
	(	var(N)
	->	get_base_color(C,RGB),
		color(N,C)
	;	color(N,C),
		get_base_color(C,RGB)
	).

get_color(I,P,C):-
	get_color_rgb(I,P,RGB),
	color_rgb(C,RGB).

main_color(I,C):-
	main_color_rgb(I,RGB),
	color_rgb(C,RGB).
