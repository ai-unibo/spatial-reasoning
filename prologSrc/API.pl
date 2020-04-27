:-module(api,[load_image/2, print_poly_list/2, show_identified_poly/1, show_identified_poly/2, negative_color/2, all_polygons_types/2, list_baricenters/2, remove_similar_baricenters/3, equal_points/4, write_list/1, boh/1]).

load_image(ImgName, Img):-
 	Path='D:/OneDrive/Documenti/Tesi Magistrale/Previous developments/Riccardo Buscaroli2/immagini/imgProve/',
	Extension='.png',
	atom_concat(Path,ImgName,Temp),
	atom_concat(Temp,Extension,ImgPath),
	load_img(ImgPath, Img).

print_poly_list(_,[]):- 
	true.
print_poly_list(Img, [poly(N, List)|T]):-
	write('Poligono con '), write(N), write(' punti. Lista: '), write(List), nl,
	print_poly_list(Img,T).
print_poly_list(Img,[_|T]):-
	print_poly_list(Img,T).
	
show_identified_poly(I):-
	main_color_rgb(I, MainColor),
	negative_color(MainColor, NC), 
	show_identified_poly(I, NC).
show_identified_poly(I, C):-
	find_polygons(I, PL),
	draw_objects(I, PL, C, 1),
	show_img(I).
	
negative_color(rgb(RED,GREEN,BLUE), NC):-
	NRED is 255-RED,
	NGREEN is 255-GREEN,
	NBLUE is 255-BLUE,
	NC=rgb(NRED,NGREEN,NBLUE).
	
all_polygons_types([], []):-
	!.
all_polygons_types([P|T],L):-
	check_polygon_type(P, Type),
%	write('Polygon: '), write(P), write(', his type is: '), write(Type), nl,
	all_polygons_types(T,  L1),
	append(L1, [Type], L).
	
list_baricenters([], []):-true.
list_baricenters([P|T], List):-
	!,
	baricenter(P, B),
	check_polygon_type(P, Type),
	append([B], [Type], Concat),
%	write('Concat list: '), write(Concat), nl, 
	list_baricenters(T, List1),
	append(List1, [Concat], List).

remove_similar_baricenters([X|[]],L, _):-
	append([], [X], L).
	
remove_similar_baricenters([[P1,F1], [P2,F2]|T], ListBarReduced, Delta):- 
	nl,nl,write('P1: '),write(P1), write(', F1: '),write(F1),
	nl,write('P2: '),write(P2), write(', F2: '),write(F2),
	equal_points(P1,P2,Answer, Delta),
	nl,write('Equal points Answer: '), write(Answer),
	remove_similar_baricenters([[P2,F2]|T], ListBarReduced1, Delta),
	(
		Answer=true, F1 = F2
			->
		append(ListBarReduced1,[],ListBarReduced)
			;
		append(ListBarReduced1,[P1,F1],ListBarReduced)
	)
	
	.
	
% A=true means that the point are inside the tollerance range, so it will be considered as equal
equal_points(p(X1,Y1),p(X2,Y2),A, Delta):-
%	nl,nl,write('X1: '),write(X1), write(', Y1: '),write(Y1), nl,
%	nl,write('X2: '),write(X2), write(', Y2: '),write(Y2),
	Xabs is abs(X1-X2), Yabs is abs(Y1-Y2),
	(
		Xabs < Delta, Yabs < Delta
			->
		A=true
			;
		A=false
	)
	.

write_list([]).
write_list([H|T]):-
	write(H), nl,
	write_list(T).
	
boh(X):-
	nl,write('CE L HAI FATTA'),nl,
	X = 'GATTOOOO'.