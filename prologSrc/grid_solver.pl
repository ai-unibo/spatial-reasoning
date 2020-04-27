:-module(grid,[solve/3]).
:-use_module('debug.pl').
:-use_module('geometry.pl').
:-use_module('image.pl').
:-use_module('shapes.pl').
:-use_module('solver.pl').
:-use_module(library(aggregate)).
:-use_module(library(apply)).
:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(yall)).

read_puzzle(I,IGrid,Diag,LPieces,UX,UY):-
    find_polygons(I,[all,border(1),threshold(dark)],LPA),
    externals(LPA,LPAExt),
	show(LPAExt),						%%%only for debug
	maplist(area,LPAExt,LA),
	max_list(LA,AMax),
	nth1(IMax,LA,AMax),
	nth1(IMax,LPAExt,GridPoly),
	subimg(I,GridPoly,IGrid,_),
	read_grid(IGrid,Diag,UX,UY),
	find_polygons(I,[border(1),threshold(dark)],LPoly),
	exclude(contains(GridPoly),LPoly,LPPieces),
	show(LPPieces),						%%%only for debug
	maplist(poly_to_piece(UX,UY),LPPieces,LPiecesDirt),
	Diag=grid_diag(_,_,Grid),
	dims(Grid,NR,NC),
	include({NR,NC}/[piece(_,Grid)]>>(dims(Grid,NRP,NCP),NRP=<NR,NCP=<NC),LPiecesDirt,LPieces),
	!.

poly_to_piece(UX,UY,Poly,piece(Poly,Grid)):-
	rect_bounds(Poly,XMin,YMin,XMax,YMax),
	NC is round((XMax-XMin)/UX),
	NR is round((YMax-YMin)/UY),
	dims(Grid,NR,NC),
	findall((I,J,p(X,Y)),(between(1,NR,I),between(1,NC,J),X is XMin+UX*(J-0.7),Y is YMin+UY*(I-0.7)),LC),
	%maplist([(I,J,P),P]>>true,LC,LPo),	%%%only for debug
	%show([Poly|LPo]),					%%%only for debug
	partition({Poly}/[(I,J,P)]>>contains(Poly,P),LC,LIn,LOut),
	maplist({Grid}/[(I,J,P)]>>nth11(I,J,Grid,0),LIn),
	maplist({Grid}/[(I,J,P)]>>nth11(I,J,Grid,-1),LOut).

read_grid(I,grid_diag(SX,SY,Grid),UX,UY):-
	between(2,4,B),
	find_polygons(I,[border(B),threshold(light)],LP),
	length(LP,NPoly),
	include([poly(N,_)]>>(N=<4),LP,LSqTr),
	length(LSqTr,NMatch),
	NMatch/NPoly>0.9,
	partition([poly(N,_)]>>(N=4),LSqTr,LSq,LTr),
	%externals(LSq,LSqExt),
	maplist(min_coords,LSq,LTLVSq,LXSq,LYSq),
	show(LTLVSq),						%%%only for debug
	%tolerance(T),
	set_tolerance(4),
	list_to_set_equals(LXSq,SXSqUnord),
	list_to_set_equals(LYSq,SYSqUnord),
	sort(SXSqUnord,SXSq),
	sort(SYSqUnord,SYSq),
	length(SXSq,NCSq),
	length(SYSq,NRSq),
	only_on_debug((nl,writeln(sq-mat(NRSq,NCSq)))),	%%%only for debug
	append([XSqMin|_],[XSqMax],SXSq),
	append([YSqMin|_],[YSqMax],SYSq),
	UX is round((XSqMax-XSqMin)/(NCSq-1)),
	UY is round((YSqMax-YSqMin)/(NRSq-1)),
	U2 is min(UX,UY)//2-1,
	set_tolerance(U2),
	only_on_debug(writeln(edge(UX,UY))),	%%%only for debug
	(	LTr=[]
	->	SX=SXSq,
		SY=SYSq
	;	maplist(min_coords,LTr,_/*LTLVTr*/,LXTr,LYTr),
		min_list(LXTr,XTrMin),
		min_list(LYTr,YTrMin),
		max_list(LXTr,XTrMax),
		max_list(LYTr,YTrMax),
		min_unit(XTrMin,UX,SXSq,SX1),
		min_unit(YTrMin,UY,SYSq,SY1),
		max_unit(XTrMax,UX,SX1,SX),
		max_unit(YTrMax,UY,SY1,SY)
	),
	length(SX,NC),
	length(SY,NR),
	only_on_debug(writeln(mat(NR,NC))),	%%%only for debug
	dims(Grid,NR,NC),
	%from here we won't consider any more half squares!!!
	maplist({SX,SY,Grid}/[P]>>point_to_cell(P,grid_diag(SX,SY,Grid),0),LTLVSq),
	fill(Grid,-1),
	!.

geometry:equals(A,B):-
	number(A),
	number(B),
	!,
	tolerance(E),
	abs(A-B)=<E.

min_coords(poly(_,LV),p(XMin,YMin),XMin,YMin):-
	maplist(coords,LV,LX,LY),
	min_list(LX,XMin),
	min_list(LY,YMin).

min_unit(Min,_,[MinSq|TSSq],[MinSq|TSSq]):-
	MinSq=<Min,
	!.
min_unit(Min,U,[MinSq|TSSq],S):-
	MinSq1 is MinSq-U,
	min_unit(Min,U,[MinSq1,MinSq|TSSq],S).

max_unit(Max,U,SSq,S):-
	last(SSq,MaxSq),
	max_unit_(Max,U,SSq,MaxSq,S).
max_unit_(Max,U,SSq,MaxSq,SSq):-
	MaxSq>=Max-U/2,
	!.
max_unit_(Max,U,SSq,MaxSq,S):-
	MaxSq1 is MaxSq+U,
	append(SSq,[MaxSq1],SSq1),
	max_unit_(Max,U,SSq1,MaxSq1,S).

dims(Grid,I,J):-
	length(Grid,I),
	maplist({J}/[R]>>length(R,J),Grid).

nth11(I,J,Grid,E):-
	nth1(I,Grid,R),
	nth1(J,R,E).

point_to_cell(p(X,Y),grid_diag(SX,SY,Grid),E):-
	member_equals(X,SX,XCl),
	member_equals(Y,SY,YCl),
	nth1(J,SX,XCl),
	nth1(I,SY,YCl),
	nth11(I,J,Grid,E).

fill([],_).
fill([R|TGrid],E):-
	fill_row(R,E),
	fill(TGrid,E).
fill_row([],_).
fill_row([E|TR],E):-!,
	fill_row(TR,E).
fill_row([_|TR],E):-
	fill_row(TR,E).

copy_not0([],[]).
copy_not0([R0|TGrid0],[RV|TGridV]):-
	copy_not0_row(R0,RV),
	copy_not0(TGrid0,TGridV).
copy_not0_row([],[]).
copy_not0_row([0|TR0],[_|TRV]):-!,
	copy_not0_row(TR0,TRV).
copy_not0_row([E|TR0],[E|TRV]):-
	copy_not0_row(TR0,TRV).

%%%piece(Poly,Grid)
%%%pc(Grid,I,Area)
%%%pc_ins(Grid,I,Orig,R,Rif)
fill_puzzle(grid_diag(SX,SY,Grid),UX,UY,LPieces,Opt,grid_diag(SX,SY,GridFilled),LPiecesTransf):-
	pc_list(LPieces,LPc),
	insert_all(Grid,LPc,Opt,GridFilled,LPcIns),
	%transform_pieces(SX,SY,LPieces,LPcIns,LPiecesTransf).
	maplist(pc_ins_to_piece(SX,SY,UX,UY),LPcIns,LPiecesTransf).

pc_list(LP,LPc):-
	pc_list(LP,1,LPc).
pc_list([],_,[]):-!.
pc_list([piece(_,Grid)|TP],I,[pc(Grid,I,A)|TPc]):-
	grid_area(Grid,A),
	I1 is I+1,
	pc_list(TP,I1,TPc).

grid_area(Grid,A):-
	aggregate_all(count,(member(R,Grid),member(0,R)),A).

pc_index(pc(_,I,_),I):-!.
pc_index(pc_ins(_,I,_,_,_),I).

pc_area(pc(_,_,A),A).

insert_all(Grid,[Pc],Opt,GridFilled,[PcIns]):-!,
	insert(Grid,Pc,Opt,GridFilled,PcIns).
insert_all(Grid,LPc,Opt,GridFilled,[PcIns|TPcIns]):-
    (	member(equals,Opt)
	->	LPc=[Pc|TPc],
		insert(Grid,Pc,Opt,Grid1,PcIns)
	;	insert_one(Grid,LPc,Opt,Grid1,PcIns),
		pc_index(PcIns,I),
		select(pc(_,I,_),LPc,TPc)
	),
	insertable(Grid1,TPc),	
	insert_all(Grid1,TPc,Opt,GridFilled,TPcIns).

insert_one(Grid,[Pc|_],Opt,Grid1,PcIns):-
	insert(Grid,Pc,Opt,Grid1,PcIns).
insert_one(Grid,[_|TPc],Opt,Grid1,PcIns):-
	insert_one(Grid,TPc,Opt,Grid1,PcIns).

insert(Grid,Pc,Opt,GridFilled,pc_ins(GridP,Ind,c(DI,DJ),R,Rif)):-
	copy_not0(Grid,GridFilled),
	possible_orientation(Pc,Opt,pc_ins(GridP,Ind,_,R,Rif)),
	first_free(Grid,IG,JG),
	first_free(GridP,IP,JP),
	dims(Grid,NRG,NCG),
	dims(GridP,NRP,NCP),
	DI is IG-IP,
	DJ is JG-JP,
	DI>=0,
	DJ>=0,
	NRP+DI=<NRG,
	NCP+DJ=<NCG,
	draw_quads(GridP,DI,DJ),		%%only for debug
	findall(c(I,J),(nth11(I0,J0,GridP,0),I is I0+DI,J is J0+DJ),LC),
	maplist({GridFilled,Ind}/[c(I,J)]>>(nth11(I,J,GridFilled,Ind)),LC),
	fill(GridFilled,0).

first_free(Grid,I,J):-
	nth11(I,J,Grid,0),
	!.

possible_orientation(pc(Grid,I,_),_,pc_ins(Grid,I,_,0,0)).
possible_orientation(pc(Grid,I,_),Opt,pc_ins(GridT,I,_,0,1)):-
	(member(overturn,Opt);member(reflect,Opt)),
	reflect(Grid,GridT).
possible_orientation(pc(Grid,I,_),Opt,pc_ins(GridT,I,_,N,Ref)):-
	select(rotate,Opt,OptNoR),
	possible_orientation(pc(Grid,I,_),OptNoR,pc_ins(GridT1,I,_,0,Ref)),
	rotate(GridT1,Grid1),
	(	N=1,
		GridT=Grid1
	;	rotate(Grid1,Grid2),
		(	N=2,
			GridT=Grid2
		;	rotate(Grid2,GridT),
			N=3
		)
	).

insertable(Grid,LPc):-	%check if next space is a too small hole
	maplist(pc_area,LPc,LA),
	min_list(LA,AMin),
	once(find_shape(Grid,AMin,_)).

find_shape(Grid,A,LCA):-
	first_free(Grid,I,J),
	findall(c(II,JJ),(nth1(II,Grid,R),nth1(JJ,R,0)),LC),
	select(c(I,J),LC,LCO),
	find_at_least_conns([c(I,J)],LCO,A,LCA).

find_at_least_conns(LCC,_,Min,LCC):-
	length(LCC,L),
	L>=Min,
	!.
find_at_least_conns(LCC,LCO,Min,LCA):-
	member(CC,LCC),
	member(CO,LCO),
	adj(CC,CO),
	select(CO,LCO,TCO),
	find_at_least_conns([CO|LCC],TCO,Min,LCA).

adj(c(I1,J),c(I2,J)):-
	succ(I1,I2).
adj(c(I1,J),c(I2,J)):-
	succ(I2,I1).
adj(c(I,J1),c(I,J2)):-
	succ(J1,J2).
adj(c(I,J1),c(I,J2)):-
	succ(J2,J1).

transform_pieces(SX,SY,LP,LPcIns,LPT):-
	transform_pieces(SX,SY,LP,LPcIns,1,LPT).
transform_pieces(_,_,[],_,_,[]):-!.
transform_pieces(SX,SY,[piece(Poly,_)|TP],LPcIns,Ind,[piece(PolyT,GridT)|TPT]):-
	member(pc_ins(GridT,Ind,c(I,J),R,Rif),LPcIns),
	rect_bounds(Poly,XMin,YMin,XMax,_),
	translate(p(-XMin,-YMin),Poly,Poly0),
	(	Rif=:=0
	->	Poly0Rif=Poly0
	;	XM is (XMax-XMin)/2,
		symmetry_x(XM,Poly0,Poly0Rif)
	),
	rotate_poly(Poly0Rif,R,Poly0T),
	nth0(J,SX,XT),
	nth0(I,SY,YT),
	translate(p(XT,YT),Poly0T,PolyT),
	Ind1 is Ind+1,
	transform_pieces(SX,SY,TP,LPcIns,Ind1,TPT).

rotate_poly(Poly,0,Poly):-!.
rotate_poly(Poly,N,PolyR):-
	symmetry_diag(Poly,PolyT),
	vertices(PolyT,LV),
	maplist(x_coord,LV,LX),
	min_list(LX,XMin),
	max_list(LX,XMax),
	XM is (XMax-XMin)/2,
	symmetry_x(XM,PolyT,PolyR1),
	N1 is N-1,
	rotate_poly(PolyR1,N1,PolyR).

reflect(Grid,GridR):-
	maplist(reverse,Grid,GridR).

/*transpose(Grid,GridT):-	%already implemented in library(clpfd)
	dims(Grid,NR,NC),
	dims(GridT,NC,NR),
	findall(c(I,J),(between(1,NR,I),between(1,NC,J)),LC),
	maplist({Grid,GridT}/[c(I,J)]>>(nth11(I,J,Grid,E),nth11(J,I,GridT,E)),LC).*/

rotate(Grid,GridR):-
	transpose(Grid,GridT),
	reflect(GridT,GridR).

divide_in_equal_parts(grid_diag(SX,SY,Grid),UX,UY,N,Opt,grid_diag(SX,SY,GridFilled),LPieces):-
	grid_area(Grid,AG),
	divmod(AG,N,A,0),
	find_piece(Grid,A,GridP),
	once(insert(Grid,pc(GridP,1,A),Opt,Grid1,PcIns)),
	numlist(2,N,LI),
	maplist({GridP,A}/[I,pc(GridP,I,A)]>>true,LI,LPc),
	insert_all(Grid1,LPc,[equals|Opt],GridFilled,LPcIns),
	maplist(pc_ins_to_piece(SX,SY,UX,UY),[PcIns|LPcIns],LPieces).

find_piece(Grid,A,GridP):-
	find_shape(Grid,A,LC),
	aggregate_all(r(min(I),min(J),max(I),max(J)),member(c(I,J),LC),r(IMin,JMin,IMax,JMax)),
	NR is IMax-IMin+1,
	NC is JMax-JMin+1,
	dims(GridP,NR,NC),
	maplist({GridP,IMin,JMin}/[c(I,J)]>>(I1 is I-IMin+1,J1 is J-JMin+1,nth11(I1,J1,GridP,0)),LC),
	fill(GridP,-1).
	
pc_ins_to_piece(SXA,SYA,UX,UY,pc_ins(Grid,_,c(DI,DJ),_,_),piece(Poly,Grid)):-
	append(SXI,SX,SXA),
	append(SYI,SY,SYA),
	length(SXI,DJ),
	length(SYI,DI),
	findall(p(X,Y),(member(X,SX),member(Y,SY),point_to_cell(p(X,Y),grid_diag(SX,SY,Grid),0)),LP),
	maplist(point_to_quad(UX,UY),LP,LQ),
	maplist(edges,LQ,LLSeg),
	append(LLSeg,LSegDup),
	shapes:clean_seg(LSegDup,LSegDupCleaned),
	remove_double_segments(LSegDupCleaned,LSegPer),
	sort_edges(LSegPer,LSegSort),
	edges(Poly,LSegSort).

point_to_quad(UX,UY,p(X,Y),poly(4,[p(X,Y),p(X,Y1),p(X1,Y1),p(X1,Y)])):-
	X1 is X+UX-1,
	Y1 is Y+UY-1.

remove_double_segments([Seg],[Seg]):-!.
remove_double_segments([Seg|TDup],LPer):-
	member_equals(Seg,TDup,SegDup),
	!,
	select(SegDup,TDup,L),
	remove_double_segments(L,LPer).
remove_double_segments([Seg|TDup],[Seg|TPer]):-
	remove_double_segments(TDup,TPer).

sort_edges([S1,S2],[S1,S2]):-!.
sort_edges([seg(P1,P2)|TSeg],[seg(P1,P2),seg(P2,P3)|TSegSorted]):-
	select(seg(P2,P3),TSeg,L),
	sort_edges([seg(P2,P3)|L],[seg(P2,P3)|TSegSorted]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solver:solve(puzzle(Opt),Img,ImgRes):-
	is_list(Opt),
	read_puzzle(Img,Img2,grid_diag(SX,SY,Grid),LP,UX,UY),
	only_on_debug((set_img(Img2),b_setval(diag,r(SX,SY,UX,UY)))),	%%%only for debug
	fill_puzzle(grid_diag(SX,SY,Grid),UX,UY,LP,Opt,_,LPT),
	write_pieces(Img2,LPT,ImgRes).
solver:solve(divide_same_shape(N,Opt),Img,ImgRes):-
	read_grid(Img,grid_diag(SX,SY,Grid),UX,UY),
	only_on_debug(b_setval(diag,r(SX,SY,UX,UY))),	%%%only for debug
	divide_in_equal_parts(grid_diag(SX,SY,Grid),UX,UY,N,Opt,_,LPieces),
	write_pieces(Img,LPieces,ImgRes).

write_pieces(Img,LPieces,Img2):-
	maplist([piece(Poly,_),TPoly]>>translate(p(-2,-2),Poly,TPoly),LPieces,LPoly),
	copy_img(Img,Img2),
	draw_objects(Img2,LPoly,red,2),
	show_img(Img2).

draw_quads(Grid):-
	draw_quads(Grid,0,0).
draw_quads(_,_,_):-
	\+debug_enabled,
	!.
draw_quads(Grid,DI,DJ):-				%%%only for debug
	b_getval(diag,r(SXA,SYA,UX,UY)),
	append(SXI,SX,SXA),
	append(SYI,SY,SYA),
	length(SXI,DJ),
	length(SYI,DI),
	findall(p(X,Y),(member(X,SX),member(Y,SY),point_to_cell(p(X,Y),grid_diag(SX,SY,Grid),0)),LP),
	maplist(point_to_quad(UX,UY),LP,LQ),
	grid_area(Grid,A),
	writeln(area(A)),
	show(LQ),!.

demo(F):-
	load_img(F,I),
	set_img(I),							%%%only for debug
	solve(puzzle([rotate]),I,_),
	%solve(divide_same_shape(2,[rotate]),I),
	/*read_puzzle(I,I2,grid_diag(SX,SY,Grid),LP,UX,UY),
	set_img(I2),						%%%only for debug
	%maplist({SX,SY,UX,UY}/[piece(_,Grid)]>>draw_quads(grid_diag(SX,SY,Grid),UX,UY),LP),

	%read_grid(I,Diag,UX,UY),
	%draw_quads(Diag,UX,UY),
	b_setval(diag,r(SX,SY,UX,UY)),
	fill_puzzle(grid_diag(SX,SY,Grid),LP,[rotate],GridFilled,LPT),
	maplist([piece(Poly,_),Poly]>>true,LPT,LPoly),
	show(LPoly),
	%maplist({SX,SY,UX,UY}/[piece(_,Grid)]>>draw_quads(Grid),LPT),*/
	%release_img(I).
	true.
demo:-
	/*demo('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2004A05.png'),
	sleep(5),
	demo('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2006A04.png'),
	sleep(5),
	demo('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2007A08.png'),
	sleep(5),
	demo('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2008A06.png'),
	sleep(5),
	demo('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2009A06.png'),
	sleep(5),*/
	%%%demo('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2011A04.png'),	%puzzle con pezzi bianchi storti
	%%%sleep(5),
	demo('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2014A05.png'),
	%sleep(5),
	%demo('C:/Users/Riccardo/Dropbox/tesi/tabelle/immagini/2017A05.png'),
	%sleep(5),
	true.
