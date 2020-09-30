:-module(solver,[main/1,mainweb/1,solve/3,invoke_constraint/1,invoke_constraint/2,invoke_constraint/3,invoke_constraint/4]).
:-use_module('debug.pl').
:-use_module('image.pl').
:-use_module(library(readutil)).
:-multifile(solve/3).

main([_,ConstrFile,ImgFile|TArg]):-!,
	(	primitives:check_installed
	->	true
	;	primitives:load_dll	%only in compiled version
	),
	load_img(ImgFile,I),
	read_file_to_terms(ConstrFile,[P],[]),
	set_img(I),                         %%%also for non-debug calls, to have all examples working
	
	write('P: '),writeln(P),
    
	solve(P,I,R),
	%writeln(R),
    %writeln(TArg),
	(	TArg=[OutFile|_],
        R=img(_)
	->	atom_concat(OutFile,'.png',OutImgFile),        
		write_img(OutImgFile,R)
	;	true
	),
	writeln(result(R)),
	%release_all_img,			        %%%only for debug, no if want to show backtracking
	%release_img(I),				    %no if want to show backtracking
	only_on_web(halt),
	true.
main(_):-
	writeln("Syntax error."),
	writeln("Arguments needed: <constraint_file> <image_file>"),
	halt.
	
mainweb(InFile, PlText, OutFile):-
    write('PlText: '),writeln(PlText),
    (	primitives:check_installed
	->	true
	;	primitives:load_dll	%only in compiled version
	),
	load_img(InFile,I),
	set_img(I),     
	atom_to_term(PlText,P,_),
	solve(P,I,R),
	(  R=img(_)
	->	write_img(OutFile,R)
	;	true
	),
	writeln(result(R)),
	only_on_web(halt),
	true.

mainweb(_):-
	writeln("Syntax error."),
	writeln("Arguments needed: <input_image_file> <prolog_text> <output_image_file>"),
	halt.

invoke_constraint(Constr):-
	copy_term(Constr,Constr1),
	Constr1=constr(F),
	invoke_(F).
invoke_constraint(Constr,A):-
	copy_term(Constr,Constr1),
	Constr1=constr(A,F),
	invoke_(F).
invoke_constraint(Constr,A,B):-
	copy_term(Constr,Constr1),
	Constr1=constr(A,B,F),
	invoke_(F).
invoke_constraint(Constr,A,B,C):-
	copy_term(Constr,Constr1),
	Constr1=constr(A,B,C,F),
	invoke_(F).

invoke_(X/Cond->Constr):-!,
	findall(X,Cond,LX),
	maplist([X]>>Constr,LX).

invoke_(Constr):-
	call(Constr).

demo(Game):-
    write('Solving: '),writeln(Game),
	atom_concat('texts/',Game,CF),
	atom_concat(CF,'.pl',ConstrFile),
	atom_concat('../images/',Game,IF),
	atom_concat(IF,'.png',ImgFile),
	atom_concat('../outimages/',Game,OutFile),
	main([_,ConstrFile,ImgFile,OutFile]).

demo(ImgFile, PlText, OutFile):-
    writeln('Solving... '),
    mainweb(ImgFile, PlText, OutFile).
	
