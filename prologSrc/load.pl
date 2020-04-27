:-use_module(library(apply)).
:-use_module(library(clpfd)).
:-use_module(library(yall)).

%create_prolog_flag(web,true,[access(read_write), type(boolean),keep(false)]).
%needs set_prolog_flag(web, true). to work with web version.


:-	directory_files('.',LF),
	include([F]>>atom_concat(_,'solver.pl',F),LF,LSolvers),
	maplist(writeln,LSolvers),
	maplist(use_module,LSolvers),
	writeln("Solvers loaded").

demo:-
%	solver:demo('2004A05'). 
%	solver:demo('2005A01'). 
%	solver:demo('2005A04'). 
	solver:demo('2006A03'). 
%	solver:demo('2006A04'). 
%	solver:demo('2006A07'). 
%	solver:demo('2006A08'). 
%	solver:demo('2007A02'). 
%   solver:demo('2007A08'). 
%   solver:demo('2008A06'). 
%	solver:demo('2012A01'). 
%	solver:demo('2012A02'). 
%	solver:demo('2013A01'). 
%   solver:demo('2013A08'). 
%	solver:demo('2014A05'). 
%	solver:demo('2017A01'). 
%	solver:demo('2017A05'). 

