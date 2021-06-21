:- op(900, xfy, '::').

:- ensure_loaded(predicados_auxiliares).
:- ensure_loaded(main).

%------------------------------------------------------------------%
%-> Extensao do predicado que permite a evolucao do conhecimento <-%
%------------------------------------------------------------------%

% Extensao do meta-predicado insercao: T -> {V, F}
insercao(T) :- assert(T).
insercao(T) :- retract(T), !, fail.

% Extensao do meta-predicado teste: L -> {V, F}
teste([]).
teste([H|T]) :- H, teste(T).

% Extensao do meta-predicado evolucao: T -> {V, F}
% Conhecimento perfeito positivo
evolucao(T) :- solucoes(I, +T::I, L),
               insercao(T),
               teste(L).

% Extensao do meta-predicado evolucao: T -> {V, F}
% Conhecimento perfeito negativo
evolucao(-T) :- solucoes(I, +(-T)::I, L),
                insercao(-T),
                teste(L).

%-----------------------------------------------------------------%
%-> Extensao do predicado que permite a reducao do conhecimento <-%
%-----------------------------------------------------------------%

% Extensao do meta-predicado remocao: T -> {V, F}
remocao(T) :- retract(T).
remocao(T) :- assert(T), !, fail.

% Extensao do meta-predicado involucao: T -> {V, F}
% Conhecimento perfeito positivo
involucao(T) :- solucoes(I, -T::I, L),
                teste(L),
                remocao(T).

% Extensao do meta-predicado involucao: T -> {V, F}
% Conhecimento perfeito negativo
involucao(-T) :- solucoes(I, -(-T)::I, L),
                 teste(L),
                 remocao(-T).
