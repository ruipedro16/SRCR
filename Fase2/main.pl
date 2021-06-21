% Sistemas de Representacao de Conhecimento e Raciocinio - Exercicio 2

% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).
:- set_prolog_flag(unknown, fail).
:- set_prolog_flag(answer_write_options, [max_depth(0)]).

:- use_module(library(lists)).
:- use_module(knowledge).
:- ensure_loaded(invariantes).
:- ensure_loaded(evolucao_involucao).
:- ensure_loaded(predicados_auxiliares).

%------------------------------------------------------------------%
%->                 PRESSUPOSTO DO MUNDO FECHADO                 <-%
%------------------------------------------------------------------%

% Pressuposto do mundo fechado para o predicado utente
-utente(ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS) :-
    nao(utente(ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS)),
    nao(excecao(utente(ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS))).

% Pressuposto do mundo fechado para o predicado centro_saude
-centro_saude(ID, NOME, M, TLF, EMAIL) :-
    nao(centro_saude(ID, NOME, M, TLF, EMAIL)),
    nao(excecao(centro_saude(ID, NOME, M, TLF, EMAIL))).

% Pressuposto do mundo fechado para o predicado staff
-staff(IDS, IDCENTRO, NOME, EMAIL) :-
    nao(staff(IDS, IDCENTRO, NOME, EMAIL)),
    nao(excecao(staff(IDS, IDCENTRO, NOME, EMAIL))).

%------------------------------------------------------------------%
%->               DEFINICAO DE FASES DE VACINACAO                <-%
%->           1a fase => Utentes com doencas cronicas            <-%
%->           2a fase => Utentes com mais de 65 anos             <-%
%->           3a fase => Restantes utentes                       <-%
%------------------------------------------------------------------%

% Extensao do predicado fases_vacinacao: R, F -> {V, F}
fases_vacinacao(R, 1) :-
    solucoes((NOME, ID),
             utente(ID, _, NOME, _, _, _, _, _, [_|_], _), UT),
    sort(UT, R).

fases_vacinacao(R, 2) :-
    solucoes((NOME, ID), (utente(ID, _, NOME, DATA, _, _, _, _, _, _),
                          idade(DATA, I), I >= 65), V),
    fases_vacinacao(F1, 1),
    subtract(V, F1, S),
    sort(S, R).

fases_vacinacao(R, 3) :-
    solucoes((NOME, ID), utente(ID, _, NOME, _, _, _, _, _, _, _), UT),
    fases_vacinacao(F1, 1), fases_vacinacao(F2, 2),
    append(F1, F2, F),
    subtract(UT, F, L),
    sort(L, R).

%------------------------------------------------------------------%
%->              IDENTIFICAR PESSOAS NAO VACINADAS               <-%
%------------------------------------------------------------------%

% Extensao do predicado nao_vacinados: R -> {V, F}
nao_vacinados(R) :-
    solucoes((NOME, ID), utente(ID, _, NOME, _, _, _, _, _, _, _), UT),
    solucoes((NOME, ID), (utente(ID, _, NOME, _, _, _, _, _, _, _),
                          vacinacao_Covid(_, ID, _, _, 2)), VAC),
    subtract(UT, VAC, L),
    sort(L, R).

%------------------------------------------------------------------%
%->                 IDENTIFICAR PESSOAS VACINADAS                <-%
%------------------------------------------------------------------%

% Extensao do predicado vacinados: R -> {V, F}
vacinados(R) :-
    solucoes((NOME, ID), (utente(ID, _, NOME, _, _, _, _, _, _, _),
                          vacinacao_Covid(_, ID, _, _, 2)), L),
    sort(L, R).

%------------------------------------------------------------------%
%->          IDENTIFICAR PESSOAS VACINADAS INDEVIDAMENTE         <-%
%------------------------------------------------------------------%

% Extensao do predicado vacinados_indevidamente: R -> {V, F}
vacinados_indevidamente(R) :-
    fase_atual(F),
    solucoes((NOME, ID), (utente(ID, _, NOME, _, _, _, _, _, _, _),
                          utente_fase(ID, FU), FU > F), S),
    sort(S, R).

%------------------------------------------------------------------%
%->   IDENTIFICAR PESSOAS NAO VACINADAS CANDIDATAS A VACINACAO   <-%
%------------------------------------------------------------------%

% Extensao do predicado candidatos: R -> {V, F}
candidatos(R) :- nao_vacinados(NV),
                 fase_atual(F),
                 solucoes((NOME, ID),
                          (utente(ID, _, NOME, _, _, _, _, _, _, _),
                           utente_fase(ID, FU), FU == F), S),
                 intersection(NV, S, L),
                 sort(L, R).

%------------------------------------------------------------------%
%->   IDENTIFICAR PESSOAS A QUEM FALTA A SEGUNTA TOMA DA VACINA  <-%
%------------------------------------------------------------------%

% Extensao do predicado falta_segunda: R -> {V, F}
falta_segunda(R) :-
    solucoes((NOME, ID), (utente(ID, _, NOME, _, _, _, _, _, _, _),
                          vacinacao_Covid(_, ID, _, _, 1)), V1),
    solucoes((NOME, ID), (utente(ID, _, NOME, _, _, _, _, _, _, _),
                          vacinacao_Covid(_, ID, _, _, 2)), VAC),
    subtract(V1, VAC, L),
    sort(L, R).

%------------------------------------------------------------------%
%->                       PREDICADOS EXTRA                       <-%
%------------------------------------------------------------------%

% Extensao do predicado sem_doencas_cronicas: R -> {V, F}
sem_doencas_cronicas(R) :-
    solucoes((NOME, ID), utente(ID, _, NOME, _, _, _, _, _, [], _), UT),
    sort(UT, R).

% Extensao do predicado utentes: R -> {V, F}
utentes(R) :-
    solucoes((NOME, ID), utente(ID, _, NOME, _, _, _, _, _, _, _), S),
    sort(S, R).

% Extensao do predicado staff: R -> {V, F}
staff(R) :-
    solucoes((NOME, ID), staff(ID, _, NOME, _), S),
    sort(S, R).

% Extensao do predicado centros_saude: R -> {V, F}
centros_saude(R) :-
    solucoes(NOME, centro_saude(_, NOME, _, _, _), S),
    sort(S, R).

% Extensao do predicado medicos: R -> {V, F}
medicos(R) :-
    solucoes((NOME, ID), medico(ID, _, NOME, _, _), S),
    sort(S, R).

% Extensao do predicado medicos_especialidade: ESP, R -> {V, F}
medicos_especialidade(ESP, R) :-
    solucoes((NOME, ID), medico(ID, _, NOME, _, ESP), L),
    sort(L, R).

% Extensao do predicado especialidades: IDC, R -> {V, F}
especialidades(IDC, R) :-
    solucoes(ESP, medico(_, IDC, _, _, ESP), L),
    list_to_set(L, S),
    sort(S, R).

% Extensao do predicado utentes_centro: IDC, R -> {V, F}
utentes_centro(IDC, R) :-
    solucoes((NOME, ID), utente(ID, _, NOME, _, _, _, _, _, _, IDC), L),
    sort(L, R).

% Extensao do predicado staff_centro: IDC, R -> {V, F}
staff_centro(IDC, R) :-
    solucoes((NOME, ID), staff(ID, IDC, NOME, _), L),
    sort(L, R).

% Extensao do predicado medicos_centro: IDC, R -> {V, F}
medicos_centro(IDC, R) :-
    solucoes((NOME, ID), medico(ID, IDC, NOME, _, _), L),
    sort(L, R).

%------------------------------------------------------------------%
%->                    SISTEMA DE INFERENCIA                     <-%
%------------------------------------------------------------------%

% Extensao do meta-predicado nao: Questao -> {V, F}
% Negacao fraca
nao(Questao) :- Questao, !, fail.
nao(_).

% Extensao do meta-predicado demo: Questao, Resposta -> {V,F}
demo(Questao, verdadeiro) :- Questao.
demo(Questao, falso) :- -Questao.
demo(Questao, desconhecido) :- nao(Questao), nao(-Questao).

% Extensao do predicado demoComp: Lista, R -> {V, F, D}
demoComp([Q], R) :- demo(Q,R).
demoComp([Q1, ou, Q2|T], R) :- demo(Q1, R1),
                               demoComp([Q2|T], R2),
                               disjuncao(R1, R2, R).
demoComp([Q1, e, Q2|T], R) :- demo(Q1, R1),
                              demoComp([Q2|T], R2),
                              conjuncao(R1,R2,R).

% Extensao do predicado conjuncao: X, Y, R -> {V, F, D}
conjuncao(verdadeiro, verdadeiro, verdadeiro).
conjuncao(verdadeiro, desconhecido, desconhecido).
conjuncao(desconhecido, verdadeiro, desconhecido).
conjuncao(desconhecido, desconhecido, desconhecido).
conjuncao(falso, _, falso).
conjuncao(_, falso, falso).

% Extensao do predicado disjuncao: X, Y, R -> {V, F, D}
disjuncao(verdadeiro, _, verdadeiro).
disjuncao(_, verdadeiro, verdadeiro).
disjuncao(falso, falso, falso).
disjuncao(falso, desconhecido, desconhecido).
disjuncao(desconhecido, falso, desconhecido).
disjuncao(desconhecido, desconhecido, desconhecido).
