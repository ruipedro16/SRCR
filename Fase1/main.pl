% Sistemas de Representacao de Conhecimento e Raciocinio - Exercicio 1

% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).
:- set_prolog_flag(unknown, fail).
:- set_prolog_flag(answer_write_options, [max_depth(0)]).

:- op(900, xfy, '::').

:- use_module(library(lists)).
:- use_module(knowledge).

%------------------------------------------------------------------%
%->                    PREDICADOS AUXILIARES                     <-%
%------------------------------------------------------------------%

% Extensao do predicado comprimento: S, N -> {V, F}
comprimento(S, N) :- length(S, N).

% Extensao do predicado year: date(Y, M , D), R -> {V, F}
year(date(Y, _, _), Y).

% Extensao do predicado month: date(Y, M , D), R -> {V, F}
month(date(_, M, _), M).

% Extensao do predicado day: date(Y, M , D), R -> {V, F}
day(date(_, _, D), D).

% Extensao do predicado years_between_dates: D1, D2, R -> {V, F}
years_between_dates(D1, D2, R):- year(D1, Y1), year(D2, Y2),
                                 month(D1, M1), month(D2, M2),
                                 M2 > M1, R is Y2 - Y1.

years_between_dates(D1, D2, R):- year(D1, Y1), year(D2,Y2),
                                 month(D1, M1), month(D2, M2),
                                 day(D1, DAY1), day(D2, DAY2),
                                 M2 == M1, DAY2 >= DAY1,
                                 R is Y2 - Y1.

years_between_dates(D1, D2, R):- year(D1, Y1), year(D2, Y2),
                                 R is Y2 - Y1-1.

% Extensao do predicado format_date: date(Y, M, D, H, Mn, S, Off, TZ, DST), date(Y, M, D) -> {V, F}
format_date(date(Y, M, D, _, _, _, _, _, _), date(Y, M, D)).

% Extensao do predicado current_date: Date -> {V, F}
current_date(Date) :- get_time(Stamp),
                      stamp_date_time(Stamp, DateTime, local),
                      format_date(DateTime, Date).

% Extensao do predicado idade: Data_Nasc, Idade -> {V, F}
idade(Data_Nasc, Idade) :- 
    current_date(Date), years_between_dates(Data_Nasc, Date, Idade).

% Extensao do predicado utente_fase: ID, F -> {V, F}
% Identifica a fase em que um utente deve ser vacinado
utente_fase(ID, 1) :- 
    solucoes(ID, utente(ID, _, _, _, _, _, _, _, [_|_], _), L),
    comprimento(L, N),
    N == 1, !.

utente_fase(ID, 2) :-
    solucoes(ID, (utente(ID, _, _, DATA, _, _, _, _, _, _),
                  idade(DATA, I), I >= 65), L),
    comprimento(L, N),
    N == 1, !.

utente_fase(_, 3).

% Extensao do predicado fase_atual: F -> {V, F}
% Identifica a fase atual de vacinacao
fase_atual(3) :- fases_vacinacao(F1, 1), fases_vacinacao(F2, 2),
                 concat(F1, F2, F),
                 vacinados(VAC),
                 subset(F, VAC).

fase_atual(2) :- fases_vacinacao(F1, 1),
                 vacinados(VAC),
                 subset(F1, VAC).

fase_atual(1).

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
evolucao(T) :- solucoes(I, +T::I, L),
               insercao(T),
               teste(L).

%-----------------------------------------------------------------%
%-> Extensao do predicado que permite a reducao do conhecimento <-%
%-----------------------------------------------------------------%

% Extensao do meta-predicado remocao: T -> {V, F}
remocao(T) :- retract(T).
remocao(T) :- assert(T), !, fail.

% Extensao do meta-predicado involucao: T -> {V, F}
involucao(T) :- solucoes(I, -T::I, L),
                teste(L),
                remocao(T).

%------------------------------------------------------------------%
%-> Extensao do predicado que permite a procura de conhecimento  <-%
%------------------------------------------------------------------%

% Extensao do meta-predicado solucoes: X, Y, Z -> {V, F}
solucoes(X, Y, Z) :- findall(X, Y, Z).

%---------------------------------------------------------------------%
%->                      INVARIANTES DO UTENTE                      <-%
%---------------------------------------------------------------------%

% O ID do utente deve ser um numero inteiro e deve ser unico
+utente(ID, _, _, _, _, _, _, _, _, _) :: (
    integer(ID),
    solucoes(ID, utente(ID, _, _, _, _, _, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% O utente tem de estar associado a um centro de saude que exista
+utente(_, _, _, _, _, _, _, _, _, CS) :: (
    solucoes(CS, centro_saude(CS, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% So pode existir um utente com um determinado numero de Seguranca Social
+utente(_, NUM, _, _, _, _, _, _, _, _) :: (
    solucoes(NUM, utente(_, NUM, _, _, _, _, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% Nao permite a remocao de um utente se existirem registos de vacinacao a si associados
-utente(ID, _, _, _, _, _, _, _, _, _) :: (
    solucoes(ID, vacinacao_Covid(_, ID, _, _, _), S),
    comprimento(S, N),
    N == 0
).

% Nao permite a remocao de um utente se existirem registos de consultas a si associaos
-utente(ID, _, _, _, _, _, _, _, _, _) :: (
    solucoes(ID, consulta(_, ID, _, _), S),
    comprimento(S, N),
    N == 0
).

% Nao permite a remocao de um utente se existirem
-utente(ID, _, _, _, _, _, _, _, _, _) :: (
    solucoes(ID, tratamento(_, ID, _, _, _), S),
    comprimento(S, N),
    N == 0
).

%---------------------------------------------------------------------%
%->                         INVARIANTES DO STAFF                    <-%
%---------------------------------------------------------------------%

% O ID de cada elemento do staff do centro de saude deve ser um numero inteiro e deve ser unico
+staff(ID, _, _, _) :: (
    integer(ID),
    solucoes(ID, staff(ID, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% Cada elemento do staff do centro de saude tem de estar associado a um centro de saude que exista
+staff(_, CS, _, _) :: (
    solucoes(CS, centro_saude(CS, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% Nao permite a remocao de um elemento do staff se existirem registos de vacinacao a si associaos
-staff(ID, _, _, _) :: (
    solucoes(ID, vacinacao_Covid(ID, _, _, _, _), S),
    comprimento(S, N),
    N == 0
).

% Nao permite a remocao de um elemento do staff se existirem registos de tratamentos a si associaos
-staff(ID, _, _, _) :: (
    solucoes(ID, tratamento(ID, _, _, _, _), S),
    comprimento(S, N),
    N == 0
).

%---------------------------------------------------------------------%
%->                 INVARIANTES DO CENTRO DE SAUDE                  <-%
%---------------------------------------------------------------------%

% O ID do centro de saude deve ser um numero inteiro e deve ser unico
+centro_saude(ID, _, _, _, _) :: (
    integer(ID),
    solucoes(ID, centro_saude(ID, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% Nao permite a remocao de um centro de saude se existirem utentes a si associaos
-centro_saude(CS, _, _, _, _) :: (
    solucoes(CS, utente(_, _, _, _, _, _, _, _, _, CS), S),
    comprimento(S, N),
    N == 0
).

% Nao permite a remocao de um centro de saude se existirem elementos do staff a si associaos
-centro_saude(CS, _, _, _, _) :: (
    solucoes(CS, staff(_, CS, _, _), S),
    comprimento(S, N),
    N == 0
).

% Nao permite a remocao de um centro de saude se existirem medicos a si associaos
-centro_saude(CS, _, _, _, _) :: (
    solucoes(CS, medico(_, CS, _, _, _), S),
    comprimento(S, N),
    N == 0
).

%---------------------------------------------------------------------%
%->                    INVARIANTES DA VACINACAO                     <-%
%---------------------------------------------------------------------%

% Nao permite a insercao de registos duplicados
+vacinacao_Covid(STAFF, UTENTE, DATA, VACINA, TOMA) :: (
    solucoes((STAFF, UTENTE, DATA, VACINA, TOMA),
              vacinacao_Covid(STAFF, UTENTE, DATA, VACINA, TOMA), S),
    comprimento(S, N),
    N == 1
).

% O registo de vacinacao tem de estar associado a um elemento do staff que exista
+vacinacao_covid(IDS, _, _, _) :: (
    solucoes(IDS, staff(IDS, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% O registo de vacinacao tem de estar associado a um utente que existe
+vacinacao_covid(_, IDU, _, _, _) :: (
    solucoes(IDU, utente(IDU, _, _, _, _, _, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% A toma da vacina so pode assumir os valores 1 e 2
+vacinacao_covid(_, _, _, _, T) :: (T > 0, T =< 2).

% A segunda toma da vacina so pode ser adminstrada apos a primeira
+vacinacao_covid(_, IDU, _, _, 2) :: (
    solucoes(IDU, vacinacao_covid(_, IDU, _, _, 1), S),
    comprimento(S, N),
    N == 1

).

% Nao permite a remocao de registos de atos de vacinacao
-vacinacao_Covid(_, _, _, _, _) :: fail.

%---------------------------------------------------------------------%
%->                       INVARIANTES DO MEDICO                     <-%
%---------------------------------------------------------------------%

% O ID do medico deve ser um numero inteiro e este deve ser unico
+medico(ID, _, _, _, _) :: (integer(ID),
                            solucoes(ID, medico(ID, _, _, _, _), S),
                            comprimento(S, N),
                            N == 1).

% O medico tem de estar associado a um centro de saude que existe
+medico(_, CS, _, _, _) :: (
    solucoes(CS, centro_saude(CS, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).


% Nao permite a remocao de um medico se existirem consultas a si associadas
-medico(ID, _, _, _, _) :: (solucoes(ID, consulta(ID, _, _, _), S),
                            comprimento(S, N),
                            N == 0).

%---------------------------------------------------------------------%
%->                     INVARIANTES DA CONSULTA                     <-%
%---------------------------------------------------------------------%

% Nao permite a insercao de registos duplicados
+consulta(IDM, IDU, IDC, DATA) :: (
    solucoes((IDM, IDU, IDC, DATA), consulta(IDM, IDU, IDC, DATA), S),
    comprimento(S, N),
    N == 1
).

% O registo de uma consulta tem de estar associado a um medico, utente e centro de saude que existam
+consulta(IDM, IDU, CS, _) :: (
    solucoes((IDM, IDU, CS), (medico(IDM, CS, _, _, _),
                              utente(IDU, _, _, _, _, _, _, _, _, CS),
                              centro_saude(CS, _, _, _, _)) , S),
    comprimento(S, N),
    N == 1
).

% Nao permite a remocao de registos de consultas
-consulta(_, _, _, _) :: fail.

%---------------------------------------------------------------------%
%->                   INVARIANTES DO TRATAMENTO                     <-%
%---------------------------------------------------------------------%

% Nao permite a insercao de registos duplicados
+tratamento(IDS, IDU, IDC, DATA, DESCR) :: (
    solucoes((IDS, IDU, IDC, DATA, DESCR),
             tratamento(IDS, IDU, IDC, DATA, DESCR), S),
    comprimento(S, N),
    N == 1
).

+tratamento(IDS, IDU, CS, _, _) :: (
    solucoes((IDS, IDU, CS), (staff(IDS, CS, _, _),
                              utente(IDU, _, _, _, _, _, _, _, _, CS),
                              centro_saude(CS, _, _, _, _)) , S),
    comprimento(S, N),
    N == 1
).

% Nao permite a remocao de registos de tratamentos
-tratamento(_, _, _, _, _) :: fail.

%------------------------------------------------------------------%
%->               PREDICADOS DE REGISTO E REMOCAO                <-%
%------------------------------------------------------------------%

% Extensao do predicado registar_utente: ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS -> {V, F}
registar_utente(ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS) :- 
    evolucao(utente(ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS)).

% Extensao do predicado remover_utente: ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS -> {V, F}
remover_utente(ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS) :- 
    involucao(utente(ID, NUM, NOME, DN, EMAIL, TLF, M, P,DC, CS)).

% Extensao do predicado registar_centro_saude: ID, NOME, M, TLF, EMAIL -> {V, F}
registar_centro_saude(ID, NOME, M, TLF, EMAIL) :- 
    evolucao(centro_saude(ID, NOME, M, TLF, EMAIL)).

% Extensao do predicado remover_centro_saude: ID, NOME, M, TLF, EMAIL -> {V, F}
remover_centro_saude(ID, NOME, M, TLF, EMAIL) :- 
    involucao(centro_saude(ID, NOME, M, TLF, EMAIL)).

% Extensao do predicado registar_staff: IDS, IDCENTRO, NOME, EMAIL -> {V, F}
registar_staff(IDS, IDCENTRO, NOME, EMAIL) :- 
    evolucao(staff(IDS, IDCENTRO, NOME, EMAIL)).

% Extensao do predicado remover_staff: IDS, IDCENTRO, NOME, EMAIL -> {V, F}
remover_staff(IDS, IDCENTRO, NOME, EMAIL) :- 
    involucao(staff(IDS, IDCENTRO, NOME, EMAIL)).

% Extensao do predicado registar_vacinacao: STAFF, UTENTE, DATA, VACINA, TOMA-> {V, F}
registar_vacinacao(STAFF, UTENTE, DATA, VACINA, TOMA) :- 
    evolucao(vacinacao_Covid(STAFF, UTENTE, DATA, VACINA, TOMA)).

% Extensao do predicado registar_medico: ID, IDCENTRO, NOME, EMAIL, ESP -> {V, F}
registar_medico(ID, IDCENTRO, NOME, EMAIL, ESP) :- 
    evolucao(medico(ID, IDCENTRO, NOME, EMAIL, ESP)).

% Extensao do predicado remover_medico: ID, IDCENTRO, NOME, EMAIL, ESP -> {V, F}
remover_medico(ID, IDCENTRO, NOME, EMAIL, ESP) :- 
    involucao(medico(ID, IDCENTRO, NOME, EMAIL, ESP)).

% Extensao do predicado registar_consulta: IDM, IDU, IDC, DATA -> {V, F}
registar_consulta(IDM, IDU, IDC, DATA) :- 
    evolucao(consulta(IDM, IDU, IDC, DATA)).

% Extensao do predicado registar_tratamento: IDS, IDU, IDC, DATA, DESCR -> {V, F}
registar_tratamento(IDS, IDU, IDC, DATA, DESCR) :- 
    evolucao(tratamento(IDS, IDU, IDC, DATA, DESCR)).

    
    
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
%->                    SISTEMA DE INFERENCIA                     <-%
%------------------------------------------------------------------%

% Extensao do meta-predicado nao: Questao -> {V,F}
nao(Questao) :- Questao, !, fail.
nao(_).

% Extensao do meta-predicado si: Questao, Resposta -> {V,F}
si(Questao, verdadeiro) :- Questao.
si(Questao, falso) :- -Questao.

% Extensao do predicado vacinado: ID -> {V, F}
vacinado(ID) :- 
    solucoes(ID, (utente(ID, _, _, _, _, _, _, _, _, _),
                  vacinacao_Covid(_, ID, _, _, 2)), S),
    comprimento(S, N),
    N == 1.

-vacinado(ID) :- nao(vacinado(ID)).

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
%->                    FUNCIONALIDADES EXTRA                     <-%
%------------------------------------------------------------------%

% Guarda estado atual da base de conhecimento no ficheiro knowledge.pl, sendo este novamente
% carregado quando o programa é executado
save() :- telling(OldStream), tell('knowledge.pl'),
          write(':- module(database, [utente/10, centro_saude/5, staff/4, vacinacao_Covid/5, medico/5, consulta/4, tratamento/5]).'), nl, nl,
          write(':- dynamic utente/10.'), nl,
          write(':- dynamic centro_saude/5.'), nl,
          write(':- dynamic staff/4.'), nl,
          write(':- dynamic vacinacao_Covid/5.'), nl,
          write(':- dynamic medico/5.'), nl,
          write(':- dynamic consulta/4.'), nl,
          write(':- dynamic tratamento/5.'), nl, nl,
          write('% Extensao do predicado utente: #Idutente, No Seguranca_Social, Nome, Data_Nasc, Email, Telefone, Morada, Profissao, [Doencas_Cronicas], #CentroSaude -> {V, F}'), nl,
          solucoes([ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS], utente(ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS), LU), write_utentes(LU), nl,
          write('% Extensao do predicado centro_saude: #Idcentro, Nome, Morada, Telefone, Email -> {V, F}'), nl,
          solucoes([ID, NOME, M, TLF, EMAIL], centro_saude(ID, NOME, M, TLF, EMAIL), LCS), write_centro_saude(LCS), nl,
          write('% Extensao do predicado staff: #Idstaff, #Idcentro, Nome, email -> {V, F}'), nl,
          solucoes([IDS, IDCENTRO, NOME, EMAIL], staff(IDS, IDCENTRO, NOME, EMAIL), LS), write_staff(LS), nl,
          write('% Extensao do predicado vacinacao_Covid: #Idstaff, #Idutente, Data, Vacina, Toma -> {V, F}'), nl,
          solucoes([STAFF, UTENTE, DATA, VACINA, TOMA], vacinacao_Covid(STAFF, UTENTE, DATA, VACINA, TOMA), LV), write_vacinacao_Covid(LV),  nl,
          write('% Extensao do predicado medico: #Idmedico, #Idcentro, Nome, Email, Especialidade -> {V, F}'), nl,
          solucoes([ID, IDCENTRO, NOME, EMAIL, ESP], medico(ID, IDCENTRO, NOME, EMAIL, ESP), ML), write_medico(ML), nl,
          write('% Extensao do predicado consulta: #Idmedico, #Idutente, #Idcentro, Data -> {V, F}'), nl,
          solucoes([IDM, IDU, IDC, DATA], consulta(IDM, IDU, IDC, DATA), CL), write_consulta(CL), nl,
          write('% Extensao do predicado tratamento: #IdStaff, #Idutente, #Idcentro, Data, Descricao -> {V, F}'), nl,
          solucoes([IDS, IDU, IDC, DATA, DSCR], tratamento(IDS, IDU, IDC, DATA, DSCR), TL), write_tratamento(TL),
          told, tell(OldStream).

% Escreve a lista dos utentes presentes na base de conhecimento (usado quando estamos a escrever num ficheiro)
write_utentes([]).
write_utentes([H|T]) :- format("utente(~k, ~k, ~k, ~k, ~k, ~k, ~k, ~k, ~k, ~k).", H), nl,
                        write_utentes(T).

% Escreve a lista dos centros de saude presentes na base de conhecimento (usado quando estamos a escrever num ficheiro)
write_centro_saude([]).
write_centro_saude([H|T]) :- format("centro_saude(~k, ~k, ~k, ~k, ~k).", H), nl,
                             write_centro_saude(T).

% Escreve a lista dos elemento do staff presentes na base de conhecimento (usado quando estamos a escrever num ficheiro)
write_staff([]).
write_staff([H|T]) :- format("staff(~k, ~k, ~k, ~k).", H), nl,
                      write_staff(T).

% Escreve a lista dos registos de vacinação presentes na base de conhecimento (usado quando estamos a escrever num ficheiro)
write_vacinacao_Covid([]).
write_vacinacao_Covid([H|T]) :- format("vacinacao_Covid(~k, ~k, ~k, ~k, ~k).", H), nl,
                                write_vacinacao_Covid(T).

% Escreve a lista dos médicos presentes na base de conhecimento (usado quando estamos a escrever num ficheiro)
write_medico([]).
write_medico([H|T]) :- format("medico(~k, ~k, ~k, ~k, ~k).", H), nl,
                       write_medico(T).

% Escreve a lista das consultas presentes na base de conhecimento (usado quando estamos a escrever num ficheiro)
write_consulta([]).
write_consulta([H|T]) :- format("consulta(~k, ~k, ~k, ~k).", H), nl,
                         write_consulta(T).

% Escreve a lista dos tratamentos presentes na base de conhecimento (usado quando estamos a escrever num ficheiro)
write_tratamento([]).
write_tratamento([H|T]) :- format("tratamento(~k, ~k, ~k, ~k, ~k).", H), nl,
                           write_tratamento(T).
