%------------------------------------------------------------------%
%->                    PREDICADOS AUXILIARES                     <-%
%------------------------------------------------------------------%

% Extensao do predicado comprimento: S, N -> {V, F}
comprimento(S, N) :- length(S, N).

% Extensao do predicado verificaData: date(D, M, A) -> {V, F}
verificaData(date(D, 1, A)) :- D > 0, D =< 31, A > 0.
verificaData(date(D, 2, A)) :- D > 0, A mod 4 =:= 0, D =< 29, A > 0.
verificaData(date(D, 2, A)) :- D > 0, A mod 4 =\= 0, D =< 28, A > 0.
verificaData(date(D, 3, A)) :- D > 0, D =< 31, A > 0.
verificaData(date(D, 4, A)) :- D > 0, D =< 30, A > 0.
verificaData(date(D, 5, A)) :- D > 0, D =< 31, A > 0.
verificaData(date(D, 6, A)) :- D > 0, D =< 30, A > 0.
verificaData(date(D, 7, A)) :- D > 0, D =< 31, A > 0.
verificaData(date(D, 8, A)) :- D > 0, D =< 31, A > 0.
verificaData(date(D, 9, A)) :- D > 0, D =< 30, A > 0.
verificaData(date(D, 10, A)) :- D > 0, D =< 31, A > 0.
verificaData(date(D, 11, A)) :- D > 0, D =< 30, A > 0.
verificaData(date(D, 12, A)) :- D > 0, D =< 31, A > 0.

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
%-> Extensao do predicado que permite a procura de conhecimento  <-%
%------------------------------------------------------------------%

% Extensao do meta-predicado solucoes: X, Y, Z -> {V, F}
solucoes(X, Y, Z) :- findall(X, Y, Z).
