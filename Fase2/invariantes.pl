:- op(900,xfy,'::').

:- ensure_loaded(predicados_auxiliares).

%------------------------------------------------------------------%
%->                    INVARIANTES UNIVERSAIS                    <-%
%------------------------------------------------------------------%

% Invariante que garante que nao existe conhecimento perfeito positivo repetido
+T :: (solucoes(T, T, R),
       comprimento(R, 1)).

% Invariante que garante que nao existe conhecimento perfeito negativo repetido
+(-T) :: (solucoes(T, -T, R),
          comprimento(R, 1)).

% Invariante que nao permite adicionar conhecimento perfeito positivo que contradiz
% conhecimento perfeito negativo
+T :: nao(-T).

% Invariante que nao permite adicionar conhecimento perfeito negativo que contradiz
% conhecimento perfeito positivo
+(-T) :: nao(T).

% Invariante que garante que não existem excecoes repetidas
+(excecao(T)) :: (solucoes(T, excecao(T), R),
                  comprimento(R, 1)).

%------------------------------------------------------------------%
%->            INVARIANTES ESTRUTURAIS E REFERENCIAIS            <-%
%------------------------------------------------------------------%

%----------------------------- UTENTE ------------------------------

% O ID do utente deve ser um numero inteiro e deve ser unico
+utente(ID, _, _, _, _, _, _, _, _, _) :: (
    integer(ID),
    solucoes(ID, utente(ID, _, _, _, _, _, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% O ID do utente deve ser um numero inteiro e deve ser unico
% Conhecimento perfeito negativo
+(-utente(ID, _, _, _, _, _, _, _, _, _)) :: (
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

% O utente tem de estar associado a um centro de saude que exista
% Conhecimento perfeito negativo
+(-utente(_, _, _, _, _, _, _, _, _, CS)) :: (
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

% So pode existir um utente com um determinado numero de Seguranca Social
% Conhecimento perfeito negativo
+(-utente(_, NUM, _, _, _, _, _, _, _, _)) :: (
    solucoes(NUM, utente(_, NUM, _, _, _, _, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% A data de nascimento do utente deve ser uma data valida
+utente(_, _, _, DN, _, _, _, _, _, _) :: (verificaData(DN)).

+(-utente(_, _, _, DN, _, _, _, _, _, _)) :: (verificaData(DN)).

% Invariante que impede a insercao de conhecimento perfeito positivo relativo a um utente
% com numero de Seguranca Social interdito (conhecimento imperfeito interdito)
+utente(ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS) :: (
    solucoes((ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS),
             (utente(ID, NUM, NOME, DN, EMAIL, TLF, M, P, DC, CS),
              nulo(NUM)), S),
    comprimento(S, N),
    N == 0
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

% Nao permite a remocao de um utente se existirem registos de tratamentos a si associaos
-utente(ID, _, _, _, _, _, _, _, _, _) :: (
    solucoes(ID, tratamento(_, ID, _, _, _), S),
    comprimento(S, N),
    N == 0
).


%---------------------------- STAFF --------------------------------

% O ID de cada elemento do staff do centro de saude deve ser um numero inteiro e deve ser unico
+staff(ID, _, _, _) :: (
    integer(ID),
    solucoes(ID, staff(ID, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% O ID de cada elemento do staff do centro de saude deve ser um numero inteiro e deve ser unico
% Conhecimento perfeito negativo
+(-staff(ID, _, _, _)) :: (
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

% Cada elemento do staff do centro de saude tem de estar associado a um centro de saude que exista
% Conhecimento perfeito negativo
+(-staff(_, CS, _, _)) :: (
    solucoes(CS, centro_saude(CS, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% Invariante que impede a insercao de conhecimento perfeito positivo relativo a um funcionario
% com email interdito (conhecimento imperfeito interdito)
+staff(IDS, IDCENTRO, NOME, EMAIL) :: (
    solucoes((IDS, IDCENTRO, NOME, EMAIL),
              (staff(IDS, IDCENTRO, NOME, EMAIL), nulo(EMAIL)), S),
    comprimento(S, N),
    N == 0
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

%------------------------ CENTRO DE SAUDE --------------------------

% O ID do centro de saude deve ser um numero inteiro e deve ser unico
+centro_saude(ID, _, _, _, _) :: (
    integer(ID),
    solucoes(ID, centro_saude(ID, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% O ID do centro de saude deve ser um numero inteiro e deve ser unico
% Conhecimento perfeito negativo
+(-centro_saude(ID, _, _, _, _)) :: (
    integer(ID),
    solucoes(ID, centro_saude(ID, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% Invariante que impede a insercao de conhecimento perfeito positivo relativo a um centro de saude
% com telefone interdito (conhecimento imperfeito interdito)
+centro_saude(ID, NOME, M, TLF, EMAIL) :: (
    solucoes((ID, NOME, M, TLF, EMAIL),
             (centro_saude(ID, NOME, M, TLF, EMAIL),
              nulo(TLF)), S),
    comprimento(S, N),
    N == 0
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


%---------------------------- VACINACACAO --------------------------

% Nao permite a insercao de registos duplicados
+vacinacao_Covid(STAFF, UTENTE, DATA, VACINA, TOMA) :: (
    solucoes((STAFF, UTENTE, DATA, VACINA, TOMA),
              vacinacao_Covid(STAFF, UTENTE, DATA, VACINA, TOMA), S),
    comprimento(S, N),
    N == 1
).

% Nao permite a insercao de registos duplicados
% Conhecimento perfeito negativo
+(-vacinacao_Covid(STAFF, UTENTE, DATA, VACINA, TOMA)) :: (
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

% O registo de vacinacao tem de estar associado a um elemento do staff que exista
% Conhecimento perfeito negativo
+(-vacinacao_covid(IDS, _, _, _)) :: (
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

% O registo de vacinacao tem de estar associado a um utente que existe
% Conhecimento perfeito negativo
+(-vacinacao_covid(_, IDU, _, _, _)) :: (
    solucoes(IDU, utente(IDU, _, _, _, _, _, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% A toma da vacina so pode assumir os valores 1 e 2
+vacinacao_covid(_, _, _, _, T) :: (T > 0, T =< 2).

% A toma da vacina so pode assumir os valores 1 e 2
% Conhecimento perfeito negativo
+(-vacinacao_covid(_, _, _, _, T)) :: (T > 0, T =< 2).

% A segunda toma da vacina so pode ser adminstrada apos a primeira
+vacinacao_covid(_, IDU, _, _, 2) :: (
    solucoes(IDU, vacinacao_covid(_, IDU, _, _, 1), S),
    comprimento(S, N),
    N == 1
).

% A segunda toma da vacina so pode ser adminstrada apos a primeira
% Conhecimento perfeito negativo
+(-vacinacao_covid(_, IDU, _, _, 2)) :: (
    solucoes(IDU, vacinacao_covid(_, IDU, _, _, 1), S),
    comprimento(S, N),
    N == 1
).

% Invariante que impede a insercao de conhecimento perfeito positivo relativo a um registo
% de vacinacao com vacina interdita (conhecimento imperfeito interdito)
+vacinacao_Covid(STAFF, UTENTE, DATA, VACINA, TOMA) :: (
    solucoes((STAFF, UTENTE, DATA, VACINA, TOMA),
             (vacinacao_Covid(STAFF, UTENTE, DATA, VACINA, TOMA), nulo(VACINA)), S),
    comprimento(S, N),
    N == 0
).

% Nao permite a remocao de registos de atos de vacinacao
-vacinacao_Covid(_, _, _, _, _) :: fail.


%-------------------------------- MEDICO ---------------------------

% O ID do medico deve ser um numero inteiro e este deve ser unico
+medico(ID, _, _, _, _) :: (
    integer(ID),
    solucoes(ID, medico(ID, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% O ID do medico deve ser um numero inteiro e este deve ser unico
% Conhecimento perfeito negativo
+(-medico(ID, _, _, _, _)) :: (
    integer(ID),
    solucoes(ID, medico(ID, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% O medico tem de estar associado a um centro de saude que existe
+medico(_, CS, _, _, _) :: (
    solucoes(CS, centro_saude(CS, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% O medico tem de estar associado a um centro de saude que existe
% Conhecimento perfeito negativo
+(-medico(_, CS, _, _, _)) :: (
    solucoes(CS, centro_saude(CS, _, _, _, _), S),
    comprimento(S, N),
    N == 1
).

% Invariante que impede a insercao de conhecimento perfeito positivo relativo a um medico
% com centro de saude interdito (conhecimento imperfeito interdito)
+medico(ID, IDCENTRO, NOME, EMAIL, ESP) :: (
    solucoes((ID, IDCENTRO, NOME, EMAIL, ESP),
              (medico(ID, IDCENTRO, NOME, EMAIL, ESP), nulo(IDCENTRO)), S),
    comprimento(S, N),
    N == 0
).

% Nao permite a remocao de um medico se existirem consultas a si associadas
-medico(ID, _, _, _, _) :: (
    solucoes(ID, consulta(ID, _, _, _), S),
    comprimento(S, N),
    N == 0
).


%----------------------------- CONSULTA ----------------------------

% Nao permite a insercao de registos duplicados
+consulta(IDM, IDU, IDC, DATA) :: (
    solucoes((IDM, IDU, IDC, DATA), consulta(IDM, IDU, IDC, DATA), S),
    comprimento(S, N),
    N == 1
).

% Nao permite a insercao de registos duplicados
% Conhecimento perfeito negativo
+(-consulta(IDM, IDU, IDC, DATA)) :: (
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

% O registo de uma consulta tem de estar associado a um medico, utente e centro de saude que existam
% Conhecimento perfeito negativo
+(-consulta(IDM, IDU, CS, _)) :: (
    solucoes((IDM, IDU, CS), (medico(IDM, CS, _, _, _),
                              utente(IDU, _, _, _, _, _, _, _, _, CS),
                              centro_saude(CS, _, _, _, _)) , S),
    comprimento(S, N),
    N == 1
).

% Invariante que impede a insercao de conhecimento perfeito positivo relativo a uma consulta
% com utente interdito (conhecimento imperfeito interdito)
+consulta(IDM, IDU, IDC, DATA) :: (
    solucoes((IDM, IDU, IDC, DATA),
             (consulta(IDM, IDU, IDC, DATA), nulo(IDU)), S),
    comprimento(S, N),
    N == 0
).

% Nao permite a remocao de registos de consultas
-consulta(_, _, _, _) :: fail.


%---------------------------- TRATAMENTO ---------------------------

% Nao permite a insercao de registos duplicados
+tratamento(IDS, IDU, IDC, DATA, DESCR) :: (
    solucoes((IDS, IDU, IDC, DATA, DESCR),
             tratamento(IDS, IDU, IDC, DATA, DESCR), S),
    comprimento(S, N),
    N == 1
).

% Nao permite a insercao de registos duplicados
% Conhecimento perfeito negativo
+(-tratamento(IDS, IDU, IDC, DATA, DESCR)) :: (
    solucoes((IDS, IDU, IDC, DATA, DESCR),
             tratamento(IDS, IDU, IDC, DATA, DESCR), S),
    comprimento(S, N),
    N == 1
).

% O registo de um tratamento tem de estar associado a um elemento do staff, utente e centro de saude que existam
+tratamento(IDS, IDU, CS, _, _) :: (
    solucoes((IDS, IDU, CS), (staff(IDS, CS, _, _),
                              utente(IDU, _, _, _, _, _, _, _, _, CS),
                              centro_saude(CS, _, _, _, _)) , S),
    comprimento(S, N),
    N == 1
).

% O registo de um tratamento tem de estar associado a um elemento do staff, utente e centro de saude que existam
% Conhecimento perfeito negativo
+(-tratamento(IDS, IDU, CS, _, _)) :: (
    solucoes((IDS, IDU, CS), (staff(IDS, CS, _, _),
                              utente(IDU, _, _, _, _, _, _, _, _, CS),
                              centro_saude(CS, _, _, _, _)) , S),
    comprimento(S, N),
    N == 1
).

% Invariante que impede a insercao de conhecimento perfeito positivo relativo a um tratamento
% com centro de saude interdito (conhecimento imperfeito interdito)
+tratamento(IDS, IDU, IDC, DATA, DSCR) :: (
    solucoes((IDS, IDU, IDC, DATA, DSCR),
             (tratamento(IDS, IDU, IDC, DATA, DSCR), nulo(IDC)), S),
    comprimento(S, N),
    N == 0
).

% Nao permite a remocao de registos de tratamentos
-tratamento(_, _, _, _, _) :: fail.
