:- module(database, [utente/10, centro_saude/5, staff/4, vacinacao_Covid/5, medico/5, consulta/4, tratamento/5, '-'/1, excecao/1]).

:- dynamic utente/10.
:- dynamic centro_saude/5.
:- dynamic staff/4.
:- dynamic vacinacao_Covid/5.
:- dynamic medico/5.
:- dynamic consulta/4.
:- dynamic tratamento/5.
:- dynamic '-'/1.
:- dynamic excecao/1.

% Extensao do predicado utente: #Idutente, No Seguranca_Social, Nome, Data_Nasc, Email, Telefone, Morada, Profissao, [Doencas_Cronicas], #CentroSaude -> {V, F, D}

%------------------------ Conhecimento Perfeito Positivo -----------------------
utente(1, 14492, 'Duarte Carvalho', date(1998, 4, 21), 'duartecarvalho@gmail.com', 91761416, 'Lisboa', 'Estudante', ['Asma'], 4).
utente(2, 95110, 'Mariana Pereira', date(1988, 11, 12), 'marianapereira@gmail.com', 915992520, 'Porto', 'Atleta', [], 4).
utente(3, 79297, 'Teresa Marques', date(1987, 8, 30), 'teresamarques@gmail.com', 913844675, 'Viseu', 'Engenheira', ['Hipertensao'], 4).
utente(4, 71723, 'Sofia Soares', date(1979, 6, 8), 'sofiasoares@gmail.com', 919555582, 'Porto', 'Advogada', ['Diabetes'], 2).
utente(5, 40203, 'Rui Rocha', date(1987, 12, 24), 'ruirocha@gmail.com', 919219565, 'Braga', 'Atleta', [], 1).
utente(6, 77645, 'Joao Martins', date(1998, 11, 12), 'joaomartins@gmail.com', 917630282, 'Coimbra', 'Estudante', ['Asma'], 2).
utente(7, 67275, 'Diogo Rodrigues', date(1995, 1, 25), 'diogorodrigues@gmail.com', 916543686, 'Lisboa', 'Engenheiro', ['Diabetes','Cancro'], 2).
utente(8, 76991, 'Francisca Lopes', date(1986, 9, 15), 'franciscalopes@gmail.com', 913672965, 'Braga', 'Enfermeira', ['Asma'], 2).
utente(9, 82539, 'Tiago Lima', date(1978, 12, 12), 'tiagolima@gmail.com', 91683448, 'Lisboa', 'Medico', ['Hipertensao'], 5).
utente(10, 16086, 'Daniela Macedo', date(1977, 10, 25), 'danielamacedo@gmail.com', 911216397, 'Coimbra', 'Medico', [], 3).
utente(11, 56418, 'Daniel Santos', date(1955, 2, 23), 'danielsantos@gmail.com', 915632478, 'Faro', 'Advogado', [], 4).
utente(12, 65474, 'Mauro Faria', date(1982, 2, 5), 'maurofaria@gmail.com', 934568253, 'Coimbra', 'Bombeiro', ['Diabetes'], 3).

%------------------------ Conhecimento Perfeito Negativo -----------------------
-utente(13, 41582, 'Duarte Pedro', date(1987, 6, 2), 'duartepedro@gmail.com', 913654782, 'Coimbra', 'Jornalista', [], 3).
-utente(14, 45217, 'Tiago Loureiro', date(1993, 5, 12), 'tiagoloureiro@gmail.com', 912396755, 'Lisboa', 'Enfermeiro', [], 4).

%------------------------ Conhecimento Imperfeito Incerto ----------------------
% Nao se conhece a idade do utente #15 uma vez que a sua data de nascimento e desconhecida
utente(15, 56257, 'Mateus Silva', data_desconhecida, 'mateussilva@gmail.com', 915698401, 'Coimbra', 'Bombeiro', [], 4).
excecao(utente(ID, NUM, NOME, _, EMAIL, TLF, M, P, DC, CS)) :- utente(ID, NUM, NOME, data_desconhecida, EMAIL, TLF, M, P, DC, CS).

%------------------------ Conhecimento Imperfeito Impreciso --------------------
 % Nao se sabe se a Teresa mora em Braga ou no Porto
excecao(utente(16, 59240, 'Teresa Soares', date(1998, 1, 23), 'teresasoares@gmail.com', 913654700, 'Braga', 'Estudante', ['Hipertensao'], 1)).
excecao(utente(16, 59240, 'Teresa Soares', date(1998, 1, 23), 'teresasoares@gmail.com', 913654700, 'Porto', 'Estudante', ['Hipertensao'], 1)).

%------------------------ Conhecimento Imperfeito Interdito --------------------
% Nao se sabe nem e possivel saber qual o numero de Seguranca Social do utente #17
utente(17, sc_desconhecido, 'Diogo Fernandes', date(1995, 9, 5), 'diogofernandes@gmail.com', 924581465, 'Lisboa', [], 4).
nulo(sc_desconhecido).
excecao(utente(ID, _, NOME, DN, EMAIL, TLF, M, P, DC, CS)) :- utente(ID, sc_desconhecido, NOME, DN, EMAIL, TLF, M, P, DC, CS).

% Extensao do predicado centro_saude: #Idcentro, Nome, Morada, Telefone, Email -> {V, F, D}

%------------------------ Conhecimento Perfeito Positivo -----------------------
centro_saude(1, 'Hospital de Braga', 'Braga', 253027000, 'hospitaldebraga@gmail.com').
centro_saude(2, 'Hospital Da Senhora Da Oliveira', 'Guimaraes', 253540330, 'hospitaldasenhoradaoliveira@gmail.com').
centro_saude(3, 'Centro Hospitalar e Universitario de Coimbra', 'Coimbra', 239400400, 'centrohospitalareuniversitariodecoimbra@gmail.com').
centro_saude(4, 'Hospital de Santa Maria', 'Lisboa', 217805000, 'hospitaldesantamaria@gmail.com').

%------------------------ Conhecimento Perfeito Negativo -----------------------
-centro_saude(5, 'Centro Hospitalar Povoa de Varzim', 'Povoa de Varzim', 252690600, 'centrohospitalarpovoadevarzim@gmail.com').

%------------------------ Conhecimento Imperfeito Incerto ----------------------
% Nao se conhece o telefone do centro de saude #6
centro_saude(6, 'Hospital da Luz', telefone_desconhecido, 'Guimaraes', 'hospitaldaluz@gmail.com').
excecao(centro_saude(ID, NOME, M, _, EMAIL)) :- centro_saude(ID, NOME, M, telefone_desconhecido, EMAIL).

%------------------------ Conhecimento Imperfeito Impreciso --------------------
% Nao se sabe se o centro de saude #7 esta localizado em Setubal ou em Lisboa
excecao(centro_saude(7, 'Hospital De Santiago', 'Setubal', 265509200, 'hospitaldesantiago@gmail.com')).
excecao(centro_saude(7, 'Hospital De Santiago', 'Lisboa', 265509200, 'hospitaldesantiago@gmail.com')).

%------------------------ Conhecimento Imperfeito Interdito --------------------
% Nao se sabe nem e possivel saber o telefone do centro de saude #8
centro_saude(8, 'Hospital da Misericordia de Evora', 'Evora', tlf_desconhecido, 'hospitaldamisericordiadeevora@gmail.com').
nulo(tlf_desconhecido).
excecao(centro_saude(ID, NOME, M, _, EMAIL)) :- centro_saude(ID, NOME, M, tlf_desconhecido, EMAIL).

% Extensao do predicado staff: #Idstaff, #Idcentro, Nome, email -> {V, F, D}

%------------------------ Conhecimento Perfeito Positivo -----------------------
staff(1, 4, 'Teresa Rodrigues', 'teresarodrigues@gmail.com').
staff(2, 3, 'Diogo Martins', 'diogomartins@gmail.com').
staff(3, 2, 'Daniela Marques', 'danielamarques@gmail.com').
staff(4, 2, 'Joao Lopes', 'joaolopes@gmail.com').
staff(5, 3, 'Rui Lima', 'ruilima@gmail.com').
staff(6, 4, 'Mariana Soares', 'marianasoares@gmail.com').
staff(7, 1, 'Duarte Pereira', 'duartepereira@gmail.com').
staff(8, 4, 'Francisca Lima', 'franciscalima@gmail.com').
staff(9, 3, 'Sofia Macedo', 'sofiamacedo@gmail.com').
staff(10, 1, 'Tiago Carvalho', 'tiagocarvalho@gmail.com').

%------------------------ Conhecimento Perfeito Negativo -----------------------
-staff(11, 3, 'Igor Rodrigues', 'igorrodrigues@gmail.com').
-staff(12, 2, 'Ana Mendes', 'anamendes@gmail.com').

%------------------------ Conhecimento Imperfeito Incerto ----------------------
% Nao se sabe em que centro de saude o Antonio exerce funcoes
staff(13, idc_desconhecido, 'Antonio Gomes', 'antoniogomes@gmail.com').
excecao(staff(IDS, _, NOME, EMAIL)) :- staff(IDS, idc_desconhecido, NOME, EMAIL).

%------------------------ Conhecimento Imperfeito Impreciso --------------------
% Nao se sabe se o Jorge exerce funcoes no centro de saude #2 ou no #3
excecao(staff(14, 2, 'Jorge Carvalho', 'jorgecarvalho@gmail.com')).
excecao(staff(14, 3, 'Jorge Carvalho', 'jorgecarvalho@gmail.com')).

%------------------------ Conhecimento Imperfeito Interdito --------------------
% Nao se conhece nem e possivel conhecer o email do Rafael
staff(15, 1, 'Rafael Costa', email_desconhecido).
nulo(email_desconhecido).
excecao(staff(IDS, IDCENTRO, NOME, _)) :- staff(IDS, IDCENTRO, NOME, email_desconhecido).

% Extensao do predicado vacinacao_Covid: #Idstaff, #Idutente, Data, Vacina, Toma -> {V, F, D}

%------------------------ Conhecimento Perfeito Positivo -----------------------
vacinacao_Covid(7, 1, date(2021,4,23), 'Pfizer', 1).
vacinacao_Covid(5, 1, date(2021,8,24), 'Pfizer', 2).
vacinacao_Covid(7, 2, date(2020,2,1), 'Astrazeneca', 1).
vacinacao_Covid(10, 3, date(2020,10,13), 'Pfizer', 1).
vacinacao_Covid(9, 4, date(2020,11,11), 'Pfizer', 1).
vacinacao_Covid(6, 4, date(2020,12,19), 'Pfizer', 2).
vacinacao_Covid(1, 5, date(2020,3,21), 'Astrazeneca', 1).
vacinacao_Covid(5, 5, date(2020,7,30), 'Astrazeneca', 2).
vacinacao_Covid(8, 6, date(2020,5,3), 'Pfizer', 1).
vacinacao_Covid(7, 6, date(2020,10,20), 'Pfizer', 2).
vacinacao_Covid(7, 8, date(2021,7,17), 'Pfizer', 1).
vacinacao_Covid(9, 9, date(2020,2,21), 'Astrazeneca', 1).
vacinacao_Covid(3, 10, date(2020,9,29), 'Pfizer', 1).
vacinacao_Covid(1, 10, date(2020,12,13), 'Pfizer', 2).

%------------------------ Conhecimento Perfeito Negativo -----------------------
-vacinacao_Covid(3, 5, date(2021, 3, 21), 'Pfizer', 1).
-vacinacao_Covid(3, 5, date(2021, 4, 2), 'Pfizer', 2).

%------------------------ Conhecimento Imperfeito Incerto ----------------------
% Nao se sabe com que vacina o utente #7 foi vacinado
vacinacao_Covid(1, 7, date(2020,3,21), vacina_desconhecida, 1).
excecao(vacinacao_Covid(STAFF, UTENTE, DATA, _, TOMA)) :- vacinacao_Covid(STAFF, UTENTE, DATA, vacina_desconhecida, TOMA).

%------------------------ Conhecimento Imperfeito Impreciso --------------------
% Nao se sabe que vacina realmente tomou o utente #11
excecao(vacinacao_Covid(3, 11, date(2021, 3, 21), 'Pfizer', 1)).
excecao(vacinacao_Covid(3, 11, date(2021, 3, 21), 'Astrazeneca', 1)).

%------------------------ Conhecimento Imperfeito Interdito --------------------
% Nao se conhece nem e possivel conhecer qual a vacina administrada
vacinacao_covid(3, 12, date(2021,7,17), vac_desconhecida, 1).
nulo(vac_desconhecida).
excecao(vacinacao_Covid(STAFF, UTENTE, DATA, _, TOMA)) :- vacinacao_Covid(STAFF, UTENTE, DATA, vac_desconhecida, TOMA).

% Extensao do predicado medico: #Idmedico, #Idcentro, Nome, Email, Especialidade -> {V, F, D}

%------------------------ Conhecimento Perfeito Positivo -----------------------
medico(1, 3, 'Goncalo Santos', 'goncalosantos@gmail.com', 'Cardiologia').
medico(2, 3, 'Roberto Moreira', 'robertomoreira@gmail.com', 'Anestesiologia').
medico(3, 4, 'Rui Santos', 'ruisantos@gmail.com', 'Clinica Geral').
medico(4, 2, 'Ines Nunes', 'inesnunes@gmail.com', 'Dermatologia').
medico(5, 1, 'Hugo Alves', 'hugoalves@gmail.com', 'Gastrenterologia').
medico(6, 1, 'Ricardo Sousa', 'ricardosousa@gmail.com', 'Medicina Dentaria').

%------------------------ Conhecimento Perfeito Negativo -----------------------
-medico(7, 4, 'Cesar Martins', 'cesarmartins@gmail.com', 'Oftalmologia').

%------------------------ Conhecimento Imperfeito Incerto ----------------------
% Nao se conhece a especialidade do medico #8
medico(8, 2, 'Henrique Ferreira', 'henriqueferreira@gmail', especialidade_desconhecida).
excecao(medico(ID, IDCENTRO, NOME, EMAIL, _)) :- medico(ID, IDCENTRO, NOME, EMAIL, especialidade_desconhecida).

%------------------------ Conhecimento Imperfeito Impreciso --------------------
% Nao se sabe se o medico #9 e dermatologista ou cardiologista
excecao(medico(9, 1, 'Pedro Reis', 'pedroreis@gmail.com', 'Dermatologia')).
excecao(medico(9, 1, 'Pedro Reis', 'pedroreis@gmail.com', 'Cardiologia')).

%------------------------ Conhecimento Imperfeito Interdito --------------------
% Nao se sabe nem e possivel saber em que centro de saude o medico #10 exerce funcoes
medico(10, centro_desconhecido, 'Rui Costa', 'ruicosta@gmail.com', 'Otorrinolaringologia').
nulo(centro_desconhecido).
excecao(medico(ID, _, NOME, EMAIL, ESP)) :- medico(ID, centro_desconhecido, NOME, EMAIL, ESP).


% Extensao do predicado consulta: #Idmedico, #Idutente, #Idcentro, Data -> {V, F, D}

%------------------------ Conhecimento Perfeito Positivo -----------------------
consulta(3, 1, 4, date(2020,10,15)).
consulta(6, 5, 1, date(2021,3,2)).
consulta(4, 8, 2, date(2020,12,20)).
consulta(3, 2, 4, date(2020,8,14)).

%------------------------ Conhecimento Perfeito Negativo -----------------------
-consulta(3, 1, 4, date(2021,2,15)).

%------------------------ Conhecimento Imperfeito Incerto ----------------------
% Nao se sabe qual o utente que foi a consulta
consulta(4, utente_desconhecido, 2, date(2021,2,15)).
excecao(consulta(IDM, _, IDC, DATA)) :- consulta(IDM, utente_desconhecido, IDC, DATA).

%------------------------ Conhecimento Imperfeito Impreciso --------------------
% Nao se sabe se foi o utente #5 ou #8 que foi a consultas
excecao(consulta(4, 5, 2, date(2021,3,1))).
excecao(consulta(4, 8, 2, date(2021,3,1))).

%------------------------ Conhecimento Imperfeito Interdito --------------------
% Nao se sabe nem e possivel saber o utente que foi a consulta
consulta(4, ut_desconhecido, 2, date(2021,1,15)).
nulo(ut_desconhecido).
excecao(consulta(IDM, _, IDC, DATA)) :- consulta(IDM, ut_desconhecido, IDC, DATA).

% Extensao do predicado tratamento: #IdStaff, #Idutente, #Idcentro, Data, Descricao -> {V, F, D}

%------------------------ Conhecimento Perfeito Positivo -----------------------
tratamento(4, 4, 2, date(2021,3,14), 'Radiografia Perna').
tratamento(6, 1, 4, date(2021,2,1), 'Eletrocardiograma').
tratamento(7, 5, 1, date(2020,5,14), 'Exame Pulmonar').
tratamento(3, 8, 2, date(2021,3,1), 'Endoscopia').

%------------------------ Conhecimento Perfeito Negativo -----------------------
-tratamento(7, 5, 1, date(2021,2,17), 'Radiografia').

%------------------------ Conhecimento Imperfeito Incerto ----------------------
% Nao se sabe qual o elemento do staff do centro de saude responsavel pelo tratamentos
tratamento(ids_desconhecido, 1, 4, date(2020,11,4), 'Biopsia').
excecao(tratamento(_, IDU, IDC, DATA, DSCR)) :- tratamento(ids_desconhecido, IDU, IDC, DATA, DSCR).

%------------------------ Conhecimento Imperfeito Impreciso --------------------
% Nao se sabe se foi o elemento do staff do centro de saude #3 ou #5 responsavel pelo tratamento
excecao(tratamento(3, 8, 2, date(2020,12,15), 'Prova de Esforco')).
excecao(tratamento(5, 8, 2, date(2020,12,15), 'Prova de Esforco')).

%------------------------ Conhecimento Imperfeito Interdito --------------------
% Nao se conhece nem e possivel conhecer qual o centro de saude em que o tratamento foi realizado
tratamento(7, 5, cs_desconhecido, date(2020,11,20), 'Exame Pulmonar').
nulo(cs_desconhecido).
excecao(tratamento(IDS, IDU, IDC, _, DSCR)) :- tratamento(IDS, IDU, cs_desconhecido, DATA, DSCR).
