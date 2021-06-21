:- module(database, [utente/10, centro_saude/5, staff/4, vacinacao_Covid/5, medico/5, consulta/4, tratamento/5]).

:- dynamic utente/10.
:- dynamic centro_saude/5.
:- dynamic staff/4.
:- dynamic vacinacao_Covid/5.
:- dynamic medico/5.
:- dynamic consulta/4.
:- dynamic tratamento/5.

% Extensao do predicado utente: #Idutente, No Seguranca_Social, Nome, Data_Nasc, Email, Telefone, Morada, Profissao, [Doencas_Cronicas], #CentroSaude -> {V, F}
utente(1, 14492, 'Duarte Carvalho', date(1998,4,21), 'duartecarvalho@gmail.com', 91761416, 'Lisboa', 'Estudante', ['Asma'], 4).
utente(2, 95110, 'Mariana Pereira', date(1988,11,12), 'marianapereira@gmail.com', 915992520, 'Porto', 'Atleta', [], 4).
utente(3, 79297, 'Teresa Marques', date(1987,8,30), 'teresamarques@gmail.com', 913844675, 'Viseu', 'Engenheira', ['Hipertensao'], 4).
utente(4, 71723, 'Sofia Soares', date(1979,6,8), 'sofiasoares@gmail.com', 919555582, 'Porto', 'Advogada', ['Diabetes'], 2).
utente(5, 40203, 'Rui Rocha', date(1987,12,24), 'ruirocha@gmail.com', 919219565, 'Braga', 'Atleta', [], 1).
utente(6, 77645, 'Joao Martins', date(1998,11,12), 'joaomartins@gmail.com', 917630282, 'Coimbra', 'Estudante', ['Asma'], 2).
utente(7, 67275, 'Diogo Rodrigues', date(1995,1,25), 'diogorodrigues@gmail.com', 916543686, 'Lisboa', 'Engenheiro', ['Diabetes','Cancro'], 2).
utente(8, 76991, 'Francisca Lopes', date(1986,9,15), 'franciscalopes@gmail.com', 913672965, 'Braga', 'Enfermeira', ['Asma'], 2).
utente(9, 82539, 'Tiago Lima', date(1978,12,12), 'tiagolima@gmail.com', 91683448, 'Lisboa', 'Medico', ['Hipertensao'], 5).
utente(10, 16086, 'Daniela Macedo', date(1977,10,25), 'danielamacedo@gmail.com', 911216397, 'Coimbra', 'Medico', [], 3).
utente(11, 56418, 'Daniel Santos', date(1955, 2, 23), 'danielsantos@gmail.com', 915632478, 'Faro', 'Advogado', [], 4).

% Extensao do predicado centro_saude: #Idcentro, Nome, Morada, Telefone, Email -> {V, F}
centro_saude(1, 'Hospital de Braga', 'Braga', 253027000, 'hospitaldebraga@gmail.com').
centro_saude(2, 'Hospital Da Senhora Da Oliveira', 'Guimaraes', 253540330, 'hospitaldasenhoradaoliveira@gmail.com').
centro_saude(3, 'Centro Hospitalar e Universitario de Coimbra', 'Coimbra', 239400400, 'centrohospitalareuniversitáriodecoimbra@gmail.com').
centro_saude(4, 'Hospital de Santa Maria', 'Lisboa', 217805000, 'hospitaldesantamaria@gmail.com').

% Extensao do predicado staff: #Idstaff, #Idcentro, Nome, email -> {V, F}
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

% Extensao do predicado vacinacao_Covid: #Idstaff, #Idutente, Data, Vacina, Toma -> {V, F}
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

% Extensao do predicado medico: #Idmedico, #Idcentro, Nome, Email, Especialidade -> {V, F}
medico(1, 3, 'Goncalo Santos', 'goncalosantos@gmail.com', 'Cardiologia').
medico(2, 3, 'Roberto Moreira', 'robertomoreira@gmail.com', 'Anestesiologia').
medico(3, 4, 'Rui Santos', 'ruisantos@gmail.com', 'Clinica Geral').
medico(4, 2, 'Ines Nunes', 'inesnunes@gmail.com', 'Dermatologia').
medico(5, 1, 'Hugo Alves', 'hugoalves@gmail.com', 'Gastrenterologia').
medico(6, 1, 'Ricardo Sousa', 'ricardosousa@gmail.com', 'Medicina Dentaria').

% Extensao do predicado consulta: #Idmedico, #Idutente, #Idcentro, Data -> {V, F}
consulta(3, 1, 4, date(2020,10,15)).
consulta(6, 5, 1, date(2021,3,2)).
consulta(4, 8, 2, date(2020,12,20)).
consulta(3, 2, 4, date(2020,8,14)).

% Extensao do predicado tratamento: #IdStaff, #Idutente, #Idcentro, Data, Descricao -> {V, F}
tratamento(4, 4, 2, date(2021,3,14), 'Radiografia Perna').
tratamento(6, 1, 4, date(2021,2,1), 'Eletrocardiograma').
tratamento(7, 5, 1, date(2020,5,14), 'Exame Pulmonar').
tratamento(3, 8, 2, date(2021,3,1), 'Analises Clinicas').
