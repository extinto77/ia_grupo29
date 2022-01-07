% ---------------------------------------------------------------------------------------------------------             
% ---------- BASE CONHECIMENTO
% ---------------------------------------------------------------------------------------------------------             

% ---------- mes(mes, dias maximo), fevereiro pode ser bissexto
mes(1, 31).
mes(3, 31).
mes(4, 30).
mes(5, 31).
mes(6, 30).
mes(7, 31).
mes(8, 31).
mes(9, 30).
mes(10, 31).
mes(11, 30).
mes(12, 31).

% ----------- veiculo(tipo, velocidadeMedia, cargaMax)
veiculo(bicicleta, 10, 5).
veiculo(mota,      35, 20).
veiculo(carro,     25, 100). 

preco(bicicleta, 0.5).
preco(mota,      1).
preco(carro,     1.5).

% ----------- cliente(id, nrEncomendas) /2

cliente(daniel,  6).
cliente(joao,    2).
cliente(caldas,  2).
cliente(abacao,  7).
cliente(joana,   4).
cliente(freitas, 6).
cliente(claudia, 2).

% ----------- estafeta(id, nrEntregas, avaliacao) /3

estafeta(ups,        4, 3.5).
estafeta(chronopost, 2, 4.7).
estafeta(ctt,        4, 3.85).
estafeta(amazon,     2, 3.16).

% ----------- encomenda(Estado, Id, IdClinet, Peso, Volume, Freguesia/Morada, prazo) /7

encomenda(registada, cadeira, daniel,    2 ,30,  casteloes/rua_ponte_santa,  date(0,0,0)/time(12,0,0)).
encomenda(registada, gorro, joao,        1, 10,  landim/rua_ponte,  date(0,0,0)/time(12,0,0)).
encomenda(registada, folha, daniel,      0.1 ,5, landim/rua_nossa_senhora,  date(0,0,0)/time(12,0,0)).
encomenda(registada, portao, freitas,    16 ,30, escordo/rua_jesuita, date(0,0,1)/time(12,0,0)).
encomenda(registada, sal, abacao,        2 ,30,  escordo/rua_sapos, date(0,0,0)/time(12,0,0)).
encomenda(registada, joelheiras, abacao, 80 ,30, escordo/rua_feliz, date(0,0,0)/time(12,0,0)).
encomenda(registada, camara, freitas,    2  ,4 , escordo/alameda_joao, date(0,0,0)/time(1,0,0)).
encomenda(registada, comando, freitas,   3  ,5 , guimaraes/rua_25abril, date(0,0,3)/time(0,0,0)).
encomenda(registada, estojo, joana,      8  ,10 , guimaraes/rua_ponte, date(0,0,10)/time(0,0,0)).
encomenda(registada, hub, joana,         1  ,2 , guimaraes/rua_brito, date(0,0,2)/time(0,0,0)).
encomenda(registada, calculadora, joana, 10 ,7 , guimaraes/avenida_brasil, date(0,0,0)/time(20,0,0)).


encomenda(distribuicao, corta_unhas, freitas, 2 ,30,  landim/rua_ponte,  date(0,0,0)/time(12,0,0)).
encomenda(distribuicao, queijo, abacao,       1, 10,  landim/rua_ponte,  date(0,0,0)/time(12,0,0)).
encomenda(distribuicao, moto4, daniel,        50 ,10, landim/rua_ponte,  date(0,0,0)/time(12,0,0)).
encomenda(distribuicao, jetski, daniel,       16 ,30, escordo/rua_ponte, date(0,0,1)/time(12,0,0)).
encomenda(distribuicao, tabuleiro, abacao,    2 ,30,  escordo/rua_ponte, date(0,0,0)/time(12,0,0)).
encomenda(distribuicao, oculos, abacao,       80 ,30, escordo/rua_ponte, date(0,0,0)/time(12,0,0)).

encomenda(entregue, luvas, joana,       1, 10, landim/rua_ponte,   date(0,0,2)/time(12,0,0)).
encomenda(entregue, boqueira, freitas,  2 ,30, landim/rua_ponte,   date(0,0,0)/time(1,0,0)).
encomenda(entregue, disquete, claudia,  50 ,10, landim/rua_ponte,  date(0,0,0)/time(0,20,0)).
encomenda(entregue, granito, daniel,    16 ,30, escordo/rua_ponte, date(0,0,1)/time(0,0,0)).
encomenda(entregue, isqueiro, claudia,  2 ,30, escordo/rua_ponte,  date(0,0,0)/time(12,0,0)).
encomenda(entregue, tricornio, freitas, 80 ,30, escordo/rua_ponte, date(0,0,0)/time(0,10,0)). %imediato
encomenda(entregue, porta, abacao,      1, 10, landim/rua_ponte,   date(0,0,0)/time(20,0,0)).
encomenda(entregue, tesoura, daniel,    2 ,30, landim/rua_ponte,   date(0,0,0)/time(7,0,0)).
encomenda(entregue, loureiro, joao,     50 ,10, landim/rua_ponte,  date(0,0,0)/time(10,0,0)).
encomenda(entregue, marmore, caldas,    16 ,30, escordo/rua_ponte, date(0,0,1)/time(12,0,0)).
encomenda(entregue, flor, caldas,        2 ,30, escordo/rua_ponte, date(0,0,0)/time(2,0,0)).
encomenda(entregue, capa, abacao,       80 ,30, guimaraes/rua_ponte, date(0,0,3)/time(12,0,0)).
 


% ----------- entrega(idEncomenda, idEstafeta, veiculo, DataInicio, DataFim, avaliacao) /6

entrega(cadeira, empty, empty,        date(2021,11, 4)/time(12,30,0), empty/empty, empty).
entrega(gorro, empty, empty,          date(2021,10, 7)/time(1,34,0),  empty/empty, empty).
entrega(folha, empty, empty,          date(2021,2, 3)/time(11,30,0),  empty/empty, empty).
entrega(portao, empty, empty,         date(2021,3, 10)/time(6,30,0),  empty/empty, empty).
entrega(sal, empty, empty,            date(2021,12, 23)/time(7,54,0), empty/empty, empty).
entrega(joelheiras, empty, empty,     date(2021,10, 4)/time(1,30,0),  empty/empty, empty).
entrega(camara, empty, empty,         date(2022,1,4)/time(22,34,0),   empty/empty, empty).
entrega(comando, empty, empty,        date(2022,1,4)/time(22,34,0),   empty/empty, empty).
entrega(estojo, empty, empty,         date(2022,1,4)/time(22,34,0),   empty/empty, empty).
entrega(hub, empty, empty,            date(2022,1,4)/time(22,34,0),   empty/empty, empty).
entrega(calculadora, empty, empty,    date(2022,1,4)/time(22,34,0),   empty/empty, empty).

entrega(corta_unhas, ups, bicicleta,  date(2021,10, 4)/time(1,30,0),  empty/empty, empty).
entrega(queijo, ctt, mota,            date(2021, 1,14)/time(2,10,0),  empty/empty, empty).
entrega(moto4, ctt, carro,            date(2021, 3,24)/time(3,20,0),  empty/empty, empty).
entrega(jetski, ups, carro,           date(2021, 2,25)/time(5,30,0),  empty/empty, empty).
entrega(tabuleiro, chronopost, mota,  date(2021, 6, 7)/time(7,0,0),   empty/empty, empty).
entrega(oculos, amazon, carro,        date(2021, 7,10)/time(3,15,0),  empty/empty, empty).

entrega(luvas, ups, bicicleta,       date(2021,10, 4)/time(1,30,0),  date(2021,10, 4)/time(0,0,0),   3.2).
entrega(boqueira, ctt, mota,         date(2021, 1,14)/time(2,10,0),  date(2021, 1,14)/time(3,0,0),   5.0).
entrega(disquete, ctt, carro,        date(2021, 3,24)/time(3,20,0),  date(2021, 3,24)/time(4,0,0),   2.4).
entrega(granito, ups, carro,         date(2021, 2,25)/time(5,30,0),  date(2021, 2,25)/time(6,0,0),   3.8).
entrega(isqueiro, chronopost, mota,  date(2021, 6, 7)/time(7,0,0),   date(2021, 6, 7)/time(17,0,0),  4.8).
entrega(tricornio, amazon, carro,    date(2021, 7,10)/time(3,15,0),  date(2021, 7,10)/time(3,20,0),  4.9).
entrega(porta, amazon, bicicleta,    date(2021, 7,16)/time(10,5,0),  date(2021, 7,16)/time(10,50,0), 1.4).
entrega(tesoura, ups, bicicleta,     date(2021, 5,15)/time(1,40,0),  date(2021, 5,15)/time(20,0,0),  4.1).
entrega(loureiro, ctt, carro,        date(2021, 5, 7)/time(8,40,0),  date(2021, 5, 7)/time(22,0,0),  3.6).
entrega(marmore, chronopost, carro,  date(2021, 4, 9)/time(12,5,0),  date(2021, 4, 9)/time(23,0,0),  4.6).
entrega(flor, ups, mota,             date(2021, 1, 4)/time(16,5,0),  date(2021, 1, 14)/time(1,0,0),   3.9).
entrega(capa, ctt, carro,            date(2021, 2, 2)/time(7,5,0),   date(2021, 2, 8)/time(0,0,0),   4.4).

% ---------------------------------------------------------




% ------------------FASE 2 -----------------------------------
% todos os caminhos sãp ambidirecionados
grafo(x1,  x2,  1.5).
grafo(x1,  x3,  3.1).
grafo(x2,  x12, 1).
grafo(x2,  x4,  4).
grafo(x3,  x4,  2).
grafo(x12, x11, 4).
grafo(x12, x9,  1.5).
grafo(x4,  x8,  2).
grafo(x9,  x11, 6).
grafo(x9,  x8,  1.5).
grafo(x8,  x7,  1.2).
grafo(x8,  x6,  4).
grafo(x6,  x5,  1.5).
grafo(x6,  x7,  2.2).
grafo(x10, x11, 3).
grafo(x10, x7,  4).

localizacao(x1,  casteloes/rua_ponte_santa).
localizacao(x2,  casteloes/green_distribution).
localizacao(x3,  landim/rua_nossa_senhora).
localizacao(x4,  landim/rua_ponte).
localizacao(x5,  escordo/rua_jesuita).
localizacao(x6,  escordo/rua_sapos).
localizacao(x7,  escordo/rua_feliz).
localizacao(x8,  escordo/alameda_joao).
localizacao(x9,  guimaraes/rua_25abril).
localizacao(x10, guimaraes/rua_ponte).
localizacao(x11, guimaraes/rua_brito).
localizacao(x12, guimaraes/avenida_brasil).


% --- ESTIMAS -----------------------
estima(x1,  x1, 0).
estima(x2,  x1, 1.1).
estima(x3,  x1, 2.6).
estima(x4,  x1, 3.5).   
estima(x5,  x1, 3.8).
estima(x6,  x1, 4.8).
estima(x7,  x1, 4.5).
estima(x8,  x1, 3.4).
estima(x9,  x1, 3).
estima(x10, x1, 3.6).
estima(x11, x1, 2.8).
estima(x12, x1, 2).
% -----------------
estima(x2,  x2, 0).
estima(x3,  x2, 0.8).
estima(x4,  x2, 3).
estima(x5,  x2, 3.8).
estima(x6,  x2, 4.2).
estima(x7,  x2, 3.5).
estima(x8,  x2, 2).
estima(x9,  x2, 1.5).
estima(x10, x2, 3.5).
estima(x11, x2, 1).
estima(x12, x2, 0.9).
% -----------------
estima(x3,  x3, 0).
estima(x4,  x3, 1.7).
estima(x5,  x3, 3.5).
estima(x6,  x3, 5).
estima(x7,  x3, 4.5).
estima(x8,  x3, 3).
estima(x9,  x3, 4.3).
estima(x10, x3, 4.8).
estima(x11, x3, 5.6).
estima(x12, x3, 3.3).
% -----------------
estima(x4,  x4, 0).
estima(x5,  x4, 1).
estima(x6,  x4, 2.7).
estima(x7,  x4, 2.8).
estima(x8,  x4, 1.8).
estima(x9,  x4, 3).
estima(x10, x4, 3.5).
estima(x11, x4, 5).
estima(x12, x4, 3.2).
% -----------------
estima(x5,  x5, 0).
estima(x6,  x5, 1.2).
estima(x7,  x5, 2).
estima(x8,  x5, 1.7).
estima(x9,  x5, 2.7).
estima(x10, x5, 3.8).
estima(x11, x5, 4.9).
estima(x12, x5, 3.4).
% -----------------
estima(x6,  x6, 0).
estima(x7,  x6, 1.7).
estima(x8,  x6, 2).
estima(x9,  x6, 3.5).
estima(x10, x6, 4).
estima(x11, x6, 5.2).
estima(x12, x6, 4.1).
% -----------------
estima(x7,  x7, 0).
estima(x8,  x7, 1).
estima(x9,  x7, 1.3).
estima(x10, x7, 2.1).
estima(x11, x7, 3.4).
estima(x12, x7, 2.5).
% -----------------
estima(x8,  x8, 0).
estima(x9,  x8, 0.9).
estima(x10, x8, 1.5).
estima(x11, x8, 3.5).
estima(x12, x8, 2).
% -----------------
estima(x9,  x9, 0).
estima(x10, x9, 0.6).
estima(x11, x9, 2).
estima(x12, x9, 0.5).
% -----------------
estima(x10, x10, 0).
estima(x11, x10, 1.7).
estima(x12, x10, 1.2).
% -----------------
estima(x11, x11, 0).
estima(x12, x11, 1.4).
% -----------------
estima(x12, x12, 0).
% ------------------------------------------------

%circuito(Caminho,Encomendas,Peso,Volume).
circuito([x2,x4,x2], [luvas], 2, 10).
circuito([x2,x1,x2], [fixe],  4 ,15).
circuito([x2,x3,x2], [ola],  8 ,15).

drag(bicicleta, 0.7).
drag(mota,      0.5).
drag(carro,     0.1).