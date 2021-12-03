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
cliente(joana,   1).
cliente(freitas, 4).
cliente(claudia, 2).

% ----------- estafeta(id, nrEntregas, avaliacao) /3

estafeta(ups,        4, 3.5).
estafeta(chronopost, 2, 4.7).
estafeta(ctt,        4, 3.85).
estafeta(amazon,     2, 3.16 ).

% ----------- encomenda(Estado, Id, IdClinet, Peso, Volume, Freguesia/Morada, prazo) /7

encomenda(registada, cadeira, daniel,    2 ,30,  landim/rua_ponte,  date(0,0,0)/time(12,0,0)).
encomenda(registada, gorro, joao,        1, 10,  landim/rua_ponte,  date(0,0,0)/time(12,0,0)).
encomenda(registada, folha, daniel,      0.1 ,5, landim/rua_ponte,  date(0,0,0)/time(12,0,0)).
encomenda(registada, portao, freitas,    16 ,30, escordo/rua_ponte, date(0,0,1)/time(12,0,0)).
encomenda(registada, sal, abacao,        2 ,30,  escordo/rua_ponte, date(0,0,0)/time(12,0,0)).
encomenda(registada, joelheiras, abacao, 80 ,30, escordo/rua_ponte, date(0,0,0)/time(12,0,0)).

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
