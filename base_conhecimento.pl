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
veiculo(mota, 35, 20).
veiculo(carro, 25, 100). 

preco(bicicleta,0.5).
preco(mota,1).
preco(carro,1.5).

% ----------- cliente(id, nrEncomendas) /2

cliente(daniel, 3).
cliente(joao, 3).
cliente(caldas, 3).
cliente(abacao, 3).
cliente(joana, 3).
cliente(freitas, 3).
cliente(cunha, 3).
cliente(jota, 3).
cliente(claudia, 3).
cliente(antonio,0).

% ----------- estafeta(id, nrEntregas, avaliacao) /3

estafeta(ups, 1, 4.4).
estafeta(chronopost, 3, 4.4).
estafeta(ctt,2,4.4).
estafeta(amazon,0,0).

% ----------- encomenda(Estado, Id, IdClinet, Peso, Volume, Freguesia/Morada, prazo) /7

encomenda(registada, cadeira, daniel, 2 ,30, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(registada, gorro, abacao, 1, 10, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(registada, folha, daniel, 50 ,10, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(registada, portal, daniel, 16 ,30, escordo/rua_ponte,date(0,0,1)/time(12,0,0)).
encomenda(registada, sal, abacao, 2 ,30, escordo/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(registada, joelheiras, abacao, 80 ,30, escordo/rua_ponte,date(0,0,0)/time(12,0,0)).

encomenda(distribuicao, corta_unhas, daniel, 2 ,30, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(distribuicao, queijo, abacao, 1, 10, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(distribuicao, moto4, daniel, 50 ,10, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(distribuicao, jetski, daniel, 16 ,30, escordo/rua_ponte,date(0,0,1)/time(12,0,0)).
encomenda(distribuicao, tabuleiro, abacao, 2 ,30, escordo/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(distribuicao, oculos, abacao, 80 ,30, escordo/rua_ponte,date(0,0,0)/time(12,0,0)).

encomenda(entregue, luvas, abacao, 1, 10, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, boqueira, daniel, 2 ,30, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, disquete, daniel, 50 ,10, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, granito, daniel, 16 ,30, escordo/rua_ponte,date(0,0,1)/time(12,0,0)).
encomenda(entregue, isqueiro, abacao, 2 ,30, escordo/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, tricornio, abacao, 80 ,30, escordo/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, porta, abacao, 1, 10, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, tesoura, daniel, 2 ,30, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, , daniel, 50 ,10, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, granito, daniel, 16 ,30, escordo/rua_ponte,date(0,0,1)/time(12,0,0)).
encomenda(entregue, isqueiro, abacao, 2 ,30, escordo/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, tricornio, abacao, 80 ,30, escordo/rua_ponte,date(0,0,0)/time(12,0,0)).
 


% ----------- entrega(idEncomenda, idEstafeta, veiculo, DataInicio, DataFim, avaliacao) /6
entrega(cadeira_gayming, caldas, bicicleta, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(cadeira, ctt, mota, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(folha, ctt, bicicleta, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(portal, joao, carro, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(sal, caldas, carro, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(joelhos, ctt, mota, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).



