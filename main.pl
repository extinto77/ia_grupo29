




% assert -> adiciona predicados


%escrever factos de estafetas, clientes e encomendas para termos já alguns na BD

% Factos:
% ----------- veiculo(tipo, velocidadeMedia, cargaMax)
veiculo(bicicleta, 10, 5).
veiculo(mota, 35, 20).
veiculo(carro, 25, 100). 

cliente(mauricio, 0).
cliente(sebastiao, 3).

estafeta(merdas, 0, 0).
estafeta(burro, 5, 4.3).


% estados: 1->registada, 2->em distribuição, 3->entregue
% , DataInicio, DataFim, Avaliacao

% ----------- encomenda(estado, id, idCliente, idEstafeta, peso, volume, freguesia, morada, prazo)
% ----------- estafeta(id, nrEntregas, avaliacao)
% ----------- cliente(id, nrEncomendas)
% ----------- entrega(idEncomenda, idEstafeta, veiculo, DataInicio, DataFim, avaliacao)


ajuda(geral, "escrever todos os comandos de ajuda aqui").
% mudar esta parte
ajuda(encomenda, "Usar a funcao createEncomenda() com os parâmetros: 
                Id, IdCliente, IdEstafeta, Peso, Volume, Freguesia, 
                Morada, Prazo, DataInicio, DataFim, Avaliacao").

help(X) :- ajuda(X, Y), write(Y). % tentar tirar o true que aparece depois




createEstafeta(Id) :- \+estafeta(Id, _, _), dynamic(estafeta/3), assert(estafeta(Id, 0, 0)) write("Estafeta adicionado"), !;
                        write("Estafeta já existente")


createCliente(Id) :- \+cliente(Id, _), dynamic(cliente/2), assert(cliente(Id, 0)), write("Cliente adicionado"), !;
                        write("Cliente já existente").



                %fazer os teste de id da encomenda e assim antes, esxiste id cliente, estafeta e assim
createEncomenda(Id, IdCliente, Peso, Volume, Freguesia, 
                Morada, Prazo) :- 
                    encomenda(_, Id, _, _, _, _, _, _, _, _, _ ), write("Encomenda já existente"), !; % verificar nr argumentos
                    \+encomenda(_, _, IdCliente, _, _, _, _, _, _, _, _ ), write("Cliente não existente"), !; % verificar nr argumentos
                    \+encomenda(_, _, _, IdEstafeta, _, _, _, _, _, _, _ ), write("Estafeta não existente"), !; % verificar nr argumentos
                    dynamic(encomenda/9), assert(encomenda(registada, Id, IdCliente, empty, Peso, Volume, Freguesia, Morada, Prazo)).

createEntrega(IdEncomenda, IdEstafeta, Veiculo, Inicio, Prazo) :-
                \+encomenda(_, IdEncomenda, _, _, _, _, _, _, _), write(Encomenda não existente), !;
                \+encomenda(_, _, _, IdEstafeta, _, _, _, _, _), write(Estafeta não existente), !;
                encomenda(entregue, IdEncomenda, _, _, _, _, _, _, _), write("A encomenda já foi entregue"), !;
                encomenda(distribuicao, IdEncomenda, _, _, _, _, _, _, _), write("A encomenda já se encontra em distribuição"), !;
                veiculo(Veiculo, _, Max), Max < Peso, write("Veículo selecionado não suporta carga da encomenda"), !;

                replace_existing_fact(encomenda(_, IdEncomenda, IdCliente, IdEstafeta, Peso, Volume, Freguesia, Morada, Prazo), 
                                    encomenda(registada, IdEncomenda, IdCliente, IdEstafeta, Peso, Volume, Freguesia, Morada, Prazo)), 
                        dynamic(entrega/6), assert(entrega(IdEncomenda, IdEstafeta, Veiculo, Inicio, empty, -1 )), 
                        write("Entrega registada").


entregarEncomenda(IdEncomenda, DataFim, Avaliacao) :-
                    \+encomenda(_, IdEncomenda, _, _, _, _, _, _, _), write(Encomenda não existente), !;
                    encomenda(entregue, IdEncomenda, _, _, _, _, _, _, _), write("A encomenda já foi entregue"), !;
                    encomenda(registada, IdEncomenda, _, _, _, _, _, _, _), write("A encomenda ainda não está em distribuição"), !;

                    replace_existing_fact(encomenda(_, IdEncomenda, IdCliente, IdEstafeta, Peso, Volume, Freguesia, Morada, Prazo), 
                                        encomenda(entregue, IdEncomenda, IdCliente, IdEstafeta, Peso, Volume, Freguesia, Morada, Prazo)), 
                    replace_existing_fact(entrega(IdEncomenda, IdEstafeta, Veiculo, Volume, DataInicio, _, Avaliacao), 
                                        entrega(IdEncomenda, IdEstafeta, Veiculo, Volume, DataInicio, DataFim, Avaliacao)), 
                    replace_existing_fact(estafeta(IdEstafeta, EncomendasAnt, Rating), estafeta(IdEstafeta, EncomendasAnt + 1, Rating)). %alterar Rating, ver penalização
                    













replace_existing_fact(OldFact, NewFact) :-
    call(OldFact),
    retract(OldFact),
    assertz(NewFact).


% evitar duplicação de entregas?? ter lista de entregas com id
% -------- greenDistribution(estafetas<estafetas>, clientes<clente>)



% -------- estafeta(porEntregar<entrega>, rating)




% ------ entrega(peso, volume, freguesia, morada, prazo, dataInicio, dataFim, cliente, estafeta, avaliacao)



%--------- cliente(listaEncomendas<entrega>)






% -------fazer preço



% ----queries
% 1-> identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico;
% 2-> dentificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente;
% 3-> identificar os clientes servidos por um determinado estafeta;
% 4-> calcular o valor faturado pela Green Distribution num determinado dia;
% 5-> identificar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution;
% 6-> calcular a classificação media de satisfação de cliente para um determinado estafeta;
% 7-> identificar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo;
% 8-> identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo;
% 9-> calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo;
% 10-> calcular o peso total transportado por estafeta num determinado dia.
% 11-> ++++++