




assert -> adiciona predicados




% Factos:
% ----------- veiculo(tipo, velocidadeMedia, cargaMax)
veiculo(bicicleta, 10, 5).
veiculo(mota, 35, 20).
veiculo(carro, 25, 100). 


% ----------- entrega(id, peso, volume, freguesia, morada, prazo, dataInicio, dataFim, cliente, estafeta, avaliacao)




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