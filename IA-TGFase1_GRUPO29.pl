:- op( 900,xfy,'::' ).

:- dynamic(veiculo/3).
:- dynamic(estafeta/3).
:- dynamic(cliente/2).
:- dynamic(encomenda/9).
:- dynamic(entrega/6).




% usar ficha 8 limitações ao nr de predicatos adicionados



% assert -> adiciona predicados


%escrever factos de estafetas, clientes e encomendas para termos já alguns na BD

% Factos:
% ----------- veiculo(tipo, velocidadeMedia, cargaMax)
veiculo(bicicleta, 10, 5). %inserir preço por km ?? ou só na função do custo??
veiculo(mota, 35, 20).
veiculo(carro, 25, 100). 

cliente(daniel, 0).
cliente(abacao, 3).

estafeta(joao, 0, 0).
estafeta(caldas, 1, 4.3).  

encomenda(entregue, cadeira_gayming, abacao, caldas, 1, 10, landim, rua_ponte, "3 dias").
entrega(cadeira_gayming, caldas, mota, cinco_outubro, seis_outubro, 4.4).
entrega(cadeira_gayming, caldas, mota, cinco_outubro, seis_outubro, 4.5).
entrega(cadeira_gayming, caldas, mota, cinco_outubro, seis_outubro, 4.6).
entrega(cadeira_gayming, caldas, mota, cinco_outubro, seis_outubro, 4.7).
entrega(cadeira_gayming, caldas, mota, cinco_outubro, seis_outubro, 4.8).
entrega(cadeira_gayming, caldas, mota, cinco_outubro, seis_outubro, 4.9).


solucoes(X, Y, Z) :- findall(X, Y, Z).


% Invariante Estrutural:  nao permitir a insercao de conhecimento repetido
% fazer insert veiculo????
+veiculo(Name, A, B) :: (solucoes( (Name, A, B),(veiculo(Name, A, B)),S ),
                        length( S,N ), N == 1 ).
+estafeta(Id, A, B) :: (solucoes( (Id, A, B),(estafeta(Id, A, B)),S ),
                        length( S,N ), N == 1 ).
+cliente(Id, A) :: (solucoes( (Id,A),(cliente(Id, A)),S ),
                        length( S,N ), N == 1 ).
+encomenda(A, Id, B, C, D, E, F, G, H) :: (solucoes( (A, Id, B, C, D, E, F, G, H),(encomenda(A, Id, B, C, D, E, F, G, H)),S ),
                        length( S,N ), N == 1 ).
+cliente(Id, A, B, C, D, E) :: (solucoes( (Id, A, B, C, D, E),(cliente(Id, A, B, C, D, E)),S ),
                        length( S,N ), N == 1 ).


% estados: 1->registada, 2->distribuição, 3->entregue
% prazos de entrega (imediato, 2h, 6h, 1 dia, etc).

% ----------- encomenda(estado, id, idCliente, idEstafeta, peso, volume, freguesia, morada, prazo) /9
% ----------- estafeta(id, nrEntregas, avaliacao) /3
% ----------- cliente(id, nrEncomendas) /2
% ----------- entrega(idEncomenda, idEstafeta, veiculo, DataInicio, DataFim, avaliacao) /6


ajuda(geral, "escrever todos os comandos de ajuda aqui").
% mudar esta parte
ajuda(encomenda, "Usar a funcao createEncomenda() com os parâmetros: 
                Id, IdCliente, IdEstafeta, Peso, Volume, Freguesia, 
                Morada, Prazo, DataInicio, DataFim, Avaliacao").

help(X) :- ajuda(X, Y), write(Y). % tentar tirar o true que aparece depois




%createEstafeta(Id) :- \+estafeta(Id, _, _), assert(estafeta(Id, 0, 0)),  write("Estafeta adicionado"), !;
%                        write("Estafeta já existente").
createEstafeta(Id) :- \+estafeta(Id, _, _), assert(estafeta(Id, 0, 0)),  write("Estafeta adicionado"), !;
                        write("Estafeta já existente").


createCliente(Id) :- \+cliente(Id, _), assert(cliente(Id, 0)), write("Cliente adicionado"), !;
                        write("Cliente já existente").



                %fazer os teste de id da encomenda e assim antes, esxiste id cliente, estafeta e assim
createEncomenda(Id, IdCliente, Peso, Volume, Freguesia, Morada, Prazo) :- 
                    encomenda(_, Id, _, _, _, _, _, _, _ ), write("Encomenda já existente"), !;
                    \+cliente(IdCliente, _), write("Cliente não existente"), !;
                    Peso > 100, write("Nenhum veiculo suporta a entrega da encomenda"), !; 
                    assert(encomenda(registada, Id, IdCliente, empty, Peso, Volume, Freguesia, Morada, Prazo)).




% só deve ser feita pelo sistema da empresa, é preciso certificar outra vez os campos??
createEntrega(IdEncomenda, IdEstafeta, Veiculo, Inicio, Prazo) :-
                \+encomenda(_, IdEncomenda, _, _, _, _, _, _, _), write("Encomenda não existente"), !;
                \+estafeta(IdEstafeta, _, _), write("Estafeta não existente"), !;
                encomenda(entregue, IdEncomenda, _, _, _, _, _, _, _), write("A encomenda já foi entregue"), !;
                encomenda(distribuicao, IdEncomenda, _, _, _, _, _, _, _), write("A encomenda já se encontra em distribuição"), !;
                
                encomenda(_, IdEncomenda, _, _, Peso, _, _, _, _ ), veiculo(Veiculo, _, Max), 
                            Max < Peso, write("Veículo selecionado não suporta carga da encomenda"), !;
                % adicionar info aos outros factos !!!!
                replace_existing_fact(encomenda(_, IdEncomenda, IdCliente, IdEstafeta, Peso, Volume, Freguesia, Morada, Prazo), 
                                    encomenda(registada, IdEncomenda, IdCliente, IdEstafeta, Peso, Volume, Freguesia, Morada, Prazo)), 
                        assert(entrega(IdEncomenda, IdEstafeta, Veiculo, Inicio, empty, -1 )), 
                        write("Entrega registada").


entregarEncomenda(IdEncomenda, DataFim, Avaliacao) :-
                    \+encomenda(_, IdEncomenda, _, _, _, _, _, _, _), write("Encomenda não existente"), !;
                    encomenda(entregue, IdEncomenda, _, _, _, _, _, _, _), write("A encomenda já foi entregue"), !;
                    encomenda(registada, IdEncomenda, _, _, _, _, _, _, _), write("A encomenda ainda não está em distribuição"), !;

                    replace_existing_fact(encomenda(_, IdEncomenda, IdCliente, IdEstafeta, Peso, Volume, Freguesia, Morada, Prazo), 
                                        encomenda(entregue, IdEncomenda, IdCliente, IdEstafeta, Peso, Volume, Freguesia, Morada, Prazo)), 
                    replace_existing_fact(entrega(IdEncomenda, IdEstafeta, Veiculo, Volume, DataInicio, _, Avaliacao), 
                                        entrega(IdEncomenda, IdEstafeta, Veiculo, Volume, DataInicio, DataFim, Avaliacao)), 
                    replace_existing_fact(estafeta(IdEstafeta, EncomendasAnt, Rating), estafeta(IdEstafeta, EncomendasAnt + 1, Rating)). %alterar Rating, ver penalização
                    

%

% fazer
% calculaPreco(Veiculo, Distancia, TempoReserva, Valor) :- Valor is 4.






% --------------------------------------- identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico; só falta testar
findEstafetasPorVeiculo(Veiculo, Bag) :-
                        setof(IdEstafeta, entrega(_, IdEstafeta, Veiculo, _, _, _), Bag). % Bag possui lista de estafetas que usaram o veiculo em analise

estafetaQuatasVezesVeiculo(Veiculo, Estafeta, Nr):-
                        findall(1, entrega(_, Estafeta, Veiculo, _, _, _), Bag), length(Bag, Nr).  % DUVIDA : é assim que se encontra todos??

calcularEstafetaQueMaisUsouVeiculo(_, [], _, _). % podemos fazer isto sendo que Answer devia lá estar??
calcularEstafetaQueMaisUsouVeiculo(Veiculo, [BagHead | Tail], Max, _) :- %esta mal
                        estafetaQuatasVezesVeiculo(Veiculo, BagHead, Nr), 
                        Nr>Max, calcularEstafetaQueMaisUsouVeiculo(Veiculo, Tail, Nr, BagHead). % se for por percentagem alterar aqui a comparação
calcularEstafetaQueMaisUsouVeiculo(Veiculo, [_ | Tail], Max, Answer) :-
                        calcularEstafetaQueMaisUsouVeiculo(Veiculo, Tail, Max, Answer).
        
maisEcologico(Veiculo, Answer) :-% se for só bicicleta cagar no 'Veiculo'
            findEstafetasPorVeiculo(Veiculo, Bag), calcularEstafetaQueMaisUsouVeiculo(Veiculo, Bag, 0, Answer).
% ---------------------------------------


% --------------------------------------- identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente;
% cliente entra onde ???? DUVIDA -> basiscamente é a reverse da de baixo, atraves do cliente determina estafetas que o serviram
trackEncomenda(Id) :- \+encomenda(_, Id, _, _, _, _, _, _, _), write("Encomenda não existente"), !;
                        encomenda(entregue, Id, _, IdEstafeta, _, _, _, _, _),
                                write("A encomenda já foi entregue por: "), write(IdEstafeta), !;
                        encomenda(registada, Id, _, _, _, _, _, _, _),
                                write("A encomenda está registada, dentro de momentos irá ser distribuída"), !;
                        encomenda(distribuicao, Id, _, IdEstafeta, _, _, _, _, _),
                                write("A encomenda encontra-se em distribuição pelo estafeta: "), write(IdEstafeta).
% ---------------------------------------


% esta mal
% --------------------------------------- identificar os clientes servidos por um determinado estafeta;
findClientesServidosPorEstafeta(IdEstafeta, Answer) :- % resolver para quando nao existe, 
                estafeta(IdEstafeta, Nr, _), Nr =:= 0, Answer = "O estafeta ainda não realizou entregas", !;
                setof(IdCliente, encomenda(entregue, _, IdCliente, IdEstafeta, _, _, _, _, _), Answer).
% ---------------------------------------


% --------------------------------------- calcular o valor faturado pela Green Distribution num determinado dia;

% ---------------------------------------


% ----------- encomenda(estado, id, idCliente, idEstafeta, peso, volume, freguesia, morada, prazo) /9
% --------------------------------------- identificar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution;
%bestZonas() :- findall(Freguesia, encomenda(_, _, _, _, _, _, Freguesia, _, _), Bag),
%                calculaTop(Bag, Top1, Top2, Top3).


%calculaTop(Bag, Top1, Top2, Top3) :-
%                countall(Bag, Elem, Count). % fazer cenas com Elem e Count
            
            


count([],_,0).
count([Elem|Tail],Elem,Y):- count(Tail,Elem,Z), Y is 1+Z.
count([Elem1|Tail],Elem,Z):- Elem1\=Elem,count(Tail,Elem,Z).

countall(List,Elem,Count) :-
        %N1 is 0, N2 is 0, N3 is 0,
        sort(List,List1),
        member(Elem,List1),
        count(List,Elem,Count).


% ---------------------------------------

% --------------------------------------- calcular a classificação média de satisfação de cliente para um determinado estafeta;
calcularMediaSatisfacaoEstafeta(IdEstafeta, Answer) :- 
                    estafeta(IdEstafeta, Nr, _), Nr =:= 0, Answer = "O estafeta ainda não realizou entregas", !;
                    findall(Avaliacao, entrega( _, IdEstafeta, _, _, _, Avaliacao), X), 
                    validarAvaliacao(X, NewX), averageList(NewX, Val), arredondar(Val, Answer, 2).


arredondar(X,Y,D) :- Z is X * 10^D, round(Z, ZA), Y is ZA / 10^D.

validarAvaliacao([], []).
validarAvaliacao([Head|Tail], List) :- 
            (Head < 0 -> List = Output ;  List = [Head|Output] ), 
            validarAvaliacao(Tail, Output).

listSum( [], 0 ).
listSum( [Head|Tail], Sum ) :-
        listSum( Tail, Temp ),
        Sum is Temp + Head.

averageList( List, Avg ) :-
        listSum( List, Sum ),
        length( List, Length ),
        Avg is Sum / Length.
% ---------------------------------------


% --------------------------------------- identificar o número total de entregas pelos diferentes meios de transporte,num determinado intervalo de tempo;

% ---------------------------------------


% --------------------------------------- identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo;

% ---------------------------------------


% --------------------------------------- calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo;

% ---------------------------------------


% --------------------------------------- calcular o peso total transportado por estafeta num determinado dia.
%pesoNumDia(IdEstafeta, Dia Answer) :- findall(Peso, ).
% ---------------------------------------









% write($X).





replace_existing_fact(OldFact, NewFact) :-
    call(OldFact),
    retract(OldFact),
    assertz(NewFact).
