:- op( 900,xfy,'::' ).

%:- dynamic(veiculo/3).
:- dynamic(estafeta/3).
:- dynamic(cliente/2).
:- dynamic(encomenda/9).
:- dynamic(entrega/6).



%escrever factos de estafetas, clientes e encomendas para termos já alguns na BD

% Factos:
% ----------- veiculo(tipo, velocidadeMedia, cargaMax)
veiculo(bicicleta, 10, 5). %inserir preço por km ?? ou só na função do custo??
veiculo(mota, 35, 20).
veiculo(carro, 25, 100). 

% ----------- cliente(id, nrEncomendas) /2
cliente(daniel, 3).
cliente(abacao, 3).

% ----------- estafeta(id, nrEntregas, avaliacao) /3
estafeta(joao, 1, 4.4).
estafeta(caldas, 3, 4.4).
estafeta(ctt,2,4.4).

% (3:2:2) -> prazo 3dias 2h 2min 
% encomenda(Estado, Id, Peso, Volume, Freguesia/Morada, prazo)
% ----------- encomenda(estado, id, idCliente, peso, volume, freguesia, morada, prazo -> (dias:horas:minutos)) /9
encomenda(entregue, cadeira_gayming, abacao, 1, 10, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, cadeira, daniel, 2 ,30, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, folha, daniel, 50 ,10, landim/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, portal, daniel, 16 ,30, escordo/rua_ponte,date(0,0,1)/time(12,0,0)).
encomenda(entregue, sal, abacao, 2 ,30, escordo/rua_ponte,date(0,0,0)/time(12,0,0)).
encomenda(entregue, joelhos, abacao, 80 ,30, escordo/rua_ponte,date(0,0,0)/time(12,0,0)).
% 
% ----------- entrega(idEncomenda, idEstafeta, veiculo, DataInicio, DataFim, avaliacao) /6
entrega(cadeira_gayming, caldas, mota, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(cadeira, ctt, mota, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(folha, ctt, mota, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(portal, joao, mota, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(sal, caldas, mota, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(joelhos, ctt, mota, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).


solucoes(X, Y, Z) :- findall(X, Y, Z).


% Invariante Estrutural:  nao permitir a insercao de conhecimento repetido

+estafeta(Id, A, B) :: (solucoes( Id , estafeta(Id, A, B) ,S ),
                        length( S,N ), N == 1 ).
        
+estafeta(Id, A, B) :: (solucoes( A , estafeta(Id, A, B) ,S ),
                A >= 0 ).


+cliente(Id, A) :: (solucoes( Id , cliente(Id, A) ,S ),
                        length( S,N ), N == 1 ).
+encomenda(A, Id, B, C, D, E, F, G, H) :: (solucoes( Id , encomenda(A, Id, B, C, D, E, F, G, H) ,S ),
                        length( S,N ), N == 1 ).
+cliente(Id, A, B, C, D, E) :: (solucoes( Id , cliente(Id, A, B, C, D, E) ,S ),
                        length( S,N ), N == 1 ).


% estados: 1->registada, 2->distribuição, 3->entregue
% prazos de entrega (imediato, 2h, 6h, 1 dia, etc).


% Evolução do conhecimento
evolucao( Termo ) :- findall(Invariante, +Termo::Invariante, Lista),
                     insercao(Termo),
                     teste(Lista).

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo), !, fail.

teste([]).
teste([R|LR]) :- R, teste(LR).

adicionaParEstafeta( [] , L , [L]).
adicionaParEstafeta( [(Est,L)|T], (Est, Enc), [(Est,L2)|T] ) :- append(L,[Enc],L2).
adicionaParEstafeta([(H,H2)|T1],L,[(H,H2)|T3]) :- adicionaParEstafeta(T1,L,T3).


ajuda(geral, "escrever todos os comandos de ajuda aqui").
% mudar esta parte
ajuda(encomenda, "Usar a funcao createEncomenda() com os parâmetros: 
                Id, IdCliente, IdEstafeta, Peso, Volume, Freguesia, 
                Morada, Prazo, DataInicio, DataFim, Avaliacao").

help(X) :- ajuda(X, Y), write(Y). % tentar tirar o true que aparece depois


createEstafeta(Id) :- evolucao(estafeta(Id,0,0)).

addEstafeta(Id,Av) :- estafeta(Id,Nr,Media), NewNr is Nr + 1, NewMedia is (Media*Nr + Av)/NewNr,
                      replace_existing_fact(estafeta(Id,Nr,Media),estafeta(Id,NewNr,NewMedia)).

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


% --------------------------------------- identificar os clientes servidos por um determinado estafeta;
findClientesServidosPorEstafeta(IdEstafeta, Answer) :- 
                estafeta(IdEstafeta, Nr, _), Nr == 0, Answer = [], write(IdEstafeta),write(" ainda não realizou entregas"), !;
                findall(IdEnc, entrega(IdEnc, IdEstafeta, _, _, _, _), Aux),
                clientesPorEncomenda(Aux,[],Answer).

clientesPorEncomenda([],L,L).
clientesPorEncomenda([Id|T],L,Answer) :- encomenda(_,Id,IdCliente,_,_,_,_), \+member(IdCliente,L), !,
                                         append([IdCliente],L,L2), clientesPorEncomenda(T,L2,Answer).
clientesPorEncomenda([H|T],L,Answer) :- clientesPorEncomenda(T,L,Answer).

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
pesoNumDia(IdEstafeta, Dia/Mes/Ano, Answer) :- findall(IdEnc,
                                                (entrega(IdEnc,IdEstafeta,_,Di/_,Df/_,_), 
                                                dateStamp(Di,date(Ano,Mes,Dia),Dif1), Dif1 >= 0,
                                                dateStamp(Df,date(Ano,Mes,Dia),Dif2), Dif2 =< 0),
                                                Ans), calcPesoEnc(Ans,0,Answer).

calcPesoEnc([],Acc,Acc).
calcPesoEnc([IdEnc|T],Acc,R) :- encomenda(_,IdEnc, _, Peso, _, _ ,_), Acc2 is Acc + Peso, calcPesoEnc(T,Acc2,R).


replace_existing_fact(OldFact, NewFact) :-
    retract(OldFact),
    assert(NewFact).


dateStamp(DateI, DateF, Days) :-
        date_time_stamp(DateI, TimeStamp1), 
        date_time_stamp(DateF, TimeStamp2),
        Days is (TimeStamp2 - TimeStamp1)/86400.