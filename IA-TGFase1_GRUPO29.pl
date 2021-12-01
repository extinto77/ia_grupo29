
:- op( 900,xfy,'::' ).

%:- dynamic(veiculo/3).
:- dynamic(estafeta/3).
:- dynamic(cliente/2).
:- dynamic(encomenda/7).
:- dynamic(entrega/6).



%escrever factos de estafetas, clientes e encomendas para termos já alguns na BD

% Factos:

validateTime(H1, M1, S1) :-
        H1<24, H1>=0, M1<60, M1>=0, S1<60, S1>=0.
%validateDate(A, M, D) .

timeStamp(time(H1, M1, S1), time(H2, M2, S2), Days):-
        TimeStamp1 is (H1 + (M1/60) + (S1/60)/60)/24,
        TimeStamp2 is (H2 + (M2/60) + (S2/60)/60)/24,
        Days is TimeStamp2 -TimeStamp1.

dateStamp(DateI, DateF, Days) :-
        date_time_stamp(DateI, TimeStamp1), 
        date_time_stamp(DateF, TimeStamp2),
        Days is (TimeStamp2 - TimeStamp1)/86400.

timeElapsed(DateI, TimeI, DateF, TimeF, Ans) :-
        dateStamp(DateI, DateF, X1), timeStamp(TimeI, TimeF, X2),
        Ans is X1 + X2.

% dizer que prazo máximo é de 30 dias
convertTime(Val, Date/Time) :-
        Val>0, rounding(Val, D, Dec1),
        rounding(Dec1*24, H, Dec2), 
        rounding(Dec2*60, M, _),
        Date = date(0, 0, D), Time = time(H, M, 0).

rounding(Val, Int, Decimal) :-
        (Val>1) -> (
                round(Val, Rounded), ((Val>Rounded)-> Int is Rounded; Int is Rounded-1), Decimal is Val-Int
        );
        Int is 0, Decimal is Val.



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
entrega(cadeira_gayming, caldas, bicicleta, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(cadeira, ctt, mota, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(folha, ctt, mota, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(portal, joao, carro, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(sal, caldas, carro, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
entrega(joelhos, ctt, mota, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).




% Invariante Estrutural:  nao permitir a insercao de conhecimento repetido

+estafeta(Id, A, B) :: (solucoes( Id , estafeta(Id, A, B) ,S ),
                        length( S,N ), N == 1 ).
        
+estafeta(Id, A, B) :: (solucoes( A , (estafeta(Id, A, B),A >=0) ,S ),
                        length(S,N), N == 1).


+cliente(Id, A) :: (solucoes( Id , cliente(Id, A) ,S ),
                        length( S,N ), N == 1 ).
+encomenda(A, Id, B, C, D, E, F, G, H) :: (solucoes( Id , encomenda(A, Id, B, C, D, E, F, G, H) ,S ),
                        length( S,N ), N == 1 ).
+cliente(Id, A, B, C, D, E) :: (solucoes( Id , cliente(Id, A, B, C, D, E) ,S ),
                        length( S,N ), N == 1 ).



solucoes(X, Y, Z) :- findall(X, Y, Z).


% estados: 1->registada, 2->distribuição, 3->entregue
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
                Id, IdCliente, Peso, Volume, Freguesia, Morada, Dias, Horas, Minutos sendo os últimos 3 parâmetros o prazo").

ajuda(estafeta, "Usar a funcao createEstafeta() com os parâmetros: 
                Id").

ajuda(cliente, "Usar a funcao createCliente() com os parâmetros: 
                Id").

help(X) :- ajuda(X, Y), write(Y). % tentar tirar o true que aparece depois


                        
createEstafeta(Id) :- \+estafeta(Id, _, _), assert(estafeta(Id, 0, 0)),  write("Estafeta adicionado"), !;
                        write("Estafeta já existente").

addEstafeta(Id,Av) :- estafeta(Id,Nr,Media), NewNr is Nr + 1, arredondar(((Media*Nr + Av)/NewNr), NewMedia, 2),
                      replace_existing_fact(estafeta(Id,Nr,Media),estafeta(Id,NewNr,NewMedia)).

createCliente(Id) :- \+cliente(Id, _), assert(cliente(Id, 0)), write("Cliente adicionado"), !;
                        write("Cliente já existente").



                %fazer os teste de id da encomenda e assim antes, esxiste id cliente, estafeta e assim
createEncomenda(Id, IdCliente, Peso, Volume, Freguesia, Morada, Dias, Horas, Minutos) :- 
                encomenda(_, Id, _, _, _, _, _), write("Encomenda já existente"), !;
                \+cliente(IdCliente, _), write("Cliente não existente"), !;
                Peso > 100, write("Nenhum veiculo suporta a entrega da encomenda"), !;
                Peso < 0, write("Peso impossível"), !;
                Volume < 0, write("Volume impossível"), !;
                (Dias < 0; Horas < 0; Minutos < 0), write("Prazo impossível"), !; 
                assert(encomenda(registada, Id, IdCliente, Peso, Volume, Freguesia/Morada, date(0,0,Dias)/time(Horas,Minutos,0))),
                addCliente(IdCliente).
                    
                    


% só deve ser feita pelo sistema da empresa, é preciso certificar outra vez os campos??
createEntrega(IdEncomenda, IdEstafeta, Veiculo, DateI, TimeI, Date ,Time) :-
                \+encomenda(_, IdEncomenda, _, _, _, _, _), write("Encomenda não existente"), !;
                \+estafeta(IdEstafeta, _, _), write("Estafeta não existente"), !;
                encomenda(entregue, IdEncomenda, _, _, _, _, _), write("A encomenda já foi entregue"), !;
                encomenda(distribuicao, IdEncomenda, _, _, _, _, _), write("A encomenda já se encontra em distribuição"), !;
                
                encomenda(_, IdEncomenda, _, _, Peso, _, _), veiculo(Veiculo, _, Max), 
                            Max < Peso, write("Veículo selecionado não suporta carga da encomenda"), !;
                % adicionar info aos outros factos !!!!
                replace_existing_fact(encomenda(registada, IdEncomenda, IdCliente, Peso, Volume, Freguesia/Morada, Date/Time), 
                                    encomenda(registada, IdEncomenda, IdCliente, Peso, Volume, Freguesia/Morada, Date/Time)), 
                        assert(entrega(IdEncomenda, IdEstafeta, Veiculo, DateI/TimeI, empty, -1 )), 
                        write("Entrega registada").

% encomenda(Estado, Id, Peso, Volume, Freguesia/Morada, prazo)
entregarEncomenda(IdEncomenda, Dia, Mes, Ano, Horas, Minutos, Avaliacao) :-
        \+encomenda(_, IdEncomenda, _, _, _, _, _), write("Encomenda não existente"), !;
        encomenda(entregue, IdEncomenda, _, _, _, _, _), write("A encomenda já foi entregue"), !;
        encomenda(registada, IdEncomenda, _, _, _, _, _), write("A encomenda ainda não está em distribuição"), !;
        replace_existing_fact(encomenda(distribuicao, IdEncomenda, IdCliente, Peso, Volume, ADDRS, Prazo), 
                                encomenda(entregue, IdEncomenda, IdCliente, Peso, Volume, ADDRS, Prazo)), 
        replace_existing_fact(entrega(IdEncomenda, IdEstafeta, Veiculo1, DataInicio1, _, _), 
                                entrega(IdEncomenda, IdEstafeta, Veiculo1, DataInicio1, 
                                        date(Ano, Mes, Dia)/time(Horas, Minutos, 0), Avaliacao)), 
        addEstafeta(IdEstafeta, Avaliacao). 
                        %ver penalização
        

%

% fazer
% calculaPreco(Veiculo, Distancia, TempoReserva, Valor) :- Valor is 4.






% --------------------------------------- identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico; só falta testar
findEstafetasPorVeiculo(Veiculo, Bag) :-
                        setof(IdEstafeta, entrega(_, IdEstafeta, Veiculo, _, _, _), Bag). % Bag possui lista de estafetas que usaram o veiculo em analise

estafetaQuantasVezesVeiculo(Veiculo, Estafeta, Nr):-
                        findall(arroz, entrega(_, Estafeta, Veiculo, _, _, _), Bag), length(Bag, Nr).  % DUVIDA : é assim que se encontra todos??

calcularEstafetaQueMaisUsouVeiculo(_, [], _, Ans).
calcularEstafetaQueMaisUsouVeiculo(Veiculo, [BagHead | Tail], Max, _) :- %esta mal
                        estafetaQuatasVezesVeiculo(Veiculo, BagHead, Nr), 
                        Nr>Max, calcularEstafetaQueMaisUsouVeiculo(Veiculo, Tail, Nr, BagHead). % se for por percentagem alterar aqui a comparação
calcularEstafetaQueMaisUsouVeiculo(Veiculo, [_ | Tail], Max, Answer) :-
                        calcularEstafetaQueMaisUsouVeiculo(Veiculo, Tail, Max, Answer).
        
maisEcologico(Veiculo, Answer) :-% se for só bicicleta cagar no 'Veiculo'
            findEstafetasPorVeiculo(Veiculo, Bag), calcularEstafetaQueMaisUsouVeiculo(Veiculo, Bag, 0, Answer).
% ---------------------------------------


% --------------------------------------- identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente;

trackEncomenda(IdCliente, R) :- findall(IdEnc, encomenda(entregue,IdEnc,IdCliente,_,_,_,_),Aux),
                                idEncPorEstafeta(Aux,[],R).

idEncPorEstafeta([],R,R).
idEncPorEstafeta([IdEnc|T],L,R) :- entrega(IdEnc,IdEst,_,_,_,_), \+member((IdEst,_),L), append([(IdEst,[IdEnc])],L,L2), idEncPorEstafeta(T,L2,R).
idEncPorEstafeta([IdEnc|T],L,R) :- entrega(IdEnc,IdEst,_,_,_,_), adicionaElemento(IdEnc,IdEst,L,[],L2), idEncPorEstafeta(T,L2,R).

adicionaElemento(IdEnc,IdEst,[(IdEst,List)|T],Acc,R) :- append([IdEnc],List,List2), append([(IdEst,List2)|T],Acc,R).
adicionaElemento(IdEnc,IdEst,[H|T],Acc,R) :- append([H],Acc,Acc2), adicionaElemento(IdEnc,IdEst,T,Acc2,R).


trackSomeEncomenda(IdCliente,ListEnc,R) :- checkClienteEnc(IdCliente,ListEnc,[],Aux), idEncPorEstafeta(Aux,[],R).

checkClienteEnc(_,[],R,R).
checkClienteEnc(IdC, [IdEnc|T], Acc, R) :- encomenda(entregue,IdEnc,IdC,_,_,_,_), append([IdEnc],Acc,Acc2),
                                         checkClienteEnc(IdC,T,Acc2,R).
checkClienteEnc(IdC,[_|T],Acc,R) :- checkClienteEnc(IdC,T,Acc,R).

% ---------------------------------------


% --------------------------------------- identificar os clientes servidos por um determinado estafeta;
findClientesServidosPorEstafeta(IdEstafeta, Answer) :- 
                estafeta(IdEstafeta, Nr, _), Nr == 0, Answer = [], write(IdEstafeta),write(" ainda não realizou entregas"), !;
                findall(IdEnc, entrega(IdEnc, IdEstafeta, _, _, _, _), Aux),
                clientesPorEncomenda(Aux,[],Answer).

clientesPorEncomenda([],L,L).
clientesPorEncomenda([Id|T],L,Answer) :- encomenda(_,Id,IdCliente,_,_,_,_), \+member(IdCliente,L), !,
                                         append([IdCliente],L,L2), clientesPorEncomenda(T,L2,Answer).
clientesPorEncomenda([_|T],L,Answer) :- clientesPorEncomenda(T,L,Answer).

% ---------------------------------------


% --------------------------------------- calcular o valor faturado pela Green Distribution num determinado dia;

% ---------------------------------------


% ----------- encomenda(estado, id, idCliente, idEstafeta, peso, volume, freguesia, morada, prazo) /9
% --------------------------------------- identificar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution;
bestZonas(Top1, Top2, Top3) :- findall(Freguesia, encomenda(_, _, _, _, _, Freguesia/_, _), Bag),
                calculaTop(Bag, Top1, Top2, Top3).


calculaTop(Bag, Top1, Top2, Top3) :-
                countall(Bag, Count),
                findTop(Count, Top1, Top2, Top3, 0, 0, 0).
            
findTop([], Top1, Top2, Top3, Top1, Top2, Top3).
findTop([H|T], N1, N2, N3, Top1, Top2, Top3) :- 
                                (H>Top1) -> findTop(T, N1, N2, N3, H, Top1, Top2); 
                                (H>Top2) -> findTop(T, N1, N2, N3, Top1, H, Top2);
                                (H>Top3) -> findTop(T, N1, N2, N3, Top1, Top2, H);
                                findTop(T, N1, N2, N3, Top1, Top2, Top3).


count([],_,0).
count([Elem|Tail],Elem,Y):- count(Tail,Elem,Z), Y is 1+Z.
count([Elem1|Tail],Elem,Z):- Elem1\=Elem,count(Tail,Elem,Z).
                
countall(List,Count) :-
        sort(List,List1),
        counteach([],List,List1,Count).

counteach(L1,_,[],L1).
counteach(L1,L,[H|T],R) :- count(L,H,Count), append(L1,[Count],L2), counteach(L2,L,T,R).



%count([],_,0).
%count([Elem|Tail],Elem,Y):- count(Tail,Elem,Z), Y is 1+Z.
%count([Elem1|Tail],Elem,Z):- Elem1\=Elem,count(Tail,Elem,Z).

%countall(List,Elem,Count) :-
%        sort(List,List1),
%        member(Elem,List1),
%        count(List,Elem,Count).

% ---------------------------------------

% --------------------------------------- calcular a classificação media de satisfação de cliente para um determinado estafeta;
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
nrEntregasPorTransporte(DiaI/MesI/AnoI, Hi:Mi, DiaF/MesF/AnoF, Hf:Mf,R) :- 
                                        findall(Veiculo,
                                        (entrega(_,_,Veiculo,_,D/H,_),
                                        checkTimeInterval(D,H,date(AnoI,MesI,DiaI),time(Hi,Mi,0),date(AnoF,MesF,DiaF),time(Hf,Mf,0))),
                                        Aux), listToPairList(Aux,[],R).
% [mota,mota,carro,bicicleta,carro,mota]
listToPairList([],R,R).
listToPairList([H|T],L,R) :- \+member((H,_),L), append([(H,1)],L,L2), listToPairList(T,L2,R).
listToPairList([H|T],L,R) :-  incrementaPar(H,L,[],Aux), listToPairList(T,Aux,R).

incrementaPar(H,[(H,Nr)|T],L,R) :- NewNr is Nr + 1, append([(H,NewNr)|T],L,R).
incrementaPar(H,[(H2,Nr)|T],L,R) :- append([(H2,Nr)],L,Aux),incrementaPar(H,T,Aux,R). 

checkTimeInterval(D,H, Di,Hi,Df,Hf) :- timeElapsed(D,H,Di,Hi,Res1), Res1 =< 0,
                                       timeElapsed(D,H,Df,Hf,Res2), Res2 >= 0.
% ---------------------------------------

% ----------- entrega(idEncomenda, idEstafeta, veiculo, DataInicio, DataFim, avaliacao) /6
% --------------------------------------- identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo;
nrEntregasPorEstafeta(DiaI/MesI/AnoI, Hi:Mi, DiaF/MesF/AnoF, Hf:Mf,R) :-
                                        findall(IdEst, 
                                                (entrega(_,IdEst,_,_,D/H,_),
                                        checkTimeInterval(D,H,date(AnoI,MesI,DiaI),time(Hi,Mi,0),date(AnoF,MesF,DiaF),time(Hf,Mf,0))),
                                        Aux), listToPairList(Aux,[],R).
% ---------------------------------------


% --------------------------------------- calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo;

% ---------------------------------------


% --------------------------------------- calcular o peso total transportado por estafeta num determinado dia.
addCliente(Id) :- cliente(Id,Nr), NewNr is Nr + 1, replace_existing_fact(cliente(Id,Nr),cliente(Id,NewNr)).

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

