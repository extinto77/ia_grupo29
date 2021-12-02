
:- op( 900,xfy,'::' ).

%:- dynamic(veiculo/3).
:- dynamic(estafeta/3).
:- dynamic(cliente/2).
:- dynamic(encomenda/7).
:- dynamic(entrega/6).



%escrever factos de estafetas, clientes e encomendas para termos já alguns na BD

% Factos:

mes(1, janeiro, 31).
mes(2, fevereiro, 28).
mes(3, marco, 31).
mes(4, abril, 30).
mes(5, maio, 31).
mes(6, junho, 30).
mes(7, julho, 31).
mes(8, agosto, 31).
mes(9, setembro, 30).
mes(10, outubro, 31).
mes(11, novembro, 30).
mes(12, dezembro, 31).




validateTime(H1, M1, S1) :-
        H1<24, H1>=0, M1<60, M1>=0, S1<60, S1>=0.

verBissexto(Ano, Max) :-
        (Mod1 is Ano mod 4, Mod2 is Ano mod 100, Mod3 is Ano mod 400) ,
        (Mod1==0, (Mod2=\= 0 ; Mod3==0)) -> Max is 29;
        Max is 28.

validateDate(A, 2, D) :-
        D>0, verBissexto(A, Max), D=<Max, !. % é preciso o '!' ??
validateDate(A, M, D) :-
        M>0, M=<12, mes(M, _, Max), D>0, D=<Max.


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

preco(bicicleta,0.5).
preco(mota,1).
preco(carro,1.5).

% ----------- cliente(id, nrEncomendas) /2
cliente(daniel, 3).
cliente(abacao, 3).

% ----------- estafeta(id, nrEntregas, avaliacao) /3
estafeta(joao, 1, 4.4).
estafeta(caldas, 3, 4.4).
estafeta(ctt,2,4.4).

% (3:2:2) -> prazo 3dias 2h 2min 
% encomenda(Estado, Id, IdClinet, Peso, Volume, Freguesia/Morada, prazo)
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
entrega(folha, ctt, bicicleta, date(2021,10,4)/time(0,0,0), date(2021,10,5)/time(0,0,0), 4.4).
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


ajuda(geral, 
"\ncreateEstafeta(Id) -> Adiciona um estafeta a lista de estafetas. 
createCliente(Id) -> Adiciona um cliente a lista de clientes.
createEncomenda(Id, IdCliente, Peso, Volume, Freguesia, Morada, Dias, Horas, Minutos, Date/Time) -> Adiciona uma encomenda a lista de encomendas.
createEntrega(IdEncomenda, IdEstafeta, Veiculo, DataI, TimeI, Date ,Time) -> Adiciona uma encomenda a lista de Entregas.
entregarEncomenda(IdEncomenda, Dia, Mes, Ano, Horas, Minutos, Avaliacao) -> Realiza a entrega de uma encomenda.
findEstafetasPorVeiculo(Veiculo, Bag) -> Devolve os estafetas que usaram determinado veiculo.
trackEncomenda(IdCliente, R) -> Identifica que estafeta realizou a entrega a determinado cliente.
findClientesServidosPorEstafeta(IdEstafeta, Answer) -> Identifica os clientes servidos por determinado estafeta.
bestZonas(Top1, Top2, Top3) -> Identifica as zonas com maior volume de entregas.
calcularMediaSatisfacaoEstafeta(IdEstafeta, Answer) -> Calcula a classificacao media de um estafeta.
nrEntregasPorTransporte(DiaI/MesI/AnoI, Hi:Mi, DiaF/MesF/AnoF, Hf:Mf,R) -> Calcula o numero de entregas realizadas pelos diferentes meios de transporte,num determinado intervalo de tempo.
pesoNumDia(IdEstafeta, Dia/Mes/Ano, Answer) -> Calcula o peso total transportado num determinado dia.
").
% mudar esta parte
ajuda(encomenda, "Usar a funcao createEncomenda() com os parametros: 
                Id, IdCliente, Peso, Volume, Freguesia, Morada, Dias, Horas, Minutos sendo os ultimos 3 parametros o prazo").

ajuda(estafeta, "Usar a funcao createEstafeta() com os parametros: 
                Id").

ajuda(cliente, "Usar a funcao createCliente() com os parametros: 
                Id").

help(X) :- ajuda(X, Y), write(Y). % tentar tirar o true que aparece depois


                        
createEstafeta(Id) :- \+estafeta(Id, _, _), assert(estafeta(Id, 0, 0)),  write("Estafeta adicionado"), !;
                        write("Estafeta já existente").

addEstafeta(Id,Av) :- estafeta(Id,Nr,Media), NewNr is Nr + 1, arredondar(((Media*Nr + Av)/NewNr), NewMedia, 2),
                      replace_existing_fact(estafeta(Id,Nr,Media),estafeta(Id,NewNr,NewMedia)).

createCliente(Id) :- \+cliente(Id, _), assert(cliente(Id, 0)), write("Cliente adicionado"), !;
                        write("Cliente já existente").



                %fazer os teste de id da encomenda e assim antes, esxiste id cliente, estafeta e assim
createEncomenda(Id, IdCliente, Peso, Volume, Freguesia, Morada, Dias, Horas, Minutos, DataTime) :- 
                encomenda(_, Id, _, _, _, _, _), write("Encomenda já existente"), !;
                \+cliente(IdCliente, _), write("Cliente não existente"), !;
                Peso > 100, write("Nenhum veiculo suporta a entrega da encomenda"), !;
                Peso < 0, write("Peso impossível"), !;
                Volume < 0, write("Volume impossível"), !;
                (Dias < 0; Horas < 0; Minutos < 0), write("Prazo impossível"), !; 
                assert(encomenda(registada, Id, IdCliente, Peso, Volume, Freguesia/Morada, date(0,0,Dias)/time(Horas,Minutos,0))),
                assert(entrega(Id,empty,empty,DataTime,empty/empty,empty)),
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



% -------- identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico;

maisEcologico(Answer) :-% se for só bicicleta cagar no 'Veiculo'
            findall((IdEst,Peso,Veiculo),
            (entrega(IdEnc,IdEst,Veiculo,_,_,_),encomenda(entregue,IdEnc,_,Peso,_,_,_)),
            Bag),setof(Id,estafeta(Id,_,_),BagE), 

% [(caldas,5,carro),(caldas,20,mota)] , caldas,
organizar([],_,[]).
%organizar([(A,_,_)|T1],IdEst,X) :- \+(A = IdEst), organizar(T1,IdEst,X).
organizar([(IdEst,P,V)|T1],IdEst,X) :- checkEcologica(P,V,Aux), organizar(T1,IdEst,[Aux|X]). 
organizar([_|T1],IdEst,X) :- organizar(T1,IdEst,X).



mediaEcologia(L,R) :- somaLista(L,Aux), length(L,Size), R is Aux/Size. 

checkEcologica(Peso,bicicleta,1). 
checkEcologica(Peso,mota,R) :- (Peso > 5) -> R is 2 ; R is 4.
checkEcologica(Peso,carro,R) :- (Peso > 20) -> R is 3 ; (Peso > 5) -> R is 6 ; R is 9.      

% ---------------------------------------


% ----------- identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente;

trackEncomenda(IdCliente, R) :- findall(IdEnc, encomenda(entregue,IdEnc,IdCliente,_,_,_,_),Aux),
                                idEncPorEstafeta(Aux,[],R).

idEncPorEstafeta([],R,R).
idEncPorEstafeta([IdEnc|T],L,R) :- entrega(IdEnc,IdEst,_,_,_,_), \+member((IdEst,_),L), 
                                        append([(IdEst,[IdEnc])],L,L2), idEncPorEstafeta(T,L2,R).
idEncPorEstafeta([IdEnc|T],L,R) :- entrega(IdEnc,IdEst,_,_,_,_), adicionaElemento(IdEnc,IdEst,L,[],L2), 
                                        idEncPorEstafeta(T,L2,R).

adicionaElemento(IdEnc,IdEst,[(IdEst,List)|T],Acc,R) :- append([IdEnc],List,List2), append([(IdEst,List2)|T],Acc,R).
adicionaElemento(IdEnc,IdEst,[H|T],Acc,R) :- append([H],Acc,Acc2), adicionaElemento(IdEnc,IdEst,T,Acc2,R).


trackSomeEncomenda(IdCliente,ListEnc,R) :- checkClienteEnc(IdCliente,ListEnc,[],Aux), idEncPorEstafeta(Aux,[],R).

checkClienteEnc(_,[],R,R).
checkClienteEnc(IdC, [IdEnc|T], Acc, R) :- encomenda(entregue,IdEnc,IdC,_,_,_,_), append([IdEnc],Acc,Acc2),
                                         checkClienteEnc(IdC,T,Acc2,R).
checkClienteEnc(IdC,[_|T],Acc,R) :- checkClienteEnc(IdC,T,Acc,R).

% ---------------------------------------


% ------------- identificar os clientes servidos por um determinado estafeta;
findClientesServidosPorEstafeta(IdEstafeta, Answer) :- 
                estafeta(IdEstafeta, Nr, _), Nr == 0, Answer = [], write(IdEstafeta),write(" ainda não realizou entregas"), !;
                findall(IdEnc, entrega(IdEnc, IdEstafeta, _, _, _, _), Aux),
                clientesPorEncomenda(Aux,[],Answer).

clientesPorEncomenda([],L,L).
clientesPorEncomenda([Id|T],L,Answer) :- encomenda(_,Id,IdCliente,_,_,_,_), \+member(IdCliente,L), !,
                                         append([IdCliente],L,L2), clientesPorEncomenda(T,L2,Answer).
clientesPorEncomenda([_|T],L,Answer) :- clientesPorEncomenda(T,L,Answer).

% ---------------------------------------

%quanto maior o prazo menor o preco, o peso influencia mais o preço do que o volume
% ----------------- calcular o valor faturado pela Green Distribution num determinado dia;
calcFaturacao(Dia/Mes/Ano,R) :- findall( Preco, 
                        (encomenda(entregue,IdEnc,_,_,_,_,_),
                        entrega(IdEnc,_,_,_,date(Ano,Mes,Dia)/_,_),   
                        calcPreco(IdEnc,Preco)), Bag),
                        somaLista(Bag,R). 

% calcPreco podia ter distancia mas so para fase2 :)
calcPreco(IdEnc,Preco) :- encomenda(_,IdEnc,_,Peso,Volume,_,Dp/Tp), entrega(IdEnc,_,Veiculo,_,_,_), 
                          preco(Veiculo,Pv), timeElapsed(date(0,0,0),time(0,0,0),Dp,Tp,Aux),
                          AuxPreco is (Pv*(Peso*0.7+Volume*0.3))/(Aux+1), arredondar(AuxPreco,Preco,2).

somaLista([],0).
somaLista([H|T],R) :- somaLista(T,Resto), R is Resto + H. 

% ---------------------------------------

% -------------- identificar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution;
% refazer esta !!!!!!
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

% ---------------------------------------

% ----------- calcular a classificação media de satisfação de cliente para um determinado estafeta;
calcularMediaSatisfacaoEstafeta(IdEstafeta, Answer) :- 
                    estafeta(IdEstafeta, Nr, _), Nr =:= 0, Answer = "O estafeta ainda não realizou entregas", !;
                    findall(Avaliacao, entrega( _, IdEstafeta, _, _, _, Avaliacao), X), 
                    validarAvaliacao(X, NewX), averageList(NewX, Val), arredondar(Val, Answer, 2).


arredondar(X,Y,D) :- Z is X * 10^D, round(Z, ZA), Y is ZA / 10^D.

validarAvaliacao([], []).
validarAvaliacao([Head|Tail], List) :- 
            (Head < 0 -> List = Output ;  List = [Head|Output] ), 
            validarAvaliacao(Tail, Output).


averageList( List, Avg ) :-
        somaLista( List, Sum ),
        length( List, Length ),
        Avg is Sum / Length.
% ---------------------------------------

% ---------- identificar o número total de entregas pelos diferentes meios de transporte,num determinado intervalo de tempo;
nrEntregasPorTransporte(DiaI/MesI/AnoI, Hi:Mi, DiaF/MesF/AnoF, Hf:Mf,R) :- 
                                        findall(Veiculo,
                                        (entrega(_,_,Veiculo,_,D/H,_),
                                        checkTimeInterval(D,H,date(AnoI,MesI,DiaI),time(Hi,Mi,0),date(AnoF,MesF,DiaF),time(Hf,Mf,0))),
                                        Aux), listToPairList(Aux,[],R).

listToPairList([],R,R).
listToPairList([H|T],L,R) :- \+member((H,_),L), append([(H,1)],L,L2), listToPairList(T,L2,R).
listToPairList([H|T],L,R) :-  incrementaPar(H,L,[],Aux), listToPairList(T,Aux,R).

incrementaPar(H,[(H,Nr)|T],L,R) :- NewNr is Nr + 1, append([(H,NewNr)|T],L,R).
incrementaPar(H,[(H2,Nr)|T],L,R) :- append([(H2,Nr)],L,Aux),incrementaPar(H,T,Aux,R). 

checkTimeInterval(D,H, Di,Hi,Df,Hf) :- timeElapsed(D,H,Di,Hi,Res1), Res1 =< 0,
                                       timeElapsed(D,H,Df,Hf,Res2), Res2 >= 0.
% ---------------------------------------
     

% ----------- identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo;
nrEntregasPorEstafeta(DiaI/MesI/AnoI, Hi:Mi, DiaF/MesF/AnoF, Hf:Mf,R) :-
                                        findall(IdEst, 
                                                (entrega(_,IdEst,_,_,D/H,_),
                                        checkTimeInterval(D,H,date(AnoI,MesI,DiaI),time(Hi,Mi,0),date(AnoF,MesF,DiaF),time(Hf,Mf,0))),
                                        Aux), listToPairList(Aux,[],R).
% ---------------------------------------


% --------- calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo;
% dizer que encomendas nao entregues para trás não contam.
numEncomendas(DateI, TimeI, DateF, TimeF,Answer) :- findall(1 ,(entrega(_,_,_,Data1/Time1,empty/empty,_),
                                                                checkTimeInterval(Data1,Time1,DateI,TimeI,DateF,TimeF)), 
                                                        Bag1),
                                          length(Bag1, NrNaoEntregue),
                                          findall(Data/Time,(entrega(_,_,_,_,Data/Time,_), \+(Data = empty), \+(Time = empty),
                                          checkTimeInterval(Data,Time,DateI,TimeI,DateF,TimeF)),Bag2),
                                          length(Bag2, NrEntregue), 
                                        Answer = [(entregues, NrEntregue), (nao_entregues, NrNaoEntregue)].
% ---------------------------------------


% ------ calcular o peso total transportado por estafeta num determinado dia.
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

