
:- [base_conhecimento].

:- op( 900,xfy,'::' ).

:- dynamic(estafeta/3).
:- dynamic(cliente/2).
:- dynamic(encomenda/7).
:- dynamic(entrega/6).




% ---------------------------------------------------------------------------------------------------------             
% ---------- Predicados relacionados com datas/horas
% ---------------------------------------------------------------------------------------------------------             
validateTime(H, M, S) :-
        H<24, H>=0, M<60, M>=0, S<60, S>=0.

verBissexto(Ano, Max) :-
        (Mod1 is Ano mod 4, Mod2 is Ano mod 100, Mod3 is Ano mod 400) ,
        (Mod1==0, (Mod2=\= 0 ; Mod3==0)) -> Max is 29;
        Max is 28.

validateDate(A, 2, D) :-
        D>0, verBissexto(A, Max), D=<Max, !.
validateDate(_, M, D) :-
        M>0, M=<12, mes(M, Max), D>0, D=<Max.


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
% ---------------------------------------------------------------------------------------------------------             





% ---------------------------------------------------------------------------------------------------------             
% ---------- AJUDA
% ---------------------------------------------------------------------------------------------------------             
ajuda(geral, 
        "\tcreateEstafeta(Id) -> Adiciona um estafeta a lista de estafetas. 
        createCliente(Id) -> Adiciona um cliente a lista de clientes.
        createEncomenda(Id, IdCliente, Peso, Volume, Freguesia, Morada, Dias, Horas, Minutos, Date/Time) -> Adiciona uma encomenda a lista de encomendas.
        createEntrega(IdEncomenda, IdEstafeta, Veiculo) -> Adiciona uma encomenda a lista de Entregas.
        entregarEncomenda(IdEncomenda, Dia, Mes, Ano, Horas, Minutos, Avaliacao) -> Realiza a entrega de uma encomenda.
        query1(Answer) -> Identifica o estafeta que mais utilizou meios de transporte mais ecologicos.
        query2(Cliente, R) -> Identifica que estafeta realizou a entrega a determinado cliente.
        query3(IdEstafeta, Answer) -> Identifica os clientes servidos por determinado estafeta.
        query4(Dia, Mes, Ano, Answer) -> Calcula o valor faturado pela Green Distribution num determinado dia.
        query5(Tag, Top1, Top2, Top3) -> Identifica as zonas com maior volume de entregas.
        query6(IdEstafeta, Answer) -> Calcula a classificacao media de um estafeta.
        query7(DiaI, MesI, AnoI, Hi, Mi, DiaF, MesF, AnoF, Hf, Mf, R) -> Calcula o numero de entregas realizadas pelos diferentes meios de transporte,num determinado intervalo de tempo.
        query8(DiaI, MesI, AnoI, Hi, Mi, DiaF, MesF, AnoF, Hf, Mf, R) -> Calcula o numero de entregas realizadas pelos diferentes estafetas, num determinado intervalo de tempo.
        query9(DiaI, MesI, AnoI, Hi, Mi, DiaF, MesF, AnoF, Hf, Mf, R) -> Calcula o numero de encomendass entregues e nao entregues pela Green Distribution, num determinado intervalo de tempo.
        query10(IdEstafeta, Dia/Mes/Ano, Answer) -> Calcula o peso total transportado num determinado dia.
        ").

ajuda(entrega, "Usar a funcao createEntrega() com os paramentros:
                IdEncomenda, IdEstafeta, Veiculo").

ajuda(encomenda, "Usar a funcao createEncomenda() com os parametros: 
                Id, IdCliente, Peso, Volume, Freguesia, Morada, Dias, Horas, Minutos sendo os ultimos 3 parametros o prazo").

ajuda(estafeta, "Usar a funcao createEstafeta() com os parametros: 
                Id").

ajuda(cliente, "Usar a funcao createCliente() com os parametros: 
                Id").

help(X) :- ajuda(X, Y), write(Y). % tentar tirar o true que aparece depois
% ---------------------------------------------------------------------------------------------------------             









                        
createEstafeta(Id) :- \+estafeta(Id, _, _), assert(estafeta(Id, 0, 0)),  write("Estafeta adicionado"), !;
                        write("Estafeta já existente").

addEstafeta(Id,Av) :- estafeta(Id,Nr,Media), NewNr is Nr + 1, arredondar(((Media*Nr + Av)/NewNr), NewMedia, 2),
                      replace_existing_fact(estafeta(Id,Nr,Media),estafeta(Id,NewNr,NewMedia)).

createCliente(Id) :- \+cliente(Id, _), assert(cliente(Id, 0)), write("Cliente adicionado"), !;
                        write("Cliente já existente").



%fazer os teste de id da encomenda e assim antes, esxiste id cliente, estafeta e assim
createEncomenda(Id, IdCliente, Peso, Volume, Freguesia, Morada, Dias, Horas, Minutos, date(A, M, D)/time(H2, M2, S2)) :- 
                encomenda(_, Id, _, _, _, _, _), write("Encomenda já existente"), !;
                \+cliente(IdCliente, _), write("Cliente não existente"), !;
                Peso > 100, write("Nenhum veiculo suporta a entrega da encomenda"), !;
                Peso < 0, write("Peso inválido"), !;
                Volume < 0, write("Volume inválido"), !;
                (Dias < 0; Horas < 0; Minutos < 0), write("Prazo impossível"), !;
                \+validateDate(A, M, D), \+validateTime(H2, M2, S2), write("Data ou horas de Inicio erradas"), !;
                
                assert(encomenda(registada, Id, IdCliente, Peso, Volume, Freguesia/Morada, date(0,0,Dias)/time(Horas,Minutos,0))),
                assert(entrega(Id,empty,empty,date(A, M, D)/time(H2, M2, S2),empty/empty,empty)),
                addCliente(IdCliente),
                write("Encomenda registada").
                    
addCliente(Id) :- cliente(Id,Nr), NewNr is Nr + 1, replace_existing_fact(cliente(Id,Nr),cliente(Id,NewNr)).
              


createEntrega(IdEncomenda, IdEstafeta, Veiculo) :-
                \+encomenda(_, IdEncomenda, _, _, _, _, _), write("Encomenda não existente"), !;
                \+estafeta(IdEstafeta, _, _), write("Estafeta não existente"), !;
                encomenda(entregue, IdEncomenda, _, _, _, _, _), write("A encomenda já foi entregue"), !;
                encomenda(distribuicao, IdEncomenda, _, _, _, _, _), write("A encomenda já se encontra em distribuição"), !;
                
                encomenda(_, IdEncomenda, _, _, Peso, _, _), veiculo(Veiculo, _, Max), 
                            Max < Peso, write("Veículo selecionado não suporta carga da encomenda"), !;

                % adicionar info aos outros factos !!!!
                replace_existing_fact(encomenda(registada,    IdEncomenda, IdCliente, Peso, Volume, ADDR, InfoPrazo), 
                                      encomenda(distribuicao, IdEncomenda, IdCliente, Peso, Volume, ADDR, InfoPrazo)), 
                replace_existing_fact(entrega(IdEncomenda, _,          _,       InfoInicio, InfoFim, Aval )  ,
                                      entrega(IdEncomenda, IdEstafeta, Veiculo, InfoInicio, InfoFim, Aval ) ),
                % add potencial -> atribuicao                   
                write("Entrega registada, a encomenda ira ser distribuida").
        
entregarEncomenda(IdEncomenda, Dia, Mes, Ano, Horas, Minutos, Avaliacao) :-
                \+encomenda(_, IdEncomenda, _, _, _, _, _), write("Encomenda não existente"), !;
                encomenda(entregue, IdEncomenda, _, _, _, _, _), write("A encomenda já foi entregue"), !;
                encomenda(registada, IdEncomenda, _, _, _, _, _), write("A encomenda ainda não está em distribuição"), !;
                \+validateDate(Ano, Mes, Dia), \+validateTime(Horas, Minutos, 0), write("Data ou horas de Fim erradas"), !;
                (Avaliacao<0; Avaliacao>5), write("Avaliacao invalida"), !;
        
                replace_existing_fact(encomenda(distribuicao, IdEncomenda, IdCliente, Peso, Volume, ADDRS, Prazo), 
                                      encomenda(entregue,     IdEncomenda, IdCliente, Peso, Volume, ADDRS, Prazo)), 
                replace_existing_fact(entrega(IdEncomenda, IdEstafeta, Veiculo, DataInicio, _, _), 
                                      entrega(IdEncomenda, IdEstafeta, Veiculo, DataInicio, 
                                                                date(Ano, Mes, Dia)/time(Horas, Minutos, 0), Avaliacao)), 
                addEstafeta(IdEstafeta, Avaliacao),
                write("Encomenda entregue"). 
                %ver penalização
                
                
query1(Ans) :-                    maisEcologico(Ans).
query2(Cliente, Ans) :-           trackEncomenda(Cliente, Ans).
query3(Estafeta, Ans) :-          findClientesServidosPorEstafeta(Estafeta, Ans).
query4(Dia, Mes, Ano, Ans) :-     calcFaturacao(Dia, Mes, Ano, Ans).
query5(Tag, Top1, Top2, Top3) :-  bestZonas(Tag, Top1, Top2, Top3). % tag pode ser freguesia ou rua
query6(Estafeta, Ans) :-          calcularMediaSatisfacaoEstafeta(Estafeta, Ans).
query7(DiaI, MesI, AnoI, Hi, Mi, DiaF, MesF, AnoF, Hf, Mf, Ans) :-  nrEntregasPorTransporte(DiaI/MesI/AnoI, Hi:Mi, DiaF/MesF/AnoF, Hf:Mf,Ans).
query8(DiaI, MesI, AnoI, Hi, Mi, DiaF, MesF, AnoF, Hf, Mf, Ans) :-  nrEntregasPorEstafeta(DiaI/MesI/AnoI, Hi:Mi, DiaF/MesF/AnoF, Hf:Mf,Ans).
query9(DiaI, MesI, AnoI, Hi, Mi, DiaF, MesF, AnoF, Hf, Mf, Ans) :-  numEncomendas(date(AnoI, MesI, DiaI), time(Hi, Mi, 0), 
                                                                                  date(AnoF, MesF, DiaF), time(Hf, Mf, 0), Ans).
query10(Estafeta, Dia, Mes, Ano, Ans) :-   pesoNumDia(Estafeta, Dia/Mes/Ano, Ans).


% ---------------------------------------------------------------------------------------------------------             
% -------- identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico;
% ---------------------------------------------------------------------------------------------------------             
maisEcologico(Answer) :-
        findall((IdEst,R),
                (entrega(IdEnc,IdEst,Veiculo,_,_,_),
                encomenda(entregue,IdEnc,_,Peso,_,_,_),
                checkEcologica(Peso,Veiculo,R)),
        Bag), 
        organizaDuplo(Bag,[],Res), getHead(Res,IdAux,MinAux),getTail(Res,Tail),
        convertDuplo(Tail,IdAux,MinAux,Answer).


getTail([_|T],T).

getHead([(IdEnc,L)|_],IdEnc,Min) :- mediaEcologia(L,Min).    

convertDuplo([],IdEnc,_,IdEnc).
convertDuplo([(IdEnc,List)|T],IdEncMin,Min,R) :- 
                                mediaEcologia(List,X), 
                                ((X < Min) -> convertDuplo(T,IdEnc,X,R);
                                (X > Min) -> convertDuplo(T,IdEncMin,Min,R);
                                (is_list(IdEncMin) -> append([IdEnc],IdEncMin,New);
                                                      append([IdEnc],[IdEncMin],New)), 
                                convertDuplo(T,New,Min,R)).

organizaDuplo([],Acc,Acc).
organizaDuplo([(IdEnc,Eco)|T],Acc,R) :- \+member((IdEnc,_),Acc), 
                                        append([(IdEnc,[Eco])],Acc,Acc2), 
                                        organizaDuplo(T,Acc2,R).  
organizaDuplo([(IdEnc,Eco)|T],Acc,R) :- adicionaElemento(Eco,IdEnc,Acc,[],L2), organizaDuplo(T,L2,R).

mediaEcologia(L,R) :- somaLista(L,Aux), length(L,Size), R is Aux/Size. 

checkEcologica(_,bicicleta,1). 
checkEcologica(Peso,mota,R) :- (Peso > 5) -> R is 2 ; R is 4.
checkEcologica(Peso,carro,R) :- (Peso > 20) -> R is 3 ; (Peso > 5) -> R is 6 ; R is 9.      
% ---------------------------------------------------------------------------------------------------------             



% ---------------------------------------------------------------------------------------------------------             
% ---------- identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente;
% ---------------------------------------------------------------------------------------------------------             
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
% ---------------------------------------------------------------------------------------------------------             



% ---------------------------------------------------------------------------------------------------------             
% ------------- identificar os clientes servidos por um determinado estafeta;
% ---------------------------------------------------------------------------------------------------------             
findClientesServidosPorEstafeta(IdEstafeta, Answer) :- 
                estafeta(IdEstafeta, Nr, _), Nr == 0, Answer = [], write(IdEstafeta),write(" ainda não realizou entregas"), !;
                findall(IdEnc, entrega(IdEnc, IdEstafeta, _, _, _, _), Aux),
                clientesPorEncomenda(Aux,[],Answer).


clientesPorEncomenda([],L,L).
clientesPorEncomenda([Id|T],L,Answer) :- encomenda(_,Id,IdCliente,_,_,_,_), \+member(IdCliente,L), !,
                                         append([IdCliente],L,L2), clientesPorEncomenda(T,L2,Answer).
clientesPorEncomenda([_|T],L,Answer) :- clientesPorEncomenda(T,L,Answer).
% ---------------------------------------------------------------------------------------------------------             



% ---------------------------------------------------------------------------------------------------------             
% ----------------- calcular o valor faturado pela Green Distribution num determinado dia;
% ---------------------------------------------------------------------------------------------------------             

%quanto maior o prazo menor o preco, o peso influencia mais o preço do que o volume

calcFaturacao(Dia, Mes, Ano, R) :- findall( Preco, 
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
% ---------------------------------------------------------------------------------------------------------             



% ---------------------------------------------------------------------------------------------------------             
% -------------- identificar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution;
% ---------------------------------------------------------------------------------------------------------             
bestZonas(freguesia, Top1, Top2, Top3) :- findall(Freguesia, encomenda(_, _, _, _, _, Freguesia/_, _), Bag),
                calculaTop(Bag, Top1, Top2, Top3).
bestZonas(rua, Top1, Top2, Top3) :- findall(Freguesia/Rua, encomenda(_, _, _, _, _, Freguesia/Rua, _), Bag),
        calculaTop(Bag, Top1, Top2, Top3).


calculaTop(Bag, Top1, Top2, Top3) :-
                countall(Bag, List, Count),
                makePair(List, Count, Res),
                findTop(Res, 0, 0, 0,(null,null,null),Top1, Top2, Top3).

        
findTop([], _, _, _,(T1,T2,T3),T1, T2, T3).
findTop([(Name, Val)|T], N1, N2, N3,(T1,T2,T3),Top1, Top2, Top3) :- 
                (Val>N1) -> findTop(T, Val, N1, N2, (Name,T1,T2), Top1, Top2,Top3); 
                (Val>N2) -> findTop(T, N1, Val, N2, (T1,Name,T2),Top1, Top2, Top3);
                (Val>N3) -> findTop(T, N1, N2, Val, (T1,T2,Name),Top1, Top2, Top3);
                            findTop(T, N1, N2, N3, (T1,T2,T3),Top1, Top2, Top3).

makePair([], [], []).
makePair([H|T], [Val|TVal], Ans) :- append([(H, Val)], Y, Ans), makePair(T, TVal, Y).

count([],_,0).
count([Elem|Tail],Elem,Y):- count(Tail,Elem,Z), Y is 1+Z.
count([Elem1|Tail],Elem,Z):- Elem1\=Elem,count(Tail,Elem,Z).
                
countall(List,List1, Count) :-
        sort(List,List1),
        counteach([],List,List1,Count).

counteach(L1,_,[],L1).
counteach(L1,L,[H|T],R) :- count(L,H,Count), append(L1,[Count],L2), counteach(L2,L,T,R).
% ---------------------------------------------------------------------------------------------------------             



% ---------------------------------------------------------------------------------------------------------             
% ----------- calcular a classificação media de satisfação de cliente para um determinado estafeta;
% ---------------------------------------------------------------------------------------------------------             
calcularMediaSatisfacaoEstafeta(IdEstafeta, Answer) :- 
                    estafeta(IdEstafeta, Nr, _), Nr =:= 0, Answer = "O estafeta ainda não realizou entregas", !;
                    findall(Avaliacao, (entrega(_, IdEstafeta, _, _, _, Avaliacao), \+(Avaliacao=empty)), X), 
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
% ---------------------------------------------------------------------------------------------------------             



% ---------------------------------------------------------------------------------------------------------             
% ---------- identificar o número total de entregas pelos diferentes meios de transporte,num determinado intervalo de tempo;
% ---------------------------------------------------------------------------------------------------------             
nrEntregasPorTransporte(DiaI/MesI/AnoI, Hi:Mi, DiaF/MesF/AnoF, Hf:Mf,R) :- 
                                        findall(Veiculo,
                                        (entrega(_,_,Veiculo,_,D/H,_), \+(D=empty),
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
     

% ---------------------------------------------------------------------------------------------------------             
% ----------- identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo;
% ---------------------------------------------------------------------------------------------------------             
nrEntregasPorEstafeta(DiaI/MesI/AnoI, Hi:Mi, DiaF/MesF/AnoF, Hf:Mf,R) :-
                                        findall(IdEst, 
                                                (entrega(_,IdEst,_,_,D/H,_), \+(D=empty), 
                                        checkTimeInterval(D,H,date(AnoI,MesI,DiaI),time(Hi,Mi,0),date(AnoF,MesF,DiaF),time(Hf,Mf,0))),
                                        Aux), listToPairList(Aux,[],R).
% ---------------------------------------


% ---------------------------------------------------------------------------------------------------------             
% --------- calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo;
% ---------------------------------------------------------------------------------------------------------             

% dizer que encomendas nao entregues para trás não contam.
numEncomendas(DateI, TimeI, DateF, TimeF,Answer) :- findall(1 ,(entrega(_,_,_,Data1/Time1,empty/empty,_),
                                                                checkTimeInterval(Data1,Time1,DateI,TimeI,DateF,TimeF)), 
                                                        Bag1),
                                          length(Bag1, NrNaoEntregue),
                                          findall(Data/Time,(entrega(_,_,_,_,Data/Time,_), \+(Data = empty), \+(Time = empty),
                                          checkTimeInterval(Data,Time,DateI,TimeI,DateF,TimeF)),Bag2),
                                          length(Bag2, NrEntregue), 
                                        Answer = [(entregues, NrEntregue), (nao_entregues, NrNaoEntregue)].
% ---------------------------------------------------------------------------------------------------------             



% ---------------------------------------------------------------------------------------------------------             
% ------ calcular o peso total transportado por estafeta num determinado dia.
% ---------------------------------------------------------------------------------------------------------             
pesoNumDia(IdEstafeta, Dia/Mes/Ano, Answer) :- findall(IdEnc,
                                                (entrega(IdEnc,IdEstafeta,_,Di/_,Df/_,_), \+(Df=empty),
                                                dateStamp(Di,date(Ano,Mes,Dia),Dif1), Dif1 >= 0,
                                                dateStamp(Df,date(Ano,Mes,Dia),Dif2), Dif2 =< 0),
                                                Ans), calcPesoEnc(Ans,0,Answer).

calcPesoEnc([],Acc,Acc).
calcPesoEnc([IdEnc|T],Acc,R) :- encomenda(_,IdEnc, _, Peso, _, _ ,_), Acc2 is Acc + Peso, calcPesoEnc(T,Acc2,R).
% ---------------------------------------------------------------------------------------------------------             



replace_existing_fact(OldFact, NewFact) :-
    retract(OldFact),
    assert(NewFact).



% ----------------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------------             
% ------ FASE 2
% ---------------------------------------------------------------------------------------------------------             

% ---------Gerar os circuitos de entrega, caso existam, que cubram um determinado território (e.g. rua ou freguesia);
% ---------Representação dos diversos pontos de entrega em forma de grafo, tendo em conta que apenas se devem ter localizações (rua e/ou freguesia) disponíveis;
% ---------Identificar quais os circuitos com maior número de entregas (por volume e peso);
% ---------Comparar circuitos de entrega tendo em conta os indicadores de produtividade;
% ---------Escolher o circuito mais rápido (usando o critério da distância);
% ---------Escolher o circuito mais ecológico (usando um critério de tempo);







getMoradasPeso([], []).
getMoradasPeso([IdEnc|T], L) :- encomenda(registada, IdEnc, _, Peso, _, Morada1, _), 
                                getMoradasPeso(T, L2), 
                                append(Morada1/[IdEnc/Peso], L2, L).
getMoradasPeso([H|T], X) :- getMoradasPeso(T, X).

joinMoradas1([], L).
joinMoradas1([Morada/Info| T], L) :- \+member((Morada/_), T), joinMoradas1(T, Y), append(Morada/Info, Y, L).
joinMoradas1([Morada/Info| T], L) :- add1(Morada/Info, T, L). 

add1(Morada/Info, [], [Morada/Info]).
add1(Morada/Info1, [Morada/Info2|T], L) :- append(Info1, Info2, Info), [Info|L].
add1(Morada/Info, [H|T], L) :- add1(Morada/Info, T, Y), append(H, Y).


joinMoradas([] , L , L).
joinMoradas([Morada/H|T], Acc, X) :- adicionaMorada(Morada/H, Acc,[],Res).
                                     joinMoradas(T,Res,X).


adicionaMorada(Morada/H, [], Acc, [Morada/H|Acc]).
adicionaMorada(Morada/H1, [Morada/H2 | T], Acc, X) :- append(H1,H2,Res),
                                                      append(Morada/Res, T ,R),
                                                      append(R,Acc,X).
adicionaMorada(H1 , [H2 | T], Acc,X) :- append(H2,Acc,Res),
                                        adicionaMorada(H1,T,Res,X).
                     
moradaPertence(_, []) :- false.
moradaPertence(Morada,[Morada/_ | T]).
moradaPertence(Morada,[Morada1/_ | T]) :- moradaPertence(Morada,T). 

separarVoltas(L, X) :- encomenda(registada, IdEnc, _, Peso, _, _, _)setof(L, Bag), separarVoltas(Bag, X, 0).
separarVoltas([],L, _).
separarVoltas([encomenda(registada, IdEnc, _, Peso, _, _, _)|T], [L1|L], TotUsed) :- 
                (TotUsed+Peso > 100) -> separarVoltas(T, [L2|L], 0);
                                        append(IdEnc,L2,L1), separarVoltas(T, [L2|L], TotUsed+Peso).

% makeCircuito(IdEstafeta) :-
        % findall(IdEnc, entrega(IdEnc, IdEstafeta, _, _, _, empty), Bag),
        % getMoradasPeso(Bag, Bag1),
        % joinMoradas(Bag1, Bag2),
        % separarVoltas(Bag, RET),
        % getTotalPeso(Bag, X).
        % ((X>100, darSplit, !); (X>20, veiculo é carro ); (X>5, veiculo é mota ); (X>=0, veiculo é bicicleta)),
        % alterarEstadoParaEmDistribuicao,

        % resolve_aestrela().


%---------------------------------pesquisa a estrela 

resolve_aestrela(Nodo,CaminhoDistancia/CustoDist) :-
	estima(Nodo, goal(X), EstimaD),
	aestrela_distancia([[Nodo]/0/EstimaD], InvCaminho/CustoDist/_),
	inverso(InvCaminho, CaminhoDistancia).


aestrela_distancia(Caminhos, Caminho) :-
	obtem_melhor_distancia(Caminhos, Caminho),
	Caminho = [Nodo|_]/_/_,goal(Nodo).
aestrela_distancia(Caminhos, SolucaoCaminho) :-
	obtem_melhor_distancia(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrela_distancia(MelhorCaminho, ExpCaminhos),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
        aestrela_distancia(NovoCaminhos, SolucaoCaminho).	


obtem_melhor_distancia([Caminho], Caminho) :- !.
obtem_melhor_distancia([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
	Custo1 + Est1 =< Custo2 + Est2, !,
	obtem_melhor_distancia([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho). 
obtem_melhor_distancia([_|Caminhos], MelhorCaminho) :- 
	obtem_melhor_distancia(Caminhos, MelhorCaminho).
	

expande_aestrela_distancia(Caminho, ExpCaminhos) :-
	findall(NovoCaminho, adjacente_distancia(Caminho,NovoCaminho), ExpCaminhos).


adjacente_distancia([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/EstDist) :-
	grafo(Nodo, ProxNodo, PassoCustoDist),
	\+ member(ProxNodo, Caminho),
	NovoCusto is Custo + PassoCustoDist,
	estima(ProxNodo, goal(X), EstDist).


%---------------------------------
inverso(Xs, Ys):-
	inverso(Xs, [], Ys).
inverso([], Xs, Xs).
inverso([X|Xs],Ys, Zs):-
	inverso(Xs, [X|Ys], Zs).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E, Xs, Ys).




















% A* algorithm
% author: Jorge Carlos Valverde Rebaza
% date: 02/09/2011

getEstima(Origem, Destino, Distancia):- estima(Origem, Destino, Distancia).
getEstima(Origem, Destino, Distancia):- !, estima(Destino, Origem, Distancia).


% regla principal del programa
encontrarCaminho(Origem, Destino):-
        localizacao(O,Origem),
        localizacao(D,Destino),
        buscaHeuristica([[0,O]],CaminhoInv,D),
        inverso(CaminhoInv, Caminho),
        imprimirCaminho(Caminho). % em vez de imprimir guardar
encontrarCaminho(_,_):-
        write("Caminho inexiste.").


% condição de parada de busqueda heuristica
buscaHeuristica(Caminhos, [Custo,Destino|Caminho], Destino):-
        member([Custo,Destino|Caminho],Caminhos),
        escolherProximo(Caminhos, [Custo1|_], Destino),
        Custo1 == Custo.
buscaHeuristica(Caminhos, Solucao, Destino):-
        escolherProximo(Caminhos, Prox, Destino),
        removerCaminho(Prox, Caminhos, CaminhosRestantes),
        procurarProx(Prox, NovosCaminhos),
        concatenarCaminhos(CaminhosRestantes, NovosCaminhos, ListaCompleta),
        buscaHeuristica(ListaCompleta, Solucao, Destino).

concatenarCaminhos([],L,L).
concatenarCaminhos([X|Y],L,[X|Lista]):- concatenarCaminhos(Y,L,Lista).

% Obtiene todos los caminos recorridos hasta el momento 
% y realiza las comparaciones. Solo se detiene cuando se encuentra
% el menor camino (menor costo)
escolherProximo([X],X,_):-!.
escolherProximo([[Custo1,X1|R1],[Custo2,X2|_]|T], MelhorCaminho, Destino):-
        getEstima(X1, Destino, Av1),
        getEstima(X2, Destino, Av2),
        Av1+Custo1 =< Av2+Custo2,
        escolherProximo([[Custo1,X1|R1]|T], MelhorCaminho, Destino).
escolherProximo([[Custo1,X1|_],[Custo2,X2|Resto2]|T], MelhorCaminho, Destino):-
        getEstima(X1, Destino, Av1),
        getEstima(X2, Destino, Av2),
        Av1+Custo1 > Av2+Custo2,
        escolherProximo([[Custo2,X2|Resto2]|T], MelhorCaminho, Destino).

procurarProx([Custo,N|Caminho],NovosCaminhos):-
        findall([Custo,NovoN,N|Caminho], (verificarMovimento(N, NovoN,_), \+(member(NovoN,Caminho))), L),
        atualizarCustosCaminhos(L, NovosCaminhos).

% Actualizacion de los costos de los caminos
atualizarCustosCaminhos([],[]):- !.
atualizarCustosCaminhos([[Custo,NovoN,N|Caminho]|T],[[NovoCusto,NovoN,N|Caminho]|T1]):-
        verificarMovimento(N, NovoN, Distancia),
        NovoCusto is Custo + Distancia,
        atualizarCustosCaminhos(T,T1).

verificarMovimento(Origem, Destino, Distancia):-
        grafo(Origem, Destino, Distancia).
verificarMovimento(Origem, Destino, Distancia):-
        grafo(Destino, Origem, Distancia).
 
removerCaminho(X,[X|T],T):-!.
removerCaminho(X,[Y|T],[Y|T2]):-removerCaminho(X,T,T2).






% imprimir el resultado
imprimirCaminho([Custo]):-
        nl,
        write('La distancia total recorrida fue de: '),
        write(Custo),
        write(' kilometros.').
imprimirCaminho([CiudadActual|T]):-
        localizacao(CiudadActual, X),
        write(X),
        write(', '),
        imprimirCaminho(T).


imprimirCaminho([CiudadActual|Cola]):- 
        write('Camino a recorrer: '),
        imprimirCaminho([CiudadActual|Cola]).


% Para ejecutar el programa se debe utilizar la función encontrarCamino(), por ejemplo:

% encontrarCamino('Mehadia' , 'Bucharest').
% Camino a recorrer: Mehadia, Dobreta, Craiova, Pitesti, Bucharest
% La distancia total recorrida fue de: 434 kilometros.
