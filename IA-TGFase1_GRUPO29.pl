
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
validateDate(A, M, D) .

timeStamp(time(H1, M1, S1), time(H2, M2, S2), Days):-
        TimeStamp1 is (H1 + (M1/60) + (S1/60)/60)/24,
        TimeStamp2 is (H2 + (M1/60) + (S1/60)/60)/24,
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


        
        

% -----------




% ----------- veiculo(tipo, velocidadeMedia, cargaMax)
veiculo(bicicleta, 10, 5). %inserir preço por km ?? ou só na função do custo??
veiculo(mota, 35, 20).
veiculo(carro, 25, 100). 

% ----------- cliente(id, totalEncomendas) /2
cliente(daniel, 0).
cliente(abacao, 1).

% ----------- estafeta(id, totalEntregas, Media_avaliacao) /3
estafeta(joao, 0, 0).
estafeta(caldas, 1, 4.4).



% ----------- encomenda(estado, id, idCliente, peso, volume, freguesia/morada, Date(Y,M,D)/Time(H,M,S) (prazo)) /7
encomenda(entregue, cadeira_gayming, abacao, 1, 10, landim/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, landim/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, landim/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, landim/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, joane/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, joane/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, joane/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, requiao/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, silvares/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, silvares/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, silvares/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, silvares/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, landim/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, landim/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).
encomenda(entregue, cadeira_gayming, abacao, 1, 10, landim/rua_ponte, date(0, 0, 0)/time(12, 0, 0)).




% ----------- entrega(idEncomenda, idEstafeta, veiculo, DateI/TimeI, DateF/TimeF, avaliacao) /6
entrega(cadeira_gayming, caldas, mota, date(2001, 10, 5)/time(0, 0, 0), date(2001, 10, 6)/time(0, 0, 0), 4.4).


solucoes(X, Y, Z) :- findall(X, Y, Z).


% estados: 1->registada, 2->distribuição, 3->entregue


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


createCliente(Id) :- \+cliente(Id, _), assert(cliente(Id, 0)), write("Cliente adicionado"), !;
                        write("Cliente já existente").



                %fazer os teste de id da encomenda e assim antes, esxiste id cliente, estafeta e assim
createEncomenda(Id, IdCliente, Peso, Volume, Freguesia, Morada, Dias, Horas, Minutos) :- 
                encomenda(_, Id, _, _, _, _, _), write("Encomenda já existente"), !;
                \+cliente(IdCliente, NrEnc), write("Cliente não existente"), !;
                Peso > 100, write("Nenhum veiculo suporta a entrega da encomenda"), !;
                Peso < 0, write("Peso impossível"), !;
                Volume < 0, write("Volume impossível"), !;
                (Dias < 0; Horas < 0; Minutos < 0), write("Prazo impossível"), !; 
                assert(encomenda(registada, Id, IdCliente, Peso, Volume, Freguesia/Morada, date(0,0,Dias)/time(Horas,Minutos,0))),
                addCliente(IdCliente).
                    
                    


% só deve ser feita pelo sistema da empresa, é preciso certificar outra vez os campos??
createEntrega(IdEncomenda, IdEstafeta, Veiculo, DateI, TimeI, Date ,Time) :-
                \+encomenda(_, IdEncomenda, _, _, _, _, _, _, _), write("Encomenda não existente"), !;
                \+estafeta(IdEstafeta, _, _), write("Estafeta não existente"), !;
                encomenda(entregue, IdEncomenda, _, _, _, _, _), write("A encomenda já foi entregue"), !;
                encomenda(distribuicao, IdEncomenda, _, _, _, _, _, _, _), write("A encomenda já se encontra em distribuição"), !;
                
                encomenda(_, IdEncomenda, _, _, Peso, _, _, _, _ ), veiculo(Veiculo, _, Max), 
                            Max < Peso, write("Veículo selecionado não suporta carga da encomenda"), !;
                % adicionar info aos outros factos !!!!
                replace_existing_fact(encomenda(registada, IdEncomenda, IdCliente, IdEstafeta, Peso, Volume, Freguesia, Morada, Date/Time), 
                                    encomenda(registada, IdEncomenda, IdCliente, IdEstafeta, Peso, Volume, Freguesia, Morada, Date/Time)), 
                        assert(entrega(IdEncomenda, IdEstafeta, Veiculo, Inicio, empty, -1 )), 
                        write("Entrega registada").


entregarEncomenda(IdEncomenda, Dia, Mes, Ano, Horas, Minutos, Avaliacao) :-
        \+encomenda(_, IdEncomenda, _, _, _, _, _), write("Encomenda não existente"), !;
        encomenda(entregue, IdEncomenda, _, _, _, _, _), write("A encomenda já foi entregue"), !;
        encomenda(registada, IdEncomenda, _, _, _, _, _), write("A encomenda ainda não está em distribuição"), !;
        replace_existing_fact(encomenda(distribuicao, IdEncomenda, IdCliente, Peso, Volume, ADDRS, Prazo), 
                                encomenda(entregue, IdEncomenda, IdCliente, Peso, Volume, ADDRS, Prazo)), 
        replace_existing_fact(entrega(IdEncomenda, IdEstafeta, Veiculo1, DataInicio1, Null, PrevAv), 
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
% cliente entra onde ???? DUVIDA -> basicamente é a reverse da de baixo, atraves do cliente determina estafetas que o serviram
trackEncomenda(Id, Ans) :- \+encomenda(_, Id, _, _, _, _, _, _, _), write("Encomenda não existente"), !;
                        encomenda(entregue, Id, _, IdEstafeta, _, _, _, _, _),
                                write("A encomenda já foi entregue por: "), write(IdEstafeta), !;
                        encomenda(registada, Id, _, _, _, _, _, _, _),
                                write("A encomenda está registada, dentro de momentos irá ser distribuída"), !;
                        encomenda(distribuicao, Id, _, IdEstafeta, _, _, _, _, _).
% ---------------------------------------


% esta mal
% --------------------------------------- identificar os clientes servidos por um determinado estafeta;
findClientesServidosPorEstafeta(IdEstafeta, Answer) :- % resolver para quando nao existe, 
                \+estafeta(IdEstafeta, _, _), write("Estafeta não existente"), !;
                estafeta(IdEstafeta, Nr, _), Nr =:= 0, Answer = "O estafeta ainda não realizou entregas", !;
                setof(IdCliente, encomenda(entregue, A, IdCliente, IdEstafeta, B, C, D), Answer).
        
% ---------------------------------------


% --------------------------------------- calcular o valor faturado pela Green Distribution num determinado dia;

% ---------------------------------------


% ----------- encomenda(estado, id, idCliente, idEstafeta, peso, volume, freguesia, morada, prazo) /9
% --------------------------------------- identificar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution;
bestZonas(Top1, Top2, Top3) :- findall(Freguesia, encomenda(A, B, C, D, E, Freguesia/Null, F), Bag),
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

% ---------------------------------------


% --------------------------------------- identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo;

% ---------------------------------------


% --------------------------------------- calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo;

% ---------------------------------------


% --------------------------------------- calcular o peso total transportado por estafeta num determinado dia.
%pesoNumDia(IdEstafeta, Dia Answer) :- findall(Peso, ).
% ---------------------------------------









% write($X).


addCliente(Id) :- cliente(Id,Nr), NewNr is Nr + 1, replace_existing_fact(cliente(Id,Nr),cliente(Id,NewNr)).


addEstafeta(Id,Av) :- estafeta(Id,Nr,Media), NewNr is Nr + 1, arredondar(((Media*Nr + Av)/NewNr), NewMedia, 2),
                        replace_existing_fact(estafeta(Id,Nr,Media),estafeta(Id,NewNr,NewMedia)).


replace_existing_fact(OldFact, NewFact) :-
    retract(OldFact),
    assert(NewFact).

