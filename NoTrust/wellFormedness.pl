%clean the orchestration for the other phases
wellFormedTest(OrchId, NewOrchestration):- 
	functionOrch(OrchId, _, _, Orchestration),
	wellFormed(Orchestration, NewOrchestration).

wellFormed(fun(F,B,L),fun(F,B,L)).
%bring seq(A,seq(B,C)) to seq(seq(A,B),C)
wellFormed(seq(fun(F,B,L),seq(S1,S2)), seq(seq(fun(F,B,L),S1res),S2res)):-
	wellFormed(S1,S1res),
	wellFormed(S2,S2res).
wellFormed(seq(A,B), seq(Ares,Bres)):-
	\+ (A = fun(_,_,_),B = seq(_,_)),
	wellFormed(A,Ares),
	wellFormed(B,Bres).
wellFormed(par([A]),Ares):- wellFormed(A,Ares).
wellFormed(par([H|T]), par(ResPar)):-
	T \== [],
	wellFormedPar([H|T],ResPar).
wellFormed(if(fun(F,B,L),True,False), if(fun(F,B,L),ResT,ResF)):-
	wellFormed(True,ResT),
	wellFormed(False,ResF).

wellFormedPar([],[]).
wellFormedPar([P|Ps],[ResP|ResPs]):-
	P \== par(_),
	wellFormed(P,ResP),
	wellFormedPar(Ps,ResPs).
