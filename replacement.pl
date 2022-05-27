%replacment starting from Fstart
replacement(MaxTime, Fstart,StartingNodes,OrchId, Placement):-
	functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration),
	replace(Fstart,PadOrchestration, CuttedOrchestration),
	compatibleList(CList),
	get_time(Start),
	placementOpt((Start,MaxTime),CList, CuttedOrchestration, StartingNodes, _,[], _, Placement).

%cut the orchestration until Fstart
replace(Fstart,ft(Fstart, FType,FServices,RequiredLatency),ft(Fstart, FType,FServices,RequiredLatency)).
replace(Fstart,ft(F, _,_,_),[]):-
	dif(Fstart,F).

replace(Fstart,fpad(Fstart,FType,FServices,RequiredLatency,SWReqs,HWReqs,FServicesReqs),fpad(Fstart,FType,FServices,RequiredLatency,SWReqs,HWReqs,FServicesReqs)).
replace(Fstart,fpad(F,_,_,_,_,_,_),[]):-
	dif(Fstart,F).

replace(Fstart,seq(S1,S2),seq(S1res,S2)):-
	replace(Fstart,S1,S1res),
	dif(S1res,[]).
replace(Fstart,seq(S1,S2),S2res):-
	replace(Fstart,S1,[]),
	replace(Fstart,S2,S2res),
	dif(S2res,[]).
replace(Fstart,seq(S1,S2),[]):-
	replace(Fstart,S1,[]),
	replace(Fstart,S2,[]).

replace(Fstart,if(G,True,False), if(ResG,True,False)):-
	replace(Fstart, G, ResG),
	dif(ResG,[]).
replace(Fstart,if(G,True,_), ResT):-
	replace(Fstart, G, []),
	replace(Fstart,True,ResT),
	dif(ResT,[]).
replace(Fstart,if(G,True,False), ResF):-
	replace(Fstart, G, []),
	replace(Fstart,True,[]),
	replace(Fstart,False,ResF),
	dif(ResF,[]).
replace(Fstart,if(G,True,False), []):-
	replace(Fstart, G, []),
	replace(Fstart,True,[]),
	replace(Fstart,False,[]).

replace(Fstart, par(ParList), ResPar):-
	replacePar(Fstart, ParList, ResPar),
	dif(ResPar,[]).
replace(Fstart, par(ParList), []):-
	replacePar(Fstart, ParList, []).

replacePar(Fstart, [P|_],Pres):-
	replace(Fstart, P, Pres),
	dif(Pres,[]).
replacePar(Fstart, [P|Ps],ResPar):-
	replace(Fstart, P, []),
	replacePar(Fstart, Ps, ResPar).
replacePar(_,[],[]).
