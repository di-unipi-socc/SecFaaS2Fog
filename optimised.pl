%find suitable placements, assigning nodes to functions resolving the costraints
placementOpt(Times,Orchestration, GeneratorId, Placement) :-
	eventGenerator(GeneratorId, _,GeneratorNode),
	once(compatibleList(CList)),
	placementOpt(Times,CList, Orchestration, [GeneratorNode], _,[], _, Placement).%[] is starting empty placement

%ft case
placementOpt(Times,CList, ft(F, FType,FServices,RequiredLatency), PreviousNodes, [N], OldPlacement, [on(F,N,HWReqs)|OldPlacement], fp(F, FType, SWReqs, HWReqs,FServicesBinding,N)):-
	checkPreviousNodesLat(PreviousNodes, N, RequiredLatency),
	compatibleNodeType(CList, FType,N),
	getNode(N, SWCaps, HWCaps),
	functionReqs(F, SWReqs, HWReqs, FServicesReqs),
    canHost(SWReqs, HWReqs, SWCaps, HWCaps),
	hwReqsOK(HWReqs, HWCaps, N, OldPlacement),
    once(bindServices(CList,N, FServices, FType, FServicesReqs, FServicesBinding)),
	checkTime(Times).
%fpad case
placementOpt(Times,CList, fpad(F,FType,FServices,RequiredLatency,SWReqs,HWReqs,FServicesReqs), PreviousNodes, [N], OldPlacement,[on(F,N,HWReqs)|OldPlacement], fp(F, FType,SWReqs, HWReqs,FServicesBinding,N)):-
	checkPreviousNodesLat(PreviousNodes, N, RequiredLatency),
	compatibleNodeType(CList, FType,N),
	getNode(N, SWCaps, HWCaps),
    canHost(SWReqs, HWReqs, SWCaps, HWCaps),
	hwReqsOK(HWReqs, HWCaps, N, OldPlacement),
    once(bindServices(CList,N, FServices, FType, FServicesReqs, FServicesBinding)),
	checkTime(Times).
%seq case
placementOpt(Times,CList, seq(S1,S2), PreviousNodes, LastNodesS2, OldPlacement, PlacementS2, seq(P1,P2)):-
	placementOpt(Times,CList, S1,PreviousNodes, LastNodesS1,OldPlacement, PlacementS1, P1),
	placementOpt(Times,CList, S2,LastNodesS1, LastNodesS2,PlacementS1, PlacementS2, P2).
%seqif case
placementOpt(Times,CList, seqIf(S1,S2), PreviousNodes, LastNodesS2, OldPlacement, PlacementS2, seqIf(P1,P2)):-
	placementOpt(Times,CList, S1,PreviousNodes, LastNodesS1,OldPlacement, PlacementS1, P1),
	placementOpt(Times,CList, S2,LastNodesS1, LastNodesS2,PlacementS1, PlacementS2, P2).
%if case
placementOpt(Times,CList, if(G,L,R), PreviousNodes, LastNodesIF, OldPlacement, PlacementR, if(PG,PL,PR)):-
	placementOpt(Times,CList, G,PreviousNodes, LastNodesG,OldPlacement, PlacementG, PG),
	placementOpt(Times,CList, L,LastNodesG, LastNodesL,PlacementG, PlacementL, PL),
	placementOpt(Times,CList, R,LastNodesG, LastNodesR,PlacementL, PlacementR, PR),
	append(LastNodesL, LastNodesR, LastNodesIF).

%par case
placementOpt(Times,CList, par(Par), PreviousNodes, NewLastNodesPar, OldPlacement, NewPlacement, par(Placement)):-
	placementOptPar(Times,CList, Par, PreviousNodes, NewLastNodesPar, OldPlacement, NewPlacement, Placement).

placementOptPar(Times,CList, [F|FList], PreviousNodes, NewLastNodesPar, OldPlacement, NewPlacement, [Pf|PFList]):-
	placementOpt(Times,CList, F,PreviousNodes, LastNodesF,OldPlacement, Placement, Pf),
	placementOptPar(Times,CList, FList,PreviousNodes, LastNodesPar,Placement, NewPlacement, PFList),
	append(LastNodesF, LastNodesPar, NewLastNodesPar).
placementOptPar(_,_,[], _, [], Placement, Placement, []).


%compatibleNodeType(Ftype,N) :- nodeLabel(N, Ntype), compatible(Ftype, Ntype).
%compatibleServiceType(Ftype,S,Stype) :- serviceLabel(S,Stype,Slabel), compatible(Ftype, Slabel).
%compatible(T,T).
%compatible(T1,T2) :- dif(T1,T2), lattice_higherThan(T2, T1).
compatibleNodeType(CList, Ftype,N) :- nodeLabel(N,Ntype), once(member((Ntype,Ftype),CList)).
compatibleServiceType(CList,Ftype,S,Stype) :- serviceLabel(S,Stype,Slabel), once(member((Slabel,Ftype),CList)).



bindServices(CList,Node, InstanceBindings, FType, [(SType,RequiredLatency)|ReqList], [(SType,S,ServiceNode)|Binding]):-
	getService(S, SType, ServiceNode, InstanceBindings),
	link(Node, ServiceNode, FeaturedLatency), FeaturedLatency =< RequiredLatency,
	compatibleServiceType(CList,FType, S, SType),
	compatibleNodeType(CList,FType, ServiceNode),
	getNode(ServiceNode, _,_),
	bindServices(CList,Node, InstanceBindings, FType, ReqList, Binding).
bindServices(_,_, _, _, [], []).



%bindServicesBlob(_,[],_,_,[]).
%bindServicesBlob([(F,Bindings)|FList], N, FType, [(F,FServicesBinding,N)|PlacedFunctions]):-
%	functionReqs(F, _, _, FServicesReqs),
%    bindServices(N, Bindings, FType, FServicesReqs, FServicesBinding),
%	bindServicesBlob(FList, N, FType, PlacedFunctions).

compatibleList([(Lowest,Lowest)|CList]):-
	findall(T, g_lattice_higherThan(T,_),Labels),
	list_to_set(Labels, LabelsSet),
	buildClist(LabelsSet,CList),
	lowestType(Lowest).

buildClist([L|Ls],[(L,L)|CList]):-
	findall((L,B), lattice_higherThan(L,B),Res),
	list_to_set(Res,SetRes),
	buildClist(Ls,SubRes),
	append(SetRes,SubRes,CList).
buildClist([],[]).

logPlacement([on(F,N,_)|OldPlacement]):-
	writef('on(%t,%t) ',[F,N]),
	logPlacement(OldPlacement).
logPlacement([]):- writef(' E\n').

checkTime((Start,Max)):-
	get_time(Now),
	Time is Now - Start,
	(Time =< Max;!,fail).
checkTime().