%find suitable placements, assigning nodes to functions resolving the costraints
placement(Orchestration, GeneratorId, Placement) :-
	eventGenerator(GeneratorId, _,GeneratorNode),
	placement(Orchestration, [GeneratorNode], _,[], _, Placement).%[] is starting empty placement

%ft case
placement(ft(F, FType,FServices,RequiredLatency), PreviousNodes, [N], OldPlacement, [on(F,N,HWReqs)|OldPlacement], fp(F, FType, SWReqs, HWReqs,FServicesBinding,N)):-
	checkPreviousNodesLat(PreviousNodes, N, RequiredLatency),
	compatibleNodeType(FType,N),
	getNode(N, SWCaps, HWCaps),
	functionReqs(F, SWReqs, HWReqs, FServicesReqs),
    canHost(SWReqs, HWReqs, SWCaps, HWCaps),
	hwReqsOK(HWReqs, HWCaps, N, OldPlacement),
    bindServices(N, FServices, FType, FServicesReqs, FServicesBinding).
%fpad case
placement(fpad(F,FType,FServices,RequiredLatency,SWReqs,HWReqs,FServicesReqs), PreviousNodes, [N], OldPlacement,[on(F,N,HWReqs)|OldPlacement], fp(F, FType,SWReqs, HWReqs,FServicesBinding,N)):-
	checkPreviousNodesLat(PreviousNodes, N, RequiredLatency),
	compatibleNodeType(FType,N),
	getNode(N, SWCaps, HWCaps),
    canHost(SWReqs, HWReqs, SWCaps, HWCaps),
	hwReqsOK(HWReqs, HWCaps, N, OldPlacement),
    bindServices(N, FServices, FType, FServicesReqs, FServicesBinding).
%seq case
placement(seq(S1,S2), PreviousNodes, LastNodesS2, OldPlacement, PlacementS2, seq(P1,P2)):-
	placement(S1,PreviousNodes, LastNodesS1,OldPlacement, PlacementS1, P1),
	placement( S2,LastNodesS1, LastNodesS2,PlacementS1, PlacementS2, P2).
%seqif case
placement(seqIf(S1,S2), PreviousNodes, LastNodesS2, OldPlacement, PlacementS2, seqIf(P1,P2)):-
	placement(S1,PreviousNodes, LastNodesS1,OldPlacement, PlacementS1, P1),
	placement(S2,LastNodesS1, LastNodesS2,PlacementS1, PlacementS2, P2).
%if case
placement(if(G,L,R), PreviousNodes, LastNodesIF, OldPlacement, PlacementR, if(PG,PL,PR)):-
	placement(G,PreviousNodes, LastNodesG,OldPlacement, PlacementG, PG),
	placement(L,LastNodesG, LastNodesL,PlacementG, PlacementL, PL),
	placement(R,LastNodesG, LastNodesR,PlacementL, PlacementR, PR),
	append(LastNodesL, LastNodesR, LastNodesIF).

%par case
placement(par(Par), PreviousNodes, NewLastNodesPar, OldPlacement, NewPlacement, par(Placement)):-
	placementPar(Par, PreviousNodes, NewLastNodesPar, OldPlacement, NewPlacement, Placement).

placementPar([F|FList], PreviousNodes, NewLastNodesPar, OldPlacement, NewPlacement, [Pf|PFList]):-
	placement(F,PreviousNodes, LastNodesF,OldPlacement, Placement, Pf),
	placementPar(FList,PreviousNodes, LastNodesPar,Placement, NewPlacement, PFList),
	append(LastNodesF, LastNodesPar, NewLastNodesPar).
placementPar([], _, [], Placement, Placement, []).


compatibleNodeType(Ftype,N) :- nodeLabel(N, Ntype), compatible(Ftype, Ntype).
compatibleServiceType(Ftype,S,Stype) :- serviceLabel(S,Stype,Slabel), compatible(Ftype, Slabel).
compatible(T,T).
compatible(T1,T2) :- dif(T1,T2), lattice_higherThan(T2, T1).


bindServices(Node, InstanceBindings, FType, [(SType,RequiredLatency)|ReqList], [(SType,S,ServiceNode)|Binding]):-
	getService(S, SType, ServiceNode, InstanceBindings),
	link(Node, ServiceNode, FeaturedLatency), FeaturedLatency =< RequiredLatency,
	compatibleServiceType(FType, S, SType),
	compatibleNodeType(FType, ServiceNode),
	getNode(ServiceNode, _,_),
	bindServices(Node, InstanceBindings, FType, ReqList, Binding).
bindServices(_, _, _, [], []).

