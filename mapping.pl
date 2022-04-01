%find suitable placements, assigning nodes to functions resolving the costraints
placement(TypedBlobbedOrchestration, GeneratorId, Placement) :-
	eventGenerator(GeneratorId, GeneratorNode),
	placement(TypedBlobbedOrchestration, [GeneratorNode], _,[], _, Placement).%[] is starting empty placement

%placement(_, [], [], _, Placement, Placement, []).
%ft case
placement(ft(F, FType,FServices,RequiredLatency), PreviousNodes, [N], OldPlacement, [on(F,N,HWReqs)|OldPlacement], fp(F, FType, SWReqs, HWReqs,FServicesBinding,N)):-
	getNode(N, SWCaps, HWCaps),
	checkPreviousNodesLat(PreviousNodes, N, RequiredLatency),
	functionReqs(F, SWReqs, HWReqs, FServicesReqs),
    swReqsOK(SWReqs, SWCaps),
    hwReqsOK(HWReqs, HWCaps, N, OldPlacement),
    compatibleNodeType(FType,N),
    bindServices(N, FServices, FType, FServicesReqs, FServicesBinding).
%fpad case
placement(fpad(F,FType,FServices,RequiredLatency,SWReqs,HWReqs,FServicesReqs), PreviousNodes, [N], OldPlacement,[on(F,N,HWReqs)|OldPlacement], fp(F, FType,SWReqs, HWReqs,FServicesBinding,N)):-
	getNode(N, SWCaps, HWCaps),
	checkPreviousNodesLat(PreviousNodes, N, RequiredLatency),
	swReqsOK(SWReqs, SWCaps),
    hwReqsOK(HWReqs, HWCaps, N, OldPlacement),
    compatibleNodeType(FType,N),
    bindServices(N, FServices, FType, FServicesReqs, FServicesBinding).
%seq case
placement(seq(S1,S2), PreviousNodes, LastNodesS2, OldPlacement, PlacementS2, seq(P1,P2)):-
	placement(S1,PreviousNodes, LastNodesS1,OldPlacement, PlacementS1, P1),
	placement(S2,LastNodesS1, LastNodesS2,PlacementS1, PlacementS2, P2).
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

%blob case
/*
placement(blob(FList,FType,RequiredLatency, (SWReqs, HWReqs, _)), PreviousNodes, [N], OldPlacement, [on(FList,N,HWReqs)|OldPlacement], blob(PlacedFunctions,FType)):-
	getNode(N, SWCaps, HWCaps),
	checkPreviousNodesLat(PreviousNodes, N, RequiredLatency),
	swReqsOK(SWReqs, SWCaps),
    hwReqsOK(HWReqs, HWCaps, N, OldPlacement),
    compatibleNodeType(FType,N),
	bindServicesBlob(FList, N, FType, PlacedFunctions).*/

%par case
placement(par(Par), PreviousNodes, NewLastNodesPar, OldPlacement, NewPlacement, par(Placement)):-
	placementPar(Par, PreviousNodes, NewLastNodesPar, OldPlacement, NewPlacement, Placement).

placementPar([], _, [], Placement, Placement, []).
placementPar([F|FList], PreviousNodes, NewLastNodesPar, OldPlacement, NewPlacement, [Pf|PFList]):-
	placement(F,PreviousNodes, LastNodesF,OldPlacement, Placement, Pf),
	placementPar(FList,PreviousNodes, LastNodesPar,Placement, NewPlacement, PFList),
	append(LastNodesF, LastNodesPar, NewLastNodesPar).

getNode(N, SWCaps, HWCaps) :-
	node(N, _, _, SWCaps, HWCaps).

compatibleNodeType(Ftype,N) :- nodeLabel(N, Ntype), compatible(Ftype, Ntype).
compatibleServiceType(Ftype,S,Stype) :- serviceLabel(S,Stype,Slabel), compatible(Ftype, Slabel).
compatible(T,T).
compatible(T1,T2) :- dif(T1,T2), lattice_higherThan(T2, T1).

checkPreviousNodesLat([],_,_).
checkPreviousNodesLat([PrevNode|PrevNodes], ThisNode, RequiredLatency):-
	link(PrevNode, ThisNode, FeaturedLatency), FeaturedLatency =< RequiredLatency,
	checkPreviousNodesLat(PrevNodes, ThisNode, RequiredLatency).

bindServices(_, _, _, [], []).
bindServices(Node, InstanceBindings, FType, [(SType,RequiredLatency)|ReqList], [(SType,S,ServiceNode)|Binding]):-
	getService(S, SType, ServiceNode, InstanceBindings),
	compatibleServiceType(FType, S, SType),
	compatibleNodeType(FType, ServiceNode),
	link(Node, ServiceNode, FeaturedLatency), FeaturedLatency =< RequiredLatency,
	getNode(ServiceNode, _,_),
	bindServices(Node, InstanceBindings, FType, ReqList, Binding).


%getService(S, ServiceType, ServiceNode, []) :-	service(S, _, ServiceType, ServiceNode).
getService(S, ServiceType, ServiceNode, InstanceBindings) :-
	member(S, InstanceBindings), service(S, _, ServiceType, ServiceNode).

getService(S2, ServiceType, ServiceNode, InstanceBindings) :-
	\+((member(S1, InstanceBindings), service(S1, _, ServiceType, _))),
	service(S2, _, ServiceType, ServiceNode).

bindServicesBlob(_,[],_,_,[]).
bindServicesBlob([(F,Bindings)|FList], N, FType, [(F,FServicesBinding,N)|PlacedFunctions]):-
	functionReqs(F, _, _, FServicesReqs),
    bindServices(N, Bindings, FType, FServicesReqs, FServicesBinding),
	bindServicesBlob(FList, N, FType, PlacedFunctions).