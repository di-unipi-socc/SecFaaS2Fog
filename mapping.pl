%find suitable placements, assigning nodes to functions resolving the costraints
placement(Orchestration, GeneratorId, Placement) :-
	eventGenerator(GeneratorId, _,GeneratorNode),
	compatibleList(CList),
	placement(CList, Orchestration, [GeneratorNode], _,[], _, Placement).%[] is starting empty placement

%ft case
placement(CList, ft(F, FType,FServices,RequiredLatency), PreviousNodes, [N], OldPlacement, [on(F,N,HWReqs)|OldPlacement], fp(F, FType, SWReqs, HWReqs,FServicesBinding,N)):-
	checkPreviousNodesLat(PreviousNodes, N, RequiredLatency),
	compatibleNodeType(CList, FType,N),
	getNode(N, SWCaps, HWCaps),
	functionReqs(F, SWReqs, HWReqs, FServicesReqs),
    canHost(SWReqs, HWReqs, SWCaps, HWCaps),
	hwReqsOK(HWReqs, HWCaps, N, OldPlacement),
    once(bindServices(CList,N, FServices, FType, FServicesReqs, FServicesBinding)).
%fpad case
placement(CList, fpad(F,FType,FServices,RequiredLatency,SWReqs,HWReqs,FServicesReqs), PreviousNodes, [N], OldPlacement,[on(F,N,HWReqs)|OldPlacement], fp(F, FType,SWReqs, HWReqs,FServicesBinding,N)):-
	checkPreviousNodesLat(PreviousNodes, N, RequiredLatency),
	compatibleNodeType(CList, FType,N),
	getNode(N, SWCaps, HWCaps),
    canHost(SWReqs, HWReqs, SWCaps, HWCaps),
	hwReqsOK(HWReqs, HWCaps, N, OldPlacement),
    once(bindServices(CList,N, FServices, FType, FServicesReqs, FServicesBinding)).
%seq case
placement(CList, seq(S1,S2), PreviousNodes, LastNodesS2, OldPlacement, PlacementS2, seq(P1,P2)):-
	placement(CList, S1,PreviousNodes, LastNodesS1,OldPlacement, PlacementS1, P1),
	placement(CList, S2,LastNodesS1, LastNodesS2,PlacementS1, PlacementS2, P2).
%seqif case
placement(CList, seqIf(S1,S2), PreviousNodes, LastNodesS2, OldPlacement, PlacementS2, seqIf(P1,P2)):-
	placement(CList, S1,PreviousNodes, LastNodesS1,OldPlacement, PlacementS1, P1),
	placement(CList, S2,LastNodesS1, LastNodesS2,PlacementS1, PlacementS2, P2).
%if case
placement(CList, if(G,L,R), PreviousNodes, LastNodesIF, OldPlacement, PlacementR, if(PG,PL,PR)):-
	placement(CList, G,PreviousNodes, LastNodesG,OldPlacement, PlacementG, PG),
	placement(CList, L,LastNodesG, LastNodesL,PlacementG, PlacementL, PL),
	placement(CList, R,LastNodesG, LastNodesR,PlacementL, PlacementR, PR),
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
placement(CList, par(Par), PreviousNodes, NewLastNodesPar, OldPlacement, NewPlacement, par(Placement)):-
	placementPar(CList, Par, PreviousNodes, NewLastNodesPar, OldPlacement, NewPlacement, Placement).

placementPar(CList, [F|FList], PreviousNodes, NewLastNodesPar, OldPlacement, NewPlacement, [Pf|PFList]):-
	placement(CList, F,PreviousNodes, LastNodesF,OldPlacement, Placement, Pf),
	placementPar(CList, FList,PreviousNodes, LastNodesPar,Placement, NewPlacement, PFList),
	append(LastNodesF, LastNodesPar, NewLastNodesPar).
placementPar(_,[], _, [], Placement, Placement, []).

getNode(N, SWCaps, HWCaps) :-
	node(N, _, _, _, SWCaps, HWCaps).

%compatibleNodeType(Ftype,N) :- nodeLabel(N, Ntype), compatible(Ftype, Ntype).
%compatibleServiceType(Ftype,S,Stype) :- serviceLabel(S,Stype,Slabel), compatible(Ftype, Slabel).
%compatible(T,T).
%compatible(T1,T2) :- dif(T1,T2), lattice_higherThan(T2, T1).
compatibleNodeType(CList, Ftype,N) :- nodeLabel(N,Ntype), member((Ntype,Ftype),CList).
compatibleServiceType(CList,Ftype,S,Stype) :- serviceLabel(S,Stype,Slabel), member((Slabel,Ftype),CList).

checkPreviousNodesLat([PrevNode|PrevNodes], ThisNode, RequiredLatency):-
	link(PrevNode, ThisNode, FeaturedLatency), FeaturedLatency =< RequiredLatency,
	checkPreviousNodesLat(PrevNodes, ThisNode, RequiredLatency).
checkPreviousNodesLat([],_,_).

canHost(SWReqs, (RAMReq,VCPUsReq,CPUReq), SWCaps, (RAMCap,VCPUsCap,CPUCap)):-
	subset(SWReqs, SWCaps),
	RAMCap >= RAMReq,
	VCPUsCap >= VCPUsReq,
	CPUCap >= CPUReq.

%TODO: check
bindServices(CList,Node, InstanceBindings, FType, [(SType,RequiredLatency)|ReqList], [(SType,S,ServiceNode)|Binding]):-
	getService(S, SType, ServiceNode, InstanceBindings),
	link(Node, ServiceNode, FeaturedLatency), FeaturedLatency =< RequiredLatency,
	compatibleServiceType(CList,FType, S, SType),
	compatibleNodeType(CList,FType, ServiceNode),
	getNode(ServiceNode, _,_),
	bindServices(CList,Node, InstanceBindings, FType, ReqList, Binding).
bindServices(_,_, _, _, [], []).

%getService(S, ServiceType, ServiceNode, []) :-	service(S, _, ServiceType, ServiceNode).
getService(S, ServiceType, ServiceNode, InstanceBindings) :-
	member(S, InstanceBindings), service(S, _, ServiceType, ServiceNode).

getService(S2, ServiceType, ServiceNode, InstanceBindings) :-
	\+((member(S1, InstanceBindings), service(S1, _, ServiceType, _))),
	service(S2, _, ServiceType, ServiceNode).

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
	buildClist(Ls,SubRes),
	append(Res,SubRes,CList).
buildClist([],[]).

logPlacement([on(F,N,_)|OldPlacement]):-
	writef('on(%t,%t) ',[F,N]),
	logPlacement(OldPlacement).
logPlacement([]):- writef(' E\n').