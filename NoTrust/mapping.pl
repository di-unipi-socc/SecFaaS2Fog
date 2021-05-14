mapping(AppOp, TypedBlobbedOrchestration, GeneratorNode, Placement) :-
	mapping(AppOp, TypedBlobbedOrchestration, [GeneratorNode], _,[], _, Placement).

%mapping(_, [], [], _, AllocHW, AllocHW, []).
%ft case
mapping(AppOp, ft(F, FType,FServices,RequiredLatency), PreviousNodes, [N], OldAllocHW, NewAllocHW, fp(F, FType,FServicesBinding,N)):-
	getNode(AppOp, N, SWCaps, HWCaps),
	checkPreviousNodesLat(PreviousNodes, N, RequiredLatency),
	%link(PreviousNode, N, FeaturedLatency), FeaturedLatency =< RequiredLatency,
	functionReqs(F, SWReqs, HWReqs, FServicesReqs),
    swReqsOK(SWReqs, SWCaps),
    hwReqsOK(HWReqs, HWCaps, N, OldAllocHW, NewAllocHW),
    compatibleNodeType(FType,N),
    bindServices(AppOp, N, FServices, FType, FServicesReqs, FServicesBinding).

%seq case
mapping(AppOp, seq(S1,S2), PreviousNodes, LastNodesS2, OldAllocHW, AllocHWS2, seq(P1,P2)):-
	mapping(AppOp,S1,PreviousNodes, LastNodesS1,OldAllocHW, AllocHWS1, P1),
	mapping(AppOp,S2,LastNodesS1, LastNodesS2,AllocHWS1, AllocHWS2, P2).
%seqif case
mapping(AppOp, seqIf(S1,S2), PreviousNodes, LastNodesS2, OldAllocHW, AllocHWS2, seqIf(P1,P2)):-
	mapping(AppOp,S1,PreviousNodes, LastNodesS1,OldAllocHW, AllocHWS1, P1),
	mapping(AppOp,S2,LastNodesS1, LastNodesS2,AllocHWS1, AllocHWS2, P2).
%blob case
mapping(AppOp, blob(FList,FType,RequiredLatency, (SWReqs, HWReqs, _)), PreviousNodes, [N], OldAllocHW, NewAllocHW, blob(PlacedFunctions,FType)):-
	getNode(AppOp, N, SWCaps, HWCaps),
	checkPreviousNodesLat(PreviousNodes, N, RequiredLatency),
	swReqsOK(SWReqs, SWCaps),
    hwReqsOK(HWReqs, HWCaps, N, OldAllocHW, NewAllocHW),
    compatibleNodeType(FType,N),
	bindServicesBlob(AppOp, FList, N, FType, PlacedFunctions).

%par case
mapping(AppOp, par(Par), PreviousNodes, NewLastNodesPar, OldAllocHW, NewAllocHW, par(Placement)):-
	mappingPar(AppOp,Par, PreviousNodes, NewLastNodesPar, OldAllocHW, NewAllocHW, Placement).

mappingPar(_, [], _, [], AllocHW, AllocHW, []).
mappingPar(AppOp, [F|FList], PreviousNodes, NewLastNodesPar, OldAllocHW, NewAllocHW, [Pf|PFList]):-
	mapping(AppOp,F,PreviousNodes, LastNodesF,OldAllocHW, AllocHW, Pf),
	mappingPar(AppOp,FList,PreviousNodes, LastNodesPar,AllocHW, NewAllocHW, PFList),
	append(LastNodesF, LastNodesPar, NewLastNodesPar).

getNode(_, N, SWCaps, HWCaps) :-
	node(N, _, _, SWCaps, HWCaps).

compatibleNodeType(Ftype,N) :- assignNodeLabel(N, Ntype), compatible(Ftype, Ntype).
compatibleServiceType(Ftype,S,Stype) :- assignServiceLabel(S,Stype,Slabel), compatible(Ftype, Slabel).
compatible(T,T).
compatible(T1,T2) :- dif(T1,T2), lattice_higherThan(T2, T1).

checkPreviousNodesLat([],_,_).
checkPreviousNodesLat([PrevNode|PrevNodes], ThisNode, RequiredLatency):-
	link(PrevNode, ThisNode, FeaturedLatency), FeaturedLatency =< RequiredLatency,
	checkPreviousNodesLat(PrevNodes, ThisNode, RequiredLatency).

bindServices(_, _, _, _, [], []).

bindServices(AppOp, Node, InstanceBindings, FType, [(SType,RequiredLatency)|ReqList], [(SType,S,ServiceNode)|Binding]):-
	getService(AppOp, S, SType, ServiceNode, InstanceBindings),
	compatibleServiceType(FType, S, SType),
	compatibleNodeType(FType, ServiceNode),
	link(Node, ServiceNode, FeaturedLatency), FeaturedLatency =< RequiredLatency,
	getNode(AppOp, ServiceNode, _,_),
	bindServices(AppOp, Node, InstanceBindings, FType, ReqList, Binding).


%getService(_, S, ServiceType, ServiceNode, []) :-	service(S, _, ServiceType, ServiceNode).
getService(_, S, ServiceType, ServiceNode, InstanceBindings) :-
	member(S, InstanceBindings), service(S, _, ServiceType, ServiceNode).

getService(_, S2, ServiceType, ServiceNode, InstanceBindings) :-
	\+((member(S1, InstanceBindings), service(S1, _, ServiceType, _))),
	service(S2, _, ServiceType, ServiceNode).

bindServicesBlob(_,[],_,_,[]).
bindServicesBlob(AppOp,[(F,Bindings)|FList], N, FType, [(F,FServicesBinding,N)|PlacedFunctions]):-
	functionReqs(F, _, _, FServicesReqs),
    bindServices(AppOp, N, Bindings, FType, FServicesReqs, FServicesBinding),
	bindServicesBlob(AppOp,FList, N, FType, PlacedFunctions).