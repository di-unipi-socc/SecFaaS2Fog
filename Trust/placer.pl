:- use_module(library(lists)).
:- consult('../examples/FORTE2021/infrastructure').
:- consult('../examples/FORTE2021/application').
:- consult('../examples/FORTE2021/trust').

faas2fog(ChainId, Placement):-
    functionChain(ChainId, AppOp, (GeneratorId,TriggerTypes), FunctionList, LatencyReqs),
	eventGenerator(GeneratorId, GeneratorNode), 
    typePropagation(TriggerTypes, FunctionList, TypedFunctionList),
    mapping(AppOp, TypedFunctionList, LatencyReqs, GeneratorNode, Placement).

typePropagation(_,[], []). 
typePropagation(InTypes, [(F,FServices)|FunctionList], [(F,FServices,FType)|TypedFunctionList]) :-
    functionBehaviour(F, InTypes, InteractionsTypes, OutTypes),
    append(InTypes, InteractionsTypes, TempTypes), append(TempTypes, OutTypes, AllTypes),
    sort(AllTypes, AllTypesSorted),
    highestType(AllTypesSorted,FType),								%HP: AllTypesSorted will never by empty
    typePropagation(OutTypes, FunctionList, TypedFunctionList).

mapping(AppOp, TypedFunctionList, LatencyReqs, GeneratorNode, Placement) :-
	mapping(AppOp, TypedFunctionList, LatencyReqs, GeneratorNode, [], _, Placement).

mapping(_, [], [], _, AllocHW, AllocHW, []).
mapping(AppOp, [(F,FServices,FType)|FunctionList], [RequiredLatency|LatencyReqs], PreviousNode, OldAllocHW, NewAllocHW, [on(F,N,FServicesBinding)|P]):-
	getNode(AppOp, N, SWCaps, HWCaps),
	link(PreviousNode, N, FeaturedLatency), FeaturedLatency =< RequiredLatency,
	functionReqs(F, SWReqs, HWReqs, FServicesReqs),
    swReqsOK(SWReqs, SWCaps),
    hwReqsOK(HWReqs, HWCaps, N, OldAllocHW, AllocHW),
    compatibleNodeType(FType,N),
    bindServices(AppOp, N, FServices, FType, FServicesReqs, FServicesBinding),
    mapping(AppOp, FunctionList, LatencyReqs, N, AllocHW, NewAllocHW, P).

getNode(AppOp, N, SWCaps, HWCaps) :-
	node(N, Nop, _, SWCaps, HWCaps),
	trusts(AppOp,Nop,3).

compatibleNodeType(Ftype,N) :- assignNodeLabel(N, Ntype), compatible(Ftype, Ntype).
compatibleServiceType(Ftype,S,Stype) :- assignServiceLabel(S,Stype,Slabel), compatible(Ftype, Slabel).
compatible(T,T).
compatible(T1,T2) :- dif(T1,T2), lattice_higherThan(T2, T1).

bindServices(_, _, [], _, [], []).
bindServices(AppOp, Node, [S|Ss], FType, [(SType,RequiredLatency)|ReqList], [(SType,S,ServiceNode)|Binding]):-
	getService(AppOp, S, SType, ServiceNode),
	compatibleServiceType(FType, S, SType),
	compatibleNodeType(FType, ServiceNode),
	link(Node, ServiceNode, FeaturedLatency), FeaturedLatency =< RequiredLatency,
	getNode(AppOp, ServiceNode, _,_),
	bindServices(AppOp, Node, Ss, FType, ReqList, Binding).

getService(AppOp, S, ServiceType, ServiceNode) :-
	service(S, ServiceProvider, ServiceType, ServiceNode),
	trusts(AppOp,ServiceProvider,3).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%find highest type in a list
highestType([T], T).
highestType([T1,T2|Ts], MaxT) :-
	highestType([T2|Ts], MaxTofRest),
	once(maxType(T1, MaxTofRest, MaxT)).
maxType(X, X, X).
maxType(X, Y, X) :- dif(X,Y), lattice_higherThan(X,Y).
maxType(X, Y, Y) :- dif(X,Y), lattice_higherThan(Y,X).
maxType(X, Y, Top) :-											%labels not reachable with path (on different branch) 
	dif(X,Y), \+ lattice_higherThan(X,Y), \+ lattice_higherThan(Y,X),
	lattice_higherThan(Top, X), lattice_higherThan(Top, Y),
	\+ (lattice_higherThan(Top, LowerTop), lattice_higherThan(LowerTop, X), lattice_higherThan(LowerTop, Y)).
lattice_higherThan(X, Y) :- g_lattice_higherThan(X,Y).
lattice_higherThan(X, Y) :- g_lattice_higherThan(X,W), lattice_higherThan(W,Y).

swReqsOK(SWReqs, SWCaps):- subset(SWReqs, SWCaps).

hwReqsOK((RAMReq,VCPUsReq,CPUReq), (RAMCap,VCPUsCap,CPUCap), N, [], [(N,(RAMReq,VCPUsReq,CPUCap))]) :-
	RAMCap >= RAMReq, CPUCap >= CPUReq, VCPUsCap >= VCPUsReq.
hwReqsOK((RAMReq,VCPUsReq,CPUReq), (RAMCap,VCPUsCap,CPUCap), N, [(N,(AllocRAM,AllocVCPUs,CPUCap))|L], [(N,(NewAllocRAM,NewAllocVCPUs,CPUCap))|L]) :-
	NewAllocRAM is AllocRAM + RAMReq, RAMCap >= NewAllocRAM,
	NewAllocVCPUs is AllocVCPUs + VCPUsReq, VCPUsCap >= NewAllocVCPUs,
	CPUCap >= CPUReq.
hwReqsOK(HWReqs, HWCaps, N, [(N1,AllocHW)|L], [(N1,AllocHW)|NewL]) :-
	N \== N1, 
	hwReqsOK(HWReqs, HWCaps, N, L, NewL).