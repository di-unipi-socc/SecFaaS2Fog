:- use_module(library(lists)).
%:- consult('infrastructure').
%:- consult('application').
:- consult('../examples/FORTE2021/infrastructure').
:- consult('../examples/FORTE2021/application').

%orchestration costructs: nil, seq, if
wellFormedOrchestration(OrchId):-
	functionOrch(OrchId, _, (_,_), Orchestration),
	wellFormed(Orchestration).

wellFormed(nil).
wellFormed(f(_,_,_)).
wellFormed(seq(F1,F2)) :-
	wellFormed(F1), wellFormed(F2).
wellFormed(if(_, F1, F2)) :-
	wellFormedIf(F1, F2).

%aggragate is vertical, blob is horizontal
wellFormedIf(F1,F2) :-
	wellFormed(F1), wellFormed(F2),
	checkAggregation((noblob,empty,empty), F1, F2,(noblob,_,_)).

%(Mode, TrueReqs, FalseReqs)
%Mode == noblob, Reqs = empty
%Mode == blob, Reqs to be aggregated

%checkAggregation(InputMode[blob\noblob], TrueBranch, FalseBranch, ResultMode[blob\noblob])
%checkAggregation -> 
checkAggregation((noblob,_,_), nil, nil, (noblob,_,_)).

checkAggregation((noblob,_,_), f(F1,_,_), f(F2,_,_),(noblob,_,_)) :-
	aggregable(F1, F2, _, _, yes).
checkAggregation((noblob,_,_), f(F1,_,_), f(F2,_,_),(blob,Agg1,Agg2)) :-
	aggregable(F1, F2, Agg1, Agg2, no).
checkAggregation((blob,OldAgg1,OldAgg2), f(F1,_,_), f(F2,_,_),(Mode, SumAgg1, SumAgg2)) :-
	aggregable(F1, F2, Agg1, Agg2, _),
	checkBlob(OldAgg1, OldAgg2, Agg1, Agg2, SumAgg1, SumAgg2, Mode).

checkAggregation(OldMode, seq(F1,L1), seq(F2,L2), ResMode) :-
	checkAggregation(OldMode, F1, F2, Mode),
	checkAggregation(Mode, L1, L2, ResMode).

%checkBlob -> sum reqs and check if the blob is ended
checkBlob(OldAgg1, OldAgg2, Agg1, Agg2, SumAgg1, SumAgg1, noblob):-
	sumReqs(OldAgg1, Agg1, SumAgg1),
	sumReqs(OldAgg2, Agg2, SumAgg1).

checkBlob(OldAgg1, OldAgg2, Agg1, Agg2, SumAgg1, SumAgg2, blob):-
	sumReqs(OldAgg1, Agg1, SumAgg1),
	sumReqs(OldAgg2, Agg2, SumAgg2),
	SumAgg1\==SumAgg2.

%sumReqs -> union of SW and Ser reqs, max of HW reqs
sumReqs((OldSW, (OldMem, OldCore,OldCPU), OldSER), (SW,(Mem,Core,CPU),SER), (NewSW,(NewMem, NewCore,NewCPU),NewSER)):-
	ord_union(OldSW, SW, NewSW),
	NewMem is max(OldMem, Mem),
	NewCore is max(OldCore, Core),
	NewCPU is max(OldCPU, CPU),
	ord_union(OldSER, SER, NewSER).
	
aggregable(F1, F2, (Sw, Hw, Ser),(Sw, Hw, Ser),yes):-
	functionReqs(F1, Sw, Hw, Ser),
	functionReqs(F2, Sw, Hw, Ser).

aggregable(F1, F2, (Sw1, Hw1, Ser1),(Sw2, Hw2, Ser2),no):-
	functionReqs(F1, Sw1, Hw1, Ser1),
	functionReqs(F2, Sw2, Hw2, Ser2),
	(dif(F1,F2); set_dif(Sw1,Sw2); dif(Hw1, Hw2); set_dif(Ser1,Ser2)).
	%TODO max
	%Hw is max(Hw1, Hw2), union(Sw1, Sw2, Sw), union(Ser1, Ser2, Ser).

set_dif(A,B) :- member(X,A), \+ member(X,B).
set_dif(A,B) :- member(X,B), \+ member(X,A).
%TODO sequenze lunghe diverse
	
faas2fog(ChainId, Placement):-
    functionOrch(ChainId, AppOp, (GeneratorId,TriggerTypes), Orchestration),
	eventGenerator(GeneratorId, GeneratorNode), 
    typePropagation([],TriggerTypes, Orchestration, TypedFunctionList,_), %[] is first guard type
    mapping(AppOp, TypedFunctionList, GeneratorNode, Placement).

typePropagation(_,OutputTypes,[], [],OutputTypes). 
%function case
typePropagation(GuardTypes,InTypes, [(F,FServices,Latency)|FunctionList], [(F,FServices,FType,Latency)|TypedFunctionList], Output) :-
    functionBehaviour(F, InTypes, InteractionsTypes, OutTypes),
	append(InTypes,GuardTypes, InputTypes),
    append(InputTypes, InteractionsTypes, TempTypes), append(TempTypes, OutTypes, AllTypes),
    sort(AllTypes, AllTypesSorted),
    highestType(AllTypesSorted,FType),								%HP: AllTypesSorted will never by empty
    typePropagation(GuardTypes,OutTypes, FunctionList, TypedFunctionList,Output).
%if case
typePropagation(GuardTypes,InTypes, [if(Cond,TrueBranch,FalseBranch)|FunctionList], AllFunctionList,FinalOutput) :-
	condTypes(Cond, InTypes, CondTypes),
	append(GuardTypes, CondTypes, NewGuardTypes),
	typePropagation(NewGuardTypes, InTypes, TrueBranch, TrueFunctionList,OutputTrue), %true branch
	typePropagation(NewGuardTypes, InTypes, FalseBranch, FalseFunctionList,OutputFalse), %false branch
	append(TrueFunctionList, FalseFunctionList, BranchFunctionList),
	highestListType(OutputTrue, OutputFalse, OutputBranch),
    typePropagation(GuardTypes, OutputBranch, FunctionList, ContinuationFunctionList,FinalOutput), %continuation
	append(BranchFunctionList, ContinuationFunctionList, AllFunctionList).


mapping(AppOp, TypedFunctionList, GeneratorNode, Placement) :-
	mapping(AppOp, TypedFunctionList, GeneratorNode, [], _, Placement).

mapping(_, [], [], _, AllocHW, AllocHW, []).
mapping(AppOp, [(F,FServices,FType,RequiredLatency)|FunctionList], PreviousNode, OldAllocHW, NewAllocHW, [on(F,N,FServicesBinding)|P]):-
	getNode(AppOp, N, SWCaps, HWCaps),
	link(PreviousNode, N, FeaturedLatency), FeaturedLatency =< RequiredLatency,
	functionReqs(F, SWReqs, HWReqs, FServicesReqs),
    swReqsOK(SWReqs, SWCaps),
    hwReqsOK(HWReqs, HWCaps, N, OldAllocHW, AllocHW),
    compatibleNodeType(FType,N),
    bindServices(AppOp, N, FServices, FType, FServicesReqs, FServicesBinding),
    mapping(AppOp, FunctionList, N, AllocHW, NewAllocHW, P).

getNode(_, N, SWCaps, HWCaps) :-
	node(N, _, _, SWCaps, HWCaps).

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

getService(_, S, ServiceType, ServiceNode) :-
	service(S, _, ServiceType, ServiceNode).
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

%highestListType(List1, List2, ListHigher) %given two lists pick the highest type in the same position
highestListType([],[],[]).
highestListType(L,[],L).
highestListType([],L,L).
highestListType(L,L,L).
highestListType([T1|L1],[T2|L2],[Tmax|LRes]):-
	maxType(T1,T2,Tmax),
	highestListType(L1,L2,LRes).
	
%condTypes(Positions, TypeList, PosList) extract types of Position from TypeList  into PosList
condTypes([],_,[]).
condTypes([Pos|CondList],InTypes, [TypePos|CondTypes]):-
	nth0(Pos,InTypes,TypePos),
	condTypes(CondList, InTypes, CondTypes).

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