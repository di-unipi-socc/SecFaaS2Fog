%security type assignment to function of an orchestration
typePropagation(TriggerTypes,WfOrchestration,TypedOrchestration):-
	lowestType(Lowest),
	typePropagation(Lowest,TriggerTypes,WfOrchestration,TypedOrchestration,_).

typePropagation(_,InputTypes,[], [],InputTypes).
%f case
typePropagation(GuardType,InTypes, fun(F,FServices,Latency), ft(F,FType,FServices,Latency), OutTypes) :-
    functionBehaviour(F, InTypes, InteractionsTypes, OutTypes),
	%union(InTypes,GuardType, InputTypes),
    union([GuardType|InTypes], InteractionsTypes, TempTypes), union(TempTypes, OutTypes, AllTypes),
    highestType(AllTypes,FType).
%sec case
typePropagation(GuardType,InTypes, seq(S1,S2), seq(NewS1,NewS2), OutTypes) :-
	typePropagation(GuardType,InTypes, S1, NewS1, OutS1),
	typePropagation(GuardType,OutS1, S2, NewS2, OutTypes).
%if case
typePropagation(GuardType,InTypes, if(FGuard,TrueBranch,FalseBranch), if(FGuardT,TrueBranchT,FalseBranchT),OutTypes) :-
	typePropagation(GuardType,InTypes, FGuard, FGuardT, [Guard|OutGuard]), %assuming first variable is the guard
	maxType(GuardType,Guard, NewGuardType),
	typePropagation(NewGuardType,[Guard|OutGuard], TrueBranch, TrueBranchT, OutTrueBranch),
	typePropagation(NewGuardType,[Guard|OutGuard], FalseBranch, FalseBranchT, OutFalseBranch),
	higherPerPosition(OutTrueBranch, OutFalseBranch, OutTypes). %conservative ouput of if
%par case
typePropagation(GuardType,InTypes, par(ParList), par(ParListT),OutTypes) :-
	typePropagationPar(GuardType,InTypes, ParList, ParListT,OutTypes).

typePropagationPar(_,_, [], [],[]).
typePropagationPar(GuardType,InTypes, [P|ParList], [PT|ParListT],OutTypes) :-
	typePropagation(GuardType,InTypes, P, PT,OutP),
	typePropagationPar(GuardType,InTypes, ParList, ParListT,OutPList),
	append(OutP, OutPList, OutTypes).

higherPerPosition([],[],[]).
higherPerPosition([Type1|List1], [Type2|List2], [TypeMax|ListResult]):-
	maxType(Type1,Type2, TypeMax),
	higherPerPosition(List1,List2,ListResult).

