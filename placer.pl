:- use_module(library(lists)).
:- consult('infrastructure').
:- consult('application').
:- consult('wellformedness').
:- consult('typing').
:- consult('padding').
:- consult('mapping').
:- consult('optimised').
:- consult('replacement').
:- consult('utils').
:- consult('print').

%in: id generator, orchestratationid out: placement
%finds an eligible placement per query with padding
secfaas2fog(GeneratorId,OrchId, Placement):-
	functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration),
    placement(PadOrchestration, GeneratorId, Placement).

%finds an elibplacement without padding
noPad(GeneratorId,OrchId, Placement):-
	functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    placement(TypedOrchestration, GeneratorId, Placement).

%execute a placement in optimised version with a time bound in seconds
secfaas2fogOpt(MaxExecTime,GeneratorId,OrchId, Placement):-
	get_time(StartTime),
	functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration),
    placementOpt((StartTime, MaxExecTime),PadOrchestration, GeneratorId, Placement).
%optimised placement without padding
secfaas2fogOptNoPad(MaxExecTime,GeneratorId,OrchId, Placement):-
	get_time(StartTime),
	functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    placementOpt((StartTime, MaxExecTime),TypedOrchestration, GeneratorId, Placement).


%replacement starting from Fstart
replacement(MaxTime, Fstart,StartingNodes,OrchId, Placement):-
	functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration),
	replace(Fstart,PadOrchestration, CuttedOrchestration),
	get_time(Start),
	placementOpt((Start,MaxTime),CuttedOrchestration, StartingNodes, Placement).

%replacement starting from Fstart without padding
replacementNoPad(MaxTime, Fstart,StartingNodes,OrchId, Placement):-
	functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
	replace(Fstart,TypedOrchestration, CuttedOrchestration),
	get_time(Start),
	placementOpt((Start,MaxTime),CuttedOrchestration, StartingNodes, Placement).