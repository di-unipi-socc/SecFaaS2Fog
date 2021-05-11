:- use_module(library(lists)).
%:- consult('infrastructure').
%:- consult('application').
:- consult('typing').
:- consult('wellFormedness').
:- consult('mapping').
:- consult('utils').
:- consult('../examples/FORTE2021/infrastructure').
:- consult('../examples/FORTE2021/application').


faas2fogNew(OrchId, Placement):-
	functionOrch(OrchId, AppOp, (GeneratorId,TriggerTypes), Orchestration),
	eventGenerator(GeneratorId, GeneratorNode), 
	lowestType(Lowest),
	typePropagation(Lowest,TriggerTypes,Orchestration,TypedOrchestration,_),
	wellFormed(TypedOrchestration, BlobbedOrchestration),
	mapping(AppOp, BlobbedOrchestration, GeneratorNode, Placement).

notDuplicate(OrchId):-
	findall(P,faas2fogNew(OrchId, P), Ps),
	list_to_set(Ps,S),
	length(Ps, R1),
	length(S, R2),
	R1 =:= R2. 

testTyping(OrchId, TypedOrchestration):-
	functionOrch(OrchId, _, (_,TriggerTypes), Orchestration),
	lowestType(Lowest),
	typePropagation(Lowest,TriggerTypes,Orchestration,TypedOrchestration,_).

testTypingBlob(OrchId, BlobedOrchestration):-
	functionOrch(OrchId, _, (_,TriggerTypes), Orchestration),
	lowestType(Lowest),
	typePropagation(Lowest,TriggerTypes,Orchestration,TypedOrchestration,_),
	wellFormed(TypedOrchestration, BlobedOrchestration).