:- use_module(library(lists)).
%:- consult('infrastructure').
%:- consult('application').
:- consult('wellformedness').
:- consult('typing').
:- consult('blobify').
:- consult('padding').
:- consult('mapping').
:- consult('utils').
:- consult('../examples/FORTE2021/infrastructure').
:- consult('../examples/FORTE2021/application').

faas2fogNew(OrchId, Placement):-
	functionOrch(OrchId, AppOp, (GeneratorId,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
	typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),	%f  -> ft
	blobify(TypedOrchestration, BlobbedOrchestration),					%if -> seq, add blobs
	mapping(AppOp, BlobbedOrchestration, GeneratorId, Placement).		%ft -> fp

notDuplicate(OrchId):-
	findall(P,faas2fogNew(OrchId, P), Ps),
	list_to_set(Ps,S),
	length(Ps, R1),
	length(S, R2),
	R1 =:= R2. 

testTyping(OrchId, TypedOrchestration):-
	functionOrch(OrchId, _, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
	typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration).

testTypingBlob(OrchId, BlobedOrchestration):-
	functionOrch(OrchId, _, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
	typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
	blobify(TypedOrchestration, BlobedOrchestration).
