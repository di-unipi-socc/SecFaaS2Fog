:- use_module(library(lists)).
%:- consult('/examples/SIoTEC2022/application').
%:- consult('/examples/SIoTEC2022/infrastructure').
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
secfaas2fog(GeneratorId,OrchId, Placement):-
	functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration),
    placement(PadOrchestration, GeneratorId, Placement).

%placement without padding
noPad(GeneratorId,OrchId, Placement):-
	functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    placement(TypedOrchestration, GeneratorId, Placement).

%execute a placement in optimised version with a time bound in seconds
secfaas2fogOpt(MaxExecTime,GeneratorId,OrchId, Placement):-
	get_time(StartTime),
	once(((functionOrch(OrchId, (_,TriggerTypes), Orchestration),
			wellFormed(Orchestration,WFOrchestration),
    		typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    		padding(TypedOrchestration, PadOrchestration),
    		placementOpt((StartTime, MaxExecTime),PadOrchestration, GeneratorId, Placement)
		);false)).
%optimised placement without padding
secfaas2fogOptNoPad(MaxExecTime,GeneratorId,OrchId, Placement):-
	get_time(StartTime),
	once(((functionOrch(OrchId, (_,TriggerTypes), Orchestration),
			wellFormed(Orchestration,WFOrchestration),
    		typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    		placementOpt((StartTime, MaxExecTime),TypedOrchestration, GeneratorId, Placement)
		);false)).


%%%%TEST predicates
notDuplicate(G,OrchId):-
	findall(P,secfaas2fog(G,OrchId, P), Ps),
	list_to_set(Ps,S),
	length(Ps, R1),
	length(S, R2),
	R1 =:= R2. 

testTyping(OrchId, TypedOrchestration):-
	functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
	typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration).

%testTypingBlob(OrchId, BlobedOrchestration):-
%	functionOrch(OrchId, (_,TriggerTypes), Orchestration),
%	wellFormed(Orchestration,WFOrchestration),
%	typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
%	blobify(TypedOrchestration, BlobedOrchestration).

testPadFormat(OrchId, PadOrchestration):-
    functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration).

testPad(OrchId, PadOrchestration):-
    functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration).

testPadMap(OrchId, Placement):-
    functionOrch(OrchId, (GeneratorId,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration),
    placement(PadOrchestration, GeneratorId, Placement).

notDuplicatePad(OrchId):-
	findall(P,testPadMap(OrchId, P), Ps),
	sort(Ps,S),
	length(Ps, R1),
	length(S, R2),
	R1 =:= R2,
    dif(R1,0). 

wellFormedTest(OrchId, NewOrchestration):- 
	functionOrch(OrchId, _, Orchestration),
	wellFormed(Orchestration, NewOrchestration).
