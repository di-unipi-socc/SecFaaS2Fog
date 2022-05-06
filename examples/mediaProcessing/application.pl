%%AR GATHERING APPLICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% functionReqs(functionId, listOfSWReqs, HWReqs(memory, vCPU, Htz), timeout, listOfServiceReqs(serviceType, latency))
functionReqs(fDocAnalysis, [py3], (1024, 2, 500), [(bucket, 160)]).
functionReqs(fProcDoc, [js], (1024, 1, 800), [userDB, 210]).
functionReqs(fPayAppr, [py3], (256, 2, 400), []).
functionReqs(fNotify, [py3], (128, 2, 500), []).
functionReqs(fArchive, [py3], (256, 2, 500), []).

%functionBehaviour(functionId, listOfInputs, listOfun(serviceReq, TypeParam), listOfOutputs)
functionBehaviour(fDocAnalysis, [Head,User,Value],[Head],[Head,User,Value]).
functionBehaviour(fProcDoc, [Head,User,Value],[User],[Head,Value]).
functionBehaviour(fPayAppr, [Head,Value],[],[Head,Value]).
functionBehaviour(fNotify, [Head,Value],[],[Res,low]):- maxType(Head, Value, Res). 
functionBehaviour(fArchive, [Head,Value],[],[Head,Value]).                              

%functionOrch(functionOrchId, operatorId, triggeringEvent(eventSource, eventType, inputParameters), (latency from source, dest)
%               listOfFunctions(functionId(listOfServiceInstances), latencyFromPrevious)

functionOrch(
  mediaOrch,(event02, [low,top,medium]), %trigger
  seq(fun(fDocAnalysis,[],220),
    seq(fun(fProcDoc,[],200),
        if(fun(fPayAppr,[],180), fun(fNotify,[],220),fun(fArchive,[],200))
    )
  )
).


% lattice of security types
g_lattice_higherThan(top, medium).
g_lattice_higherThan(medium, low).

% lattice security types color for print, if do not needed use 'latticeColor(_,default).'
latticeColor(low,red).
latticeColor(medium,orange).
latticeColor(top,green).

% node labeling
nodeLabel(NodeId, top)    :- node(NodeId,_,_,SecCaps,_,_), member(antiTamp, SecCaps), member(pubKeyE, SecCaps).
nodeLabel(NodeId, medium) :- node(NodeId,_,_,SecCaps,_,_), \+(member(antiTamp, SecCaps)), member(pubKeyE, SecCaps).
nodeLabel(NodeId, low)    :- node(NodeId,_,_,SecCaps,_,_), \+(member(pubKeyE, SecCaps)).

%service labeling
serviceLabel(SId, _, top) :- service(SId, appOp, _, _).
serviceLabel(SId, _, top) :- service(SId, pa, _, _).
serviceLabel(SId, _, top) :- service(SId, cloudProvider, Stype, _), \+(Stype == maps).
serviceLabel(SId, maps, medium) :- service(SId, cloudProvider, maps, _).
serviceLabel(SId, Type, low) :- 
    service(SId, Provider, Type, _),
    \+(Provider == appOp),
    \+(Provider == pa),
    \+(Provider == cloudProvider).