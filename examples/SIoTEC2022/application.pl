%%AR GATHERING APPLICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% functionReqs(functionId, listOfSWReqs, HWReqs(memory, vCPU, Htz), timeout, listOfServiceReqs(serviceType, latency))
functionReqs(fLogin, [js], (1024, 2, 500), [(userDB, 13)]).
functionReqs(fCrop, [py3, numPy], (2048, 4, 1200), []).
functionReqs(fGeo, [js], (256, 2, 400), [(maps, 30)]).
functionReqs(fDCC, [js], (128, 2, 500), []).
functionReqs(fCheckDCC, [js], (1600, 2, 500), [(checkGp, 50)]).
functionReqs(fRules, [py3], (1800, 1, 400), [(checkRules, 20)]).
functionReqs(fAR, [py3, numPy], (2048, 4, 1200), []).

%functionBehaviour(functionId, listOfInputs, listOfun(serviceReq, TypeParam), listOfOutputs)
functionBehaviour(fLogin, [U, Sc, G],[U], [U,Sc, G]).
functionBehaviour(fCrop,[_,Sc, G],[], [Sc,G]).
functionBehaviour(fGeo, [Sc,G], [G], [Sc]).
functionBehaviour(fDCC, [U,_,_], [], [U,U]).
functionBehaviour(fCheckDCC, [_,U], [U], [low]).
functionBehaviour(fRules, [_, U], [U], [low]).
functionBehaviour(fAR, [U,Draw], [], [ScAr]):- maxType(U, Draw, ScAr).

%functionOrch(functionOrchId, operatorId, triggeringEvent(eventSource, eventType, inputParameters), (latency from source, dest)
%               listOfFunctions(functionId(listOfServiceInstances), latencyFromPrevious)

functionOrch(
  arOrch,(userDevice, [top,low,medium]), %trigger
  seq(fun(fLogin,[myUserDb],25),
    seq(par([
          if(fun(fDCC,[],15),
              fun(fCheckDCC,[],15),
              fun(fRules,[],18)),
          seq(fun(fCrop,[],18),fun(fGeo,[],12))]),
    fun(fAR,[],18)))
).


% lattice of security types
g_lattice_higherThan(top, medium).
g_lattice_higherThan(medium, low).

% lattice security types color for print, if do not needed use 'latticeColor(_,default).'
latticeColor(low,red).
latticeColor(medium,orange).
latticeColor(top,green).

% node labeling
nodeLabel(NodeId, top)    :- node(NodeId,_,SecCaps,_,_), member(antiTamp, SecCaps), member(pubKeyE, SecCaps).
nodeLabel(NodeId, medium) :- node(NodeId,_,SecCaps,_,_), \+(member(antiTamp, SecCaps)), member(pubKeyE, SecCaps).
nodeLabel(NodeId, low)    :- node(NodeId,_,SecCaps,_,_), \+(member(pubKeyE, SecCaps)).

%service labeling
serviceLabel(SId, _, top) :- service(SId, appOp, _, _).
serviceLabel(SId, _, top) :- service(SId, pa, _, _).
serviceLabel(SId, maps, medium) :- service(SId, cloudProvider, maps, _).
serviceLabel(SId, Type, low) :- 
    service(SId, Provider, Type, _),
    \+(Provider == appOp),
    \+((Provider == cloudProvider, Type == maps)).