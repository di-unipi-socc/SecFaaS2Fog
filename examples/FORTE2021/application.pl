%%AR GATHERING APPLICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% functionReqs(functionId, listOfSWReqs, HWReqs(memory, vCPU, Htz), timeout, listOfServiceReqs(serviceType, latency))
functionReqs(fLogin, [py3], (1024, 2, 500), [(userDB, 13)]).
functionReqs(fShop, [py3, numPy], (2048, 4, 1500), []).
functionReqs(fGeo, [js], (1024, 2, 400), [(maps, 30)]).
functionReqs(fGather, [js], (1024, 2, 500), [(shops, 12)]).
functionReqs(fNav, [js], (1024, 2, 500), [(maps, 30)]).
functionReqs(fBook, [js], (1024, 2, 500), [(shops, 12)]).
functionReqs(fAR, [py3, numPy], (2048, 4, 1200), []).

%functionBehaviour(functionId, listOfInputs, listOf(serviceReq, TypeParam), listOfOutputs)
functionBehaviour(fLogin, [U, Sc, G, Se],[U], [Sc, G, Se]).
functionBehaviour(fShop,[Sc, G, Se],[], [Sc, low, G, Se]).
functionBehaviour(fGeo, [Sc, Sb, G, Se], [Sb, G, Se, low], [Sc, Sb, low]).
functionBehaviour(fGather, [Sc, Sb, Sh], [Sh, medium], [Sc, Info]):- maxType(Sb, Sh,Info).
functionBehaviour(fNav, [Sc, Sh], [Nav], [Sc, Nav]):- maxType(Sc, Sh, Nav).
functionBehaviour(fBook, [Sc, Sh], [Book], [Sc, Book]):- maxType(Sc, Sh, Book).
functionBehaviour(fAR, [Sc,Draw], [], [ScAr]):- maxType(Sc, Draw, ScAr).

%functionOrch(functionOrchId, operatorId, triggeringEvent(eventSource, eventType, inputParameters), (latency from source, dest)
%               listOfFunctions(functionId(listOfServiceInstances), latencyFromPrevious)
functionOrch(
  orchGath, appOp,(userDevice, [top,low,low, low]), %[userInfo, screen, geo, sensors], latency
  [(fLogin,[myUsers],12),(fShop,[],10),(fGeo,[_],13),(fGather, [myShop],10), 
  if([(fNav,[_],12)],[(fBook,[myShop],15)]),(fAR, [],15)]).

functionOrch(
  orchTest1, appOp,(userDevice, [top,low,low, low]), %[userInfo, screen, geo, sensors], latency
  seq(if([0],
          seq(f(fNav,[_],12),(
          seq(f(fBook,[myShop],15),
          nil))),
          seq(f(fBook,[myShop],15),(
          seq(f(fNav,[_],12),
          nil)))
      ),
  nil)
).

functionOrch(
  orchTest2, appOp,(userDevice, [top,low,low, low]), %[userInfo, screen, geo, sensors], latency
  seq(f(fNav,[_],12),(
  seq(f(fBook,[myShop],15),
  nil)))
).

functionOrch(
  orchTest3, appOp,(userDevice, [top,low,low, low]), %[userInfo, screen, geo, sensors], latency
  seq(if([0],
          f(fNav,[_],12),
          f(fNav,[_],12)
      ),
  nil)
).

% lattice of security types
g_lattice_higherThan(top, medium).
g_lattice_higherThan(medium, low).


% node labeling
assignNodeLabel(NodeId, top)    :- node(NodeId,_,SecCaps,_,_), member(antiTamp, SecCaps), member(pubKeyE, SecCaps).
assignNodeLabel(NodeId, medium) :- node(NodeId,_,SecCaps,_,_), \+(member(antiTamp, SecCaps)), member(pubKeyE, SecCaps).
assignNodeLabel(NodeId, low)    :- node(NodeId,_,SecCaps,_,_), \+(member(pubKeyE, SecCaps)).

%service labeling
assignServiceLabel(SId, _, top) :- service(SId, appOp, _, _).
%assignServiceLabel(SId, T, medium) :- service(SId, appOp, T, _), \+(T == userDB).
assignServiceLabel(SId, maps, medium) :- service(SId, cloudProvider, maps, _).
assignServiceLabel(SId, Type, low) :- 
    service(SId, Provider, Type, _),
    \+(Provider == appOp),
    \+((Provider == cloudProvider, Type == maps)).