%%AR GATHERING APPLICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% functionReqs(functionId, listOfSWReqs, HWReqs(memory, vCPU, Htz), timeout, listOfServiceReqs(serviceType, latency))
functionReqs(fLogin, [py3], (1024, 2, 500), [(userDB, 13)]).
functionReqs(fShop, [py3, numPy], (2048, 4, 1200), []).
functionReqs(fGeo, [js], (1024, 2, 400), [(maps, 30)]).
functionReqs(fGather, [js], (1024, 2, 500), [(shops, 12)]).
functionReqs(fNav, [js], (1024, 2, 500), [(maps, 300)]).
functionReqs(fNav2, [js], (1024, 2, 500), [(maps, 300)]).
functionReqs(fBook, [js], (1024, 2, 500), [(shops, 120)]).
functionReqs(fBook2, [js], (1024, 2, 500), [(shops, 120)]).
functionReqs(fAR, [py3, numPy], (2048, 4, 1200), []).
functionReqs(fAR2, [py3, numPy], (2048, 4, 1200), []).
functionReqs(fSynch, [], (0, 0, 0), []).

%functionBehaviour(functionId, listOfInputs, listOf(serviceReq, TypeParam), listOfOutputs)
functionBehaviour(fLogin, [U, Sc, G, Se],[U], [Sc, G, Se]).
functionBehaviour(fLogin, [U, Sc, G, _],[U], [Sc, G]).
functionBehaviour(fShop,[Sc, G, Se],[], [Sc, low, G, Se]).
functionBehaviour(fShop,[Sc, G, Se],[], [Sc, G, Se]).
functionBehaviour(fGeo, [Sc, Sb, G, Se], [Sb, G, Se, low], [Sc, Sb, low]).
functionBehaviour(fGather, [Sc, Sb, Sh], [Sh, medium], [Sc, Info]):- maxType(Sb, Sh,Info).
functionBehaviour(fNav, [Sc, Sh], [Nav], [Sc, Nav]):- maxType(Sc, Sh, Nav).
functionBehaviour(fNav2, [Sc, Sh], [Nav], [Sc, Nav]):- maxType(Sc, Sh, Nav).
functionBehaviour(fBook, [Sc, Sh], [Book], [Sc, Book]):- maxType(Sc, Sh, Book).
functionBehaviour(fBook2, [Sc, Sh], [Book], [Sc, Book]):- maxType(low, Sh, Book).
functionBehaviour(fAR, [Sc,Draw], [], [ScAr]):- maxType(Sc, Draw, ScAr).
functionBehaviour(fAR2, [Sc,Draw,_], [], [ScAr,low,low]):- maxType(Sc, Draw, ScAr).
functionBehaviour(fSynch, [_,_,_,_,_,_,_], [], [low]).

%functionOrch(functionOrchId, operatorId, triggeringEvent(eventSource, eventType, inputParameters), (latency from source, dest)
%               listOfFunctions(functionId(listOfServiceInstances), latencyFromPrevious)
functionOrch(
  orchGath, appOp,(userDevice, [top,low,low, low]), %[userInfo, screen, geo, sensors], latency
  [(fLogin,[myUsers],12),(fShop,[],10),(fGeo,[_],13),(fGather, [myShop],10), 
  if([(fNav,[_],12)],[(fBook,[myShop],15)]),(fAR, [],15)]).

functionOrch(
  orchTest1, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  if(f(fLogin,[],250),
        seq(f(fNav,[],120),seq(f(fBook2,[myShop],150),f(fAR,[],100))),
        seq(f(fBook,[myShop],150),seq(f(fNav2,[],100),f(fAR,[],180)))
  )
).

functionOrch(
  orchTest10, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  if(f(fLogin,[],50),
        seq(f(fNav,[],120),f(fBook,[],100)),
        seq(f(fNav2,[],120),f(fBook2,[],100))
  )
).

functionOrch(
  orchTest2, appOp,(userDevice, [medium,low]), %[userInfo, screen, geo, sensors], latency
  seq(f(fNav,[],120),seq(f(fBook2,[myShop],150),f(fAR,[],100)))
).

functionOrch(
  orchTest3, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  if(f(fLogin,[myUsers],60),
          seq(f(fShop,[],120),f(fShop,[],100)),
          f(fAR2,[],30)
      )
).

functionOrch(
  orchTest4, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  seq(f(fLogin,[myUsers],60),
      seq(par([seq(f(fShop,[],100),f(fShop,[],120)), f(fAR2,[],120)]), 
      f(fSynch,[],150)))
).

functionOrch(
  orchTest5, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  if(f(fLogin,[myUsers],60),
        seq(f(fNav,[],120),seq(f(fBook2,[myShop],150),f(fAR,[],100))),
        seq(seq(f(fNav,[],120),f(fBook2,[myShop],150)),f(fAR,[],100))
      )
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