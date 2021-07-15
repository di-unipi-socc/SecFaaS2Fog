%%AR GATHERING APPLICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% functionReqs(functionId, listOfSWReqs, HWReqs(memory, vCPU, Htz), timeout, listOfServiceReqs(serviceType, latency))
functionReqs(fLogin, [py3], (1024, 2, 500), [(userDB, 13)]).
functionReqs(fShop, [py3, numPy], (2048, 4, 1200), []).
functionReqs(fGeo, [js], (1024, 2, 400), [(maps, 300)]).
functionReqs(fGather, [js], (102, 2, 500), [(shops, 1020)]).
functionReqs(fNav, [js], (1024, 2, 500), [(maps, 300)]).
functionReqs(fNav2, [js], (1024, 2, 500), [(maps, 300)]).
functionReqs(fNav3, [js], (256, 2, 500), [(maps, 600)]).
functionReqs(fBook, [js], (1024, 2, 500), [(shops, 1200)]).
functionReqs(fBook2, [js], (528, 2, 500), [(shops, 650)]).
functionReqs(fAR, [py3, numPy], (2048, 4, 1200), []).
functionReqs(fAR2, [py3, numPy], (456, 4, 1200), []).
functionReqs(fAR3, [py3, numPy], (556, 4, 1200), []).
functionReqs(fSynch, [], (0, 0, 0), []).

%functionBehaviour(functionId, listOfInputs, listOfun(serviceReq, TypeParam), listOfOutputs)
functionBehaviour(fLogin, [U, Sc, G, Se],[U], [Sc, G, Se]).
functionBehaviour(fLogin, [U, Sc, G, _],[U], [Sc, G]).
functionBehaviour(fShop,[Sc, G, Se],[], [Sc, low, G, Se]).
functionBehaviour(fShop,[Sc, G, Se],[], [Sc, G, Se]).
functionBehaviour(fGeo, [Sc, Sb, G, Se], [Sb, G, Se, low], [Sc, Sb, low]).
functionBehaviour(fGather, [Sc, Sb, Sh], [Sh, medium], [Sc, Info]):- maxType(Sb, Sh,Info).
functionBehaviour(fNav, [Sc, Sh], [Nav], [Sc, Nav]):- maxType(Sc, Sh, Nav).
functionBehaviour(fNav2, [Sc, Sh], [Nav], [Sc, Nav]):- maxType(Sc, Sh, Nav).
functionBehaviour(fNav3, [Sc, Sh,_], [Nav], [Sc, Nav]):- maxType(Sc, Sh, Nav).
functionBehaviour(fBook, [Sc, Sh], [Book], [Sc, Book]):- maxType(Sc, Sh, Book).
functionBehaviour(fBook2, [Sc, Sh], [Book], [Sc, Book]):- maxType(low, Sh, Book).
functionBehaviour(fAR, [Sc,Draw], [], [ScAr]):- maxType(Sc, Draw, ScAr).
functionBehaviour(fAR2, [Sc,Draw,_], [], [ScAr,low,low]):- maxType(Sc, Draw, ScAr).
functionBehaviour(fAR3, [Sc,Draw], [], [ScAr,low]):- maxType(Sc, Draw, ScAr).
functionBehaviour(fSynch, [_,_,_,_,_,_,_], [], [low,low]).

%functionOrch(functionOrchId, operatorId, triggeringEvent(eventSource, eventType, inputParameters), (latency from source, dest)
%               listOfFunctions(functionId(listOfServiceInstances), latencyFromPrevious)
functionOrch(
  orchGath, appOp,(userDevice, [top,low,low, low]), %[userInfo, screen, geo, sensors], latency
  [(fLogin,[myUsers],12),(fShop,[],10),(fGeo,[_],13),(fGather, [myShop],10), 
  if([(fNav,[_],12)],[(fBook,[myShop],15)]),(fAR, [],15)]).

functionOrch(
  orchTest1, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  if(fun(fLogin,[],250),
        seq(fun(fNav,[],120),seq(fun(fBook2,[myShop],150),fun(fAR,[],100))),
        seq(fun(fBook,[myShop],150),seq(fun(fNav2,[],100),fun(fAR,[],180)))
  )
).

functionOrch(
  orchTest10, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  if(fun(fLogin,[],50),
        seq(fun(fNav,[],120),fun(fBook,[],100)),
        seq(fun(fNav2,[],120),fun(fBook2,[],100))
  )
).

functionOrch(
  orchTest2, appOp,(userDevice, [medium,low]), %[userInfo, screen, geo, sensors], latency
  seq(fun(fNav,[],120),seq(fun(fBook2,[myShop],150),fun(fAR,[],100)))
).

functionOrch(
  orchTest3, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  if(fun(fLogin,[myUsers],60),
          seq(fun(fShop,[],120),fun(fShop,[],100)),
          fun(fAR2,[],30)
      )
).

functionOrch(
  orchTest4, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  seq(fun(fLogin,[myUsers],60),
      seq(par([seq(fun(fShop,[],100),fun(fShop,[],120)), fun(fAR2,[],120)]), 
      fun(fSynch,[],150)))
).

functionOrch(
  orchTest5, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  if(fun(fLogin,[myUsers],60),
        seq(fun(fNav,[],120),seq(fun(fBook2,[myShop],150),fun(fAR,[],100))),
        seq(seq(fun(fNav,[],120),fun(fBook2,[myShop],150)),fun(fAR,[],100))
      )
).

functionOrch(
  orchTest6, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  if(fun(fLogin,[myUsers],60),
        if(fun(fNav3,[],120),fun(fBook2,[myShop],150),seq(fun(fAR3,[],100),fun(fAR3,[],100))),
        seq(seq(fun(fNav3,[],120),fun(fBook2,[myShop],150)),fun(fAR3,[],100))
      )
).

functionOrch(
  orchTest7, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  if(fun(fLogin,[myUsers],60),
        seq(if(fun(fNav3,[],120),fun(fBook2,[myShop],150),seq(fun(fAR3,[],100),fun(fAR3,[],100))),fun(fAR3,[],100)),
        seq(seq(fun(fNav3,[],120),fun(fBook2,[myShop],150)),fun(fAR3,[],100))
      )
).

functionOrch(
  orchTest8, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  if(fun(fLogin,[myUsers],600),
        seq(par([seq(fun(fNav3, [], 1000), fun(fAR3, [], 1000)), fun(fGather, [], 1000),fun(fAR2, [], 1000)]),fun(fSynch,[],1500)),
        seq(seq(fun(fNav3,[],1200),fun(fBook2,[myShop],1500)),fun(fAR3,[],1000))
      )
).

functionOrch(
  orchTest9, appOp,(userDevice, [top,medium,low, low]), %[userInfo, screen, geo, sensors], latency
  if(fun(fLogin,[myUsers],600),
        if(fun(fShop, [], 300),
            seq(par([seq(fun(fNav3, [], 1000), fun(fAR3, [], 1000)), fun(fGather, [], 1000),fun(fAR2, [], 1000)]),fun(fSynch,[],1500)),
            fun(fNav3,[],900)),
        seq(seq(fun(fNav3,[],1200),fun(fBook2,[myShop],1500)),fun(fAR3,[],1000))
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