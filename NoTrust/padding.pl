%---------------- test predicates
testPadFormat(OrchId, FormattedPadOrchestration):-
    functionOrch(OrchId, _, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration),
    formatPaddedOrch(PadOrchestration, FormattedPadOrchestration).

testPad(OrchId, PadOrchestration):-
    functionOrch(OrchId, _, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration).

testPadMap(OrchId, Placement):-
   functionOrch(OrchId, AppOp, (GeneratorId,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration),
    formatPaddedOrch(PadOrchestration, FormattedPadOrchestration),
    mapping(AppOp, FormattedPadOrchestration, GeneratorId, Placement).

notDuplicatePad(OrchId):-
	findall(P,testPadMap(OrchId, P), Ps),
	list_to_set(Ps,S),
	length(Ps, R1),
	length(S, R2),
	R1 =:= R2,
    dif(R1,0). 
%----------------------------------
%main padding predicate, only if case is relevant for padding
padding(ft(Fid,Label,Binding,Latency),ft(Fid,Label,Binding,Latency)).
padding(seq(F1,F2),seq(Wf1,Wf2)) :-
	padding(F1,Wf1), padding(F2,Wf2).
padding(if(ft(Fid,Type,Binding,Latency), LeftB, RightB), if(ft(Fid,Type,Binding,Latency), LeftPadded,RightPadded)) :-
	paddingIf(LeftB, RightB, LeftPadded,RightPadded).
padding(par([]),[]).
padding(par(List), par(PaddedList)) :-
	paddingPar(List, PaddedList).

%paddingIf(LeftBranch, RightBranch, LeftBranchAfterPadding, RightBranchAfterPadding)
%given the left and right branch it pad them and give left branch padded and right branch padded
%end marks the end of a branch, seq became list, inner if(F,L,R) became [F,innerIf(FirstL,FirstR)...innerif(endL,endR)]
paddingIf(end,end,[],[]).
paddingIf(end, RightBranch, [Fdummy|LeftResult],RightResult):-
    dif(RightBranch,end),
    extractToPad(RightBranch,FR, RightContinuation), 
    dummyPad(FR, Fdummy), %insert dummy function to balance branches
    paddingIf(end, RightContinuation, LeftResult, RightTemp),
    append(FR, RightTemp, RightResult).

paddingIf(LeftBranch, end, LeftResult,[Fdummy|RightResult]):-
    dif(LeftBranch,end), 
    extractToPad(LeftBranch,FL, LeftContinuation),
    dummyPad(FL, Fdummy), %insert dummy function to balance branches
    paddingIf(LeftContinuation, end, LeftTemp, RightResult),
    append(FL,LeftTemp, LeftResult).

paddingIf(LeftBranch, RightBranch, LeftResult,RightResult):-
    dif(LeftBranch, end), 
    dif(RightBranch, end), 
    extractToPad(LeftBranch, FL, LeftContinuation),
    extractToPad(RightBranch,FR, RightContinuation),
    pad(FL, FR, FLp, FRp),
    paddingIf(LeftContinuation, RightContinuation, LeftTemp, RightTemp),
    append(FLp,LeftTemp, LeftResult),
    append(FRp, RightTemp, RightResult).

paddingPar([],[]).
paddingPar([P|Plist], [Pres|PlistRes]):-
    padding(P,Pres),
    paddingPar(Plist, PlistRes).

%dummyPad(ListOfFunctionsOnOtherBranch, DummyFunction) insert a dummy function to balance branches
%input is a list because inner if create sub branches to be pad
%single ft to clone
dummyPad([ft(FId,Label,Bindings,Lat)],fpad(fDummyPatch,Label,Bindings,Lat,Sw,Hw,Ser)):- %dummy clone per tappare buco su ramo
    functionReqs(FId, Sw,Hw,Ser).
%multiple ft to clone
dummyPad([F|FList],fpad(fDummyPatch,Label,Bindings,Lat,Sw,Hw,Ser)):- 
    dif(FList,[]),
    lowestType(Lowest),
    findCommonReqs([F|FList], ([], (0,0,0), [],[],Lowest,inf), (Sw,Hw,Ser,Bindings,Label,Lat)).

%extractToPad(Branch, F to pad, Continuation)
%given a branch, extract the function(s) to be padded and the continuation of the branch
extractToPad(end,end,end).
%extractToPad(endPar,endPar,endPar).
extractToPad(ft(F,T,B,L), [ft(F,T,B,L)], end).
extractToPad(seq(S1,S2), F, S2):-
    extractToPad(S1,F,end).
extractToPad(seq(S1,S2), F, seq(Cont,S2)):-
    extractToPad(S1,F,Cont),
    dif(Cont, end).
extractToPad(if(ft(Fid,Type,Binding,Latency), L, R), [ft(Fid,Type,Binding,Latency)], innerIf(L,R)).
extractToPad(innerIf(L,R), [innerIf(FL,FR)], Result):-
    extractToPad(L,FL,CL),
    extractToPad(R,FR,CR),
    innerIfcont(CL,CR, Result).
extractToPad(par(List), [par(NewFs)], par(Conts)):-
    \+ (isEndList(List)),
    extractToPad(innerPar(List), Fs, Conts),
    \+ (isEndList(Conts)),
    substituteEnds(Fs, NewFs).
extractToPad(par(List), [par(NewFs)], end):-
    \+ (isEndList(List)),
    extractToPad(innerPar(List), Fs, Conts),
    isEndList(Conts),
    substituteEnds(Fs, NewFs).
extractToPad(innerPar([]), [], []).
extractToPad(innerPar([end|Plist]),[end|FPlist], [end|ContPlist]):-
    extractToPad(innerPar(Plist), FPlist, ContPlist).
extractToPad(innerPar([P|Plist]),Fpar, ContPar):-
    dif(P, end),
    extractToPad(P, Fp, ContP),
    extractToPad(innerPar(Plist), FPlist, ContPlist),
    append(Fp, FPlist, Fpar),
    append([ContP], ContPlist, ContPar).

%Attenzione a interpretare gli innerIf -> [f, innerIf(A,B), innerIf(C,D)] sarebbe if(f,seq(A,C),seq(B,D))
%innerIfcont: calculate the continuation of an inner if
innerIfcont(end, end, end).
innerIfcont(end, CR, innerIf(end, CR)):- dif(CR, end).
innerIfcont(CL, end, innerIf(CL, end)):- dif(CL, end).
innerIfcont(CL, CR, innerIf(CL,CR)):- dif(CL, end), dif(CR, end).

%isEndList: used to understand if a parallel list is all ended
isEndList([end]).
isEndList([end|P]):- isEndList(P).

%substituteEnds: it substitute end with endPar to mark the end of a parallel execution (to avoid confusion with end of an if branch)
substituteEnds([],[]).
substituteEnds([F|Fs],[F|Res]):-
    dif(F,end),
    substituteEnds(Fs,Res).
substituteEnds([end|Fs],[endPar|Res]):-
    substituteEnds(Fs,Res).

%pad(LeftBranchFunctions, RightBranchFunctions, LBpadded, RBpadded)
%produce padding of extracted functions from left and right branches with same reqs (introduce dummies for service call)
pad(F,F,F,F).
pad(FL, FR, FLpad, FRpad):-
    dif(FL, FR),
    checkPar(FL, FR, FLpar, FRpar), %when par is involved we have to build an equal par structure on both branches
    append(FLpar,FRpar, Fs),%to calculate the common reqs we use all the functions
    lowestType(Lowest),
    findCommonReqs(Fs, ([], (0,0,0), [],[],Lowest,inf), PaddedReqs), %(SW, (Mem,Core,CPU), Ser, Binds,Type,Latency)
    padToReqs(FLpar, PaddedReqs, FLpad), %questo forse si può fare in findCommonReqs (togliendo l'append)
    padToReqs(FRpar, PaddedReqs, FRpad).

%TODO par dentro innerIf
%checks if a branch has a par and build the par on the other branch
checkPar([],[],[],[]).
checkPar(Left, Right, Left, Rres):-
    member(X,Left), isPar(X),
    \+ (member(Y,Right), isPar(Y)),
    buildPar(X, Right, Rres).
checkPar(Left, Right, Lres, Right):-
    member(Y,Right), isPar(Y),
    \+ (member(X,Left), isPar(X)),
    buildPar(Y, Left, Lres).
checkPar(Left, Right, Left, Right):-
    \+ (member(X,Left), isPar(X)),
    \+ (member(Y,Right), isPar(Y)).

buildPar(par(ParList), Branch, [par(BranchRes)]):-
    createOtherBranch(ParList, Branch, BranchRes).

createOtherBranch([],B,B).
createOtherBranch([endPar|Ps],B, [endPar|Bres]) :- createOtherBranch(Ps,B,Bres).
createOtherBranch([F1|Ps],[F2|Bs], [F2|Bres]) :- 
    dif(F1,endPar),
    createOtherBranch(Ps,Bs,Bres).
createOtherBranch([F1|Ps],[], [dummyPar|Bres]) :- 
    dif(F1,endPar),
    createOtherBranch(Ps,[],Bres).

countNotEndP([],0).
countNotEndP([endPar|L],R):-
    countNotEndP(L,R).
countNotEndP([F|L],R):-
    dif(F,endPar),
    countNotEndP(L,N),
    R is N + 1.

dummyList(0,[]).
dummyList(N,[dummyPar|List]):-
    N > 0,
    NewN is N - 1,
    dummyList(NewN, List).

%scan the function list to find the common reqs to be padded
findCommonReqs([],R,R).
findCommonReqs(end,R,R).
findCommonReqs([end|FR],Reqs,PaddedReqs):- findCommonReqs(FR, Reqs, PaddedReqs).
findCommonReqs([endPar|FR],Reqs,PaddedReqs):- findCommonReqs(FR, Reqs, PaddedReqs).
findCommonReqs([dummyPar|FR],Reqs,PaddedReqs):- findCommonReqs(FR, Reqs, PaddedReqs).
findCommonReqs([innerIf(L,R)|FR], Reqs, NewPaddedReqs):-
    findCommonReqs(L, Reqs, PaddedReqsL),
    findCommonReqs(R, Reqs, PaddedReqsR),
    padReqs(PaddedReqsL,PaddedReqsR,PaddedReqs),
    findCommonReqs(FR, PaddedReqs, NewPaddedReqs).
findCommonReqs([fpad(_,_,_,_,_,_,_,_)|FR],Reqs,PaddedReqs):- findCommonReqs(FR, Reqs, PaddedReqs).
findCommonReqs([ft(F,T,B,L)|FR],OldPaddedReqs,NewPaddedReqs):-
    functionReqs(F, SWReqs, HWReqs, FServicesReqs),
    padReqs((SWReqs,HWReqs,FServicesReqs, B,T,L), OldPaddedReqs, PaddedReqs),
    findCommonReqs(FR, PaddedReqs, NewPaddedReqs).
findCommonReqs([par(List)|FR], Reqs, NewPaddedReqs):- 
    findCommonReqs(List, Reqs, PaddedReqs),
    findCommonReqs(FR, PaddedReqs, NewPaddedReqs).

%calculates the common reqs between two reqs
padReqs((Sw1,(Mem1, Core1,CPU1),Ser1,Binds1,T1,Lat1),(Sw2,(Mem2, Core2,CPU2),Ser2,Binds2,T2,Lat2),(SwR,(MemR, CoreR,CPUR),SerR,BindsR,TMax,LatMin)):-
    union_sort(Sw1,Sw2, SwR),
    MemR is max(Mem1, Mem2),
    CoreR is max(Core1, Core2),
    CPUR is max(CPU1, CPU2),
    padServiceReqs(Ser1,Ser2, SerR), %finds min latency for same service reqs
    union_sort(Binds1,Binds2,BindsR),
    maxType(T1,T2,TMax),
    LatMin is min(Lat1,Lat2).

%in case of [(serviceReq, 5ms)] vs [(serviceReq, 10 ms)] we should select the first one
padServiceReqs([],[],[]).
padServiceReqs(Ser1,Ser2,SerR):-
    append(Ser1,Ser2,SerApp),
    sort(SerApp,SerAppSorted),
    buildMinSer(SerAppSorted, SerR).

%build the list with minimum latency for same service req
buildMinSer([X],[X]).
buildMinSer([(Ser1,Lat1),(Ser2,Lat2)|OtherS],[(Ser1,Lat1)|SerR]):-
    dif(Ser1,Ser2),
    buildMinSer([(Ser2,Lat2)|OtherS],SerR).
buildMinSer([(Ser,Lat1),(Ser,Lat2)|OtherS],SerR):-
    LatMin is min(Lat1,Lat2),
    buildMinSer([(Ser,LatMin)|OtherS],SerR).


%given the common reqs, it add the padding to every function
padToReqs([],_,[]).
padToReqs(end,(SwReqs,HWReqs,SerReqs, Binds,T,_),[fpad(fDummyPatch,T,Binds,0,SwReqs,HWReqs,SerReqs)]).
padToReqs([endPar|FR],(SwReqs,HWReqs,SerReqs, Binds,T,L),[endPar|TempFR]):-
    padToReqs(FR, (SwReqs,HWReqs,SerReqs, Binds,T, L), TempFR).
padToReqs([end|FR],(SwReqs,HWReqs,SerReqs, Binds,T,L),[fpad(fDummyPatch,T,Binds,0,SwReqs,HWReqs,SerReqs)|TempFR]):-
    padToReqs(FR, (SwReqs,HWReqs,SerReqs, Binds,T, L), TempFR).
padToReqs([dummyPar|FR],(SwReqs,HWReqs,SerReqs, Binds,T,L),[fpad(fDummyPatch,T,Binds,0,SwReqs,HWReqs,SerReqs)|TempFR]):-
    padToReqs(FR, (SwReqs,HWReqs,SerReqs, Binds,T, L), TempFR).
padToReqs([fpad(FId,T,Binds,0,SwReqs,HWReqs,SerReqs)|FR],(SwReqs,HWReqs,SerReqs, Binds,Tr,L),[fpad(FId,T,Binds,0,SwReqs,HWReqs,SerReqs)|TempFR]):-
    padToReqs(FR, (SwReqs,HWReqs,SerReqs, Binds,Tr,L), TempFR).
%ft that does not need padding
padToReqs([ft(FId,Ft,Fb,Fl)|FR],Reqs,[ft(FId,Ft,Fb,Fl)|FRResult]):-
   functionReqs(FId,SwF,HWF,FSerReqs),
    dontNeedsPad((SwF,HWF,FSerReqs, Fb,Ft,Fl),Reqs),
    padToReqs(FR,Reqs, FRResult).
%ft that needs padding and dummy function for service call
padToReqs([ft(FId,Ft,Fb,Fl)|FR],(SwReqs,HWReqs,SerReqs, Binds,Type,Lat),[seq(fpad(FId,Type,Fb,Lat,SwReqs,HWReqs,FSerReqs),DummyF)|FRResult]):-
    functionReqs(FId,SwF,HWF,FSerReqs),
    \+(dontNeedsPad((SwF,HWF,FSerReqs, Fb,Ft,Fl),(SwReqs,HWReqs,SerReqs, Binds,Type,Lat))),
    subtract(SerReqs,FSerReqs,ServicesToPad),
    dif(ServicesToPad, []),
    subtract(Binds,Fb,DummyBinds),
    createDummy(ServicesToPad,SwReqs,HWReqs,DummyBinds,Type, DummyF),
    padToReqs(FR,(SwReqs,HWReqs,SerReqs, Binds,Type,Lat), FRResult).
%ft that need padding but not dummy function for service call
padToReqs([ft(FId,Ft,Fb,Fl)|FR],(SwReqs,HWReqs,SerReqs, Binds,Type,Lat),[fpad(FId,Type,Fb,Lat,SwReqs,HWReqs,FSerReqs)|FRResult]):-
    functionReqs(FId,SwF,HWF,FSerReqs),
    \+(dontNeedsPad((SwF,HWF,FSerReqs, Fb,Ft,Fl),(SwReqs,HWReqs,SerReqs, Binds,Type,Lat))),
    subtract(SerReqs,FSerReqs,[]),
    padToReqs(FR,(SwReqs,HWReqs,SerReqs, Binds,Type,Lat), FRResult).
padToReqs([innerIf(L,R)|FR],Reqs,[innerIf(LRes,RRes)|FRResult]):-
    padToReqs(L,Reqs,LRes),
    padToReqs(R,Reqs,RRes),
    padToReqs(FR,Reqs,FRResult).
padToReqs([par(List)|FR],Reqs,[par(ListRes)|FRResult]):-
    padToReqs(List, Reqs, ListRes),
    padToReqs(FR,Reqs,FRResult).

%create a dummy function to make service call
createDummy(SerReqs,_,_,Binds,T, fpad(fDummy, T,Binds,0,[],(0,0,0),SerReqs)).

%used to avoid to add reduntant padding to functions already with the common reqs
dontNeedsPad((SwF, HWReqs, SerF,Binds,T,L), (SwReqs,HWReqs,SerReqs, BindsReq,T,L)):-
    \+(set_dif(SwF,SwReqs)),
    \+(set_dif(SerF,SerReqs)),
    \+(set_dif(Binds,BindsReq)).


%------------------------------------------
%Format the output of the padding building sequences, inner ifs and parallels has input orchestrations
formatPaddedOrch(PadOrchestration, FormattedPadOrchestration):-
    formatP(PadOrchestration, FormattedPadOrchestration).

formatP(ft(Fid,Label,Binding,Latency),ft(Fid,Label,Binding,Latency)).
formatP(seq(F1,F2),seq(Wf1,Wf2)) :-
	formatP(F1,Wf1), formatP(F2,Wf2).
formatP(if(G, L, R), if(G,Fl,Fr)) :-
	 formatIf(L,Fl), formatIf(R,Fr).
formatP(par(List), par(Res)):-
    formatPar(List, Res).

formatIf(A,A):- \+(is_list(A)).
formatIf([A],A).
formatIf([ft(Fid,Label,Binding,Latency),Succ|Rem],seq(ft(Fid,Label,Binding,Latency),FRest)):- \+(isInnerIf(Succ)), formatIf([Succ|Rem], FRest).
formatIf([fpad(FId, T, B, L, Sw, HW, Ser),Succ|Rem],seq(fpad(FId, T, B, L, Sw, HW, Ser),FRest)):- \+(isInnerIf(Succ)), formatIf([Succ|Rem], FRest).
formatIf([seq(A,B)|Rem],seq(seq(ResA,ResB),ResRem)):-
    formatIf(A,ResA),
    formatIf(B,ResB),
    formatIf(Rem,ResRem).
formatIf([Guard,innerIf(L,R)|Rem], if(Guard,Fl,Fr)):-
    \+(isInnerIf(Guard)),
    buildBranches(L,R,Rem,Fl,Fr,[]).
formatIf([Guard,innerIf(L,R)|Rem], seq(if(Guard,Fl,Fr),FormattedAfterIf)):-
   \+(isInnerIf(Guard)),
    buildBranches(L,R,Rem,Fl,Fr,AfterIf),
    dif(AfterIf,[]),
    formatIf(AfterIf, FormattedAfterIf).
formatIf([par(Par)|Rem], par(MergedPar)):-
    mergePar(Par,Rem,MergedPar,[]).
formatIf([par(Par)|Rem], seq(par(MergedPar), FormattedAfterPar)):-
    mergePar(Par,Rem,MergedPar,AfterPar),
    formatIf(AfterPar, FormattedAfterPar),
    dif(FormattedAfterPar,[]).

formatPar([],[]).
formatPar([P|Ps],[Pp|Psp]):-
    formatP(P,Pp),
    formatPar(Ps, Psp).
buildBranches(L,R,[],LSeq,RSeq,[]):-
    buildSeq(L,LSeq),
    buildSeq(R,RSeq).
buildBranches(L1,R1,[innerIf(L2,R2)|Rem],seq(LSeq,Lf),seq(RSeq,Rf),AfterIf):-
    buildSeq(L1,LSeq),
    buildSeq(R1,RSeq),
    buildBranches(L2,R2,Rem, Lf,Rf, AfterIf).
buildBranches(L1,R1,[C|Rem],LSeq,RSeq,[C|Rem]):-
    \+(isInnerIf(C)),
    buildSeq(L1,LSeq),
    buildSeq(R1,RSeq).

buildSeq([A],A).
buildSeq([A|List],seq(A,Res)):-
    dif(List,[]),
    buildSeq(List,Res).

mergePar(Par,[],Par,[]).
mergePar(Par1,[par(Par2)|Rem],MergedPar,AfterPar):-
    mergeParLists(Par1,Par2,ParMerged),
    mergePar(ParMerged, Rem, MergedPar, AfterPar).
mergePar(Par,[F|Rem],Par,[F|Rem]):-
    \+(isPar(F)).

mergeParLists([],[],[]).
mergeParLists([L1|List1],[L2|List2],[seq(L1,L2)|Lmerge]):-
    dif(L2,endPar),
    mergeParLists(List1,List2,Lmerge).
mergeParLists([L1|List1],[endPar|List2],[L1|Lmerge]):-
    mergeParLists(List1,List2,Lmerge).

isInnerIf(innerIf(_,_)).
isPar(par([_|_])).