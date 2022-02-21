%---------------- test predicates
%----------------------------------
padding(TypedOrchestration, FormattedPadOrchestration):-
    innerPadding(TypedOrchestration, PadOrchestration),
    formatPaddedOrch(PadOrchestration, FormattedPadOrchestration).

%main padding predicate, only if case is relevant for padding
innerPadding(ft(Fid,Label,Binding,Latency),ft(Fid,Label,Binding,Latency)).
innerPadding(seq(F1,F2),seq(Wf1,Wf2)) :-
	innerPadding(F1,Wf1), innerPadding(F2,Wf2).
innerPadding(if(ft(Fid,Type,Binding,Latency), LeftB, RightB), if(ft(Fid,Type,Binding,Latency), LeftPadded,RightPadded)) :-
	paddingIf(LeftB, RightB, LeftPadded,RightPadded).
innerPadding(par(List), par(PaddedList)) :-
    dif(List,[]), paddingPar(List, PaddedList).
innerPadding(par([]),[]).

%paddingIf(TrueBranch, RightBranch, LeftBranchAfterPadding, RightBranchAfterPadding)
%given the left and right branch it pad them and give left branch padded and right branch padded
%end marks the end of a branch, seq became list, inner if(F,L,R) became [F,innerIf(FirstL,FirstR)...innerif(endL,endR)]
paddingIf(end,end,[],[]).
paddingIf(TrueBranch,FalseBranch,TrueBranchPadded, FalseBranchPadded):-
    \+ (TrueBranch=end,FalseBranch=end),
    extractToPad(TrueBranch, Ft, TrueContinuation),
    extractToPad(FalseBranch,Ff, FalseContinuation),
    pad(Ft, Ff, Ftp, Ffp),
    paddingIf(TrueContinuation, FalseContinuation, TTemp, FTemp),
    append(Ftp,TTemp, TrueBranchPadded),
    append(Ffp, FTemp, FalseBranchPadded).



paddingPar([],[]).
paddingPar([P|Plist], [Pres|PlistRes]):-
    innerPadding(P,Pres),
    paddingPar(Plist, PlistRes).

%dummyPad(ListOfFunctionsOnOtherBranch, DummyFunction) insert a dummy function to balance branches
%input is a list because inner if create sub branches to be pad
%single ft to clone
dummyPad([ft(FId,Label,Bindings,Lat)],fpad(FCid,Label,Bindings,Lat,Sw,Hw,Ser)):- %dummy clone per tappare buco su ramo
    functionReqs(FId, Sw,Hw,Ser),
    gensym(fContextPad,FCid).
%multiple ft to clone
dummyPad(FList,fpad(FCid,Label,Bindings,Lat,Sw,Hw,Ser)):- 
    dif(FList,[_]),
    lowestType(Lowest),
    findCommonReqs(FList, ([], (0,0,0), [],[],Lowest,inf), (Sw,Hw,Ser,Bindings,Label,Lat)),
    gensym(fContextPad,FCid).

%extractToPad(Branch, F to pad, Continuation)
%given a branch, extract the function(s) to be padded and the continuation of the branch
extractToPad(end,end,end).
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
isEndList(L) :- \+ (member(X,L), dif(X,end)).

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

%check if there are par involved and build the par structure to balance if and inner ifs branches
checkPar(Left,Right,NewLeft,NewRight):-
    longestPar(Left, LongestLeft),
    longestPar(Right, LongestRight),
    greaterPar(LongestLeft, LongestRight, Longest),
    buildPar(Longest, Left, NewLeft),
    buildPar(Longest, Right, NewRight).

%it finds the biggest par structure of the branch that will be used to balance the pars of if branches
longestPar([par(ParList)],ParList).
longestPar([innerIf(L,R)],Longest):-
    longestPar(L, LongestL),
    longestPar(R, LongestR),
    greaterPar(LongestL, LongestR, Longest).
longestPar(X,[]):-
    \+(isPar(X)), \+(isInnerIf(X)).

%given two par structure, generated the template for the max common par structure
greaterPar([],[],[]).
greaterPar([endPar|P1],[endPar|P2],[endPar|PG]):-
    greaterPar(P1,P2,PG).
greaterPar([X|P1],[endPar|P2],[dummyPar|PG]):-
    dif(X,endPar),
    greaterPar(P1,P2,PG).
greaterPar([endPar|P1],[X|P2],[dummyPar|PG]):-
    dif(X,endPar),
    greaterPar(P1,P2,PG).
greaterPar([X|P1],[Y|P2],[dummyPar|PG]):-
    dif(X,endPar),dif(Y,endPar),
    greaterPar(P1,P2,PG).
greaterPar([],[endPar|P2],[endPar|PG]):-
    greaterPar([],P2,PG).
greaterPar([],[X|P2],[dummyPar|PG]):-
    dif(X,endPar),
    greaterPar([],P2,PG).
greaterPar([endPar|P1],[],[endPar|PG]):-
    greaterPar(P1,[],PG).
greaterPar([X|P1],[],[dummyPar|PG]):-
    dif(X,endPar),
    greaterPar(P1,[],PG).

%buildPar(LongestPar, Branch, NewBranch) it build the longest par structure in Branch, building id in NewBranch
%no par structure to insert
buildPar([],Branch, Branch).
%no par and no innerIf
buildPar(ParList, Branch, NewBranch):-
    dif(ParList,[]), \+(isPar(Branch)), \+(isInnerIf(Branch)),
    buildParStructure([par(ParList)],Branch, NewBranch).
%different par structure used, to balance
buildPar(ParList, [par(BranchParList)], [par(NewBranchList)]):-
    dif(ParList,[]), dif(ParList, BranchParList),
    balanceLists(ParList, BranchParList, NewBranchList).
%innerIf
buildPar(ParList, [innerIf(L,R)], [innerIf(NewL,NewR)]):-
    dif(ParList,[]),
    buildPar(ParList,L,NewL),
    buildPar(ParList,R, NewR).

%build a par structure inside a branch without par or innerIf
buildParStructure([par(ParList)], Branch, [par(BranchRes)]):-
    createParInBranch(ParList, Branch, BranchRes).

%create the branch with a par structure as ParList
createParInBranch(ParList,end,Res):-
    createParInBranch(ParList,[],Res).
createParInBranch([],B,B).
createParInBranch([endPar|Ps],B, [endPar|Bres]) :- createParInBranch(Ps,B,Bres).
createParInBranch([F1|Ps],[F2|Bs], [F2|Bres]) :- 
    dif(F1,endPar),
    createParInBranch(Ps,Bs,Bres).
createParInBranch([F1|Ps],[], [dummyPar|Bres]) :- 
    dif(F1,endPar),
    createParInBranch(Ps,[],Bres).

%balanceLists(TemplateParList, ParList, ParListTemplated)
%given the template par structure, build it in the given par list
balanceLists([],[],[]).
balanceLists([endPar|L],[],[endPar|NewS]):-
    balanceLists(L,[],NewS).
balanceLists([dummyPar|L],[],[dummyPar|NewS]):-
    balanceLists(L,[],NewS).
balanceLists([endPar|L],[endPar|S],[endPar|NewS]):-
    balanceLists(L,S,NewS).
balanceLists([dummyPar|L],[endPar|S],[dummyPar|NewS]):-
    balanceLists(L,S,NewS).
balanceLists([dummyPar|L],[Element|S],[Element|NewS]):-
    dif(Element,endPar),
    balanceLists(L,S,NewS).

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

%given the common reqs, it add the padding to every function %TODO-Question: dummypatch lat at 0?
padToReqs([],_,[]).
padToReqs(end,(SwReqs,HWReqs,SerReqs, Binds,T,_),[fpad(FCid,T,Binds,0,SwReqs,HWReqs,SerReqs)]):-
    gensym(fContextPad,FCid).
padToReqs([endPar|FR],(SwReqs,HWReqs,SerReqs, Binds,T,L),[endPar|TempFR]):-
    padToReqs(FR, (SwReqs,HWReqs,SerReqs, Binds,T, L), TempFR).
padToReqs([end|FR],(SwReqs,HWReqs,SerReqs, Binds,T,L),[fpad(FCid,T,Binds,0,SwReqs,HWReqs,SerReqs)|TempFR]):-
    padToReqs(FR, (SwReqs,HWReqs,SerReqs, Binds,T, L), TempFR),
    gensym(fContextPad,FCid).
padToReqs([dummyPar|FR],(SwReqs,HWReqs,SerReqs, Binds,T,L),[fpad(FCid,T,Binds,0,SwReqs,HWReqs,SerReqs)|TempFR]):-
    padToReqs(FR, (SwReqs,HWReqs,SerReqs, Binds,T, L), TempFR),
    gensym(fContextPad,FCid).
padToReqs([fpad(FId,T,Binds,0,SwReqs,HWReqs,SerReqs)|FR],(SwReqs,HWReqs,SerReqs, Binds,Tr,L),[fpad(FId,T,Binds,0,SwReqs,HWReqs,SerReqs)|TempFR]):-
    padToReqs(FR, (SwReqs,HWReqs,SerReqs, Binds,Tr,L), TempFR).
%ft that does not need padding
padToReqs([ft(FId,Ft,Fb,Fl)|FR],Reqs,[ft(FId,Ft,Fb,Fl)|FRResult]):-
   functionReqs(FId,SwF,HWF,FSerReqs),
    dontNeedPad((SwF,HWF,FSerReqs, Fb,Ft,Fl),Reqs),
    padToReqs(FR,Reqs, FRResult).
%ft that needs padding and dummy function for service call
padToReqs([ft(FId,Ft,Fb,Fl)|FR],(SwReqs,HWReqs,SerReqs, Binds,Type,Lat),[Seq|FRResult]):-
    functionReqs(FId,SwF,HWF,FSerReqs),
    \+(dontNeedPad((SwF,HWF,FSerReqs, Fb,Ft,Fl),(SwReqs,HWReqs,SerReqs, Binds,Type,Lat))),
    serviceSubtract(SerReqs,FSerReqs,ServicesToPad),
    dif(ServicesToPad, []),%TODO same service, different bind needs the padding
    subtract(Binds,Fb,DummyBinds),
    createDummy(ServicesToPad,DummyBinds,Type, DummyF),
    padToReqs(FR,(SwReqs,HWReqs,SerReqs, Binds,Type,Lat), FRResult),
    padOrderedSeq(fpad(FId,Type,Fb,Lat,SwReqs,HWReqs,FSerReqs), DummyF, Seq).
%ft that need padding but not dummy function for service call
padToReqs([ft(FId,Ft,Fb,Fl)|FR],(SwReqs,HWReqs,SerReqs, Binds,Type,Lat),[fpad(FId,Type,Fb,Lat,SwReqs,HWReqs,FSerReqs)|FRResult]):-
    functionReqs(FId,SwF,HWF,FSerReqs),
    \+(dontNeedPad((SwF,HWF,FSerReqs, Fb,Ft,Fl),(SwReqs,HWReqs,SerReqs, Binds,Type,Lat))),
    serviceSubtract(SerReqs,FSerReqs,[]),
    padToReqs(FR,(SwReqs,HWReqs,SerReqs, Binds,Type,Lat), FRResult).
padToReqs([innerIf(L,R)|FR],Reqs,[innerIf(LRes,RRes)|FRResult]):-
    padToReqs(L,Reqs,LRes),
    padToReqs(R,Reqs,RRes),
    padToReqs(FR,Reqs,FRResult).
padToReqs([par(List)|FR],Reqs,[par(ListRes)|FRResult]):-
    padToReqs(List, Reqs, ListRes),
    padToReqs(FR,Reqs,FRResult).

%create a dummy function to make service call
createDummy(SerReqs,Binds,T, fpad(FSid, T,Binds,0,[],(0,0,0),SerReqs)):-
    gensym(fServicePad,FSid).

%used to avoid to add reduntant padding to functions already with the common reqs
dontNeedPad((SwF, HWReqs, SerF,Binds,T,L), (SwReqs,HWReqs,SerReqs, BindsReq,T,L)):-
    \+(set_dif(SwF,SwReqs)),
    \+(set_dif(SerF,SerReqs)),
    \+(set_dif(Binds,BindsReq)).

%remove services of L2 form L1 (can't use list:subract because of latencies)
serviceSubtract(L1, L2, Lres):-
    findall((S,L), 
           (member((S,L),L1),\+ member((S,_),L2)),
            Lres).

%assuming one service in SerReqs it create the seq using the alphabetical order of the services
padOrderedSeq(fpad(FId,Type,Fb,Lat,SwReqs,HWReqs,FSerReqs),fpad(FSid, T,Binds,0,[],(0,0,0),SerReqs), seq(fpad(FId,Type,Fb,Lat,SwReqs,HWReqs,FSerReqs),fpad(FSid, T,Binds,0,[],(0,0,0),SerReqs))):-
    alphaOrder(FSerReqs, SerReqs, FSerReqs).
padOrderedSeq(fpad(FId,Type,Fb,Lat,SwReqs,HWReqs,FSerReqs),fpad(FSid, T,Binds,0,[],(0,0,0),SerReqs), seq(fpad(FSid, T,Binds,Lat,[],(0,0,0),SerReqs),fpad(FId,Type,Fb,0,SwReqs,HWReqs,FSerReqs))):-
    alphaOrder(FSerReqs, SerReqs, SerReqs).


alphaOrder([(A,La)],[(B,_)],[(A,La)]):-
    sort([A,B], [A,B]).
alphaOrder([(A,_)],[(B,Lb)],[(B,Lb)]):-
    sort([A,B], [B,A]).
alphaOrder(A,[],A):-
    dif(A,[]).
alphaOrder([],B,B):-
    dif(B,[]).
alphaOrder([],[],[]).


%------------------------------------------
%Format the output of the padding building sequences, inner ifs and parallels has input orchestrations
formatPaddedOrch(PadOrchestration, FormattedPadOrchestration):-
    formatP(PadOrchestration, FormattedPadOrchestration).
%basic formatting
formatP(ft(Fid,Label,Binding,Latency),ft(Fid,Label,Binding,Latency)).
formatP(fpad(FId,T,B,L,Sw,HW,Ser),fpad(FId,T,B,L,Sw,HW,Ser)).
formatP(seq(F1,F2),seq(Wf1,Wf2)) :-
	formatP(F1,Wf1), formatP(F2,Wf2).
formatP(par(List), par(Res)):-
    formatPar(List, Res).
formatP(if(G, L, R), if(G,Fl,Fr)) :-
	 formatIf(L,Fl), formatIf(R,Fr).

%format par construct
formatPar([],[]).
formatPar([P|Ps],[Pp|Psp]):-
    formatP(P,Pp),
    formatPar(Ps, Psp).

%if formatting
%formatIf(A,A):- \+(is_list(A)).
%single element cases
formatIf([ft(Fid,Label,Binding,Latency)],ft(Fid,Label,Binding,Latency)).
formatIf([fpad(FId, T, B, L, Sw, HW, Ser)],fpad(FId, T, B, L, Sw, HW, Ser)).
formatIf([seq(A,B)],seq(ResA,ResB)):-
    formatIf([A],ResA), formatIf([B],ResB).
%multi element, to avoid lost guard of innerif    
formatIf([ft(Fid,Label,Binding,Latency),Succ|Rem],seq(ft(Fid,Label,Binding,Latency),FRest)):- \+(isInnerIf(Succ)), formatIf([Succ|Rem], FRest).
formatIf([fpad(FId, T, B, L, Sw, HW, Ser),Succ|Rem],seq(fpad(FId, T, B, L, Sw, HW, Ser),FRest)):- \+(isInnerIf(Succ)), formatIf([Succ|Rem], FRest).
formatIf([seq(A,B),Succ|Rem],seq(seq(ResA,ResB),ResRem)):-
    \+(isInnerIf(Succ)),
    formatIf([A],ResA),
    formatIf([B],ResB),
    formatIf([Succ|Rem],ResRem).
formatIf([Guard,innerIf(L,R)|Rem], if(GuardF,Fl,Fr)):-
    \+(isInnerIf(Guard)),
    formatIf([Guard], GuardF),
    separateInnerIf([innerIf(L,R)|Rem], Left,Right,[]),
    formatIf(Left,Fl),
    formatIf(Right,Fr).
formatIf([Guard,innerIf(L,R)|Rem], seq(if(GuardF,Fl,Fr),FormattedAfterIf)):-
    \+(isInnerIf(Guard)),
    formatIf([Guard], GuardF),
    separateInnerIf([innerIf(L,R)|Rem], Left,Right,AfterIf), 
    dif(AfterIf,[]),
    formatIf(Left,Fl),
    formatIf(Right,Fr),
    formatIf(AfterIf, FormattedAfterIf).
formatIf([par(Par)|Rem], par(FormattedPar)):-
    mergePar(Par,Rem,MergedPar,[]),
    formatParIf(MergedPar, FormattedPar).
formatIf([par(Par)|Rem], seq(par(FormattedPar), FormattedAfterPar)):-
    mergePar(Par,Rem,MergedPar,AfterPar),
    dif(AfterPar,[]),
    formatParIf(MergedPar, FormattedPar),
    formatIf(AfterPar, FormattedAfterPar).

%separateInnerIf: build left and right branches from innerIfs
%MainList, LeftBranchSeparated, RightBranchSeparated, StuffAfterIf
separateInnerIf([],[],[],[]).
separateInnerIf([E|Rem],[],[],[E|Rem]):-
    \+(isInnerIf(E)).
separateInnerIf([innerIf([L],[R])|Rem],[L|LeftBranch],[R|RightBranch],Rest):-
    separateInnerIf(Rem, LeftBranch,RightBranch, Rest).

%mergePar: builds a par list, giving the merging and the continuation after the par
%ParList, SuccessorOnBranch, MergedPar, ContinuationAfterPar
mergePar(Par,[],Par,[]).
mergePar(Par1,[par(Par2)|Rem],MergedPar,AfterPar):-
    mergeParLists(Par1,Par2,ParMerged),
    mergePar(ParMerged, Rem, MergedPar, AfterPar).
mergePar(Par,[F|Rem],Par,[F|Rem]):-
    \+(isPar(F)).

%add the element of the second list to the first list skipping endPars
%the result is a list of list, each list is a parallel composition to format
mergeParLists([],[],[]).
mergeParLists([L1|List1],[L2|List2],[[L1,L2]|Lmerge]):-
    \+(is_list(L1)),
    dif(L2,endPar),
    mergeParLists(List1,List2,Lmerge).
mergeParLists([L1|List1],[endPar|List2],[[L1]|Lmerge]):-
     \+(is_list(L1)),
    mergeParLists(List1,List2,Lmerge).
mergeParLists([L1|List1],[L2|List2],[Lres|Lmerge]):-
    is_list(L1),
    dif(L2,endPar),
    mergeParLists(List1,List2,Lmerge),
    append(L1,[L2],Lres).
mergeParLists([L1|List1],[endPar|List2],[L1|Lmerge]):-
    is_list(L1),
    mergeParLists(List1,List2,Lmerge).

%format every component of a parallel inside an if
formatParIf([],[]).
formatParIf([P|Ps],[Pp|Psp]):-
    formatIf(P,Pp),
    formatParIf(Ps, Psp).

%check if something is an innerIf
isInnerIf(innerIf(_,_)).
isInnerIf([innerIf(_,_)]).
%checks if something is a par
isPar(par([_|_])).
isPar([par([_|_])]).