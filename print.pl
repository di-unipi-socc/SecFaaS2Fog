printWF(OrchId,String):-
    functionOrch(OrchId, _, Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    printOrch(WFOrchestration, Graph,Preamble),
    string_concat(Preamble,Graph,String).

printTyped(OrchId, String):-
    functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    printOrch(TypedOrchestration, Graph,Preamble),
    string_concat(Preamble,Graph,String).

printPadded(OrchId, String):-
    functionOrch(OrchId, (_,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration),
    printOrch(PadOrchestration, Graph,Preamble),
    string_concat(Preamble,Graph,String).

printNotPaddedMap(OrchId, String):-
    functionOrch(OrchId, (GeneratorId,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    placement(TypedOrchestration, GeneratorId, Placement),
    printOrch(Placement, Graph,Preamble),
    string_concat(Preamble,Graph,String).

printMap(OrchId, String):-
    functionOrch(OrchId, (GeneratorId,TriggerTypes), Orchestration),
	wellFormed(Orchestration,WFOrchestration),
    typePropagation(TriggerTypes,WFOrchestration,TypedOrchestration),
    padding(TypedOrchestration, PadOrchestration),
    placement(PadOrchestration, GeneratorId, Placement),
    printOrch(Placement, Graph,Preamble),
    string_concat(Preamble,Graph,String).

printOrch(Orchestration, Graph, Preamble):-
    printOrch(Orchestration, Graph,_,_, Preamble).
printOrch(fun(Fid,_,_),"",[Fid],[Fid],"").
printOrch(ft(Fid,L,_,_),"",[Fid],[Fid],Res):-
    latticeColor(L,C),
    swritef(Res,'%w[color=%w];',[Fid,C]).
printOrch(fpad(Fid,L,_,_,_,_,_),"",[Fid],[Fid],Res):-
    latticeColor(L,C),
    swritef(Res,'%w[color=%w];',[Fid,C]).
printOrch(fp(Fid, L, B, Node),"",[Fid],[Fid],Res):-
    latticeColor(L,C),
    nodeLabel(Node, NL),
    latticeColor(NL,NC),
    swritef(StrColor,'%w[color=%w];',[Fid,C]),
    swritef(StrNode,'subgraph cluster_%w { %w label=%w color=%w;}',[Node,Fid,Node,NC]),
    printBindings(Fid,B,StrB),
    string_concat(StrNode,StrColor, StrTemp),
    string_concat(StrTemp,StrB,Res).
    
printOrch(seq(F1,F2),String,F1Init,F2End,Res):-
    printOrch(F1, F1String,F1Init,F1End,F1Res),
    printOrch(F2, F2String,F2Init,F2End,F2Res),
    printPairs(F1End,F2Init,SeqString),
    string_concat(F1String,SeqString,TempString),
    string_concat(TempString,F2String,String),
    string_concat(F1Res,F2Res,Res).
printOrch(if(Fguard, LeftB, RightB), String, [Fid], Ifend,Res):-
	printOrch(LeftB, LString, Linit,Lend,Lres),
    printOrch(RightB, RString, Rinit,Rend,Rres),
    printIf(Fguard,Linit,Rinit,IfString,Fid,Ifres),
    string_concat(IfString,LString, TempString2),
    string_concat(TempString2, RString, String),
    append(Lend,Rend,Ifend),
    string_concat(Ifres,Lres,TempString3),
    string_concat(TempString3,Rres,Res).
printOrch(par([P|Ps]), String, ParInit, ParEnd,Res) :-
    printOrch(par(Ps),PsString, PsInit, PsEnd,PsRes),
    printOrch(P, PString,PInit, PEnd,Pres),
    append(PInit,PsInit, ParInit),
    append(PEnd,PsEnd,ParEnd),
    string_concat(PString,PsString,String),
    string_concat(Pres,PsRes,Res).
printOrch(par([P]),PString, PInit, PEnd,Res):-
    printOrch(P, PString,PInit, PEnd,Res).

printPairs(F1,F2, Res):-
    \+(is_list(F1)), \+(is_list(F2)),
    printPair(F1,F2,Res).
printPairs([F|Fs],L2,Res):-
    printList(F,L2,ResF),
    printPairs(Fs,L2,ResFs),
    string_concat(ResF,ResFs,Res).
printPairs([],_,"").

printList(F,[F2|Fs],Res):-
    printPair(F,F2,ResF2),
    printList(F,Fs, ResFs),
    string_concat(ResF2,ResFs,Res).
printList(_,[],"").
printList(F,[F2|Fs],ArchLabel,Res):-
    printPair(F,F2,ArchLabel,ResF2),
    printList(F,Fs, ArchLabel,ResFs),
    string_concat(ResF2,ResFs,Res).
printList(_,[],_,"").

printPair(F1s,F2s,Res):-
    swritef(Res,'%w->%w;',[F1s,F2s]).
printPair(F1s,F2s, ArchLabel,Res):-
    swritef(Res,'%w->%w[label=%w];',[F1s,F2s,ArchLabel]).

printIf(Fguard,Left,Right,IfRes,F,Res):-
    printOrch(Fguard,_,[F],[F],Res),
    printList(F,Left,"T",LString),
    printList(F,Right,"F",RString),
    swritef(IfRes,'%w[shape=diamond];%w%w',[F,LString,RString]).
    /*string_concat(F, "[shape=diamond];",IfString),
    string_concat(IfString,LString,TempString),
    string_concat(TempString,RString,IfRes).*/

printBindings(_,[],"").
printBindings(Fid,[(_,S,N)|Bindings],Res):-
    nodeLabel(N, NL),
    latticeColor(NL,NC),
    swritef(FRes,
            '%w[shape=hexagon] subgraph cluster_%w { %w label=%w color=%w;} %w->%w[arrowhead=none,style=dashed];',
            [S,N,S,N,NC,Fid,S]),
    printBindings(Fid,Bindings,TempRes),
    string_concat(FRes,TempRes,Res).

