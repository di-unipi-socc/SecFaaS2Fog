%orchestration costructs: f,seq, if, par
wellFormed(ft(Fid,Binding,Label,Latency),ft(Fid,Binding,Label,Latency)).
wellFormed(seq(F1,F2),seq(Wf1,Wf2)) :-
	wellFormed(F1,Wf1), wellFormed(F2,Wf2).
wellFormed(if(ft(Fid,Binding,Type,Latency), F1, F2), seq(ft(Fid,Binding,Type,Latency), Aggregation)) :-
	wellFormedIf(F1, F2, Aggregation).

wellFormed(par(Par), par(ParList)) :-
	wellFormedPar(Par,ParList).

wellFormedPar([],[]).
wellFormedPar([P|Ps],[Wfp|WfPs]):-
	wellFormed(P,Wfp), wellFormedPar(Ps,WfPs).

%aggregate is vertical, blob is horizontal
wellFormedIf(F1,F2,IfAgg) :-
	%wellFormed(F1,_), wellFormed(F2,_),
	empty(E),
	checkAggregation(noblob,E, F1, F2, noblob,_,IfAgg).

empty((([],(0,0,0),[]),([],(0,0,0),[]),[],[],Lowest,inf,[])):- lowestType(Lowest).
%ModeReqs = Mode, (Fs,TrueReqs), (Fs,FalseReqs))
%Mode == noblob, Reqs = []
%Mode == blob, Reqs to be aggregated

%checkAggregation(Input ModeReqs, TrueBranch, FalseBranch, Result ModeReqs)
%checkAggregation -> check the aggregation of the branches, given the input mode reqs
%function case
% checkAggregation(a,noblob,_, ft(F,Binding,Type,Lat), ft(F,Binding,Type,Lat),noblob,E,blob([F,Binding],Type, Lat,Req)) :-
% 	getReqs(F,Req),
% 	empty(E).
% checkAggregation(a,blob,(Agg1,Agg2,AggBinds1,AggBinds2,TypeMax,Lat,BlobFunctions), ft(F,Binding,Type,_), ft(F,Binding,Type,Lat),blob,(NewAgg1, NewAgg2,NewAggBinds,NewTypeMax,Lat,NewBlob),NewBlob) :-
% 	getReqs(F,Req),
% 	sumReqs(Agg1, Req, NewAgg1),
% 	sumReqs(Agg2, Req, NewAgg2),
% 	append(Binding, AggBinds, NewAggBinds),
% 	maxType(Type, TypeMax, NewTypeMax),
% 	append(BlobFunctions, [(F,Binding)], NewBlob).

checkAggregation(noblob,_, ft(F1,Binding1,Type1,Lat1), ft(F2,Binding2,Type2,Lat2),noblob,E,blob([(F1,Binding1),(F2,Binding2)],TypeMax,Lat,Req)) :-
	aggregable(F1, F2),
	\+ set_dif(Binding1,Binding2),
	getReqs(F1,Req),
	Lat is min(Lat1, Lat2),
	maxType(Type1,Type2, TypeMax),
	empty(E).
checkAggregation(noblob,_, ft(F1,B1,T1,L1), ft(F2,B2,T2,L2),blob,(Req1,Req2,B1,B2,TypeMax,Lat,[(F1,B1),(F2,B2)]),_):-
	(\+ aggregable(F1, F2); set_dif(B1,B2)),
	getReqs(F1, Req1), getReqs(F2, Req2),
	Lat is min(L1, L2),
	maxType(T1,T2, TypeMax).

checkAggregation(blob,(Agg1,Agg2,AggBinds1,AggBinds2,TypeMax,Lat,BlobFunctions), ft(F1,B1,T1,_), ft(F2,B2,T2,_),blob,(NewAgg1, NewAgg2,NewAggBinds1, NewAggBinds2,NewTypeMax,Lat,NewBlob),[]) :-
	getReqs(F1, Req1), getReqs(F2, Req2),
	checkBlob(Agg1, Agg2, Req1, Req2, NewAgg1, NewAgg2, AggBinds1,AggBinds2,B1, B2, NewAggBinds1, NewAggBinds2, blob),
	maxType(T1,T2, TempTypeMax),
	maxType(TempTypeMax, TypeMax, NewTypeMax),
	append(BlobFunctions, [(F1,B1),(F2,B2)], NewBlob).
checkAggregation(blob,(Agg1,Agg2,AggBinds1,AggBinds2,TypeMax,Lat,BlobFunctions),ft(F1,B1,T1,_), ft(F2,B2,T2,_),noblob,E, blob(NewBlob,NewTypeMax,Lat,NewAgg)) :-
	getReqs(F1, Req1), getReqs(F2, Req2),
	checkBlob(Agg1, Agg2, Req1, Req2, NewAgg, NewAgg,AggBinds1,AggBinds2,B1, B2, NewAggBinds, NewAggBinds, noblob),
	maxType(T1,T2, TempTypeMax),
	maxType(TempTypeMax, TypeMax, NewTypeMax),
	append(BlobFunctions, [(F1,B1),(F2,B2)], NewBlob),
	empty(E).
%seq case
checkAggregation(OldMode,OldAgg, seq(F1,L1), seq(F2,L2), NewMode,NewAgg,seq(SeqAgg1,SeqAgg2)) :-
	checkAggregation(OldMode, OldAgg, F1, F2, noblob, e,SeqAgg1),
	checkAggregation(noblob, e, L1, L2, NewMode,NewAgg,SeqAgg2).
checkAggregation(OldMode,OldAgg, seq(F1,L1), seq(F2,L2), NewMode,NewAgg,SeqAgg2) :-
	checkAggregation(OldMode, OldAgg, F1, F2, blob, TempAgg,_),
	checkAggregation(blob, TempAgg, L1, L2, NewMode,NewAgg,SeqAgg2).
checkAggregation(_,(Agg1,Agg2,AggBinds1,AggBinds2,TypeMax,Lat,BlobFunctions), seq(F,L), ft(F1,B1,T1,Lat1), NewMode,NewAgg,SeqAgg) :-
	unrollSeq((Agg1,AggBinds1,TypeMax,Lat,BlobFunctions), F, L, (AggSeq,BlobBinds,NewTypeMax,NewBlobFunctions), LastF,NewBlobFunctions),
	checkAggregation(blob, (AggSeq, Agg2,BlobBinds,AggBinds2,NewTypeMax,Lat,NewBlobFunctions), LastF, ft(F1,B1,T1,Lat1), NewMode, NewAgg, SeqAgg).
checkAggregation(_,(Agg1,Agg2,AggBinds1,AggBinds2,TypeMax,Lat,BlobFunctions), ft(F1,B1,T1,Lat1), seq(F,L), NewMode,NewAgg,SeqAgg) :-
	unrollSeq((Agg2,AggBinds2,TypeMax,Lat,BlobFunctions), F, L, (AggSeq,BlobBinds,NewTypeMax,NewBlobFunctions), LastF,NewBlobFunctions),
	checkAggregation(blob, (Agg1,AggSeq,AggBinds1,BlobBinds,NewTypeMax,NewBlobFunctions), LastF, ft(F1,B1,T1,Lat1), NewMode, NewAgg, SeqAgg).
%par case
checkAggregation(OldMode,OldAgg, par(P1), par(P2), NewMode,NewAgg,par(ParAgg)) :-
	checkParAggregation(OldMode, OldAgg, P1, P2, NewMode, NewAgg,ParAgg).

checkParAggregation(Mode, Agg,[],[], Mode, Agg,Agg).
checkParAggregation(OldMode, OldAgg, [P1|Ps1], [P2|Ps2], NewMode, NewAgg,[PAgg|PsAgg]):-
	checkAggregation(OldMode, OldAgg, P1, P2, NewMode, NewAgg,PAgg),
	checkParAggregation(OldMode, OldAgg, Ps1, Ps2, NewMode, NewAgg,PsAgg).

getReqs(F, (Sw,Hw,Ser)):- functionReqs(F, Sw, Hw, Ser).
%TODO: latency and coming from blob
unrollSeq((Aggr,Binds,TypeMax,_,BlobFunctions), ft(F,B,T,_) , seq(A,B), NewAgg, LastF,BlobAgg):-
	getReqs(F, Req),
	sumReqs(Aggr, Req, SumAgg),
	union_sort(B, Binds, NewBinds),
	maxType(T, TypeMax, NewTypeMax),
	append(BlobFunctions, [(F,B)], NewBlob),
	unrollSeq((SumAgg, NewBinds, NewTypeMax, NewBlob), A, B, NewAgg, LastF, BlobAgg).
unrollSeq((Aggr,Binds,TypeMax,_,BlobFunctions), ft(F1,B1,T1,_) , ft(F2,B2,T2,L2), (SumAgg, NewBinds, NewTypeMax, NewBlob), ft(F2,B2,T2,L2),NewBlob):-
	getReqs(F1, Req),
	sumReqs(Aggr, Req, SumAgg),
	union_sort(B1, Binds, NewBinds),
	maxType(T1, TypeMax, NewTypeMax),
	append(BlobFunctions, [(F1,B1)], NewBlob).

%checkBlob -> sum reqs and check if the blob is ended
checkBlob(Agg1, Agg2, Req1, Req2, SumAgg1, SumAgg2,AggBinds1,AggBinds2,B1, B2, SortedBinds1, SortedBinds2, Mode):-
	sumReqs(Req1, Agg1, SumAgg1),
	sumReqs(Req2, Agg2, SumAgg2),
	append(B1, AggBinds1, NewAggBinds1),
	sort(NewAggBinds1, SortedBinds1),
	append(B2, AggBinds2, NewAggBinds2),
	sort(NewAggBinds2, SortedBinds2),
	blobMode(SumAgg1, SumAgg2, SortedBinds1, SortedBinds2,Mode).

%blobMode -> check summed reqs and give noblob\blob (finshed blob or to be continued)
blobMode(SumAgg1, SumAgg2, AggBinds1, AggBinds2, blob):-
	difReqs(SumAgg1,SumAgg2) ; set_dif(AggBinds1, AggBinds2) .
blobMode(SumAgg1, SumAgg2, AggBinds1, AggBinds2, noblob):-
	\+ difReqs(SumAgg1,SumAgg2),
	\+ set_dif(AggBinds1, AggBinds2).

%sumReqs -> union of SW and Ser reqs, max of HW reqs
sumReqs(e, Reqs, Reqs).
sumReqs(Reqs, e, Reqs).
sumReqs((OldSW, (OldMem, OldCore,OldCPU), OldSer), (SW,(Mem,Core,CPU),Ser), (NewSW,(NewMem, NewCore,NewCPU),NewSer)):-
	union_sort(OldSW, SW, NewSW),
	NewMem is max(OldMem, Mem),
	NewCore is max(OldCore, Core),
	NewCPU is max(OldCPU, CPU),
	union_sort(OldSer, Ser, NewSer).
%sumReqs([], (SW,(Mem,Core,CPU),Ser), (SW,(Mem, Core,CPU),Ser)).

%aggragable(functionId1, functionId2, Reqs1, Reqs2, yes\no)
%aggragable -> check if the two functions are aggragable and return their requirements
aggregable(F1, F2):-
	functionReqs(F1, Sw1, Hw1, Ser1),
	functionReqs(F2, Sw2, Hw2, Ser2),
	eqReqs((Sw1, Hw1, Ser1),(Sw2, Hw2, Ser2)).

%difReqs -> given two sets of reqs check if they are different
difReqs((Sw1, Hw1, Ser1),(Sw2, Hw2, Ser2)):-
	set_dif(Sw1,Sw2); dif(Hw1, Hw2); set_dif(Ser1,Ser2).

eqReqs((Sw1, Hw, Ser1),(Sw2, Hw, Ser2)):-
	sort(Sw1, Sw), sort(Sw2,Sw),
	sort(Ser1, Ser), sort(Ser2, Ser).

union_sort(L1,L2,Lres):-
	append(L1,L2,Ltemp),
	sort(Ltemp, Lres).
