%check the well formedness of the orchestration, creating blobs for if cases
blobify(ft(Fid,Label,Binding,Latency),ft(Fid,Label,Binding,Latency)).
blobify(seq(F1,F2),seq(Wf1,Wf2)) :-
	blobify(F1,Wf1), blobify(F2,Wf2).
blobify(if(ft(Fid,Type,Binding,Latency), F1, F2), seqIf(ft(Fid,Type,Binding,Latency), Aggregation)) :-
	blobifyIf(F1, F2, Aggregation).

blobify(par(Par), par(ParList)) :-
	blobifyPar(Par,ParList).

blobifyPar([],[]).
blobifyPar([P|Ps],[Wfp|WfPs]):-
	blobify(P,Wfp), blobifyPar(Ps,WfPs).

%aggregate is vertical, blob is horizontal
blobifyIf(F1,F2,IfAgg) :-
	%blobify(F1,_), blobify(F2,_),
	checkAggregation(noblob,_,_,F1, F2, noblob,_,_,IfAgg). %start with no blob and must end with noblob

%checkAggregation -> check the aggregation of the branches, giving the resulting blobs
%same function case
checkAggregation(noblob,_,_, ft(F,Type1,Binding,Lat1), ft(F,Type2,Binding,Lat2),noblob,_,_,blob([(F,Binding)],TypeMax, Lat,Req)) :-
 	getReqs(F,Req),
	maxType(Type1, Type2, TypeMax),
	Lat is min(Lat1, Lat2).
checkAggregation(blob,(Agg1,Agg2,AggBinds1,AggBinds2),(TypeMax,Lat,BlobFunctions), ft(F,Type,Binding,_), ft(F,Type,Binding,Lat),blob,(NewAgg1, NewAgg2,NewAggBinds1,NewAggBinds2),(NewTypeMax,Lat,NewBlob),_) :-
 	getReqs(F,Req),
 	sumReqs(Agg1, Req, NewAgg1),
 	sumReqs(Agg2, Req, NewAgg2),
 	append(Binding, AggBinds1, NewAggBinds1),
 	append(Binding, AggBinds2, NewAggBinds2),
 	maxType(Type, TypeMax, NewTypeMax),
 	append(BlobFunctions, [(F,Binding)], NewBlob).
%different functions case
checkAggregation(noblob,_,_, ft(F1,Type1,Binding1,Lat1), ft(F2,Type2,Binding2,Lat2),noblob,_,_,blob([(F1,Binding1),(F2,Binding2)],TypeMax,Lat,Req)) :-
	F1\==F2, aggregable(F1, F2,Binding1,Binding2),
	getReqs(F1,Req),
	Lat is min(Lat1, Lat2),
	maxType(Type1,Type2, TypeMax).
checkAggregation(noblob,_,_, ft(F1,T1,B1,L1), ft(F2,T2,B2,L2),blob,(Req1,Req2,B1,B2),(TypeMax,Lat,[(F1,B1),(F2,B2)]),_):-
	F1\==F2, \+ aggregable(F1, F2,B1,B2),
	getReqs(F1, Req1), getReqs(F2, Req2),
	Lat is min(L1, L2),
	maxType(T1,T2, TypeMax).

checkAggregation(blob,Agg,(TypeMax,Lat,BlobFunctions), ft(F1,T1,B1,_), ft(F2,T2,B2,_),blob,NewAgg,(NewTypeMax,Lat,NewBlob),_) :-
	F1\==F2, getReqs(F1, Req1), getReqs(F2, Req2),
	checkBlob(Agg, Req1, Req2, B1, B2, NewAgg, blob),
	maxType(T1,T2, TempTypeMax),
	maxType(TempTypeMax, TypeMax, NewTypeMax),
	append(BlobFunctions, [(F1,B1),(F2,B2)], NewBlob).
checkAggregation(blob,Agg,(TypeMax,Lat,BlobFunctions),ft(F1,T1,B1,_), ft(F2,T2,B2,_),noblob,_,_, blob(NewBlob,NewTypeMax,Lat,NewAgg)) :-
	F1\==F2, getReqs(F1, Req1), getReqs(F2, Req2),
	checkBlob(Agg, Req1, Req2,B1,B2, (NewAgg,_,_,_), noblob),
	maxType(T1,T2, TempTypeMax),
	maxType(TempTypeMax, TypeMax, NewTypeMax),
	append(BlobFunctions, [(F1,B1),(F2,B2)], NewBlob).
%seq-seq case
checkAggregation(OldMode,OldAgg,OldInfo, seq(F1,L1), seq(F2,L2), NewMode,NewAgg,NewInfo,seq(SeqAgg1,SeqAgg2)) :-
	checkAggregation(OldMode, OldAgg, OldInfo,F1, F2, noblob, _,_,SeqAgg1),
	checkAggregation(noblob, _, _,L1, L2, NewMode,NewAgg,NewInfo,SeqAgg2).
checkAggregation(OldMode,OldAgg,OldInfo, seq(F1,L1), seq(F2,L2), NewMode,NewAgg,NewInfo,SeqAgg2) :-
	checkAggregation(OldMode, OldAgg, OldInfo,F1, F2, blob, TempAgg,TempInfo,_),
	checkAggregation(blob, TempAgg,TempInfo, L1, L2, NewMode,NewAgg,NewInfo,SeqAgg2).
%seq-ft case
checkAggregation(noblob,_, _,seq(F,L), ft(F1,T1,B1,Lat1), NewMode,NewAgg,NewInfo,SeqAgg) :-
	unrollSeq(noblob,_,F, L, (AggSeq,BlobBinds,SeqTypeMax,SeqLat,SeqBlobFunctions), LastF),
	Lat is min(SeqLat, Lat1),
	checkAggregation(blob, (AggSeq, ([],(0,0,0),[]),BlobBinds,[]),(SeqTypeMax,Lat,SeqBlobFunctions), LastF, ft(F1,T1,B1,Lat1), NewMode, NewAgg,NewInfo, SeqAgg).
checkAggregation(blob,(Agg1,Agg2,AggBinds1,AggBinds2),(TypeMax,Lat,BlobFunctions), seq(F,L), ft(F1,T1,B1,Lat1), NewMode,NewAgg,NewInfo,SeqAgg) :-
	unrollSeq(blob,(Agg1, AggBinds1, TypeMax, Lat, BlobFunctions),F, L, (AggSeq,BlobBinds,SeqTypeMax,SeqLat,SeqBlobFunctions), LastF),
	checkAggregation(blob, (AggSeq, Agg2,BlobBinds,AggBinds2),(SeqTypeMax,SeqLat,SeqBlobFunctions), LastF, ft(F1,T1,B1,Lat1), NewMode, NewAgg, NewInfo,SeqAgg).
%ft-seq case
checkAggregation(noblob,_,_, ft(F1,T1,B1,Lat1), seq(F,L), NewMode,NewAgg,NewInfo,SeqAgg) :-
	unrollSeq(noblob,_,F, L, (AggSeq,BlobBinds,SeqTypeMax,SeqLat,SeqBlobFunctions), LastF),
	Lat is min(SeqLat, Lat1),
	checkAggregation(blob, (([],(0,0,0),[]),AggSeq,[],BlobBinds),(SeqTypeMax,Lat,SeqBlobFunctions), LastF, ft(F1,T1,B1,Lat1), NewMode, NewAgg,NewInfo, SeqAgg).
checkAggregation(blob,(Agg1,Agg2,AggBinds1,AggBinds2),(TypeMax,Lat,BlobFunctions), ft(F1,T1,B1,Lat1), seq(F,L), NewMode,NewAgg,NewInfo,SeqAgg) :-
	unrollSeq(nonblob,(Agg2,AggBinds2,TypeMax,Lat,BlobFunctions), F, L, (AggSeq,BlobBinds,SeqTypeMax,SeqLat,SeqBlobFunctions), LastF),
	checkAggregation(blob, (Agg1,AggSeq,AggBinds1,BlobBinds),(SeqTypeMax,SeqLat,SeqBlobFunctions), LastF, ft(F1,T1,B1,Lat1), NewMode, NewAgg,NewInfo, SeqAgg).
%par case TODO: improve check
checkAggregation(OldMode,OldAgg,OldInfo, par(P1), par(P2), NewMode,NewAgg,NewInfo,par(ParAgg)) :-
	checkParAggregation(OldMode, OldAgg,OldInfo, P1, P2, NewMode, NewAgg,NewInfo,ParAgg).
%unroll the parallel
checkParAggregation(Mode, Agg,Info,[],[], Mode, Agg,Info,Agg).
checkParAggregation(OldMode, OldAgg,OldInfo, [P1|Ps1], [P2|Ps2], NewMode, NewAgg,NewInfo,[PAgg|PsAgg]):-
	checkAggregation(OldMode, OldAgg, OldInfo,P1, P2, NewMode, NewAgg,NewInfo,PAgg),
	checkParAggregation(OldMode, OldAgg,OldInfo, Ps1, Ps2, NewMode, NewAgg,NewInfo,PsAgg).%SUM all agg and info?
/*checkAggregation(noblob,_,_, par(P1), par(P2), NewMode,NewAgg,NewInfo,par(ParAgg)) :-
	checkParAggregation(OldMode, OldAgg,OldInfo, P1, P2,NewMode, NewAgg,NewInfo,[],UsedP2,ParAgg),
	length(P2,L), length(UsedP2,L).
%unroll the parallel
checkParAggregation(noblob, Agg,Info,[],_, noblob, Agg,Info,UsedP2,UsedP2,Agg).
checkParAggregation(OldMode, OldAgg,OldInfo, [P1|Ps1], P2, NewMode, NewAgg,NewInfo,UsedP2,NewUsedP2,[PAgg|PsAgg]):-
	member(X,P2), \+ member(X, UsedP2),
	checkAggregation(OldMode, OldAgg, OldInfo,P1, X, NewMode, NewAgg,NewInfo,PAgg),
	checkParAggregation(OldMode, OldAgg,OldInfo, Ps1, Ps2, NewMode, NewAgg,NewInfo,PsAgg).%SUM all agg and info?
*/
getReqs(F, (Sw,Hw,Ser)):- functionReqs(F, Sw, Hw, Ser).
%unroll a seq to check against a ft
unrollSeq(noblob,_, ft(F,T,B,L) , seq(A,B), NewAgg, LastF,BlobAgg):-
	getReqs(F, Req),
	unrollSeq(blob,(Req, B, T, L,[(F,B)]), A, B, NewAgg, LastF, BlobAgg).
unrollSeq(blob,(Aggr,Binds,TypeMax,Lat,BlobFunctions), ft(F,T,B,_) , seq(A,B), NewAgg, LastF,BlobAgg):-
	getReqs(F, Req),
	sumReqs(Aggr, Req, SumAgg),
	union_sort(B, Binds, NewBinds),
	maxType(T, TypeMax, NewTypeMax),
	append(BlobFunctions, [(F,B)], NewBlob),
	unrollSeq((SumAgg, NewBinds, NewTypeMax, Lat,NewBlob), A, B, NewAgg, LastF, BlobAgg).
unrollSeq(noblob,_, ft(F1,T1,B1,L1) , ft(F2,T2,B2,L2), (Req, B1, T1,L1,[(F1,B1)]), ft(F2,T2,B2,L2)):-
	getReqs(F1, Req).
unrollSeq(blob,(Aggr,Binds,TypeMax,FirstLat,BlobFunctions), ft(F1,T1,B1,_) , ft(F2,T2,B2,L2), (SumAgg, NewBinds, NewTypeMax, FirstLat,NewBlob), ft(F2,T2,B2,L2)):-
	getReqs(F1, Req),
	sumReqs(Aggr, Req, SumAgg),
	union_sort(B1, Binds, NewBinds),
	maxType(T1, TypeMax, NewTypeMax),
	append(BlobFunctions, [(F1,B1)], NewBlob).
%checkBlob -> sum reqs and check if the blob is ended
checkBlob((Agg1, Agg2, AggBinds1,AggBinds2),Req1, Req2,B1, B2, (NewAgg1, NewAgg2, SortedBinds1, SortedBinds2), Mode):-
	sumReqs(Req1, Agg1, NewAgg1),
	sumReqs(Req2, Agg2, NewAgg2),
	union_sort(B1, AggBinds1, SortedBinds1),
	union_sort(B2, AggBinds2, SortedBinds2),
	blobMode(NewAgg1, NewAgg2, SortedBinds1, SortedBinds2,Mode).

%blobMode -> check summed reqs and give noblob\blob (finshed blob or to be continued)
blobMode(SumAgg1, SumAgg2, AggBinds1, AggBinds2, noblob):-
	\+ difReqs(SumAgg1,SumAgg2),
	\+ set_dif(AggBinds1, AggBinds2).
blobMode(SumAgg1, SumAgg2, _, _, blob):-
	difReqs(SumAgg1,SumAgg2).
blobMode(SumAgg1, SumAgg2, AggBinds1, AggBinds2, blob):-
	\+ (difReqs(SumAgg1,SumAgg2)), set_dif(AggBinds1, AggBinds2).


%sumReqs -> union of SW and Ser reqs, max of HW reqs
sumReqs((OldSW, (OldMem, OldCore,OldCPU), OldSer), (SW,(Mem,Core,CPU),Ser), (NewSW,(NewMem, NewCore,NewCPU),NewSer)):-
	union_sort(OldSW, SW, NewSW),
	NewMem is max(OldMem, Mem),
	NewCore is max(OldCore, Core),
	NewCPU is max(OldCPU, CPU),
	union_sort(OldSer, Ser, NewSer).
%sumReqs([], (SW,(Mem,Core,CPU),Ser), (SW,(Mem, Core,CPU),Ser)).

%aggregable -> check if the two functions are aggregable
aggregable(F1,F2,B1,B2):-
	functionReqs(F1, Sw1, Hw1, Ser1),
	functionReqs(F2, Sw2, Hw2, Ser2),
	\+ set_dif(B1,B2),
	eqReqs((Sw1, Hw1, Ser1),(Sw2, Hw2, Ser2)).

%difReqs -> given two sets of reqs check if they are different
difReqs((Sw1, Hw1, Ser1),(Sw2, Hw2, Ser2)):- \+ eqReqs((Sw1, Hw1, Ser1),(Sw2, Hw2, Ser2)).
%	set_dif(Sw1,Sw2); dif(Hw1, Hw2); set_dif(Ser1,Ser2).

eqReqs((Sw1, Hw, Ser1),(Sw2, Hw, Ser2)):-
	sort(Sw1, Sw), sort(Sw2,Sw),
	sort(Ser1, Ser), sort(Ser2, Ser).
