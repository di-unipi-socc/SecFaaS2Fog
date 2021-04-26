%%%% ProbLog does not support dif/2 %%%%
dif(A,B):- \+(A==B).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

query(placeChain(c1,P,C)).

%%% trustable relations declared by appOp
.8::trusts(appOp, provider1).
.2::trusts(appOp, provider3).
.9::trusts(appOp, ispOp).

%%% trust relations declared by provider1
%.9::trusts(provider1, provider2).
.4::trusts(provider1, provider3).

%%% trust relation declared by provider2
%.8::trusts(provider2, provider1).

%%% trust relation declared by provider3
.2::trusts(provider3, provider1).

%%% trust relations declared by ispOp  
.9::trusts(ispOp, provider1).
%.8::trusts(ispOp, provider2).
