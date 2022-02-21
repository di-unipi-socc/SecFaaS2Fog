:- use_module(library(aproblog)).

:- use_semiring(
    sr_plus,   % addition (arity 3)
    sr_times,  % multiplication (arity 3)
    sr_zero,   % neutral element of addition
    sr_one,    % neutral element of multiplication
    sr_neg,    % negation of fact label
    false,      % requires solving disjoint sum problem?
    false).    % requires solving neutral sum problem?

sr_zero((0.0, 0.0)).
sr_one((1.0, 1.0)).
sr_times((Ta, Ca), (Tb, Cb), (Tc, Cc)) :- Tc is Ta*Tb, Cc is Ca*Cb.
sr_plus((Ta, Ca), (Tb, Cb), (Ta, Ca)) :- Ca > Cb.
sr_plus((Ta, Ca), (Tb, Cb), (Tb, Cb)) :- Cb > Ca.
sr_plus((Ta, Ca), (Tb, Cb), (Tc, Ca)) :- Ca == Cb, Tc is max(Ta, Tb).
sr_neg((Ta, Ca), (Tb, Ca)) :- Tb is 1.0-Ta.


%%%% ProbLog does not support dif/2 %%%%
dif(A,B):- \+(A==B).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

query(faas2fog(chainGath,P)).
%query(trusts(appOp,X,4)).

trustOpinion(X,X).

trusts(A,B,D) :-
    D > 0,
    trustOpinion(A,B).
trusts(A,B,D) :-
    D > 0,
    trustOpinion(A,C),
    NewD is D - 1,
    trusts(C,B,NewD).

%%% trustable relations declared by appOp
(0.9,0.9)::trustOpinion(appOp, cloudProvider).
(0.99,0.9)::trustOpinion(appOp, telco).

%%% trustable relations declared by telco
(0.5,0.9)::trustOpinion(telco, privateCitizen1).
(0.9,0.9)::trustOpinion(telco, university).

%%% trustable relations declared by university
(0.8,0.85)::trustOpinion(university, privateCitizen1).
(0.9,0.9)::trustOpinion(university, openS).

%%% trustable relations declared by private1
(0.6,1)::trustOpinion(privateCitizen1, privateCitizen2).

%%% trustable relations declared by private2
(0.8,1)::trustOpinion(privateCitizen2, openS).
(0.8,0.9)::trustOpinion(privateCitizen2, cloudProvider).

%%% trustable relations declared by cloudProvider
(0.9,0.9)::trustOpinion(cloudProvider, telco).
(0.89,0.9)::trustOpinion(cloudProvider, openS).
