%% APPLICATION (defined by operator) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% function(functionId, listOfSWReqs, HWReqs(memory, vCPU, Htz), timeout, listOfServiceReqs(serviceType, latency))
function(f1, [java],(512, 1, 500), 45,  [(amazonDB, 350), (posteAuth, 350)]).
function(f2, [python], (256, 2, 550), _,[]).

%functionBehaviour(functionId, listOfInputs, listOf(serviceReq, TypeParam), listOfOutputs)
%f1
functionBehaviour(f1, [X,Y,Z],[Z,top], [W]) :- maxType(X,Y,W).
%f2
functionBehaviour(f2, [X],[],[X]).

%functionChain(functionChainId, operatorId, triggeringEvent(eventSource, eventType, inputParameters,
%               listOfFunctions(functionId(listOfServiceInstances)),
%               listOfIntraFunctionLatencies).
functionChain(
  c1, appOp,(aws_bucket_5678, record_read, [top,medium,top]),
  [(f1,[dbAws1, X]),(f2,[])],
  [350]
).

% lattice of security types
g_lattice_higherThan(top, high).
g_lattice_higherThan(high, medium).
g_lattice_higherThan(medium, low).


% node labeling
assignNodeLabel(NodeId, top)    :- node(NodeId,_,SecCaps,_,_,_), member(antitampering, SecCaps), member(data_encryption, SecCaps).
assignNodeLabel(NodeId, medium) :- node(NodeId,_,SecCaps,_,_,_), \+(member(antitampering, SecCaps)), member(data_encryption, SecCaps).
assignNodeLabel(NodeId, low)    :- node(NodeId,_,SecCaps,_,_,_), \+(member(data_encryption, SecCaps)).

%service labeling
assignServiceLabel(SId, amazonDB, top) :- service(SId, aws_EU, amazonDB, _).
assignServiceLabel(SId, amazonDB, medium) :- service(SId, aws_US, amazonDB, _).
assignServiceLabel(SId, amazonDB, low) :- service(SId, ServiceProvider, amazonDB, _), \+ (ServiceProvider = aws_EU, ServiceProvider = aws_US).
assignServiceLabel(SId, posteAuth, top) :- service(SId, poste_ITA, posteAuth, _).
assignServiceLabel(SId, posteAuth, low) :- service(SId, ServiceProvider, posteAuth, _), \+(ServiceProvider = poste_ITA).