%% INFRASTRUCTURE (info provided by node provider(s) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% node(nodeId, providerId, listOfSupportedSecurityProperties,listOFSoftwareCapabilities, listOFHWcapabilities (memory, cpu, mhz))
%provider1
node(edge1, provider1, [data_encryption], [python,javascript,java], (1024,2,2000), [([0,999],10,20,35),([1000, inf],1,1,1)]).
node(edge2, provider1, [data_encryption, antitampering],    [python, java], (2048,2, 4000), [([0,999],1,2,3), ([1000, inf],10,20,30)] ).
%provider2
%node(edge3, provider2, [antitampering],                     [javascript, java]).
%node(edge4, provider2, [data_encryption,antitampering],     [python,java]).
%provider3
node(edge5, provider3, [data_encryption,antitampering],     [java], (4096, 4, 3000), [([0,999],10,20,35), ([1000, inf],1,1,1)] ).
node(edge6, provider3, [wireless_security,antitampering],   [java],(8192,8, 4000), [([0,999],10,20,35), ([1000, inf],1,1,1)] ).

%link(node1, node2, latencyInMs)
%e2--50--e1------100------e5--75--e6
link(X,X,0).
link(X,Y,L) :- dif(X,Y), g_link(Y,X,L).
g_link(edge1, edge2, 50).
g_link(edge1, edge5, 100).
g_link(edge1, edge6, 175).
g_link(edge2, edge5, 150).
g_link(edge2, edge6, 225).
g_link(edge5, edge6, 75).
%eventGenerator(generatorId, sourceType, listOfEvents)
eventGenerator(aws_bucket_5678, record_read).


%service(serviceId, serviceProvider, serviceType, deployedNode)
service(dbAws1, aws_EU, amazonDB, edge2).
service(dbAws2, aws_EU, amazonDB, edge5).
service(dbPoste1, poste_FR, posteAuth, edge2).
service(dbPoste2, poste_ITA, posteAuth, edge5).