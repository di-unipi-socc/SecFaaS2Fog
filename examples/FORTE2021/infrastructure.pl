%% AR GATHERING INFRASTRUCTURE (info provided by node provider(s) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% node(nodeId, providerId, listOfSupportedSecurityProperties,listOFSoftwareCapabilities, listOFHWcapabilities (memory, cpu, mhz), Price)
%privateCitzen
node(private1, privateCitizen1, [], [js], (1024,4,2500)).
node(private2, privateCitizen2, [pubKeyE], [py3], (512,2,1500)).
%telco
node(ispRouter, telco, [pubKeyE, antiTamp], [py3,js,numPy],(40960, 16, 2000)).
node(northAntenna, telco, [pubKeyE, antiTamp], [js],(2048, 2, 1500)).
node(southAntenna, telco, [pubKeyE], [py3,numPy],(2048, 4, 1500)).
%universityversity
node(labServer, university, [pubKeyE, antiTamp], [py3,js,numPy],(4096, 4, 2000)).
node(officeServer, university, [], [py3],(1024, 2, 1000)).
node(switch, university, [pubKeyE], [py3,js],(2048, 2, 2000)).
%cloudProvider
node(cloudNode, cloudProvider, [pubKeyE,antiTamp], [py3,js,numPy], (inf, inf, inf)).


%eventGenerator(generatorId, eventType, SourceNodes)
eventGenerator(userDevice, ispRouter).
eventGenerator(userDevice2, ispRouter).

%service(serviceId, serviceProvider, serviceType, deployedNode)
service(myUsers, appOp, userDB, ispRouter).
service(cMaps, cloudProvider, maps, cloudNode).
service(openM, openS, maps, private1).
service(myShop, appOp, shops,switch).
%service(publicGatherService, pub_amm, gatherService, southAntenna).

%link(node1, node2, latencyInMs)
link(X,X,0).
link(X,Y,L) :- dif(X,Y), (g_link(X,Y,L);g_link(Y,X,L)).
%cloudNode
g_link(cloudNode, ispRouter, 30).
g_link(cloudNode, northAntenna, 38).
g_link(cloudNode, southAntenna, 37).
g_link(cloudNode, private1, 36).
g_link(cloudNode, private2, 45).
g_link(cloudNode, switch, 35).
g_link(cloudNode, officeServer, 42).
g_link(cloudNode, labServer, 40).
%ispRouter
g_link(ispRouter, northAntenna, 8).
g_link(ispRouter, southAntenna, 7).
g_link(ispRouter, private1, 6).
g_link(ispRouter, private2, 15).
g_link(ispRouter, switch, 5).
g_link(ispRouter, officeServer, 12).
g_link(ispRouter, labServer, 10).
%northAntenna
g_link(northAntenna, southAntenna, 12).
g_link(northAntenna, private1, 14).
g_link(northAntenna, private2, 7).
g_link(northAntenna, switch, 13).
g_link(northAntenna, officeServer, 20).
g_link(northAntenna, labServer, 18).
%southAntenna
g_link(southAntenna, private1, 13).
g_link(southAntenna, private2, 19).
g_link(southAntenna, switch, 12).
g_link(southAntenna, officeServer, 19).
g_link(southAntenna, labServer, 20).
%private1
g_link(private1, private2, 21).
g_link(private1, switch, 11).
g_link(private1, officeServer, 18).
g_link(private1, labServer, 16).
%private2
g_link(private2, switch, 20).
g_link(private2, officeServer, 27).
g_link(private2, labServer, 25).
%switch
g_link(switch, officeServer, 7).
g_link(switch, labServer, 5).
%officeServer
g_link(officeServer, labServer, 7).