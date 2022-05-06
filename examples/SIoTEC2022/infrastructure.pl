%% AR GATHERING INFRASTRUCTURE (info provided by node provider(s) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% node(nodeId, nodeType, providerId, listOfSupportedSecurityProperties,listOFSoftwareCapabilities, listOFHWcapabilities (memory, cpu, mhz), Price)
%privateCitzen
node(private1, privatenode, privateCitizen1, [], [js], (1024,4,2500)).
node(private2, privatenode, privateCitizen2, [pubKeyE], [py3], (512,2,1500)).
%telco
node(ispRouter, telconode,telco, [pubKeyE, antiTamp], [js,py3],(3500, 16, 2000)).
node(antenna1, telconode, telco,[pubKeyE, antiTamp], [js,py3],(2048, 3, 1500)).
node(antenna2, telconode, telco,[pubKeyE], [py3,numPy],(2048, 4, 1500)).
%university
node(labServer, uninode,university, [pubKeyE, antiTamp], [py3,numPy],(4096, 4, 2000)).
node(officeServer, uninide, university, [], [py3],(1024, 2, 1000)).
node(switch, uninode,university, [pubKeyE], [py3,js],(2048, 2, 2000)).
%cloudProvider
node(cloudNode, cloudnode,cloudProvider, [pubKeyE,antiTamp], [js,py3,numPy], (inf, inf, inf)).


%eventGenerator(generatorId, eventList, SourceNodes)
eventGenerator(userDevice1, [event01,event02],ispRouter).
eventGenerator(userDevice2, [event02],antenna1).
%trigger(generatorId,eventId)


%service(serviceId, serviceProvider, serviceType, deployedNode)
service(myUserDb, appOp, userDB, ispRouter).
service(cMaps, cloudProvider, maps, cloudNode).
service(openM, openS, maps, private1).
%service(myShop, appOp, shops,switch).
service(gp, pa, checkGp, ispRouter).
service(rules, pa, checkRules, antenna1).
service(bucket01, cloudProvider, bucket, antenna1).
%service(publicGatherService, pub_amm, gatherService, antenna2).

%link(node1, node2, latencyInMs)
link(X,X,0).
link(X,Y,L) :- dif(X,Y), (latency(X,Y,L);latency(Y,X,L)).
%cloudNode
latency(cloudNode, ispRouter, 30).
latency(cloudNode, antenna1, 38).
latency(cloudNode, antenna2, 37).
latency(cloudNode, private1, 36).
latency(cloudNode, private2, 45).
latency(cloudNode, switch, 35).
latency(cloudNode, officeServer, 42).
latency(cloudNode, labServer, 40).
%ispRouter
latency(ispRouter, antenna1, 8).
latency(ispRouter, antenna2, 7).
latency(ispRouter, private1, 6).
latency(ispRouter, private2, 15).
latency(ispRouter, switch, 5).
latency(ispRouter, officeServer, 12).
latency(ispRouter, labServer, 10).
%antenna1
latency(antenna1, antenna2, 12).
latency(antenna1, private1, 14).
latency(antenna1, private2, 7).
latency(antenna1, switch, 13).
latency(antenna1, officeServer, 20).
latency(antenna1, labServer, 18).
%antenna2
latency(antenna2, private1, 13).
latency(antenna2, private2, 19).
latency(antenna2, switch, 12).
latency(antenna2, officeServer, 19).
latency(antenna2, labServer, 20).
%private1
latency(private1, private2, 21).
latency(private1, switch, 11).
latency(private1, officeServer, 18).
latency(private1, labServer, 16).
%private2
latency(private2, switch, 20).
latency(private2, officeServer, 27).
latency(private2, labServer, 25).
%switch
latency(switch, officeServer, 7).
latency(switch, labServer, 5).
%officeServer
latency(officeServer, labServer, 7).