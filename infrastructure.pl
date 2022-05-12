node(cloud1, cloud, cloudProvider, [pubKeyE, antiTamp], [js, py3, numPy], (inf, inf, inf)).
node(cloud2, cloud, cloudProvider, [pubKeyE, antiTamp], [js, py3, numPy], (inf, inf, inf)).
node(cloud3, cloud, cloudProvider, [pubKeyE, antiTamp], [js, py3, numPy], (inf, inf, inf)).
node(cloud4, cloud, cloudProvider, [pubKeyE, antiTamp], [js, py3, numPy], (inf, inf, inf)).
node(cloud5, cloud, cloudProvider, [pubKeyE, antiTamp], [js, py3, numPy], (inf, inf, inf)).
node(fog1, fog, telco, [pubKeyE, antiTamp], [js, py3], (3500, 16, 2000)).
node(fog2, fog, university, [pubKeyE, antiTamp], [py3, numPy], (4096, 4, 2000)).
node(fog3, fog, telco, [pubKeyE, antiTamp], [js, py3], (3500, 16, 2000)).
node(fog4, fog, telco, [pubKeyE, antiTamp], [js, py3], (3500, 16, 2000)).
node(fog5, fog, telco, [pubKeyE, antiTamp], [js, py3], (3500, 16, 2000)).
node(fog6, fog, telco, [pubKeyE], [py3, numPy], (2048, 4, 1500)).
node(fog7, fog, telco, [pubKeyE, antiTamp], [js, py3], (3500, 16, 2000)).
node(fog8, fog, university, [pubKeyE, antiTamp], [py3, numPy], (4096, 4, 2000)).
node(fog9, fog, telco, [pubKeyE], [py3, numPy], (2048, 4, 1500)).
node(fog10, fog, university, [pubKeyE, antiTamp], [py3, numPy], (4096, 4, 2000)).
node(fog11, fog, telco, [pubKeyE, antiTamp], [js, py3], (2048, 4, 1500)).
node(fog12, fog, telco, [pubKeyE], [py3, numPy], (2048, 4, 1500)).
node(fog13, fog, telco, [pubKeyE], [py3, numPy], (2048, 4, 1500)).
node(fog14, fog, telco, [pubKeyE, antiTamp], [js, py3], (3500, 16, 2000)).
node(fog15, fog, university, [], [py3], (1024, 2, 1000)).
node(fog16, fog, university, [], [py3], (1024, 2, 1000)).
node(fog17, fog, telco, [pubKeyE, antiTamp], [js, py3], (3500, 16, 2000)).
node(fog18, fog, telco, [pubKeyE, antiTamp], [js, py3], (3500, 16, 2000)).
node(fog19, fog, telco, [pubKeyE, antiTamp], [js, py3], (2048, 4, 1500)).
node(fog20, fog, telco, [pubKeyE, antiTamp], [js, py3], (3500, 16, 2000)).
node(fog21, fog, telco, [pubKeyE, antiTamp], [js, py3], (3500, 16, 2000)).
node(fog22, fog, university, [pubKeyE, antiTamp], [py3, numPy], (4096, 4, 2000)).
node(fog23, fog, telco, [pubKeyE, antiTamp], [js, py3], (3500, 16, 2000)).
node(fog24, fog, university, [pubKeyE, antiTamp], [py3, numPy], (4096, 4, 2000)).
node(fog25, fog, university, [], [py3], (1024, 2, 1000)).
node(edge1, edge, privateCitizen2, [pubKeyE], [py3], (512, 2, 1500)).
node(edge2, edge, privateCitizen1, [], [js], (1024, 4, 2500)).
node(edge3, edge, privateCitizen1, [], [js], (1024, 4, 2500)).
node(edge4, edge, privateCitizen2, [pubKeyE], [py3], (512, 2, 1500)).
node(edge5, edge, privateCitizen2, [pubKeyE], [py3], (512, 2, 1500)).
node(edge6, edge, privateCitizen1, [], [js], (1024, 4, 2500)).
node(edge7, edge, privateCitizen1, [], [js], (1024, 4, 2500)).
node(edge8, edge, privateCitizen2, [pubKeyE], [py3], (512, 2, 1500)).
node(edge9, edge, privateCitizen1, [], [js], (1024, 4, 2500)).
eventGenerator(device0, [event5, event4, event6, event1, event2, event3, event7], fog4).
eventGenerator(device1, [event1, event3, event7, event4, event5, event6, event2], fog4).
eventGenerator(device2, [event5, event1, event2, event7, event4, event3, event6], fog4).
eventGenerator(device3, [event5, event3, event1, event4, event2, event7, event6], fog4).
eventGenerator(device4, [event1, event5, event7, event3, event4, event2, event6], fog4).
eventGenerator(device5, [event2, event4, event1, event5, event3, event6, event7], fog4).
eventGenerator(device6, [event6, event7, event1, event3, event2, event4, event5], fog4).
eventGenerator(device7, [event7, event5, event6, event1, event4, event2, event3], fog4).
service(cMaps1, cloudProvider, maps, cloud1).
service(bucket2, cloudProvider, bucket, cloud1).
service(cMaps3, cloudProvider, maps, cloud2).
service(bucket4, cloudProvider, bucket, cloud2).
service(cMaps5, cloudProvider, maps, cloud3).
service(bucket6, cloudProvider, bucket, cloud3).
service(cMaps7, cloudProvider, maps, cloud4).
service(bucket8, cloudProvider, bucket, cloud4).
service(cMaps9, cloudProvider, maps, cloud5).
service(bucket10, cloudProvider, bucket, cloud5).
service(myUserDb11, appOp, userDB, fog1).
service(gp12, pa, checkGp, fog1).
service(rules13, pa, checkRules, fog1).
service(myUserDb14, appOp, userDB, fog2).
service(gp15, pa, checkGp, fog2).
service(rules16, pa, checkRules, fog2).
service(myUserDb17, appOp, userDB, fog3).
service(gp18, pa, checkGp, fog3).
service(rules19, pa, checkRules, fog3).
service(myUserDb20, appOp, userDB, fog4).
service(gp21, pa, checkGp, fog4).
service(rules22, pa, checkRules, fog4).
service(myUserDb23, appOp, userDB, fog5).
service(gp24, pa, checkGp, fog5).
service(rules25, pa, checkRules, fog5).
service(myUserDb26, appOp, userDB, fog6).
service(gp27, pa, checkGp, fog6).
service(rules28, pa, checkRules, fog6).
service(myUserDb29, appOp, userDB, fog7).
service(gp30, pa, checkGp, fog7).
service(rules31, pa, checkRules, fog7).
service(myUserDb32, appOp, userDB, fog8).
service(gp33, pa, checkGp, fog8).
service(rules34, pa, checkRules, fog8).
service(myUserDb35, appOp, userDB, fog9).
service(gp36, pa, checkGp, fog9).
service(rules37, pa, checkRules, fog9).
service(myUserDb38, appOp, userDB, fog10).
service(gp39, pa, checkGp, fog10).
service(rules40, pa, checkRules, fog10).
service(myUserDb41, appOp, userDB, fog11).
service(gp42, pa, checkGp, fog11).
service(rules43, pa, checkRules, fog11).
service(myUserDb44, appOp, userDB, fog12).
service(gp45, pa, checkGp, fog12).
service(rules46, pa, checkRules, fog12).
service(myUserDb47, appOp, userDB, fog13).
service(gp48, pa, checkGp, fog13).
service(rules49, pa, checkRules, fog13).
service(myUserDb50, appOp, userDB, fog14).
service(gp51, pa, checkGp, fog14).
service(rules52, pa, checkRules, fog14).
service(myUserDb53, appOp, userDB, fog15).
service(gp54, pa, checkGp, fog15).
service(rules55, pa, checkRules, fog15).
service(myUserDb56, appOp, userDB, fog16).
service(gp57, pa, checkGp, fog16).
service(rules58, pa, checkRules, fog16).
service(myUserDb59, appOp, userDB, fog17).
service(gp60, pa, checkGp, fog17).
service(rules61, pa, checkRules, fog17).
service(myUserDb62, appOp, userDB, fog18).
service(gp63, pa, checkGp, fog18).
service(rules64, pa, checkRules, fog18).
service(myUserDb65, appOp, userDB, fog19).
service(gp66, pa, checkGp, fog19).
service(rules67, pa, checkRules, fog19).
service(myUserDb68, appOp, userDB, fog20).
service(gp69, pa, checkGp, fog20).
service(rules70, pa, checkRules, fog20).
service(myUserDb71, appOp, userDB, fog21).
service(gp72, pa, checkGp, fog21).
service(rules73, pa, checkRules, fog21).
service(myUserDb74, appOp, userDB, fog22).
service(gp75, pa, checkGp, fog22).
service(rules76, pa, checkRules, fog22).
service(myUserDb77, appOp, userDB, fog23).
service(gp78, pa, checkGp, fog23).
service(rules79, pa, checkRules, fog23).
service(myUserDb80, appOp, userDB, fog24).
service(gp81, pa, checkGp, fog24).
service(rules82, pa, checkRules, fog24).
service(myUserDb83, appOp, userDB, fog25).
service(gp84, pa, checkGp, fog25).
service(rules85, pa, checkRules, fog25).
service(openM86, openS, maps, edge1).
service(openM87, openS, maps, edge2).
service(openM88, openS, maps, edge3).
service(openM89, openS, maps, edge4).
service(openM90, openS, maps, edge5).
service(openM91, openS, maps, edge6).
service(openM92, openS, maps, edge7).
service(openM93, openS, maps, edge8).
service(openM94, openS, maps, edge9).
service(openM95, openS, maps, edge10).
service(myUserDb1, appOp, userDB, fog1).
link(X,X,0).
link(X,Y,L) :- dif(X,Y), (latency(X,Y,L);latency(Y,X,L)).
latency(cloud1, cloud2, 1).
latency(cloud1, cloud3, 1).
latency(cloud1, cloud4, 1).
latency(cloud1, cloud5, 1).
latency(cloud1, fog2, 1).
latency(cloud1, fog4, 1).
latency(cloud1, fog5, 1).
latency(cloud1, fog6, 1).
latency(cloud1, fog7, 1).
latency(cloud1, fog8, 1).
latency(cloud1, fog9, 1).
latency(cloud1, fog10, 1).
latency(cloud1, fog12, 1).
latency(cloud1, fog13, 1).
latency(cloud1, fog17, 1).
latency(cloud1, fog18, 1).
latency(cloud1, fog19, 1).
latency(cloud1, fog20, 1).
latency(cloud1, fog21, 1).
latency(cloud1, fog23, 1).
latency(cloud1, fog25, 1).
latency(cloud1, edge2, 1).
latency(cloud2, cloud3, 1).
latency(cloud2, cloud4, 1).
latency(cloud2, cloud5, 1).
latency(cloud2, fog1, 1).
latency(cloud2, fog2, 1).
latency(cloud2, fog3, 1).
latency(cloud2, fog6, 1).
latency(cloud2, fog7, 1).
latency(cloud2, fog8, 1).
latency(cloud2, fog9, 1).
latency(cloud2, fog10, 1).
latency(cloud2, fog13, 1).
latency(cloud2, fog14, 1).
latency(cloud2, fog15, 1).
latency(cloud2, fog16, 1).
latency(cloud2, fog17, 1).
latency(cloud2, fog18, 1).
latency(cloud2, fog20, 1).
latency(cloud2, fog23, 1).
latency(cloud2, fog24, 1).
latency(cloud2, fog25, 1).
latency(cloud2, edge4, 1).
latency(cloud2, edge9, 1).
latency(cloud3, cloud4, 1).
latency(cloud3, cloud5, 1).
latency(cloud3, fog1, 1).
latency(cloud3, fog2, 1).
latency(cloud3, fog3, 1).
latency(cloud3, fog6, 1).
latency(cloud3, fog7, 1).
latency(cloud3, fog8, 1).
latency(cloud3, fog10, 1).
latency(cloud3, fog11, 1).
latency(cloud3, fog12, 1).
latency(cloud3, fog14, 1).
latency(cloud3, fog15, 1).
latency(cloud3, fog17, 1).
latency(cloud3, fog18, 1).
latency(cloud3, fog19, 1).
latency(cloud3, fog20, 1).
latency(cloud3, fog21, 1).
latency(cloud3, fog22, 1).
latency(cloud3, fog25, 1).
latency(cloud3, edge3, 1).
latency(cloud4, cloud5, 1).
latency(cloud4, fog1, 1).
latency(cloud4, fog2, 1).
latency(cloud4, fog3, 1).
latency(cloud4, fog5, 1).
latency(cloud4, fog6, 1).
latency(cloud4, fog10, 1).
latency(cloud4, fog12, 1).
latency(cloud4, fog13, 1).
latency(cloud4, fog16, 1).
latency(cloud4, fog20, 1).
latency(cloud4, fog22, 1).
latency(cloud4, fog23, 1).
latency(cloud4, fog24, 1).
latency(cloud4, fog25, 1).
latency(cloud5, fog1, 1).
latency(cloud5, fog2, 1).
latency(cloud5, fog3, 1).
latency(cloud5, fog4, 1).
latency(cloud5, fog5, 1).
latency(cloud5, fog6, 1).
latency(cloud5, fog7, 1).
latency(cloud5, fog9, 1).
latency(cloud5, fog10, 1).
latency(cloud5, fog11, 1).
latency(cloud5, fog12, 1).
latency(cloud5, fog13, 1).
latency(cloud5, fog15, 1).
latency(cloud5, fog16, 1).
latency(cloud5, fog17, 1).
latency(cloud5, fog19, 1).
latency(cloud5, fog21, 1).
latency(cloud5, fog22, 1).
latency(cloud5, fog25, 1).
latency(cloud5, edge3, 1).
latency(fog1, fog2, 1).
latency(fog1, fog3, 1).
latency(fog1, fog4, 1).
latency(fog1, fog5, 1).
latency(fog1, fog6, 1).
latency(fog1, fog7, 1).
latency(fog1, fog8, 1).
latency(fog1, fog9, 1).
latency(fog1, fog10, 1).
latency(fog1, fog11, 1).
latency(fog1, fog12, 1).
latency(fog1, fog13, 1).
latency(fog1, fog14, 1).
latency(fog1, fog15, 1).
latency(fog1, fog16, 1).
latency(fog1, fog17, 1).
latency(fog1, fog18, 1).
latency(fog1, fog19, 1).
latency(fog1, fog20, 1).
latency(fog1, fog21, 1).
latency(fog1, fog22, 1).
latency(fog1, fog23, 1).
latency(fog1, fog24, 1).
latency(fog1, fog25, 1).
latency(fog1, edge1, 1).
latency(fog1, edge2, 1).
latency(fog1, edge4, 1).
latency(fog1, edge6, 1).
latency(fog1, edge7, 1).
latency(fog1, edge8, 1).
latency(fog1, edge9, 1).
latency(fog2, fog3, 1).
latency(fog2, fog4, 1).
latency(fog2, fog5, 1).
latency(fog2, fog6, 1).
latency(fog2, fog7, 1).
latency(fog2, fog8, 1).
latency(fog2, fog9, 1).
latency(fog2, fog10, 1).
latency(fog2, fog11, 1).
latency(fog2, fog12, 1).
latency(fog2, fog13, 1).
latency(fog2, fog14, 1).
latency(fog2, fog15, 1).
latency(fog2, fog16, 1).
latency(fog2, fog17, 1).
latency(fog2, fog18, 1).
latency(fog2, fog19, 1).
latency(fog2, fog20, 1).
latency(fog2, fog21, 1).
latency(fog2, fog22, 1).
latency(fog2, fog23, 1).
latency(fog2, fog24, 1).
latency(fog2, fog25, 1).
latency(fog2, edge1, 1).
latency(fog2, edge2, 1).
latency(fog2, edge3, 1).
latency(fog2, edge5, 1).
latency(fog2, edge7, 1).
latency(fog2, edge8, 1).
latency(fog2, edge9, 1).
latency(fog3, fog4, 1).
latency(fog3, fog5, 1).
latency(fog3, fog6, 1).
latency(fog3, fog7, 1).
latency(fog3, fog8, 1).
latency(fog3, fog9, 1).
latency(fog3, fog10, 1).
latency(fog3, fog11, 1).
latency(fog3, fog12, 1).
latency(fog3, fog13, 1).
latency(fog3, fog14, 1).
latency(fog3, fog15, 1).
latency(fog3, fog16, 1).
latency(fog3, fog17, 1).
latency(fog3, fog18, 1).
latency(fog3, fog19, 1).
latency(fog3, fog20, 1).
latency(fog3, fog21, 1).
latency(fog3, fog22, 1).
latency(fog3, fog23, 1).
latency(fog3, fog24, 1).
latency(fog3, fog25, 1).
latency(fog3, edge1, 1).
latency(fog3, edge2, 1).
latency(fog3, edge3, 1).
latency(fog3, edge4, 1).
latency(fog3, edge5, 1).
latency(fog3, edge6, 1).
latency(fog3, edge7, 1).
latency(fog3, edge8, 1).
latency(fog3, edge9, 1).
latency(fog4, fog5, 1).
latency(fog4, fog6, 1).
latency(fog4, fog7, 1).
latency(fog4, fog8, 1).
latency(fog4, fog9, 1).
latency(fog4, fog10, 1).
latency(fog4, fog11, 1).
latency(fog4, fog12, 1).
latency(fog4, fog13, 1).
latency(fog4, fog14, 1).
latency(fog4, fog15, 1).
latency(fog4, fog16, 1).
latency(fog4, fog17, 1).
latency(fog4, fog18, 1).
latency(fog4, fog19, 1).
latency(fog4, fog20, 1).
latency(fog4, fog21, 1).
latency(fog4, fog22, 1).
latency(fog4, fog23, 1).
latency(fog4, fog24, 1).
latency(fog4, fog25, 1).
latency(fog4, edge1, 1).
latency(fog4, edge2, 1).
latency(fog4, edge3, 1).
latency(fog4, edge4, 1).
latency(fog4, edge5, 1).
latency(fog4, edge6, 1).
latency(fog4, edge8, 1).
latency(fog4, edge9, 1).
latency(fog5, fog6, 1).
latency(fog5, fog7, 1).
latency(fog5, fog8, 1).
latency(fog5, fog9, 1).
latency(fog5, fog10, 1).
latency(fog5, fog11, 1).
latency(fog5, fog12, 1).
latency(fog5, fog13, 1).
latency(fog5, fog14, 1).
latency(fog5, fog15, 1).
latency(fog5, fog16, 1).
latency(fog5, fog17, 1).
latency(fog5, fog18, 1).
latency(fog5, fog19, 1).
latency(fog5, fog20, 1).
latency(fog5, fog21, 1).
latency(fog5, fog22, 1).
latency(fog5, fog23, 1).
latency(fog5, fog24, 1).
latency(fog5, fog25, 1).
latency(fog5, edge1, 1).
latency(fog5, edge3, 1).
latency(fog5, edge4, 1).
latency(fog5, edge5, 1).
latency(fog5, edge7, 1).
latency(fog5, edge8, 1).
latency(fog5, edge9, 1).
latency(fog6, fog7, 1).
latency(fog6, fog8, 1).
latency(fog6, fog9, 1).
latency(fog6, fog10, 1).
latency(fog6, fog11, 1).
latency(fog6, fog12, 1).
latency(fog6, fog13, 1).
latency(fog6, fog14, 1).
latency(fog6, fog15, 1).
latency(fog6, fog16, 1).
latency(fog6, fog17, 1).
latency(fog6, fog18, 1).
latency(fog6, fog19, 1).
latency(fog6, fog20, 1).
latency(fog6, fog21, 1).
latency(fog6, fog22, 1).
latency(fog6, fog23, 1).
latency(fog6, fog24, 1).
latency(fog6, fog25, 1).
latency(fog6, edge1, 1).
latency(fog6, edge2, 1).
latency(fog6, edge3, 1).
latency(fog6, edge4, 1).
latency(fog6, edge5, 1).
latency(fog6, edge6, 1).
latency(fog6, edge7, 1).
latency(fog6, edge8, 1).
latency(fog6, edge9, 1).
latency(fog7, fog8, 1).
latency(fog7, fog9, 1).
latency(fog7, fog10, 1).
latency(fog7, fog11, 1).
latency(fog7, fog12, 1).
latency(fog7, fog13, 1).
latency(fog7, fog14, 1).
latency(fog7, fog15, 1).
latency(fog7, fog16, 1).
latency(fog7, fog17, 1).
latency(fog7, fog18, 1).
latency(fog7, fog19, 1).
latency(fog7, fog20, 1).
latency(fog7, fog21, 1).
latency(fog7, fog22, 1).
latency(fog7, fog23, 1).
latency(fog7, fog24, 1).
latency(fog7, fog25, 1).
latency(fog7, edge1, 1).
latency(fog7, edge2, 1).
latency(fog7, edge3, 1).
latency(fog7, edge4, 1).
latency(fog7, edge6, 1).
latency(fog7, edge7, 1).
latency(fog7, edge8, 1).
latency(fog7, edge9, 1).
latency(fog8, fog9, 1).
latency(fog8, fog10, 1).
latency(fog8, fog11, 1).
latency(fog8, fog12, 1).
latency(fog8, fog13, 1).
latency(fog8, fog14, 1).
latency(fog8, fog15, 1).
latency(fog8, fog16, 1).
latency(fog8, fog17, 1).
latency(fog8, fog18, 1).
latency(fog8, fog19, 1).
latency(fog8, fog20, 1).
latency(fog8, fog21, 1).
latency(fog8, fog22, 1).
latency(fog8, fog23, 1).
latency(fog8, fog24, 1).
latency(fog8, fog25, 1).
latency(fog8, edge1, 1).
latency(fog8, edge2, 1).
latency(fog8, edge3, 1).
latency(fog8, edge4, 1).
latency(fog8, edge5, 1).
latency(fog8, edge6, 1).
latency(fog8, edge7, 1).
latency(fog8, edge8, 1).
latency(fog8, edge9, 1).
latency(fog9, fog10, 1).
latency(fog9, fog11, 1).
latency(fog9, fog12, 1).
latency(fog9, fog13, 1).
latency(fog9, fog14, 1).
latency(fog9, fog15, 1).
latency(fog9, fog16, 1).
latency(fog9, fog17, 1).
latency(fog9, fog18, 1).
latency(fog9, fog19, 1).
latency(fog9, fog20, 1).
latency(fog9, fog21, 1).
latency(fog9, fog22, 1).
latency(fog9, fog23, 1).
latency(fog9, fog24, 1).
latency(fog9, fog25, 1).
latency(fog9, edge1, 1).
latency(fog9, edge2, 1).
latency(fog9, edge3, 1).
latency(fog9, edge4, 1).
latency(fog9, edge5, 1).
latency(fog9, edge6, 1).
latency(fog9, edge7, 1).
latency(fog9, edge8, 1).
latency(fog9, edge9, 1).
latency(fog10, fog11, 1).
latency(fog10, fog12, 1).
latency(fog10, fog13, 1).
latency(fog10, fog14, 1).
latency(fog10, fog15, 1).
latency(fog10, fog16, 1).
latency(fog10, fog17, 1).
latency(fog10, fog18, 1).
latency(fog10, fog19, 1).
latency(fog10, fog20, 1).
latency(fog10, fog21, 1).
latency(fog10, fog22, 1).
latency(fog10, fog23, 1).
latency(fog10, fog24, 1).
latency(fog10, fog25, 1).
latency(fog10, edge1, 1).
latency(fog10, edge2, 1).
latency(fog10, edge3, 1).
latency(fog10, edge4, 1).
latency(fog10, edge6, 1).
latency(fog10, edge7, 1).
latency(fog10, edge8, 1).
latency(fog10, edge9, 1).
latency(fog11, fog12, 1).
latency(fog11, fog13, 1).
latency(fog11, fog14, 1).
latency(fog11, fog15, 1).
latency(fog11, fog16, 1).
latency(fog11, fog17, 1).
latency(fog11, fog18, 1).
latency(fog11, fog19, 1).
latency(fog11, fog20, 1).
latency(fog11, fog21, 1).
latency(fog11, fog22, 1).
latency(fog11, fog23, 1).
latency(fog11, fog24, 1).
latency(fog11, fog25, 1).
latency(fog11, edge1, 1).
latency(fog11, edge2, 1).
latency(fog11, edge3, 1).
latency(fog11, edge4, 1).
latency(fog11, edge5, 1).
latency(fog11, edge6, 1).
latency(fog11, edge7, 1).
latency(fog11, edge8, 1).
latency(fog11, edge9, 1).
latency(fog12, fog13, 1).
latency(fog12, fog14, 1).
latency(fog12, fog15, 1).
latency(fog12, fog16, 1).
latency(fog12, fog17, 1).
latency(fog12, fog18, 1).
latency(fog12, fog19, 1).
latency(fog12, fog20, 1).
latency(fog12, fog21, 1).
latency(fog12, fog22, 1).
latency(fog12, fog23, 1).
latency(fog12, fog24, 1).
latency(fog12, fog25, 1).
latency(fog12, edge1, 1).
latency(fog12, edge2, 1).
latency(fog12, edge3, 1).
latency(fog12, edge4, 1).
latency(fog12, edge6, 1).
latency(fog12, edge7, 1).
latency(fog12, edge8, 1).
latency(fog12, edge9, 1).
latency(fog13, fog14, 1).
latency(fog13, fog15, 1).
latency(fog13, fog16, 1).
latency(fog13, fog17, 1).
latency(fog13, fog18, 1).
latency(fog13, fog19, 1).
latency(fog13, fog20, 1).
latency(fog13, fog21, 1).
latency(fog13, fog22, 1).
latency(fog13, fog23, 1).
latency(fog13, fog24, 1).
latency(fog13, fog25, 1).
latency(fog13, edge1, 1).
latency(fog13, edge2, 1).
latency(fog13, edge3, 1).
latency(fog13, edge4, 1).
latency(fog13, edge5, 1).
latency(fog13, edge6, 1).
latency(fog13, edge7, 1).
latency(fog13, edge8, 1).
latency(fog13, edge9, 1).
latency(fog14, fog15, 1).
latency(fog14, fog16, 1).
latency(fog14, fog17, 1).
latency(fog14, fog18, 1).
latency(fog14, fog19, 1).
latency(fog14, fog20, 1).
latency(fog14, fog21, 1).
latency(fog14, fog22, 1).
latency(fog14, fog23, 1).
latency(fog14, fog24, 1).
latency(fog14, fog25, 1).
latency(fog14, edge1, 1).
latency(fog14, edge2, 1).
latency(fog14, edge3, 1).
latency(fog14, edge4, 1).
latency(fog14, edge5, 1).
latency(fog14, edge6, 1).
latency(fog14, edge8, 1).
latency(fog14, edge9, 1).
latency(fog15, fog16, 1).
latency(fog15, fog17, 1).
latency(fog15, fog18, 1).
latency(fog15, fog19, 1).
latency(fog15, fog20, 1).
latency(fog15, fog21, 1).
latency(fog15, fog22, 1).
latency(fog15, fog23, 1).
latency(fog15, fog24, 1).
latency(fog15, fog25, 1).
latency(fog15, edge1, 1).
latency(fog15, edge2, 1).
latency(fog15, edge3, 1).
latency(fog15, edge4, 1).
latency(fog15, edge5, 1).
latency(fog15, edge6, 1).
latency(fog15, edge7, 1).
latency(fog15, edge8, 1).
latency(fog15, edge9, 1).
latency(fog16, fog17, 1).
latency(fog16, fog18, 1).
latency(fog16, fog19, 1).
latency(fog16, fog20, 1).
latency(fog16, fog21, 1).
latency(fog16, fog22, 1).
latency(fog16, fog23, 1).
latency(fog16, fog24, 1).
latency(fog16, fog25, 1).
latency(fog16, edge1, 1).
latency(fog16, edge2, 1).
latency(fog16, edge3, 1).
latency(fog16, edge4, 1).
latency(fog16, edge5, 1).
latency(fog16, edge6, 1).
latency(fog16, edge7, 1).
latency(fog16, edge8, 1).
latency(fog16, edge9, 1).
latency(fog17, fog18, 1).
latency(fog17, fog19, 1).
latency(fog17, fog20, 1).
latency(fog17, fog21, 1).
latency(fog17, fog22, 1).
latency(fog17, fog23, 1).
latency(fog17, fog24, 1).
latency(fog17, fog25, 1).
latency(fog17, edge1, 1).
latency(fog17, edge2, 1).
latency(fog17, edge3, 1).
latency(fog17, edge5, 1).
latency(fog17, edge6, 1).
latency(fog17, edge7, 1).
latency(fog17, edge8, 1).
latency(fog17, edge9, 1).
latency(fog18, fog19, 1).
latency(fog18, fog20, 1).
latency(fog18, fog21, 1).
latency(fog18, fog22, 1).
latency(fog18, fog23, 1).
latency(fog18, fog24, 1).
latency(fog18, fog25, 1).
latency(fog18, edge1, 1).
latency(fog18, edge2, 1).
latency(fog18, edge3, 1).
latency(fog18, edge4, 1).
latency(fog18, edge5, 1).
latency(fog18, edge6, 1).
latency(fog18, edge7, 1).
latency(fog18, edge8, 1).
latency(fog19, fog20, 1).
latency(fog19, fog21, 1).
latency(fog19, fog22, 1).
latency(fog19, fog23, 1).
latency(fog19, fog24, 1).
latency(fog19, fog25, 1).
latency(fog19, edge1, 1).
latency(fog19, edge2, 1).
latency(fog19, edge3, 1).
latency(fog19, edge4, 1).
latency(fog19, edge5, 1).
latency(fog19, edge6, 1).
latency(fog19, edge8, 1).
latency(fog19, edge9, 1).
latency(fog20, fog21, 1).
latency(fog20, fog22, 1).
latency(fog20, fog23, 1).
latency(fog20, fog24, 1).
latency(fog20, fog25, 1).
latency(fog20, edge1, 1).
latency(fog20, edge2, 1).
latency(fog20, edge3, 1).
latency(fog20, edge4, 1).
latency(fog20, edge5, 1).
latency(fog20, edge6, 1).
latency(fog20, edge7, 1).
latency(fog20, edge8, 1).
latency(fog20, edge9, 1).
latency(fog21, fog22, 1).
latency(fog21, fog23, 1).
latency(fog21, fog24, 1).
latency(fog21, fog25, 1).
latency(fog21, edge1, 1).
latency(fog21, edge2, 1).
latency(fog21, edge3, 1).
latency(fog21, edge4, 1).
latency(fog21, edge5, 1).
latency(fog21, edge6, 1).
latency(fog21, edge7, 1).
latency(fog21, edge8, 1).
latency(fog21, edge9, 1).
latency(fog22, fog23, 1).
latency(fog22, fog24, 1).
latency(fog22, fog25, 1).
latency(fog22, edge1, 1).
latency(fog22, edge2, 1).
latency(fog22, edge3, 1).
latency(fog22, edge4, 1).
latency(fog22, edge5, 1).
latency(fog22, edge6, 1).
latency(fog22, edge7, 1).
latency(fog22, edge8, 1).
latency(fog22, edge9, 1).
latency(fog23, fog24, 1).
latency(fog23, fog25, 1).
latency(fog23, edge1, 1).
latency(fog23, edge2, 1).
latency(fog23, edge3, 1).
latency(fog23, edge4, 1).
latency(fog23, edge5, 1).
latency(fog23, edge6, 1).
latency(fog23, edge7, 1).
latency(fog23, edge8, 1).
latency(fog23, edge9, 1).
latency(fog24, fog25, 1).
latency(fog24, edge1, 1).
latency(fog24, edge2, 1).
latency(fog24, edge3, 1).
latency(fog24, edge4, 1).
latency(fog24, edge5, 1).
latency(fog24, edge6, 1).
latency(fog24, edge7, 1).
latency(fog24, edge8, 1).
latency(fog24, edge9, 1).
latency(fog25, edge1, 1).
latency(fog25, edge2, 1).
latency(fog25, edge3, 1).
latency(fog25, edge4, 1).
latency(fog25, edge5, 1).
latency(fog25, edge6, 1).
latency(fog25, edge7, 1).
latency(fog25, edge8, 1).
latency(fog25, edge9, 1).
latency(edge1, edge2, 1).
latency(edge1, edge3, 1).
latency(edge1, edge4, 1).
latency(edge1, edge5, 1).
latency(edge1, edge6, 1).
latency(edge1, edge7, 1).
latency(edge1, edge8, 1).
latency(edge1, edge9, 1).
latency(edge2, edge3, 1).
latency(edge2, edge4, 1).
latency(edge2, edge5, 1).
latency(edge2, edge6, 1).
latency(edge2, edge7, 1).
latency(edge2, edge8, 1).
latency(edge2, edge9, 1).
latency(edge3, edge4, 1).
latency(edge3, edge5, 1).
latency(edge3, edge6, 1).
latency(edge3, edge7, 1).
latency(edge3, edge8, 1).
latency(edge3, edge9, 1).
latency(edge4, edge5, 1).
latency(edge4, edge6, 1).
latency(edge4, edge7, 1).
latency(edge4, edge8, 1).
latency(edge4, edge9, 1).
latency(edge5, edge6, 1).
latency(edge5, edge7, 1).
latency(edge5, edge8, 1).
latency(edge5, edge9, 1).
latency(edge6, edge7, 1).
latency(edge6, edge8, 1).
latency(edge6, edge9, 1).
latency(edge7, edge8, 1).
latency(edge7, edge9, 1).
latency(edge8, edge9, 1).
