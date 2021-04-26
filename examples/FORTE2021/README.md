
<p><img align="left" width="100"> <h1>FORTE2021 Example</h1></p>

Here are stored the declaration to run the motivating example of the FORTE2120 paper.

## Running the example

To run the example the [examples parent directory](https://github.com/di-unipi-socc/FaaS2Fog/tree/main/examples) instruction should be followed.
The first question posed by the motivating example is:

> **Q1**  - Is there any eligible placement and service binding of `chainGath`
> over the available Fog infrastructure, such that all software,
> hardware, latency, external service, security requirements are met,
> and data leaks are avoided?

The answer to this question is given issuing the query `?- faas2Fog(chainGath, P).` to the placer with no trust version.
The results are:

    P = [on(fLogin, ispRouter, [(userDB, myUsers, ispRouter)]), on(fShop, southAntenna, []), on(fGeo, private1, [(maps, openM, private1)]), on(fGather, ispRouter, [(shops, myShop, switch)]), on(fAR, labServer, [])] ;
    P = [on(fLogin, ispRouter, [(userDB, myUsers, ispRouter)]), on(fShop, southAntenna, []), on(fGeo, ispRouter, [(maps, cMaps, cloudNode)]), on(fGather, switch, [(shops, myShop, switch)]), on(fAR, labServer, [])] ;
    P = [on(fLogin, ispRouter, [(userDB, myUsers, ispRouter)]), on(fShop, southAntenna, []), on(fGeo, ispRouter, [(maps, openM, private1)]), on(fGather, switch, [(shops, myShop, switch)]), on(fAR, labServer, [])] ;
    P = [on(fLogin, ispRouter, [(userDB, myUsers, ispRouter)]), on(fShop, southAntenna, []), on(fGeo, northAntenna, [(maps, openM, private1)]), on(fGather, ispRouter, [(shops, myShop, switch)]), on(fAR, labServer, [])] ;
    P = [on(fLogin, ispRouter, [(userDB, myUsers, ispRouter)]), on(fShop, southAntenna, []), on(fGeo, switch, [(maps, openM, private1)]), on(fGather, ispRouter, [(shops, myShop, switch)]), on(fAR, labServer, [])] ;
    P = [on(fLogin, ispRouter, [(userDB, myUsers, ispRouter)]), on(fShop, labServer, []), on(fGeo, ispRouter, [(maps, cMaps, cloudNode)]), on(fGather, switch, [(shops, myShop, switch)]), on(fAR, southAntenna, [])] ;
    P = [on(fLogin, ispRouter, [(userDB, myUsers, ispRouter)]), on(fShop, labServer, []), on(fGeo, ispRouter, [(maps, openM, private1)]), on(fGather, switch, [(shops, myShop, switch)]), on(fAR, southAntenna, [])] ;
    P = [on(fLogin, ispRouter, [(userDB, myUsers, ispRouter)]), on(fShop, labServer, []), on(fGeo, switch, [(maps, openM, private1)]), on(fGather, ispRouter, [(shops, myShop, switch)]), on(fAR, southAntenna, [])] ;
    P = [on(fLogin, labServer, [(userDB, myUsers, ispRouter)]), on(fShop, ispRouter, []), on(fGeo, labServer, [(maps, openM, private1)]), on(fGather, switch, [(shops, myShop, switch)]), on(fAR, southAntenna, [])] ;

The second question posed by motivating example is:

> **Q2** – How are the results of ***Q1*** affected, when considering trust
> relations among the involved stakeholders?


The answer to this question is given running the placer with no trust version.
The results are:

    faas2fog(chainGath,[on(fLogin,ispRouter,[(userDB, myUsers, ispRouter)]), on(fShop,labServer,[]), on(fGeo,ispRouter,[(maps, cMaps, cloudNode)]), on(fGather,switch,[(shops, myShop, switch)]), on(fAR,southAntenna,[])]):   0.611528074761776, 0.31381059609
    faas2fog(chainGath,[on(fLogin,ispRouter,[(userDB, myUsers, ispRouter)]), on(fShop,labServer,[]), on(fGeo,ispRouter,[(maps, openM, private1)]), on(fGather,switch,[(shops, myShop, switch)]), on(fAR,southAntenna,[])]):   0.2694086933363, 0.22876792454961
    faas2fog(chainGath,[on(fLogin,ispRouter,[(userDB, myUsers, ispRouter)]), on(fShop,labServer,[]), on(fGeo,switch,[(maps, openM, private1)]), on(fGather,ispRouter,[(shops, myShop, switch)]), on(fAR,southAntenna,[])]):   0.2694086933363, 0.22876792454961
    faas2fog(chainGath,[on(fLogin,ispRouter,[(userDB, myUsers, ispRouter)]), on(fShop,southAntenna,[]), on(fGeo,ispRouter,[(maps, cMaps, cloudNode)]), on(fGather,switch,[(shops, myShop, switch)]), on(fAR,labServer,[])]):   0.770301940419, 0.4782969
    faas2fog(chainGath,[on(fLogin,ispRouter,[(userDB, myUsers, ispRouter)]), on(fShop,southAntenna,[]), on(fGeo,ispRouter,[(maps, openM, private1)]), on(fGather,switch,[(shops, myShop, switch)]), on(fAR,labServer,[])]):   0.33935651985159, 0.3486784401
    faas2fog(chainGath,[on(fLogin,ispRouter,[(userDB, myUsers, ispRouter)]), on(fShop,southAntenna,[]), on(fGeo,northAntenna,[(maps, openM, private1)]), on(fGather,ispRouter,[(shops, myShop, switch)]), on(fAR,labServer,[])]):   0.335962954653074, 0.31381059609
    faas2fog(chainGath,[on(fLogin,ispRouter,[(userDB, myUsers, ispRouter)]), on(fShop,southAntenna,[]), on(fGeo,private1,[(maps, openM, private1)]), on(fGather,ispRouter,[(shops, myShop, switch)]), on(fAR,labServer,[])]):   0.167981477326538, 0.282429536481
    faas2fog(chainGath,[on(fLogin,ispRouter,[(userDB, myUsers, ispRouter)]), on(fShop,southAntenna,[]), on(fGeo,switch,[(maps, openM, private1)]), on(fGather,ispRouter,[(shops, myShop, switch)]), on(fAR,labServer,[])]):   0.302366659187767, 0.282429536481
    faas2fog(chainGath,[on(fLogin,labServer,[(userDB, myUsers, ispRouter)]), on(fShop,ispRouter,[]), on(fGeo,labServer,[(maps, openM, private1)]), on(fGather,switch,[(shops, myShop, switch)]), on(fAR,southAntenna,[])]):   0.242467824002671, 0.205891132094649




