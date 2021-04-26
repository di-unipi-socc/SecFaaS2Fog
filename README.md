
<p><img align="left" width="100"> <h1>FaaS2Fog</h1></p>

FaaS2Fog is a declarative prototype to place orchestrated FaaS applications onto a Fog infrastructure satisfying QoS, hardware and software requirements of serverless functions and employing information-flow security techniques to prevent leaks of critical information. During the placement, FaaS2Fog can resolve bindings of service with functions, considering the service type and latency requierements.
FaaS2Fog has two versions: the _Trust_ one, which uses trust relations among stakeholders to rank the eligible placements, based on the model of [SecFog](https://github.com/di-unipi-socc/SecFog).; the *NoTrust* one, which does not employ trust techniques.
<br></br>
## Prerequisites

Before using **FaaS2Fog** you need to install the latest stable release of [SWI-Prolog](https://www.swi-prolog.org/download/stable) to use the *NoTrust* version.
You need to install [ProbLog2 ](https://dtai.cs.kuleuven.be/problog/index.html) to use the *Trust* version.
## Tutorial

To try **FaaS2Fog** *NoTrust*:

1. Download or clone this repository.

2. Open a terminal in the project folder and run `swipl placer.pl`.

3. Inside the running program either run the query
   ```prolog
   :- placeChain(ChainName, Placement).
   ``` 
   The output are the elegible placements for the application described in `application.pl` onto the infrastructure described in `infrastructure.pl` . Each Placement is composed by the function identifier, the node selected for the placement and the list of resolved bindings with services (triples of service type, service instance and node hosting the service).
   E.g. of single placement
   ```prolog
   Placement= [on(fLogin,labServer,[(database, myUserDB, centralRouter)]),
    on(fNav,switch,[(mapService, openMaps, officeServer)]),
    on(fAR,centralRouter,[])]
   ```
   
To try **FaaS2Fog** *NoTrust*:
1. Download or clone this repository.

2. Open a terminal in the project folder and run `problog placer.pl`.

   The outputs are the elegible placements for the application described in `application.pl` onto the infrastructure described in `infrastructure.pl`, using the trust model and trust network of file `trust.pl`. Each placement has the _trust_ and the _confidence_.
   E.g. of single placement
    ```prolog
   Placement= [on(fLogin,labServer,[(database, myUserDB, centralRouter)]),
    on(fNav,switch,[(mapService, openMaps, officeServer)]),
    on(fAR,centralRouter,[])]: 0.6932717463771, 0.43046721
   ```
