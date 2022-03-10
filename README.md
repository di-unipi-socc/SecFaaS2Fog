
  

<p><img  align="left"  width="100">  <h1>SecFaaS2Fog</h1></p>

  

SecFaaS2Fog is a declarative prototype to place orchestrated FaaS applications onto a Fog infrastructure satisfying QoS, hardware and software requirements of serverless functions and employing information-flow security techniques to prevent leaks of critical information. During the placement, SecFaaS2Fog can resolve bindings of service with functions, considering the service type and latency requierements. To avoid leaking the value of an if statement guard, SecFaaS2Fog pads the requirements of the functions of two branches to make them indistinguishable from an external attacker.

<br></br>

## Prerequisites

  

Before using **FaaS2Fog** you need to install the latest stable release of [SWI-Prolog](https://www.swi-prolog.org/download/stable) .


## Tutorial

  

To try **SecFaaS2Fog** :

  

1. Download or clone this repository.

  

2. Open a terminal in the project folder and run `swipl placer.pl`.

  

3. Inside the running program either run the query

```prolog

:- secfaas2fog(OrchestrationId, Placement).

```

The output are the elegible placements for the application described in `application.pl` onto the infrastructure described in `infrastructure.pl` . Each Placement is composed by the function identifier, the node selected for the placement and the list of resolved bindings with services (triples of service type, service instance and node hosting the service).

E.g. of single placement

```prolog

Placement= [on(fLogin,labServer,[(database, myUserDB, centralRouter)]),

on(fNav,switch,[(mapService, openMaps, officeServer)]),

on(fAR,centralRouter,[])]

```

4. To have a rendering of the placement, inside swipl run the query

```prolog

:- printMap(OrchestrationId, String).

```
The output is a [Graphviz](https://graphviz.org/)script to be inserted in a diagraph.
