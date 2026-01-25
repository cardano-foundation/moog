---
date: 2025-05-01
slug: poc-results
title: Antithesis Proof-of-Concept Results
description: A summary of our findings and some next steps
authors:
  - abailly
categories:
  - infra
---

Since the [start](./2025-04-02-kick-off-antithesis.md) of our project, Cardano Foundation and Antithesis have been busy working on a Proof-of-Concept aimed at evaluating Antithesis's capability to enhance Cardano's testing infrastructure. The results have been promising, with the platform successfully identifying both previously known and new unknown bugs in the cardano-node.

This blog post outlines our journey and findings so far, and sketches out plans for the short and medium term in order to ensure this project delivers the most value for the Cardano community.

<!-- more -->

## Overview

Our exploration with Antithesis began in January 2025, when we first made contact with their team. Quite excited with the prospect this tool offered for testing existing and feature core components of the Cardano network, a small team was assembled within the Cardano Foundation to work on a Proof-of-Concept with Antithesis team. This project was kicked-off on April 2nd at which point both teams started to collaborate actively to understand how to best use the platform to test cardano-node.

### Goals and Objectives

The PoC was undertaken with the main goal of determining if Antithesis could significantly improve our ability to test the Cardano network and its components. As a stretch goal, we also wanted to gauge interest from the broader Cardano community in using such a tool.

To achieve these goals, we established several concrete objectives:
- Learn how to run Antithesis (AT) on a cluster of [cardano-node](https://github.com/IntersectMBO/cardano-node)
- Reproduce already known bugs, aka. [Brown M&Ms](https://www.smithsonianmag.com/arts-culture/why-did-van-halen-demand-concert-venues-remove-brown-mms-from-the-menu-180982570/)
- Discover new, unknown bugs
- Engage with the Core team to gather early feedback and build Cardano specific test environment (properties, [test composer](https://antithesis.com/docs/test_templates/)...)
- Outline how Antithesis can benefit the community in the future

### The Teams

This initiative brought together expertise from both organizations:

* On the Cardano side, people from the HAL, Network and Infrastructure team at the Cardano Foundation, as well as involvement of engineers from IOE's Consensus team
* On the Antithesis side, solution engineers to support the team.

### Results and Achievements

When measured against our initial objectives, the PoC delivered significantly positive results. Some objectives were not fully completed but they mostly relate to our stated stretch goal. Most importantly, the main goal of this Proof-of-Concept period, namely to demonstrate Antithesis' capabilities to find issues in Cardano network was achieved.

* :white_check_mark: **Learn how to run tests with Haskell node**: Successfully achieved
* :white_check_mark: **Reproduce already known bugs**: Successfully achieved
* :white_check_mark: **Find new and unknown bugs**: Successfully achieved
* :orange_circle: **Engage with other teams to gather early feedback**: Partially achieved
* :orange_circle: **Learn how to best use AT platform for Cardano**: Partially achieved
* :red_circle: **Engage with community to ensure interest and funding**: Not achieved

## More details about our findings

### Infrastructure

Using Antithesis to test cardano-node required some initial effort to create the necessary basic infrastructure of Docker images and compose descriptors. As our first objective was to be able to reproduce some peer-to-peer network related bugs, we leveraged and open-sourced our in-house tool for [generating testnets](https://github.com/cardano-foundation/testnet-generation-tool) configuration.

This enabled us to test relatively large clusters of up to 12 nodes while simulating extended time periods of several hours, enough to trigger various changes in the P2P controller. Throughout this process, we had to overcome various minor challenges integrating cardano-node in Antithesis, and it took us a couple of weeks to be able to run a meaningful test to completion.

As examplified by the [example_10.2.1](https://github.com/cardano-foundation/moog/blob/07474674fc8a8bfbb8a56316939c5eee3e594a6e/compose/testnets/example_10.2.1/README.md#L1) directory, the configuration needed to run a cluster of Cardano nodes in AT will be defined as a [docker compose](https://docs.docker.com/compose/) file containing:

* A set of _n_ networked nodes pulled either from a public or dedicated repository. Each of those containers is typically built from either pre-built cardano-node binaries or a specific source tree, and runs a node along with a process that injects transaction at a regular interval,
* A _sidecar_ container whose purpose is to monitor the rest of the cluster and notify the AT runner of the main events of interest, most importantly when the cluster is ready to be tested.

An AT run then "simply" consists in feeding the `docker-compose.yaml` file to the API and waiting for the result to be notified.

### Bug Discovery

Obviously, the purpose of AT is to find bugs and this was our main objective of the proof-of-concept project. Our experiments with AT were able to:

* Find three known bugs we reintroduced or left in the nodes voluntarily,
* Find three unknown bugs, which is the most interesting outcome we could have expected.

The three known bugs were:

* Race in the Inbound governor startup logic, an [old bug](https://github.com/IntersectMBO/ouroboros-network/pull/5017/commits/f11758176591c9868beb06a42e7f22c9ac36d9b0) that was discovered with [IOSimPOR](https://github.com/input-output-hk/io-sim/blob/main/io-sim/how-to-use-IOSimPOR.md) and fixed in later versions of cardano-node,
* Peer-sharing [known bug](https://github.com/IntersectMBO/ouroboros-network/issues/5058) which is trivial to trigger,
* Blockfetch protocol bug which was reinserted during work on new features.

Of the three previously unknown bugs, two are related to networking and one to consensus. The latter was notified to the consensus core team which quickly confirmed the issue and was able to reproduce it locally, leading to patch included in the 10.4.1 version of the node.

Some assumptions made by AT can lead it to report bugs which aren't ones, like the fact the cardano-node outputs some metrics at a severity level classified as `Critical` which AT wrongly interprets as bugs.

### Troubleshooting bug findings

It's great to find bugs but once one is found, it's even better to be able to investigate promptly its origin. AT provides a few tools to do so, but we haven't had the time yet to leverage those to their fullest extant.

Being a deterministic simulation testing platform, AT can explore various "multiverses", travelling to the past and trying different configurations and fault injections, to better understand how easily a bug is triggered and what kind of circumstances lead to it. The following picture is an example _Bug likelihood graph_ that is generated from the history of executions

![Bug likelihood graph](../assets/img/bug-likelihood-graph.png)

But the most powerful tool at our disposal is the so-called [_Multiverse Debugger_](https://antithesis.com/docs/multiverse_debugging/overview/) which provides an environment, similar to Notebook, where one can interact with the System-Under-Test using any tool available in the Docker images and more importantly where it's possible to travel back in time and _branch_ execution of the SUT in order to investigate various hypothesis on the origin of a bug.

We tried to investigate one bug through the Multiverse Debugger by using [tcpdump](https://www.tcpdump.org/) to generate pcap files and troubleshoot networking issues, but we were not able to reproduce the bug once observed with tcpdump. However, having the containers fully under our control allowed us to be able to download the node's database which was quite helpful to reproduce issues locally.

### Community Engagement

Community outreach during the PoC period remained an area with significant room for development. We introduced and discussed the platform at the Node Diversity workshop, which sparked some interest among attendees, but as we hadn't had concrete results at the time, this interest did not lead to concrete collaboration.
However, the fact we were able to find a genuinely new bug, and the collaboration this lead to with the consensus team, are significantly positive signals that Antithesis can become a valuable asset in the development process of both existing and future "nodes".

We haven't yet had the opportunity to test Amaru on the Antithesis platform due to lack of time and Amaru state of development which, so far, would not make it possible to run meaningful end-to-end tests in a cluster.

## Conclusion

This proof-of-concept project achieved its goals in demonstrating the ability of Antithesis to find interesting bugs, supplementing the existing extensive testing infrastructure the cardano-node possesses. There's still a lot of work lying ahead of us and the Cardano Foundation is committed to keep working with Antithesis to reach that ambitious goal, namely open access to these sophisticated testing capabilities for other node implementations, and more generally the whole Cardano community.

### Next steps

To this end, we plan to work along the following axes:

1. Make it easier for node implementors to use Antithesis for testing,
2. Provide generic properties that can easily be integrated in test composer _sidecar_ that any validator node should respect, such as the one outlined on the [Ouroboros Consensus page](https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/CardanoPraosBasics) and drawn from the research papers.

More concretely, in the short term we'll be focusing on:

1. Better documentation on how to run new tests on the platform,
2. Automating test executions and report sharing through GitHub actions,
3. Enable logs based SDK using [cardano-tracer](https://github.com/cardano-foundation/moog/pull/5) to aggregate cardano-node logs,
4. Express more interesting properties (Chain prefix, chain density, chain growth...) as composable scripts or executables to be included in test composer images,
5. Collaborate with cardano-node, Amaru, and other node implementor teams interested in leveraging Antithesis.

We also want to contribute useful tools and techniques, that can be used both within and without the Antithesis platform, as has been discussed in the [Node Diversity Workshop](https://forum.cardano.org/t/node-diversity-workshop-april-2025/145248/1) that took place in Paris a month ago:

1. Generalise the _block tree_ generators and other testing tools already existing in ouroboros-consensus to make them usable with arbitrary node implementations,
2. Implement an _adversarial node_ that would actively try to thwart honest nodes.

### Call To Action

Because we believe the safety and security of the network is the responsibility of the whole community, this project is meant to be as open as possible and we actively seek and encourage contributions which could take a wide variety of forms:

* Improve infrastructure code (eg. docker images, containers, compose files) we use to run tests,
* Better document this infrastructure and how to run those tests,
* Define [tests](https://antithesis.com/docs/test_templates/) that explore "interesting" scenarios,
* ...
