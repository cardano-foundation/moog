test
![](/assets/tartarus.jpeg)

# Antithesis-Cardano

The [Cardano blockchain's][Cardano] core node software implements complex
algorithms and protocols who run in a networked, concurrent context. Many
nodes, some of which could be adversarial, collaborate to achieve the overall
system's behaviour of being a decentralized, immutable ledger. Such a system is
subject to many unpredictable factors such as communication delays, network
partitions, nodes appearing or disappearing.

[Antithesis][Antithesis] is a testing tool which is capable of generating
random events (such as communication delays, network partitions, nodes
appearing or disappearing) in a simulated environment, such that if any
specific random sequence of combination of events leads to a software error, it
can be reproduced.

This project aims to facilitate the use of Antithesis for testing various
components of the Cardano ecosystem, while tracking these test efforts on the
blockchain.

> [!IMPORTANT]
>
> This project is currently in a very early stage: A team at the
> Cardano Foundation is responsible to testing the platform and
> evaluating how it fits the needs of Cardano.  We strive to work in
> the open, engage with the community as much as possible, and expects
> questions and contributions, but the requirement to move fast and do
> a thorough due diligence work might lead us to not be as responsive
> as expected.

## Goal(s)

This project is conceptually split in potentially three phases, with
different goals:
1. Proof-of-concept and evaluation: Does Antithesis significantly
   improves how we can test Cardano network and components?
2. Community validation: Is the "Cardano Community" at large of
   interested in such a tool?
3. Production phase: Here the goal is simply to keep operating the
   platform in the most satisfying and cost-effective way on behalf of
   the community

## Timeline

All dates are expressed relative to project's start date:

| Time        | Phase       | Deliverable                                                                                                                                   |
| ----------- | ----------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| T+1m        | PoC         | PoC report detailing:                                                                                                                         |
|             |             | - experiments run using cardano-node only                                                                                                     |
|             |             | - results (eg. did we find the bugs we expected to find, and possibly other ones?)                                                            |
|             |             | - analysis of fitness-for-purpose of the platform w.r.t testing cardano-node, possibly detailing needed adjustments to the code (eg. tracing) |
|             |             | - feedback from community (eg. core team, other node implementors, SMEs, SPOs, etc.                                                           |
|             |             | - *Answer to the question*: Shall we keep evaluating the platform to test the cardano-node and/or other components of Cardano?                |
| T+3m        | Prototyping | Evaluation report detailing:                                                                                                                  |
|             |             | - as above plus                                                                                                                               |
|             |             | - possible plan(s) to make the platform acccessible to community members                                                                      |
|             |             | - evaluation of cost/benefit for node/components implementors                                                                                 |
|             |             | - economic analysis of operating model for the project                                                                                        |
|             |             | - *Answer to the question*: should the Cardano community pursue investment in the platform?                                                   |
| T+ (k * 3m) | Production  | Produce quarterly reports on the use of the platform                                                                                          |

## Basic repository structure

Here is a sketch of the expected structure of this repository, and the overall process to run and analyse tests results.

There shall be _one directory per test_, containing all the material to run the test on Antithesis along with outcomes:
* `docker-compose` definition
* Dockerfile to build the image if needed
* workload and test drivers
* Test results artifacts and analysis

Each test run will be triggered from a Github action:
* Initially through a manual trigger with a parameter pointing at the right directory:
* Later on through submitting a _Pull Request_

Test results will be available through an authenticated access to antithesis platform, so they won't be directly shareable. Moreover, in the case test results expose some potential security vulnerability, we certainly don't want to disclose those publicly before notifying interested parties. The general process should look like:
* Once test results are available, acknowledge the end of test by updating the [Logbook](./Logbook.md) with some basic data from the result (time run, number of tests executed)
* Analyse the result
* Write a report and publish it in the relevant directory

> [!NOTE]
>
> It's still unclear how the results are available, what kind of
> reports are produced, and how to analyse and use those.  Things will
> become clearer after onboarding and a few tests have been run.

### Tests plan

We'll use GH Issue(s) to keep track of the list of interesting tests
we want to run. At the time of this writing, we mostly want to have a
baseline (or smoke) test to onboard the team with the platform, then a
few tests to reproduce _known bugs_ in the network, consensus, or even
ledger parts, in order to assess effectiveness of the platform and
understand how tests can be best structured to reap the most benefits
out of it.

## Logbook

At least during the PoC/Prototyping period, it's critical the team
updates the [Logbook](./Logbook.md) on frequent basis, at least
weekly and ideally daily, to keep track of experiments run, issues,
successes, problems, etc.

## Radicle

We are using [radicle](https://radicle.xyz/) to track the code and the issues.
You can observe
- code at [rad:z2a7Te5b28CX5YyPQ7ihrdG2EEUsC](https://app.radicle.xyz/nodes/ash.radicle.garden/rad:z2a7Te5b28CX5YyPQ7ihrdG2EEUsC) - issues at [issues](https://app.radicle.xyz/nodes/ash.radicle.garden/rad:z2a7Te5b28CX5YyPQ7ihrdG2EEUsC/issues)

Developers:
 - paolino, [did:key:z6MksH6Yr4pkJqPYnY4N5e5a5bCdyCW88grKRkkK6KeMGwLN](https://app.radicle.xyz/nodes/ash.radicle.garden/users/did:key:z6MksH6Yr4pkJqPYnY4N5e5a5bCdyCW88grKRkkK6KeMGwLN)
 - paweljakubas, [did:key:z6Mks4nj3eXrWhjEXknLooeH8ac9c8XcTSzmM7GaooaVyEMN](https://app.radicle.xyz/nodes/ash.radicle.garden/users/did:key:z6Mks4nj3eXrWhjEXknLooeH8ac9c8XcTSzmM7GaooaVyEMN)
 - anviking, [did:key:z6MkoqswZoM5EtGgsWyTYbrbAw2MXWd2JmSvsQ8Ns9jstmCX](https://app.radicle.xyz/nodes/ash.radicle.garden/users/did:key:z6MkoqswZoM5EtGgsWyTYbrbAw2MXWd2JmSvsQ8Ns9jstmCX)
 - abailly, [did:key:z6MkhgPg6WShnhJcmfwox4G5yL3EvJ2zW8L31SZLD95yUi11](https://app.radicle.xyz/nodes/ash.radicle.garden/users/did:key:z6MkhgPg6WShnhJcmfwox4G5yL3EvJ2zW8L31SZLD95yUi11)

## See Also

- Experimental CLI [README](cli/README.md)
- [Project blog][Blog]
- License: see LICENSE
- Contributing: see CONTRIBUTING.md
- Security: see SECURITY.md
- Other projects by [HAL][HAL]
- Other projects by the [Cardano Foundation][CF]
- About [Cardano][Cardano]

<!-- MARKDOWN LINKS & IMAGES -->

[Antithesis]: https://antithesis.com
[Blog]: https://cardano-foundation.github.io/antithesis/
[HAL]: https://github.com/cardano-foundation/hal
[CF]: https://github.com/cardano-foundation
[Cardano]: https://cardano.org/
