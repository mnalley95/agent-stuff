# System Design Resource Directory

## Source 1: karanpratapsingh/system-design

Base URL: `https://github.com/karanpratapsingh/system-design`

All topics are in the README. Fetch the base URL with a prompt targeting the specific topic,
e.g. `WebFetch(url, "Explain the design principles and trade-offs for <topic>")`.

### Chapter I — Networking & Infrastructure Fundamentals

| Topic | Anchor |
|-------|--------|
| IP | `#ip` |
| OSI Model | `#osi-model` |
| TCP and UDP | `#tcp-and-udp` |
| Domain Name System (DNS) | `#domain-name-system-dns` |
| Load Balancing | `#load-balancing` |
| Clustering | `#clustering` |
| Caching | `#caching` |
| Content Delivery Network (CDN) | `#content-delivery-network-cdn` |
| Proxy | `#proxy` |
| Availability | `#availability` |
| Scalability | `#scalability` |
| Storage | `#storage` |

### Chapter II — Data & Databases

| Topic | Anchor |
|-------|--------|
| Databases and DBMS | `#databases-and-dbms` |
| SQL databases | `#sql-databases` |
| NoSQL databases | `#nosql-databases` |
| SQL vs NoSQL | `#sql-vs-nosql-databases` |
| Database Replication | `#database-replication` |
| Indexes | `#indexes` |
| Normalization and Denormalization | `#normalization-and-denormalization` |
| ACID and BASE consistency models | `#acid-and-base-consistency-models` |
| CAP theorem | `#cap-theorem` |
| PACELC Theorem | `#pacelc-theorem` |
| Transactions | `#transactions` |
| Distributed Transactions | `#distributed-transactions` |
| Sharding | `#sharding` |
| Consistent Hashing | `#consistent-hashing` |
| Database Federation | `#database-federation` |

### Chapter III — Architecture Patterns

| Topic | Anchor |
|-------|--------|
| N-tier architecture | `#n-tier-architecture` |
| Message Brokers | `#message-brokers` |
| Message Queues | `#message-queues` |
| Publish-Subscribe | `#publish-subscribe` |
| Enterprise Service Bus (ESB) | `#enterprise-service-bus-esb` |
| Monoliths and Microservices | `#monoliths-and-microservices` |
| Event-Driven Architecture (EDA) | `#event-driven-architecture-eda` |
| Event Sourcing | `#event-sourcing` |
| CQRS | `#command-and-query-responsibility-segregation-cqrs` |
| API Gateway | `#api-gateway` |
| REST, GraphQL, gRPC | `#rest-graphql-grpc` |
| Long polling, WebSockets, SSE | `#long-polling-websockets-server-sent-events-sse` |

### Chapter IV — Reliability, Security & Operations

| Topic | Anchor |
|-------|--------|
| Geohashing and Quadtrees | `#geohashing-and-quadtrees` |
| Circuit Breaker | `#circuit-breaker` |
| Rate Limiting | `#rate-limiting` |
| Service Discovery | `#service-discovery` |
| SLA, SLO, SLI | `#sla-slo-sli` |
| Disaster Recovery | `#disaster-recovery` |
| VMs and Containers | `#virtual-machines-vms-and-containers` |
| OAuth 2.0 and OpenID Connect | `#oauth-20-and-openid-connect-oidc` |
| Single Sign-On (SSO) | `#single-sign-on-sso` |
| SSL, TLS, mTLS | `#ssl-tls-mtls` |

### Chapter V — Real-World System Design Case Studies

| System | Anchor |
|--------|--------|
| URL Shortener | `#url-shortener` |
| WhatsApp | `#whatsapp` |
| Twitter | `#twitter` |
| Netflix | `#netflix` |
| Uber | `#uber` |

---

## Source 2: Learn You Some Erlang (LYSE)

Focus on **design concepts** (concurrency models, fault isolation, message passing) and
**ignore Erlang-specific syntax** unless directly relevant to the design principle.

### Concurrency & Actor Model

| Topic | URL |
|-------|-----|
| Concurrency concepts, isolation, scaling | https://learnyousomeerlang.com/the-hitchhikers-guide-to-concurrency |
| Process state, message passing, timeouts | https://learnyousomeerlang.com/more-on-multiprocessing |
| Fault isolation between processes, links, monitors | https://learnyousomeerlang.com/errors-and-processes |
| Designing a concurrent application (end-to-end example) | https://learnyousomeerlang.com/designing-a-concurrent-application |

### OTP Design Patterns

| Topic | URL |
|-------|-----|
| What is OTP: generic vs specific, server abstraction | https://learnyousomeerlang.com/what-is-otp |
| Client-server behaviour (gen_server) | https://learnyousomeerlang.com/clients-and-servers |
| Finite-state machines as a design pattern | https://learnyousomeerlang.com/finite-state-machines |
| Event handlers / observer pattern | https://learnyousomeerlang.com/event-handlers |
| Supervision trees and fault-tolerance strategies | https://learnyousomeerlang.com/supervisors |
| Building concurrent applications with OTP | https://learnyousomeerlang.com/building-applications-with-otp |

### Distributed Systems (Design Focus)

| Topic | URL |
|-------|-----|
| Fallacies of distributed computing, CAP theorem, distributed design | https://learnyousomeerlang.com/distribunomicon |
| Distributed OTP applications, failover, takeover | https://learnyousomeerlang.com/distributed-otp-applications |

---

## Source 3: Erlang OTP Design Principles (Official Docs)

Base URL: `https://www.erlang.org/doc/system/`

These pages cover the official OTP design model. Fetch individual pages for depth.

| Topic | URL |
|-------|-----|
| Overview: supervision trees, behaviours, applications, releases | https://www.erlang.org/doc/system/design_principles.html |
| gen_server behaviour | https://www.erlang.org/doc/system/gen_server_concepts.html |
| gen_statem behaviour (state machines) | https://www.erlang.org/doc/system/statem.html |
| gen_event behaviour | https://www.erlang.org/doc/system/events.html |
| Supervisor behaviour | https://www.erlang.org/doc/system/sup_princ.html |
| sys and proc_lib (process tracing & debug) | https://www.erlang.org/doc/system/spec_proc.html |
| Applications | https://www.erlang.org/doc/system/applications.html |
| Included applications | https://www.erlang.org/doc/system/included_applications.html |
| Distributed applications | https://www.erlang.org/doc/system/distributed_applications.html |
| Releases | https://www.erlang.org/doc/system/releases.html |
| Release handling (hot upgrades) | https://www.erlang.org/doc/system/release_handling.html |

---

## Source 4: Akka Typed (Actor Model, JVM)

Base URL: `https://doc.akka.io/libraries/akka-core/current/`

Use for actor model applied to real production systems. Concepts are language-agnostic;
ignore Scala/Java syntax and focus on structural patterns.

### Core Actor Model

| Topic | URL |
|-------|-----|
| Introduction to Actors | https://doc.akka.io/libraries/akka-core/current/typed/actors.html |
| Actor lifecycle | https://doc.akka.io/libraries/akka-core/current/typed/actor-lifecycle.html |
| Interaction patterns (request-response, ask, tell, forward, etc.) | https://doc.akka.io/libraries/akka-core/current/typed/interaction-patterns.html |
| Fault tolerance (supervision strategies) | https://doc.akka.io/libraries/akka-core/current/typed/fault-tolerance.html |
| Actor discovery (receptionist pattern) | https://doc.akka.io/libraries/akka-core/current/typed/actor-discovery.html |
| Routers (pool, group) | https://doc.akka.io/libraries/akka-core/current/typed/routers.html |
| Stash (deferring messages) | https://doc.akka.io/libraries/akka-core/current/typed/stash.html |
| Behaviors as finite state machines | https://doc.akka.io/libraries/akka-core/current/typed/fsm.html |
| Coordinated shutdown | https://doc.akka.io/libraries/akka-core/current/coordinated-shutdown.html |
| Dispatchers (threading model) | https://doc.akka.io/libraries/akka-core/current/typed/dispatchers.html |
| Mailboxes | https://doc.akka.io/libraries/akka-core/current/typed/mailboxes.html |
| Style guide | https://doc.akka.io/libraries/akka-core/current/typed/style-guide.html |

### Cluster & Distributed Actors

| Topic | URL |
|-------|-----|
| Cluster overview | https://doc.akka.io/libraries/akka-core/current/typed/index-cluster.html |

### Persistence / Event Sourcing

| Topic | URL |
|-------|-----|
| Persistence (Event Sourcing) | https://doc.akka.io/libraries/akka-core/current/typed/index-persistence.html |
| Persistence (Durable State) | https://doc.akka.io/libraries/akka-core/current/typed/index-persistence-durable-state.html |

### Getting Started

| Topic | URL |
|-------|-----|
| Getting started guide | https://doc.akka.io/libraries/akka-core/current/typed/guide/index.html |
| General concepts | https://doc.akka.io/libraries/akka-core/current/general/index.html |
