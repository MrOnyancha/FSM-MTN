# Float Management FSM prototype

This is a standalone domain prototype, not a production or deployable money service. It has no Play API, authentication or role checks, MongoDB persistence, Kafka, MTN integration, durable outbox, reconciliation, or crash recovery. The Akka actor stores state in memory and loses it on restart. Do not connect it to real wallets or physical-cash operations.

The FSM models requests, assignment reservations, physical handover, settlement outcomes, balance snapshots, cancellation, and deposit acknowledgement. ConfirmHandover assumes that an upstream authorized caller has verified the correct party: the SA_DSD or DSD for float, the Agent for cash. Both distribution structures use owner and worker identifiers; branch mapping and wallet ownership remain to be defined.

Pending deposits reduce available cash immediately but count against the collection limit until acknowledged. The cash field therefore represents cash counted against the limit, not necessarily cash physically still in hand; pendingDeposit records the difference. This is a conservative prototype accounting convention to be verified with finance.

Command IDs are deduplicated in memory only, and reuse with a different action is rejected. This has no bounded retention policy. Production must persist command identities and payload hashes atomically with state changes, enforce a unique transfer reference, and make cash reservation and request assignment atomic under concurrent and multi-node access.

Before production, add durable MongoDB state and audit, a transactional outbox/inbox, authorization and maker-checker controls, deposit evidence and disputes, failure and reversal handling, timeout and reassignment policies, MTN-specific contracts and status lookup, real balance enquiry, cash reconciliation, supervision and recovery, and end-to-end concurrency and crash tests. Gateway outcomes must not be interpreted as final MTN settlement without contract confirmation. Verify library versions and licensing with the target platform.

Run tests with `sbt test` in an environment with sbt and dependency access.

GitHub Actions runs `sbt -batch clean test` on pushes, pull requests, and manual dispatch. The workflow requires GitHub-hosted runners and access to Maven dependencies. It has not been executed in the current environment.
