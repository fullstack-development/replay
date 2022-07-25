Some approaches that we attempted, but that ultimately led nowhere.

### Implement `MonadBeam` for `BeamTraced`

The general function [`runReturningMany`](https://hackage.haskell.org/package/beam-core-0.9.2.1/docs/Database-Beam.html#t:MonadBeam) is given a context of `FromBackendRow x` for some arbitrary `x`, and is then expected to produce `x`-values. When replaying, we retrieve the results from JSONs - however, it is impossible to procure a `FromJSON` instance from only `FromBackendRow`, and there is no way to get it elsewhere. Thus, we have to introduce a brand new typeclass, `MonadBeamTraced`. Once we have a typeclass of our own, however, we can simply add a `FromJSON` to the class definition.

### Record and replay at the level of `http-client`

A [`Connection`](https://hackage.haskell.org/package/http-client-0.7.11/docs/Network-HTTP-Client-Internal.html#t:Connection) object is, in fact, an abstract interface that can contain arbitrary functions for "read chunk" and "write chunk" operations; and the `ManagerSettings` can be configured to produce our own connection objects. Thus, it is completely possible to "hijack" all network traffic of `http-client` and its transitive dependencies (including `servant-client`) for recording and replaying.

We don't do this, however, because it may contain volatile data, such as the current datetime or a random nonce. To properly deal with those, we have to work on a higher level, where we can properly determine what data is actually meaningful, and what can be safely ignored for the purposes of regression testing.
