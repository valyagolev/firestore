# firestore

This is an arguably convenient wrapper around `gogol`'s `Network.Google.FireStore`. This is likely not the idiomatic or best API for Google Firestore/Datastore/Firebase. I don't even understand the difference between all of those trademarks. Bug reports and suggestions are welcome.

It's created to avoid working with `Gogol` directly, which is awesome, but begs for a little wrapper. It also tries to add some checks that Gogol's types don't really express.

I have plans to evolve this somehow, but that's what I needed for now. Suggestions and feedback is always welcome.