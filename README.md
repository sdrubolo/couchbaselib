# couchbaselib

Haskell wrapper for couchbase SDK. In order to run this you should install [Couchbase C SDK](https://docs.couchbase.com/c-sdk/current/hello-world/start-using-sdk.html).
If you want to run the tests please run

```
docker-compose -f ./docker-compose.yml up --build
```

from docker folder. In another terminal window run

```
cabal test --test-show-details=always --test-option=--color
```

from the main folder
