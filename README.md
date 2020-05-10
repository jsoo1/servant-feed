# servant-feed

Provides Servant support for `atom+xml` and `rss+xml` content types.

Provide to and from xml instances from `xmlbf` to automatically serialize and deserialize atom and RSS feeds.

``` haskell
import Servant.API
import Servant.Atom
import Servant.RSS
import Xmlbf

instance ToXml MyAtomFeed where
    ...

instance FromXml MyAtomFeed where
    ...

instance ToXml MyRSSFeed where
    ...

instance FromXml MyRSSFeed where
    ...

type API =
    "feed.xml" :> Get '[Atom] MyAtomFeed
    "rss.xml" :> Get '[RSS] MyRSSFeed
```

Heavily inspired by `servant-xml`.
