module Type.Trout.Client.BaseURI
       ( BaseURI
       , baseURI
       , print
       ) where

import Prelude
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Text.Parsing.Parser (parseErrorMessage, runParser)
import URI (AbsoluteURI, HierPath, Host, Path, Port, Query, UserInfo)
import URI.AbsoluteURI (parser, print) as URI
import URI.AbsoluteURI (AbsoluteURIOptions)
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair

newtype BaseURI = BaseURI (AbsoluteURI UserInfo (HostPortPair Host Port) Path HierPath Query)

absoluteURIOptions ::  Record (AbsoluteURIOptions UserInfo (HostPortPair Host Port) Path HierPath Query)
absoluteURIOptions =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseQuery: pure
  , printQuery: identity
  }

baseURI :: String -> Either String BaseURI
baseURI url =
  bimap
    parseErrorMessage
    BaseURI
    (runParser url $ URI.parser absoluteURIOptions)

print :: BaseURI -> String
print (BaseURI uri) = URI.print absoluteURIOptions uri
