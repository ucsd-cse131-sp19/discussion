module SimpleJQ.Calculate where

import SimpleJQ.Types

calculate :: JSON -> JSON
calculate = id

-- foldJSON :: (a -> JSON -> a) -> a -> JSON -> a
-- foldJSON f acc json = undefined
--   where
--     go (JObject kvs) = undefined
--     go (JArray  js)  = undefined
--     go (JString s)   = undefined
--     go (JNumber n)   = undefined
--     go (JBoolean b)  = undefined
--     go JNull         = undefined
