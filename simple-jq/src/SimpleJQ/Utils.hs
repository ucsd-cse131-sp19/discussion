module SimpleJQ.Utils where

import SimpleJQ.Types

-- | Returns the key value pairs if the given json is an object.
getElements :: JSON -> Maybe [(String, JSON)]
getElements (JObject kvs) = Just kvs
getElements _             = Nothing

-- | Returns the items of the list if the given json is an array.
getItems :: JSON -> Maybe [JSON]
getItems (JArray js) = Just js
getItems _           = Nothing


-- | Returns the string if the given json is a string.
getString :: JSON -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

-- | Returns the number if the given json is a number.
getNumber :: JSON -> Maybe Double
getNumber (JNumber n) = Just n
getNumber _           = Nothing

getAverageTemp :: JSON -> Double
getAverageTemp _ = undefined
