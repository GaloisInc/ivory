
module Ivory.Compile.AADL.Identifier where

identifier :: String -> String
identifier s = escapeReserved $ map aux s
  where
  aux '-' = '_'
  aux ' ' = '_'
  aux a   = a

escapeReserved :: String -> String
escapeReserved w | w `elem` reserved = 'x':w
                 | otherwise = w
  where
  reserved =
    [ "mode"
    , "port"
    , "group"
    , "public"
    , "private"
    , "properties"
    , "end"
    , "data"
    , "subprogram"
    , "thread"
    , "process"
    , "system"
    , "abstract"
    , "memory"
    , "processor"
    , "bus"
    , "device"
    , "virtual"
    , "extends"
    , "prototypes"
    , "features"
    , "flows"
    , "modes"
    , "subcomponents"
    , "calls"
    , "connections"
    , "flows"
    , "modes"
    , "refined"
    , "in"
    , "out"
    , "feature"
    , "event"
    , "provides"
    , "requires"
    , "access"
    , "parameter"
    , "inverse"
    , "source"
    , "sink"
    , "classifier"
    , "binding"
    , "and"
    , "or"
    , "not"
    , "true"
    , "false"
    , "delta"
    , "reference"
    , "compute"
    , "type"
    , "aadlboolean"
    , "aadlstring"
    , "aadlreal"
    , "aadlinteger"
    , "enumeration"
    , "units"
    , "constant"
    , "record"
    , "range"
    , "list"
    , "of"
    , "inherit"
    , "applies"
    , "to"
    , "all"
    ]

