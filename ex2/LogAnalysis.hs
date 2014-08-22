module LogAnalysis where

import Log

eatMessageTypeMap :: [String]  -> (MessageType, [String])
eatMessageTypeMap ("W":rest) = ( Warning, rest)
eatMessageTypeMap ("I":rest) = ( Info, rest)
eatMessageTypeMap ("E":rest) = ( Error (read (rest !! 0 )::Int), drop 1 rest)

parseMessage :: String -> LogMessage
parseMessage line = LogMessage messageType timestamp message
    where
        lineWords = words line
        (messageType, rest) = eatMessageTypeMap lineWords
        timestamp = read $ rest !! 0 :: Int
        message = unwords $ drop 1 rest

parse :: String -> [LogMessage]
parse log = map parseMessage $ lines log

insert :: LogMessage -> MessageTree -> MessageTree
insert lm tree =  
