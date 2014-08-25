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
insert lm Leaf = Node Leaf lm Leaf
insert (Unknown _) tree = tree
insert lm@(LogMessage _ lval _) tree@(Node lt tlm@(LogMessage _ tval _) rt)
    | lval >= tval =  Node lt tlm (insert lm rt)
    | lval <  tval =  Node (insert lm lt) tlm rt


build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left val right) = inOrder left ++ [val] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = [log | x@(LogMessage message val log) <- inOrder (build messages), 
    case message of 
        Error n -> n >= 50 
        _ -> False ]

