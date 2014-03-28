module Types where

type ClientName = String
type ChannelName = String

data Message
    = Notice String
    | Tell ClientName String
    | Broadcast ChannelName ClientName String
    | Command String
