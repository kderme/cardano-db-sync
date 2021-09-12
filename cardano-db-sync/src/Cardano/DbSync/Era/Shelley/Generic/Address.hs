module Cardano.DbSync.Era.Shelley.Generic.Address where

import Cardano.Api

newtype SerialisedAddress = SerialisedAddress
  { unSerialisedAddress :: ByteString
  , 
  }

serialisedAddressBech32 :: SerialisedAddress -> Text
serialisedAddressBech32 sa =

instance SerialiseAsRawBytes SerialisedAddress
    serialiseToRawBytes = unSerialisedAddress
    -- TODO, but never used
    deserialiseFromRawBytes _ _ = Nothing

instance SerialiseAsBech32 SerialisedAddress where
    bech32PrefixFor = 

instance SerialiseAddress SerialisedAddress where
    serialiseAddress = serialiseToBech32
