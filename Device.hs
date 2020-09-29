{-# LANGUAGE RecordWildCards #-}
module Device
  (deviceFromProbeResponse
  ,printDevice) where

import Text.XML.Light.Types
import Text.XML.Light.Proc
import Text.XML.Light.Input
import Text.XML.Light.Lexer
import Data.Maybe
import Utils

data Device = Device
  { ipAddress :: String
  , subnetMask :: String
  , gateway :: String
  , serialNumber :: String
  , description :: String
  , activated :: Bool}

printDevice :: Device -> IO ()
printDevice d = do
  putStrLn ""
  putStrLn $ "New device found at " ++ ipAddress d ++ ":"
  putStrLn $ "    Subnet Mask: " ++ subnetMask d
  putStrLn $ "    Gateway: " ++ gateway d
  putStrLn $ "    Description: " ++ description d
  putStrLn $ "    Serial Number: " ++ serialNumber d
  putStrLn $ "    Is activated: " ++ (show $ activated d)

parseBool :: String -> Maybe Bool
parseBool "true"  = Just True
parseBool "false" = Just False
parseBool _       = Nothing

getProp :: String -> [Element] -> Maybe String
getProp s = fmap strContent . listToMaybe . concatMap (findElements $ simpleName s)

deviceFromProbeResponse :: (XmlSource s) => s -> Maybe Device
deviceFromProbeResponse s = do
  let d = onlyElems $ parseXML s
  ipAddress    <- getProp "IPv4Address" d
  subnetMask   <- getProp "IPv4SubnetMask" d
  gateway      <- getProp "IPv4Gateway" d
  serialNumber <- getProp "DeviceSN" d
  description  <- getProp "DeviceDescription" d
  activated    <- getProp "Activated" d >>= parseBool
  return Device {..}
