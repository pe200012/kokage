{-# LANGUAGE OverloadedStrings #-}

module Types.ShioriSpec ( spec ) where

import           Test.Hspec
import qualified Data.Map.Strict as Map
import           Data.Text ( Text )
import qualified Data.Text as T

import           Types.Shiori

spec :: Spec
spec = do
  describe "ShioriEvent" $ do
    describe "eventToId / eventFromId" $ do
      it "roundtrips OnBoot" $
        eventFromId (eventToId OnBoot) `shouldBe` OnBoot

      it "roundtrips OnMouseClick" $
        eventFromId (eventToId OnMouseClick) `shouldBe` OnMouseClick

      it "roundtrips OnInstallComplete" $
        eventFromId (eventToId OnInstallComplete) `shouldBe` OnInstallComplete

      it "preserves unknown events" $
        eventFromId "OnCustomEvent" `shouldBe` OnEvent "OnCustomEvent"

      it "roundtrips unknown events via eventToId" $
        eventToId (OnEvent "OnMyCustomEvent") `shouldBe` "OnMyCustomEvent"

    describe "eventCategory" $ do
      it "categorizes boot events correctly" $
        eventCategory OnBoot `shouldBe` CatBoot

      it "categorizes mouse events correctly" $
        eventCategory OnMouseClick `shouldBe` CatMouse

      it "categorizes install events correctly" $
        eventCategory OnInstallComplete `shouldBe` CatInstall

      it "categorizes time events correctly" $
        eventCategory OnSecondChange `shouldBe` CatTime

  describe "ShioriResource" $ do
    describe "resourceToId / resourceFromId" $ do
      it "roundtrips ResVersion" $
        resourceFromId (resourceToId ResVersion) `shouldBe` ResVersion

      it "roundtrips ResHomeUrl" $
        resourceFromId (resourceToId ResHomeUrl) `shouldBe` ResHomeUrl

      it "preserves unknown resources" $
        resourceFromId "custom.resource" `shouldBe` ResUnknown "custom.resource"

  describe "parseShioriRequest" $ do
    it "parses a minimal GET request" $ do
      let input = "GET SHIORI/3.0\r\nCharset: UTF-8\r\nSender: SSP\r\nID: OnBoot\r\n\r\n"
      case parseShioriRequest input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right req -> do
          srqMethod req `shouldBe` MethodGet
          srqVersion req `shouldBe` Shiori30
          srqCharset req `shouldBe` "UTF-8"
          srqSender req `shouldBe` "SSP"
          srqId req `shouldBe` "OnBoot"

    it "parses a request with references" $ do
      let input = "GET SHIORI/3.0\r\nCharset: UTF-8\r\nSender: SSP\r\nID: OnMouseClick\r\nReference0: 0\r\nReference1: 123\r\nReference2: 456\r\nReference3: head\r\n\r\n"
      case parseShioriRequest input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right req -> do
          srqId req `shouldBe` "OnMouseClick"
          Map.lookup 0 (srqReferences req) `shouldBe` Just "0"
          Map.lookup 1 (srqReferences req) `shouldBe` Just "123"
          Map.lookup 2 (srqReferences req) `shouldBe` Just "456"
          Map.lookup 3 (srqReferences req) `shouldBe` Just "head"

    it "parses a NOTIFY request" $ do
      let input = "NOTIFY SHIORI/3.0\r\nCharset: UTF-8\r\nSender: SSP\r\nID: basewareversion\r\nReference0: SSP/2.6.00\r\n\r\n"
      case parseShioriRequest input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right req -> do
          srqMethod req `shouldBe` MethodNotify
          srqId req `shouldBe` "basewareversion"

    it "parses SSP extension fields" $ do
      let input = T.unlines
            [ "GET SHIORI/3.0\r"
            , "Charset: UTF-8\r"
            , "Sender: SSP\r"
            , "SenderType: internal,raise\r"
            , "SecurityLevel: local\r"
            , "SecurityOrigin: sstp://192.168.1.1:9801\r"
            , "Status: choosing,balloon(0=0)\r"
            , "ID: OnFirstBoot\r"
            , "BaseID: OnBoot\r"
            , "Reference0: 1\r"
            , "X-SSTP-PassThru-Custom: value\r"
            , "\r"
            ]
      case parseShioriRequest input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right req -> do
          srqSenderType req `shouldBe` [SenderInternal, SenderRaise]
          srqSecurityLevel req `shouldBe` Just SecurityLocal
          srqSecurityOrigin req `shouldBe` Just (Origin "sstp" "192.168.1.1" (Just 9801))
          srqStatus req `shouldBe` [StatusChoosing, StatusBalloon [(0, 0)]]
          srqId req `shouldBe` "OnFirstBoot"
          srqBaseId req `shouldBe` Just "OnBoot"
          Map.lookup "Custom" (srqPassThru req) `shouldBe` Just "value"

    it "rejects empty input" $
      parseShioriRequest "" `shouldSatisfy` isLeft

    it "rejects malformed first line" $
      parseShioriRequest "BADMETHOD SHIORI/3.0\r\n" `shouldSatisfy` isLeft

  describe "parseShioriResponse" $ do
    it "parses a 200 OK response" $ do
      let input = "SHIORI/3.0 200 OK\r\nCharset: UTF-8\r\nSender: TestGhost\r\nValue: \\0\\s[0]Hello!\\e\r\n\r\n"
      case parseShioriResponse input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right res -> do
          srsVersion res `shouldBe` Shiori30
          srsStatus res `shouldBe` Status200
          srsCharset res `shouldBe` "UTF-8"
          srsSender res `shouldBe` "TestGhost"
          srsValue res `shouldBe` Just "\\0\\s[0]Hello!\\e"

    it "parses a 204 No Content response" $ do
      let input = "SHIORI/3.0 204 No Content\r\nCharset: UTF-8\r\nSender: TestGhost\r\n\r\n"
      case parseShioriResponse input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right res -> do
          srsStatus res `shouldBe` Status204
          srsValue res `shouldBe` Nothing

    it "parses SSP extension fields" $ do
      let input = T.unlines
            [ "SHIORI/3.0 200 OK\r"
            , "Charset: UTF-8\r"
            , "Sender: TestGhost\r"
            , "Value: \\0Hello\\e\r"
            , "ValueNotify: \\![sound,beep]\r"
            , "SecurityLevel: local\r"
            , "Marker: Status Info\r"
            , "ErrorLevel: warning\x01\&error\r"
            , "ErrorDescription: Minor issue\x01\&Major issue\r"
            , "BalloonOffset: 10,20\r"
            , "Age: 3\r"
            , "MarkerSend: Partner Marker\r"
            , "Reference0: Partner\r"
            , "X-SSTP-PassThru-ReturnValue: success\r"
            , "\r"
            ]
      case parseShioriResponse input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right res -> do
          srsValue res `shouldBe` Just "\\0Hello\\e"
          srsValueNotify res `shouldBe` Just "\\![sound,beep]"
          srsSecurityLevel res `shouldBe` Just SecurityLocal
          srsMarker res `shouldBe` Just "Status Info"
          srsErrorLevel res `shouldBe` [ErrorWarning, ErrorError]
          srsErrorDescription res `shouldBe` ["Minor issue", "Major issue"]
          srsBalloonOffset res `shouldBe` Just (BalloonOffset 10 20)
          srsAge res `shouldBe` Just 3
          srsMarkerSend res `shouldBe` Just "Partner Marker"
          Map.lookup 0 (srsReferences res) `shouldBe` Just "Partner"
          Map.lookup "ReturnValue" (srsPassThru res) `shouldBe` Just "success"

  describe "serializeShioriRequest" $ do
    it "produces valid SHIORI format" $ do
      let req = ShioriRequest
            { srqMethod         = MethodGet
            , srqVersion        = Shiori30
            , srqCharset        = "UTF-8"
            , srqSender         = "Kokage"
            , srqSenderType     = [SenderInternal]
            , srqSecurityLevel  = Just SecurityLocal
            , srqSecurityOrigin = Nothing
            , srqStatus         = []
            , srqId             = "OnBoot"
            , srqBaseId         = Nothing
            , srqReferences     = Map.singleton 0 "0"
            , srqPassThru       = Map.empty
            , srqHeaders        = Map.empty
            }
          output = serializeShioriRequest req
      -- Check that output contains expected headers
      output `shouldSatisfy` ("GET SHIORI/3.0" `elem`) . lines'
      output `shouldSatisfy` ("Charset: UTF-8" `elem`) . lines'
      output `shouldSatisfy` ("Sender: Kokage" `elem`) . lines'
      output `shouldSatisfy` ("ID: OnBoot" `elem`) . lines'
      output `shouldSatisfy` ("SecurityLevel: local" `elem`) . lines'
      output `shouldSatisfy` ("Reference0: 0" `elem`) . lines'

  describe "serializeShioriResponse" $ do
    it "produces valid SHIORI format" $ do
      let res = ShioriResponse
            { srsVersion          = Shiori30
            , srsStatus           = Status200
            , srsCharset          = "UTF-8"
            , srsSender           = "TestGhost"
            , srsValue            = Just "\\0Hello!\\e"
            , srsValueNotify      = Nothing
            , srsSecurityLevel    = Nothing
            , srsMarker           = Nothing
            , srsErrorLevel       = []
            , srsErrorDescription = []
            , srsBalloonOffset    = Nothing
            , srsAge              = Nothing
            , srsMarkerSend       = Nothing
            , srsReferences       = Map.empty
            , srsPassThru         = Map.empty
            , srsHeaders          = Map.empty
            }
          output = serializeShioriResponse res
      output `shouldSatisfy` ("SHIORI/3.0 200 OK" `elem`) . lines'
      output `shouldSatisfy` ("Value: \\0Hello!\\e" `elem`) . lines'

  describe "SenderType" $ do
    it "parses single sender type" $
      parseSenderType "internal" `shouldBe` Just SenderInternal

    it "parses comma-separated sender types" $
      parseSenderTypes "internal,raise,sstp" `shouldBe` [SenderInternal, SenderRaise, SenderSSTP]

    it "serializes sender types" $
      serializeSenderTypes [SenderInternal, SenderPlugin] `shouldBe` "internal,plugin"

  describe "GhostStatus" $ do
    it "parses simple status" $
      parseGhostStatus "talking" `shouldBe` Just StatusTalking

    it "parses opening status with types" $
      parseGhostStatus "opening(communicate/input)" `shouldBe` Just (StatusOpening ["communicate", "input"])

    it "parses balloon status" $
      parseGhostStatus "balloon(0=2/1=0)" `shouldBe` Just (StatusBalloon [(0, 2), (1, 0)])

    it "serializes ghost statuses" $
      serializeGhostStatuses [StatusTalking, StatusChoosing] `shouldBe` "talking,choosing"

  describe "SecurityOrigin" $ do
    it "parses null origin" $
      parseSecurityOrigin "null" `shouldBe` OriginNull

    it "parses origin without port" $
      parseSecurityOrigin "http://example.com" `shouldBe` Origin "http" "example.com" Nothing

    it "parses origin with port" $
      parseSecurityOrigin "sstp://192.168.1.1:9801" `shouldBe` Origin "sstp" "192.168.1.1" (Just 9801)

    it "serializes origin" $
      serializeSecurityOrigin (Origin "https" "example.com" (Just 443)) `shouldBe` "https://example.com:443"

  describe "ErrorLevel" $ do
    it "parses single error level" $
      parseErrorLevel "warning" `shouldBe` Just ErrorWarning

    it "parses multiple error levels" $
      parseErrorLevels "info\x01\&error\x01\&critical" `shouldBe` [ErrorInfo, ErrorError, ErrorCritical]

    it "serializes error levels" $
      serializeErrorLevels [ErrorNotice, ErrorWarning] `shouldBe` "notice\x01warning"

  describe "BalloonOffset" $ do
    it "parses balloon offset" $
      parseBalloonOffset "10,20" `shouldBe` Just (BalloonOffset 10 20)

    it "serializes balloon offset" $
      serializeBalloonOffset (BalloonOffset (-5) 15) `shouldBe` "-5,15"

-- Helper functions

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- Strip \r from line endings for comparison
lines' :: Text -> [Text]
lines' = map (T.filter (/= '\r')) . T.lines
