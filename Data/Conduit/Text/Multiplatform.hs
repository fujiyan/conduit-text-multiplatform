module Data.Conduit.Text.Multiplatform
    ( linesCRLF
    , encodeByICU
    , decodeByICU
    ) where

import Prelude hiding (break, null)
import Data.ByteString (ByteString)
import Data.Text
import Data.Conduit
import Data.Conduit.Binary
import Data.Text.ICU.Convert
import Control.Monad
import Control.Monad.Trans


linesCRLF :: Monad m => Conduit Text m Text
linesCRLF = loop id
  where
    loop front = await >>= maybe (finish front) (goCR front)

    finish front =
        let final = front empty
         in unless (null final) (yield final)

    goCR sofar more =
        case uncons second of
            Just (_, second') ->
                    yield (sofar first') >>
                        case uncons second' of
                            Just (lf, second'') ->
                                case lf of
                                    '\n' -> goCR id second''
                                    _    -> goCR id second'
                            Nothing -> goCR id second'
            Nothing -> goLF sofar more

      where
        (first', second) = break (== '\r') more

    goLF sofar more =
        case uncons second of
            Just (_, second') -> yield (sofar first') >> goCR id second'
            Nothing ->
                let rest = sofar more
                 in loop $ append rest

      where
        (first', second) = break (== '\n') more

convertByICU :: MonadIO m => (Converter -> s -> d) -> String -> Conduit s m d
convertByICU f name = do
    conv <- liftIO $ open name (Just False)
    loop f conv

  where
    loop f conv = await >>= maybe (return ()) (go f conv)

    go f conv s = do
        yield $ f conv s
        loop f conv

decodeByICU :: MonadIO m => String -> Conduit ByteString m Text
decodeByICU = convertByICU toUnicode

encodeByICU :: MonadIO m => String -> Conduit Text m ByteString
encodeByICU = convertByICU fromUnicode
