{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

import           Brick
import           Brick.Focus
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Control.Applicative.Free
import           Control.Applicative.Lift
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Maybe
import           Data.Semigroup
import           Refined
import           Text.Printf
import           Text.Read                 (readMaybe)
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import qualified Graphics.Vty              as Vty

data Input = IText (Maybe T.Text)
           | IArea (Maybe Int) (Maybe T.Text)
           | IList [T.Text] (Maybe Int)

makePrisms ''Input

data InputWidget n = IWEditor (Editor T.Text n)
                   | IWList   (List n T.Text)

makePrisms ''InputWidget

data InputState n = IS { _isWidget :: InputWidget n
                       , _isLabel  :: T.Text
                       , _isValidate :: T.Text -> Maybe String
                       }

makeLenses ''InputState

renderInputState :: (Show n, Ord n) => Bool -> InputState n -> Widget n
renderInputState f is = txt (is ^. isLabel)
                    <+> str ": "
                    <+> widg
  where
    widg = case is ^. isWidget of
      IWEditor e -> renderEditor (txt . T.unlines) f e
                <+> case (is ^. isValidate) (T.unlines $ getEditContents e) of
                      Nothing  -> emptyWidget
                      Just err -> str $ printf " Error: %s" err
      IWList   l -> renderList goL f l
                <+> case l ^. listSelectedL of
                      Nothing -> str " (No item selected)"
                      Just i  -> case l ^? listElementsL . ix i of
                        Nothing -> str " (Improper selection?)"
                        Just x  -> case (is ^. isValidate) x of
                          Nothing -> emptyWidget
                          Just err -> str $ printf " Error: %s" err
    goL :: Bool -> T.Text -> Widget n
    goL True  = withAttr listSelectedAttr . txt
    goL False = txt

handleInputStateEvent :: Ord n => Vty.Event -> InputState n -> EventM n (InputState n)
handleInputStateEvent evt = isWidget $ _IWList   (handleListEvent   evt)
                                   <=< _IWEditor (handleEditorEvent evt)

instance Named (InputState n) n where
    getName is = case is ^. isWidget of
      IWEditor e -> getName e
      IWList   l -> getName l

data FormF n a = FormF { _ffElem   :: Input
                       , _ffName   :: n
                       , _ffParser :: T.Text -> Either String a
                       , _ffLabel  :: T.Text
                       }

makeLenses ''FormF

inputState :: FormF n a -> InputState n
inputState ff = IS widg (ff ^. ffLabel) p
  where
    widg = case ff ^. ffElem of
      IText d   -> IWEditor $ editorText (ff ^. ffName) (Just 1) (fromMaybe "" d)
      IArea h d -> IWEditor $ editorText (ff ^. ffName) h        (fromMaybe "" d)
      IList e c -> IWList   $ list       (ff ^. ffName) (V.fromList e) 1
                                & listSelectedL .~ c
    p = either Just (const Nothing) . (ff ^. ffParser)


type Form n = Ap (FormF n)

textInput_
    :: (T.Text -> Either String a)
    -> n
    -> T.Text
    -> Maybe (a, T.Text)
    -> Form n a
textInput_ p n l d = liftAp $ FormF (IText (snd <$> d)) n p' l
  where
    p' t = case p t of
      Right x -> Right x
      Left e -> case fst <$> d of
        Just x | T.null t -> Right x
        _                 -> Left e


textInput :: (Read a, Show a) => n -> T.Text -> Maybe a -> Form n a
textInput n l d = textInput_ p n l ((\x -> (x, T.pack (show x))) <$> d)
  where
    p (T.unpack->str) = case readMaybe str of
      Nothing -> Left $ if null str
                          then "Empty input"
                          else "No parse: " ++ str
      Just  x -> Right x

refinedTextInput :: (Read a, Show a, Predicate p a) => n -> T.Text -> Maybe (Refined p a) -> Form n (Refined p a)
refinedTextInput n l d = textInput_ p n l ((\x -> (x, T.pack (show (unrefine x)))) <$> d)
  where
    p (T.unpack->str) = case readMaybe str of
      Nothing -> Left $ if null str
                          then "Empty input"
                          else "No parse: " ++ str
      Just x  -> refine x

data FormState n = FS { _fsRing    :: FocusRing n
                      , _fsWidgets :: [InputState n]
                      }

makeLenses ''FormState

data FormError = FE { _feLabel :: T.Text
                    , _feError :: String
                    }
  deriving (Show)

newtype ChunkP e s a = CP { runCP :: [s] -> (Either e a, [s]) }
  deriving (Functor)

instance Semigroup e => Applicative (ChunkP e s) where
    pure x    = CP $ \s -> (Right x, s)
    cf <*> cx = CP $ \xs ->
      case runCP cf xs of
        (Right f, ys) -> case runCP cx ys of
          (Right x, zs) -> (Right (f x), zs)
          (Left  e, zs) -> (Left  e    , zs)
        (Left e, ys ) -> case runCP cx ys of
          (Right _, zs) -> (Left  e      , zs)
          (Left  d, zs) -> (Left (e <> d), zs)

mainForm :: forall n a. (Ord n, Show n) => Form n a -> IO (Either [FormError] a)
mainForm fm = renderState <$> defaultMain app s0
  where
    app :: App (FormState n) () n
    app = App ((:[]) . draw)
              (focusRingCursor (view fsRing))
              handle
              return
              (const aMap)
    draw :: FormState n -> Widget n
    draw (FS r w) = vBox $ withFocusRing r renderInputState <$> w
    handle :: FormState n -> BrickEvent n () -> EventM n (Next (FormState n))
    handle fs = \case
      VtyEvent ev -> case ev of
        Vty.EvKey Vty.KEsc         [] -> halt fs
        Vty.EvKey (Vty.KChar '\t') [] -> continue $ fs & fsRing %~ focusNext
        Vty.EvKey Vty.KBackTab     [] -> continue $ fs & fsRing %~ focusPrev
        _                             -> continue =<< case focusGetCurrent (fs ^. fsRing) of
          Nothing -> return fs
          Just n  -> handleEventLensed fs (fsWidgets . atName n) handleInputStateEvent ev
    aMap :: AttrMap
    aMap = attrMap Vty.defAttr
             [ (editAttr               , Vty.white `on` Vty.blue  )
             , (editFocusedAttr        , Vty.black `on` Vty.yellow)
             , (listAttr               , Vty.white `on` Vty.blue  )
             , (listSelectedAttr       , Vty.blue  `on` Vty.white )
             , (listSelectedFocusedAttr, Vty.black `on` Vty.yellow)
             ]
    s0 = FS (focusRing (getName <$> is)) is
    is = runAp_ ((:[]) . inputState) fm
    renderState :: FormState n -> Either [FormError] a
    renderState = fst . runCP (runAp go fm) . view fsWidgets
      where
        go :: forall b. FormF n b -> ChunkP [FormError] (InputState n) b
        go ff = CP $ \case
          []   -> (Left [FE "Internal" "Something went wrong with FormState"], [])
          x:xs -> let r = do
                        y <- case x ^. isWidget of
                          IWEditor e -> Right . T.unlines $ getEditContents e
                          IWList   l -> do
                            i <- maybe (Left "No item selected")   Right $ l ^. listSelectedL
                            maybe (Left "Improper item selection") Right $ l ^? listElementsL . ix i
                        (ff ^. ffParser) y
                  in  (over _Left ((:[]) . FE (ff ^. ffLabel)) r, xs)

intBool :: Form String (Refined (LessThan 20) Int, Bool)
intBool = (,) <$> refinedTextInput "Int" "Int" (Just x)
              <*> textInput "Bool" "Bool" Nothing
  where
    Right x = refine 10

main :: IO ()
main = print =<< mainForm intBool

atName
    :: forall w n. (Named w n, Eq n)
    => n
    -> Lens' [w] w
atName n f = go
  where
    go [] = error "Name not found but how can this be"
    go (x:xs)
      | getName x == n = (: xs) <$> f x
      | otherwise      = (x :) <$> go xs
