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

import           Brick
import           Brick.Focus
import           Brick.Widgets.Edit
import           Control.Applicative.Free
import           Control.Lens
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Text.Read                 (readMaybe)
import qualified Data.Text                 as T
import qualified Graphics.Vty              as V

data Labeled w = Lab { _labLabel  :: T.Text
                     , _labWidget :: w
                     }

instance Named w n => Named (Labeled w) n where
    getName = getName . _labWidget

makeLenses ''Labeled

data EditorP n a = EP { _epEditor :: Labeled (Editor T.Text n)
                      , _epParser :: T.Text -> Maybe a
                      }
  deriving Functor

makeLenses ''EditorP

editorP :: forall a n. Read a => n -> Maybe Int -> T.Text -> EditorP n a
editorP n lim lab = EP (Lab lab (editorText n lim "")) (readMaybe . T.unpack)

renderLabeled
    :: (w -> Widget n)
    -> Labeled w
    -> Widget n
renderLabeled f l = txt (l ^. labLabel)
                <+> str ": "
                <+> f (l ^. labWidget)

type Editing n = Ap (EditorP n)

data EditingS n = ES { _esFR      :: FocusRing n
                     , _esWidgets :: [Labeled (Editor T.Text n)]
                     }

makeLenses ''EditingS

intBool :: Editing T.Text (Int, Bool)
intBool = (,) <$> liftAp (editorP @Int "Int"   (Just 1) "Int" )
              <*> liftAp (editorP @Bool "Bool" (Just 1) "Bool")

runEditing
    :: forall n a e. (Ord n, Show n)
    => Editing n a
    -> (App (EditingS n) e n, EditingS n)
runEditing e = (eApp, eState)
  where
    eApp   = App (\s -> [vBox . fmap (withFocusRing (s ^. esFR) (renderLabeled . renderEditor (txt . T.unlines)))
                            $ s ^. esWidgets]
                 )
                 (focusRingCursor (^. esFR))
                 (\s -> \case
                    VtyEvent ev -> case ev of
                      V.EvKey V.KEsc []         -> halt s
                      V.EvKey (V.KChar '\t') [] -> continue $ s & esFR %~ focusNext
                      _ -> continue =<< case focusGetCurrent (s ^. esFR) of
                             Nothing -> return s
                             Just n  -> handleEventLensed s (esWidgets . atName n . labWidget)
                                handleEditorEvent ev
                    _ -> continue s
                 )
                 return
                 (const $ attrMap V.defAttr
                    [ (editAttr,        V.white `on` V.blue  )
                    , (editFocusedAttr, V.black `on` V.yellow)
                    ]
                 )
    eState = ES (focusRing fr) wd
      where
        (fr, wd) = runAp_ (\ep -> ([getName (ep ^. epEditor)], [ep ^. epEditor])) e

editingRes
    :: forall n a. ()
    => Editing n a
    -> EditingS n
    -> Maybe a
editingRes e s = evalStateT (runAp go e) (s ^. esWidgets)
  where
    go :: forall b. EditorP n b -> StateT [Labeled (Editor T.Text n)] Maybe b
    go ep = do
      t <- T.unlines . getEditContents . (^. labWidget) <$> StateT uncons
      lift $ (ep ^. epParser) t


main :: IO ()
main = do
    s <- uncurry defaultMain (runEditing intBool)
    print $ editingRes intBool s

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
