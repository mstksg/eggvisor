{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

import           Brick
import           Brick.Focus
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Control.Applicative
import           Control.Applicative.Free
import           Control.Applicative.Lift
import           Control.Lens
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Bifunctor
import           Data.Finite
import           Data.Maybe
import           Data.Proxy
import           Data.Reflection
import           Data.Semigroup
import           Data.Text.Prettyprint.Doc  (pretty)
import           Data.Typeable
import           Egg
import           GHC.TypeLits
import           Refined
import           Text.Printf
import           Text.Read                  (readMaybe)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Data.Vector.Sized          as SV
import qualified Graphics.Vty               as Vty

data Input = IText (Maybe T.Text)
           | IArea (Maybe Int) (Maybe T.Text)
           | IList [T.Text] (Maybe Int)

makePrisms ''Input

data InputWidget = IWEditor (Editor T.Text Int)
                 | IWList   (List Int T.Text)

makePrisms ''InputWidget

data InputState = IS { _isWidget   :: InputWidget
                     , _isLabel    :: T.Text
                     , _isValidate :: T.Text -> Maybe String
                     }

makeLenses ''InputState

newtype GenIntF a = GIGet (Int -> a)
    deriving Functor

type GenInt = Free GenIntF

genInt :: GenInt Int
genInt = liftF $ GIGet id

runGenInt :: forall a. GenInt a -> a
runGenInt = ($ 0) . iterM go
  where
    go :: GenIntF (Int -> a) -> Int -> a
    go = \case
      GIGet f -> \i -> f i (succ i)

renderInputState :: Bool -> InputState -> Widget Int
renderInputState f is = txt (is ^. isLabel)
                    <+> str ": "
                    <+> widg
  where
    widg = case is ^. isWidget of
      IWEditor e -> renderEditor (txt . T.intercalate "\n") f e
                <+> case (is ^. isValidate) (T.intercalate "\n" $ getEditContents e) of
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
    goL :: Bool -> T.Text -> Widget Int
    goL True  = withAttr listSelectedAttr . txt
    goL False = txt

handleInputStateEvent :: Vty.Event -> InputState -> EventM Int InputState
handleInputStateEvent evt = isWidget $ _IWList   (handleListEvent   evt)
                                   <=< _IWEditor (handleEditorEvent evt)

instance Named InputState Int where
    getName is = case is ^. isWidget of
      IWEditor e -> getName e
      IWList   l -> getName l

data FormF a = FormF { _ffElem   :: Input
                     , _ffParser :: T.Text -> Either String a
                     , _ffLabel  :: T.Text
                     }

makeLenses ''FormF

inputState :: FormF a -> GenInt InputState
inputState ff = do
    n <- genInt
    let widg = case ff ^. ffElem of
          IText d   -> IWEditor $ editorText n (Just 1) (fromMaybe "" d)
          IArea h d -> IWEditor $ editorText n h        (fromMaybe "" d)
          IList e c -> IWList   $ list       n (V.fromList e) 1
                                    & listSelectedL .~ c
    return $ IS widg (ff ^. ffLabel) p
  where
    p = either Just (const Nothing) . (ff ^. ffParser)

type Form = Ap FormF

textInput_
    :: forall a. Typeable a
    => (T.Text -> Either String a)
    -> T.Text
    -> Maybe (a, T.Text)
    -> Form a
textInput_ p l d = liftAp $ FormF (IText (snd <$> d)) p'
                     (T.pack $ printf "%s [%s]" l (show (typeRep (Proxy @a))))
  where
    p' t = case p t of
      Right x -> Right x
      Left e -> case fst <$> d of
        Just x | T.null t -> Right x
        _                 -> Left e

textInputWith
    :: (Read a, Show a, Typeable b)
    => (a -> Either String b)
    -> (b -> a)
    -> T.Text
    -> Maybe b
    -> Form b
textInputWith v r l d = textInput_ p l d'
  where
    p (T.unpack->st) = case readMaybe st of
      Nothing -> Left $ if null st
                          then "Empty input"
                          else "No parse: " ++ st
      Just  x -> v x
    d' = fmap (\x -> (x, T.pack (show (r x)))) d


textInput :: (Read a, Show a, Typeable a) => T.Text -> Maybe a -> Form a
textInput = textInputWith Right id

data FormState = FS { _fsRing    :: FocusRing Int
                    , _fsWidgets :: [InputState]
                    }

makeLenses ''FormState

data FormError = FE { _feLabel :: T.Text
                    , _feError :: String
                    }
  deriving (Show)

mainForm :: forall a. Form a -> IO (Either [FormError] a)
mainForm fm = renderState <$> defaultMain app s0
  where
    app :: App FormState () Int
    app = App ((:[]) . draw)
              (focusRingCursor (view fsRing))
              handle
              return
              (const aMap)
    draw :: FormState -> Widget Int
    draw (FS r w) = vBox $ withFocusRing r renderInputState <$> w
    handle :: FormState -> BrickEvent Int () -> EventM Int (Next FormState)
    handle fs = \case
      VtyEvent ev -> case ev of
        Vty.EvKey Vty.KEsc         [] -> halt fs
        Vty.EvKey (Vty.KChar '\t') [] -> continue $ fs & fsRing %~ focusNext
        Vty.EvKey Vty.KBackTab     [] -> continue $ fs & fsRing %~ focusPrev
        _                             -> continue =<< case focusGetCurrent (fs ^. fsRing) of
          Nothing -> return fs
          Just n  -> handleEventLensed fs (fsWidgets . atName n) handleInputStateEvent ev
      _ -> continue fs
    aMap :: AttrMap
    aMap = attrMap Vty.defAttr
             [ (editAttr               , Vty.white `on` Vty.blue  )
             , (editFocusedAttr        , Vty.black `on` Vty.yellow)
             , (listAttr               , Vty.white `on` Vty.blue  )
             , (listSelectedAttr       , Vty.blue  `on` Vty.white )
             , (listSelectedFocusedAttr, Vty.black `on` Vty.yellow)
             ]
    s0 = FS (focusRing (getName <$> is)) is
    is = runGenInt . runConstT $ runAp (ConstT . fmap (:[]) . inputState) fm
    renderState :: FormState -> Either [FormError] a
    renderState = fst . runCP (runAp go fm) . view fsWidgets
      where
        go :: forall b. FormF b -> ChunkP [FormError] InputState b
        go ff = CP $ \case
          []   -> (Left [FE "Internal" "Something went wrong with FormState"], [])
          x:xs -> let r = do
                        y <- case x ^. isWidget of
                          IWEditor e -> Right . T.intercalate "\n" $ getEditContents e
                          IWList   l -> do
                            i <- maybe (Left "No item selected")   Right $ l ^. listSelectedL
                            maybe (Left "Improper item selection") Right $ l ^? listElementsL . ix i
                        (ff ^. ffParser) y
                  in  (over _Left ((:[]) . FE (ff ^. ffLabel)) r, xs)

-- intBool :: Form (Refined (LessThan 20) Int, Bool)
-- intBool = (,) <$> formFor "Int"  "Int" (Just x)
--               <*> formFor "Bool" "Bool" Nothing
--   where
--     Right x = refine 10

main :: IO ()
-- main = print =<< return ()
-- main = print =<< mainForm (traversableForm @(SV.Vector 10) @(Finite 4) "Vec" Nothing)
main = print =<< mainForm (formFor @(HabStatus 8) "Habs" Nothing)

class Nullable a b | a -> b where
    toNull   :: Maybe a -> b
    fromNull :: b -> Maybe a

class HasForm a where
    formFor     :: T.Text -> Maybe a -> Form a

instance HasForm Int where
    formFor = textInput

instance HasForm Integer where
    formFor = textInput

instance HasForm Double where
    formFor = textInput

instance HasForm Bool where
    formFor = textInput

instance (HasForm b, Nullable a b) => HasForm (Maybe a) where
    formFor l d = fromNull <$> formFor @b l (toNull <$> d)

instance HasForm String where
    formFor l d = textInput_ (Right . T.unpack) l ((\x -> (x, T.pack x)) <$> d)

instance KnownNat m => HasForm (Finite m) where
    formFor = textInputWith p getFinite
      where
        p x = case packFinite x of
          Nothing -> Left $ printf "Not in range (%d not in Finite %d)" x (natVal (Proxy @m))
          Just y  -> Right y


newtype MaybeFinite n = MF { getMF :: Maybe (Finite n) }

instance KnownNat m => HasForm (MaybeFinite m) where
    formFor l d = textInput_ p l ((\x -> (x, s x)) <$> d)
      where
        s = \case
          MF (Just f) -> T.pack . show . getFinite $ f
          MF Nothing  -> "x"
        p (T.unpack -> st)
          | st == "x" = Right (MF Nothing)
          | otherwise = do
              x <- maybe (Left nerr) Right $ readMaybe st
              let rerr = printf "Not in rage (%d not in Finite %d)" x (natVal (Proxy @m))
              maybe (Left rerr) (Right . MF . Just) $ packFinite x
          where
            nerr | null st   = "Empty input"
                 | otherwise = "No parse: " ++ st

instance KnownNat n => Nullable (Finite n) (MaybeFinite n) where
    toNull   = MF
    fromNull = getMF


instance (Read a, Show a, Predicate p a, Typeable p, Typeable a) => HasForm (Refined p a) where
    -- formFor = textInputWith refine unrefine
    formFor = textInputWith (first (show . pretty) . refine) unrefine

instance KnownNat habs => HasForm (HabStatus habs) where
    formFor l hs =
        HabStatus <$> traversableForm (l <> " Slots") (hs ^? _Just . hsSlots)
                  <*> traversableForm (l <> " Pops")  (hs ^? _Just . hsPop  )

traversableForm
    :: forall t a. (Applicative t, Traversable t, HasForm a)
    => T.Text
    -> Maybe (t a)
    -> Form (t a)
traversableForm l d = go (ixUp (maybe (pure Nothing) (fmap Just) d))
  where
    go :: t (Int, Maybe a) -> Form (t a)
    go = traverse $ \(i, dx) -> formFor (T.pack $ printf "%s (%d)" l i) dx
    ixUp :: t (Maybe a) -> t (Int, Maybe a)
    ixUp = flip evalState 0 . traverse
            (\x -> state $ \i -> ((i, x), i + 1))

-- -- data HabStatus habs
-- --     = HabStatus { _hsSlots :: Vec N4 (Maybe (Finite habs))
-- --                 , _hsPop   :: Vec N4 Double
-- --                 }
-- --   deriving (Show, Eq, Ord, Generic)

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

newtype ConstT c m a = ConstT { runConstT :: m c }
  deriving (Functor)

instance (Applicative m, Monoid c) => Applicative (ConstT c m) where
    pure _ = ConstT $ pure mempty
    f <*> x = ConstT $ liftA2 mappend (runConstT f) (runConstT x)

