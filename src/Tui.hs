{-# LANGUAGE OverloadedStrings #-}

-- for developing fast use:
-- stack install --file-watch

module Tui where

import System.Directory

import Control.Monad
import Control.Monad.IO.Class
import Cursor.Simple.List.NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import System.Exit

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Core
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

newtype TuiState = TuiState
    { tuiStatePaths :: NonEmptyCursor POC
    } deriving (Show, Eq)

data POC = File FilePath
         | Directory FilePath
         deriving (Show, Eq)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ 
                attrMap 
                    mempty 
                        [
                        ("selected" <> "file", fg red), 
                        ("selected" <> "directory", fg red), 
                        ("file", fg blue),
                        ("directory", fg yellow)
                        ]
        }

buildInitialState :: IO TuiState
buildInitialState = do
    here <- getCurrentDirectory
    contents <-  getDirectoryContents here
    contents' <- forM contents $ \fp -> do
        e <- doesFileExist fp
        pure $ 
            if e 
                then File fp
                else Directory fp 
    case NE.nonEmpty contents' of
        Nothing -> die "There are no contents"
        Just ne -> pure TuiState { tuiStatePaths = makeNonEmptyCursor ne }

-- NE.nonEmpty :: [a] -> Maybe (NonEmpty a)

-- vBox :: [Widget n] -> Widget n
drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
    let nec = tuiStatePaths ts
     in [ border $
        vBox $ 
        concat
            [map (drawPath "False") $ reverse $ nonEmptyCursorPrev nec
            , [drawPath "True" $ nonEmptyCursorCurrent nec]
            , map (drawPath "False") $ nonEmptyCursorNext nec
            ]
        ]

-- str :: String -> Widget n
drawPath :: FilePath -> POC -> Widget n 
drawPath b poc = 
    (if b == "True" 
        then forceAttr "selected" 
        else id) $
    case poc of
        File fp -> withAttr "file" $ str fp
        Directory fp -> withAttr "directory" $ str fp

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                EvKey KDown [] -> do
                    let nec=tuiStatePaths s
                    case nonEmptyCursorSelectNext nec of
                         Nothing -> continue s
                         Just nec' -> continue $ s {tuiStatePaths=nec'}
                EvKey KUp [] -> do
                    let nec=tuiStatePaths s
                    case nonEmptyCursorSelectPrev nec of
                         Nothing -> continue s 
                         Just nec' -> continue $ s {tuiStatePaths=nec'}
                EvKey KEnter [] -> do
                    let fp=nonEmptyCursorCurrent $ tuiStatePaths s 
                    case fp of
                        File _ -> continue s
                        Directory fp -> do
                            liftIO $ setCurrentDirectory fp
                            s' <- liftIO buildInitialState
                            continue s'
                _ -> continue s
        _ -> continue s
