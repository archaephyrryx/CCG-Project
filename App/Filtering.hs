{-# LANGUAGE RecordWildCards #-}

module App.Filtering where

import API.Filter
import App.Core.AppData
import Control.Applicative
import Control.Applicative.Indexed  ( IndexedFunctor(..) , IndexedApplicative(..) )
import Reactive.Threepenny

behaveBFilter :: UBL -> Behavior Filter
behaveBFilter bls@FCBL{..} = CardFilter <$> bPowMin  <*> bPowMax
                                        <*> bCostMin <*> bCostMax
                                        <*> bReqMin  <*> bReqMax
                                        <*> bColSelect
                                        <*> bSetSelect
                                        <*> bTypSelect
                                        <*> bRarSelect
behaveBFilter bls@DBBL{..} = DeckFilter <$> bColSelect 
                                        <*> bSetSelect
                                        <*> bTypSelect
                                        <*> bRarSelect
