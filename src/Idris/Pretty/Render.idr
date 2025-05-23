module Idris.Pretty.Render

import Core.Context
import Core.Core

import Idris.REPL.Opts

import Libraries.Text.PrettyPrint.Prettyprinter
import public Libraries.Text.PrettyPrint.Prettyprinter.Render.Terminal

import System
import System.Term

%default total

getPageWidth : {auto o : Ref ROpts REPLOpts} -> Core PageWidth
getPageWidth = do
  consoleWidth <- getConsoleWidth
  case consoleWidth of
    Nothing => do
      cols <- coreLift getTermCols
      pure $ if cols == 0 then Unbounded else AvailablePerLine cols 1
    Just 0 => pure $ Unbounded
    Just cw => pure $ AvailablePerLine (cast cw) 1

export
render' : PageWidth ->
          Maybe (ann -> AnsiStyle) ->
          Doc ann -> String
render' pageWidth stylerAnn doc = do
  let opts = MkLayoutOptions pageWidth
  let layout = layoutPretty opts doc
  renderString $ case stylerAnn of
    Just stylerAnn => reAnnotateS stylerAnn layout
    Nothing => unAnnotateS layout

export
render : {auto o : Ref ROpts REPLOpts} ->
         (ann -> AnsiStyle) ->
         Doc ann -> Core String
render stylerAnn doc = pure $ render' !getPageWidth (toMaybe !getColor stylerAnn) doc

export
renderWithoutColor : {auto o : Ref ROpts REPLOpts} -> Doc ann -> Core String
renderWithoutColor doc = do
  pageWidth <- getPageWidth
  let opts = MkLayoutOptions pageWidth
  let layout = layoutPretty opts doc
  pure $ renderString $ unAnnotateS layout

export
renderWithSpans : {auto o : Ref ROpts REPLOpts} ->
  Doc ann ->
  Core (String, List (Span ann))
renderWithSpans doc = do
  pageWidth <- getPageWidth
  let opts = MkLayoutOptions pageWidth
  let layout = layoutPretty opts doc
  pure $ displaySpans layout
