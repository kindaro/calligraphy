{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering takes a callgraph, and produces a dot file
module Calligraphy.Phases.Render.GraphViz
  ( GraphVizConfig,
    pGraphVizConfig,
    renderGraphViz,
  )
where

import Calligraphy.Phases.Measure
import Calligraphy.Phases.Render.Common
import Calligraphy.Prelude hiding (DeclType)
import Calligraphy.Util.Printer
import Calligraphy.Util.Types
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Options.Applicative hiding (style)
import Text.Printf
import Text.Show (showListWith)

data GraphVizConfig = GraphVizConfig
  { showChildArrowhead :: Bool,
    clusterGroups :: Bool,
    landscapeLayout :: Bool,
    orthogonalEdges :: Bool,
    splines :: Bool,
    reverseDependencyRank :: Bool
  }

pGraphVizConfig :: Parser GraphVizConfig
pGraphVizConfig =
  GraphVizConfig
    <$> flag False True (long "show-child-arrowhead" <> help "Put an arrowhead at the end of a parent-child edge")
    <*> flag True False (long "no-cluster-trees" <> help "Don't draw definition trees as a cluster.")
    <*> flag False True (long "landscape-layout" <> help "…") -- TODO write help message
    <*> flag False True (long "orthogonal-edges" <> help "…") -- TODO write help message
    <*> flag True False (long "no-splines" <> help "Render arrows as straight lines instead of splines")
    <*> flag False True (long "reverse-dependency-rank" <> help "Make dependencies have lower rank than the dependee, i.e. show dependencies above their parent.")

renderGraphViz :: GraphVizConfig -> Prints RenderGraph
renderGraphViz GraphVizConfig {..} (RenderGraph roots calls types) = do
  brack "digraph calligraphy {" "}" $ do
    unless splines $ textLn "splines=false;"
    when landscapeLayout $ textLn "rankdir=\"RL\";"
    textLn "node [style=filled fillcolor=\"#ffffffcf\"];"
    textLn $ "graph [outputorder=edgesfirst" <> (if orthogonalEdges then ", splines=ortho" else "") <> "];"
    case roots of
      Left modules -> mapM_ printModule modules
      Right trees -> mapM_ printTree trees
    forM_ calls $ \(caller, callee) ->
      if reverseDependencyRank
        then edge caller callee []
        else edge callee caller ["dir" .= "back"]
    forM_ types $ \(caller, callee) ->
      if reverseDependencyRank
        then edge caller callee ["style" .= "dotted"]
        else edge callee caller ["style" .= "dotted", "dir" .= "back"]
  where
    printTree :: Prints (Tree RenderNode)
    printTree (Tree.Node nodeInfo children) = wrapCluster $ do
      printNode nodeInfo
      forM_ children $ \child@(Tree.Node childInfo _) -> do
        printTree child
        edge (nodeId nodeInfo) (nodeId childInfo) . catMaybes $
          [ pure ("style" .= "dashed"),
            if' (not showChildArrowhead) ("arrowhead" .= "none")
          ]
      where
        wrapCluster inner
          | clusterGroups && not (null children) = brack ("subgraph cluster_" <> nodeId nodeInfo <> " {") "}" $ do
              textLn "style=invis;"
              inner
          | otherwise = inner

    printModule :: Prints RenderModule
    printModule (RenderModule lbl modId trees) =
      brack ("subgraph cluster_module_" <> modId <> " {") "}" $ do
        strLn $ "label=" <> show lbl <> ";"
        strLn "bgcolor=\"lightgray\""
        forM_ trees printTree

    printNode :: Prints RenderNode
    printNode (RenderNode nId typ lbll tangledness exported) =
      strLn $ nId <> " " <> renderAttrs attrs
      where
        attrs =
          [ "label"
              .= let measurements = case tangledness of
                      Nothing -> []
                      Just Tangledness {..} -> [show willBeRecompiled <> " / " <> show mustBeRecompiled]
                  in ("\"" <> intercalate "\n" (lbll ++ measurements) <> "\""),
            "shape" .= nodeShape typ,
            "style" .= nodeStyle
          ]
            ++ case tangledness of
              Nothing -> []
              Just Tangledness {..} ->
                let
                  red = if normalizedTangledness > 0 then round (normalizedTangledness * 255) :: Int else 0
                  green = if normalizedTangledness < 0 then round (normalizedTangledness * (-255)) :: Int else 0
                  hexy = printf "%02x" :: Int -> String
                 in
                  ["fillcolor" .= ("\"" <> "#" <> hexy (255 - green) <> hexy (255 - red) <> hexy (255 - max red green) <> "\"")]
        nodeStyle =
          show . intercalate ", " . catMaybes $
            [ if' (typ == RecDecl) "rounded",
              if' (not exported) "dashed",
              pure "filled"
            ]

nodeShape :: DeclType -> String
nodeShape DataDecl = "octagon"
nodeShape ConDecl = "box"
nodeShape RecDecl = "box"
nodeShape ClassDecl = "house"
nodeShape ValueDecl = "note"

edge :: ID -> ID -> Attributes -> Printer ()
edge from to attrs = strLn $ show from <> " -> " <> show to <> " " <> renderAttrs attrs

(.=) :: String -> String -> (String, String)
(.=) = (,)

renderAttrs :: Attributes -> String
renderAttrs attrs = showListWith (\(key, val) -> showString key . showChar '=' . showString val) attrs ";"

type Attributes = [(String, String)]
