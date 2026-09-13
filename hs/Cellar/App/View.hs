{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}

-- | The window, as a function of what it is showing.
--
-- Every widget in Cellar is here, from the title down to a cell, and nothing
-- here does anything: a window is what a state looks like, and what a person
-- does to it comes back as an 'Event'.  gi-gtk4-declarative works out which
-- widgets have to change.
--
-- The menus are the exception, and deliberately: they are 'Gio.MenuModel'
-- values naming application actions, which is what lets a keystroke and a menu
-- item mean the same thing, so they stay in the Blueprint file and are handed
-- in here.
module Cellar.App.View
  ( ViewEnv (..)
  , windowView
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.FilePath (takeDirectory, takeFileName)

import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import GI.Gtk.Declarative
import GI.Gtk.Declarative.Container.Stack (StackChild (..), StackChildProperties (..), defaultStackChildProperties)
import GI.Gtk.Declarative.Adwaita.Bin ()
import GI.Gtk.Declarative.Adwaita.HeaderBar
import GI.Gtk.Declarative.Adwaita.References (tabBarView)
import GI.Gtk.Declarative.Adwaita.Slots (titleWidget)
import qualified GI.Gtk.Declarative.Adwaita.TabView as TV
import GI.Gtk.Declarative.Adwaita.TabView (tabView, defaultTabViewParams, TabViewParams (..))
import GI.Gtk.Declarative.Adwaita.ToolbarView
import GI.Gtk.Declarative.App.Simple (AppView)

import Cellar.App.Event
import Cellar.App.State
import Cellar.Config (abbreviate)
import Cellar.Grid.Model
import Cellar.Ref

-- | What the window needs that a state cannot say.
data ViewEnv = ViewEnv
  { viewPrimaryMenu :: Gio.MenuModel
    -- | The gestures for one sheet's grid.  They are made when the grid's
    -- widgets are, and they say what they noticed rather than doing anything.
  , viewGridHandlers :: TabId -> GridHandlers
    -- | The window and the toast overlay, which the parts of Cellar that are
    -- not drawn -- dialogs, toasts -- need a handle on.
  , viewTookWindow :: Adw.ApplicationWindow -> IO ()
  , viewTookToasts :: Adw.ToastOverlay -> IO ()
  }

windowView :: ViewEnv -> State -> AppView Adw.ApplicationWindow Event
windowView env state = bin Adw.ApplicationWindow
  [ #title := "Cellar"
  , #defaultWidth := 1100
  , #defaultHeight := 720
  , on #closeRequest (False, WindowClosing)
  , afterCreated (viewTookWindow env)
  ]
  $ container Adw.ToolbarView []
      [ toolbarTop (headerBar env state)
      , toolbarTop (tabBar state)
      , toolbarTop (cellBar state)
      , toolbarContent $ bin Adw.ToastOverlay
          [afterCreated (viewTookToasts env)]
          (pages env state)
      ]

--
-- The bars
--

headerBar :: ViewEnv -> State -> Widget Event
headerBar env state = container Adw.HeaderBar
  [ titleWidget $ widget Adw.WindowTitle
      [#title := "Cellar", #subtitle := subtitleOf state]
  ]
  [ headerBarStart $ widget Gtk.Button
      [ #iconName := "view-refresh-symbolic"
      , #tooltipText := "Recalculate the sheet"
      , #visible := sheetShowing state
      , on #clicked RecalculatePressed
      ]
  , headerBarEnd $ widget Gtk.MenuButton
      [ #iconName := "open-menu-symbolic"
      , #tooltipText := "Main menu"
      , #menuModel := viewPrimaryMenu env
      , #primary := True
      ]
  ]

-- | The tabs.  The bar is drawn here and the tabs in it are the pages of the
-- view below, which is the one widget that holds both.
tabBar :: State -> Widget Event
tabBar state = widget Adw.TabBar
  [ #autohide := False
  , #expandTabs := False
  , #visible := sheetShowing state
  , tabBarView "sheet-tabs"
  , slot "end-action-widget" Adw.tabBarSetEndActionWidget $ widget Gtk.Button
      [ #iconName := "list-add-symbolic"
      , #tooltipText := "Add a sheet to this workbook (Ctrl+T)"
      , #actionName := "app.add-sheet"
      , classes ["flat"]
      ]
  ]

-- | Which cell is selected, and the Guile source behind it.
cellBar :: State -> Widget Event
cellBar state = container Gtk.Box
  [ #spacing := 6
  , #marginStart := 6
  , #marginEnd := 6
  , #marginTop := 3
  , #marginBottom := 3
  , #visible := sheetShowing state
  , classes ["toolbar"]
  ]
  [ BoxChild defaultBoxChildProperties $ widget Gtk.Label
      [ #label := T.pack (refName active)
      , #widthChars := 6
      , #xalign := 0
      , classes ["monospace", "heading"]
      ]
  , BoxChild defaultBoxChildProperties $
      widget Gtk.Separator [#orientation := Gtk.OrientationVertical]
  , BoxChild defaultBoxChildProperties { expand = True, fill = True } $
      widget Gtk.Label
        [ #label := said
        , #hexpand := True
        , #xalign := 0
        , #ellipsize := Pango.EllipsizeModeEnd
        , #singleLineMode := True
        , classes (if empty' then ["monospace", "dim-label"] else ["monospace"])
        ]
  , BoxChild defaultBoxChildProperties $ widget Gtk.Button
      [ #iconName := "document-edit-symbolic"
      , #tooltipText := "Edit this cell's Guile source (Enter)"
      , classes ["flat"]
      , on #clicked EditPressed
      ]
  , BoxChild defaultBoxChildProperties $ widget Gtk.Button
      [ #iconName := "document-open-symbolic"
      , #tooltipText := "Open this cell's file in your text editor (Ctrl+Shift+E)"
      , #actionName := "app.open-cell"
      , classes ["flat"]
      ]
  ]
  where
    active = maybe (Ref 0 0) (modelActive . tabGrid) (currentTab state)
    source = sourceOf active state
    empty' = maybe True (null . unwords . words) source
    said
      | empty' = "empty \8212 double-click a cell to write Guile"
      | otherwise = T.pack (unwords (words (fromMaybe "" source)))

--
-- The two pages
--

pages :: ViewEnv -> State -> Widget Event
pages env state = container Gtk.Stack
  [#visibleChildName := if sheetShowing state then "sheet" else "start"]
  [ StackChild defaultStackChildProperties { name = "start" } (startPage state)
  , StackChild defaultStackChildProperties { name = "sheet" } (sheets env state)
  ]

-- | What is on screen with no workbook open.
startPage :: State -> Widget Event
startPage state = bin Adw.StatusPage
  [ #iconName := "dev.enzuru.Cellar"
  , #title := "Cellar"
  , #description := T.concat
      [ "A workbook is a folder of small files \8212 one for each cell \8212 so a "
      , "set of spreadsheets can be kept under version control like anything "
      , "else you write."
      ]
  ]
  $ container Gtk.Box
      [ #orientation := Gtk.OrientationVertical
      , #spacing := 12
      , #halign := Gtk.AlignCenter
      ]
      [ BoxChild defaultBoxChildProperties $ widget Gtk.Button
          [ #label := "Open Workbook\8230"
          , #actionName := "app.open"
          , classes ["pill", "suggested-action"]
          ]
      , BoxChild defaultBoxChildProperties $ widget Gtk.Button
          [ #label := "New Workbook\8230"
          , #actionName := "app.new"
          , classes ["pill"]
          ]
      , BoxChild defaultBoxChildProperties $ widget Gtk.Button
          [ #label := "New Scratch Workbook"
          , #actionName := "app.new-scratch"
          , classes ["flat"]
          ]
      , BoxChild defaultBoxChildProperties (recent state)
      ]

-- | The workbooks opened lately.  Hidden rather than empty when there are
-- none, since a heading over nothing says only that something is missing.
recent :: State -> Widget Event
recent state = container Gtk.Box
  [ #orientation := Gtk.OrientationVertical
  , #spacing := 6
  , #marginTop := 18
  , #widthRequest := 380
  , #visible := not (null (stateRecent state))
  , #name := "recent-workbooks"
  ]
  [ BoxChild defaultBoxChildProperties $ widget Gtk.Label
      [#label := "Recent Workbooks", #xalign := 0, classes ["heading"]]
  , BoxChild defaultBoxChildProperties $ container Gtk.ListBox
      [ #selectionMode := Gtk.SelectionModeNone
      , #name := "recent-list"
      , classes ["boxed-list"]
      ]
      (V.fromList (map (recentRow (stateHome state)) (stateRecent state)))
  ]

recentRow :: FilePath -> FilePath -> Widget Event
recentRow home path = widget Adw.ActionRow
  [ #title := T.pack (takeFileName path)
  , #subtitle := T.pack (abbreviate (Just home) (takeDirectory path))
  , #useMarkup := False
  , #activatable := True
  , on #activated (Act (OpenRecentAt path))
  , afterCreated $ \this ->
      Adw.actionRowAddPrefix this
        =<< Gtk.imageNewFromIconName (Just "folder-symbolic")
  ]

-- | The sheets of the workbook, one tab each.
sheets :: ViewEnv -> State -> Widget Event
sheets env state = tabView
  [ #hexpand := True
  , #vexpand := True
  , #name := "sheet-tabs"
  ]
  defaultTabViewParams
    { tabs = V.fromList (map (sheet env) (stateTabs state))
    , selected = keyOf . openCurrent <$> stateOpen state
    , onSelected = Just (TabSelected . readKey)
    , onReordered = Just (TabsReordered . map readKey . V.toList)
    , onClosePage = Just (TabCloseAsked . readKey)
    , closeAnswer = stateCloseAnswer state
    }

sheet :: ViewEnv -> Tab -> TV.Tab Event
sheet env tab = TV.Tab
  { TV.tabKey = keyOf (tabId tab)
  , TV.tabTitle = T.pack (tabName tab)
  , TV.tabChild = GridSaid (tabId tab) <$> gridView (viewGridHandlers env (tabId tab))
                                                    (tabGrid tab)
  }

keyOf :: TabId -> Text
keyOf = T.pack . show

-- | The sheet a key stands for.  The keys are Cellar's own, written by
-- 'keyOf', so one always reads; a key that does not is answered with a name no
-- sheet has, and every handler passes over it.
readKey :: Text -> TabId
readKey key = TabId (fromMaybe 0 (readMaybe (T.unpack key)))
  where readMaybe text = case reads text of { [(n, "")] -> Just n; _ -> Nothing }
