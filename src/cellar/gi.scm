;;; Cellar -- GObject Introspection imports.
;;;
;;; G-Golf defines the bindings for imported GI names into whichever module
;;; runs the import, and re-exports them, so doing this once here keeps the
;;; rest of the program free of import boilerplate.
;;;
;;; A note on naming: G-Golf gives every method both a long name
;;; (gtk-file-dialog-open) and a short generic (open). Where the short name
;;; would collide with core Guile -- `open', `close', `sort' -- the rest of
;;; this program uses the long name.

(define-module (cellar gi)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:duplicates (merge-generics replace warn-override-core warn last))

(eval-when (expand load eval)
  (g-irepository-require "Gtk" #:version "4.0")
  (g-irepository-require "Adw" #:version "1")
  (g-irepository-require "GtkSource" #:version "5")
  (g-irepository-require "Pango" #:version "1.0")

  (for-each (lambda (name) (gi-import-by-name "Gio" name))
      '("SimpleAction"
        "SimpleActionGroup"
        "ActionMap"
        "Menu"
        "MenuItem"
        "MenuModel"
        "File"
        "Cancellable"
        "AsyncResult"))

  (for-each (lambda (name) (gi-import-by-name "Pango" name))
      '("EllipsizeMode"))

  (for-each (lambda (name) (gi-import-by-name "Gdk" name))
      '("Display"
        "ModifierType"))

  (for-each (lambda (name) (gi-import-by-name "Gtk" name))
      '("Application"
        "ApplicationWindow"
        "Builder"
        "Widget"
        "Box"
        "Label"
        "Button"
        "CheckButton"
        "Entry"
        "Stack"
        "StackPage"
        "MenuButton"
        "Separator"
        "ScrolledWindow"
        "CssProvider"
        "StyleContext"
        "IconTheme"
        "Window"
        "ColumnView"
        "ColumnViewColumn"
        "ColumnViewCell"
        "ListItem"
        "ListItemFactory"
        "SignalListItemFactory"
        "StringList"
        "StringObject"
        "NoSelection"
        "SingleSelection"
        "SelectionModel"
        "GestureClick"
        "GestureDrag"
        "GestureSingle"
        "Popover"
        "PopoverMenu"
        "PickFlags"
        "EventControllerKey"
        "ListScrollFlags"
        "ScrollInfo"
        "FileDialog"
        "FileFilter"
        "TextBuffer"
        "TextIter"
        "TextView"))

  (for-each (lambda (name) (gi-import-by-name "Adw" name))
      '("Application"
        "ApplicationWindow"
        "ToolbarView"
        "HeaderBar"
        "WindowTitle"
        "Dialog"
        "AlertDialog"
        "AboutDialog"
        "StatusPage"
        "Toast"
        "ToastOverlay"
        "StyleManager"
        "Bin"))

  (for-each (lambda (name) (gi-import-by-name "GtkSource" name))
      '("init"
        "View"
        "Buffer"
        "Language"
        "LanguageManager"
        "StyleScheme"
        "StyleSchemeManager")))
