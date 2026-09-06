# Cellar -- making workbooks for the tests to open.
#
# Sourced by the GUI smoke tests.  These used to be built by calling into the
# shell's own store module, which was Guile and could be called from a script.
# The store is Haskell now and lives inside the binary, so the fixtures are
# written out here instead -- which is arguably the better test anyway: it
# means the format the tests assume is the format as documented, not whatever
# the code happens to produce.

# cellar_sheet <sheet-directory> <rows> <columns>
# Make an empty sheet folder.
cellar_sheet () {
  local directory="$1" rows="$2" columns="$3"
  mkdir -p "$directory/cells"
  cat > "$directory/sheet.scm" <<EOF
;; A Cellar sheet. The cells are in cells/, one file each.
((format . 1)
 (rows . $rows)
 (columns . $columns)
 (widths))
EOF
}

# cellar_cell <sheet-directory> <name> <source>
cellar_cell () {
  printf '%s\n' "$3" > "$1/cells/$2.scm"
}

# cellar_workbook <workbook-directory> <sheet> [<sheet>...]
# Make a workbook of the current format with one folder per sheet named.
cellar_workbook () {
  local workbook="$1"; shift
  local first="$1"
  mkdir -p "$workbook/sheets"
  {
    echo ";; A Cellar workbook. Each sheet is a folder under sheets/."
    echo "((format . 2)"
    echo " (sheets"
    for sheet in "$@"; do printf '  "%s"\n' "$sheet"; done
    echo " )"
    printf ' (active . "%s"))\n' "$first"
  } > "$workbook/workbook.scm"
  for sheet in "$@"; do
    cellar_sheet "$workbook/sheets/$sheet" 100 26
  done
}

# cellar_active <workbook-directory> <sheet>
# Rewrite the index so that a named sheet is the one showing.
cellar_active () {
  local workbook="$1" active="$2"
  local sheets
  sheets=$(sed -n '/(sheets/,/)/p' "$workbook/workbook.scm" | grep '"' || true)
  {
    echo ";; A Cellar workbook. Each sheet is a folder under sheets/."
    echo "((format . 2)"
    echo " (sheets"
    printf '%s\n' "$sheets"
    echo " )"
    printf ' (active . "%s"))\n' "$active"
  } > "$workbook/workbook.scm"
}
