# Cellar -- making workbooks for the tests to open.
#
# Sourced by the GUI smoke tests.  These used to be built by calling into the
# shell's own store module, which was Guile and could be called from a script.
# The store is Haskell now and lives inside the binary, so the fixtures are
# written out here instead -- which is arguably the better test anyway: it
# means the format the tests assume is the format as documented, not whatever
# the code happens to produce.
#
# A workbook is one file.  Rather than edit that file in place, which means
# counting parentheses, each of these records what the workbook should hold in
# a scratch folder beside it and writes the whole file out again.

# Where the sheets and cells of a workbook are remembered between calls.
_cellar_state () {
  printf '%s/.%s.fixture' "$(dirname "$1")" "$(basename "$1")"
}

# Write the file from what has been recorded.
_cellar_write () {
  local workbook="$1" state sheet
  state=$(_cellar_state "$workbook")
  {
    echo ";; A Cellar workbook: every sheet, and every cell of each."
    echo "((format . 3)"
    printf ' (active . "%s")\n' "$(cat "$state/active")"
    printf ' (sheets'
    while IFS= read -r sheet; do
      printf '\n  ("%s"\n' "$sheet"
      echo "   (rows . 100)"
      echo "   (columns . 26)"
      echo "   (widths)"
      if [ -s "$state/cells/$sheet" ]; then
        printf '   (cells\n'
        # In a substitution, so that the trailing newline goes and the
        # parenthesis that closes the sheet lands on the same line.
        printf '%s' "$(sed 's/^/    /' "$state/cells/$sheet" | sed '$ s/$/)/')"
      else
        printf '   (cells)'
      fi
      printf ')'
    done < "$state/sheets"
    echo '))'
  } > "$workbook"
}

# cellar_workbook <workbook-file> <sheet> [<sheet>...]
# Make a workbook holding one empty sheet per name, showing the first.
cellar_workbook () {
  local workbook="$1"; shift
  local state sheet
  state=$(_cellar_state "$workbook")
  rm -rf "$state"
  mkdir -p "$state/cells" "$(dirname "$workbook")"
  printf '%s\n' "$1" > "$state/active"
  : > "$state/sheets"
  for sheet in "$@"; do
    printf '%s\n' "$sheet" >> "$state/sheets"
    : > "$state/cells/$sheet"
  done
  _cellar_write "$workbook"
}

# cellar_cell <workbook-file> <sheet> <name> <source>
cellar_cell () {
  local workbook="$1" sheet="$2" name="$3" source="$4" state escaped
  state=$(_cellar_state "$workbook")
  escaped=$(printf '%s' "$source" | sed 's/\\/\\\\/g; s/"/\\"/g')
  printf '("%s" . "%s")\n' "$name" "$escaped" >> "$state/cells/$sheet"
  _cellar_write "$workbook"
}

# cellar_add_sheet <workbook-file> <sheet>
# Add an empty sheet, the way somebody else's commit would.
cellar_add_sheet () {
  local workbook="$1" sheet="$2" state
  state=$(_cellar_state "$workbook")
  printf '%s\n' "$sheet" >> "$state/sheets"
  : > "$state/cells/$sheet"
  _cellar_write "$workbook"
}

# cellar_active <workbook-file> <sheet>
cellar_active () {
  local workbook="$1" state
  state=$(_cellar_state "$workbook")
  printf '%s\n' "$2" > "$state/active"
  _cellar_write "$workbook"
}

# cellar_value <workbook-file> <sheet> <cell>
# What a cell holds, read back out of the file.
cellar_value () {
  CELLAR_SHEET="$2" CELLAR_NAME="$3" awk '
    BEGIN { sheet = ENVIRON["CELLAR_SHEET"]; name = ENVIRON["CELLAR_NAME"]
            opening = "    (\"" name "\" . \"" }
    $0 == "  (\"" sheet "\"" { here = 1; next }
    here && /^  \("/ { here = 0 }
    here && index($0, opening) == 1 {
      line = substr($0, length(opening) + 1)
      sub(/"\)+$/, "", line)
      gsub(/\\"/, "\"", line)
      gsub(/\\\\/, "\\", line)
      print line
      exit
    }
  ' "$1"
}

# cellar_holds <workbook-file> <sheet> <cell> <source>
# Whether a cell holds exactly this, which is what the assertions ask.
cellar_holds () {
  [ -f "$1" ] || return 1
  [ "$(cellar_value "$1" "$2" "$3")" = "$4" ]
}

# cellar_empty <workbook-file> <sheet> <cell>
cellar_empty () {
  [ -z "$(cellar_value "$1" "$2" "$3")" ]
}
