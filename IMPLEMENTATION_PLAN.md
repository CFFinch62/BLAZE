# BLAZE Implementation Plan
## BLAZing-fast Algorithmic Zone Environment (Pascal IDE)

**Author:** Chuck Finch — Fragillidae Software  
**Reference Design:** FORTE (Fortran IDE) — see `../FORTE`  
**Stack:** Python 3.10+, PyQt6, ptyprocess, pyte  
**Target:** Linux-first, portable to macOS/Windows

---

## Architecture Overview

```
BLAZE/
├── blaze/
│   ├── main.py                  # Entry point
│   ├── app.py                   # MainWindow (QMainWindow)
│   ├── browser/
│   │   └── file_browser.py      # Left-panel file tree
│   ├── config/
│   │   ├── pascal_detector.py   # Detect fpc/gpc compilers
│   │   ├── settings.py          # JSON settings (~/.config/blaze/)
│   │   ├── settings_dialog.py   # Tabbed preferences dialog
│   │   └── themes.py            # Dark/Light theme dataclasses + QSS
│   ├── editor/
│   │   ├── code_editor.py       # QPlainTextEdit + line number gutter
│   │   ├── find_replace.py      # Find/Replace dialog
│   │   ├── highlighter.py       # Pascal syntax highlighter
│   │   └── tab_widget.py        # Multi-tab editor container
│   └── terminal/
│       ├── pty_process.py       # PTY process wrapper (QThread)
│       └── terminal_widget.py   # pyte-based terminal display widget
├── examples/                    # Sample .pas programs
├── images/                      # blaze_icon.png
├── requirements.txt
├── run.sh / setup.sh
└── IMPLEMENTATION_PLAN.md
```

### Key Design Decisions vs FORTE

| Aspect | FORTE | BLAZE |
|--------|-------|-------|
| Language | Fortran (.f90/.f/.for) | Pascal (.pas/.pp/.p/.lpr) |
| Runtime model | Compile → Run | Compile → Run (identical) |
| Compiler detector | fortran_detector | pascal_detector |
| File extensions | .f90 .f95 .f03 .f .for .f77 | .pas .pp .p .lpr .inc |
| Tab width default | 3 | 2 (Pascal convention) |
| Settings dir | ~/.config/forte | ~/.config/blaze |
| Build system | gfortran | fpc / gpc |
| Case sensitivity | Case-insensitive | Case-insensitive |
| Comment style | `!` line | `//` line + `{ }` + `(* *)` block |
| String delimiter | `'` and `"` | `'` only (standard Pascal) |
| Clean artifacts | binary only | binary + .o + .ppu |

---

## Phase Checklist

| Phase | Title | Status |
|-------|-------|--------|
| 1 | Project Bootstrap | ☐ |
| 2 | Config Layer | ☐ |
| 3 | File Browser | ☐ |
| 4 | Editor Layer | ☐ |
| 5 | Terminal Widget | ☐ |
| 6 | Build System | ☐ |
| 7 | Main Window | ☐ |
| 8 | Examples & Polish | ☐ |
| 9 | Packaging | ☐ |

Mark each phase ☑ when all tasks pass acceptance criteria.

---

## Phase 1 — Project Bootstrap

**Goal:** The project runs and shows a placeholder window.

### Tasks

- [ ] 1.1 Verify `blaze/main.py` launches cleanly with `python3 -m blaze.main`
- [ ] 1.2 Confirm `setup.sh` creates venv and installs `requirements.txt` without errors
- [ ] 1.3 Confirm `run.sh` activates venv and launches the app
- [ ] 1.4 Placeholder `MainWindow` appears (title "BLAZE - Pascal Teaching Environment", 1024×768)
- [ ] 1.5 Create `images/` directory; add placeholder `blaze_icon.png`

### Files

| File | Action |
|------|--------|
| `blaze/main.py` | Create — copy FORTE/forte/main.py; rename forte→blaze |
| `blaze/app.py` | Create — stub placeholder window |
| `blaze/__init__.py` | Create — `__version__ = "0.1.0"` |
| `setup.sh` | Create — copy FORTE/setup.sh; rename forte→blaze |
| `run.sh` | Create — copy FORTE/run.sh; rename forte→blaze |
| `requirements.txt` | Create — PyQt6, ptyprocess, pyte, pyinstaller |
| `images/blaze_icon.png` | Create (64×64 PNG placeholder; replace in Phase 8) |

### Acceptance Criteria

- `./run.sh` shows a window titled "BLAZE - Pascal Teaching Environment"
- No import errors in the terminal

---

## Phase 2 — Config Layer

**Goal:** Settings, themes, and compiler detection all work correctly.

### Tasks

#### 2a — `blaze/config/settings.py`
- [ ] 2a.1 Copy `FORTE/forte/config/settings.py` as starting point
- [ ] 2a.2 Change config dir to `~/.config/blaze/`
- [ ] 2a.3 Update "build" section defaults:
  ```python
  "build": {
      "compiler_path": "/usr/bin/fpc",
      "compiler_flags": "-O2 -gl",
      "output_dir": "."
  }
  ```
- [ ] 2a.4 Change `"fortran_filter"` → `"pascal_filter": True`
- [ ] 2a.5 Set default `"tab_width": 2`
- [ ] 2a.6 Write unit test: settings load/save round-trip, defaults applied

#### 2b — `blaze/config/themes.py`
- [ ] 2b.1 Copy `FORTE/forte/config/themes.py` as starting point
- [ ] 2b.2 Rename color fields:
  - `intrinsic` → `builtin` (WriteLn, ReadLn, Length, etc.)
  - `preprocessor` → `directive` (`{$IFDEF}`, `{$DEFINE}`, `{$INCLUDE}`, etc.)
- [ ] 2b.3 Dark theme: type_keyword `#94e2d5`, builtin `#89dceb`, directive `#f5c2e7`
- [ ] 2b.4 Light theme: type_keyword `#179299`, builtin `#04a5e5`, directive `#ea76cb`
- [ ] 2b.5 Update `apply_theme_to_app()` QSS — same structure as FORTE

#### 2c — `blaze/config/pascal_detector.py`
- [ ] 2c.1 Copy `FORTE/forte/config/fortran_detector.py` as starting point
- [ ] 2c.2 Rename: `PascalCompiler` dataclass with `name`, `path`, `version`
- [ ] 2c.3 `KNOWN_COMPILERS` in preference order:
  ```python
  ("fpc",     "Free Pascal Compiler", ["--version"], <parser>),
  ("gpc",     "GNU Pascal Compiler",  ["--version"], <parser>),
  ("ppcx64",  "Free Pascal (x64)",    ["--version"], <parser>),
  ("ppci386", "Free Pascal (i386)",   ["--version"], <parser>),
  ```
- [ ] 2c.4 Implement `detect_pascal_compilers()`, `get_default_compiler()`, `is_valid_compiler()`
- [ ] 2c.5 Write unit test: mock `shutil.which`, verify detection list

#### 2d — `blaze/config/settings_dialog.py`
- [ ] 2d.1 Copy `FORTE/forte/config/settings_dialog.py` as starting point
- [ ] 2d.2 Rename all "fortran" → "pascal", update imports
- [ ] 2d.3 **Editor tab:** font family, font size, tab width (default 2), show line numbers
- [ ] 2d.4 **Build tab:** compiler dropdown (from `pascal_detector`), custom path,
           compiler flags, output directory with Browse button
- [ ] 2d.5 **Appearance tab:** dark/light theme combo

### Acceptance Criteria

- `Settings()` creates `~/.config/blaze/settings.json` with correct defaults on first run
- `get_theme("dark")` returns a `Theme` object with all fields populated
- `detect_pascal_compilers()` returns at least `fpc` if installed
- Preferences dialog opens; all three tabs render; settings save/load correctly

---

## Phase 3 — File Browser

**Goal:** Left-panel file tree filters Pascal files; double-click opens in editor.

### Tasks

- [ ] 3.1 Copy `FORTE/forte/browser/file_browser.py` as starting point
- [ ] 3.2 Rename filter class `PascalFileFilterProxy`
- [ ] 3.3 Accepted extensions: `.pas`, `.pp`, `.p`, `.lpr`, `.lpk`, `.inc`, `.dpr`, `.dpk`
- [ ] 3.4 Setting key: `browser → pascal_filter`
- [ ] 3.5 Context menu: New File → default name `untitled.pas`
- [ ] 3.6 Emit `file_selected(str)` signal on double-click
- [ ] 3.7 Persist `last_directory` in settings

### Acceptance Criteria

- File tree opens to last-used directory on startup
- Non-Pascal files hidden when filter is enabled
- Double-clicking a `.pas` file emits `file_selected`
- Context menu: New File, New Folder, Rename, Delete all functional

---

## Phase 4 — Editor Layer

**Goal:** Syntax-highlighted, multi-tab code editor with line numbers and find/replace.

### Tasks

#### 4a — `blaze/editor/highlighter.py`
- [ ] 4a.1 Copy `FORTE/forte/editor/highlighter.py` structure as starting point
- [ ] 4a.2 Name class `PascalHighlighter(QSyntaxHighlighter)`
- [ ] 4a.3 Use `QRegularExpression.PatternOption.CaseInsensitiveOption` for ALL word rules
- [ ] 4a.4 Implement rule groups in order:
  1. **Numbers** — integers, hex `$FF`, reals `3.14`, exponents
  2. **Type keywords** — `INTEGER`, `REAL`, `BOOLEAN`, `CHAR`, `STRING`, `BYTE`,
     `WORD`, `LONGINT`, `CARDINAL`, `SHORTINT`, `SINGLE`, `DOUBLE`, `EXTENDED`,
     `ARRAY`, `RECORD`, `SET`, `FILE`, `POINTER`, `OBJECT`, `CLASS`, `INTERFACE`
  3. **Control keywords** — `PROGRAM`, `UNIT`, `USES`, `CONST`, `TYPE`, `VAR`,
     `PROCEDURE`, `FUNCTION`, `BEGIN`, `END`, `IF`, `THEN`, `ELSE`, `FOR`,
     `TO`, `DOWNTO`, `WHILE`, `DO`, `REPEAT`, `UNTIL`, `CASE`, `OF`,
     `WITH`, `LABEL`, `GOTO`, `PACKED`, `NIL`, `AND`, `OR`, `NOT`,
     `DIV`, `MOD`, `IN`, `SHL`, `SHR`, `XOR`, `IMPLEMENTATION`,
     `INITIALIZATION`, `FINALIZATION`, `INHERITED`, `OVERRIDE`,
     `VIRTUAL`, `ABSTRACT`, `PROPERTY`, `READ`, `WRITE`
  4. **Built-in routines** — `WRITELN`, `WRITE`, `READLN`, `READ`, `LENGTH`,
     `POS`, `COPY`, `DELETE`, `INSERT`, `CONCAT`, `STR`, `VAL`, `UPCASE`,
     `LOWERCASE`, `TRIM`, `SUCC`, `PRED`, `ORD`, `CHR`, `HIGH`, `LOW`,
     `INC`, `DEC`, `NEW`, `DISPOSE`, `ASSIGN`, `RESET`, `REWRITE`,
     `CLOSE`, `EOF`, `EOLN`, `HALT`, `EXIT`, `BREAK`, `CONTINUE`,
     `SIZEOF`, `ABS`, `SQR`, `SQRT`, `SIN`, `COS`, `ARCTAN`,
     `EXP`, `LN`, `TRUNC`, `ROUND`, `FRAC`, `ODD`, `RANDOM`, `RANDOMIZE`
  5. **Compiler directives** — pattern `\{\$[^\}]*\}` → directive color
  6. **Strings** — `'([^']|'')*'` (single-quoted; `''` escape)
  7. **Block comments** — `\{[^\}]*\}` and `\(\*.*?\*\)` (multiline-aware)
  8. **Line comments** — `//.*` *(added last to override everything)*
- [ ] 4a.5 `highlightBlock()` with multiline support for `{ }` block comments
  using `previousBlockState()` / `setCurrentBlockState()`
- [ ] 4a.6 Manual test: open a `.pas` file and visually confirm correct coloring

#### 4b — `blaze/editor/code_editor.py`
- [ ] 4b.1 Copy `FORTE/forte/editor/code_editor.py` as starting point
- [ ] 4b.2 Replace `FortranHighlighter` → `PascalHighlighter`
- [ ] 4b.3 Default tab width 2 from settings
- [ ] 4b.4 Keep auto-indent on Return; keep line number gutter

#### 4c — `blaze/editor/tab_widget.py`
- [ ] 4c.1 Copy `FORTE/forte/editor/tab_widget.py`
- [ ] 4c.2 Import `from blaze.editor.code_editor import CodeEditor`
- [ ] 4c.3 `new_file()`: default tab label `"untitled.pas"`

#### 4d — `blaze/editor/find_replace.py`
- [ ] 4d.1 Copy `FORTE/forte/editor/find_replace.py` verbatim
- [ ] 4d.2 Update imports to use `blaze.*` namespace

### Acceptance Criteria

- Pascal keywords, types, builtins, directives, strings, comments, numbers in distinct colors
- Case-insensitive: `begin`, `BEGIN`, `Begin` all highlighted identically
- Block comments `{ }` and `(* *)` work correctly, including multiline
- Line numbers, multi-tab, auto-indent, Find/Replace all functional

---

## Phase 5 — Terminal Widget

**Goal:** Working PTY terminal (bash shell) in the lower panel.

### Tasks

- [ ] 5.1 Copy `FORTE/forte/terminal/pty_process.py` verbatim → `blaze/terminal/pty_process.py`
- [ ] 5.2 Copy `FORTE/forte/terminal/terminal_widget.py` → `blaze/terminal/terminal_widget.py`
- [ ] 5.3 Default command: system shell (`$SHELL` or `/bin/bash`) — no change from FORTE
- [ ] 5.4 Update all imports to `blaze.*` namespace

### Acceptance Criteria

- Terminal panel shows a working shell on startup
- Keyboard input, Ctrl+C, Ctrl+D, Tab completion all work
- Terminal resizes correctly; `clear()` and `restart()` work

---

## Phase 6 — Build System

**Goal:** Save → Compile → Run workflow with output in the terminal panel.

### Design

```
blaze/build/
    __init__.py
    build_manager.py     # Orchestrates compile + run (fpc/gpc)
```

FPC-specific: uses `-o<path>` (no space) and produces `.o` + `.ppu` intermediates.

### Tasks

- [ ] 6a.1 Create empty `blaze/build/__init__.py`
- [ ] 6b.1 Copy `FORTE/forte/build/build_manager.py` as starting point
- [ ] 6b.2 Adapt compiler command to fpc syntax:
  `[compiler_path] + flags.split() + [f'-o{output_path}', source_path]`
- [ ] 6b.3 `compile(source_path)` → QProcess, stream stdout/stderr to terminal
- [ ] 6b.4 `compile_and_run(source_path)` → chain compile → run on success
- [ ] 6b.5 `run(executable_path)` → write to terminal PTY
- [ ] 6b.6 Handle compile errors with coloured BLAZE message in terminal
- [ ] 6b.7 `clean()`: delete binary + matching `.o` and `.ppu` files
- [ ] 6b.8 Unit test: mock QProcess, verify `-o<path>` construction (deferred)

### Menu items (wired in Phase 7):

| Action | Shortcut | Calls |
|--------|----------|-------|
| Compile & Run | Ctrl+R | `build_manager.compile_and_run(current_file)` |
| Compile Only | Ctrl+B | `build_manager.compile(current_file)` |
| Run Last Build | Ctrl+Shift+R | `build_manager.run(last_binary)` |
| Clean | — | Delete binary + `.o` + `.ppu` |

### Acceptance Criteria

- Ctrl+R compiles a `.pas` file and runs it; output visible in terminal
- Compiler errors appear with file/line info (fpc format: `file.pas(5,1) Error: ...`)
- Ctrl+B compiles only; `clean()` removes binary AND `.o`/`.ppu` files

---

## Phase 7 — Main Window

**Goal:** Fully wired `MainWindow` with all panels, menus, toolbar, and status bar.

### Tasks

- [ ] 7.1 Copy `FORTE/forte/app.py` as starting point
- [ ] 7.2 Replace "fortran"/"FORTE" references with "pascal"/"BLAZE"
- [ ] 7.3 Update all imports to `blaze.*` namespace
- [ ] 7.4 Instantiate `BuildManager`; connect signals to status bar and terminal
- [ ] 7.5 **Layout:** same as FORTE — horizontal + vertical splitters, default sizes `[200,800]`/`[500,268]`
- [ ] 7.6 **File Menu:** New/Open/Save/Save As/Exit
  - Open filter: `"Pascal Files (*.pas *.pp *.p *.lpr *.lpk *.inc);;All Files (*)"`
  - Save As filter: `"Pascal Source (*.pas);;Pascal Program (*.lpr);;Include (*.inc);;All Files (*)"`
- [ ] 7.7 **Edit Menu:** Find/Replace (Ctrl+F), Preferences
- [ ] 7.8 **View Menu:** Show File Browser (checkable), Show Terminal (checkable)
- [ ] 7.9 **Build Menu:** Compile & Run (Ctrl+R), Compile Only (Ctrl+B), Run Last Build (Ctrl+Shift+R),
  Clean Build Output, Clear Terminal Output, Restart Terminal, Interrupt (Ctrl+Shift+C)
- [ ] 7.10 **Help Menu:** About BLAZE
- [ ] 7.11 **Toolbar:** New, Open, Save | Compile & Run, Compile Only
- [ ] 7.12 **Status bar:** "Ready" + cursor position (Ln/Col)
- [ ] 7.13 Wire `file_browser.file_selected` → `open_file_from_browser()`
- [ ] 7.14 Wire `editor_tabs.currentChanged` → `update_cursor_position()`
- [ ] 7.15 `apply_theme()` on startup and after preferences save
- [ ] 7.16 `closeEvent()`: save geometry, splitter state, terminate PTY
- [ ] 7.17 **About dialog:** "BLAZE — BLAZing-fast Algorithmic Zone Environment"

### Acceptance Criteria

- All menus present and functional; full file→build→terminal workflow works
- Window geometry / splitter positions persist across restarts
- Theme applies correctly; About dialog shows correct information

---

## Phase 8 — Examples & Polish

**Goal:** Include beginner-friendly example programs; fix any rough edges.

### Example Programs (`examples/`)
- [ ] 8a.1 `hello_world.pas` — `program Hello; begin WriteLn('Hello, World!'); end.`
- [ ] 8a.2 `variables.pas` — INTEGER, REAL, BOOLEAN, CHAR, STRING declarations
- [ ] 8a.3 `loops.pas` — FOR/TO, WHILE/DO, REPEAT/UNTIL
- [ ] 8a.4 `conditionals.pas` — IF/THEN/ELSE, CASE/OF
- [ ] 8a.5 `procedures.pas` — PROCEDURE, FUNCTION, VAR/CONST parameters
- [ ] 8a.6 `arrays.pas` — ARRAY[1..10] OF type; string arrays
- [ ] 8a.7 `records.pas` — RECORD, field access, WITH statement
- [ ] 8a.8 `fileio.pas` — TEXT file read/write: ASSIGN, RESET, REWRITE, CLOSE

### Polish Tasks
- [ ] 8b.1 Create proper `images/blaze_icon.png` (64×64 and 256×256)
- [ ] 8b.2 Ensure no hardcoded theme colors in `code_editor.py`
- [ ] 8b.3 Add `--version` CLI flag to `main.py`
- [ ] 8b.4 FPC error line parsing — highlight error lines in editor (stretch goal)
- [ ] 8b.5 Status bar build indicator: "Compiling…" / "Build succeeded" / "Build failed (N errors)"
- [ ] 8b.6 Unsaved file check before compile: auto-save or prompt
- [ ] 8b.7 Status bar message on `clean()` confirming removed files

### Acceptance Criteria

- All 8 example files compile cleanly with `fpc -O2 -Wall`
- No hardcoded colors; build status visible in status bar

---

## Phase 9 — Packaging

**Goal:** Distributable standalone binary via PyInstaller.

### Tasks

- [ ] 9.1 Create `BLAZE.spec` (reference: `FORTE/FORTE.spec`)
- [ ] 9.2 Include `images/` and `examples/` in datas
- [ ] 9.3 Test `pyinstaller BLAZE.spec` produces a working binary
- [ ] 9.4 Create `build.py` helper script (reference: `FORTE/build.py`)
- [ ] 9.5 Test built binary: window opens, --version works, assets in _internal/
- [ ] 9.6 Bare-machine test (deferred)

### Acceptance Criteria

- `python3 build.py` produces `dist/BLAZE` executable
- Binary runs standalone; all assets accessible

---

## Pascal-Specific Notes

### FPC Invocation
```bash
fpc -O2 -gl -o./hello hello.pas
# Produces: hello (binary), hello.o, hello.ppu (if UNIT)
```

### FPC Error Format
```
hello.pas(5,1) Error: Unknown identifier "x"
hello.pas(5,1) Fatal: There were 1 errors compiling module, stopping
```
Pattern: `<filename>(<line>,<col>) <severity>: <message>`

### Multiline Block Comments
`{ }` comments can span multiple lines. Use `previousBlockState()` /
`setCurrentBlockState()` in `highlightBlock()` to track open block comment state.

---

## Reference Files (by phase)

| Phase | Primary Reference | Notes |
|-------|------------------|-------|
| 1 | `FORTE/forte/main.py`, `setup.sh`, `run.sh` | Rename forte→blaze |
| 2a | `FORTE/forte/config/settings.py` | pascal_filter, tab_width=2, fpc path |
| 2b | `FORTE/forte/config/themes.py` | Rename intrinsic→builtin, preprocessor→directive |
| 2c | `FORTE/forte/config/fortran_detector.py` | Adapt for fpc/gpc |
| 2d | `FORTE/forte/config/settings_dialog.py` | Rename fortran→pascal |
| 3 | `FORTE/forte/browser/file_browser.py` | .pas extensions |
| 4a | `FORTE/forte/editor/highlighter.py` | Structure only; all rules new |
| 4b–4d | `FORTE/forte/editor/` | Copy & swap import namespaces |
| 5 | `FORTE/forte/terminal/` | Copy verbatim |
| 6 | `FORTE/forte/build/build_manager.py` | Adapt for fpc `-o<path>` syntax |
| 7 | `FORTE/forte/app.py` | Adapt menus/filters |
| 9 | `FORTE/FORTE.spec`, `FORTE/build.py` | Adapt |

---

## Progress Log

| Date | Phase | Notes |
|------|-------|-------|
| 2026-02-23 | 0 | Implementation plan created (adapted from FORTE) |

---

*BLAZE — BLAZing-fast Algorithmic Zone Environment (Pascal IDE)*  
*(c) Fragillidae Software — Chuck Finch*
