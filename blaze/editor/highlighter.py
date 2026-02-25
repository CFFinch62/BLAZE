# blaze/editor/highlighter.py - Pascal syntax highlighter

from __future__ import annotations

from PyQt6.QtGui import QSyntaxHighlighter, QTextCharFormat, QColor, QFont
from PyQt6.QtCore import QRegularExpression

from blaze.config.themes import Theme, get_theme

# Block-comment state constants
_STATE_NORMAL = 0
_STATE_IN_BRACE_COMMENT = 1    # inside {  }
_STATE_IN_PAREN_COMMENT = 2    # inside (* *)


class PascalHighlighter(QSyntaxHighlighter):
    """Syntax highlighter for Pascal / Free Pascal / Delphi source code.

    Rules are applied in order; later rules override earlier ones on the same
    span of text.  Block comments and line comments are handled last so they
    always win.  All keyword rules are case-insensitive.
    """

    def __init__(self, document, theme: Theme | None = None):
        super().__init__(document)
        self.highlighting_rules: list[tuple] = []
        self._theme: Theme = theme or get_theme("dark")
        self._build_rules(self._theme)

    def set_theme(self, theme: Theme) -> None:
        """Rebuild highlighting rules for *theme* and re-colour the document."""
        self._theme = theme
        self._build_rules(theme)
        self.rehighlight()

    # ------------------------------------------------------------------
    # Rule construction
    # ------------------------------------------------------------------

    def _build_rules(self, theme: Theme) -> None:
        self.highlighting_rules.clear()
        ci = QRegularExpression.PatternOption.CaseInsensitiveOption

        # ── 1. Numbers ────────────────────────────────────────────────
        # Integers, hex ($FF), reals (3.14), exponents (1.5e-3)
        num_fmt = QTextCharFormat()
        num_fmt.setForeground(QColor(theme.number))
        self.highlighting_rules.append((
            QRegularExpression(
                r"\$[0-9A-Fa-f]+\b"                    # hex literal $FF
                r"|\b[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?\b"  # int / real / exp
            ),
            num_fmt,
        ))

        # ── 2. Type-specifier keywords ────────────────────────────────
        type_fmt = QTextCharFormat()
        type_fmt.setForeground(QColor(theme.type_keyword))
        for word in [
            "INTEGER", "REAL", "BOOLEAN", "CHAR", "STRING", "BYTE",
            "WORD", "LONGINT", "CARDINAL", "SHORTINT", "SINGLE",
            "DOUBLE", "EXTENDED", "ARRAY", "RECORD", "SET", "FILE",
            "POINTER", "OBJECT", "CLASS", "INTERFACE", "TEXT",
            "ANSISTRING", "WIDESTRING", "PCHAR", "LONGWORD",
            "INT64", "QWORD", "SMALLINT",
        ]:
            self.highlighting_rules.append((
                QRegularExpression(rf"\b{word}\b", ci), type_fmt
            ))

        # ── 3. Control / program-structure keywords (bold) ───────────
        kw_fmt = QTextCharFormat()
        kw_fmt.setForeground(QColor(theme.keyword))
        kw_fmt.setFontWeight(QFont.Weight.Bold)
        for word in [
            "PROGRAM", "UNIT", "USES", "CONST", "TYPE", "VAR",
            "PROCEDURE", "FUNCTION", "BEGIN", "END", "IF", "THEN",
            "ELSE", "FOR", "TO", "DOWNTO", "WHILE", "DO", "REPEAT",
            "UNTIL", "CASE", "OF", "WITH", "LABEL", "GOTO", "PACKED",
            "NIL", "AND", "OR", "NOT", "DIV", "MOD", "IN", "SHL",
            "SHR", "XOR", "IMPLEMENTATION", "INTERFACE", "INITIALIZATION",
            "FINALIZATION", "INHERITED", "OVERRIDE", "VIRTUAL", "ABSTRACT",
            "PROPERTY", "READ", "WRITE", "PUBLISHED", "PROTECTED",
            "PRIVATE", "PUBLIC", "FORWARD", "EXTERNAL", "INLINE",
            "OPERATOR", "AS", "IS", "ON", "EXCEPT", "TRY", "FINALLY",
            "RAISE", "AT", "CONSTRUCTOR", "DESTRUCTOR", "SELF",
            "RESULT", "TRUE", "FALSE",
        ]:
            self.highlighting_rules.append((
                QRegularExpression(rf"\b{word}\b", ci), kw_fmt
            ))

        # ── 4. Built-in routines ──────────────────────────────────────
        builtin_fmt = QTextCharFormat()
        builtin_fmt.setForeground(QColor(theme.builtin))
        for word in [
            "WRITELN", "WRITE", "READLN", "READ", "LENGTH",
            "POS", "COPY", "DELETE", "INSERT", "CONCAT", "STR", "VAL",
            "UPCASE", "LOWERCASE", "TRIM", "SUCC", "PRED", "ORD", "CHR",
            "HIGH", "LOW", "INC", "DEC", "NEW", "DISPOSE",
            "ASSIGN", "RESET", "REWRITE", "CLOSE", "EOF", "EOLN",
            "HALT", "EXIT", "BREAK", "CONTINUE",
            "SIZEOF", "TYPEOF", "ABS", "SQR", "SQRT", "SIN", "COS",
            "ARCTAN", "EXP", "LN", "TRUNC", "ROUND", "FRAC",
            "ODD", "RANDOM", "RANDOMIZE", "SWAP",
            "FILLCHAR", "MOVE", "BLOCKREAD", "BLOCKWRITE",
            "SEEK", "FILEPOS", "FILESIZE", "FLUSH",
            "GETTIME", "GETDATE", "PARAMCOUNT", "PARAMSTR",
            "ALLOCATED", "FREEMEM", "GETMEM",
            "SETLENGTH", "SETSTRING",
        ]:
            self.highlighting_rules.append((
                QRegularExpression(rf"\b{word}\b", ci), builtin_fmt
            ))

        # ── 5. Compiler directives  {$…} ─────────────────────────────
        # Must be matched before block-comment rule  { }
        dir_fmt = QTextCharFormat()
        dir_fmt.setForeground(QColor(theme.directive))
        self.highlighting_rules.append((
            QRegularExpression(r"\{\$[^}]*\}"),
            dir_fmt,
        ))

        # ── 6. String literals  '…' ('' is an escaped quote inside) ──
        str_fmt = QTextCharFormat()
        str_fmt.setForeground(QColor(theme.string))
        self.highlighting_rules.append((
            QRegularExpression(r"'([^']|'')*'"),
            str_fmt,
        ))

        # NOTE: Block comments ({ } and (* *)) and line comments (//)
        # are handled in highlightBlock() with state tracking so that
        # multi-line { } blocks work correctly.

        # Store pre-built formats for use in highlightBlock()
        self._comment_fmt = QTextCharFormat()
        self._comment_fmt.setForeground(QColor(theme.comment))

    # ------------------------------------------------------------------
    # Qt override — handles per-line highlighting + multiline comments
    # ------------------------------------------------------------------

    def highlightBlock(self, text: str) -> None:
        # Apply inline rules first (numbers, keywords, strings, directives)
        for pattern, fmt in self.highlighting_rules:
            it = pattern.globalMatch(text)
            while it.hasNext():
                match = it.next()
                self.setFormat(match.capturedStart(), match.capturedLength(), fmt)

        # ── Multiline { } block comment state machine ─────────────────
        prev_state = self.previousBlockState()
        in_brace = (prev_state == _STATE_IN_BRACE_COMMENT)
        in_paren = (prev_state == _STATE_IN_PAREN_COMMENT)

        i = 0
        start = 0

        while i < len(text):
            ch = text[i]

            if in_brace:
                if ch == "}":
                    # End of brace comment
                    self.setFormat(start, i - start + 1, self._comment_fmt)
                    in_brace = False
                    i += 1
                else:
                    i += 1
            elif in_paren:
                if text[i:i+2] == "*)":
                    # End of paren comment
                    self.setFormat(start, i - start + 2, self._comment_fmt)
                    in_paren = False
                    i += 2
                else:
                    i += 1
            else:
                # Not currently in a block comment
                if text[i:i+2] == "//":
                    # Line comment — colour rest of line and stop
                    self.setFormat(i, len(text) - i, self._comment_fmt)
                    break
                elif ch == "{" and (i + 1 >= len(text) or text[i+1] != "$"):
                    # Opening brace comment (not a directive {$ )
                    in_brace = True
                    start = i
                    i += 1
                elif text[i:i+2] == "(*":
                    # Opening paren comment
                    in_paren = True
                    start = i
                    i += 2
                else:
                    i += 1

        # If we ended in an open comment, mark the rest of the line and
        # set the carry-over state for the next block.
        if in_brace:
            self.setFormat(start, len(text) - start, self._comment_fmt)
            self.setCurrentBlockState(_STATE_IN_BRACE_COMMENT)
        elif in_paren:
            self.setFormat(start, len(text) - start, self._comment_fmt)
            self.setCurrentBlockState(_STATE_IN_PAREN_COMMENT)
        else:
            self.setCurrentBlockState(_STATE_NORMAL)
