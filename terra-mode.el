;;; terra-mode.el --- a major-mode for editing Terra scripts

;; Based on lua-mode.

;; Author: 2011-2013 immerrr <immerrr+lua@gmail.com>
;;         2010-2011 Reuben Thomas <rrt@sc3d.org>
;;         2006 Juergen Hoetzel <juergen@hoetzel.info>
;;         2004 various (support for Lua 5 and byte compilation)
;;         2001 Christian Vogler <cvogler@gradient.cis.upenn.edu>
;;         1997 Bret Mogilefsky <mogul-lua@gelatinous.com> starting from
;;              tcl-mode by Gregor Schmid <schmid@fb3-s7.math.tu-berlin.de>
;;              with tons of assistance from
;;              Paul Du Bois <pld-lua@gelatinous.com> and
;;              Aaron Smith <aaron-lua@gelatinous.com>.
;;
;; URL:         http://immerrr.github.com/lua-mode
;; Version:     20151025
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;; Keywords: languages, processes, tools

;; This field is expanded to commit SHA, date & associated heads/tags during
;; archive creation.
;; Revision: $Format:%h (%cD %d)$
;;

;;; Commentary:

;; terra-mode provides support for editing Terra, including automatical
;; indentation, syntactical font-locking, running interactive shell,
;; interacting with `hs-minor-mode' and online documentation lookup.

;; The following variables are available for customization (see more via
;; `M-x customize-group terra`):

;; - Var `terra-indent-level':
;;   indentation offset in spaces
;; - Var `terra-indent-string-contents':
;;   set to `t` if you like to have contents of multiline strings to be
;;   indented like comments
;; - Var `terra-mode-hook':
;;   list of functions to execute when terra-mode is initialized
;; - Var `terra-documentation-url':
;;   base URL for documentation lookup
;; - Var `terra-documentation-function': function used to
;;   show documentation (`eww` is a viable alternative for Emacs 25)

;; These are variables/commands that operate on Terra subprocess:

;; - Var `terra-default-application':
;;   command to start up the subprocess (REPL)
;; - Var `terra-default-command-switches':
;;   arguments to pass to the subprocess on startup (make sure `-i` is there
;;   if you expect working with Terra shell interactively)
;; - Cmd `terra-start-process': start new REPL process, usually happens automatically
;; - Cmd `terra-kill-process': kill current REPL process

;; These are variables/commands for interaction with Terra subprocess:

;; - Cmd `terra-show-process-buffer': switch to REPL buffer
;; - Cmd `terra-hide-process-buffer': hide window showing REPL buffer
;; - Var `terra-always-show': show REPL buffer after sending something
;; - Cmd `terra-send-buffer': send whole buffer
;; - Cmd `terra-send-current-line': send current line
;; - Cmd `terra-send-defun': send current top-level function
;; - Cmd `terra-send-region': send active region
;; - Cmd `terra-restart-with-whole-file': restart REPL and send whole buffer

;; See "M-x apropos-command ^terra-" for a list of commands.
;; See "M-x customize-group terra" for a list of customizable variables.


;;; Code:
(eval-when-compile
  (require 'cl))

(require 'comint)
(require 'newcomment)
(require 'rx)


;; rx-wrappers for Terra

(eval-when-compile
  ;; Silence compilation warning about `compilation-error-regexp-alist' defined
  ;; in compile.el.
  (require 'compile))

(eval-and-compile
  (defvar terra-rx-constituents)
  (defvar rx-parent)

  (defun terra-rx-to-string (form &optional no-group)
    "Terra-specific replacement for `rx-to-string'.

See `rx-to-string' documentation for more information FORM and
NO-GROUP arguments."
    (let ((rx-constituents terra-rx-constituents))
      (rx-to-string form no-group)))

  (defmacro terra-rx (&rest regexps)
    "Terra-specific replacement for `rx'.

See `rx' documentation for more information about REGEXPS param."
    (cond ((null regexps)
           (error "No regexp"))
          ((cdr regexps)
           (terra-rx-to-string `(and ,@regexps) t))
          (t
           (terra-rx-to-string (car regexps) t))))

  (defun terra--new-rx-form (form)
    "Add FORM definition to `terra-rx' macro.

FORM is a cons (NAME . DEFN), see more in `rx-constituents' doc.
This function enables specifying new definitions using old ones:
if DEFN is a list that starts with `:rx' symbol its second
element is itself expanded with `terra-rx-to-string'. "
    (let ((name (car form))
          (form-definition (cdr form)))
      (when (and (listp form-definition) (eq ':rx (car form-definition)))
        (setcdr form (terra-rx-to-string (cadr form-definition) 'nogroup)))
      (push form terra-rx-constituents)))

  (defun terra--rx-symbol (form)
    ;; form is a list (symbol XXX ...)
    ;; Skip initial 'symbol
    (setq form (cdr form))
    ;; If there's only one element, take it from the list, otherwise wrap the
    ;; whole list into `(or XXX ...)' form.
    (setq form (if (eq 1 (length form))
                   (car form)
                 (append '(or) form)))
    (rx-form `(seq symbol-start ,form symbol-end) rx-parent))

  (setq terra-rx-constituents (copy-sequence rx-constituents))

  ;; group-n is not available in Emacs23, provide a fallback.
  (unless (assq 'group-n rx-constituents)
    (defun terra--rx-group-n (form)
      (concat (format "\\(?%d:" (nth 1 form))
              (rx-form `(seq ,@(nthcdr 2 form)) ':)
              "\\)"))
    (push '(group-n terra--rx-group-n 1 nil) terra-rx-constituents))

  (mapc #'terra--new-rx-form
        `((symbol terra--rx-symbol 1 nil)
          (ws . "[ \t]*") (ws+ . "[ \t]+")
          (terra-name :rx (symbol (regexp "[[:alpha:]_]+[[:alnum:]_]*")))
          (terra-funcname
           :rx (seq terra-name (* ws "." ws terra-name)
                    (opt ws ":" ws terra-name)))
          (terra-funcheader
           ;; Outer (seq ...) is here to shy-group the definition
           :rx (seq (or (seq (symbol "function"

                                     ;; Terra keywords
                                     "struct"
                                     "terra")
                             ws (group-n 1 terra-funcname))
                        (seq (group-n 1 terra-funcname) ws "=" ws
                             (symbol "function"

                                     ;; Terra keywords
                                     "struct"
                                     "terra")))))
          (terra-number
           :rx (seq (or (seq (+ digit) (opt ".") (* digit))
                        (seq (* digit) (opt ".") (+ digit)))
                    (opt (regexp "[eE][+-]?[0-9]+"))))
          (terra-assignment-op
           :rx (seq "=" (or buffer-end (not (any "=")))))
          (terra-token
           :rx (or "+" "-" "*" "/" "%" "^" "#" "==" "~=" "<=" ">=" "<"
                   ">" "=" ";" ":" "," "." ".." "..."

                   ;; Terra tokens
                   "`" "@"))
          (terra-keyword
           :rx (symbol "and" "break" "do" "else" "elseif" "end"  "for" "function"
                       "goto" "if" "in" "local" "not" "or" "repeat" "return"
                       "then" "until" "while"

                       ;; Terra keywords
                       "defer"
                       "escape"
                       "emit"
                       "import"
                       "quote"
                       "struct"
                       "terra"
                       "var")))
        ))


(eval-and-compile
  (if (fboundp 'setq-local)
      (defalias 'terra--setq-local 'setq-local)
    (defmacro terra--setq-local (var val)
      `(set (make-local-variable (quote ,var)) ,val)))

  ;; Backward compatibility for Emacsen < 24.1
  (unless (fboundp 'prog-mode)
    (define-derived-mode prog-mode fundamental-mode "Prog"))

  (defalias 'terra--cl-assert
    (if (fboundp 'cl-assert) 'cl-assert 'assert))

  (defalias 'terra--cl-labels
    (if (fboundp 'cl-labels) 'cl-labels 'flet))

  ;; backward compatibility for Emacsen < 23.3
  ;; Emacs 23.3 introduced with-silent-modifications macro
  (if (fboundp 'with-silent-modifications)
      (defalias 'terra--with-silent-modifications 'with-silent-modifications)

    (defmacro terra--with-silent-modifications (&rest body)
      "Execute BODY, pretending it does not modifies the buffer.

This is a reimplementation of macro `with-silent-modifications'
for Emacsen that doesn't contain one (pre-23.3)."
      `(let ((old-modified-p (buffer-modified-p))
            (inhibit-modification-hooks t)
            (buffer-undo-list t))

        (unwind-protect
            ,@body
          (set-buffer-modified-p old-modified-p))))))

;; Local variables
(defgroup terra nil
  "Major mode for editing terra code."
  :prefix "terra-"
  :group 'languages)

(defcustom terra-indent-level 2
  "Amount by which Terra subexpressions are indented."
  :type 'integer
  :group 'terra
  :safe #'integerp)

(defcustom terra-comment-start "-- "
  "Default value of `comment-start'."
  :type 'string
  :group 'terra)

(defcustom terra-comment-start-skip "---*[ \t]*"
  "Default value of `comment-start-skip'."
  :type 'string
  :group 'terra)

(defcustom terra-default-application "terra"
  "Default application to run in terra subprocess."
  :type 'string
  :group 'terra)

(defcustom terra-default-command-switches (list "-i")
  "Command switches for `terra-default-application'.
Should be a list of strings."
  :type '(repeat string)
  :group 'terra)
(make-variable-buffer-local 'terra-default-command-switches)

(defcustom terra-always-show t
  "*Non-nil means display terra-process-buffer after sending a command."
  :type 'boolean
  :group 'terra)

(defcustom terra-documentation-function 'browse-url
  "Function used to fetch the Terra reference manual."
  :type `(radio (function-item browse-url)
                ,@(when (fboundp 'eww) '((function-item eww)))
                ,@(when (fboundp 'w3m-browse-url) '((function-item w3m-browse-url)))
                (function :tag "Other function"))
  :group 'terra)

(defcustom terra-documentation-url
  (or (and (file-readable-p "/usr/share/doc/lua/manual.html")
           "file:///usr/share/doc/lua/manual.html")
      "http://www.lua.org/manual/5.1/manual.html")
  "URL pointing to the Lua reference manual."
  :type 'string
  :group 'terra)


(defvar terra-process nil
  "The active Terra subprocess")

(defvar terra-process-buffer nil
  "Buffer used for communication with Terra subprocess")

(defun terra--customize-set-prefix-key (prefix-key-sym prefix-key-val)
  (terra--cl-assert (eq prefix-key-sym 'terra-prefix-key))
  (set prefix-key-sym (if (and prefix-key-val (> (length prefix-key-val) 0))
                          ;; read-kbd-macro returns a string or a vector
                          ;; in both cases (elt x 0) is ok
                          (elt (read-kbd-macro prefix-key-val) 0)))
  (if (fboundp 'terra-prefix-key-update-bindings)
      (terra-prefix-key-update-bindings)))

(defcustom terra-prefix-key "\C-c"
  "Prefix for all terra-mode commands."
  :type 'string
  :group 'terra
  :set 'terra--customize-set-prefix-key
  :get '(lambda (sym)
          (let ((val (eval sym))) (if val (single-key-description (eval sym)) ""))))

(defvar terra-mode-menu (make-sparse-keymap "Terra")
  "Keymap for terra-mode's menu.")

(defvar terra-prefix-mode-map
  (eval-when-compile
    (let ((result-map (make-sparse-keymap)))
      (mapc (lambda (key_defn)
              (define-key result-map (read-kbd-macro (car key_defn)) (cdr key_defn)))
            '(("C-l" . terra-send-buffer)
              ("C-f" . terra-search-documentation)))
      result-map))
  "Keymap that is used to define keys accessible by `terra-prefix-key'.

If the latter is nil, the keymap translates into `terra-mode-map' verbatim.")

(defvar terra--electric-indent-chars
  (mapcar #'string-to-char '("}" "]" ")")))


(defvar terra-mode-map
  (let ((result-map (make-sparse-keymap))
        prefix-key)
    (unless (boundp 'electric-indent-chars)
      (mapc (lambda (electric-char)
              (define-key result-map
                (read-kbd-macro
                 (char-to-string electric-char))
                #'terra-electric-match))
            terra--electric-indent-chars))
    (define-key result-map [menu-bar terra-mode] (cons "Terra" terra-mode-menu))

    ;; FIXME: see if the declared logic actually works
    ;; handle prefix-keyed bindings:
    ;; * if no prefix, set prefix-map as parent, i.e.
    ;;      if key is not defined look it up in prefix-map
    ;; * if prefix is set, bind the prefix-map to that key
    (if (boundp 'terra-prefix-key)
        (define-key result-map (vector terra-prefix-key) terra-prefix-mode-map)
      (set-keymap-parent result-map terra-prefix-mode-map))
    result-map)
  "Keymap used in terra-mode buffers.")

(defvar terra-electric-flag t
  "If t, electric actions (like automatic reindentation) will happen when an electric
 key like `{' is pressed")
(make-variable-buffer-local 'terra-electric-flag)

(defcustom terra-prompt-regexp "[^\n]*\\(>[\t ]+\\)+$"
  "Regexp which matches the Terra program's prompt."
  :type  'regexp
  :group 'terra)

(defcustom terra-traceback-line-re
  ;; This regexp skips prompt and meaningless "stdin:N:" prefix when looking
  ;; for actual file-line locations.
  "^\\(?:[\t ]*\\|.*>[\t ]+\\)\\(?:[^\n\t ]+:[0-9]+:[\t ]*\\)*\\(?:\\([^\n\t ]+\\):\\([0-9]+\\):\\)"
  "Regular expression that describes tracebacks and errors."
  :type 'regexp
  :group 'terra)

(defvar terra--repl-buffer-p nil
  "Buffer-local flag saying if this is a Terra REPL buffer.")
(make-variable-buffer-local 'terra--repl-buffer-p)


(defadvice compilation-find-file (around terra--repl-find-file
                                         (marker filename directory &rest formats)
                                         activate)
  "Return Terra REPL buffer when looking for \"stdin\" file in it."
  (if (and
       terra--repl-buffer-p
       (string-equal filename "stdin")
       ;; NOTE: this doesn't traverse `compilation-search-path' when
       ;; looking for filename.
       (not (file-exists-p (expand-file-name
                        filename
                        (when directory (expand-file-name directory))))))
      (setq ad-return-value (current-buffer))
    ad-do-it))


(defadvice compilation-goto-locus (around terra--repl-goto-locus
                                          (msg mk end-mk)
                                          activate)
  "When message points to Terra REPL buffer, go to the message itself.
Usually, stdin:XX line number points to nowhere."
  (let ((errmsg-buf (marker-buffer msg))
        (error-buf (marker-buffer mk)))
    (if (and (with-current-buffer errmsg-buf terra--repl-buffer-p)
             (eq error-buf errmsg-buf))
        (progn
          (compilation-set-window (display-buffer (marker-buffer msg)) msg)
          (goto-char msg))
      ad-do-it)))


(defcustom terra-indent-string-contents nil
  "If non-nil, contents of multiline string will be indented.
Otherwise leading amount of whitespace on each line is preserved."
  :group 'terra
  :type 'boolean)

(defcustom terra-jump-on-traceback t
  "*Jump to innermost traceback location in *terra* buffer.  When this
variable is non-nil and a traceback occurs when running Terra code in a
subprocess, jump immediately to the source code of the innermost
traceback location."
  :type 'boolean
  :group 'terra)

(defcustom terra-mode-hook nil
  "Hooks called when Terra mode fires up."
  :type 'hook
  :group 'terra)

(defvar terra-region-start (make-marker)
  "Start of special region for Terra communication.")

(defvar terra-region-end (make-marker)
  "End of special region for Terra communication.")

(defvar terra-emacs-menu
  '(["Restart With Whole File" terra-restart-with-whole-file t]
    ["Kill Process" terra-kill-process t]
    ["Hide Process Buffer" terra-hide-process-buffer t]
    ["Show Process Buffer" terra-show-process-buffer t]
    ["Beginning Of Proc" terra-beginning-of-proc t]
    ["End Of Proc" terra-end-of-proc t]
    ["Set Terra-Region Start" terra-set-terra-region-start t]
    ["Set Terra-Region End" terra-set-terra-region-end t]
    ["Send Terra-Region" terra-send-terra-region t]
    ["Send Current Line" terra-send-current-line t]
    ["Send Region" terra-send-region t]
    ["Send Proc" terra-send-proc t]
    ["Send Buffer" terra-send-buffer t]
    ["Search Documentation" terra-search-documentation t])
  "Emacs menu for Terra mode.")

;; the whole defconst is inside eval-when-compile, because it's later referenced
;; inside another eval-and-compile block
(eval-and-compile
  (defconst
    terra--builtins
    (let*
        ((modules
          '("_G" "_VERSION" "assert" "collectgarbage" "dofile" "error" "getfenv"
            "getmetatable" "ipairs" "load" "loadfile" "loadstring" "module"
            "next" "pairs" "pcall" "print" "rawequal" "rawget" "rawlen" "rawset"
            "require" "select" "setfenv" "setmetatable" "tonumber" "tostring"
            "type" "unpack" "xpcall" "self"
            ("bit32" . ("arshift" "band" "bnot" "bor" "btest" "bxor" "extract"
                        "lrotate" "lshift" "replace" "rrotate" "rshift"))
            ("coroutine" . ("create" "isyieldable" "resume" "running" "status"
                            "wrap" "yield"))
            ("debug" . ("debug" "getfenv" "gethook" "getinfo" "getlocal"
                        "getmetatable" "getregistry" "getupvalue" "getuservalue"
                        "setfenv" "sethook" "setlocal" "setmetatable"
                        "setupvalue" "setuservalue" "traceback" "upvalueid"
                        "upvaluejoin"))
            ("io" . ("close" "flush" "input" "lines" "open" "output" "popen"
                     "read" "stderr" "stdin" "stdout" "tmpfile" "type" "write"))
            ("math" . ("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "cosh"
                       "deg" "exp" "floor" "fmod" "frexp" "huge" "ldexp" "log"
                       "log10" "max" "maxinteger" "min" "mininteger" "modf" "pi"
                       "pow" "rad" "random" "randomseed" "sin" "sinh" "sqrt"
                       "tan" "tanh" "tointeger" "type" "ult"))
            ("os" . ("clock" "date" "difftime" "execute" "exit" "getenv"
                     "remove"  "rename" "setlocale" "time" "tmpname"))
            ("package" . ("config" "cpath" "loaded" "loaders" "loadlib" "path"
                          "preload" "searchers" "searchpath" "seeall"))
            ("string" . ("byte" "char" "dump" "find" "format" "gmatch" "gsub"
                         "len" "lower" "match" "pack" "packsize" "rep" "reverse"
                         "sub" "unpack" "upper"))
            ("table" . ("concat" "insert" "maxn" "move" "pack" "remove" "sort"
                        "unpack"))
            ("utf8" . ("char" "charpattern" "codepoint" "codes" "len"
                       "offset")))))

      (terra--cl-labels
       ((module-name-re (x)
                        (concat "\\(?1:\\_<"
                                (if (listp x) (car x) x)
                                "\\_>\\)"))
        (module-members-re (x) (if (listp x)
                                   (concat "\\(?:[ \t]*\\.[ \t]*"
                                           "\\_<\\(?2:"
                                           (regexp-opt (cdr x))
                                           "\\)\\_>\\)?")
                                 "")))

       (concat
        ;; common prefix:
        ;; - beginning-of-line
        ;; - or neither of [ '.', ':' ] to exclude "foo.string.rep"
        ;; - or concatenation operator ".."
        "\\(?:^\\|[^:. \t]\\|[.][.]\\)"
        ;; optional whitespace
        "[ \t]*"
        "\\(?:"
        ;; any of modules/functions
        (mapconcat (lambda (x) (concat (module-name-re x)
                                       (module-members-re x)))
                   modules
                   "\\|")
        "\\)"))))

  "A regexp that matches terra builtin functions & variables.

This is a compilation of 5.1, 5.2 and 5.3 builtins taken from the
index of respective Lua reference manuals.")

(eval-and-compile
  (defun terra-make-delimited-matcher (elt-regexp sep-regexp end-regexp)
    "Construct matcher function for `font-lock-keywords' to match a sequence.

It's supposed to match sequences with following EBNF:

ELT-REGEXP { SEP-REGEXP ELT-REGEXP } END-REGEXP

The sequence is parsed one token at a time.  If non-nil is
returned, `match-data' will have one or more of the following
groups set according to next matched token:

1. matched element token
2. unmatched garbage characters
3. misplaced token (i.e. SEP-REGEXP when ELT-REGEXP is expected)
4. matched separator token
5. matched end token

Blanks & comments between tokens are silently skipped.
Groups 6-9 can be used in any of argument regexps."
    (lexical-let*
        ((delimited-matcher-re-template
          "\\=\\(?2:.*?\\)\\(?:\\(?%s:\\(?4:%s\\)\\|\\(?5:%s\\)\\)\\|\\(?%s:\\(?1:%s\\)\\)\\)")
         ;; There's some magic to this regexp. It works as follows:
         ;;
         ;; A. start at (point)
         ;; B. non-greedy match of garbage-characters (?2:)
         ;; C. try matching separator (?4:) or end-token (?5:)
         ;; D. try matching element (?1:)
         ;;
         ;; Simple, but there's a trick: pt.C and pt.D are embraced by one more
         ;; group whose purpose is determined only after the template is
         ;; formatted (?%s:):
         ;;
         ;; - if element is expected, then D's parent group becomes "shy" and C's
         ;;   parent becomes group 3 (aka misplaced token), so if D matches when
         ;;   an element is expected, it'll be marked with warning face.
         ;;
         ;; - if separator-or-end-token is expected, then it's the opposite:
         ;;   C's parent becomes shy and D's will be matched as misplaced token.
         (elt-expected-re (format delimited-matcher-re-template
                                  3 sep-regexp end-regexp "" elt-regexp))
         (sep-or-end-expected-re (format delimited-matcher-re-template
                                         "" sep-regexp end-regexp 3 elt-regexp)))

      (lambda (end)
        (let* ((prev-elt-p (match-beginning 1))
               (prev-sep-p (match-beginning 4))
               (prev-end-p (match-beginning 5))

               (regexp (if prev-elt-p sep-or-end-expected-re elt-expected-re))
               (comment-start (terra-comment-start-pos (syntax-ppss)))
               (parse-stop end))

          ;; If token starts inside comment, or end-token was encountered, stop.
          (when (and (not comment-start)
                     (not prev-end-p))
            ;; Skip all comments & whitespace. forward-comment doesn't have boundary
            ;; argument, so make sure point isn't beyond parse-stop afterwards.
            (while (and (< (point) end)
                        (forward-comment 1)))
            (goto-char (min (point) parse-stop))

            ;; Reuse comment-start variable to store beginning of comment that is
            ;; placed before line-end-position so as to make sure token search doesn't
            ;; enter that comment.
            (setq comment-start
                  (terra-comment-start-pos
                   (save-excursion
                     (parse-partial-sexp (point) parse-stop
                                         nil nil nil 'stop-inside-comment)))
                  parse-stop (or comment-start parse-stop))

            ;; Now, let's match stuff.  If regular matcher fails, declare a span of
            ;; non-blanks 'garbage', and the next iteration will start from where the
            ;; garbage ends.  If couldn't match any garbage, move point to the end
            ;; and return nil.
            (or (re-search-forward regexp parse-stop t)
                (re-search-forward "\\(?1:\\(?2:[^ \t]+\\)\\)" parse-stop 'skip)
                (prog1 nil (goto-char end)))))))))


(defvar terra-font-lock-keywords
  `(;; highlight the hash-bang line "#!/foo/bar/terra" as comment
    ("^#!.*$" . font-lock-comment-face)

    ;; Builtin constants
    (,(terra-rx (symbol "true" "false" "nil"))
     . font-lock-constant-face)

    ;; Keywords
    (,(terra-rx terra-keyword)
     . font-lock-keyword-face)

    ;; Labels used by the "goto" statement
    ;; Highlights the following syntax:  ::label::
    (,(terra-rx "::" ws terra-name ws "::")
      . font-lock-constant-face)

    ;; Hightlights the name of the label in the "goto" statement like
    ;; "goto label"
    (,(terra-rx (symbol (seq "goto" ws+ (group-n 1 terra-name))))
      (1 font-lock-constant-face))

    ;; Highlight terra builtin functions and variables
    (,terra--builtins
     (1 font-lock-builtin-face) (2 font-lock-builtin-face nil noerror))

    ("^[ \t]*\\_<for\\_>"
     (,(terra-make-delimited-matcher (terra-rx terra-name) ","
                                   (terra-rx (or (symbol "in") terra-assignment-op)))
      nil nil
      (1 font-lock-variable-name-face nil noerror)
      (2 font-lock-warning-face t noerror)
      (3 font-lock-warning-face t noerror)))

    ;; Handle local variable/function names
    ;;  local blalba, xyzzy =
    ;;        ^^^^^^  ^^^^^
    ;;
    ;;  local function foobar(x,y,z)
    ;;                 ^^^^^^
    ;;  local foobar = function(x,y,z)
    ;;        ^^^^^^
    ("^[ \t]*\\_<local\\_>"
     (0 font-lock-keyword-face)

     ;; (* nonl) at the end is to consume trailing characters or otherwise they
     ;; delimited matcher would attempt to parse them afterwards and wrongly
     ;; highlight parentheses as incorrect variable name characters.
     (,(terra-rx point ws terra-funcheader (* nonl))
      nil nil
      (1 font-lock-function-name-face nil noerror))

     (,(terra-make-delimited-matcher (terra-rx terra-name) ","
                                   (terra-rx terra-assignment-op))
      nil nil
      (1 font-lock-variable-name-face nil noerror)
      (2 font-lock-warning-face t noerror)
      (3 font-lock-warning-face t noerror)))

    (,(terra-rx (or bol ";") ws terra-funcheader)
     (1 font-lock-function-name-face))

    (,(terra-rx (or (group-n 1
                           "@" (symbol "author" "copyright" "field" "release"
                                       "return" "see" "usage" "description"))
                  (seq (group-n 1 "@" (symbol "param" "class" "name")) ws+
                       (group-n 2 terra-name))))
     (1 font-lock-keyword-face t)
     (2 font-lock-variable-name-face t noerror)))

  "Default expressions to highlight in Terra mode.")

(defvar terra-imenu-generic-expression
  `((nil ,(terra-rx (or bol ";") ws (opt (seq (symbol "local") ws)) terra-funcheader) 1))
  "Imenu generic expression for terra-mode.  See `imenu-generic-expression'.")

(defvar terra-sexp-alist '(("then" . "end")
                      ("function" . "end")
                      ("do" . "end")
                      ("repeat" . "until")

                      ;; Terra keywords
                      ("escape" . "end")
                      ("quote" . "end")
                      ("terra" . "end")))

(defvar terra-mode-abbrev-table nil
  "Abbreviation table used in terra-mode buffers.")

(define-abbrev-table 'terra-mode-abbrev-table
  ;; Emacs 23 introduced :system property that prevents abbrev
  ;; entries from being written to file specified by abbrev-file-name
  ;;
  ;; Emacs 22 and earlier had this functionality implemented
  ;; by simple nil/non-nil flag as positional parameter
  (if (>= emacs-major-version 23)
      '(("end"    "end"    terra-indent-line :system t)
        ("else"   "else"   terra-indent-line :system t)
        ("elseif" "elseif" terra-indent-line :system t))
    '(("end"    "end"      terra-indent-line nil 'system)
      ("else"   "else"     terra-indent-line nil 'system)
      ("elseif" "elseif"   terra-indent-line nil 'system))))

(defvar terra-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; main comment syntax: begins with "--", ends with "\n"
    (modify-syntax-entry ?- ". 12")
    (modify-syntax-entry ?\n ">")

    ;; main string syntax: bounded by ' or "
    (modify-syntax-entry ?\' "\"")
    (modify-syntax-entry ?\" "\"")

    ;; single-character binary operators: punctuation
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?* ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?^ ".")
    (modify-syntax-entry ?% ".")
    (modify-syntax-entry ?> ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?~ ".")

    (syntax-table))
  "`terra-mode' syntax table.")

;;;###autoload
(define-derived-mode terra-mode prog-mode "Terra"
  "Major mode for editing Terra code."
  :abbrev-table terra-mode-abbrev-table
  :syntax-table terra-mode-syntax-table
  :group 'terra
  (setq comint-prompt-regexp terra-prompt-regexp)


  (terra--setq-local font-lock-defaults '(terra-font-lock-keywords ;; keywords
                                        nil                    ;; keywords-only
                                        nil                    ;; case-fold
                                        nil                    ;; syntax-alist
                                        nil                    ;; syntax-begin
                                        ))

  (if (boundp 'syntax-propertize-function)
      (terra--setq-local syntax-propertize-function
                       'terra--propertize-multiline-bounds)
    (with-no-warnings
      ;; font-lock-syntactic-keywords are deprecated since 24.1
      (terra--setq-local
       font-lock-syntactic-keywords 'terra-font-lock-syntactic-keywords)
      (terra--setq-local font-lock-extra-managed-props  '(syntax-table))))
  (terra--setq-local parse-sexp-lookup-properties   t)
  (terra--setq-local indent-line-function           'terra-indent-line)
  (terra--setq-local beginning-of-defun-function    'terra-beginning-of-proc)
  (terra--setq-local end-of-defun-function          'terra-end-of-proc)
  (terra--setq-local comment-start                  terra-comment-start)
  (terra--setq-local comment-start-skip             terra-comment-start-skip)
  (terra--setq-local comment-use-syntax             t)
  (terra--setq-local fill-paragraph-function        #'terra--fill-paragraph)
  (with-no-warnings
    (terra--setq-local comment-use-global-state     t))
  (terra--setq-local imenu-generic-expression       terra-imenu-generic-expression)
  (when (boundp 'electric-indent-chars)
    ;; If electric-indent-chars is not defined, electric indentation is done
    ;; via `terra-mode-map'.
    (terra--setq-local electric-indent-chars
                  (append electric-indent-chars terra--electric-indent-chars)))


  ;; setup menu bar entry (XEmacs style)
  (if (and (featurep 'menubar)
           (boundp 'current-menubar)
           (fboundp 'set-buffer-menubar)
           (fboundp 'add-menu)
           (not (assoc "Terra" current-menubar)))
      (progn
        (set-buffer-menubar (copy-sequence current-menubar))
        (add-menu nil "Terra" terra-emacs-menu)))
  ;; Append Terra menu to popup menu for Emacs.
  (if (boundp 'mode-popup-menu)
      (setq mode-popup-menu
            (cons (concat mode-name " Mode Commands") terra-emacs-menu)))

  ;; hideshow setup
  (unless (assq 'terra-mode hs-special-modes-alist)
    (add-to-list 'hs-special-modes-alist
                 `(terra-mode
                   ,(regexp-opt (mapcar 'car terra-sexp-alist) 'words) ;start
                   ,(regexp-opt (mapcar 'cdr terra-sexp-alist) 'words) ;end
                   nil terra-forward-sexp))))



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.t$" . terra-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("terra" . terra-mode))

(defun terra-electric-match (arg)
  "Insert character and adjust indentation."
  (interactive "P")
  (let (blink-paren-function)
   (self-insert-command (prefix-numeric-value arg)))
  (if terra-electric-flag
      (terra-indent-line))
  (blink-matching-open))

;; private functions

(defun terra--fill-paragraph (&optional justify region)
  ;; Implementation of forward-paragraph for filling.
  ;;
  ;; This function works around a corner case in the following situations:
  ;;
  ;;     <>
  ;;     -- some very long comment ....
  ;;     some_code_right_after_the_comment
  ;;
  ;; If point is at the beginning of the comment line, fill paragraph code
  ;; would have gone for comment-based filling and done the right thing, but it
  ;; does not find a comment at the beginning of the empty line before the
  ;; comment and falls back to text-based filling ignoring comment-start and
  ;; spilling the comment into the code.
  (while (and (not (eobp))
              (progn (move-to-left-margin)
                     (looking-at paragraph-separate)))
    (forward-line 1))
  (let ((fill-paragraph-handle-comment t))
    (fill-paragraph justify region)))


(defun terra-prefix-key-update-bindings ()
  (let (old-cons)
    (if (eq terra-prefix-mode-map (keymap-parent terra-mode-map))
        ;; if prefix-map is a parent, delete the parent
        (set-keymap-parent terra-mode-map nil)
      ;; otherwise, look for it among children
      (if (setq old-cons (rassoc terra-prefix-mode-map terra-mode-map))
          (delq old-cons terra-mode-map)))

    (if (null terra-prefix-key)
        (set-keymap-parent terra-mode-map terra-prefix-mode-map)
      (define-key terra-mode-map (vector terra-prefix-key) terra-prefix-mode-map))))

(defun terra-set-prefix-key (new-key-str)
  "Changes `terra-prefix-key' properly and updates keymaps

This function replaces previous prefix-key binding with a new one."
  (interactive "sNew prefix key (empty string means no key): ")
  (terra--customize-set-prefix-key 'terra-prefix-key new-key-str)
  (message "Prefix key set to %S"  (single-key-description terra-prefix-key))
  (terra-prefix-key-update-bindings))

(defun terra-string-p (&optional pos)
  "Returns true if the point is in a string."
  (save-excursion (elt (syntax-ppss pos) 3)))

(defun terra-comment-start-pos (parsing-state)
  "Return position of comment containing current point.

If point is not inside a comment, return nil."
  (and parsing-state (nth 4 parsing-state) (nth 8 parsing-state)))

(defun terra-comment-or-string-p (&optional pos)
  "Returns true if the point is in a comment or string."
  (save-excursion (let ((parse-result (syntax-ppss pos)))
                    (or (elt parse-result 3) (elt parse-result 4)))))

(defun terra-comment-or-string-start-pos (&optional pos)
  "Returns start position of string or comment which contains point.

If point is not inside string or comment, return nil."
  (save-excursion (elt (syntax-ppss pos) 8)))

;; They're propertized as follows:
;; 1. generic-comment
;; 2. generic-string
;; 3. equals signs
(defconst terra-ml-begin-regexp
  "\\(?:\\(?1:-\\)-\\[\\|\\(?2:\\[\\)\\)\\(?3:=*\\)\\[")


(defun terra-try-match-multiline-end (end)
  "Try to match close-bracket for multiline literal around point.

Basically, detect form of close bracket from syntactic
information provided at point and re-search-forward to it."
  (let ((comment-or-string-start-pos (terra-comment-or-string-start-pos)))
    ;; Is there a literal around point?
    (and comment-or-string-start-pos
         ;; It is, check if the literal is a multiline open-bracket
         (save-excursion
           (goto-char comment-or-string-start-pos)
           (looking-at terra-ml-begin-regexp))

         ;; Yes it is, look for it matching close-bracket.  Close-bracket's
         ;; match group is determined by match-group of open-bracket.
         (re-search-forward
          (format "]%s\\(?%s:]\\)"
                  (match-string-no-properties 3)
                  (if (match-beginning 1) 1 2))
          end 'noerror))))


(defun terra-try-match-multiline-begin (limit)
  "Try to match multiline open-brackets.

Find next opening long bracket outside of any string/comment.
If none can be found before reaching LIMIT, return nil."

  (let (last-search-matched)
    (while
        ;; This loop will iterate skipping all multiline-begin tokens that are
        ;; inside strings or comments ending either at EOL or at valid token.
        (and (setq last-search-matched
                   (re-search-forward terra-ml-begin-regexp limit 'noerror))

             (or (terra-comment-or-string-start-pos (match-beginning 0))
                 ;; Handle triple-hyphen '---[[' situation: match-beginning is
                 ;; BEFORE the second hyphen, but the comment would only starts
                 ;; AFTER it.
                 (and (eq ?- (char-after (match-beginning 0)))
                      (eq ?- (char-before (match-beginning 0)))))))

    last-search-matched))

(defun terra-match-multiline-literal-bounds (limit)
  ;; First, close any multiline literal spanning from previous block. This will
  ;; move the point accordingly so as to avoid double traversal.
  (or (terra-try-match-multiline-end limit)
      (terra-try-match-multiline-begin limit)))

(defun terra-remove-syntax-table-property (limit)
  "Remove syntax-table property on given region.

This is a workaround for `font-lock-default-fontify-region'
sometimes forgetting to unpropertize region which may cause
multiline recognition to fail.

Returns nil so that it's only called once as a syntactic keyword.
"
  (remove-text-properties (point) limit '(syntax-table))
  nil)

(defvar terra-font-lock-syntactic-keywords
  '((terra-remove-syntax-table-property nil)
    (terra-match-multiline-literal-bounds
     (1 "!" nil noerror)
     (2 "|" nil noerror))))


(defun terra--propertize-multiline-bounds (start end)
  "Put text properties on beginnings and ends of multiline literals.

Intended to be used as a `syntax-propertize-function'."
  (save-excursion
    (goto-char start)
    (while (terra-match-multiline-literal-bounds end)
      (when (match-beginning 1)
        (put-text-property (match-beginning 1) (match-end 1)
                           'syntax-table (string-to-syntax "!")))
      (when (match-beginning 2)
        (put-text-property (match-beginning 2) (match-end 2)
                           'syntax-table (string-to-syntax "|"))))))


(defun terra-indent-line ()
  "Indent current line for Terra mode.
Return the amount the indentation changed by."
  (let (indent
        (case-fold-search nil)
        ;; save point as a distance to eob - it's invariant w.r.t indentation
        (pos (- (point-max) (point))))
    (back-to-indentation)
    (if (terra-comment-or-string-p)
        (setq indent (terra-calculate-string-or-comment-indentation)) ;; just restore point position
      (setq indent (max 0 (terra-calculate-indentation nil))))

    (when (not (equal indent (current-column)))
      (delete-region (line-beginning-position) (point))
      (indent-to indent))

    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))

    indent))

(defun terra-calculate-string-or-comment-indentation ()
  "This function should be run when point at (current-indentation) is inside string"
  (if (and (terra-string-p) (not terra-indent-string-contents))
      ;; if inside string and strings aren't to be indented, return current indentation
      (current-indentation)

    ;; At this point, we know that we're inside comment, so make sure
    ;; close-bracket is unindented like a block that starts after
    ;; left-shifter.
    (let ((left-shifter-p (looking-at "\\s *\\(?:--\\)?\\]\\(?1:=*\\)\\]")))
      (save-excursion
        (goto-char (terra-comment-or-string-start-pos))
        (+ (current-indentation)
           (if (and left-shifter-p
                    (looking-at (format "--\\[%s\\["
                                        (match-string-no-properties 1))))
               0
             terra-indent-level))))))

(defun terra-find-regexp (direction regexp &optional limit ignore-p)
  "Searches for a regular expression in the direction specified.
Direction is one of 'forward and 'backward.
By default, matches in comments and strings are ignored, but what to ignore is
configurable by specifying ignore-p. If the regexp is found, returns point
position, nil otherwise.
ignore-p returns true if the match at the current point position should be
ignored, nil otherwise."
  (let ((ignore-func (or ignore-p 'terra-comment-or-string-p))
        (search-func (if (eq direction 'forward)
                         're-search-forward 're-search-backward))
        (case-fold-search nil))
    (catch 'found
      (while (funcall search-func regexp limit t)
        (if (and (not (funcall ignore-func (match-beginning 0)))
                 (not (funcall ignore-func (match-end 0))))
            (throw 'found (point)))))))

(defconst terra-block-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("do" "function" "repeat" "then"
                   "else" "elseif" "end" "until"

                   ;; Terra keywords
                   "escape"
                   "quote"
                   "terra") t)
     "\\_>\\)\\|"
     (regexp-opt '("{" "(" "[" "]" ")" "}") t))))

(defconst terra-block-token-alist
  '(("do"       "\\_<end\\_>"   "\\_<for\\|while\\_>"                       middle-or-open)
    ("function" "\\_<end\\_>"   nil                                       open)
    ("repeat"   "\\_<until\\_>" nil                                       open)
    ("then"     "\\_<\\(e\\(lse\\(if\\)?\\|nd\\)\\)\\_>" "\\_<\\(else\\)?if\\_>" middle)
    ("{"        "}"           nil                                       open)
    ("["        "]"           nil                                       open)
    ("("        ")"           nil                                       open)
    ("if"       "\\_<then\\_>"  nil                                       open)
    ("for"      "\\_<do\\_>"    nil                                       open)
    ("while"    "\\_<do\\_>"    nil                                       open)
    ("else"     "\\_<end\\_>"   "\\_<then\\_>"                              middle)
    ("elseif"   "\\_<then\\_>"  "\\_<then\\_>"                              middle)
    ("end"      nil           "\\_<\\(do\\|function\\|then\\|else\\|quote\\|terra\\)\\_>" close) ;; Terra keywords
    ("until"    nil           "\\_<repeat\\_>"                            close)
    ("}"        nil           "{"                                       close)
    ("]"        nil           "\\["                                     close)
    (")"        nil           "("                                       close)

    ;; Terra keywords
    ("escape"   "\\_<end\\_>" nil                                       open)
    ("quote"    "\\_<end\\_>" nil                                       open)
    ("terra"    "\\_<end\\_>" nil                                       open))
  "This is a list of block token information blocks.
Each token information entry is of the form:
  KEYWORD FORWARD-MATCH-REGEXP BACKWARDS-MATCH-REGEXP TOKEN-TYPE
KEYWORD is the token.
FORWARD-MATCH-REGEXP is a regexp that matches all possble tokens when going forward.
BACKWARDS-MATCH-REGEXP is a regexp that matches all possble tokens when going backwards.
TOKEN-TYPE determines where the token occurs on a statement. open indicates that the token appears at start, close indicates that it appears at end, middle indicates that it is a middle type token, and middle-or-open indicates that it can appear both as a middle or an open type.")

(defconst terra-indentation-modifier-regexp
  ;; The absence of else is deliberate, since it does not modify the
  ;; indentation level per se. It only may cause the line, in which the
  ;; else is, to be shifted to the left.
  (concat
   "\\(\\_<"
   (regexp-opt '("do" "function" "repeat" "then" "if" "else" "elseif" "for" "while"

                 ;; Terra keywords
                 "escape"
                 "quote"
                 "terra") t)
   "\\_>\\|"
   (regexp-opt '("{" "(" "["))
   "\\)\\|\\(\\_<"
   (regexp-opt '("end" "until") t)
   "\\_>\\|"
   (regexp-opt '("]" ")" "}"))
   "\\)")
  )

(defun terra-get-block-token-info (token)
  "Returns the block token info entry for TOKEN from terra-block-token-alist"
  (assoc token terra-block-token-alist))

(defun terra-get-token-match-re (token-info direction)
  "Returns the relevant match regexp from token info"
  (cond
   ((eq direction 'forward) (cadr token-info))
   ((eq direction 'backward) (caddr token-info))
   (t nil)))

(defun terra-get-token-type (token-info)
  "Returns the relevant match regexp from token info"
   (cadddr token-info))

(defun terra-backwards-to-block-begin-or-end ()
  "Move backwards to nearest block begin or end.  Returns nil if not successful."
  (interactive)
  (terra-find-regexp 'backward terra-block-regexp))

(defun terra-find-matching-token-word (token &optional direction)
  (let* ((token-info (terra-get-block-token-info token))
         (match-type (terra-get-token-type token-info))
         ;; If we are on a middle token, go backwards. If it is a middle or open,
         ;; go forwards
         (search-direction (or direction
                               (if (or (eq match-type 'open)
                                       (eq match-type 'middle-or-open))
                                   'forward
                                 'backward)
                               'backward))
         (match (terra-get-token-match-re token-info search-direction))
         maybe-found-pos)
    ;; if we are searching forward from the token at the current point
    ;; (i.e. for a closing token), need to step one character forward
    ;; first, or the regexp will match the opening token.
    (if (eq search-direction 'forward) (forward-char 1))
    (catch 'found
      ;; If we are attempting to find a matching token for a terminating token
      ;; (i.e. a token that starts a statement when searching back, or a token
      ;; that ends a statement when searching forward), then we don't need to look
      ;; any further.
      (if (or (and (eq search-direction 'forward)
                   (eq match-type 'close))
              (and (eq search-direction 'backward)
                   (eq match-type 'open)))
          (throw 'found nil))
      (while (terra-find-regexp search-direction terra-indentation-modifier-regexp)
        ;; have we found a valid matching token?
        (let ((found-token (match-string 0))
              (found-pos (match-beginning 0)))
          (let ((found-type (terra-get-token-type
                             (terra-get-block-token-info found-token))))
            (if (not (and match (string-match match found-token)))
                ;; no - then there is a nested block. If we were looking for
                ;; a block begin token, found-token must be a block end
                ;; token; likewise, if we were looking for a block end token,
                ;; found-token must be a block begin token, otherwise there
                ;; is a grammatical error in the code.
                (if (not (and
                          (or (eq match-type 'middle)
                              (eq found-type 'middle)
                              (eq match-type 'middle-or-open)
                              (eq found-type 'middle-or-open)
                              (eq match-type found-type))
                          (goto-char found-pos)
                          (terra-find-matching-token-word found-token
                                                        search-direction)))
                    (when maybe-found-pos
                      (goto-char maybe-found-pos)
                      (throw 'found maybe-found-pos)))
              ;; yes.
              ;; if it is a not a middle kind, report the location
              (when (not (or (eq found-type 'middle)
                             (eq found-type 'middle-or-open)))
                (throw 'found found-pos))
              ;; if it is a middle-or-open type, record location, but keep searching.
              ;; If we fail to complete the search, we'll report the location
              (when (eq found-type 'middle-or-open)
                (setq maybe-found-pos found-pos))
              ;; Cannot use tail recursion. too much nesting on long chains of
              ;; if/elseif. Will reset variables instead.
              (setq token found-token)
              (setq token-info (terra-get-block-token-info token))
              (setq match (terra-get-token-match-re token-info search-direction))
              (setq match-type (terra-get-token-type token-info))))))
      maybe-found-pos)))

(defun terra-goto-matching-block-token (&optional parse-start direction)
  "Find block begion/end token matching the one at the point.
This function moves the point to the token that matches the one
at the current point. Returns the point position of the first character of
the matching token if successful, nil otherwise."
  (if parse-start (goto-char parse-start))
  (let ((case-fold-search nil))
    (if (looking-at terra-indentation-modifier-regexp)
        (let ((position (terra-find-matching-token-word (match-string 0)
                                                      direction)))
          (and position
               (goto-char position))))))

(defun terra-goto-matching-block (&optional noreport)
  "Go to the keyword balancing the one under the point.
If the point is on a keyword/brace that starts a block, go to the
matching keyword that ends the block, and vice versa."
  (interactive)
  ;; search backward to the beginning of the keyword if necessary
  (if (eq (char-syntax (following-char)) ?w)
      (re-search-backward "\\_<" nil t))
  (let ((position (terra-goto-matching-block-token)))
    (if (and (not position)
             (not noreport))
        (error "Not on a block control keyword or brace")
      position)))

(defun terra-forward-line-skip-blanks (&optional back)
  "Move 1 line forward (back if BACK is non-nil) skipping blank lines.

Moves point 1 line forward (or backward) skipping lines that contain
no Terra code besides comments. The point is put to the beginning of
the line.

Returns final value of point as integer or nil if operation failed."
  (catch 'found
    (while t
      (unless (eql (forward-line (if back -1 1)) 0)    ;; 0 means success
        (throw 'found nil))
      (unless (or (looking-at "\\s *\\(--.*\\)?$")
                  (terra-comment-or-string-p))
        (throw 'found (point))))))

(eval-when-compile
  (defconst terra-operator-class
    "-+*/^.=<>~:&|"))

(defconst terra-cont-eol-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("and" "or" "not" "in" "for" "while"
                   "local" "function" "if" "until" "elseif" "return"

                   ;; Terra keywords
                   "defer"
                   "emit"
                   "escape"
                   "import"
                   "quote"
                   "terra"
                   "var")
                 t)
     "\\_>\\|"
     "\\(^\\|[^" terra-operator-class "]\\)"
     (regexp-opt '("+" "-" "*" "/" "%" "^" ".." "=="
                   "=" "<" ">" "<=" ">=" "~=" "." ":"
                   "&" "|" "~" ">>" "<<" "~")
                 t)
     "\\)"
     "\\s *\\="))
  "Regexp that matches the ending of a line that needs continuation

This regexp starts from eol and looks for a binary operator or an unclosed
block intro (i.e. 'for' without 'do' or 'if' without 'then') followed by
an optional whitespace till the end of the line.")

(defconst terra-cont-bol-regexp
  (eval-when-compile
    (concat
     "\\=\\s *"
     "\\(\\_<"
     (regexp-opt '("and" "or" "not") t)
     "\\_>\\|"
     (regexp-opt '("+" "-" "*" "/" "%" "^" ".." "=="
                   "=" "<" ">" "<=" ">=" "~=" "." ":"
                   "&" "|" "~" ">>" "<<" "~")
                 t)
     "\\($\\|[^" terra-operator-class "]\\)"
     "\\)"))
  "Regexp that matches a line that continues previous one

This regexp means, starting from point there is an optional whitespace followed
by Terra binary operator. Terra is very liberal when it comes to continuation line,
so we're safe to assume that every line that starts with a binop continues
previous one even though it looked like an end-of-statement.")

(defun terra-last-token-continues-p ()
  "Returns true if the last token on this line is a continuation token."
  (let ((line-begin (line-beginning-position))
        (line-end (line-end-position)))
    (save-excursion
      (end-of-line)
      ;; we need to check whether the line ends in a comment and
      ;; skip that one.
      (while (terra-find-regexp 'backward "-" line-begin 'terra-string-p)
        (if (looking-at "--")
            (setq line-end (point))))
      (goto-char line-end)
      (re-search-backward terra-cont-eol-regexp line-begin t))))

(defun terra-first-token-continues-p ()
  "Returns true if the first token on this line is a continuation token."
  (let ((line-end (line-end-position)))
    (save-excursion
      (beginning-of-line)
      ;; if first character of the line is inside string, it's a continuation
      ;; if strings aren't supposed to be indented, `terra-calculate-indentation' won't even let
      ;; the control inside this function
      (re-search-forward terra-cont-bol-regexp line-end t))))

(defconst terra-block-starter-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("do" "while" "repeat" "until" "if" "then"
                   "else" "elseif" "end" "for" "local") t)
     "\\_>\\)")))

(defun terra-first-token-starts-block-p ()
  "Returns true if the first token on this line is a block starter token."
  (let ((line-end (line-end-position)))
    (save-excursion
      (beginning-of-line)
      (re-search-forward (concat "\\s *" terra-block-starter-regexp) line-end t))))

(defun terra-is-continuing-statement-p (&optional parse-start)
  "Return non-nil if the line continues a statement.
More specifically, return the point in the line that is continued.
The criteria for a continuing statement are:

* the last token of the previous line is a continuing op,
  OR the first token of the current line is a continuing op

"
  (let ((prev-line nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      (save-excursion (setq prev-line (terra-forward-line-skip-blanks 'back)))
      (and prev-line
           (not (terra-first-token-starts-block-p))
           (or (terra-first-token-continues-p)
               (and (goto-char prev-line)
                    ;; check last token of previous nonblank line
                    (terra-last-token-continues-p)))))))

(defun terra-make-indentation-info-pair (found-token found-pos)
  "This is a helper function to terra-calculate-indentation-info. Don't
use standalone."
  (cond
   ;; function is a bit tricky to indent right. They can appear in a lot ot
   ;; different contexts. Until I find a shortcut, I'll leave it with a simple
   ;; relative indentation.
   ;; The special cases are for indenting according to the location of the
   ;; function. i.e.:
   ;;       (cons 'absolute (+ (current-column) terra-indent-level))
   ;; TODO: Fix this. It causes really ugly indentations for in-line functions.
   ((string-equal found-token "function")
    (cons 'relative terra-indent-level))

   ((member found-token (list

                         ;; Terra keywords
                         "quote"
                         "terra"))
    (cons 'relative terra-indent-level))

   ;; block openers
   ((member found-token (list "{" "(" "["))
    (save-excursion
      (let ((found-bol (line-beginning-position)))
        (forward-comment (point-max))
        ;; If the next token is on this line and it's not a block opener,
        ;; the next line should align to that token.
        (if (and (zerop (count-lines found-bol (line-beginning-position)))
                 (not (looking-at terra-indentation-modifier-regexp)))
            (cons 'absolute (current-column))
          (cons 'relative terra-indent-level)))))

   ;; These are not really block starters. They should not add to indentation.
   ;; The corresponding "then" and "do" handle the indentation.
   ((member found-token (list "if" "for" "while"))
    (cons 'relative 0))
   ;; closing tokens follow: These are usually taken care of by
   ;; terra-calculate-indentation-override.
   ;; elseif is a bit of a hack. It is not handled separately, but it needs to
   ;; nullify a previous then if on the same line.
   ((member found-token (list "until" "elseif"))
    (save-excursion
      (let ((line (line-number-at-pos)))
        (if (and (terra-goto-matching-block-token found-pos 'backward)
                 (= line (line-number-at-pos)))
            (cons 'remove-matching 0)
          (cons 'relative 0)))))

   ;; else is a special case; if its matching block token is on the same line,
   ;; instead of removing the matching token, it has to replace it, so that
   ;; either the next line will be indented correctly, or the end on the same
   ;; line will remove the effect of the else.
   ((string-equal found-token "else")
     (save-excursion
       (let ((line (line-number-at-pos)))
         (if (and (terra-goto-matching-block-token found-pos 'backward)
                  (= line (line-number-at-pos)))
             (cons 'replace-matching (cons 'relative terra-indent-level))
                   (cons 'relative terra-indent-level)))))

   ;; Block closers. If they are on the same line as their openers, they simply
   ;; eat up the matching indentation modifier. Otherwise, they pull
   ;; indentation back to the matching block opener.
   ((member found-token (list ")" "}" "]" "end"))
    (save-excursion
      (let ((line (line-number-at-pos)))
        (terra-goto-matching-block-token found-pos 'backward)
        (if (/= line (line-number-at-pos))
            (terra-calculate-indentation-info (point))
          (cons 'remove-matching 0)))))

   ;; Everything else. This is from the original code: If opening a block
   ;; (match-data 1 exists), then push indentation one level up, if it is
   ;; closing a block, pull it one level down.
   ('other-indentation-modifier
    (cons 'relative (if (nth 2 (match-data))
                        ;; beginning of a block matched
                        terra-indent-level
                      ;; end of a block matched
                      (- terra-indent-level))))))

(defun  terra-add-indentation-info-pair (pair info)
  "Add the given indentation info pair to the list of indentation information.
This function has special case handling for two tokens: remove-matching,
and replace-matching. These two tokens are cleanup tokens that remove or
alter the effect of a previously recorded indentation info.

When a remove-matching token is encountered, the last recorded info, i.e.
the car of the list is removed. This is used to roll-back an indentation of a
block opening statement when it is closed.

When a replace-matching token is seen, the last recorded info is removed,
and the cdr of the replace-matching info is added in its place. This is used
when a middle-of the block (the only case is 'else') is seen on the same line
the block is opened."
  (cond
   ( (eq 'remove-matching (car pair))
     ; Remove head of list
     (cdr info))
   ( (eq 'replace-matching (car pair))
     ; remove head of list, and add the cdr of pair instead
     (cons (cdr pair) (cdr info)))
   ( (listp (cdr-safe pair))
     (nconc pair info))
   ( t
     ; Just add the pair
     (cons pair info))))

(defun terra-calculate-indentation-info-1 (indentation-info bound)
  "Helper function for `terra-calculate-indentation-info'.

Return list of indentation modifiers from point to BOUND."
  (while (terra-find-regexp 'forward terra-indentation-modifier-regexp
                          bound)
    (let ((found-token (match-string 0))
          (found-pos (match-beginning 0))
          (found-end (match-end 0))
          (data (match-data)))
      (setq indentation-info
            (terra-add-indentation-info-pair
             (terra-make-indentation-info-pair found-token found-pos)
             indentation-info))))
  indentation-info)


(defun terra-calculate-indentation-info (&optional parse-end)
  "For each block token on the line, computes how it affects the indentation.
The effect of each token can be either a shift relative to the current
indentation level, or indentation to some absolute column. This information
is collected in a list of indentation info pairs, which denote absolute
and relative each, and the shift/column to indent to."
  (let ((combined-line-end (line-end-position))
        indentation-info)

    (while (terra-is-continuing-statement-p)
      (terra-forward-line-skip-blanks 'back))

    ;; calculate indentation modifiers for the line itself
    (setq indentation-info (list (cons 'absolute (current-indentation))))

    (back-to-indentation)
    (setq indentation-info
          (terra-calculate-indentation-info-1
           indentation-info (min parse-end (line-end-position))))

    ;; and do the following for each continuation line before PARSE-END
    (while (and (eql (forward-line 1) 0)
                (<= (point) parse-end))

      ;; handle continuation lines:
      (if (terra-is-continuing-statement-p)
          ;; if it's the first continuation line, add one level
          (unless (eq (car (car indentation-info)) 'continued-line)
            (push (cons 'continued-line terra-indent-level) indentation-info))

        ;; if it's the first non-continued line, subtract one level
        (when (eq (car (car indentation-info)) 'continued-line)
          (pop indentation-info)))

      ;; add modifiers found in this continuation line
      (setq indentation-info
            (terra-calculate-indentation-info-1
             indentation-info (min parse-end (line-end-position)))))

    indentation-info))


(defun terra-accumulate-indentation-info (info)
  "Accumulates the indentation information previously calculated by
terra-calculate-indentation-info. Returns either the relative indentation
shift, or the absolute column to indent to."
  (let ((info-list (reverse info))
        (type 'relative)
        (accu 0))
    (mapc (lambda (x)
            (setq accu (if (eq 'absolute (car x))
                           (progn (setq type 'absolute)
                                  (cdr x))
                         (+ accu (cdr x)))))
          info-list)
    (cons type accu)))

(defun terra-calculate-indentation-block-modifier (&optional parse-end)
  "Return amount by which this line modifies the indentation.
Beginnings of blocks add terra-indent-level once each, and endings
of blocks subtract terra-indent-level once each. This function is used
to determine how the indentation of the following line relates to this
one."
  (let (indentation-info)
    (save-excursion
      ;; First go back to the line that starts it all
      ;; terra-calculate-indentation-info will scan through the whole thing
      (let ((case-fold-search nil))
        (setq indentation-info
              (terra-accumulate-indentation-info
               (terra-calculate-indentation-info parse-end)))))

    (if (eq (car indentation-info) 'absolute)
        (- (cdr indentation-info) (current-indentation))
      (cdr indentation-info))))


(eval-when-compile
  (defconst terra--function-name-rx
    '(seq symbol-start
          (+ (any alnum "_"))
          (* "." (+ (any alnum "_")))
          (? ":" (+ (any alnum "_")))
          symbol-end)
    "Terra function name regexp in `rx'-SEXP format."))


(defconst terra--left-shifter-regexp
  (eval-when-compile
    (rx
     ;; This regexp should answer the following questions:
     ;; 1. is there a left shifter regexp on that line?
     ;; 2. where does block-open token of that left shifter reside?
     ;;
     ;; NOTE: couldn't use `group-n' keyword of `rx' macro, because it was
     ;; introduced in Emacs 24.2 only, so for the sake of code clarity the named
     ;; groups don't really match anything, they just report the position of the
     ;; match.
     (or (seq (regexp "\\_<local[ \t]+") (regexp "\\(?1:\\)function\\_>"))

         ;; Terra keywords
         (seq (regexp "\\_<local[ \t]+") (regexp "\\(?1:\\)terra\\_>"))

         (seq (eval terra--function-name-rx) (* blank) (regexp "\\(?1:\\)[{(]"))
         (seq (or
               ;; assignment statement prefix
               (seq (* nonl) (not (any "<=>~")) "=" (* blank))
               ;; return statement prefix
               (seq word-start "return" word-end (* blank)))
              (regexp "\\(?1:\\)")
              ;; right hand side
              (or "{"
                  "function"

                  ;; Terra keywords
                  "terra"

                  (seq
                   (eval terra--function-name-rx) (* blank)
                   (regexp "\\(?1:\\)") (any "({")))))))

  "Regular expression that matches left-shifter expression.

Left-shifter expression is defined as follows.  If a block
follows a left-shifter expression, its contents & block-close
token should be indented relative to left-shifter expression
indentation rather then to block-open token.

For example:
   -- 'local a = ' is a left-shifter expression
   -- 'function' is a block-open token
   local a = function()
      -- block contents is indented relative to left-shifter
      foobarbaz()
   -- block-end token is unindented to left-shifter indentation
   end

The following left-shifter expressions are currently handled:
1. local function definition with function block, begin-end
2. function call with arguments block, () or {}
3. assignment/return statement with
   - table constructor block, {}
   - function call arguments block, () or {} block
   - function expression a.k.a. lambda, begin-end block.")


(defun terra-point-is-after-left-shifter-p ()
  "Check if point is right after a left-shifter expression.

See `terra--left-shifter-regexp' for description & example of
left-shifter expression. "
  (save-excursion
    (let ((old-point (point)))
      (back-to-indentation)
      (and
       (/= (point) old-point)
       (looking-at terra--left-shifter-regexp)
       (= old-point (match-end 1))))))



(defun terra-calculate-indentation-override (&optional parse-start)
  "Return overriding indentation amount for special cases.

If there's a sequence of block-close tokens starting at the
beginning of the line, calculate indentation according to the
line containing block-open token for the last block-close token
in the sequence.

If not, return nil."
  (let (case-fold-search token-info block-token-pos)
    (save-excursion
      (if parse-start (goto-char parse-start))

      (back-to-indentation)
      (unless (terra-comment-or-string-p)
        (while
            (and (looking-at terra-indentation-modifier-regexp)
                 (setq token-info (terra-get-block-token-info (match-string 0)))
                 (not (eq 'open (terra-get-token-type token-info))))
          (setq block-token-pos (match-beginning 0))
          (goto-char (match-end 0))
          (skip-syntax-forward " " (line-end-position)))

        (when (terra-goto-matching-block-token block-token-pos 'backward)
          ;; Exception cases: when the start of the line is an assignment,
          ;; go to the start of the assignment instead of the matching item
          (if (terra-point-is-after-left-shifter-p)
              (current-indentation)
            (current-column)))))))

(defun terra-calculate-indentation (&optional parse-start)
  "Return appropriate indentation for current line as Terra code."
  (save-excursion
    (let ((continuing-p (terra-is-continuing-statement-p))
          (cur-line-begin-pos (line-beginning-position)))
      (or
       ;; when calculating indentation, do the following:
       ;; 1. check, if the line starts with indentation-modifier (open/close brace)
       ;;    and if it should be indented/unindented in special way
       (terra-calculate-indentation-override)

       (when (terra-forward-line-skip-blanks 'back)
         ;; the order of function calls here is important. block modifier
         ;; call may change the point to another line
         (let* ((modifier
                 (terra-calculate-indentation-block-modifier cur-line-begin-pos)))
           (+ (current-indentation) modifier)))

       ;; 4. if there's no previous line, indentation is 0
       0))))

(defvar terra--beginning-of-defun-re
  (terra-rx-to-string '(: bol (? (symbol "local") ws+) terra-funcheader))
  "Terra top level (matches only at the beginning of line) function header regex.")


(defun terra-beginning-of-proc (&optional arg)
  "Move backward to the beginning of a terra proc (or similar).

With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of proc.

Returns t unless search stops due to beginning or end of buffer."
  (interactive "P")
  (or arg (setq arg 1))

  (while (and (> arg 0)
              (re-search-backward terra--beginning-of-defun-re nil t))
    (setq arg (1- arg)))

  (while (and (< arg 0)
              (re-search-forward terra--beginning-of-defun-re nil t))
    (beginning-of-line)
    (setq arg (1+ arg)))

  (zerop arg))

(defun terra-end-of-proc (&optional arg)
  "Move forward to next end of terra proc (or similar).
With argument, do it that many times.  Negative argument -N means move
back to Nth preceding end of proc.

This function just searches for a `end' at the beginning of a line."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
        (ret t))
    (if (and (< arg 0)
             (not (bolp))
             (save-excursion
               (beginning-of-line)
               (eq (following-char) ?})))
        (forward-char -1))
    (while (> arg 0)
      (if (re-search-forward "^end" nil t)
          (setq arg (1- arg)
                found t)
        (setq ret nil
              arg 0)))
    (while (< arg 0)
      (if (re-search-backward "^end" nil t)
          (setq arg (1+ arg)
                found t)
        (setq ret nil
              arg 0)))
    (if found
        (progn
          (beginning-of-line)
          (forward-line)))
    ret))

(defvar terra-process-init-code
  (mapconcat
   'identity
   '("local loadstring = loadstring or load"
     "function terramode_loadstring(str, displayname, lineoffset)"
     "  if lineoffset > 1 then"
     "    str = string.rep('\\n', lineoffset - 1) .. str"
     "  end"
     ""
     "  x, e = loadstring(str, '@'..displayname)"
     "  if e then"
     "    error(e)"
     "  end"
     "  return x()"
     "end")
   " "))

(defun terra-make-terra-string (str)
  "Convert string to Terra literal."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "[\"'\\\t\\\n]" nil t)
        (cond
	 ((string= (match-string 0) "\n")
	  (replace-match "\\\\n"))
	 ((string= (match-string 0) "\t")
	  (replace-match "\\\\t"))
	 (t
          (replace-match "\\\\\\&" t))))
      (concat "'" (buffer-string) "'"))))

;;;###autoload
(defalias 'run-terra #'terra-start-process)

;;;###autoload
(defun terra-start-process (&optional name program startfile &rest switches)
  "Start a terra process named NAME, running PROGRAM.
PROGRAM defaults to NAME, which defaults to `terra-default-application'.
When called interactively, switch to the process buffer."
  (interactive)
  (or switches
      (setq switches terra-default-command-switches))
  (setq name (or name terra-default-application))
  (setq program (or program name))
  (setq terra-process-buffer (apply 'make-comint name program startfile switches))
  (setq terra-process (get-buffer-process terra-process-buffer))
  (set-process-query-on-exit-flag terra-process nil)
  (with-current-buffer terra-process-buffer
    ;; wait for prompt
    (while (not (terra-prompt-line))
      (accept-process-output (get-buffer-process (current-buffer)))
      (goto-char (point-max)))
    ;; send initialization code
    (terra-send-string terra-process-init-code)

    ;; enable error highlighting in stack traces
    (require 'compile)
    (setq terra--repl-buffer-p t)
    (make-local-variable 'compilation-error-regexp-alist)
    (setq compilation-error-regexp-alist
          (cons (list terra-traceback-line-re 1 2)
                ;; Remove 'gnu entry from error regexp alist, it somehow forces
                ;; leading TAB to be recognized as part of filename in Emacs23.
                (delq 'gnu compilation-error-regexp-alist)))
    (compilation-shell-minor-mode 1))

  ;; when called interactively, switch to process buffer
  (if (called-interactively-p 'any)
      (switch-to-buffer terra-process-buffer)))

(defun terra-get-create-process ()
  "Return active Terra process creating one if necessary."
  (or (and (comint-check-proc terra-process-buffer)
           terra-process)
      (terra-start-process))
  terra-process)

(defun terra-kill-process ()
  "Kill Terra subprocess and its buffer."
  (interactive)
  (when (buffer-live-p terra-process-buffer)
    (kill-buffer terra-process-buffer)))

(defun terra-set-terra-region-start (&optional arg)
  "Set start of region for use with `terra-send-terra-region'."
  (interactive)
  (set-marker terra-region-start (or arg (point))))

(defun terra-set-terra-region-end (&optional arg)
  "Set end of region for use with `terra-send-terra-region'."
  (interactive)
  (set-marker terra-region-end (or arg (point))))

(defun terra-send-string (str)
  "Send STR plus a newline to Terra subprocess.

If `terra-process' is nil or dead, start a new process first."
  (unless (string-equal (substring str -1) "\n")
    (setq str (concat str "\n")))
  (process-send-string (terra-get-create-process) str))

(defun terra-send-current-line ()
  "Send current line to Terra subprocess, found in `terra-process'.
If `terra-process' is nil or dead, start a new process first."
  (interactive)
  (terra-send-region (line-beginning-position) (line-end-position)))

(defun terra-send-defun (pos)
  "Send the function definition around point to terra subprocess."
  (interactive "d")
  (save-excursion
    (let ((start (if (save-match-data (looking-at "^function[ \t]"))
                     ;; point already at the start of "function".
                     ;; We need to handle this case explicitly since
                     ;; terra-beginning-of-proc will move to the
                     ;; beginning of the _previous_ function.
                     (point)
                   ;; point is not at the beginning of function, move
                   ;; there and bind start to that position
                   (terra-beginning-of-proc)
                   (point)))
          (end (progn (terra-end-of-proc) (point))))

      ;; make sure point is in a function defintion before sending to
      ;; the subprocess
      (if (and (>= pos start) (< pos end))
          (terra-send-region start end)
        (error "Not on a function definition")))))

(defun terra-maybe-skip-shebang-line (start)
  "Skip shebang (#!/path/to/interpreter/) line at beginning of buffer.

Return a position that is after Terra-recognized shebang line (1st
character in file must be ?#) if START is at its beginning.
Otherwise, return START."
  (save-restriction
    (widen)
    (if (and (eq start (point-min))
             (eq (char-after start) ?#))
        (save-excursion
          (goto-char start)
          (forward-line)
          (point))
      start)))

(defun terra-send-region (start end)
  (interactive "r")
  (setq start (terra-maybe-skip-shebang-line start))
  (let* ((lineno (line-number-at-pos start))
         (terra-file (or (buffer-file-name) (buffer-name)))
         (region-str (buffer-substring-no-properties start end))
         (command
          ;; Print empty line before executing the code so that the first line
          ;; of output doesn't end up on the same line as current prompt.
          (format "print(''); terramode_loadstring(%s, %s, %s);\n"
                  (terra-make-terra-string region-str)
                  (terra-make-terra-string terra-file)
                  lineno)))
    (terra-send-string command)
    (when terra-always-show (terra-show-process-buffer))))

(defun terra-prompt-line ()
  (save-excursion
    (save-match-data
      (forward-line 0)
      (if (looking-at comint-prompt-regexp)
          (match-end 0)))))

(defun terra-send-terra-region ()
  "Send preset terra region to terra subprocess."
  (interactive)
  (unless (and terra-region-start terra-region-end)
    (error "terra-region not set"))
  (terra-send-region terra-region-start terra-region-end))

(defalias 'terra-send-proc 'terra-send-defun)

(defun terra-send-buffer ()
  "Send whole buffer to terra subprocess."
  (interactive)
  (terra-send-region (point-min) (point-max)))

(defun terra-restart-with-whole-file ()
  "Restart terra subprocess and send whole file as input."
  (interactive)
  (terra-kill-process)
  (terra-send-buffer))

(defun terra-show-process-buffer ()
  "Make sure `terra-process-buffer' is being displayed.
Create a Terra process if one doesn't already exist."
  (interactive)
  (display-buffer (process-buffer (terra-get-create-process))))


(defun terra-hide-process-buffer ()
  "Delete all windows that display `terra-process-buffer'."
  (interactive)
  (when (buffer-live-p terra-process-buffer)
    (delete-windows-on terra-process-buffer)))

(defun terra-funcname-at-point ()
  "Get current Name { '.' Name } sequence."
  ;; FIXME: copying/modifying syntax table for each call may incur a penalty
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "_")
    (current-word t)))

(defun terra-search-documentation ()
  "Search Terra documentation for the word at the point."
  (interactive)
  (let ((url (concat terra-documentation-url "#pdf-" (terra-funcname-at-point))))
    (funcall terra-documentation-function url)))

(defun terra-toggle-electric-state (&optional arg)
  "Toggle the electric indentation feature.
Optional numeric ARG, if supplied, turns on electric indentation when
positive, turns it off when negative, and just toggles it when zero or
left out."
  (interactive "P")
  (let ((num_arg (prefix-numeric-value arg)))
    (setq terra-electric-flag (cond ((or (null arg)
                                       (zerop num_arg)) (not terra-electric-flag))
                                  ((< num_arg 0) nil)
                                  ((> num_arg 0) t))))
  (message "%S" terra-electric-flag))

(defun terra-forward-sexp (&optional count)
  "Forward to block end"
  (interactive "p")
  ;; negative offsets not supported
  (assert (or (not count) (>= count 0)))
  (save-match-data
    (let* ((count (or count 1))
           (block-start (mapcar 'car terra-sexp-alist))
           (block-end (mapcar 'cdr terra-sexp-alist))
           (block-regex (regexp-opt (append  block-start block-end) 'words))
           current-exp)
      (while (> count 0)
        ;; skip whitespace
        (skip-chars-forward " \t\n")
        (if (looking-at (regexp-opt block-start 'words))
            (let ((keyword (match-string 1)))
              (terra-find-matching-token-word keyword 'forward))
          ;; If the current keyword is not a "begin" keyword, then just
          ;; perform the normal forward-sexp.
          (forward-sexp 1))
        (setq count (1- count))))))


;; menu bar

(define-key terra-mode-menu [restart-with-whole-file]
  '("Restart With Whole File" .  terra-restart-with-whole-file))
(define-key terra-mode-menu [kill-process]
  '("Kill Process" . terra-kill-process))

(define-key terra-mode-menu [hide-process-buffer]
  '("Hide Process Buffer" . terra-hide-process-buffer))
(define-key terra-mode-menu [show-process-buffer]
  '("Show Process Buffer" . terra-show-process-buffer))

(define-key terra-mode-menu [end-of-proc]
  '("End Of Proc" . terra-end-of-proc))
(define-key terra-mode-menu [beginning-of-proc]
  '("Beginning Of Proc" . terra-beginning-of-proc))

(define-key terra-mode-menu [send-terra-region]
  '("Send Terra-Region" . terra-send-terra-region))
(define-key terra-mode-menu [set-terra-region-end]
  '("Set Terra-Region End" . terra-set-terra-region-end))
(define-key terra-mode-menu [set-terra-region-start]
  '("Set Terra-Region Start" . terra-set-terra-region-start))

(define-key terra-mode-menu [send-current-line]
  '("Send Current Line" . terra-send-current-line))
(define-key terra-mode-menu [send-region]
  '("Send Region" . terra-send-region))
(define-key terra-mode-menu [send-proc]
  '("Send Proc" . terra-send-proc))
(define-key terra-mode-menu [send-buffer]
  '("Send Buffer" . terra-send-buffer))
(define-key terra-mode-menu [search-documentation]
  '("Search Documentation" . terra-search-documentation))


(provide 'terra-mode)

;;; terra-mode.el ends here
