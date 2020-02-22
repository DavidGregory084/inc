(defvar inc-mode-syntax-table nil "Syntax table for `inc-mode'.")

(setq inc-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; comments

        ;; normal line comments
        (modify-syntax-entry ?/ ". 124" synTable)
        (modify-syntax-entry ?\n ">" synTable)

        ;; block comments
        (modify-syntax-entry ?* ". 23bn" synTable)

        ;; brackets
        (modify-syntax-entry ?\( "()" synTable)
        (modify-syntax-entry ?\) ")(" synTable)
        (modify-syntax-entry ?\[ "(]" synTable)
        (modify-syntax-entry ?\] ")[" synTable)
        (modify-syntax-entry ?\{ "(}" synTable)
        (modify-syntax-entry ?\} "){" synTable)

        ;; char
        (modify-syntax-entry ?\' "\"" synTable)

        synTable))

(setq inc-highlights
      `(;; keywords
        (,(regexp-opt '("module" "import" "let" "if" "then" "else" "data" "case" "match" "with") 'symbols) . font-lock-keyword-face)
        ;; literals
        (,(regexp-opt '("true" "false") 'symbols) . font-lock-constant-face)
        ("[:digit:]+(?:\\.[:digit:]+)?[LDF]?" . font-lock-constant-face)
        ;; built-in types
        (,(regexp-opt '("Int" "Long" "Float" "Double" "Boolean" "Char" "String" "Unit") 'symbols) . font-lock-type-face)
        ;; function calls
        ("\\_<\\([_[:alpha:]][_[:alnum:]]*\\)\\s-*(" (1 font-lock-function-name-face))
        ;; identifiers
        ("\\_<let\\s-+\\([_[:alpha:]][_[:alnum:]]*\\)\\_>"
         (1 font-lock-variable-name-face))
        ("\\_<case\\s-+\\([_[:alpha:]][_[:alnum:]]*\\)\\s-*{"
         (1 font-lock-function-name-face)
         ;; Highlight the constructor patterns inside the case
         ("\\_<\\([_[:alpha:]][_[:alnum:]]*\\)\\s-*{"
          nil
          nil
          (1 font-lock-function-name-face)))
        ("\\_<case\\s-+\\([_[:alpha:]][_[:alnum:]]*\\)\\_>"
         (1 font-lock-variable-name-face))
        ("\\_<data\\s-+\\([_[:alpha:]][_[:alnum:]]*\\)\\_>"
         (1 font-lock-type-face))
        ;; Package references
        ("\\_<module\\s-+\\([_[:alpha:]][_[:alnum:]]*\\)"
         (1 font-lock-string-face)
         ;; Highlight package name segments
         ("/\\([_[:alpha:]][_[:alnum:]]*\\)\\_>"
          nil
          nil
          (1 font-lock-string-face)))
        ;; Imports
        ("import\\s-+\\([_[:alpha:]][_[:alnum:]]*\\)"
         (1 font-lock-string-face)
         ;; Highlight package name segments
         ("/\\([_[:alpha:]][_[:alnum:]]*\\)\\_>"
          nil
          nil
          (1 font-lock-string-face))
         ("\\.\\([_[:alpha:]][_[:alnum:]]*\\)\\_>"
          nil
          nil
          (1 font-lock-variable-name-face)))
        ;; Type ascriptions
        (":\\s-*\\([_[:alpha:]][_[:alnum:]]*\\)" (1 font-lock-type-face))
        ;; Type parameters
        ("\\["
         ("\\([_[:alpha:]][_[:alnum:]]*\\)"
          (save-excursion (up-list) (point))
          nil
          (1 font-lock-type-face)))))

(define-derived-mode inc-mode prog-mode "inc mode"
  (setq font-lock-defaults '(inc-highlights)))

(provide 'inc-mode)
