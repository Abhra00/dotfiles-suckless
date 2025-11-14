;; -*- lexical-binding: t; -*-

;; Restore normal GC threshold after startup
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 800000)))

;; Required for rc.el
(package-initialize)
(setq package-install-upgrade-built-in t)

;; Set different path for custom file
(setq custom-file "~/.config/emacs/custom.el")

;; Load rc.el
(load-file "~/.config/emacs/lisps/emacs.rc/rc.el")

;; Load miscellaneous rc file
(load-file "~/.config/emacs/lisps/emacs.rc/misc-rc.el")

;; Load auto-commit rc file
(load-file "~/.config/emacs/lisps/emacs.rc/autocommit-rc.el")

;; Disable menu bar
(menu-bar-mode -1)

;; Disable tool bar
(tool-bar-mode -1)

;; Disable scrollbar
(scroll-bar-mode -1)

;; Global Font Settings
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :weight 'regular
                    :height 210)

;; Fallback for icons/symbols
(when (member "Symbols Nerd Font Mono" (font-family-list))
  (set-fontset-font t nil (font-spec :family "Symbols Nerd Font Mono") nil 'append))

;; Make sure both pitch types use Iosevka
(set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 210)
(set-face-attribute 'variable-pitch nil :family "Iosevka" :height 210)

;; Enable ligatures
(dolist (char/ligature-re
         `((?-  . ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
           (?/  . ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
           (?*  . ,(rx (or (or "*>" "*/") (+ "*"))))
           (?<  . ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
                               "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>"
                               "<*>" "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+"
                               "</" "<*")
                           (+ "<"))))
           (?:  . ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
           (?=  . ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
           (?!  . ,(rx (or (or "!==" "!=") (+ "!"))))
           (?>  . ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
           (?&  . ,(rx (+ "&")))
           (?|  . ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>"
                               "|]" "|}" "|=")
                           (+ "|"))))
           (?.  . ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
           (?+  . ,(rx (or "+>" (+ "+"))))
           (?\[ . ,(rx (or "[<" "[|")))
           (?\{ . ,(rx "{|"))
           (?\? . ,(rx (or (or "?." "?=" "?:") (+ "?"))))
           (?#  . ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(")
                           (+ "#"))))
           (?\; . ,(rx (+ ";")))
           (?_  . ,(rx (or "_|_" "__")))
           (?~  . ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
           (?$  . ,(rx "$>"))
           (?^  . ,(rx "^="))
           (?\] . ,(rx "]#"))))
  (let ((char (car char/ligature-re))
        (ligature-re (cdr char/ligature-re)))
    (set-char-table-range composition-function-table char
                          `([,ligature-re 0 font-shape-gstring]))))

;;; --- Modus Vivendi Tinted Theme ---
(setq custom-safe-themes t)

;; Modus theme options
(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-variable-pitch-ui t
      modus-themes-mixed-fonts t
      modus-themes-prompts '(bold intense)
      modus-themes-completions '((t . (extrabold)))
      modus-themes-headings '((1 . (overline background)))
      modus-themes-fringes 'nil)

;; Override palette for line numbers
(setq modus-themes-common-palette-overrides
      '((bg-line-number-active   bg-main)
        (bg-line-number-inactive bg-main)
        (fg-line-number-active   "white")
        (fg-line-number-inactive "gray50")))

;; Load theme
(load-theme 'modus-vivendi-tinted :no-confirm)

;;; Add modeline padding
(defun rc/setup-modern-mode-line ()
  "Setup a spacious, padded mode-line compatible with modus-vivendi-tinted."
  (let* ((active-bg   (face-attribute 'mode-line :background))
         (active-fg   (face-attribute 'mode-line :foreground))
         (inactive-bg (face-attribute 'mode-line-inactive :background))
         (inactive-fg (face-attribute 'mode-line-inactive :foreground))
         (padding     7))  ;; Vertical padding in pixels
    ;; Active mode-line
    (set-face-attribute 'mode-line nil
                        :foreground active-fg
                        :background active-bg
                        :box `(:line-width ,padding :color ,active-bg)
                        :overline nil
                        :underline nil
                        :height 1.0)
    ;; Inactive mode-line
    (set-face-attribute 'mode-line-inactive nil
                        :foreground inactive-fg
                        :background inactive-bg
                        :box `(:line-width ,padding :color ,inactive-bg)
                        :overline nil
                        :underline nil
                        :height 1.0)
    ;; Refresh
    (setq-default mode-line-format mode-line-format)))

(rc/setup-modern-mode-line)
(add-hook 'modus-themes-after-load-theme-hook #'rc/setup-modern-mode-line)

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 25)
   (internal-border-width . 25)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;; Enable line & column numbers
(column-number-mode 1)
(global-display-line-numbers-mode 1)

;; Make line numbers relative
(setq display-line-numbers-type 'relative)

;; Save cursor positions across sessions
(save-place-mode 1)

;; Automatically refresh buffers when files change
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Highlight matching parens instantly
(show-paren-mode 1)

;; Enable recent files menu
(recentf-mode 1)
(setq recentf-max-menu-items 50
      recentf-max-saved-items 100)

;; Persist minibuffer history across sessions
(savehist-mode 1)

;; Undo/redo window configurations
(winner-mode 1)

;; Smoother scrolling
(setq scroll-margin 10
      scroll-conservatively 9999
      scroll-step 1)

;; Ido
(rc/require 'smex 'ido-completing-read+)

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Use ibuffer for improved buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Org mode
(load-file "~/.config/emacs/lisps/emacs.rc/org-mode-rc.el")

;; Load the sequence manipulation library
(require 'seq)

;; Magit
(rc/require 'magit)

(setq magit-auto-revert-mode nil)

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

;; Multiple cursors
(rc/require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; Convenient bindings for query-replace operations
(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-c R") 'query-replace-regexp)

;; Keybinding to cycle whitespace in region or point
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Dired
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-mouse-drag-files t)

;; Enable automatic insertion of matching brackets, braces, and quotes globally
(electric-pair-mode 1)

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-j")
                            (quote eval-print-last-sexp))))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

;; Yasnippet
(rc/require 'yasnippet)

(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.config/emacs/snippets/"))

(yas-global-mode 1)

;; Company
(rc/require 'company)

(global-company-mode)

(add-hook 'tuareg-mode-hook
          (lambda ()
            (interactive)
            (company-mode 0)))

;; Smarter on-demand completion using hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs))


;;; Move Text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; Whitespace mode
(setq whitespace-style '(face tabs spaces trailing space-before-tab space-after-tab space-mark tab-mark))
(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'before-save-hook 'delete-trailing-whitespace))

(add-hook 'tuareg-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c++-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'java-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'rust-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'scala-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'haskell-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'erlang-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'asm-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'nim-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'porth-mode-hook 'rc/set-up-whitespace-handling)

;; Nxml
(add-to-list 'auto-mode-alist '("\\.html\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.ant\\'" . nxml-mode))

;; Eldoc mode
(defun rc/turn-on-eldoc-mode ()
  (interactive)
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'rc/turn-on-eldoc-mode)

;;; Ebisp
(add-to-list 'auto-mode-alist '("\\.ebi\\'" . lisp-mode))

;; C-mode
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

;; Typescript
(rc/require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-mode))

;;; Packages that don't require configuration
(rc/require
 'scala-mode
 'd-mode
 'yaml-mode
 'glsl-mode
 'tuareg
 'lua-mode
 'less-css-mode
 'graphviz-dot-mode
 'clojure-mode
 'cmake-mode
 'rust-mode
 'csharp-mode
 'nim-mode
 'jinja2-mode
 'markdown-mode
 'purescript-mode
 'nix-mode
 'dockerfile-mode
 'toml-mode
 'nginx-mode
 'kotlin-mode
 'go-mode
 'php-mode
 'racket-mode
 'qml-mode
 'ag
 'elpy
 'typescript-mode
 'rfc-mode
 'sml-mode
 'zig-mode
 )
