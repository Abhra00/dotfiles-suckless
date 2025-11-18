;; -*- lexical-binding: t; -*-
;;; init.el --- Enhanced Lean Emacs Configuration

;;; Commentary:
;; A modern, lean Emacs config focusing on speed and simplicity.
;; No LSP, no tree-sitter - just fast, reliable editing.

;;; Code:

;;; ============================================================================
;;; Performance & Startup
;;; ============================================================================

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

;; Suppress compiler warnings
(setq native-comp-async-report-warnings-errors nil)

;;; ============================================================================
;;; UI Configuration
;;; ============================================================================

;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
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
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 210)

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

;;; Set theme
(add-to-list 'custom-theme-load-path
             (expand-file-name "~/.config/emacs/lisps/emacs.local/"))
(load-theme 'gruber-darker t)

;;; Add modeline padding
(defun rc/setup-modern-mode-line ()
  "Setup a spacious, padded modern mode-line."
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

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 25)
   (internal-border-width . 25)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-attribute 'fringe nil :inherit 'default)


;;; ============================================================================
;;; Editor Behavior
;;; ============================================================================

;; Enable line & column numbers
(column-number-mode 1)
(global-display-line-numbers-mode 1)

;; Make line numbers relative
(setq display-line-numbers-type 'relative)

;; Disable line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Save cursor positions across sessions
(save-place-mode 1)

;; Automatically refresh buffers when files change
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Highlight matching parens instantly
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Enable recent files menu
(recentf-mode 1)
(setq recentf-max-menu-items 50
      recentf-max-saved-items 100)

;; Exclude some directories from recentf
(add-to-list 'recentf-exclude "~/.config/emacs/elpa/.*")
(add-to-list 'recentf-exclude "~/.config/emacs/auto-saves/.*")
(add-to-list 'recentf-exclude "/tmp/.*")

;; Persist minibuffer history across sessions
(savehist-mode 1)

;; Undo/redo window configurations
(winner-mode 1)

;; Smoother scrolling
(setq scroll-margin 10
      scroll-conservatively 9999
      scroll-step 1)

;; Better backup and autosave management
(setq backup-directory-alist '(("." . "~/.config/emacs/backups"))
      auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-saves/" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Create backup directories if they don't exist
(make-directory "~/.config/emacs/backups" t)
(make-directory "~/.config/emacs/auto-saves" t)

;;; ============================================================================
;;; Completion Framework (Vertico + Orderless + Marginalia + Consult + Embark)
;;; ============================================================================

;; Vertico - Vertical completion UI
(rc/require 'vertico)
(vertico-mode +1)
(setq vertico-cycle t
      vertico-resize t
      vertico-count 15)

;; Vertico directory
(require 'vertico-directory)
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  (define-key vertico-map (kbd "C-l") #'vertico-directory-up)
)

;; Orderless - Flexible completion style (ESSENTIAL for modern Emacs)
(rc/require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion))))

;; Marginalia - Rich annotations in minibuffer
(rc/require 'marginalia)
(marginalia-mode +1)

;; Consult - Enhanced search and navigation commands
(rc/require 'consult)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)
(global-set-key (kbd "C-x r b") 'consult-bookmark)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "M-g g") 'consult-goto-line)
(global-set-key (kbd "M-g M-g") 'consult-goto-line)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-g I") 'consult-imenu-multi)
(global-set-key (kbd "C-c s") 'consult-ripgrep)
(global-set-key (kbd "C-c f") 'consult-find)

;; Embark - Contextual actions
(rc/require 'embark 'embark-consult)
(global-set-key (kbd "C-S-;") 'embark-act)
(global-set-key (kbd "C-S-,") 'embark-dwim)
(global-set-key (kbd "C-h B") 'embark-bindings)

;; Embark-Consult integration
(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; ============================================================================
;;; Project Management
;;; ============================================================================

(require 'project)
(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p s") #'project-find-regexp)
(global-set-key (kbd "C-c p d") #'project-dired)
(global-set-key (kbd "C-c p k") #'project-kill-buffers)

;;; ============================================================================
;;; Which-Key - Keybinding Discovery
;;; ============================================================================

(rc/require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.5
      which-key-popup-type 'minibuffer
      which-key-sort-order 'which-key-key-order-alpha)

;;; ============================================================================
;;; Buffer Management
;;; ============================================================================

;; Use ibuffer for improved buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Ibuffer settings
(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil)

;;; ============================================================================
;;; Org Mode
;;; ============================================================================

(load-file "~/.config/emacs/lisps/emacs.rc/org-mode-rc.el")

;;; ============================================================================
;;; Version Control - Magit
;;; ============================================================================

;; Load the sequence manipulation library
(require 'seq)

;; Magit
(rc/require 'magit)
(setq magit-auto-revert-mode nil
      magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)
(global-set-key (kbd "C-c m b") 'magit-blame)

;;; ============================================================================
;;; Multiple Cursors
;;; ============================================================================

(rc/require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;;; ============================================================================
;;; Search & Replace
;;; ============================================================================

;; Convenient bindings for query-replace operations
(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-c R") 'query-replace-regexp)

;; Keybinding to cycle whitespace in region or point
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;;; ============================================================================
;;; Dired
;;; ============================================================================

(require 'dired-x)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")
      dired-dwim-target t
      dired-listing-switches "-alh"
      dired-mouse-drag-files t
      dired-kill-when-opening-new-dired-buffer t)


;;; ============================================================================
;;; Code Editing
;;; ============================================================================

;; Enable automatic insertion of matching brackets, braces, and quotes globally
(electric-pair-mode 1)

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-j")
                            (quote eval-print-last-sexp))))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

;;; ============================================================================
;;; Snippets
;;; ============================================================================

(rc/require 'yasnippet)
(setq yas/triggers-in-field nil
      yas-snippet-dirs '("~/.config/emacs/snippets/"))
(yas-global-mode 1)

;;; ============================================================================
;;; Completion - Corfu + Cape
;;; ============================================================================

(rc/require 'corfu)
(global-corfu-mode)
(setq corfu-cycle t
      corfu-auto t
      corfu-auto-delay 0.1
      corfu-auto-prefix 1
      corfu-quit-no-match 'separator
      corfu-preview-current nil)

;; Corfu popupinfo for documentation
(add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
(setq corfu-popupinfo-delay '(0.5 . 0.2))

;; Cape - Additional completion sources
(rc/require 'cape)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; Smarter on-demand completion using hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line))

(global-set-key (kbd "M-/") #'hippie-expand)

;;; ============================================================================
;;; Navigation - Dumb Jump (LSP Alternative)
;;; ============================================================================

(rc/require 'dumb-jump)
(require 'xref)
(require 'seq)
(require 'ring)
(require 'tramp)
(require 'org)
(setq dumb-jump-prefer-searcher 'rg
      dumb-jump-aggressive nil
      dumb-jump-selector 'completing-read)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(global-set-key (kbd "M-.") #'xref-find-definitions)
(global-set-key (kbd "M-,") #'xref-go-back)
(global-set-key (kbd "M-?") #'xref-find-references)

;;; ============================================================================
;;; Documentation - DevDocs
;;; ============================================================================

(rc/require 'devdocs)
(global-set-key (kbd "C-h D") 'devdocs-lookup)

;;; ============================================================================
;;; Text Manipulation
;;; ============================================================================

;;; Move Text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;;; ============================================================================
;;; Whitespace Management
;;; ============================================================================

(setq whitespace-style
      '(face tabs spaces trailing space-before-tab space-after-tab space-mark tab-mark))

(defun rc/set-up-whitespace-handling ()
  "Enable whitespace mode for programming."
  (interactive)
  (whitespace-mode 1))

(dolist (hook '(tuareg-mode-hook
                c++-mode-hook
                c-mode-hook
                emacs-lisp-mode-hook
                java-mode-hook
                lua-mode-hook
                rust-mode-hook
                scala-mode-hook
                markdown-mode-hook
                haskell-mode-hook
                python-mode-hook
                erlang-mode-hook
                asm-mode-hook
                go-mode-hook
                nim-mode-hook
                yaml-mode-hook
                porth-mode-hook))
  (add-hook hook #'rc/set-up-whitespace-handling))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;; ============================================================================
;;; Language Modes
;;; ============================================================================

;; Nxml
(add-to-list 'auto-mode-alist '("\\.html\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.ant\\'" . nxml-mode))

;; Eldoc mode
(defun rc/turn-on-eldoc-mode ()
  "Enable eldoc mode."
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

;;; ============================================================================
;;; Language Modes (No Configuration Required)
;;; ============================================================================

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

;;; ============================================================================
;;; End of Configuration
;;; ============================================================================

;;; init.el ends here
