;;; gruber-darker-theme.el --- Gruber Darker color theme for Emacs 30+.

;; Copyright (C) 2025-2026 Abhra Mondal a.k.a bugs
;; Copyright (C) 2013-2016 Alexey Kutepov a.k.a rexim
;; Copyright (C) 2009-2010 Jason R. Blevins

;; Author: Abhra Mondal <abhramondal9674@gmail.com>
;; URL: http://github.com/Abhra00/gruber-darker-theme
;; Version: 0.8

;; NOTE: This is a modernized and cleaned-up version of the original
;; Gruber Darker theme. It fixes typos, removes obsolete faces, and
;; adds styling for several commonly-used modern packages (vertico,
;; marginalia, corfu, eglot/lsp, tab-line, completions, etc.).

;;; License: MIT (same as original)

(deftheme gruber-darker
  "Gruber Darker color theme for Emacs 30+")

(let ((gruber-darker-fg        "#e4e4ef")
      (gruber-darker-fg+1      "#f4f4ff")
      (gruber-darker-fg+2      "#f5f5f5")
      (gruber-darker-white     "#ffffff")
      (gruber-darker-black     "#000000")
      (gruber-darker-bg-1      "#101010")
      (gruber-darker-bg        "#181818")
      (gruber-darker-bg+1      "#282828")
      (gruber-darker-bg+2      "#453d41")
      (gruber-darker-bg+3      "#484848")
      (gruber-darker-bg+4      "#52494e")
      (gruber-darker-red-1     "#c73c3f")
      (gruber-darker-red       "#f43841")
      (gruber-darker-red+1     "#ff4f58")
      (gruber-darker-green     "#73c936")
      (gruber-darker-yellow    "#ffdd33")
      (gruber-darker-blue      "#4f94cd")
      (gruber-darker-purple    "#a020f0")
      (gruber-darker-brown     "#cc8c3c")
      (gruber-darker-quartz    "#95a99f")
      (gruber-darker-niagara-2 "#303540")
      (gruber-darker-niagara-1 "#565f73")
      (gruber-darker-niagara   "#96a6c8")
      (gruber-darker-wisteria  "#9e95c7")
      )


  (custom-theme-set-variables
   'gruber-darker
   '(frame-background-mode (quote dark)))


  (custom-theme-set-faces
   'gruber-darker


   ;; Basic
   `(default ((t (:foreground ,gruber-darker-fg :background ,gruber-darker-bg))))
   `(cursor ((t (:background ,gruber-darker-yellow))))
   `(fringe ((t (:background nil :foreground ,gruber-darker-bg+2))))
   `(vertical-border ((t (:foreground ,gruber-darker-bg+2))))
   `(link ((t (:foreground ,gruber-darker-niagara :underline t))))
   `(link-visited ((t (:foreground ,gruber-darker-wisteria :underline t))))
   `(shadow ((t (:foreground ,gruber-darker-bg+4))))
   `(minibuffer-prompt ((t (:foreground ,gruber-darker-niagara))))
   `(region ((t (:background ,gruber-darker-bg+3))))
   `(secondary-selection ((t (:background ,gruber-darker-bg+3))))
   `(trailing-whitespace ((t (:foreground ,gruber-darker-black :background ,gruber-darker-red))))
   `(tooltip ((t (:background ,gruber-darker-bg+4 :foreground ,gruber-darker-white))))


   ;; Frame/background helpers
   `(border ((t (:background ,gruber-darker-bg-1 :foreground ,gruber-darker-bg+2))))


   ;; Line/Highlighting
   `(hl-line ((t (:background ,gruber-darker-bg+1))))
   `(highlight ((t (:background ,gruber-darker-bg+1))))


   ;; Line numbers
   `(line-number ((t (:foreground ,gruber-darker-bg+4 :background ,gruber-darker-bg))))
   `(line-number-current-line ((t (:foreground ,gruber-darker-yellow :background ,gruber-darker-bg))))


   ;; Mode line
   `(mode-line ((t (:background ,gruber-darker-bg+1 :foreground ,gruber-darker-white))))
   `(mode-line-inactive ((t (:background ,gruber-darker-bg+1 :foreground ,gruber-darker-quartz))))
   `(mode-line-buffer-id ((t (:foreground ,gruber-darker-white :background ,gruber-darker-bg+1))))
   `(mode-line-highlight ((t (:box nil :foreground ,gruber-darker-yellow))))


   ;; Tab-line
   `(tab-line ((t (:background ,gruber-darker-bg+1 :foreground ,gruber-darker-bg+4))))
   `(tab-line-tab ((t (:background ,gruber-darker-bg :foreground ,gruber-darker-yellow :weight bold))))
   `(tab-line-tab-inactive ((t (:background ,gruber-darker-bg :foreground ,gruber-darker-bg+4))))


   ;; Completion / minibuffer helpers
   `(completions-annotations ((t (:inherit shadow))))
   `(completions-common-part ((t (:foreground ,gruber-darker-quartz))))
   `(completions-first-difference ((t (:foreground ,gruber-darker-yellow))))


   ;; IDO
   `(ido-first-match ((t (:foreground ,gruber-darker-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,gruber-darker-green :weight bold))))
   `(ido-subdir ((t (:foreground ,gruber-darker-niagara))))
   `(ido-virtual ((t (:foreground ,gruber-darker-quartz))))
   `(ido-indicator ((t (:foreground ,gruber-darker-red+1))))


   ;; Smex
   `(smex-primary-selection ((t (:background ,gruber-darker-bg+2 :foreground ,gruber-darker-yellow))))
   `(smex-secondary-selection ((t (:background ,gruber-darker-bg+1 :foreground ,gruber-darker-niagara))))
   `(smex-highlight-face ((t (:foreground ,gruber-darker-green :weight bold))))


   ;; Dired
   `(dired-directory ((t (:foreground ,gruber-darker-niagara :weight bold))))
   `(dired-symlink ((t (:foreground ,gruber-darker-wisteria :weight bold))))
   `(dired-flagged ((t (:foreground ,gruber-darker-red))))
   `(dired-marked ((t (:foreground ,gruber-darker-yellow :weight bold))))
   `(dired-warning ((t (:foreground ,gruber-darker-red+1 :weight bold))))
   `(dired-header ((t (:foreground ,gruber-darker-niagara :weight bold))))
   `(dired-perm-write ((t (:foreground ,gruber-darker-brown))))
   `(dired-ignored ((t (:foreground ,gruber-darker-bg+4))))
   `(dired-broken-symlink ((t (:foreground ,gruber-darker-red+1 :weight bold))))


   ;; Vertico / Marginalia (popular completion UI)
   `(vertico-current ((t (:background ,gruber-darker-bg+1))))
   `(vertico-posframe ((t (:background ,gruber-darker-bg))))
   `(marginalia-documentation ((t (:foreground ,gruber-darker-quartz))))
   `(marginalia-key ((t (:foreground ,gruber-darker-niagara))))


   ;; Corfu (popup completion)
   `(corfu-default ((t (:background ,gruber-darker-bg+1 :foreground ,gruber-darker-fg))))
   `(corfu-current ((t (:background ,gruber-darker-bg+2 :foreground ,gruber-darker-white))))
   `(corfu-bar ((t (:background ,gruber-darker-niagara-1))))


   ;; Company
   `(company-tooltip ((t (:foreground ,gruber-darker-fg :background ,gruber-darker-bg+1))))
   `(company-tooltip-selection ((t (:foreground ,gruber-darker-fg :background ,gruber-darker-bg-1))))
   `(company-tooltip-common ((t (:foreground ,gruber-darker-yellow :weight bold))))
   `(company-tooltip-common-selection ((t (:foreground ,gruber-darker-yellow :weight bold))))
   `(company-tooltip-annotation ((t (:foreground ,gruber-darker-brown :background ,gruber-darker-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,gruber-darker-brown :background ,gruber-darker-bg-1))))
   `(company-tooltip-match ((t (:foreground ,gruber-darker-yellow :weight bold))))
   `(company-tooltip-match-selection ((t (:foreground ,gruber-darker-yellow :weight bold))))
   `(company-scrollbar-bg ((t (:background ,gruber-darker-bg+1))))
   `(company-scrollbar-fg ((t (:background ,gruber-darker-bg+4))))
   `(company-preview ((t (:foreground ,gruber-darker-quartz :background ,gruber-darker-bg))))
   `(company-preview-common ((t (:foreground ,gruber-darker-yellow :weight bold))))
   `(company-preview-search ((t (:foreground ,gruber-darker-red :weight bold))))


   ;; Completion group titles
   `(completions-group-title ((t (:foreground ,gruber-darker-niagara :weight bold))))


   ;; Compilation mode
   `(compilation-info ((t (:foreground ,gruber-darker-green :inherit unspecified))))
   `(compilation-warning ((t (:foreground ,gruber-darker-brown :weight bold :inherit unspecified))))
   `(compilation-error ((t (:foreground ,gruber-darker-red+1))))
   `(compilation-line-number ((t (:foreground ,gruber-darker-niagara))))
   `(compilation-column-number ((t (:foreground ,gruber-darker-niagara-1))))
   `(compilation-mode-line-exit ((t (:foreground ,gruber-darker-green :weight bold :inherit unspecified))))
   `(compilation-mode-line-fail ((t (:foreground ,gruber-darker-red :weight bold :inherit unspecified))))
   `(compilation-mode-line-run ((t (:foreground ,gruber-darker-brown :weight bold :inherit unspecified))))
   `(message-header-name ((t (:foreground ,gruber-darker-green))))


   ;; Org
   `(org-agenda-structure ((t (:foreground ,gruber-darker-niagara))))
   `(org-column ((t (:background ,gruber-darker-bg-1))))
   `(org-column-title ((t (:background ,gruber-darker-bg-1 :underline t :weight bold))))
   `(org-todo ((t (:foreground ,gruber-darker-black :background ,gruber-darker-red :weight bold))))
   `(org-done ((t (:foreground ,gruber-darker-black :background ,gruber-darker-green :weight bold))))
   `(org-upcoming-deadline ((t (:foreground ,gruber-darker-yellow))))


   ;; Font-lock / Syntax
   `(font-lock-builtin-face ((t (:foreground ,gruber-darker-yellow))))
   `(font-lock-comment-face ((t (:foreground ,gruber-darker-brown))))
   `(font-lock-constant-face ((t (:foreground ,gruber-darker-quartz))))
   `(font-lock-doc-face ((t (:foreground ,gruber-darker-green))))
   `(font-lock-function-name-face ((t (:foreground ,gruber-darker-niagara :weight normal))))
   `(font-lock-keyword-face ((t (:foreground ,gruber-darker-yellow :weight bold))))
   `(font-lock-string-face ((t (:foreground ,gruber-darker-green))))
   `(font-lock-type-face ((t (:foreground ,gruber-darker-quartz))))
   `(font-lock-variable-name-face ((t (:foreground ,gruber-darker-fg+1))))
   `(font-lock-warning-face ((t (:foreground ,gruber-darker-red))))


   ;; Search
   `(isearch ((t (:foreground ,gruber-darker-black :background ,gruber-darker-fg+2))))
   `(isearch-fail ((t (:foreground ,gruber-darker-black :background ,gruber-darker-red))))
   `(lazy-highlight ((t (:foreground ,gruber-darker-fg+1 :background ,gruber-darker-niagara-1))))


   ;; Show paren
   `(show-paren-match ((t (:background ,gruber-darker-blue :foreground ,gruber-darker-fg :weight normal))))
   `(show-paren-mismatch ((t (:background ,gruber-darker-purple :foreground ,gruber-darker-fg :weight normal))))


   ;; Diff / VC
   `(diff-added ((t (:foreground ,gruber-darker-green))))
   `(diff-removed ((t (:foreground ,gruber-darker-red+1))))
   `(diff-refine-added ((t (:background ,gruber-darker-green :foreground ,gruber-darker-bg))))
   `(diff-refine-removed ((t (:background ,gruber-darker-red :foreground ,gruber-darker-bg))))


   ;; Compilation / Messages / Messages
   `(compilation-error ((t (:foreground ,gruber-darker-red+1 :weight bold))))
   `(compilation-warning ((t (:foreground ,gruber-darker-brown :weight bold))))
   `(compilation-info ((t (:foreground ,gruber-darker-green))))
   `(message-header-name ((t (:foreground ,gruber-darker-green))))


   ;; Flymake / Flyspell (fix typos and use correct face names)
   `(flymake-error ((t (:underline (:style wave :color ,gruber-darker-red)))))
   `(flymake-warning ((t (:underline (:style wave :color ,gruber-darker-yellow)))))
   `(flymake-note ((t (:underline (:style wave :color ,gruber-darker-green)))))
   `(flyspell-incorrect ((t (:underline (:style wave :color ,gruber-darker-red)))))
   `(flyspell-duplicate ((t (:underline (:style wave :color ,gruber-darker-yellow)))))


   ;; Whitespace
   `(whitespace-space ((t (:background ,gruber-darker-bg :foreground ,gruber-darker-bg+1))))
   `(whitespace-tab ((t (:background ,gruber-darker-bg :foreground ,gruber-darker-bg+1))))
   `(whitespace-line ((t (:background ,gruber-darker-bg+2 :foreground ,gruber-darker-red+1))))
   `(whitespace-trailing ((t (:background ,gruber-darker-red :foreground ,gruber-darker-red))))


   ;; Shell / Term
   `(term-color-black ((t (:foreground ,gruber-darker-bg+3 :background ,gruber-darker-bg+4))))
   `(term-color-red ((t (:foreground ,gruber-darker-red-1 :background ,gruber-darker-red-1))))
   `(term-color-green ((t (:foreground ,gruber-darker-green :background ,gruber-darker-green))))
   `(term-color-blue ((t (:foreground ,gruber-darker-niagara :background ,gruber-darker-niagara))))
   `(term-color-yellow ((t (:foreground ,gruber-darker-yellow :background ,gruber-darker-yellow))))


   ;; Helm (kept for users who use helm)
   `(helm-selection ((t (:background ,gruber-darker-bg+1))))
   `(helm-source-header ((t (:foreground ,gruber-darker-yellow :background ,gruber-darker-bg :weight bold :box (:line-width -1 :style released-button)))))


   ;; Eglot / LSP
   `(eglot-highlight-symbol-face ((t (:background ,gruber-darker-niagara-2))))
   `(lsp-face-highlight-textual ((t (:background ,gruber-darker-niagara-2))))
   `(lsp-face-highlight-read ((t (:background ,gruber-darker-niagara-2))))


   ;; Which function
   `(which-func ((t (:foreground ,gruber-darker-wisteria))))


   ;; Which-key / help
   `(which-key-key-face ((t (:foreground ,gruber-darker-niagara))))
   `(which-key-command-description-face ((t (:foreground ,gruber-darker-quartz))))


   ;; Completion orderless
   `(orderless-match-face-0 ((t (:foreground ,gruber-darker-yellow))))
   `(orderless-match-face-1 ((t (:foreground ,gruber-darker-green))))
   `(orderless-match-face-2 ((t (:foreground ,gruber-darker-brown))))
   `(orderless-match-face-3 ((t (:foreground ,gruber-darker-quartz))))


   ;; Error / Warning / Success
   `(error ((t (:foreground ,gruber-darker-red))))
   `(warning ((t (:foreground ,gruber-darker-yellow))))
   `(success ((t (:foreground ,gruber-darker-green))))


   ;; Provide a fallback for older/less-common faces
   `(default-italic ((t (:slant italic :foreground ,gruber-darker-fg))))


  ;; Magit faces
  `(magit-section-heading ((t (:foreground ,gruber-darker-niagara :weight bold))))
  `(magit-section-highlight ((t (:background ,gruber-darker-bg+1))))
  `(magit-section-heading-selection ((t (:foreground ,gruber-darker-yellow :background ,gruber-darker-bg+2 :weight bold))))
  `(magit-diff-added ((t (:foreground ,gruber-darker-green))))
  `(magit-diff-added-highlight ((t (:background ,gruber-darker-bg+2 :foreground ,gruber-darker-green))))
  `(magit-diff-removed ((t (:foreground ,gruber-darker-red+1))))
  `(magit-diff-removed-highlight ((t (:background ,gruber-darker-bg+2 :foreground ,gruber-darker-red+1))))
  `(magit-diff-context ((t (:foreground ,gruber-darker-quartz))))
  `(magit-diff-context-highlight ((t (:background ,gruber-darker-bg+1 :foreground ,gruber-darker-quartz))))
  `(magit-diff-hunk-heading ((t (:background ,gruber-darker-bg+2 :foreground ,gruber-darker-fg))))
  `(magit-diff-hunk-heading-highlight ((t (:background ,gruber-darker-bg+3 :foreground ,gruber-darker-fg))))
  `(magit-branch-local ((t (:foreground ,gruber-darker-niagara))))
  `(magit-branch-remote ((t (:foreground ,gruber-darker-wisteria))))
  `(magit-branch-current ((t (:foreground ,gruber-darker-yellow :weight bold))))
  `(magit-header ((t (:foreground ,gruber-darker-yellow :weight bold))))
  `(magit-hash ((t (:foreground ,gruber-darker-quartz))))
  `(magit-tag ((t (:foreground ,gruber-darker-yellow))))
  `(magit-log-author ((t (:foreground ,gruber-darker-brown))))
  `(magit-log-date ((t (:foreground ,gruber-darker-quartz))))
  `(magit-log-graph ((t (:foreground ,gruber-darker-niagara-1))))


  ;; Org-modern integration
  `(org-modern-heading ((t (:foreground ,gruber-darker-niagara :weight bold))))
  `(org-modern-checkbox ((t (:foreground ,gruber-darker-yellow))))
  `(org-modern-date ((t (:foreground ,gruber-darker-quartz))))
  `(org-modern-label ((t (:foreground ,gruber-darker-wisteria))))
  `(org-modern-tag ((t (:foreground ,gruber-darker-brown :background ,gruber-darker-bg+2))))
  `(org-modern-statistics ((t (:foreground ,gruber-darker-green))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'gruber-darker)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; gruber-darker-theme.el ends here.
