;; Speed up startup by increasing GC threshold early
(setq gc-cons-threshold 100000000) ;; 100MB during startup

;; Early UI element disabling (optional if already disabled in init.el)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
