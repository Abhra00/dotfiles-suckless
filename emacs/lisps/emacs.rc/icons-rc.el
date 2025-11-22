;;; icons-rc.el --- Icons for Dired, Eshell, and Marginalia -*- lexical-binding: t; -*-

;;; Code:

(require 'dired)
(require 'eshell)

;;; Icon Definitions
(defvar rc/icons-file-icons
  '(("el" . "")       ("rb" . "")       ("js" . "")      ("ts" . "")
    ("json" . "")     ("md" . "")       ("txt" . "")     ("html" . "")
    ("css" . "")      ("scss" . "")     ("png" . "")     ("jpg" . "")
    ("jpeg" . "")     ("gif" . "")      ("svg" . "")     ("pdf" . "")
    ("zip" . "")      ("tar" . "")      ("gz" . "")      ("bz2" . "")
    ("7z" . "")       ("org" . "")      ("sh" . "")      ("c" . "")
    ("h" . "")        ("cpp" . "")      ("hpp" . "")     ("py" . "")
    ("java" . "")    ("go" . "")       ("rs" . "")      ("php" . "")
    ("pl" . "")       ("lua" . "")      ("ps1" . "")     ("exe" . "")
    ("dll" . "")      ("bat" . "")     ("yaml" . "")    ("toml" . "")
    ("ini" . "")      ("csv" . "")      ("xls" . "")     ("xlsx" . "")
    ("sql" . "")      ("log" . "")      ("apk" . "")     ("dmg" . "")
    ("iso" . "")      ("torrent" . "")  ("bak" . "")     ("tmp" . "")
    ("desktop" . "")  ("md5" . "")      ("sha256" . "")  ("pem" . "")
    ("sqlite" . "")   ("db" . "")       ("gpg" . "")     ("hash" . "")
    ("mp3" . "")      ("wav" . "")      ("flac" . "" )   ("mail" . "")
    ("ogg" . "")      ("m4a" . "")      ("mp4" . "")     ("avi" . "")
    ("mov" . "")      ("mkv" . "")      ("webm" . "")    ("flv" . "")
    ("ico" . "")      ("ttf" . "")      ("otf" . "")     ("eot" . "")
    ("woff" . "")     ("woff2" . "")    ("epub" . "")    ("mobi" . "")
    ("azw3" . "")     ("fb2" . "")      ("chm" . "")     ("tex" . "")
    ("bib" . "")      ("rar" . "")     ("xz" . "")
    ("zst" . "")      ("tar.xz" . "")   ("tar.zst" . "") ("tar.gz" . "")
    ("tgz" . "")      ("bz2" . "")      ("mpg" . "")     ("webp" . "")
    ("flv" . "")      ("3gp" . "")      ("ogv" . "")     ("srt" . "")
    ("vtt" . "")      ("cue" . "")      ("terminal" . "") ("info" . "ℹ")
    ("direddir" . "") ("diredfile" . "") ("wranch" . "") ("news" . "")))

;;; Dired Icons
(defun rc/icons-dired-icon-for-file (file)
  "Return icon for FILE based on extension or directory status."
  (if (file-directory-p file)
      (assoc-default "direddir" rc/icons-file-icons)
    (let* ((ext (file-name-extension file))
           (icon (and ext (assoc-default (downcase ext) rc/icons-file-icons))))
      (or icon (assoc-default "diredfile" rc/icons-file-icons)))))

(defun rc/icons-dired-icons-regexp ()
  "Return a regexp that matches any icon we use."
  (let ((icons (mapcar #'cdr rc/icons-file-icons)))
    (concat "^\\(" (regexp-opt (cons "" icons)) "\\) ")))

(defun rc/icons-dired-add-icons ()
  "Add icons and suffixes as overlays to filenames in Dired buffer."
  (when (derived-mode-p 'dired-mode)
    (let ((inhibit-read-only t)
          (icon-regex (rc/icons-dired-icons-regexp)))
      (remove-overlays (point-min) (point-max) 'rc-icons-dired-overlay t)

      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (condition-case nil
              (when-let* ((file (dired-get-filename nil t)))
                (dired-move-to-filename)
                (let* ((beg (point))
                       (end (line-end-position))
                       (icon (rc/icons-dired-icon-for-file file))
                       (suffix
                        (cond
                         ((file-directory-p file)
                          (propertize "/" 'face 'dired-directory))
                         ((file-executable-p file)
                          (propertize "*" 'face '(:foreground "#79a8ff")))
                         (t ""))))
                  ;; Add icon before filename
                  (let ((ov1 (make-overlay beg beg)))
                    (overlay-put ov1 'before-string (concat icon " "))
                    (overlay-put ov1 'rc-icons-dired-overlay t))
                  ;; Add styled suffix after filename
                  (let ((ov2 (make-overlay end end)))
                    (overlay-put ov2 'after-string suffix)
                    (overlay-put ov2 'rc-icons-dired-overlay t))))
            (error nil))
          (forward-line 1))))))

(add-hook 'dired-after-readin-hook #'rc/icons-dired-add-icons)

;;; Eshell Icons
(defun rc/icons-eshell-annotate (file)
  "Return a cons of propertized display string and file metadata.
FILE is a list (NAME IS-DIR EXECUTABLE ...), like from `eshell/ls`.
The full list is like:
(FILENAME IS-DIR SIZE OWNER GROUP MOD-TIME ACCESS-TIME CHANGE-TIME
SIZE-LONG PERMS HARDLINKS INODE DEVICE)."
  (let* ((filename (car file))
         (is-dir (eq (cadr file) t))
         (perms (nth 9 file))
         (is-exec (and perms (string-match-p "x" perms)))
         (ext (and (not is-dir) (file-name-extension filename)))
         (icon (if is-dir
                   (cdr (assoc "direddir" rc/icons-file-icons))
                 (or (cdr (assoc ext rc/icons-file-icons))
                     (cdr (assoc "diredfile" rc/icons-file-icons)))))
         (suffix (cond
                  (is-dir "/")
                  (is-exec "*")
                  (t "")))
         (display-text (propertize
                        (concat icon " " filename suffix)
                        'file-name filename
                        'mouse-face 'highlight
                        'help-echo (concat "Open " filename)
                        'keymap rc/icons-eshell-ls-keymap)))
    (cons display-text (cdr file))))

(defvar rc/icons-eshell-ls-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'rc/icons-eshell-ls-find-file)
    (define-key map (kbd "<return>") #'rc/icons-eshell-ls-find-file)
    (define-key map [mouse-1] #'rc/icons-eshell-ls-find-file)
    (define-key map (kbd "D") #'rc/icons-eshell-ls-delete-file)
    map)
  "Keymap active on Eshell file entries.")

(defun rc/icons-eshell-ls-file-at-point ()
  "Get the full path of the Eshell listing at point."
  (get-text-property (point) 'file-name))

(defun rc/icons-eshell-ls-find-file ()
  "Open the Eshell listing at point."
  (interactive)
  (find-file (rc/icons-eshell-ls-file-at-point)))

(defun rc/icons-eshell-ls-delete-file ()
  "Delete the Eshell listing at point."
  (interactive)
  (let ((file (rc/icons-eshell-ls-file-at-point)))
    (when (yes-or-no-p (format "Delete file %s?" file))
      (delete-file file 'trash))))

(advice-remove 'eshell-ls-decorated-name #'rc/icons-eshell-annotate)
(advice-add #'eshell-ls-annotate :filter-return #'rc/icons-eshell-annotate)

(provide 'icons-rc)
