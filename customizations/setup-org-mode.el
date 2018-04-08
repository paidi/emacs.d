;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-latex-create-formula-image-program 'dvipng)
(setq org-default-notes-file (concat (getenv "HOME") "/Dropbox/org/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-agenda-files (concat (getenv "HOME") "/Dropbox/org/notes.org"))
