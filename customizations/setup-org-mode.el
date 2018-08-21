;; org-mode
(require 'org)
(require 'org-trello)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Setup capture
(setq org-default-notes-file (concat (getenv "HOME") "/Dropbox/org/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;; Persist clock (for time logging)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
