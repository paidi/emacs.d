(require 'cl)

;; Set package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)

;; Load and activate emacs packages
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(ac-nrepl
    ac-slime
    auto-complete
    
    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking  

    ;; Syntax highlighting for CSS
    css-mode
    
    ;; syntax highlighting for CUDA
    cuda-mode
    
    ;; emacs mode for quicly browsing, filtering and editing directories
    ;; of plain text nodes
    deft
    
    ;; Emacs Speaks Statistics
    ess
    
    ;; Select regions by semantic units
    expand-region
    
    ;; Emacs paste mode for github gists
    gist
    
    ;; Minor mode for running gradle from within emacs
    gradle-mode

    ;; Emacs mode for Haskell
    haskell-mode

    ;; Provides a REPL for ruby
    inf-ruby
    
    ;; emacs major mode for editing lua
    lua-mode
    
    ;; Awesome emacs mode for git
    magit

    ;; Major mode for editing Markdown formatted text
    markdown-mode

    ;; Minor mode for structuring editing of S-mode data
    paredit

    ;; Major mode for python development
    python-mode
    
    ;; Minor mode for displaying strings representing colours
    rainbow-mode
    
    ;; Scala mode
    scala-mode

    ;; Visual feedback for changes to the buffer
    volatile-highlights
    
    ;; YAML mode
    yaml-mode)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;;
;; Customizations
;;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")


;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Magit customisation
(load "setup-magit.el")

;; Language-specific
(load "setup-clojure.el")

;; Colour mach parens and other structure characters to make code easy to follow
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Organizer/todo.org"))))

;; use c-mode for OpenCL and Cuda files
(setq auto-mode-alist (cons '("\.cl$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.cu$" . cuda-mode) auto-mode-alist))

(setq-default lua-indent-level 3)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; livedown
(require 'livedown)

;; Jekyll
(require 'hyde)
