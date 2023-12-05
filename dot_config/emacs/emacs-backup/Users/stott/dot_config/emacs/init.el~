;;
;; General Look and feel
;;

;; Remove startup message
(setq inhibit-startup-messsage t)

;; Turn off out of the box defaults
(scroll-bar-mode -1) 			; disable visible scrollbar
(tool-bar-mode -1)			; disable the toolbar
(tooltip-mode -1) 			; disable tooltips
(set-fringe-mode 10)			; give some room around the frames (px)
(menu-bar-mode -1)			; disable menu bar

;; Show the tab-bar as soon as tab-bar functions are invoked
(setq tab-bar-show 0)

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway
(setq default-frame-alist '((fullscreen . maximized)))

;; Add the time to the modeline, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setq display-time-format "%a %F %T")
(setq display-time-interval 1)
(display-time-mode)

(load-theme 'wombat)			; load theme

;;
;; Config Emacs
;;

;; Change from yes/no to y/n
(setq use-short-answers t)
;; Change exit to better y/n
(setq confirm-kill-emacs #'y-or-n-p)

;; Stop dired from keeping a buffer for each link/file
(setq dired-kill-when-opening-new-dired-buffer t)

;; Set calendar to y/m/d
(setq calendar-date-style 'iso)
;; Show trailing whitespace
(setq show-trailing-whitespace t)

;;
;; Tweaks interactions
;; 

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun cs--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.config/emacs/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setq make-backup-file-name-function 'cs--backup-file-name)

;; Make (mouse) right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;
;; Set up packages
;;

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Create log for tracking commands
(use-package command-log-mode)

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Set up Completion tool
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Add Counsel to use 
(use-package counsel)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

(use-package denote
  :init
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-known-keywords '("emacs" "pkm" "work" "home"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil))

;; Key bindings specifically for Dired.
(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-files)
  (define-key map (kbd "C-c C-d C-k") #'denote-dired-rename-marked-files-with-keywords)
  (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

;; Set the Journal date to the format I want
(setq denote-journal-extras-title-format 'day-date-month-year)

;; Also check the commands `denote-link-after-creating',
;; `denote-link-or-create'.  You may want to bind them to keys as well.

;; If you want to have Denote commands available via a right click
;; context menu, use the following and then enable
;; `context-menu-mode'.
(add-hook 'context-menu-functions #'denote-context-menu)

(use-package denote-menu)
(global-set-key (kbd "C-c z") #'list-denotes)
(define-key denote-menu-mode-map (kbd "c") #'denote-menu-clear-filters)
(define-key denote-menu-mode-map (kbd "/ r") #'denote-menu-filter)
(define-key denote-menu-mode-map (kbd "/ k") #'denote-menu-filter-by-keyword)
(define-key denote-menu-mode-map (kbd "/ o") #'denote-menu-filter-out-keyword)
(define-key denote-menu-mode-map (kbd "e") #'denote-menu-export-to-dired)

(use-package org
  :config
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))))

(use-package chezmoi)

;; (global-set-key (kbd "C-c C f")  #'chezmoi-find)
;; (global-set-key (kbd "C-c C s")  #'chezmoi-write)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(chezmoi which-key doom-modeline denote-menu counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
