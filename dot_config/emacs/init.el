;;
;; OS Specific Tweaks
;;

;; Turn off warning message about feature not supported by ls
(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

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
;; Show the column in tab-bar
(setq column-number-mode t)

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
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

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

(load-file "~/.config/emacs/sdcv-mode.el")
(global-set-key (kbd "C-c s") 'sdcv-search)

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.  Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings and comments get checked.  All other buffers get `flyspell-mode' to check all text.  If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
	(if (derived-mode-p 'prog-mode)
	   (progn
	     (message "Flyspell on (code)")
	     (flyspell-prog-mode))
	 ;; else
	 (progn
	   (message "Flyspell on (text)")
	   (flyspell-mode 1)))
	;; I tried putting (flyspell-buffer) here but it didn't seem to work
  )))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
	(message "Flyspell off")
	(flyspell-mode -1))
   ; else - flyspell is off, turn it on
    (flyspell-on-for-buffer-type)))
(global-set-key (kbd "C-c f") 'flyspell-toggle)

;; Set up borrowed from https://leahneukirchen.org/blog/archive/2022/03/note-taking-in-emacs-with-howm.html
(use-package howm)
;; Directory configuration
(setq howm-home-directory "~/Documents/notes/howm/")
(setq howm-directory "~/Documents/notes//howm/")
(setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
(setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.md")

;; Use ripgrep as grep
(setq howm-view-use-grep t)
(setq howm-view-grep-command "rg")
(setq howm-view-grep-option "-nH --no-heading --color never")
(setq howm-view-grep-extended-option nil)
(setq howm-view-grep-fixed-option "-F")
(setq howm-view-grep-expr-option nil)
(setq howm-view-grep-file-stdin-option nil)

;; counsel-rg for howm
(defun howm-list--counsel-rg (match)
  (if (string= match "")
  (howm-list-all)
(if (or (null ivy--old-cands)
	(equal ivy--old-cands '("No matches found")))
        (message "No match")
  (let ((howm-view-use-grep
	 #'(lambda (str file-list &optional fixed-p force-case-fold)
                 (mapcar
                  (lambda (cand)
		(if (string-match "\\`\\(.*\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
                        (let ((file (match-string-no-properties 1 cand))
			  (line (match-string-no-properties 2 cand))
			  (match-line (match-string-no-properties 3 cand)))
                          (list (expand-file-name file howm-directory)
                                (string-to-number line)
                                match-line))))
                  ivy--old-cands))))
        (howm-search ivy--old-re t)
        (riffle-set-place
     (1+ (cl-position match ivy--old-cands :test 'string=)))))))

(defun howm-counsel-rg ()
  "Interactively grep for a string in your howm notes using rg."
  (interactive)
  (let ((default-directory howm-directory)
        (counsel-ag-base-command counsel-rg-base-command)
        (counsel-ag-command (counsel--format-ag-command "--glob=!*~" "%s")))
    (ivy-read "Search all (rg): "
	      #'counsel-ag-function
	      :dynamic-collection t
	      :keymap counsel-ag-map
	      :action #'howm-list--counsel-rg
	      :require-match t
	      :caller 'counsel-rg)))

(define-key global-map (concat howm-prefix "r") 'howm-counsel-rg)

;; Default recent to sorting by mtime
(advice-add 'howm-list-recent :after #'howm-view-sort-by-mtime)
;; Default all to sorting by creation, newest first
(advice-add 'howm-list-all :after #'(lambda () (howm-view-sort-by-date t)))

;; Rename buffers to their title
(add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
(add-hook 'after-save-hook 'howm-mode-set-buffer-name)

;; Make it easy to navigate in howm mode
(add-hook 'howm-mode-hook 'orgalist-mode)

;; Stop hown from breaking help
(define-key howm-menu-mode-map "\C-h" nil)
(define-key riffle-summary-mode-map "\C-h" nil)
(define-key howm-view-contents-mode-map "\C-h" nil)

;; make wiki-links jump to single title hit if possible
(add-to-list 'action-lock-default-rules
             (list howm-wiki-regexp
                   (lambda (&optional dummy)
                     (let ((s (match-string-no-properties howm-wiki-regexp-pos)))
                       ;; letting create-p be nil here, howm-keyword-search-subr
                       ;; should check create-p after open-unique-p
                       (howm-keyword-search (concat "= " s) nil t)))
                   howm-wiki-regexp-hilit-pos))

;; Set up hown and calendar - from https://www.emacswiki.org/emacs/HowmAndCalendar
;; needed to chanage "string-to-int" to "string-to-number"
(require 'calendar)
(require 'howm-mode)
(setq
 calendar-date-display-form
 '("[" year "-" (format "%02d" (string-to-number month))
   "-" (format "%02d" (string-to-number day)) "]"))
(setq diary-file
      (expand-file-name "diary" howm-directory))

(defun howm-mark-calendar-date ()
  (interactive)
  (require 'howm-reminder)
  (let* ((today (howm-reminder-today 0))
         (limit (howm-reminder-today 1))
         (howm-schedule-types
          howm-schedule-menu-types)
         (raw (howm-reminder-search
               howm-schedule-types))
         (str nil) (yy nil) (mm nil) (dd nil))
    (while raw
      (setq str (nth 1 (car raw)))
      (when
          (string-match
           "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)"
           str)
        (setq yy (match-string 1 str))
        (setq mm (match-string 2 str))
        (setq dd (match-string 3 str)))
      (when (and yy mm dd)
        (mark-calendar-date-pattern
         (string-to-number mm)
         (string-to-number dd)
         (string-to-number yy)))
      (setq mm nil)
      (setq dd nil)
      (setq yy nil)
      (setq raw (cdr raw))
      )))

(defadvice mark-diary-entries
  (after mark-howm-entry activate)
  (howm-mark-calendar-date))

(setq
 howm-menu-display-rules
 (cons
  (cons "%hdiary[\n]?" 'howm-menu-diary)
  howm-menu-display-rules
   ))

(defun howm-menu-diary ()
  (require 'diary-lib)
  (message "scanning diary...")
  (delete-region
   (match-beginning 0) (match-end 0))
  (let* ((now (decode-time (current-time)))
         (diary-date
          (list (nth 4 now) (nth 3 now) (nth 5 now)))
         (diary-display-hook 'ignore)
         (cbuf (current-buffer))
         (howm-diary-entry nil)
         (howm-diary-entry-day nil)
         (str nil))
    (unwind-protect
        (setq howm-diary-entry
              (list-diary-entries
               diary-date howm-menu-schedule-days))
      (save-excursion
        (set-buffer
         (find-buffer-visiting diary-file))
        (subst-char-in-region
         (point-min) (point-max) ?\^M ?\n t)
        (setq selective-display nil)))

    (while howm-diary-entry
      (setq howm-diary-entry-day (car howm-diary-entry))
      (setq mm (nth 0 (car howm-diary-entry-day)))
      (setq dd (nth 1 (car howm-diary-entry-day)))
      (setq yy (nth 2 (car howm-diary-entry-day)))
      (setq str (nth 1 howm-diary-entry-day))
      (setq howm-diary-entry (cdr howm-diary-entry))
      (insert
       (format
        ">>d [%04d-%02d-%02d] %s\n" yy mm dd str))))
  (message "scanning diary...done")
  )

(setq diary-date-forms
      '((month "/" day "[^/0-9]")
        (month "/" day "/" year "[^0-9]")
        ("\\[" year "-" month "-" day "\\]" "[^0-9]")
        (monthname " *" day "[^,0-9]")
        (monthname " *" day ", *" year "[^0-9]")
        (dayname "\\W")))

(defun howm-open-diary (&optional dummy)
  (interactive)
  (let ((date-str nil) (str nil))
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward
             ">>d \\(\\[[-0-9]+\\]\\) " nil t)
        (setq str
              (concat
               "^.+"
               (buffer-substring-no-properties
                (point) (line-end-position))))
        (setq date-str
              (concat
               "^.+"
               (buffer-substring-no-properties
                (match-beginning 1)
                (match-end 1))
               " " str))
        (find-file
         (substitute-in-file-name diary-file))
        (howm-mode t)
        (goto-char (point-min))
        (if (re-search-forward date-str nil t)
            ()
          (re-search-forward str nil t))))))

(defun add-diary-action-lock-rule ()
  (let ((rule
         (action-lock-general
          'howm-open-diary
          "^\\(>>d\\) "
          1 1)))
    (if (not (member rule action-lock-default-rules))
        (progn
          (setq action-lock-default-rules
                (cons rule action-lock-default-rules))
          (action-lock-set-rules
           action-lock-default-rules)))))

(add-hook 'action-lock-mode-on-hook
          'add-diary-action-lock-rule)

(defadvice make-diary-entry
  (after howm-mode activate)
  (text-mode)
  (howm-mode t))


;;;;;;;;;;

;; M-x calendar, move cursor to a certain date, and
;; M-x howm-from-calendar to search that date in howm notes.
(defun howm-from-calendar ()
  (interactive)
  (require 'howm-mode)
  (let* ((mdy (calendar-cursor-to-date t))
         (m (car mdy))
         (d (second mdy))
         (y (third mdy))
         (key (format-time-string
               howm-date-format
               (encode-time 0 0 0 d m y))))
    (howm-keyword-search key)))

;; Bind howm-from-calendar to "d" key.
(add-hook 'initial-calendar-window-hook
          #'(lambda ()
             (local-set-key
              "d" 'howm-from-calendar)))

 	

;; Type "d" in howm menu to open calendar.
(add-hook 'howm-menu-hook
          #'(lambda ()
             (local-set-key "d" 'calendar)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(orgalist chezmoi which-key denote-menu counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
