;; Appearance and Startup Behavior

;(set-frame-parameter (selected-frame) 'alpha '(95 . 90))
;(add-to-list 'default-frame-alist '(alpha . (95 .90)))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-visual-line-mode 1) ; 1 for on, 0 for off.
(setq x-select-enable-clipboard t)
(server-start)
(add-to-list 'load-path "~/.emacs.d/lisp/")

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; open with single window
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook 'delete-other-windows)
(setq inhibit-startup-buffer-menu t)

;; create backup file directory
(defun make-backup-file-name (file)
(concat "~/.emacs_backups/" (file-name-nondirectory file)
"~"))

(add-hook 'text-mode-hook 'turn-on-flyspell) ; Turn on spell checking in text mode

;; (setq browse-url-browser-function 'browse-url-chrome
;;       browse-url-chrome-program "google-chrome-beta")

(setq browse-url-browser-function 'browse-url-firefox)

;; Automatically revert buffers
(global-auto-revert-mode 1)
(add-hook 'dired-mode-hook 'auto-revert-mode)


;; MELBA

(require 'package)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; Paradox

;;(require 'paradox)
;;(paradox-enable)


;; use-package
(eval-when-compile
  (require 'use-package))




;; recentf
(use-package recentf
  :ensure t
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
	recentf-max-menu-items 25)
  (run-at-time nil (* 10 60) 'recentf-save-list)
;;  :bind ("C-x C-r" . recentf-open-files)
  )


;; Themes
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (setf custom-safe-themes t)
    (color-theme-sanityinc-tomorrow-eighties)
  )

;; (use-package nord-theme
;;   :ensure t)

;; (use-package zenburn-theme
;;   :ensure t)


;; ido
(use-package ido
  :ensure t
  :config
;;  (setq ido-enable-flex-matching t)
;;  (setq ido-everywhere t)
  ;;  (ido-mode 1)
  )


;; org
(use-package org
  :ensure t
  :pin org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c r" . org-revert-all-org-buffers)
	 ("C-c l" . org-store-link))
  :config
  (progn
    (setq org-directory "~/OneDrive/org")
    (setq org-refile-targets (quote ((org-agenda-files :regexp . "*"))))
    (setq org-agenda-files
	  (quote
	   ("~/OneDrive/org/notes.org"
	    "~/OneDrive/org/work.org"
;;	    "~/OneDrive/org/gcal.org"
	    "~/OneDrive/org/calendar.org"
	    "~/OneDrive/org/home.org"
;;	    "~/OneDrive/org/research.org"
;;	    "~/OneDrive/org/peerReview.org"
	    "~/OneDrive/org/recipes.org"
	    )))
    (setq org-default-notes-file "~/OneDrive/org/notes.org")
    (setq org-agenda-custom-commands
          '(("c" "Agenda and TODO tasks"
             ((todo "TODAY")
	      (todo "NEXT")
	      (todo "TODO")
	      (todo "WAITING")
	      (agenda "")))
	    ("y" "Agenda and Today's tasks"
             ((todo "TODAY")
	      (agenda "")))
	    )
	  )
    (setq org-capture-templates
      (quote
       (("w"
         "Web Template"
         entry
         (file+headline "~/OneDrive/org/capture.org" "Notes")
         "* %^{Title}\n\n  Source: %:link, %:description\n\n  %i"
         :empty-lines 1)
	("t"
	 "TODO Template"
	 entry
	 (file+headline "~/OneDrive/org/capture.org" "Tasks")
	 "* TODO %?\n %i\n")
	("e"
	 "Email TODO Template"
	 entry
	 (file+headline "~/OneDrive/org/capture.org" "Tasks")
	 "* TODO %?\n %i\n %a")
	("c" "Cookbook" entry (file "~/OneDrive/org/cookbook.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ("m" "Manual Cookbook" entry (file "~/OneDrive/org/cookbook.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")
        ;; ... more templates here ...
        )))
    (setq org-file-apps
    '(("\\.docx\\'" . "lowriter %s")
      ("\\.x?html?\\'" . default)
      ("\\.pdf\\'" . default)
      (auto-mode . emacs)))
    ;;
    ;; Agenda customization from http://pragmaticemacs.com/emacs/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/
    ;;warn me of any deadlines in next 7 days
    (setq org-deadline-warning-days 21)
    ;;show me tasks scheduled or due in next fortnight
    (setq org-agenda-span (quote week))
    ;;don't show tasks as scheduled if they are already shown as a deadline
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    ;;don't give awarning colour to tasks with impending deadlines
    ;;if they are scheduled to be done
    (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
    ;;sort tasks in order of when they are due and then by priority
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-sorting-strategy
	  (quote
	   ((agenda time-up deadline-up priority-down)
	    (todo priority-down category-keep)
	    (tags priority-down category-keep)
	    (search category-keep))))
    ;; Clocking
    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)
    ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
    (setq org-clock-history-length 23)
    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)
    ;; Separate drawers for clocking and logs
    (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
    ;; Save clock data and state changes and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)
    ;; Clock out when moving task to a done state
    (setq org-clock-out-when-done t)
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    ;; Do not prompt to resume an active clock
    (setq org-clock-persist-query-resume nil)
    ;; Enable auto clock resolution for finding open clocks
    (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
    ;; Include current clocking task in clock reports
    (setq org-clock-report-include-clocking-task t)
    )
  )


(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defvar bh/organization-task-id "ab6d0d4d-97d8-4239-8d92-9a4fc4cd4431")

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)


(use-package org-habit
  :ensure nil)

(require `org-protocol)


(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode)
  (setq org-agenda-custom-commands
	'(("c" "Agenda and TODO tasks"
	   ((todo "TODAY")
	    (todo "NEXT")
	    (todo "TODO")
	    (todo "WAITING")
	    (agenda "")))
	  ("o" "Grouped TODO tasks"
	   ((todo "TODAY")
	    (tags-todo "+TASKS-DONE")
	    (todo "NEXT")
	    (tags-todo "+REVIEWS-DONE")
	    (todo "TODO")
	    (todo "WAITING")))
	  ("y" "Agenda and Today's tasks"
	   ((todo "TODAY")
	    (agenda "")))
	  ("s" "Super Agenda" agenda
	   (org-super-agenda-mode)
	   ((org-agenda-span 'day)
	    (org-super-agenda-groups
	     '(
	       (:name "Today's Items"
		      :time-grid f
		      :todo "TODAY")
	       (:name "Important"
		      :priority "A")
	       (:name "Reviews"
		      :tag "REVIEWS")
	       (:name "Next Things"
		      :todo "NEXT")
	       (:name "Habits"
		      :habit t)
	       (:name "Waiting"
		      :todo "WAITING")
	       (:discard (:anything))
	       )))
	   (org-agenda nil "a"))))
  ;; (setq org-super-agenda-groups
  ;;        '(;; Each group has an implicit Boolean OR operator between its selectors.
  ;;           (:name "TIMELINE & HABITS"  ; Optionally specify section name
  ;;                  :time-grid t
  ;; 	           :habit t)
  ;; 	    (:name "Important"
  ;;                  ;; Single arguments given alone
  ;;                  :tag "URGENT"
  ;;                  :priority "1")
  ;;           (:name "Done today"
  ;;                  :and (:regexp "State \"DONE\""
  ;;                        :log t))
  ;;           (:name "Clocked today"
  ;;                  :log t)
  ;;        ;; After the last group, the agenda will display items that didn't
  ;;        ;; match any of these groups, with the default order position of 99
  ;;     ))
  )

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/OneDrive/snippets")))

(yas-reload-all)


;; ESS
(use-package ess
  :ensure t
  :init (require 'ess-site)
  :config
  (ess-toggle-underscore t)
  (global-set-key (kbd "M--") (lambda () (interactive) (insert " <- ")))
  (ess-toggle-underscore nil)
  (add-to-list 'ispell-skip-region-alist '("^```" . "```$"))
  )



;; poly mode
;; (use-package polymode
;;   :ensure t
;;   :config
;;   (progn
;;     (require 'poly-R)
;;     (require 'poly-markdown)
;;     (require 'poly-noweb)
;;     (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))))

(use-package polymode
  :ensure t
  :ensure markdown-mode
  :ensure poly-R
  :ensure poly-noweb
  :config
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode)))

;; latex
(use-package tex
  :ensure auctex
  :config
  (setq TeX-view-program-selection
	'((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
	'(("PDF Viewer" "zathura %o")))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
  (add-hook 'markdown-mode-hook 'turn-on-reftex)
  (setq reftex-default-bibliography '("/home/rich/Dropbox/MyLibraryZ2.bib"))
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
					;(setq reftex-cite-format 'natbib)
  (eval-after-load 'reftex-vars
    '(progn
       (setq reftex-cite-format '((?p . "[@%l]")
				  (?t . "@%l")
				  (?y . "[-@%l]")
				  (?e . "[e.g., @%l]")
				  (?b . "\\bibentry{%l}")
				  (?s . "[see @%l]")))))
  (setq TeX-PDF-mode t)
  )


;; web-mode
(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :bind (("C-c i" . insert-li-tag)
	 ("C-c f" . insert-fragment))
  :config
  (setq web-mode-extra-snippets
	'((nil . (("slide" . "<section>\n<h2>|</h2>\n<ul>\n</ul>\n</section>")
		  ("slide2" . "<section>\n<h2>|</h2>\n<div class='container_2'>\n<div class='grid_1'>\n</div>\n<div class='grid_1'>\n</div>\n<div class='clear'>&nbsp;</div>\n</div>\n</section>")
		  ("imageSlide"  . "<section>\n<h2>|</h2>\n<img src='assets/' width='100%' />\n</section>")
		  ("aside"  . "<aside class='notes'>\n<ul>\n|\n</ul>\n</aside>")
		  ("iclicker"  . "<section>\n<h2>iclicker Question</h2>\n<p class='paragraph-left'></p>\n<ul class='iclicker-answers'>\n|</ul>\n</section>")
		  ("paragraph" . "<p class='paragraph-left'>|</p>")))))
  (fset 'insert-p-tag
	[tab ?< ?p ?> ?\C-e ?< ?/ ?\C-f])
  (fset 'insert-li-tag
	[tab ?< ?l ?i ?> ?\C-e ?< ?/ ?\C-f])
  ;;(fset 'insert-li-tag2
  ;;      [tab ?< ?l ?i ?> ?\M-e ?< ?/ ?\C-f])
  (fset 'insert-fragment
	" class=\"fragment\"")
  )



;; tablist
(use-package tablist
  :ensure t
  )

;; pdf-tools

(defun rel/save-buffer-no-args ()
  "Save buffer ignoring arguments"
  (save-buffer))

(use-package pdf-tools
  :ensure t
  :pin manual
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-height)
  (setq pdf-view-resize-factor 1.05)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; wait until map is available
  (with-eval-after-load "pdf-annot"
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<return>") 'pdf-annot-edit-contents-commit)
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<S-return>") 'newline)
    ;; save after adding comment
    (advice-add 'pdf-annot-edit-contents-commit :after 'rel/save-buffer-no-args))
  )


;; company-mode
(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  )


;; shell-pop
(use-package shell-pop
  :ensure t
  :bind (("C-t" . shell-pop))
  :config 
  )

;; mu4e
(use-package mu4e
  :ensure nil
  :bind ("C-c e" . mu4e)
  :init
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  (setq mu4e-compose-format-flowed t)
  (add-hook 'mu4e-compose-mode-hook (lambda () (use-hard-newlines -1)))
  (setq mu4e-compose-in-new-frame t)
;;  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-view-use-gnus t)
  (setq mail-personal-alias-file (expand-file-name "/home/rich/.mailrc"))
  :config
  (add-to-list 'mu4e-view-actions
	       '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  :custom
  (mail-user-agent 'mu4e-user-agent)
;;  (mu4e-user-mail-address-list '("lucasri@msu.edu"))
  (mu4e-reply-to-address "lucasri@msu.edu")
  (user-mail-address "lucasri@msu.edu")
  (user-full-name "Richard E. Lucas")
  (mu4e-attachment-dir "~/Downloads")
;;  (mu4e-maildir "/home/rich/Maildir")
  (mu4e-drafts-folder "/Drafts")
  (mu4e-get-mail-command "true")
  (mu4e-refile-folder "/Archive")
  (mu4e-sent-folder "/Sent")
  (mu4e-trash-folder "/Trash")
  (mu4e-update-interval 300)
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  (mu4e-maildir-shortcuts
   '(("/INBOX" . ?i)
     ("/Archive" . ?a)
     ("/Trash" . ?t)
     ("/Drafts" . ?d)
     ("/Reviews" . ?r)
     ("/Sent" . ?s)))
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-default-smtp-server "localhost")
  (smtpmail-smtp-server "localhost")
  (smtpmail-local-domain "localhost")
  (smtpmail-smtp-service 1025)
  )

(require 'gnus-icalendar)
(setq gnus-icalendar-org-capture-file "~/OneDrive/org/calendar.org")
(setq gnus-icalendar-org-capture-headline '("Calendar"))
(gnus-icalendar-org-setup)

(require 'mu4e-icalendar)
(mu4e-icalendar-setup)

(require 'org-mu4e)

(use-package mu4e-alert
  :ensure t
  :after mu4e
  ;; :hook ((after-init . mu4e-alert-enable-mode-line-display)
  ;; 	 (after-init . mu4e-alert-enable-notifications))
  :init
  (setq mu4e-alert-interesting-mail-query
	(concat
	 "flag:unread maildir:/INBOX"
	 ))
  :config
  (mu4e-alert-enable-notifications)
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  )



(use-package helm
  :ensure t
  :bind (("C-x C-f" . #'helm-find-files)
	 ("C-c h" . helm-command-prefix)
	 ("M-x" . #'helm-M-x)
	 ("C-x C-r" . #'helm-for-files)
	 ("C-x C-b" . #'helm-bibtex)
	 ("C-x b" . #'helm-buffers-list)
	 ("M-y" . #'helm-show-kill-ring)
	 )
  :init
  (progn
    (require 'helm-config)
    (helm-mode 1)
    )
  :config
  )

(setq helm-ff-keep-cached-candidates nil)

;; (use-package helm-mu
;;   :ensure t
;;   )

;; (define-key mu4e-main-mode-map "s" 'helm-mu)
;; (define-key mu4e-headers-mode-map "s" 'helm-mu)
;; (define-key mu4e-view-mode-map "s" 'helm-mu)
;;(setq helm-mu-default-search-string "(maildir:/INBOX OR maildir:/Sent OR maildir:/Archive OR maildir:/Reviews)")


;; (use-package helm-google
;;   :ensure t
;;   :bind ("C-c g" . #'helm-google)
;;   )

;; (use-package helm-chrome
;;   :ensure t
;;   :bind ("C-x c" . #'helm-chrome-bookmarks)
;;   )

(use-package helm-bibtex
  :ensure t
  :init
  (setq bibtex-completion-bibliography
	'("/home/rich/Dropbox/MyLibraryZ2.bib"))
  (setq bibtex-completion-pdf-field "File")
   :config 
  )


;; weather from wttr.in
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :bind ("C-c w" . #'wttrin)
  :init
  (setq wttrin-default-cities '("East Lansing"))
  (setq wttrin-default-accept-language '("Accept-Language" . "en-US"))
  )


(use-package fzf
  :ensure t
  :bind ("C-c z" . #'fzf-directory)
  )


(use-package dired-quick-sort
  :ensure t
  :config
  (dired-quick-sort-setup))


;; (use-package projectile
;;   :ensure t
;;   :config
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (projectile-mode +1))

;; (use-package helm-projectile
;;   :ensure t
;;   :config
;;   (helm-projectile-on))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;  (load-theme 'doom-flatwhite t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package mingus
  :ensure t)


(use-package org-noter
  :ensure t)

;; Open documents with correct program
(require 'dired-x)
(setq dired-guess-shell-alist-user '(("\\.doc//'" "libreoffice")
				     ("\\.docx//'" "libreoffice")
				     ("\\.ppt//'" "libreoffice")
				     ("\\.pptx//'" "libreoffice")
				     ("\\.xls//'" "libreoffice")
				     ("\\.xlsx//'" "libreoffice")))
				     

(use-package i3wm-config-mode
  :ensure t
  :defer t)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(custom-safe-themes
   '("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))
 '(package-selected-packages
   '(org-roam-server org-roam-bibtex org-ref org-roam polymode org-chef ## pdf-tools memory-usage zenburn-theme yasnippet wttrin web-mode uuidgen use-package shell-pop ranger poly-R paradox org-super-agenda org-pdfview org-noter org-fstree nord-theme mu4e-alert mingus markdown-preview-mode julia-mode htmlize helm-projectile helm-mu helm-google helm-chrome helm-bibtex fzf ess doom-themes dired-quick-sort company color-theme-sanityinc-tomorrow benchmark-init auctex atomic-chrome)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'default-frame-alist '(font . "Droid Sans Mono-10" ))
(set-face-attribute 'default t :font "Droid Sans Mono-10" )
;;(set-frame-font "Inconsolata Regular-20")

(require 'org-fstree)


;; (require 'mu4e-thread-folding)

;; (add-to-list 'mu4e-header-info-custom
;;              '(:empty . (:name "Empty"
;;                          :shortname ""
;;                          :function (lambda (msg) "  "))))
;; (setq mu4e-headers-fields '((:empty         .    2)
;;                             (:human-date    .   12)
;;                             (:flags         .    6)
;;                             (:mailing-list  .   10)
;;                             (:from          .   22)
;;                             (:subject       .   nil)))


;; org-chef
(use-package org-chef
  :ensure t)
(setq org-chef-prefer-json-ld t)


;; org-roam
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/home/rich/OneDrive/org/roam")
      (org-roam-index-file "/home/rich/OneDrive/org/roam/index.org")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate)))
      :config
      (require 'org-roam-protocol)
      (setq org-roam-capture-ref-templates (quote
				      (("r" "ref" plain (function org-roam-capture--get-point)
					:file-name "${slug}"
					:head "#+title: ${title}
#+roam_key: ${ref}
#+roam_tags: %^{keywords}

%?

${body}"
					:unnarrowed t))))
      )

(use-package org-ref
  :ensure t
  :config
  (setq reftex-default-bibliography '("~/OneDrive/MyLibrary.bib")
	org-ref-bibliography-notes "/home/rich/OneDrive/org/notes.org"
	org-ref-default-bibliography '("/home/rich/OneDrive/MyLibrary.bib")
	)
  )
(require 'org-ref)

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-templates
      '((\"r\" \"reference\" plain (function org-roam-capture--get-point)
	 "#+ROAM_KEY: cite:%^{citekey}
#+ROAM_TAGS: %^{keywords}
#+PROPERTY: type: %^{entry-type}
#+PROPERTY: author: %^{author} 
fullcite:%\\1
%?"
         :file-name "${citekey}"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t)))
  ) ; optional: if Org Ref is not loaded anywhere else, load it here

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))
