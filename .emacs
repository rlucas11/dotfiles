;; Appearance and Startup Behavior

(set-frame-parameter (selected-frame) 'alpha '(92 . 90))
(add-to-list 'default-frame-alist '(alpha . (92 .90)))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-visual-line-mode 1) ; 1 for on, 0 for off.
(setq x-select-enable-clipboard t)
(server-start)

(set-frame-font "Inconsolata-12")
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


;; benchmark
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))



;; recentf
(use-package recentf
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
  ;;  (color-theme-sanityinc-tomorrow-eighties)
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
    (setq org-directory "~/Dropbox/org")
    (setq org-refile-targets (quote ((org-agenda-files :regexp . "*"))))
    (setq org-agenda-files
	  (quote
	   ("~/Dropbox/org/notes.org"
	    "~/Dropbox/org/work.org"
	    "~/Dropbox/org/gcal.org"
	    "~/Dropbox/org/calendar.org"
	    "~/Dropbox/org/home.org"
	    "~/Dropbox/org/research.org"
	    "~/Dropbox/org/peerReview.org"
	    "~/Dropbox/org/recipes.org")))
    (setq org-default-notes-file "~/Dropbox/org/notes.org")
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
         (file+headline "~/Dropbox/org/capture.org" "Notes")
         "* %^{Title}\n\n  Source: %:link, %:description\n\n  %i"
         :empty-lines 1)
	("t"
	 "TODO Template"
	 entry
	 (file+headline "~/Dropbox/org/capture.org" "Tasks")
	 "* TODO %?\n %i\n")
	("e"
	 "Email TODO Template"
	 entry
	 (file+headline "~/Dropbox/org/capture.org" "Tasks")
	 "* TODO %?\n %i\n %a")
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
    (setq org-agenda-span (quote month))
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
	   ((agenda deadline-up priority-down)
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
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

;; ESS
(use-package ess
  :init (require 'ess-site)
  :config
  (ess-toggle-underscore t)
  (global-set-key (kbd "M--") (lambda () (interactive) (insert " <- ")))
  (ess-toggle-underscore nil))

;; poly mode
(use-package polymode
  :config
  (progn
    (require 'poly-R)
    (require 'poly-markdown)
    (require 'poly-noweb)
    (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))))
  
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "285efd6352377e0e3b68c71ab12c43d2b72072f64d436584f9159a58c4ff545a" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "ca849ae0c889eb918785cdc75452b1e11a00848a5128a95a23872e0119ccc8f4" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "774aa2e67af37a26625f8b8c86f4557edb0bac5426ae061991a7a1a4b1c7e375" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "a038af4fff7330f27f4baec145ef142f8ea208648e65a4b0eac3601763598665" "1d50bd38eed63d8de5fcfce37c4bb2f660a02d3dff9cbfd807a309db671ff1af" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "0809c08440b51a39c77ec5529f89af83ab256a9d48107b088d40098ce322c7d8" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(mail-user-agent (quote mu4e-user-agent))
 '(message-send-mail-function (quote smtpmail-send-it))
 '(mu4e-attachment-dir "~/Downloads")
 '(mu4e-drafts-folder "/Drafts")
 '(mu4e-get-mail-command "true")
 '(mu4e-maildir "/home/rich/Maildir")
 '(mu4e-maildir-shortcuts
   (quote
    (("/INBOX" . 105)
     ("/Archive" . 97)
     ("/Trash" . 116)
     ("/Drafts" . 100)
     ("/Reviews" . 114)
     ("/Sent" . 115))))
 '(mu4e-refile-folder "/Archive")
 '(mu4e-reply-to-address "lucasri@msu.edu" t)
 '(mu4e-sent-folder "/Sent")
 '(mu4e-trash-folder "/Trash")
 '(mu4e-update-interval 300)
 '(mu4e-use-fancy-chars t)
 '(mu4e-user-mail-address-list (quote ("lucasri@msu.edu")))
 '(mu4e-view-show-addresses t)
 '(mu4e-view-show-images t)
 '(package-selected-packages
   (quote
    (org-noter doom-themes helm-projectile projectile dired-quick-sort htmlize helm-mu org-pdfview mingus fzf wttrin polymode markdown-mode helm-bibtex helm-chrome helm-google helm mu4e-alert shell-pop pdf-tools tablist zenburn-theme nord-theme web-mode use-package ranger poly-noweb poly-markdown poly-R paradox org markdown-preview-mode julia-mode ess color-theme-sanityinc-tomorrow auctex)))
 '(smtpmail-default-smtp-server "localhost")
 '(smtpmail-local-domain "localhost")
 '(smtpmail-smtp-server "localhost")
 '(smtpmail-smtp-service 1025)
 '(user-full-name "Richard E. Lucas")
 '(user-mail-address "lucasri@msu.edu"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; tablist
(use-package tablist)

;; pdf-tools

(defun rel/save-buffer-no-args ()
  "Save buffer ignoring arguments"
  (save-buffer))

(use-package pdf-tools
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

;; org-pdfview
(use-package org-pdfview)

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
(setq gnus-icalendar-org-capture-file "~/Dropbox/org/calendar.org")
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


(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

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
				     
