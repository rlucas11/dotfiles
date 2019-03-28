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


;; Global keys
;;(global-set-key "\C-cl" 'org-store-link)
;;(global-set-key "\C-ca" 'org-agenda)
;;(global-set-key "\C-cb" 'org-iswitchb)




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
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
	recentf-max-menu-items 25)
  (run-at-time nil (* 10 60) 'recentf-save-list)
  :bind ("C-x C-r" . recentf-open-files))


;; Themes
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (setf custom-safe-themes t)
  (color-theme-sanityinc-tomorrow-eighties))

;; (use-package nord-theme
;;   :ensure t)

;; (use-package zenburn-theme
;;   :ensure t)


;; ido
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1))


;; org
(use-package org
  :ensure t
  :pin org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c l" . org-store-link))
  :config
  (progn
    (setq org-directory "~/Dropbox/org")
    (setq org-refile-targets (quote ((org-agenda-files :regexp . "*"))))
    (setq org-agenda-files
	  (quote
	   ("~/Dropbox/org/notes.org" "~/Dropbox/org/work.org" "~/Dropbox/org/gcal.org" "~/Dropbox/org/home.org" "~/Dropbox/org/recipes.org")))
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
         "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
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
      ("\\.pdf\\'" . "zathura %s")
      (auto-mode . emacs)))
    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)
    ;;
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

(use-package org-habit
  :ensure nil)

(require `org-protocol)

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
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(package-selected-packages
   (quote
    (mu4e-alert shell-pop pdf-tools tablist zenburn-theme nord-theme web-mode use-package ranger poly-noweb poly-markdown poly-R paradox org markdown-preview-mode julia-mode ess color-theme-sanityinc-tomorrow auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; tablist
(use-package tablist)

;; pdf-tools
(use-package pdf-tools
  :ensure t
  )
(pdf-tools-install)


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
  :init
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  (setq mu4e-compose-format-flowed t)
  (add-hook 'mu4e-compose-mode-hook (lambda () (use-hard-newlines -1)))
  (setq mu4e-compose-in-new-frame t)
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-user-mail-address-list '("lucasri@msu.edu"))
  (mu4e-reply-to-address "lucasri@msu.edu")
  (user-mail-address "lucasri@msu.edu")
  (user-full-name "Richard E. Lucas")
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-maildir "~/Maildir")
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
     ("/Sent" . ?s)))
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-default-smtp-server "localhost")
  (smtpmail-smtp-server "localhost")
  (smtpmail-local-domain "localhost")
  (smtpmail-smtp-service 1025)
  )

(require 'org-mu4e)

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :hook ((after-init . mu4e-alert-enable-mode-line-display)
	 (after-init . mu4e-alert-enable-notifications))
  :config
  (mu4e-alert-set-default-style 'libnotify)
  :init
  (setq mu4e-alert-interesting-mail-query
   (concat
    "flag:unread maildir:/INBOX"))
  )
