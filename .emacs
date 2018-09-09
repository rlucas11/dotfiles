;; Appearance and Startup Behavior

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-visual-line-mode 1) ; 1 for on, 0 for off.
(setq x-select-enable-clipboard t)


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

