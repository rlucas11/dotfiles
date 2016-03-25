(add-to-list 'load-path "~/emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; open with single window
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook 'delete-other-windows)
(setq inhibit-startup-buffer-menu t)

(add-hook 'text-mode-hook 'turn-on-flyspell) ; Turn on spell checking in text mode

;; create backup file directory
(defun make-backup-file-name (file)
(concat "~/.emacs_backups/" (file-name-nondirectory file)
"~"))

;;recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(run-at-time nil (* 10 60) 'recentf-save-list)


(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)

;; ;;Color theme
;; (require 'color-theme)
;; (color-theme-initialize)
;; (require 'zenburn)
;; (zenburn)
;;(load-theme 'zenburn t)


(require 'tomorrow-night-eighties-theme)


;;zeitgeist
(require 'zeitgeist)

;;IDO mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;magit
;(require 'magit)

;;web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ess-site)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auctex
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-view-program-selection
      '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "zathura %o")))

;; RefTeX
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
(add-hook 'markdown-mode-hook 'turn-on-reftex)
(setq reftex-default-bibliography '("/home/rich/Dropbox/MyLibraryZ.bib"))
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
;(setq reftex-cite-format 'natbib)

(eval-after-load 'reftex-vars
  '(progn
     (setq reftex-cite-format '((?p . "[@%l]")
				(?t . "@%l")
				(?y . "[-@%l]")
				(?e . "[e.g., @%l]")
				(?s . "[see @%l]")))))


(setq TeX-PDF-mode t)
(global-visual-line-mode 1) ; 1 for on, 0 for off.
(setq x-select-enable-clipboard t)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-habit)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "97538ac96539dea7b6ed9890e0d348f836f7638d721a5bc8aea075b774e2a6ff" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "ac072b41249ee08594e0115ccbdc5197fcb517e680106d795201f4680bb8a6e4" "c310a90f20afb1d1aed5ace2a8605dbf55d2834136ab4a7b0b0fd6cc1f2f5f42" default)))
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/notes.org" "~/Dropbox/org/research_notes.org" "~/Dropbox/org/work.org" "~/Dropbox/org/gcal.org" "~/Dropbox/org/home.org" "~/Dropbox/org/articles.org")))
 '(org-mobile-agendas (quote ("y" "a" "t")))
 '(org-mobile-files
   (quote
    ("~/Dropbox/org/home.org" "~/Dropbox/org/work.org" "~/Dropbox/org/gcal.org" "~/Dropbox/org/music.org" "~/Dropbox/org/notes.org")))
 '(post-uses-fill-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-default-notes-file "~/Dropbox/org/notes.org")
     (define-key global-map "\C-cc" 'org-capture)
;; Where to refile
(setq org-refile-targets (quote ((org-agenda-files :regexp . "*"))))
;; custom agenda
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
         :empty-lines 0)
	("t"
	 "TODO Template"
	 entry
	 (file+headline "~/Dropbox/org/capture.org" "Tasks")
	 "* TODO %?\n %i\n %a")
        ;; ... more templates here ...
        )))

;; automatically push org-mobile
(defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

(defun org-mobile-push-with-delay (secs) 
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'org-mobile-push)))

(add-hook 'after-save-hook 
 (lambda () 
   (when (eq major-mode 'org-mode)
     (dolist (file (org-mobile-files-alist))
       (if (string= (expand-file-name (car file)) (buffer-file-name))
           (org-mobile-push-with-delay 30)))
   )))

(run-at-time "00:05" 86400 '(lambda () (org-mobile-push-with-delay 1))) ;; refreshes agenda file each day

;; automatically pull org-mobile
(org-mobile-pull) ;; run org-mobile-pull at startup

(defun install-monitor (file secs)
  (run-with-timer
   0 secs
   (lambda (f p)
     (unless (< p (second (time-since (elt (file-attributes f) 5))))
       (org-mobile-pull)))
   file secs))

(install-monitor (file-truename
                  (concat
                   (file-name-as-directory org-mobile-directory)
                          org-mobile-capture-file))
                 5)

;; Do a pull every 5 minutes to circumvent problems with timestamping
;; (ie. dropbox bugs)
(run-with-timer 0 (* 30 60) 'org-mobile-pull)

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

;; org-bable
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   ))


;; lua-mode
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; ;; mutt
;; ;;(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
;; (autoload 'post-mode "post" "mode for e-mail" t)
;; (add-to-list 'auto-mode-alist 
;;              '("\\.*mutt-*\\|.article\\|\\.followup" 
;;                 . post-mode))
;; (add-hook 'post-mode 'turn-off-auto-fill)
;; ;;(remove-hook 'post-mode-hook 'turn-on-auto-fill)

;; org-protocol
(server-start)
(require 'org-protocol)

;; ;; org-reveal
;; (require 'ox-reveal)
;; (setq org-reveal-root "file:///home/rich/Dropbox/LucasTalks")


;; macros  (for web-mode)
(fset 'insert-p-tag
   [tab ?< ?p ?> ?\C-e ?< ?/ ?\C-f])
(fset 'insert-li-tag
   [tab ?< ?l ?i ?> ?\C-e ?< ?/ ?\C-f])

(global-set-key (kbd "C-c i") 'insert-li-tag)

(fset 'insert-fragment-li-tag
   [tab ?< ?l ?i ?  ?c ?l ?a ?s ?s ?= ?? backspace ?? backspace ?? backspace ?\" ?f ?r ?a ?g ?m ?e ?n ?t ?\" ?> ?\C-e ?< ?/ ?\C-n])


;; web-mode
(setq web-mode-extra-snippets
      '((nil . (("slide"  . ("<section>\n<h2>" . "</h2>\n<ul>\n</ul>\n</section>"))
	        ("slide2" . ("<section>\n<h2>" . "</h2>\n<div class='container_2'>\n<div class='grid_1'>\n</div>\n<div class='grid_1'>\n</div>\n<div class='clear'>&nbsp;</div>\n</div>\n</section>"))
		("imageSlide"  . ("<section>\n<h2>" . "</h2>\n<img src='assets/' width='100%' />\n</section>"))
		("iclicker"  . ("<section>\n<h2>iclicker Question</h2>\n<p class='paragraph-left'></p>\n<ul class='iclicker-answers'>\n" . "</ul>\n</section>"))
		("paragraph" . ("<p class='paragraph-left'>" . "</p>"))
	)))
)


;; MELBA

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)


;; Polymode & RMD Files

(setq load-path
      (append '("path/to/polymode/"  "path/to/polymode/modes")
              load-path))

(require 'poly-R)
(require 'poly-markdown)

(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
