(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6e13ff2c27cf87f095db987bf30beca8697814b90cd837ef4edca18bdd381901" default))
 '(gac-automatically-push-p t)
 '(org-agenda-files
   '("~/org/journal/202410.org" "~/org/nodes/CFLW Data.org" "~/org/nodes/Dutch Vocabulary.org" "~/org/nodes/EKAW 2024.org" "~/org/nodes/ESWC 2025.org" "~/org/nodes/Emacs.org" "~/org/nodes/NLDR Ranking.org" "~/org/nodes/PhD Meetings.org" "~/org/nodes/Routledge Dutch Intensive Course.org" "~/org/nodes/Systematic Literature Review.org" "~/org/nodes/Term Typing Ontology Enrichment Experiment 1.org" "~/org/nodes/Who Am I.org" "~/org/worklogs/log_2024-10-08.org" "~/org/worklogs/log_2024-10-09.org" "~/org/worklogs/log_2024-10-10.org" "~/org/worklogs/log_2024-10-11.org" "~/org/worklogs/log_2024-10.org"))
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.2 :html-foreground "Black" :html-background "Transparent" :html-scale 2.0 :matchers
				 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
	 (vm-imap . vm-visit-imap-folder-other-frame)
	 (gnus . org-gnus-no-new-news)
	 (file . find-file)
	 (wl . wl-other-frame)))
 '(package-selected-packages
   '(envrc projectile which-key org-anki org-ql helm-bibtex org-roam-bibtex annotate toc-org hotfuzz ruff-format nix-mode git-auto-commit lsp-ui lsp-mode latex-extra latexdiff auctex org-view-mode rainbow-delimiters flycheck origami org-journal vertico git-gutter magit git-auto-commit-mode company org-roam-ui spacious-padding org-super-agenda fzf dashboard org-transclusion org-superstar org-modern org-roam evil catppuccin-theme))
 '(python-isort-extra-args nil))

;;; Theme
(load-theme 'catppuccin :no-confirm)

;;; emacs
(use-package emacs
  :custom-face
  (default ((nil (:font "Iosevka Nerd Font" :height 220))))
  :hook (org-mode . auto-fill-mode)
  :config
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tab-bar-mode 1)
  (global-display-line-numbers-mode 1)
  (setq inhibit-startup-screen t)
  (setq auto-save-file-name-transforms `((".*" "/tmp/" t)))
  (setq backup-directory-alist '((".*" . "/tmp")))
  (setq kill-buffer-delete-auto-save-files t)
  (setq display-line-numbers-type 'relative)
  (setq-default fill-column 100)
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4)
  (setq python-indent-level 4)
  (setq visible-bell t)
  (setq visual-line-mode 1)
  (setq auto-fill-mode 1)
  (setq truncate-partial-width-windows nil)
)

;;; Evil
(use-package evil
  :config
  (evil-mode 1)
  )

;;; org
(use-package org
  :custom-face
  (org-document-title ((t (:foreground "dim gray" :weight bold :height 1.0))))
  :hook (org-mode . org-indent-mode)
  ; :custom
  ; (org-blank-before-new-entry '((heading . t)(plain-list-item . t)))
  :config
  (define-key minibuffer-local-completion-map (kbd "?") nil)
  (setq org-deadline-warning-days 0)
  (setq org-cycle-separator-lines 1)
  (setq org-adapt-indentation nil)
  (setq org-hide-emphasis-markers t)
  (setq org-display-remote-inline-images 'download)
  (setq org-display-inline-images t)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq org-return-follows-link t)

  ;;org-cite
  (setq org-cite-global-bibliography '("~/org/bibliography.bib"))

  ;; org-agenda
  (with-eval-after-load 'org
	(defun org-agenda-files (&rest _)
		(directory-files-recursively "~/org/" "\\.org$")))
  ;; (setq org-agenda-files '("~/org"))
  (setq org-agenda-start-day "+0d")
  (setq org-agenda-window-setup 'other-tab)
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timeline-if-deadline-is-shown t)
  (setq org-agenda-hide-tags-regexp ".*")
  (setq org-agenda-prefix-format '(
				(agenda . " %?-12c  %?-12t%?-b ")
				(todo . " %?-12t %s")))
  ;; org-todo
  (setq org-todo-keywords
	'((sequence "TODO" "NEXT" "IN PROGRESS" "|" "DONE" "ARCHIVED")))
  (setq org-todo-keyword-faces
	'(("TODO" . (:foreground "#f9e2af" :weight bold))
	    ("NEXT" . (:foreground "#cba6f7" :weight bold))
	    ("IN PROGRESS" . (:foreground "#89b4fa" :weight bold))
	    ("DONE" . (:foreground "#a6e3a1" :weight bold))
	    ("ARCHIVED" . (:foreground "#9399b2"))
	    ))

  ;; babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))
   )
  )

;;; org-super-agenda
;; Outside org because of org-super-agenda usage (?)

(use-package org-super-agenda
  :after org
  :init
	(setq org-agenda-custom-commands
		'(
					("u" "Super View" ((agenda "" (
									(org-agenda-span 'day)
									(org-super-agenda-groups
									'(
									(:name "For the day"
										   :date today
										   :todo ("NEXT" "IN PROGRESS" "TODO")
										   :order 1)
									(:name "Overdue"
										   :deadline past
										   :todo ("NEXT" "IN PROGRESS" "TODO")
										   :order 3)
									(:name "Reschedule"
										   :scheduled past
										   :todo ("NEXT" "IN PROGRESS" "TODO")
										   :order 4)
									(:name "Upcoming"
										   :scheduled future
										   :todo ("NEXT" "IN PROGRESS" "TODO")
										   :order 5)
									(:discard (:anything))
									)
									)
									)
												))
						)
					)
		)
  :config
  (org-super-agenda-mode t))

;; org-roam
(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/org/"))
  (setq org-roam-dailies-directory "~/org/worklogs/")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :target (file+head "nodes/${title}.org"
						  ":PROPERTIES:\n:CATEGORY: NODE\n:END:\n#+TITLE: ${title}\n#+FILETAGS:\n\n* ${title}\n\n")
	   :immediate-finish t
	   :create-file yes
	   :unnarrowed t)
	  ("n" "literature note" plain "%?"
           :target (file+head "academic/${citekey}.org"
			      "#+TITLE: ${citekey}\n#+FILETAGS: :article:\n* ${title}")
         :unnarrowed t)))
  (setq org-roam-dailies-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head+olp "log_%<%Y-%m>.org"
								":PROPERTIES:\n:CATEGORY: WORKLOG\n:END:#+TITLE: %<%Y-%m>\n\n"
								("%<%Y-W%W>" "%<%Y-%m-%d>")))))
  (setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ))
  (org-roam-db-autosync-mode 1)
  )

;;; org-superstar
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  )

;;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 3)
  )

;;; vertico
(use-package vertico
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode)
  )

;;; hotfuzz
(use-package hotfuzz
  :config
  (setq completion-styles '(hotfuzz))
  )

;;; org-journal
(use-package org-journal
  :after org
  :config
  (setq org-journal-dir "~/org/journal")
  (setq org-journal-file-type 'monthly)
  (setq org-journal-file-format "%Y%m")
  (setq org-journal-date-format "%Y-%m-%d")
  )

;;; org-roam-bibtex
(use-package org-roam-bibtex
  :after org-roam
  :config
  (setq org-cite-follow-processor 'helm-bibtex-org-cite-follow)
  (setq bibtex-completion-format-citation-functions
	'((org-mode . bibtex-completion-format-citation-org-cite)
	(latex-mode . bibtex-completion-format-citation-cite)
	(markdown-mode . bibtex-completion-format-citation-pandoc-citeproc) 
	(python-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex) 
	(rst-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex) 
	(default . bibtex-completion-format-citation-default)))
  )

;;; helm-bibtex
(use-package helm-bibtex
  :config
  (setq bibtex-completion-bibliography '("~/org/bibliography.bib")))

;;; company
(use-package company
  :config
  (company-mode 1)
  )

;;; git-auto-commit-mode
(use-package git-auto-commit-mode
  :hook (after-save . git-auto-commit-mode)
  :config
  (setq gac-automatically-push-p t)
  (git-auto-commit-mode 1)
  )

;;; git-gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode 1)
  )

;;; origami
(use-package origami
  :config
  (global-origami-mode 1)
  )

;;; flycheck
(use-package flycheck
  :config
  (global-flycheck-mode +1)
  )

;;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (after-init . rainbow-delimiter-mode)
  )

;;; auctex
(use-package auctex
  :config
  (setq TeX-parse-self t)
  )

;;; direnv
;; (use-package direnv
;;  :config
;;  (direnv-mode))

;; envrc
(use-package envrc
  :hook(after-init . envrc-global-mode))

;;; lsp-mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
		 (nix-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;;; lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)

;;; lsp

;;; reformatter
(require 'reformatter)

;; python
; isort
(defcustom python-isort-command "isort"
  "Name of the `isort` executable."
  :group 'nasy
  :type 'string)

(defvar python-isort--base-args '("--quiet" "--atomic" "--fass")
  "Base arguments to pass to isort.")

(defcustom python-isort-extra-args nil
  "Extra arguments to pass to isort."
  :group 'nasy
  :type '(repeat string))

;;;###autoload (autoload 'python-isort-buffer "python-isort" nil t)
;;;###autoload (autoload 'python-isort-region "python-isort" nil t)
;;;###autoload (autoload 'python-isort-on-save-mode "python-isort" nil t)

(reformatter-define python-isort
  :program python-isort-command
  :args (python-isort--make-args beg end)
  :lighter " isort"
  :group 'python-isort)

(defun python-isort--make-args (beg end)
  "Helper to build the argument list for isort for span BEG to END."
  (append python-isort--base-args
          python-isort-extra-args
          '("-")))

(add-hook 'python-mode-hook 'python-isort-on-save-mode)

; black
(defcustom python-black-command "black"
  "Name of the `black` executable."
  :group 'nasy
  :type 'string)

(defvar python-black--base-args '("--quiet")
  "Base arguments to pass to black.")

(defcustom python-black-extra-args nil
  "Extra arguments to pass to black."
  :group 'nasy
  :type '(repeat string))

;;;###autoload (autoload 'python-black-buffer "python-black" nil t)
;;;###autoload (autoload 'python-black-region "python-black" nil t)
;;;###autoload (autoload 'python-black-on-save-mode "python-black" nil t)

(reformatter-define python-black
  :program python-black-command
  :args (python-black--make-args beg end)
  :lighter " black"
  :group 'python-black)

(defun python-black--make-args (beg end)
  "Helper to build the argument list for isort for span BEG to END."
  (append python-black--base-args
          python-black-extra-args
          '("-")))

(add-hook 'python-mode-hook 'python-black-on-save-mode)

; nix-nixfmt
(defcustom nix-nixfmt-command "nixfmt"
  "Name of the `nix-nixfmt` executable."
  :group 'nasy
  :type 'string)

;;;###autoload (autoload 'nix-nixfmt-buffer "nix-nixfmt" nil t)
;;;###autoload (autoload 'nix-nixfmt-region "nix-nixfmt" nil t)
;;;###autoload (autoload 'nix-nixfmt-on-save-mode "nix-nixfmt" nil t)

(reformatter-define nix-nixfmt
  :program nix-nixfmt-command
  :lighter " nix-nixfmt"
  :group 'nix-nixfmt)

(add-hook 'nix-mode-hook 'nix-nixfmt-on-save-mode)

;;; org-toc
(use-package toc-org
  :hook (org-mode . toc-org-mode))

;;; annotate
(use-package annotate
  :hook (
		 (org-mode . annotate-mode)
		 (org-journal-mode .annotate-mode))
  :config
  (setq annotate-file "~/org/annotations")
  )

;;; Custom
;;;###autoload
(defun unpackaged/org-fix-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))

(add-hook 'before-save-hook
          (lambda ()
            (if (eq major-mode 'org-mode) ; Org-mode
                (let ((current-prefix-arg 4)) ; Emulate C-u
                  (call-interactively 'unpackaged/org-fix-blank-lines)))))

(add-hook 'before-save-hook
          (lambda ()
            (if (eq major-mode 'org-journal-mode) ; Org-journal-mode
                (let ((current-prefix-arg 4)) ; Emulate C-u
                  (call-interactively 'unpackaged/org-fix-blank-lines)))))

;;; org-ql

(use-package org-ql)


;;; org-anki

(use-package org-anki)

;;; python-mode
(use-package python-mode)

;;; which-key
(use-package which-key
  :config
  (which-key-mode 1))

;;; projectile
(use-package projectile
  :config
  (projectile-mode 1))

;;; Keybindings

;; General
(global-set-key (kbd "C-c r") 'eval-buffer)
(global-set-key (kbd "C-c y") 'clipboard-yank)
(global-set-key (kbd "C-c M-i") 'org-id-get-create)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c h") 'evil-next-buffer)
(global-set-key (kbd "C-c l") 'evil-prev-buffer)
(global-set-key (kbd "C-c t") (lambda () (interactive) (tab-new) (scratch-buffer)))
(global-set-key (kbd "C-c w") 'tab-close)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; org-roam
(global-set-key (kbd "C-c o") 'org-roam-node-find)
(global-set-key (kbd "C-c i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n") 'org-roam-capture)
(global-set-key (kbd "C-M-r") 'org-roam-buffer-toggle)

;; org-roam-dailies
(global-set-key (kbd "C-c M-\\") 'org-roam-dailies-goto-today)
;; org-agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; org-journal
(global-set-key (kbd "C-c M-d") 'org-journal-new-entry)

;; org-transclusion
(global-set-key (kbd "C-c M-a") 'org-transclusion-add-all)
(global-set-key (kbd "C-c M-r") 'org-transclusion-remove-all)

;; citar-org-roam
(global-set-key (kbd "C-x M-r") 'citar-open-notes)

;; org-view-mode
(global-set-key (kbd "C-c M-e") 'org-view-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; unpackaged/org-fix-blank-lines
(global-set-key (kbd "C-c f") 'unpackaged/org-fix-blank-lines)


;; projectile
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
