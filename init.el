(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq
   use-package-always-ensure t
   use-package-expand-minimally t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-date-style 'iso)
 '(custom-safe-themes
   '("6e13ff2c27cf87f095db987bf30beca8697814b90cd837ef4edca18bdd381901" default))
 '(gac-automatically-push-p t)
 '(org-agenda-block-separator 46)
 '(org-agenda-breadcrumbs-separator " -> ")
 '(org-agenda-files
   '("/home/workboots/org/Archive/Areas Archive.org" "/home/workboots/org/Archive/Dutch Vocabulary.org" "/home/workboots/org/Archive/Inbox Archive.org" "/home/workboots/org/Archive/Projects Archive.org" "/home/workboots/org/Archive/Resources Archive.org" "/home/workboots/org/Journal/202410.org" "/home/workboots/org/Journal/Journal 2024.org" "/home/workboots/org/Areas.org" "/home/workboots/org/Fleeting.org" "/home/workboots/org/Inbox.org" "/home/workboots/org/Index.org" "/home/workboots/org/Projects.org" "/home/workboots/org/Resources.org" "/home/workboots/org/Scratchpad.org" "/home/workboots/org/Slip Box.org"))
 '(org-export-backends '(ascii html icalendar latex odt org))
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.2 :html-foreground "Black" :html-background "Transparent" :html-scale 2.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(org-preview-latex-process-alist
   '((dvipng :programs
             ("latex" "dvipng")
             :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
             (1.0 . 1.0)
             :latex-compiler
             ("latex -interaction nonstopmode -output-directory %O %F")
             :image-converter
             ("dvipng -D %D -T tight -o %O %F")
             :transparent-image-converter
             ("dvipng -D %D -T tight -bg Transparent -o %O %F"))
     (dvisvgm :programs
              ("latex" "dvisvgm")
              :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
              (1.7 . 1.5)
              :latex-compiler
              ("latex -interaction nonstopmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
     (imagemagick :programs
                  ("latex" "convert")
                  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("pdflatex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim -antialias %f -quality 100 %O"))))
 '(org-priority-faces
   '((65 :foreground "red")
     (66 :foreground "yellow")
     (67 :foreground "green")))
 '(org-ql-search-directories-files-recursive t)
 '(org-roam-capture-new-node-hook
   '(org-id-get-create org-roam-capture--insert-captured-ref-h))
 '(org-super-agenda-date-format "%e %Y-%m-%d")
 '(org-use-property-inheritance '("NAME"))
 '(package-selected-packages
   '(hydra citar-org-roam citar elisp-autofmt aggressive-indent evil-nerd-commenter envrc which-key org-anki org-ql annotate toc-org hotfuzz ruff-format nix-mode git-auto-commit lsp-ui lsp-mode latex-extra latexdiff auctex org-view-mode rainbow-delimiters flycheck origami vertico git-gutter magit git-auto-commit-mode company org-roam-ui spacious-padding fzf dashboard org-transclusion org-superstar org-roam evil catppuccin-theme))
 '(python-isort-extra-args nil))

;;; Theme
(load-theme 'catppuccin :no-confirm)

;;; emacs
(use-package
 emacs
 :custom-face (default ((nil (:font "Iosevka Nerd Font" :height 220))))
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
 (setq-default indent-tabs-mode nil)
 (setq tab-always-indent 'complete)
 (setq python-indent-level 4)
 (setq visible-bell t)
 (setq visual-line-mode 1)
 (setq auto-fill-mode 1)
 (setq truncate-partial-width-windows nil))


;;; Evil
(use-package evil :config (evil-mode 1))

;;; org
(use-package
 org
 :custom-face (org-document-title ((t (:foreground "dim gray" :weight bold :height 1.0))))
 :hook (org-mode . org-indent-mode)
 :hook (org-capture-mode . org-id-get-create)
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
 (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
 (setq org-refile-use-outline-path 'file)
 (setq org-outline-path-complete-in-steps nil)
 (setq org-enforce-todo-dependencies t)
 (setq org-enforce-todo-checkbox-dependencies t)
 (setq org-startup-folded overview)

 ;;org-cite
 (setq org-cite-global-bibliography '("~/org/bibliography.bib"))

 ;; org-agenda
 (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
 (setq org-agenda-file-regexp "^[a-z0-9-_]+.org")
 (setq org-agenda-start-day "+0d")
 (setq org-agenda-window-setup 'other-tab)
 (setq org-agenda-skip-timestamp-if-done t)
 (setq org-agenda-skip-deadline-if-done t)
 (setq org-agenda-skip-scheduled-if-done t)
 (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
 (setq org-agenda-skip-timeline-if-deadline-is-shown t)
 ;; (setq org-agenda-hide-tags-regexp ".*")
 (setq org-agenda-prefix-format '((agenda . " %?-12c  %?-12t%?-b ") (todo . " %?-12t %s")))
 (setq org-agenda-view-columns-initially t)
 (setq
  org-agenda-overriding-columns-format
  "%12TODO(STATUS) %100ITEM %50NAME(HEAD) %20CATEGORY(PARA) %PRIORITY(PR.) %DEADLINE %SCHEDULED")
 (setq org-agenda-with-colors t)
 (setq org-agenda-format-date
       (lambda (date)
         (concat
          "\n"
          (org-agenda-format-date-aligned date)
          "\n"
          (make-string (string-width (org-agenda-format-date-aligned date)) 9472))))
 (setq org-log-done t)
 (setq org-agenda-start-with-log-mode t)

 ;; org-todo
 (setq org-todo-keywords
       '((sequence "TODO(t)" "NEXT(n)" "TODAY(T)" "IN PROGRESS(p)" "|" "DONE(d)" "ARCHIVED(a)")
         (sequence "LATER(l)" "|")
         (sequence "FOCUS(f)" "|")))
 (setq org-todo-keyword-faces
       '(("TODO" . (:foreground "#f9e2af" :weight bold))
         ("NEXT" . (:foreground "#cba6f7" :weight bold))
         ("TODAY" . (:foreground "#f2cdcd" :weight bold))
         ("LATER" . (:foreground "#b4befe" :weight bold))
         ("FOCUS" . (:foreground "#f38ba8" :weight bold))
         ("IN PROGRESS" . (:foreground "#89b4fa" :weight bold))
         ("DONE" . (:foreground "#a6e3a1" :weight bold))
         ("ARCHIVED" . (:foreground "#9399b2"))))
 ;; babel
 (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (python . t))))

;; org-roam
(use-package
 org-roam
 :config
 (setq org-roam-directory (file-truename "~/org"))
 (setq org-roam-dailies-directory "~/org/Journal/")
 (setq org-roam-completion-everywhere t)
 (setq org-roam-capture-templates
       '(("d" "default" plain "%?"
          :target
          (file+olp
           "Inbox.org" ("TODO ${title}\n:PROPERTIES:\n:NAME: ${title}\n:CREATED:\t%U\n:END:\n\n")))
         ("p" "project" plain "%?"
          :target
          (file+olp
           "Projects.org" ("${title}\n:PROPERTIES:\n:NAME: ${title}\n:CREATED:\t%U\n:END:\n\n")))
         ("a" "area" plain "%?"
          :target
          (file+olp
           "Areas.org" ("${title}\n:PROPERTIES:\n:NAME: ${title}\n:CREATED:\t%U\n:END:\n\n")))
         ("r" "resource" plain "%?"
          :target
          (file+head
           "Resources.org" ("${title}\n:PROPERTIES:\n:NAME: ${title}\n:CREATED:\t%U\n:END:\n\n")))
         ("l" "literature note" plain "%?"
          :target
          (file+head
           "Literature/${citar-citekey}.org"
           "#+TITLE: ${note-title}\n#+FILETAGS: :article:\n* ${note-title}\n")
          :unnarrowed t)
         ("f" "fleeting note" plain "%?"
          :target
          (file+olp
           "Fleeting.org" ("${title}\n:PROPERTIES:\n:NAME: ${title}\n:CREATED:\t%U\n:END:\n\n")))))

 (setq org-roam-dailies-capture-templates
       '(("d"
          "default"
          plain
          "%?"
          :target (file+olp "Journal %<%Y>.org" ("%<%Y-%m>" "%<%Y-%m-%d>"))
          :unnarrowed t)))
 (setq org-roam-mode-sections (list #'org-roam-backlinks-section #'org-roam-reflinks-section))
 (setq org-roam-completion-everywhere t)
 (setq org-roam-db-autosync-mode 1)
 (setq org-roam-db-update-on-save 1))

;;; org-superstar
(use-package org-superstar :hook (org-mode . org-superstar-mode))

;;; Dashboard
(use-package dashboard :config (dashboard-setup-startup-hook) (setq dashboard-startup-banner 3))

;;; vertico
(use-package
 vertico
 :custom
 (vertico-count 13) ; Number of candidates to display
 (vertico-resize t)
 (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
 :config (vertico-mode))

;;; hotfuzz
(use-package hotfuzz :config (setq completion-styles '(hotfuzz)))

;;; citar
(use-package
 citar
 :custom
 (citar-bibliography '("~/org/bibliography.bib"))
 (citar-notes-paths '("~/org/Literature/"))
 (citar-file-note-extensions '("org"))
 (org-cite-insert-processor 'citar)
 (org-cite-activate-processor 'citar)
 (org-cite-follow-processor 'citar)
 :hook
 (LaTeX-mode . citar-capf-setup)
 (org-mode . citar-capf-setup))


;;; citar-org-roam
(use-package citar-org-roam
  :after citar org-roam
 :config
 (citar-org-roam-mode)
 (setq citar-org-roam-note-title-template "${title}")
 (setq citar-org-roam-capture-template-key "l"))

;;; company
(use-package company :config (company-mode 1))

;;; git-auto-commit-mode
(use-package
 git-auto-commit-mode
 :hook (after-save . git-auto-commit-mode)
 :config
 (setq gac-automatically-push-p t)
 (git-auto-commit-mode 1))

;;; git-gutter
(use-package git-gutter :config (global-git-gutter-mode 1))

;;; origami
(use-package origami :config (global-origami-mode 1))

;;; flycheck
(use-package flycheck :config (global-flycheck-mode +1))

;;; rainbow-delimiters
(use-package rainbow-delimiters :hook (after-init . rainbow-delimiter-mode))

;;; auctex
(use-package auctex :config (setq TeX-parse-self t))

;; envrc
(use-package envrc :hook (after-init . envrc-global-mode))

;;; lsp-mode
(use-package
 lsp-mode
 :init
 ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
 (setq lsp-keymap-prefix "C-c l")
 :hook
 ( ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  (python-mode . lsp)
  (nix-mode . lsp)
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration))
 :commands lsp)

;;; lsp-ui
(use-package lsp-ui :commands lsp-ui-mode)

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

(reformatter-define
 python-isort
 :program python-isort-command
 :args (python-isort--make-args beg end)
 :lighter " isort"
 :group 'python-isort)

(defun python-isort--make-args (beg end)
  "Helper to build the argument list for isort for span BEG to END."
  (append python-isort--base-args python-isort-extra-args '("-")))

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

(reformatter-define
 python-black
 :program python-black-command
 :args (python-black--make-args beg end)
 :lighter " black"
 :group 'python-black)

(defun python-black--make-args (beg end)
  "Helper to build the argument list for isort for span BEG to END."
  (append python-black--base-args python-black-extra-args '("-")))

(add-hook 'python-mode-hook 'python-black-on-save-mode)

; nix-nixfmt
(defcustom nix-nixfmt-command "nixfmt"
  "Name of the `nix-nixfmt` executable."
  :group 'nasy
  :type 'string)

;;;###autoload (autoload 'nix-nixfmt-buffer "nix-nixfmt" nil t)
;;;###autoload (autoload 'nix-nixfmt-region "nix-nixfmt" nil t)
;;;###autoload (autoload 'nix-nixfmt-on-save-mode "nix-nixfmt" nil t)

(reformatter-define
 nix-nixfmt
 :program nix-nixfmt-command
 :lighter " nix-nixfmt"
 :group 'nix-nixfmt)

(add-hook 'nix-mode-hook 'nix-nixfmt-on-save-mode)

;;; org-toc
(use-package toc-org :hook (org-mode . toc-org-mode))

;;; annotate
(use-package
 annotate
 :hook ((org-mode . annotate-mode))
 :config (setq annotate-file "~/org/annotations"))

;;; Custom
;;;###autoload
(defun unpackaged/org-fix-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries
   (lambda ()
     (org-with-wide-buffer
      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
      ;; newlines before the current heading, so we do this part widened.
      (while (not (looking-back "\n\n" nil))
        ;; Insert blank lines before heading.
        (insert "\n")))
     (let ((end (org-entry-end-position)))
       ;; Insert blank lines before entry content
       (forward-line)
       (while (and (org-at-planning-p) (< (point) (point-max)))
         ;; Skip planning lines
         (forward-line))
       (while (re-search-forward org-drawer-regexp end t)
         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
         ;; for some reason it doesn't work correctly when operating on hidden text.
         ;; This works, taken from `org-agenda-get-some-entry-text'.
         (re-search-forward "^[ \t]*:END:.*\n?" end t)
         (goto-char (match-end 0)))
       (unless (or (= (point) (point-max)) (org-at-heading-p) (looking-at-p "\n"))
         (insert "\n"))))
   t
   (if prefix
       nil
     'tree)))

(add-hook
 'before-save-hook
 (lambda ()
   (if (eq major-mode 'org-mode) ; Org-mode
       (let ((current-prefix-arg 4)) ; Emulate C-u
         (call-interactively 'unpackaged/org-fix-blank-lines)))))

;;; org-ql
(use-package
 org-ql
 :init
 (setq org-agenda-custom-commands
       '(("z" "Zen View"
          ((org-ql-block
            '(and (level 2 8)
                  (todo "FOCUS")
                  (not (path "Archive" "Inbox")))
            ((org-ql-block-header "Presently Focusing On")))
           (org-ql-block
            '(and (todo)
                  (level 2 8)
                  (deadline :on today)
                  (not (path "Archive")))
            ((org-ql-block-header "Due Today")))
           (org-ql-block
            '(and (todo)
                  (level 2 8)
                  (scheduled :on today)
                  (not (path "Archive")))
            ((org-ql-block-header "Scheduled Today")))
           (org-ql-block
            '(and (level 2 8)
                  (todo "TODAY")
                  (not (path "Archive" "Inbox")))
            ((org-ql-block-header "Planned Today")))
           (org-ql-block
            '(and (done)
                  (level 2 8)
                  (closed :on today)
                  (not (path "Archive")))
            ((org-ql-block-header "Completed Today")))
           (org-ql-block
            '(and (level 2 8) (todo "NEXT") (not (path "Archive" "Inbox")))
            ((org-ql-block-header "Next")))
           (org-ql-block
            '(and (level 2 8)
                  (todo "IN PROGRESS")
                  (not (path "Archive" "Inbox")))
            ((org-ql-block-header "In Progress")))
           (org-ql-block
            '(and (todo)
                  (level 2 8)
                  (deadline :to -1)
                  (not (path "Archive" "Inbox")))
            ((org-ql-block-header "Overdue")))
           (org-ql-block
            '(and (todo)
                  (level 2 8)
                  (scheduled :to -1)
                  (not (path "Archive" "Inbox")))
            ((org-ql-block-header "Reschedule")))
           (org-ql-block
            '(and (todo)
                  (level 2 8)
                  (deadline :from 1 :to 30)
                  (not (path "Archive" "Inbox")))
            ((org-ql-block-header "Due Soon")))
           (org-ql-block
            '(and (todo) (path "Inbox") (not (path "Archive"))) ((org-ql-block-header "Inbox")))))
         ("i" "Inbox"
          ((org-ql-block
            '(and (todo) (path "Inbox") (not (path "Archive"))) ((org-ql-block-header "Inbox")))))
         ("f" "Fleeting Notes"
          ((org-ql-block
            '(and (level 1) (path "Fleeting") (not (path "Archive")))
            ((org-ql-block-header "Fleeting Notes")))))
         ("p" "PARA"
          ((org-ql-block
            '(and (not (path "Archive"))
                  (not (heading "Contents"))
                  (path "Projects")
                  (level 1))
            ((org-ql-block-header "Active Projects")))
           (org-ql-block
            '(and (not (path "Archive"))
                  (not (heading "Contents"))
                  (path "Areas")
                  (level 1))
            ((org-ql-block-header "Active Areas")))
           (org-ql-block
            '(and (not (path "Archive"))
                  (not (heading "Contents"))
                  (path "Resources")
                  (level 1))
            ((org-ql-block-header "Active Resources")))))
         ("r" "Archive"
          ((org-ql-block
            '(and (not (done)) (path "Projects Archive") (level 1))
            ((org-ql-block-header "Archived Projects")))
           (org-ql-block
            '(and (not (done)) (path "Areas Archive") (level 1))
            ((org-ql-block-header "Archived Areas")))
           (org-ql-block
            '(and (not (done)) (path "Resources Archive") (level 1))
            ((org-ql-block-header "Archived Resources"))))))))

;;; org-anki
(use-package org-anki)

;;; python-mode
(use-package python-mode)

;;; which-key
(use-package which-key :config (which-key-mode 1))

;;; evil-nerd-commenter
(use-package evil-nerd-commenter)

;;; aggressive-indent
(use-package aggressive-indent :config (setq global-aggressive-indent-mode 1))

;;; Keybindings

;; General
(global-set-key (kbd "C-c r") 'eval-buffer)
(global-set-key (kbd "C-c y") 'clipboard-yank)
(global-set-key (kbd "C-c M-i") 'org-id-get-create)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c h") 'evil-next-buffer)
(global-set-key (kbd "C-c l") 'evil-prev-buffer)
(global-set-key
 (kbd "C-c t")
 (lambda ()
   (interactive)
   (tab-new)
   (dashboard-open)))
(global-set-key (kbd "C-c w") 'tab-close)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; org-roam
(defhydra
 hydra-org-roam
 (:color pink :hint nil :exit t)
 "
^org-roam^
---------
_f_: Find node (or create)    _s_: Sync database
_i_: Insert node link         _j_: Goto Journal
_b_: Buffer toggle            _q_: Quit
_w_: Refile
"
 ("f" org-roam-node-find)
 ("i" org-roam-node-insert)
 ("b" org-roam-buffer-toggle)
 ("w" org-roam-refile)
 ("s" org-roam-db-sync)
 ("j" org-roam-dailies-goto-today)
 ("q" nil))
(global-set-key (kbd "C-c o") 'hydra-org-roam/body)

;; org-agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; inbox
(global-set-key
 (kbd "C-c M-\\")
 (lambda ()
   (interactive)
   (find-file "~/org/Inbox.org")))

;; org-transclusion
(global-set-key (kbd "C-c M-a") 'org-transclusion-add-all)
(global-set-key (kbd "C-c M-r") 'org-transclusion-remove-all)

;; citar
(defhydra hydra-citar (:color pink :hint nil :exit t) "
^citar^
-------
_o_: Open
_i_: Insert commands
_q_: Quit
"
          ("o" citar-open) ("i" hydra-citar-insert/body :exit t) ("q" nil))

(defhydra
 hydra-citar-insert
 (:color pink :hint nil :exit t)
 "
^citar-insert^
--------------
_c_: Insert Citation       _q_: Quit
_b_: Insert BibTeX entry
_r_: Insert Reference
_k_: Insert Key
"
 ("c" citar-insert-citation)
 ("b" citar-insert-bibtex)
 ("r" citar-insert-reference)
 ("k" citar-insert-key)
 ("q" nil))
(global-set-key (kbd "C-c M-r") 'hydra-citar/body)

;; org-view-mode
(global-set-key (kbd "C-c M-e") 'org-view-mode)

;; unpackaged/org-fix-blank-lines
(global-set-key (kbd "C-c f") 'unpackaged/org-fix-blank-lines)

;; tab deletion
(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-date ((t (:foreground "light gray" :weight normal))))
 '(org-agenda-date-today ((t (:foreground "medium spring green" :weight bold))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "dim gray"))))
 '(org-agenda-date-weekend-today ((t (:inherit org-agenda-date :foreground "dim gray" :weight bold))))
 '(org-agenda-structure-filter ((t nil)))
 '(org-scheduled ((t nil))))
