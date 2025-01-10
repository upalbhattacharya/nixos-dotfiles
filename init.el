;;; Bootstrap elpaca
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca
    :repo "https://github.com/progfolio/elpaca.git"
    :ref nil
    :depth 1
    :files (:defaults "elpaca-test.el" (:exclude "extensions"))
    :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list
   'load-path
   (if (file-exists-p build)
       build
     repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28)
      (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop
                    (apply #'call-process
                           `("git" nil ,buffer t "clone" ,@
                             (when-let* ((depth (plist-get order :depth)))
                               (list (format "--depth=%d" depth) "--no-single-branch"))
                             ,(plist-get order :repo) ,repo))))
                  ((zerop
                    (call-process "git" nil buffer t "checkout" (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop
                    (call-process emacs
                                  nil
                                  buffer
                                  nil
                                  "-Q"
                                  "-L"
                                  "."
                                  "--batch"
                                  "--eval"
                                  "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
          (progn
            (message "%s" (buffer-string))
            (kill-buffer buffer))
          (error
           "%s"
           (with-current-buffer buffer
             (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-date-style 'iso)
 '(custom-safe-themes
   '("6e13ff2c27cf87f095db987bf30beca8697814b90cd837ef4edca18bdd381901" default))
 '(gac-automatically-push-p t)
 '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
 '(org-agenda-block-separator 46)
 '(org-agenda-breadcrumbs-separator " -> ")
 '(org-agenda-files
   '("/home/upal/org/Scratchpad.org" "/home/upal/org/Slip Box.org" "/home/upal/org/marginalia.org"))
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
   '((65 :foreground "#181825" :background "#f38ba8")
     (66 :foreground "#181825" :background "#f9e2af")
     (67 :foreground "#181825" :background "#94e2d5")))
 '(org-ql-search-directories-files-recursive t)
 '(org-roam-capture-new-node-hook
   '(org-id-get-create org-roam-capture--insert-captured-ref-h))
 '(org-super-agenda-date-format "%e %Y-%m-%d")
 '(org-use-property-inheritance '("NAME"))
 '(package-selected-packages
   '(which-key wfnames vline vertico toc-org spacious-padding ruff-format rainbow-delimiters python-mode origami org-view-mode org-transclusion org-superstar org-roam-ui org-contrib org-anki nix-mode magit lsp-ui latexdiff latex-extra hydra hotfuzz gnu-elpa-keyring-update git-gutter git-auto-commit-mode fzf flycheck evil-nerd-commenter evil envrc elisp-autofmt dashboard company citar-org-roam catppuccin-theme bibtex-completion avy async annotate aggressive-indent))
 '(python-isort-extra-args nil))

;;; Use elpaca use-package
(elpaca elpaca-use-package (elpaca-use-package-mode))
(setq use-package-always-ensure t)

(use-package emacs
 :demand t
 :ensure nil
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

;;; Theme
(use-package catppuccin-theme
  :demand t
  :ensure (:wait t)
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package evil
  :demand t
  :ensure (:wait t)
  :config (evil-mode 1))

(use-package org
 :demand t
 :ensure nil
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
 (setq org-startup-folded 'overview)
 (setq org-log-into-drawer t)

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
       '((sequence
          "TODO(t)"
          "NEXT(n/!)"
          "TODAY(T/!)"
          "FOCUS(f/!)"
          "IN PROGRESS(p/!)"
          "LATER(l/!)"
          "|"
          "DONE(d/!)"
          "ARCHIVED(a/!)")))
 (setq org-todo-keyword-faces
       '(("TODO" . (:foreground "#f9e2af" :weight bold))
         ("NEXT" . (:foreground "#cba6f7" :weight bold))
         ("TODAY" . (:foreground "#f2cdcd" :weight bold))
         ("LATER" . (:foreground "#b4befe" :weight bold))
         ("FOCUS" . (:foreground "#f38ba8" :weight bold))
         ("IN PROGRESS" . (:foreground "#89b4fa" :weight bold))
         ("DONE" . (:foreground "#a6e3a1" :weight bold))
         ("ARCHIVED" . (:foreground "#9399b2")))))

 (use-package ob-mermaid
   :demand t
   :ensure (:wait t)
   :config
   (setq ob-mermaid-cli-path "~/mmdc"))
 
 ;; babel
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    ;; (mermaid . t)
    ;; (scheme . t)
    ))

(use-package org-remark
  :demand t
  :ensure (:wait t)
  :hook (after-init . org-remark-global-tracking-mode)
  :config
  (org-remark-create "yellow"
                   '(:background "#f9e2af" :foreground "#181825")
                   '(CATEGORY "warning"))

  (org-remark-create "red"
                   '(:background "#f38ba8" :foreground "#181825")
                   '(CATEGORY "urgent"))

  (org-remark-create "green"
                   '(:background "#a6e3a1" :foreground "#181825")
                   '(CATEGORY "good"))

  (org-remark-create "purple"
                   '(:background "#cba6f7" :foreground "#181825")
                   '(CATEGORY "important"))

  (org-remark-create "blue"
                   '(:background "#89dceb" :foreground "#181825")
                   '(CATEGORY "mark"))
  )


(use-package org-contrib
  :demand t
  :ensure (:wait t))

(use-package org-expiry
 :demand t
 :ensure nil
 :config
 (setq
  org-expiry-created-property-name "CREATED" ; Name of property when an item is created
  org-expiry-inactive-timestamps t ; Don't have everything in the agenda view
  ))

(use-package org-checklist
 :demand t
 :ensure nil)

(use-package org-roam
 :demand t
 :ensure (:wait t)
 :config
 (setq org-roam-directory (file-truename "~/org"))
 (setq org-roam-dailies-directory "~/org/Journal/")
 (setq org-roam-completion-everywhere t)
 (setq org-roam-capture-templates
       '(("d" "default" plain "%?"
          :target
          (file+olp
           "Inbox.org"
           (
            "TODO ${title}
:PROPERTIES:
:NAME: ${title}
:CREATED:\t%U
:ID:
:END:
")))
         ("p" "project" plain "%?"
          :target
          (file+olp
           "Projects.org"
           (
            "TODO ${title} [/]
:PROPERTIES:
:NAME: ${title}
:CREATED:\t%U
:ID:
:END:

[[org-ql-search:(and (todo) (not(done)) (level 2) (property \"NAME\" \"${title}\" inherit))][org-ql-search: Pending Tasks]]
")))
         ("a" "area" plain "%?"
          :target
          (file+olp
           "Areas.org"
           (
            "${title}
:PROPERTIES:
:NAME: ${title}
:CREATED:\t%U
:ID:
:END:

[[org-ql-search:(and (todo) (not(done)) (level 2) (property \"NAME\" \"${title}\" inherit))][org-ql-search: Pending Tasks]]
")))
         ("r" "resource" plain "%?"
          :target
          (file+head
           "Resources.org"
           (
            "${title}
:PROPERTIES:
:NAME: ${title}
:CREATED:\t%U
:ID:
:END:

[[org-ql-search:(and (todo) (not(done)) (level 2) (property \"NAME\" \"${title}\" inherit))][org-ql-search: Pending Tasks]]
")))
         ("l" "literature note" plain "%?"
          :target
          (file+head
           "Literature/${citar-citekey}.org"
           "#+TITLE: ${note-title}\n#+FILETAGS: :article:\n* ${note-title}\n")
          :unnarrowed t)
         ("f" "fleeting note" plain "%?"
          :target
          (file+olp
           "Fleeting.org" ("${title}\n:PROPERTIES:\n:NAME: ${title}\n:CREATED:\t%U\n:ID:\n:END:\n\n")))))

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

(use-package org-superstar
  :demand t
  :ensure (:wait t)
  :hook (org-mode . org-superstar-mode))

(use-package avy
  :demand t
  :ensure t)

(use-package dashboard
  :demand t
  :ensure (:wait t)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 3)
  (setq dashboard-items '((recents . 5) (bookmarks . 5) (registers . 5))))

(use-package vertico
  :demand t
  :ensure (:wait t)
  :custom
  (vertico-count 13) ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config (vertico-mode))

(use-package hotfuzz
  :demand t
  :ensure (:wait t)
  :config (setq completion-styles '(hotfuzz)))

(use-package citar
  :demand t
  :ensure (:wait t)
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

(use-package citar-org-roam
  :demand t
  :ensure (:wait t)
  :after
  citar
  org-roam
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template "${title}")
  (setq citar-org-roam-capture-template-key "l"))

(use-package company
  :demand t
  :ensure (:wait t)
  :config (company-mode 1))

(use-package git-auto-commit-mode
  :demand t
  :ensure (:wait t)
  :hook (after-save . git-auto-commit-mode)
  :config
  (setq gac-automatically-push-p t)
  (git-auto-commit-mode 1))

(use-package git-gutter
  :demand t
  :ensure (:wait t)
  :config (global-git-gutter-mode 1))

(use-package origami
  :demand t
  :ensure (:wait t)
  :config (global-origami-mode 1))

(use-package flycheck
  :demand t
  :ensure (:wait t)
  :config (global-flycheck-mode +1))

(use-package rainbow-delimiters
  :demand t
  :ensure (:wait t)
  :hook (after-init . rainbow-delimiter-mode))

(use-package auctex
  :demand t
  :ensure (:wait t)
  :config (setq TeX-parse-self t))

(use-package envrc
  :demand t
  :ensure (:wait t)
  :hook (after-init . envrc-global-mode))

(use-package lsp-mode
  :demand t
  :ensure (:wait t)
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

(use-package lsp-ui
  :demand t
  :ensure (:wait t)
  :commands lsp-ui-mode)

(use-package reformatter
  :demand t
  :ensure (:wait t))
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

(use-package org-make-toc
  :demand t
  :ensure (:wait t))

(use-package annotate
  :demand t
  :ensure (:wait t)
  :hook ((org-mode . annotate-mode))
  :config (setq annotate-file "~/org/annotations"))

(use-package org-ql
  :demand t
  :ensure (:wait t)
  :init
  (setq org-agenda-custom-commands
        '(("z" "Zen View"
           ((org-ql-block
             '(and (level 2 8)
                   (todo "FOCUS")
                   (not (path "Archive" "Inbox"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "Presently Focusing On"))
             )
            (org-ql-block
             '(and (todo)
                   (level 2 8)
                   (deadline :on today)
                   (not (path "Archive"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "Due Today"))
             )
            (org-ql-block
             '(and (todo)
                   (level 2 8)
                   (scheduled :on today)
                   (not (path "Archive"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "Scheduled Today"))
             )
            (org-ql-block
             '(and (level 2 8)
                   (todo "TODAY")
                   (not (path "Archive" "Inbox"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "Planned Today"))
             )
            (org-ql-block
             '(and (done)
                   (level 2 8)
                   (closed :on today)
                   (not (path "Archive"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "Completed Today"))
             )
            (org-ql-block
             '(and (level 2 8)
                   (todo "NEXT")
                   (not (path "Archive" "Inbox"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "Next"))
             )
            (org-ql-block
             '(and (level 2 8)
                   (todo "IN PROGRESS")
                   (not (path "Archive" "Inbox"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "In Progress"))
             )
            (org-ql-block
             '(and (todo)
                   (level 2 8)
                   (deadline :to -1)
                   (not (path "Archive" "Inbox"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "Overdue"))
             )
            (org-ql-block
             '(and (todo)
                   (level 2 8)
                   (scheduled :to -1)
                   (not (path "Archive" "Inbox"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "Reschedule"))
             )
            (org-ql-block
             '(and (todo)
                   (level 2 8)
                   (deadline :from 1 :to 30)
                   (not (path "Archive" "Inbox"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "Due Soon"))
             )
            (org-ql-block
             '(and (todo)
                   (path "Inbox")
                   (not (path "Archive"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "Inbox"))
             )
        ))
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

(use-package org-anki
  :demand t
  :ensure (:wait t))

(use-package python-mode
  :demand t
  :ensure t)

(use-package which-key
  :demand t
  :ensure (:wait t)
  :config (which-key-mode 1))

(use-package evil-nerd-commenter
  :demand t
  :ensure (:wait t))

(use-package aggressive-indent
  :demand t
  :ensure (:wait t)
  :config (setq global-aggressive-indent-mode 1))

(use-package hydra
  :demand t
  :ensure (:wait t))

(use-package elisp-autofmt
  :demand t
  :ensure (:wait t))

(use-package nix-mode
  :demand t
  :ensure (:wait t))

(use-package latex-extra
  :demand t
  :ensure (:wait t))

(use-package latexdiff
  :demand t
  :ensure (:wait t))

(use-package org-view-mode
  :demand t
  :ensure (:wait t))

(use-package magit
  :demand t
  :ensure (:wait t))

(use-package org-roam-ui
  :demand t
  :ensure (:wait t))

(use-package spacious-padding
  :demand t
  :ensure (:wait t))

(use-package fzf
  :demand t
  :ensure (:wait t))

(use-package org-transclusion
  :demand t
  :ensure (:wait t))

(defun workboots/insert-todo-metadata ()
  (org-expiry-insert-created)
  (org-id-get-create)
  (org-back-to-heading)
  (org-end-of-line))

;; Whenever a TODO entry is created, I want a timestamp
;; Advice org-insert-todo-heading to insert a created timestamp using org-expiry
(defadvice org-insert-todo-heading (after workboots/insert-todo-metadata activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (workboots/insert-todo-metadata))
;; Make it active
(ad-activate 'org-insert-todo-heading)

(defadvice org-insert-todo-subheading (after workboots/insert-todo-metadata activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (workboots/insert-todo-metadata))
;; Make it active
(ad-activate 'org-insert-todo-subheading)

(defadvice org-insert-heading (after workboots/insert-todo-metadata activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (workboots/insert-todo-metadata))
;; Make it active
(ad-activate 'org-insert-heading)

(defadvice org-insert-heading-after-current (after workboots/insert-todo-metadata activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (workboots/insert-todo-metadata))
;; Make it active
(ad-activate 'org-insert-heading-after-current)

(defadvice org-insert-heading-respect-content (after workboots/insert-todo-metadata activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (workboots/insert-todo-metadata))
;; Make it active
(ad-activate 'org-insert-heading-respect-content)

(defadvice org-insert-subheading (after workboots/insert-todo-metadata activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (workboots/insert-todo-metadata))
;; Make it active
(ad-activate 'org-insert-subheading)

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

;; time-based
(defhydra
 hydra-time
 (:color pink :hint nil :exit t)
 "
^Time-based^
------------
_d_: Set deadline     _a_: Goto active                _x_: Remove overlays
_s_: Set schedule     _t_: Display time logged        _p_: Inactive timestamp
_i_: Clock in         _c_: Cancel logging             _v_: Active timestamp
_o_: Clock out        _r_: Insert report              _q_: Quit
"
 ("d" org-deadline)
 ("s" org-schedule)
 ("i" org-clock-in)
 ("o" org-clock-out)
 ("a" org-clock-goto)
 ("t" org-clock-display)
 ("x" org-clock-remove-overlays)
 ("c" org-clock-cancel)
 ("r" org-clock-report)
 ("p" org-time-stamp-inactive)
 ("v" org-time-stamp-active)
 ("q" nil))
(global-set-key (kbd "C-c M-t") 'hydra-time/body)

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


(defhydra
 hydra-org-remark
 (:color pink :hint nil :exit t)
 "
^org-remark^
------------
_b_: Blue highlight      _p_: Purple highlight
_g_: Green highlight     _d_: Delete highlight
_y_: Yellow highlight    _q_: Quit
_r_: Red highlight
"
("b" org-remark-mark-blue)
("g" org-remark-mark-green)
("y" org-reamrk-mark-yellow)
("r" org-remark-mark-red)
("p" org-remark-mark-purple)
("d" org-remark-remove)
 ("q" nil))
(global-set-key (kbd "C-c M-v") 'hydra-org-remark/body)

(defhydra
 hydra-org-annotate
 (:color pink :hint nil :exit t)
 "
^org-annotate^
--------------
_a_: Add annotation          _p_: Go-to previous annotation     _q_: Quit
_w_: Save annotations        _d_: Delete annotation
_l_: Load annotations        _c_: Clear annotations
_n_: Go-to next annotation   _t_: Toggle annotation
"
("a" annotate-annotate)
("d" annotate-delete-annotation)
("c" annotate-clear-annotations)
("w" annotate-save-annotations)
("l" annotate-load-annotations)
("n" annotate-goto-next-annotation)
("p" annotate-goto-previous-annotation)
("t" annotate-toggle-annotation-text)
 ("q" nil))
(global-set-key (kbd "C-c M-m") 'hydra-org-annotate/body)
;; org-view-mode
(global-set-key (kbd "C-c M-e") 'org-view-mode)

;; unpackaged/org-fix-blank-lines
(global-set-key (kbd "C-c f") 'unpackaged/org-fix-blank-lines)

;; tab deletion
(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

;; avy
(global-set-key (kbd "C-M-'") 'avy-goto-char-2)
