;; Bootstrap elpaca
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
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
 '(citar-file-open-functions
   '(("html" . citar-file-open-external) (t . find-file)
     ("pdf" . citar-file-open-external)))
 '(consult-narrow-key "C->")
 '(consult-widen-key "C-<")
 '(custom-safe-themes
   '("6e13ff2c27cf87f095db987bf30beca8697814b90cd837ef4edca18bdd381901"
     default))
 '(elpaca-menu-functions
   '(elpaca-menu-lock-file elpaca-menu-extensions elpaca-menu-org
                           elpaca-menu-declarations elpaca-menu-melpa
                           elpaca-menu-gnu-elpa
                           elpaca-menu-non-gnu-elpa))
 '(gac-automatically-push-p t)
 '(global-text-scale-adjust-resizes-frames t)
 '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
 '(ignored-local-variable-values '((org-confirm-babel-evaluate)))
 '(org-agenda-breadcrumbs-separator " -> ")
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s") (todo . " %i %-12:c")
     (tags . " %i %-12:c") (search . " %i %-12:c")))
 '(org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo deadline-up category-keep priority-down)
     (tags priority-down category-keep) (search category-keep)))
 '(org-agenda-todo-keyword-format "%-1s")
 '(org-export-backends '(ascii html icalendar latex odt org))
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.2 :html-foreground
                 "Black" :html-background "Transparent" :html-scale
                 2.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-indirect-buffer-display 'current-window)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news) (file . find-file)
     (wl . wl-other-frame)))
 '(org-log-into-drawer t)
 '(org-preview-latex-process-alist
   '((dvipng :programs ("latex" "dvipng") :description "dvi > png"
             :message
             "you need to install the programs: latex and dvipng."
             :image-input-type "dvi" :image-output-type "png"
             :image-size-adjust (1.0 . 1.0) :latex-compiler
             ("latex -interaction nonstopmode -output-directory %O %F")
             :image-converter ("dvipng -D %D -T tight -o %O %F")
             :transparent-image-converter
             ("dvipng -D %D -T tight -bg Transparent -o %O %F"))
     (dvisvgm :programs ("latex" "dvisvgm") :description "dvi > svg"
              :message
              "you need to install the programs: latex and dvisvgm."
              :image-input-type "dvi" :image-output-type "svg"
              :image-size-adjust (1.7 . 1.5) :latex-compiler
              ("latex -interaction nonstopmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
     (imagemagick :programs ("latex" "convert") :description
                  "pdf > png" :message
                  "you need to install the programs: latex and imagemagick."
                  :image-input-type "pdf" :image-output-type "png"
                  :image-size-adjust (1.0 . 1.0) :latex-compiler
                  ("pdflatex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim -antialias %f -quality 100 %O"))))
 '(org-priority-faces
   '((65 :foreground "#181825" :background "#f38ba8")
     (66 :foreground "#181825" :background "#f9e2af")
     (67 :foreground "#181825" :background "#94e2d5")))
 '(org-ql-search-directories-files-recursive t)
 '(org-ql-search-directories-files-regexp ".org$")
 '(org-remark-icon-notes " 󰍩 ")
 '(org-remark-line-margin-side 'right-margin)
 '(org-remark-line-minimum-margin-width 10)
 '(org-remark-notes-display-buffer-action
   '((display-buffer-in-side-window) (side . right) (slot . 1)
     (window-width . 85)))
 '(org-transclusion-extensions '(org-transclusion-src-lines org-transclusion-font-lock))
 '(org-use-property-inheritance '("HEAD"))
 '(package-selected-packages
   '(adaptive-wrap which-key wfnames vertico toc-org spacious-padding
                   ruff-format rainbow-delimiters python-mode
                   projectile ox-pandoc origami org-view-mode
                   org-transclusion org-superstar org-roam-ui
                   org-roam-bibtex org-ql org-modern org-contrib
                   org-anki orderless nix-mode magit lsp-ui latexdiff
                   latex-extra hydra htmlize hotfuzz git-gutter
                   git-auto-commit-mode fzf flycheck
                   evil-nerd-commenter evil envrc elisp-autofmt
                   dashboard company citar-org-roam catppuccin-theme
                   avy async annotate aggressive-indent))
 '(python-isort-extra-args nil)
 '(vertico-sort-function nil))

;;; Use elpaca use-package
(elpaca elpaca-use-package (elpaca-use-package-mode))
(setq use-package-always-ensure t)

(use-package emacs
  :demand t
  :ensure nil
  :config
  (setq warning-minimum-level :error)
  (setq confirm-kill-emacs 'y-or-n-p)
  (set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 200)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font" :height 200)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Nerd Font" :height 1.3)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tab-bar-mode 1)
  (global-display-line-numbers-mode 1)
  (setq inhibit-startup-screen t)
  (setq auto-save-file-name-transforms `((".*" "/tmp/" t)))
  (setq backup-directory-alist '((".*" . "/tmp")))
  (setq kill-buffer-delete-auto-save-files t)
  (setq display-line-numbers-type 'visual)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq python-indent-level 4)
  (setq visible-bell t)
  (setq truncate-partial-width-windows nil)
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Theme
(use-package catppuccin-theme
  :demand t
  :ensure (:wait t :host github :repo "catppuccin/emacs")
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package evil
  :demand t
  :ensure (:wait t :host github :repo "emacs-evil/evil")
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "wq" 'save-and-kill-this-buffer)
  (defun save-and-kill-this-buffer()(interactive)(save-buffer)(kill-current-buffer))
  )

(use-package org
  :demand t
  :ensure nil
  :custom-face (org-document-title ((t (:foreground "dim gray" :weight bold :height 1.0))))
  ;; :hook (org-mode . org-indent-mode)
  ;; :hook (org-mode . visual-line-mode)
  :hook (org-mode . flyspell-mode)
  :hook (org-capture-mode . org-id-get-create)
  :hook (org-mode . (lambda nil
                      (set-fill-column 78)))
  :config
  (define-key minibuffer-local-completion-map (kbd "?") nil)
  (setq org-cycle-include-plain-lists 'integrate)
  (setq org-deadline-warning-days 0)
  (setq org-cycle-separator-lines 1)
  (setq org-hide-emphasis-markers t)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-hide-leading-stars nil)
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
  
  ;;org-cite
  (setq org-cite-global-bibliography '("~/org/bibliography.bib"))

  ;; org-agenda
  (setq org-agenda-files 
        (seq-filter (lambda(x) (not (string-match "marginalia.org" (file-name-nondirectory x)))) 
                    (directory-files-recursively "~/org/" "\\.org$")
                    ))
  ;; (setq org-agenda-file-regexp "^[a-z0-9-_]+.org")
  (setq org-agenda-start-day "+0d")
  (setq org-agenda-window-setup 'other-tab)
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timeline-if-deadline-is-shown t)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-block-separator 46)
  (setq org-agenda-include-deadlines t)
  (setq org-columns-default-format-for-agenda
        "%12TODO(STATUS) %50ITEM %30HEAD(HEAD) %20CATEGORY(PARA) %PRIORITY(PR.) %DEADLINE")
  (setq org-agenda-with-colors t)
  (setq org-log-done t)
  (setq org-agenda-start-with-log-mode t)

  ;; org-todo
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "NEXT(n/!)"
           "TODAY(T/!)"
           "IN PROGRESS(p/!)"
           "REVIEW(r/!)"
           "LATER(l/!)"
           "READING(R/!)"
           "|"
           "DONE(d/!)"
           "ARCHIVED(a/!)")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#f9e2af" :weight bold :underline t))
          ("NEXT" . (:foreground "#f5c2e7" :weight bold :underline t))
          ("TODAY" . (:foreground "#f2cdcd" :weight bold :underline t))
          ("IN PROGRESS" . (:foreground "#89b4fa" :weight bold :underline t))
          ("REVIEW" . (:foreground "#cba6f7" :weight bold :underline t))
          ("LATER" . (:foreground "#b4befe" :weight bold :underline t))
          ("READING" . (:foreground "#f5c2e7" :weight bold :underline t))
          ("DONE" . (:foreground "#a6e3a1" :weight bold :underline t))
          ("ARCHIVED" . (:foreground "#9399b2" :underline t)))))

(use-package transient
  :demand t
  :ensure (:wait t :host github :repo "magit/transient"))

(use-package magit
  :demand t
  :ensure (:wait t :host github :repo "magit/magit"))

; TODO Fix later
(use-package peg
  :demand t
  :ensure (:wait t))

(use-package queue
  :demand t
  :ensure (:wait t :host github :repo "emacs-straight/queue" :branch "master"))

(use-package citeproc
  :demand t
  :ensure (:wait t :host github :repo "andras-simonyi/citeproc-el"))

(use-package visual-fill-column
  :demand t
  :ensure (:wait t :host "codeberg.org" :repo "joostkremers/visual-fill-column")
  :hook (org-mode . visual-line-fill-column-mode)
  :config
  (setq-default visual-fill-column-center-text t))

;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (plantuml . t)
   ))

(use-package org-contrib
  :demand t
  :ensure (:wait t :host github :repo "emacsmirror/org-contrib"))

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
  :ensure (:wait t :host github :repo "org-roam/org-roam")
  :bind (
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (cl-defmethod org-roam-node-status ((node org-roam-node))
    (let ((status (org-roam-node-todo node)))
      (if status
          (propertize (format "%s" status) 'face (org-get-todo-face (format "%s" status)))
        )
      ))
  (cl-defmethod org-roam-node-filename ((node org-roam-node))
    (file-name-base (org-roam-node-file node)
                    ))
  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    (let ((level (org-roam-node-level node)))
      (concat
       (when (> level 0) (org-roam-node-file-title node))
       (when (> level 1) (concat " > " (string-join (org-roam-node-olp node) " > ")) ))
      ))
  (setq org-roam-node-display-template "${status:13} ${title:50} ${hierarchy:*}")
  (setq org-roam-directory (file-truename "~/org/"))
  (setq org-roam-dailies-directory "~/org/Journal/")
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "IGNORE_ORG_ROAM" (org-get-tags)))))
  (setq org-roam-capture-templates
        '(
          ("d" "default" entry "* ${title}\n:PROPERTIES:\n:HEAD:\t${title}\n:ID:\t%(org-id-uuid)\n:CREATED:\t%U\n:CATEGORY: Inbox\n:END:\n"
           :target (file "Inbox.org") :empty-lines 1)

          ("p" "project" entry "* [%] ${title}\n:PROPERTIES:\n:HEAD:\t${title}\n:ID:\t%(org-id-uuid)\n:CREATED:\t%U\n:CATEGORY: Project\n:END:\n
    [[org-ql-search:(and (todo) (not(done)) (level 2) (property \"HEAD\" \"${title}\" inherit))][org-ql-search: Pending Tasks]]"
           :target (file "Active.org") :empty-lines 1)

          ("a" "area" entry "* ${title}\n:PROPERTIES:\n:HEAD:\t${title}\n:ID:\t%(org-id-uuid)\n:CREATED:\t%U\n:CATEGORY: Area\n:END:\n
    [[org-ql-search:(and (todo) (not(done)) (level 2) (property \"HEAD\" \"${title}\" inherit))][org-ql-search: Pending Tasks]]"
           :target (file "Active.org") :empty-lines 1)

          ("r" "resource" entry "* ${title}\n:PROPERTIES:\n:HEAD:\t${title}\n:ID:\t%(org-id-uuid)\n:CREATED:\t%U\n:CATEGORY: Resource\n:END:\n
    [[org-ql-search:(and (todo) (not(done)) (level 2) (property \"HEAD\" \"${title}\" inherit))][org-ql-search: Pending Tasks]]"
           :target (file "Active.org") :empty-lines 1)

          ("l" "literature note" entry "* TODO ${note-title}\n:PROPERTIES:\n:HEAD:\t${note-title}\n:ID:\t%(org-id-uuid)\n:CREATED:\t%U\n:ROAM_REFS:\t\n:CATEGORY: Literature\n:END:\n"
           :target (file "Literature.org") :empty-lines 1)
          ))

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

(use-package adaptive-wrap
  :demand t
  :ensure (:wait t :host github :repo "emacsmirror/adaptive-wrap" :branch "master")
  :hook (org-mode . adaptive-wrap-prefix-mode))

(use-package avy
  :demand t
  :ensure (:wait t :host github :repo "abo-abo/avy"))

(use-package vertico
  :demand t
  :ensure (:wait t :host github :repo "minad/vertico")
  :custom
  (vertico-count 13) ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle t) ; Go from last to first candidate and first to last (cycle)?
  :config (vertico-mode))

(use-package hotfuzz
  :demand t
  :ensure (:wait t :host github :repo "axelf4/hotfuzz")
  :config (setq completion-styles '(hotfuzz)))

(use-package citar
  :demand t
  :ensure (:wait t :host github :repo "emacs-citar/citar")
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
  :ensure (:wait t :host github :repo "emacs-citar/citar-org-roam")
  :after
  citar
  org-roam
  :config
  (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template "${title}")
  (setq citar-org-roam-capture-template-key "l"))

(use-package corfu
  :demand t
  :ensure (:wait t :host github :repo "minad/corfu")
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto-prefix 2))

(use-package git-auto-commit-mode
  :demand t
  :ensure (:wait t :host github :repo "ryuslash/git-auto-commit-mode")
  :hook (after-save . git-auto-commit-mode)
  :config
  (setq gac-automatically-push-p t)
  (git-auto-commit-mode 1))

(use-package git-gutter
  :demand t
  :ensure (:wait t :host github :repo "emacsorphanage/git-gutter")
  :config (global-git-gutter-mode 1))

(use-package origami
  :demand t
  :ensure (:wait t :host github :repo "gregsexton/origami.el")
  :config (global-origami-mode 1))

(use-package flycheck
  :demand t
  :ensure (:wait t :host github :repo "flycheck/flycheck")
  :config (global-flycheck-mode +1))

(use-package rainbow-delimiters
  :demand t
  :ensure (:wait t :host github :repo "Fanael/rainbow-delimiters")
  :hook (after-init . rainbow-delimiter-mode))

(use-package auctex
  :demand t
  :ensure (:wait t :host github :repo "emacs-straight/auctex" :branch "master")
  :config (setq TeX-parse-self t))

(use-package envrc
  :demand t
  :ensure (:wait t :host github :repo "purcell/envrc")
  :hook (after-init . envrc-global-mode))

(use-package spinner
  :ensure (:wait t :host github :repo "Malabarba/spinner.el"))

(use-package lsp-mode
  :demand t
  :ensure (:wait t :host github :repo "emacs-lsp/lsp-mode")
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ( ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
   (python-mode . lsp-deferred)
   (nix-mode . lsp-deferred)
   ;; if you want which-key integration
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)

(use-package lsp-ui
  :demand t
  :ensure (:wait t :host github :repo "emacs-lsp/lsp-ui")
  :config
  (setq lsp-ui-sideline-show-diagnostics +1)
  (setq lsp-ui-doc-enable +1)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-delay 3)
  :commands lsp-ui-mode)

(use-package reformatter
  :demand t
  :ensure (:wait t :host github :repo "purcell/emacs-reformatter"))
;; python
; isort
(defcustom python-isort-command "isort"
  "Name of the `isort` executable."
  :group 'formatter
  :type 'string)

(defvar python-isort--base-args '("--quiet" "--atomic" "--fass")
  "Base arguments to pass to isort.")

(defcustom python-isort-extra-args nil
  "Extra arguments to pass to isort."
  :group 'formatter
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
  :group 'formatter
  :type 'string)

(defvar python-black--base-args '("--quiet")
  "Base arguments to pass to black.")

(defcustom python-black-extra-args nil
  "Extra arguments to pass to black."
  :group 'formatter
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
  :group 'formatter
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
  :ensure (:wait t :host github :repo "alphapapa/org-make-toc"))

(use-package org-super-agenda
  :demand t
  :ensure (:wait t :host github :repo "alphapapa/org-super-agenda")
  :config
  (org-super-agenda-mode +1)
  (setq org-super-agenda-header-separator "
")
  (setq org-super-agenda-header-prefix " "))

;; Custom org-super-agenda auto property with CATEGORY: propertyValue format
(org-super-agenda--def-auto-group para "the given property"
  :key-form (concat
             (org-entry-get (org-super-agenda--get-marker item)
                            "CATEGORY"
                            org-super-agenda-properties-inherit)
             (concat ": " (org-entry-get (org-super-agenda--get-marker item)
                                         (car args)
                                         org-super-agenda-properties-inherit)))
  :header-form (string-trim (format "%s" key) "[:\r\t\n]+"))


(use-package org-ql
  :demand t
  :ensure (:wait t :host github :repo "alphapapa/org-ql")
  :init
  (setq org-agenda-custom-commands
        '(("z" "A to Z View"
           (
            ;; Due Soon
            (org-ql-block
             '(and (todo)
                   (deadline :from 1 :to 30)
                   (category "Project" "Area" "Inbox" "Resource" "Literature")
                   (not (path "Archive"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "Due Soon")
              (org-super-agenda-groups '((:auto-para "HEAD")))))

            ;; Running
            (org-ql-block
             '(and (todo "NEXT" "IN PROGRESS" "REVIEW")
                   (category "Project" "Area" "Inbox" "Resource" "Literature")
                   (not (path "Archive"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "\nRunning")
              (org-super-agenda-groups '((:auto-para "HEAD")))))

            ;; Overdue
            (org-ql-block
             '(and (todo)
                   (deadline :from -20 :to -1 )
                   (category "Project" "Area" "Inbox" "Resource" "Literature")
                   (not (path "Archive"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "\nOverdue")
              (org-super-agenda-groups '((:auto-para "HEAD")))))

            ;; Stale
            (org-ql-block
             '(and (todo)
                   (deadline :to -21)
                   (category "Project" "Area" "Inbox" "Resource" "Literature")
                   (not (path "Archive"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "\nStale")
              (org-super-agenda-groups '((:auto-para "HEAD")))))


            ;; Reschedule
            (org-ql-block
             '(and (todo)
                   (scheduled :to -1)
                   (category "Project" "Area" "Inbox" "Resource" "Literature")
                   (not (path "Archive"))
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "\nReschedule")
              (org-super-agenda-groups '((:auto-para "HEAD")))))

            ;; Completed
            (org-ql-block
             '(and (done)
                   (closed :on today)
                   (category "Project" "Area" "Inbox" "Resource" "Literature")
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "\nCompleted Today")
              (org-super-agenda-groups '((:auto-para "HEAD")))))
            ))
          ("T" "Today View"
           (
            ;; Today
            (org-ql-block
             '(and
               (todo)
               (not (and
                     (not (todo "TODAY"))
                     (not (deadline :on today))
                     (not (scheduled :on today))))
               (category "Project" "Area" "Inbox" "Resource" "Literature")
               (not (path "Archive"))
               (not (tags "IGNORE_AGENDA")))
             (
              (org-ql-block-header "Today")
              (org-super-agenda-groups '((:auto-para "HEAD")))))
            (org-ql-block
             '(and
               (todo "READING")
               (category "Literature")
               (not (path "Archive"))
               (not (tags "IGNORE_AGENDA")))
             (
              (org-ql-block-header "\nReading (Literature)")
              (org-super-agenda-groups '((:auto-para "HEAD")))))
            ;; Completed
            (org-ql-block
             '(and (done)
                   (closed :on today)
                   (category "Project" "Area" "Inbox" "Resource" "Literature")
                   (not (tags "IGNORE_AGENDA")))
             ((org-ql-block-header "\nCompleted Today")
              (org-super-agenda-groups '((:auto-para "HEAD")))))
            ))
          ("p" "PARA"
           ((org-ql-block
             '(and (path "Active")
                   (category "Project")
                   (level 1)
                   (not (heading "Contents")))
             ((org-ql-block-header "Active Projects")))
            (org-ql-block
             '(and (path "Active")
                   (category "Area")
                   (level 1)
                   (not (heading "Contents")))
             ((org-ql-block-header "Active Areas")))
            (org-ql-block
             '(and (path "Active")
                   (category "Resource")
                   (level 1)
                   (not (heading "Contents")))
             ((org-ql-block-header "Active Resources"))))))
        ))
;; (defun workboots/org-ql-view--format-element (orig-fun &rest args)
;;   "This function will intercept the original function and
;; add the category to the result.

;; ARGS is `element' in `org-ql-view--format-element'"
;;   (if (not args)
;;       ""
;;     (let* ((element args)
;;            (properties (cadar element))
;;            (result (apply orig-fun element))
;;            (smt "")
;;            (category (org-entry-get (plist-get properties :org-marker) "CATEGORY")))
;;       (if (> (length category) 11)
;;           (setq category (substring category 0 10)))
;;       (if (< (length category) 11)
;;           (setq smt (make-string (- 11 (length category)) ?\s)))
;;       (org-add-props
;;           (format "   %-8s %s" (concat category ":" smt) result)
;;           (text-properties-at 0 result)))))
;; (advice-add 'org-ql-view--format-element :around #'workboots/org-ql-view--format-element)

(use-package which-key
  :demand t
  :ensure (:wait t :host github :repo "justbur/emacs-which-key")
  :config (which-key-mode 1))

(use-package aggressive-indent
  :demand t
  :ensure (:wait t :host github :repo "Malabarba/aggressive-indent-mode")
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package hydra
  :demand t
  :ensure (:wait t :host github :repo "abo-abo/hydra"))

(use-package nix-mode
  :demand t
  :ensure (:wait t :host github :repo "NixOS/nix-mode"))

(use-package spacious-padding
  :demand t
  :ensure (:wait t :host github :repo "protesilaos/spacious-padding"))

(use-package fzf
  :demand t
  :ensure (:wait t :host github :repo "bling/fzf.el"))

(use-package org-transclusion
  :demand t
  :ensure (:wait t :host github :repo "nobiot/org-transclusion")
  :hook (org-mode . org-transclusion-mode))

(use-package plantuml-mode
  :demand t
  :ensure (:wait t :host github :repo "skuro/plantuml-mode")
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/plantuml.jar"))
  (setq plantuml-jar-path (expand-file-name "~/plantuml.jar")))

(use-package textsize
  :demand t
  :ensure (:wait t :host github :repo "WJCFerguson/textsize")
  :init (textsize-mode)
  :config
  (setq textsize-default-points 18))

(use-package python-mode
  :demand t
  :ensure (:wait t :host github :repo "emacsmirror/python-mode")
  :after lsp-mode
  :mode ("\.py$")
  :hook (python-mode . lsp-deferred))

(use-package org-appear
  :demand t
  :ensure (:wait t :host github :repo "awth13/org-appear")
  :commands (org-appear-mode)
  :hook     (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t)  ; Must be activated for org-appear to work
  (setq org-appear-autoemphasis   t   ; Show bold, italics, verbatim, etc.
        org-appear-autolinks      t   ; Show links
		org-appear-autosubmarkers t)) ; Show sub- and superscripts

(use-package consult
  :demand t
  :ensure (:wait t :host github :repo "minad/consult"))

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun workboots/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

(defun workboots/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(use-package elfeed
  :demand t
  :ensure (:wait t :host github :repo "skeeto/elfeed")
  :bind (:map elfeed-search-mode-map
              ("q" . workboots/elfeed-save-db-and-bury)))

(use-package elfeed-org
  :demand t
  :ensure (:wait t :host github :repo "remyhonig/elfeed-org")
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/RSS Feeds.org")))

(use-package company
  :demand t
  :ensure (:wait t :host github :repo "company-mode/company-mode")
  :init
  (global-company-mode)
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-backends '(company-capf))

;; Scimax
(org-babel-load-file "~/.emacs.d/scimax/scimax-editmarks.org")
(add-hook 'org-mode-hook 'sem-mode)

(defun workboots/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --smart-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

(defun workboots/insert-todo-metadata ()
  (org-expiry-insert-created)
  (org-id-get-create)
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

(defadvice org-insert-todo-heading-respect-content (after workboots/insert-todo-metadata activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (workboots/insert-todo-metadata))
;; Make it active
(ad-activate 'org-insert-todo-heading-respect-content)

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
 '(company-preview ((t (:background "#6C7086" :foreground "#F5C2e7"))))
 '(org-agenda-structure ((t (:foreground "#f38ba8" :weight bold))))
 '(org-super-agenda-header ((t (:foreground "#94e2d5" :slant italic :weight light)))))
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

(defun org-add-hard-indentation (arg)
  (interactive "p")
  (save-excursion
    (beginning-of-buffer)
    (while
        (let ((indent-size
               (progn
                 (when (re-search-forward "^\\*+ " nil t)
                   (length (match-string 0))))))
          (when indent-size
            (let ((start
                   (progn
                     (next-line)
                     (move-beginning-of-line 1)
                     (point)))
                  (end
                   (progn
                     (if (re-search-forward "^\\*+ " nil t)
                         (progn (previous-line) (move-end-of-line 1))
                       (end-of-buffer))
                     (point))))

              (indent-rigidly start end (* indent-size arg))))
          indent-size))))

;;; Keybindings

;; General
(global-set-key (kbd "C-c r") 'eval-buffer)
(global-set-key (kbd "C-c y") 'clipboard-yank)
(global-set-key (kbd "C-c M-i") 'org-id-get-create)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c h") 'evil-next-buffer)
(global-set-key (kbd "C-c l") 'evil-prev-buffer)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(global-set-key
 (kbd "C-c t")
 (lambda ()
   (interactive)
   (tab-new)
   (scratch-buffer)))
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
  ("v" org-time-stamp)
  ("q" nil))
(global-set-key (kbd "C-c M-t") 'hydra-time/body)

;; org-roam
(defhydra
  hydra-org-roam
  (:color pink :hint nil :exit t)
  "
^org-roam^
---------
_c_: Capture                  _j_: Goto Journal
_i_: Insert node link         _s_: Sync database
_b_: Buffer toggle            _f_: Find (consult)
_w_: Refile                   _q_: Quit
"
  ("f" consult-org-agenda)
  ("c" org-roam-capture)
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
(defhydra hydra-org-transclusion (:color pink :hint nil :exit t) "
^org-transclusion^
------------------
_a_: Add        _o_: Open
_A_: Add all    _l_: Live commands
_r_: Remove
_R_: Remove all
"
  ("a" org-transclusion-add)
  ("A" org-transclusion-add-all)
  ("r" org-transclusion-remove)
  ("R" org-transclusion-remove-all)
  ("o" org-transclusion-open-source)
  ("l" hydra-org-transclusion-live/body :exit t)
  ("q" nil))

(defhydra
  hydra-org-transclusion-live
  (:color pink :hint nil :exit t)
  "
^org-transclusion-live^
-----------------------
_l_: Start sync
_s_: Stop sync
_p_: Paste sync
_q_: Quit
"
  ("l" org-transclusion-live-sync-start)
  ("s" org-transclusion-live-sync-exit)
  ("p" org-transclusion-live-sync-paste)
  ("q" nil))

(global-set-key (kbd "C-c M-e") 'hydra-org-transclusion/body)

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

;; unpackaged/org-fix-blank-lines
(global-set-key (kbd "C-c f") 'unpackaged/org-fix-blank-lines)

;; tab deletion
(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

;; avy
(global-set-key (kbd "C-M-'") 'avy-goto-char-2)

;; eat
(global-set-key (kbd "C-c M-RET") 'eat)

;; fold source blocks
(global-set-key (kbd "C-c M-z") 'org-fold-hide-block-toggle)

;; Narrow and widen
(define-key org-mode-map (kbd "C-x n s") 'org-narrow-to-subtree)
(define-key org-mode-map (kbd "C-x n w") 'widen)

;; Kill present buffer
(global-set-key (kbd "C-x M-k") 'kill-this-buffer)

;; consult
(define-key org-mode-map (kbd "C-x M-j") 'consult-org-heading)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'list-timers 'disabled nil)
