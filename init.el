;;; Inhibit Startup Splash Screen
(setq inhibit-startup-screen t)
;;; Disable Menubar, Toolbar, Scrollbar, Tooltip
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;; Set Font
(set-face-attribute 'default nil :font "Inconsolata Nerd Font" :height 280)

(global-display-line-numbers-mode)

;;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6e13ff2c27cf87f095db987bf30beca8697814b90cd837ef4edca18bdd381901" default))
 '(gac-automatically-push-p t)
 '(package-selected-packages
   '(citar-org-roam citar vertico git-gutter magit git-auto-commit-mode company lsp-mode org-roam-ui spacious-padding org-super-agenda fzf dashboard org-transclusion org-superstar org-modern org-roam evil catppuccin-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Theme
(load-theme 'catppuccin :no-confirm)

;;; Evil
(require 'evil)
(evil-mode 1)

;;; org
(define-key minibuffer-local-completion-map (kbd "?") nil)
(add-hook 'org-mode-hook 'org-indent-mode)
(setq-default fill-column 100)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(setq org-deadline-warning-days 0)

;; org-agenda
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
(setq org-agenda-start-day "+0d"
      org-agenda-skip-timestamp-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agend-skip-timeline-if-deadlin-is-shown t
      )

(setq org-agenda-hide-tags-regexp ".*")
(setq org-agenda-prefix-format
      '((agenda . "  %?-2i %t")
	(todo . "  %?-2i %s %t")
	)
      )

(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "IN PROGRESS" "|" "DONE" "ARCHIVED")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#f9e2af" :weight bold))
	("NEXT" . (:foreground "#cba6f7" :weight bold))
	("IN PROGRESS" . (:foreground "#89b4fa" :weight bold))
	("DONE" . (:foreground "#a6e3a1" :weight bold))
	("ARCHIVED" . (:foreground "#9399b2"))
	))

;;; org-super-agenda
(setq org-agenda-custom-commands
      '(
	("d" "Daily"
         ((agenda "" (
		      (org-agenda-span 1)
		      (org-super-agenda-groups
			      '(
				(:name "Today"
				       :auto-outline-path t
				       :todo ("NEXT" "IN PROGRESS" "TODO")
				       :deadline today)
				)
			      )
		      )
	   ))
	  )
       ("w" "Week"
        ((agenda "" (
		     (org-agenda-span 'week)
		       (org-super-agenda-groups
			      '(
				(:name "This Week"
				       :auto-outline-path t
				       :todo ("NEXT" "IN PROGRESS" "TODO"))
				)
				)
			      )
		))
	)
       ("qw" "Query-base Week"
        ((org-ql-block '(or(and (not (done))
				(or
			     (deadline auto)
			     (ts-active :on today)
			     (scheduled :to today)
			     ))
			   (closed :on today)
			   :sort (todo priority date))
                        ((org-ql-block-header "This Week"))
			))
	 )
       )
)
(org-super-agenda-mode)
;; org-roam
(setq org-roam-directory (file-truename "~/org/"))
(setq org-roam-dailies-directory "~/org/worklogs/")

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "node_%<%Y%m%d%H%M%S>.org" "
#+title: ${title}
#+filetags:

* ${title}")
	 :create-file yes
	 :unnarrowed t))
      )
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "log_%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n\n* %<%Y-%m-%d>\n\n"))))
(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))
(org-roam-db-autosync-mode)
;;; org-superstar
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;;; Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner 3)

;;; vertico
(use-package vertico
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode))

;;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)))

;;; org-journal

(setq org-journal-dir "~/org/journal")
(setq org-journal-date-format "%Y-%m-%d")

;;; company
(company-mode)

;;; git-auto-commit-mode
(git-auto-commit-mode)
(setq gac-automatically-push-p t)
(add-hook 'after-save-hook 'git-auto-commit-mode)

;;; git-gutter
(global-git-gutter-mode +1)

;;; citar
(setq citar-bibliography '("~/org/bibliography.bib"))
(setq citar-notes-paths '("~/org/academic"))

;;

;;; Keybindings

;; General
(global-set-key (kbd "C-c r") 'eval-buffer)
(global-set-key (kbd "C-c y") 'clipboard-yank)
(global-set-key (kbd "C-c M-i") 'org-id-get-create)

;; org-roam
(global-set-key (kbd "C-c p") 'org-roam-node-find)
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
