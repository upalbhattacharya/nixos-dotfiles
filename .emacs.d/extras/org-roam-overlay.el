;;; org-roam-overlay.el --- Link overlay for [id:] links to Org-roam nodes -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2021 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 2.1.0
;; Package-Requires: ((emacs "26.1") (org "9.4") (org-roam "2.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This extension provides allows to render [[id:]] links that don't have an
;; asscoiated descriptor with an overlay that displays the node's current title.
;;
;;; Code:
(require 'org-roam)

(defface org-roam-overlay
  '((((class color) (background light))
     :background "#313244" :foreground "#B4BEFE" :underline t)
    (((class color) (background dark))
     :background "#313244" :foreground "#B4BEFE" :underline t))
  "Face for the Org-roam overlay."
  :group 'org-roam-faces)

(defun org-roam-overlay--make (l r &rest props)
  "Make an overlay from L to R with PROPS."
  (let ((o (make-overlay l (or r l))))
    (overlay-put o 'category 'org-roam)
    (while props (overlay-put o (pop props) (pop props)))
    o))

(defun org-roam-node-description-breadcrumb (node)
  (let ((level (org-roam-node-level node)))
    (concat
     "(" (org-roam-node-category node) ") " (org-roam-node-file-title node) " > "
     (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ")) )
     (if (eq level 1) (org-roam-node-title node) (concat " > " (org-roam-node-title node)))
     ))
  )

(defun org-roam-breadcrumb-overlay-make-link-overlay (link)
  "Create overlay for LINK."
  (save-excursion
    (save-match-data
      (let* ((type (org-element-property :type link))
             (id (org-element-property :path link))
             (beg (org-element-property :begin link))
             (end (org-element-property :end link))
             node)
        (when (and (string-equal type "id")
                   (setq node (org-roam-node-from-id id)))
          (org-roam-overlay--make
           beg end
           'before-string (format "%s "
                                  (propertize (org-roam-node-description-breadcrumb node)
                                              'face 'org-roam-overlay))))))))

(defun org-roam-breadcrumb-overlay-enable ()
  "Enable Org-roam overlays."
  (org-roam-db-map-links
   (list #'org-roam-breadcrumb-overlay-make-link-overlay)))

(defun org-roam-overlay-disable ()
  "Disable Org-roam overlays."
  (remove-overlays nil nil 'category 'org-roam))

(defun org-roam-breadcrumb-overlay-redisplay ()
  "Redisplay Org-roam overlays."
  (org-roam-overlay-disable)
  (org-roam-breadcrumb-overlay-enable))

(define-minor-mode org-roam-breadcrumb-overlay-mode
  "Overlays for Org-roam ID links.
Org-roam overlay mode is a minor mode.  When enabled,
overlay displaying the node's title is displayed."
  :lighter " org-roam-breadcrumb-overlay"
  (if org-roam-breadcrumb-overlay-mode
      (progn
        (setq org-roam-title-overlay-mode nil)
        (remove-hook 'post-command-hook #'org-roam-title-overlay-redisplay t)
        (org-roam-breadcrumb-overlay-enable)
        (add-hook 'post-command-hook #'org-roam-breadcrumb-overlay-redisplay nil t))
    (org-roam-overlay-disable)
    (remove-hook 'post-command-hook #'org-roam-breadcrumb-overlay-redisplay t)))

(defun org-roam-title-overlay-make-link-overlay (link)
  "Create overlay for LINK."
  (save-excursion
    (save-match-data
      (let* ((type (org-element-property :type link))
             (id (org-element-property :path link))
             (beg (org-element-property :begin link))
             (end (org-element-property :end link))
             node)
        (when (and (string-equal type "id")
                   (setq node (org-roam-node-from-id id)))
          (org-roam-overlay--make
           beg end
           'before-string (format "%s "
                                  (propertize (org-roam-node-title node)
                                              'face 'org-roam-overlay))))))))

(defun org-roam-title-overlay-enable ()
  "Enable Org-roam overlays."
  (org-roam-db-map-links
   (list #'org-roam-title-overlay-make-link-overlay)))


(defun org-roam-title-overlay-redisplay ()
  "Redisplay Org-roam overlays."
  (org-roam-overlay-disable)
  (org-roam-title-overlay-enable))

(define-minor-mode org-roam-title-overlay-mode
  "Overlays for Org-roam ID links.
Org-roam overlay mode is a minor mode.  When enabled,
overlay displaying the node's title is displayed."
  :lighter " org-roam-title-overlay"
  (if org-roam-title-overlay-mode
      (progn
        (setq org-roam-breadcrumb-overlay-mode nil)
        (remove-hook 'post-command-hook #'org-roam-breadcrumb-overlay-redisplay t)
        (org-roam-title-overlay-enable)
        (add-hook 'post-command-hook #'org-roam-title-overlay-redisplay nil t))
    (org-roam-overlay-disable)
    (remove-hook 'post-command-hook #'org-roam-title-overlay-redisplay t)))

(provide 'org-roam-overlay)
;;; org-roam-overlay.el ends here
