;;; scimax-editmarks.el --- Editmarks for scimax

;;; Commentary:
;;

;;; Code
(require 'cl-lib)
(require 'color)
(require 'easymenu)
(require 's)

(defcustom sem-menu-items
  '(["accept" sem-accept-editmark t]
    ["reject" sem-reject-editmark t]
    ["clear" sem-clear-editmark t]
    ["delete" sem-delete-editmark t]
    ["next" sem-next-editmark t]
    ["previous" sem-previous-editmark t]
    ["list" sem-editmark-display t]
    ["Accept all" sem-accept-all-editmarks t]
    ["Reject all" sem-reject-all-editmarks t]
    ["Clear all" sem-clear-all-editmarks t]
    ["Delete all" sem-delete-all-editmarks t]
    ["Jump to visible" sem-jump-to-visible-editmark t]
    ["Jump to editmark" sem-jump-to-editmark t])
  "Items for the menu bar and popup menu."
  :group 'sem
  :type '(repeat (vector string function boolean)))


(defun sem-popup-command (event)
  "Pop up a menu on editmarks."
  (interactive "e")
  (popup-menu (append '("sem") sem-menu-items)))


(defvar sem-editmark-content-map
  (let ((map (copy-keymap org-mode-map)))
    (define-key map (kbd "<mouse-3>") 'sem-popup-command)
    (define-key map (kbd "s-<mouse-1>") 'sem-popup-command)
    (define-key map (kbd "s-o") (lambda () (interactive) (popup-menu (append '("sem") sem-menu-items))))
    (define-key map (kbd "C-a") (lambda ()
				  (interactive)
				  (goto-char (car (sem-editmark-bounds)))))
    (define-key map (kbd "C-e") (lambda ()
				  (interactive)
				  (goto-char (cdr (sem-editmark-bounds)))))
    (define-key map (kbd "C-n") 'sem-next-editmark)
    (define-key map (kbd "C-p") 'sem-previous-editmark)
    map)
  "Map for actions on editmark content.")

;; This is less useful than I thought.

;; (defvar sem-speed-map
;;   (let ((speedmap (make-sparse-keymap)))
;;     (define-key speedmap (kbd "n") 'sem-next-editmark)
;;     (define-key speedmap (kbd "p") 'sem-previous-editmark)
;;     (define-key speedmap (kbd "a") 'sem-accept-editmark)
;;     (define-key speedmap (kbd "A") 'sem-accept-and-next-editmark)
;;     (define-key speedmap (kbd "r") 'sem-reject-editmark)
;;     (define-key speedmap (kbd "R") 'sem-reject-and-next-editmark)
;;     (define-key speedmap (kbd "c") 'sem-clear-editmark)
;;     (define-key speedmap (kbd "d") 'sem-delete-editmark)
;;     (define-key speedmap (kbd "l") 'sem-editmark-display)
;;     (define-key speedmap (kbd "4") 'sem-editmark-spellcheck-typo)
;;     (define-key speedmap (kbd "$") 'sem-editmark-spellcheck)
;;     (define-key speedmap (kbd "q") 'sem-jump-to-visible-editmark)
;;     (define-key speedmap (kbd "j") 'sem-jump-to-editmark)
;;     (define-key speedmap (kbd "?") 'sem-speedmap-help)
;;     speedmap)
;;   "Keymap for speed keys on markers.")


;; (defun sem-speedmap-help ()
;;   "Describe the speed keys."
;;   (interactive)
;;   (describe-keymap sem-speed-map))

(defvar sem-editmarks
  `((delete :open-marker "{>-" :close-marker "-<}"
	    :marker-face (:foreground "#F38BA8" :weight ultra-light :strike-through t)
	    :face (:foreground "#F38BA8" :weight bold :strike-through t)
	    :keymap sem-editmark-content-map
	    :help-echo "Deletion. Right-click, s-click or s-o for menu."
	    :accept-func sem-delete-editmark
	    :reject-func sem-clear-editmark
	    :export sem-export-delete)

    (insert :open-marker "{>+" :close-marker "+<}"
	    :marker-face (:foreground "#94E2D5" :weight ultra-light)
	    :face (:foreground "#94E2D5" :weight bold)
	    :keymap sem-editmark-content-map
	    :help-echo "Insertion. Right-click, s-click or s-o for menu."
	    :accept-func sem-clear-editmark
	    :reject-func sem-delete-editmark
	    :export sem-export-insert)

    (comment :open-marker "{>~" :close-marker "~<}"
	     :marker-face (:foreground "#FAB387" :weight ultra-light)
	     :face (:foreground "#FAB387" :weight bold)
	     :keymap sem-editmark-content-map
	     :help-echo "Comment. Right-click, s-click or s-o for menu."
	     :accept-func sem-delete-editmark
	     :reject-func sem-delete-editmark
	     :include-author nil
	     :export sem-export-comment)

    (reply :open-marker "{r>" :close-marker "<r}"
	   :marker-face (:foreground "#F5C2E7"  :weight ultra-light)
	   :face (:foreground "#F5C2E7" :weight bold)
	   :keymap sem-editmark-content-map
	   :help-echo "Reply. Right-click, s-click or s-o for menu."
	   :accept-func sem-delete-editmark
	   :reject-func sem-delete-editmark
	   :include-author nil
	   :export sem-export-comment)

    (task :open-marker "{>*" :close-marker "*<}"
	  :marker-face (:foreground "#F9E2AF" :weight ultra-light)
	  :face (:foreground "#F9E2AF" :weight bold)
	  :keymap sem-editmark-content-map
	  :help-echo "Task. Right-click, s-click or s-o for menu."
	  :accept-func sem-delete-editmark
	  :export sem-export-task)

    (typo :open-marker "{>." :close-marker ".<}"
	  :marker-face (:foreground "#B95F89"  :weight ultra-light)
	  :face (:foreground "#B95F89" :weight bold)
	  :keymap ,(let ((map (make-sparse-keymap)))
		     (define-key map (kbd "4") 'sem-editmark-spellcheck-typo)
		     (define-key map (kbd "<return>") 'sem-editmark-spellcheck-typo)
		     map)
	  :help-echo "Typo. Type 4 to fix this.")

    (blue-highlight :open-marker "{bh>" :close-marker "<bh}"
		    :marker-face (:background "#74C7EC" :foreground "#1E1E2E" :weight ultra-light)
		    ;; :face (:underline (:color "#74C7EC" :style line :position -10))
            :face (:background "#74C7EC" :foreground "#1E1E2E")
		    :keymap sem-editmark-content-map
		    :help-echo "Blue highlight. Right-click, s-click or s-o for menu.")

    (green-highlight :open-marker "{hg>" :close-marker "<hg}"
		     :marker-face (:background "#A6E3A1" :foreground "#1E1E2E"  :weight ultra-light)
		     ;; :face (:underline (:color "#A6E3A1" :style line :position -10))
             :face (:background "#A6E3A1" :foreground "#1E1E2E")
		     :keymap sem-editmark-content-map
		     :help-echo "green highlight. Right-click, s-click or s-o for menu.")

    (purple-highlight :open-marker "{hp>" :close-marker "<hp}"
		    :marker-face (:background "#CBA6F7" :foreground "#1E1E2E"  :weight ultra-light)
		    ;; :face (:underline (:color "#CBA6F7" :style line :position -10))
		    :face (:background "#CBA6F7" :foreground "#1E1E2E")
  	      :keymap sem-editmark-content-map
		    :help-echo "purple highlight. Right-click, s-click or s-o for menu.")

    (yellow-highlight :open-marker "{hy>" :close-marker "<hy}"
		      :marker-face (:background "#F9E2AF" :foreground "#1E1E2E" :weight ultra-light)
		      ;; :face (:underline (:color "#F9E2AF" :style line :position -10))
              :face (:background "#F9E2AF" :foreground "#1E1E2E")
              :set-face-foreground ('org-link :foreground "#1E1E2E")
		      :keymap sem-editmark-content-map
		      :help-echo "yellow highlight. Right-click, s-click or s-o for menu.")
    
    (red-highlight :open-marker "{hr>" :close-marker "<hr}"
  	      :marker-face (:background "#F38BA8" :foreground "#1E1E2E" :weight ultra-light)
  	      ;; :face (:underline (:color "#F38BA8" :style line :position -10))
            :face (:background "#F38BA8" :foreground "#1E1E2E")
  	      :keymap sem-editmark-content-map
  	      :help-echo "red highlight. Right-click, s-click or s-o for menu.")
    
    (yellow-annotation :open-marker "{ay>" :close-marker "<ay}"
		 :marker-face (:foreground "#F9E2AF" :background "#1E1E2E" :weight ultra-light)
		 :face (:foreground "#F9E2AF" :background "#1E1E2E")
	     :keymap sem-editmark-content-map
	     :help-echo "Yellow annotation. Right-click, s-click or s-o for menu."
	     :accept-func sem-delete-editmark
	     :reject-func sem-delete-editmark
	     :include-author nil
	     :export sem-export-comment)
    
    (blue-annotation :open-marker "{ab>" :close-marker "<ab}"
		 :marker-face (:foreground "#74C7EC" :background "#1E1E2E" :weight ultra-light)
		 :face (:foreground "#74C7EC" :background "#1E1E2E")
	     :keymap sem-editmark-content-map
	     :help-echo "Blue annotation. Right-click, s-click or s-o for menu."
	     :accept-func sem-delete-editmark
	     :reject-func sem-delete-editmark
	     :include-author nil
	     :export sem-export-comment)
    
    (green-annotation :open-marker "{ag>" :close-marker "<ag}"
		 :marker-face (:foreground "#A6E3A1" :background "#1E1E2E" :weight ultra-light)
		 :face (:foreground "#A6E3A1" :background "#1E1E2E")
	     :keymap sem-editmark-content-map
	     :help-echo "Green annotation. Right-click, s-click or s-o for menu."
	     :accept-func sem-delete-editmark
	     :reject-func sem-delete-editmark
	     :include-author nil
	     :export sem-export-comment)
    
    (purple-annotation :open-marker "{ap>" :close-marker "<ap}"
		 :marker-face (:foreground "#CBA6F7" :background "#1E1E2E" :weight ultra-light)
		 :face (:foreground "#CBA6F7" :background "#1E1E2E")
	     :keymap sem-editmark-content-map
	     :help-echo "Purple annotation. Right-click, s-click or s-o for menu."
	     :accept-func sem-delete-editmark
	     :reject-func sem-delete-editmark
	     :include-author nil
	     :export sem-export-comment)

  (red-annotation :open-marker "{ar>" :close-marker "<ar}" :marker-face
	 (:foreground "#F38BA8" :background "#1E1E2E" :weight ultra-light) :face
	 (:foreground "#F38BA8" :background "#1E1E2E") :keymap
	 sem-editmark-content-map :help-echo "Red annotation. Right-click,
	 s-click or s-o for menu."  :accept-func sem-delete-editmark
	 :reject-func sem-delete-editmark :include-author nil :export
	 sem-export-comment)
  
     (audio :open-marker "{a>" :close-marker "<a}"
	    :marker-face (:foreground "violet" :weight ultra-light)
	    :face (:foreground "violet" :weight bold)
	    :mouse-face highlight
	    :keymap sem-editmark-audio-map
	    :help-echo sem-audio-tooltip
	    :accept-func sem-delete-editmark
	    :reject-func sem-clear-editmark)

     (video :open-marker "{v>"
	   :close-marker "<v}"
	   :help-echo "A video editmark"
	   :keymap sem-editmark-video-map
	   :marker-face (:foreground "MediumOrchid4" :weight ultra-light)
	   :face (:foreground "MediumOrchid4" :weight bold)
	   :mouse-face highlight)

    (file :open-marker "{>|"
	  :close-marker "|<}"
	  :marker-face (:foreground "cadet blue" :weight ultra-light)
	  :face (:foreground "cadet blue" :weight bold)
	  :help-echo "File location. s-↓ to open. s-→ to open in other window."
	  :include-author nil
	  :export sem-file-export
	  :keymap ,(let ((map (make-sparse-keymap)))
		     (define-key map (kbd "s-<down>") 'sem-follow-file)
		     (define-key map (kbd "s-<right>") (lambda ()
							 (interactive)
							 (sem-follow-file t)))
		     (define-key map (kbd "C-n") 'sem-next-editmark)
		     (define-key map (kbd "C-p") 'sem-previous-editmark)
		     map)))

  "The default editmarks")

(defun sem-editmark-plist-from-string (content)
  "Convert CONTENT into a plist.
We split the string into words, and reassemble it into a plist.
This is a very minimal parser to get away with not quoting things
inside editmarks.

I assume the content of the edit mark is all a plist like this

:keyword some values :next-keyword more values

This way it is not necessary to put quotes on the values, so the
plist above will end up as:
 (:keyword \"some values\" :next-keyword \"more values\")

Then, this gets read by elisp to make the plist. The downside of
this is it is not feasible to have a key without a value."
  (let* ((words (split-string content " " t "\s+"))
	 (sexp '())
	 (s '())
	 (tempstr)
	 this-word)
    (while words
      ;; pop words off one at a time
      (setq this-word (pop words))
      (if (not (s-starts-with? ":" this-word))
	  ;; Accumulate in a list of words
	  (push this-word s)
	;; word is not a :keyword
	(when s
	  (setq tempstr (string-join (reverse  s) " "))
	  (if (string= tempstr (format "%s" (string-to-number tempstr)))
	      (push (string-to-number tempstr) sexp)
	    (push tempstr sexp)))
	(setq s '())
	(push (intern this-word) sexp)))
    ;; make sure we get the last s...
    (when s
      (setq tempstr (string-join (reverse  s) " "))
      (if (string= tempstr (format "%s" (string-to-number tempstr)))
	  (push (string-to-number tempstr) sexp)
	(push tempstr sexp)))
    (flatten-list (reverse sexp))))


(defun sem-editmark-plist ()
  "Read a plist in an editmark.
Converts the content into a plist. If the contents are not a
plist, this may not do what you want."
  (let* ((bounds (sem-editmark-bounds))
	 (content-bounds (sem-content-bounds))
	 (content (s-trim (buffer-substring-no-properties
			   (car content-bounds) (cdr content-bounds)))))
    (sem-editmark-plist-from-string content)))

(defvar sem-editmark-audio-map
  (let ((map (copy-keymap org-mode-map)))
    (define-key map (kbd "M-o") 'sem-audio-listen)
    (define-key map (kbd "<return>") 'sem-audio-listen)
    (define-key map (kbd "<s-mouse-1>") 'sem-audio-listen)
    map)
  "Map for actions on editmark audio.")


(defun sem-audio-listen ()
  "Play the editmark."
  (interactive)
  (let ((plist (sem-editmark-plist)))
    (start-process "*listen*" nil
		   "sox"
		   (plist-get plist :file)
		   "-d" )))


(defun sem-audio-tooltip (window object position)
  "Tooltip for audio editmarks."

  "Audio editmark. use M-o or s-mouse-1 to listen."
  ;; a2t is an executable script in scimax python
  ;; (if (executable-find "a2t")
  ;;     (save-window-excursion
  ;; 	(goto-char position)
  ;; 	(shell-command-to-string
  ;; 	 (format "a2t %s"
  ;; 		 (plist-get (sem-editmark-plist) :file))))
  ;;   "Audio editmark. use M-o or s-mouse-1 to listen.")
  )


(defun sem-audio-insert ()
  "Insert an audio mark"
  (interactive)
  (let* ((cb (current-buffer))
	 (buffer "*record*")
	 (fname (format-time-string "%Y-%m-%d-%H-%M-%S.aiff"))
	 (process (start-process buffer buffer "sox" "-d" fname)))
    (pop-to-buffer buffer)
    (setq-local header-line-format "press q to quit.")
    (use-local-map (copy-keymap org-mode-map))
    (local-set-key "q" (lambda ()
			 (interactive)
			 ;; short pause to let the recording finish.
			 (sleep-for 1)
			 (let ((kill-buffer-query-functions nil))
			   (kill-process "*record*")
			   (kill-buffer buffer))
			 (pop-to-buffer cb)
			 (insert (format "{a> :file %s <a}" fname))))
    (recursive-edit)))

(defvar sem-editmark-video-map
  (let ((map (copy-keymap org-mode-map)))
    (define-key map (kbd "<return>") 'sem-video-watch)
    (define-key map (kbd "<mouse-1") 'sem-video-watch)
    map)
  "Map for actions on editmark video.")


(defun sem-video-insert ()
  "Insert a video editmark."
  (interactive)
  (let* ((cb (current-buffer))
	 (buffer "*record*")
	 (fname (format-time-string "%Y-%m-%d-%H-%M-%S.mov"))
	 (process (do-applescript
		   "tell application \"QuickTime Player\" to activate (new movie recording)")))
    (pop-to-buffer buffer)
    (insert "Setup Quicktime with the Camera you want and press record. Press q in this buffer when you want to stop and save the video.")
    (setq-local header-line-format "press q to quit.")
    (use-local-map (copy-keymap org-mode-map))
    (local-set-key "q" (lambda ()
			 (interactive)
			 (do-applescript
			  (format
			   "set theFilePath to POSIX path of \"%s\"

tell application \"QuickTime Player\"
    tell document \"Movie Recording\"
        pause
        save it in POSIX file theFilePath
        stop
        close
    end tell
end tell" (expand-file-name fname)))
			 (kill-buffer buffer)
			 (pop-to-buffer cb)
			 (insert (format "{v> :file %s %s <v}"
					 fname
					 (let ((note (read-string "Note: ")))
					   (if (string= "" note)
					       ""
					     (format " :note \"%s\"" note)))))))
    (recursive-edit)))


(defun sem-video-watch ()
  "Open a video to watch it.
Mac dependent"
  (interactive)
  (let ((plist (sem-editmark-plist))) 
    (shell-command (format "open %s" (plist-get plist :file)))
    (do-applescript "tell application \"System Events\" to keystroke space")))

(defvar sem-saved-file nil
  "plist for saved files.")

(defun sem-store-file-editmark ()
  "Store a file editmark."
  (interactive)
  (setq sem-saved-file (list
			:file (buffer-file-name)
			:project-root (projectile-project-root)
			:project (file-name-nondirectory
				  (directory-file-name (projectile-project-root)))
			:context (buffer-substring-no-properties
				  (max (- (point) 10) (point-min))
				  (min (+ (point) 10) (point-max)))
			:line (line-number-at-pos)
			:column (current-column))))


(defun sem-insert-file-editmark ()
  "Insert a previously stored file editmark."
  (interactive)
  (when (plist-get sem-saved-file :project)
    (plist-put sem-saved-file :file (file-relative-name
				     (plist-get sem-saved-file :file)
				     (plist-get sem-saved-file :project-root))))
  ;; I don't think we need this in the editmark
  (setq sem-saved-file (org-plist-delete sem-saved-file :project-root))
  ;; I am not sure what to do with this for now, so I am going to just delete it.
  (setq sem-saved-file (org-plist-delete sem-saved-file :context))
  (insert (format "{>|%s|<}"
		  (cl-loop for (k v) on sem-saved-file by (function cddr)
			   when v
			   concat (format "%s %s " k v)))))


(defun sem-follow-file (&optional other)
  "Function for following the editmark to its destination."
  (interactive "P")
  (org-mark-ring-push)
  (let* ((plist (sem-editmark-plist))
	 (fname (plist-get plist :file)))
    ;; I should add a :project option here
    (when-let (project (plist-get plist :project))
      ;; we need to build the path so we can open it.
      (let*
	  ((projects (remove nil (append (projectile-relevant-known-projects)
					 (list
					  (when (projectile-project-p)
					    (projectile-project-root))))))
	   ;; These are projects that match the project spec
	   (project-candidates (-filter (lambda (p)
					  (string-match (concat project "/\\'") p))
					projects))
	   ;; These are projects that match the spec, and that have the file we want.
	   (candidates (-filter (lambda (p)
				  (file-exists-p (expand-file-name fname p)))
				project-candidates)))
	(if (and (= 1 (length candidates))
		 (file-exists-p (expand-file-name fname (car candidates))))
	    (setq fname (expand-file-name fname (car candidates))))))

    (if other
	(find-file-other-window fname)
      (find-file fname))
    (forward-line (plist-get plist :line))
    (when-let  (col (plist-get plist :column))
      (move-to-column col))))


(defun sem-file-export (backend)
  "Export function for file editmarks."
  (let ((bounds (sem-editmark-bounds))
	(content-bounds (sem-content-bounds))
	(plist (sem-editmark-plist)))
    (cond
     ((eq 'latex backend)
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (format "@@latex:Line %s in \\href{file://%s}{%s}@@"
	       (plist-get plist :line)
	       (plist-get plist :file)
	       (file-name-nondirectory (plist-get plist :file))))))))

(defun sem-export-insert (backend)
  "Exporter for insert editmarks."
  (let ((bounds (sem-editmark-bounds))
	(content-bounds (sem-content-bounds)))
    (cond
     ((eq 'latex backend)
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (mapconcat
	(lambda (s)
	  (format "@@latex:\\noindent\\protect\\bgroup\\markoverwith{\\textcolor{blue}{\\rule[-0.5ex]{4pt}{1.4pt}}}\\ULon{%s}@@"
		  s))
	(string-split (buffer-substring-no-properties (car content-bounds)
						      (cdr content-bounds))
		      "\n")
	"@@latex:\\\\@@\n")))
     ((eq 'html backend)
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (mapconcat
	(lambda (s)
	  (format "@@html:<span style=\"color: blue\">%s</span>@@" s))
	(string-split (buffer-substring-no-properties (car content-bounds)
						      (cdr content-bounds))
		      "\n")
	"@@html:<br>@@"))))))


(defun sem-export-delete (backend)
  "Exporter for delete editmarks."
  (let ((bounds (sem-editmark-bounds))
	(content-bounds (sem-content-bounds)))
    (cond
     ((eq 'latex backend)
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (mapconcat
	(lambda (s)
	  (format "@@latex:\\noindent\\protect\\bgroup\\markoverwith{\\textcolor{red}{\\rule[-0.5ex]{4pt}{1.4pt}}}\\ULon{%s}@@"
		  s))
	(string-split (buffer-substring-no-properties (car content-bounds)
						      (cdr content-bounds))
		      "\n")
	"@@latex:\\\\@@\n")))
     ((eq 'html backend)
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (mapconcat
	(lambda (s)
	  (format "@@html:<span style=\"color: red\">%s</span>@@" s))
	(string-split (buffer-substring-no-properties (car content-bounds)
						      (cdr content-bounds))
		      "\n")
	"@@html:<br>@@"))))))


(defun sem-export-comment (backend)
  "Exporter for comment editmarks."
  (let ((bounds (sem-editmark-bounds))
	(content-bounds (sem-content-bounds)))
    (cond
     ((eq 'latex backend)
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (mapconcat
	(lambda (s)
	  (format "@@latex:%s@@" s))
	(string-split (format "\\todo{%s}"
			      (buffer-substring-no-properties
			       (car content-bounds)
			       (cdr content-bounds)))
		      "\n")
	"@@latex:\\\\@@\n")))
     ((eq 'html backend)
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (mapconcat
	(lambda (s)
	  (format "@@html:<span style=\"color: orange\">%s</span>@@" s))
	(string-split (buffer-substring-no-properties (car content-bounds)
						      (cdr content-bounds))
		      "\n")
	"@@html:<br>@@"))))))


(defun sem-export-task (backend)
  "Exporter for todo editmarks."
  (let ((bounds (sem-editmark-bounds))
	(content-bounds (sem-content-bounds)))
    (cond
     ((eq 'latex backend)
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (mapconcat
	(lambda (s)
	  (format "@@latex:%s@@" s))
	(string-split (format "\\todo[color=green!40]{TASK: %s}"
			      (buffer-substring-no-properties
			       (car content-bounds)
			       (cdr content-bounds)))
		      "\n")
	"@@latex:\\\\@@\n")))
     ((eq 'html backend)
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (mapconcat
	(lambda (s)
	  (format "@@html:<span style=\"color: purple\">%s</span>@@" s))
	(string-split (buffer-substring-no-properties (car content-bounds)
						      (cdr content-bounds))
		      "\n")
	"@@html:<br>@@"))))))

(defun sem-export-default (backend)
  "Default exporter for editmarks.
We wrap this something that approximates the appearance. If there
is background color in the face that takes precedence, otherwise
we go with the font color."
  (let* ((bounds (sem-editmark-bounds))
	 (content-bounds (sem-content-bounds))
	 (fg-color (or (plist-get (get-text-property (point) 'face) :foreground) "black"))
	 (bg-color (plist-get (get-text-property (point) 'face) :background))
	 (fg-rgb (color-name-to-rgb fg-color))
	 ;; the append (2) makes it use 24-bit color I think
	 (fg-hex (apply 'color-rgb-to-hex (append fg-rgb '(2))))
	 bg-rgb
	 ;; this is white
	 (bg-hex "#ffffff"))
    (when bg-color
      (setq bg-rgb (color-name-to-rgb bg-color)
	    bg-hex (apply 'color-rgb-to-hex (append bg-rgb '(2)))))

    (cond
     ((eq 'latex backend)
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (concat
	(format "@@latex:\\definecolor{%s}{rgb}{%s,%s,%s}%s{%s}{\\parbox{\\textwidth}{%s:@@"
		(or bg-color fg-color)
		(if bg-color (cl-first bg-rgb) (cl-first fg-rgb))
		(if bg-color (cl-second bg-rgb) (cl-second fg-rgb))
		(if bg-color (cl-third bg-rgb) (cl-third fg-rgb))
		(if bg-color
		    "\\colorbox"
		  "\\textcolor")
		(or bg-color fg-color)
		(get-text-property (point) 'sem-type))
	(mapconcat
	 (lambda (s)
	   (format "@@latex:%s@@" s))
	 (string-split (buffer-substring-no-properties
			(car content-bounds)
			(cdr content-bounds))
		       "\n")
	 "@@latex:\\\\@@\n")
	"@@latex: }}@@")))

     ((eq 'html backend)
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (mapconcat
	(lambda (s)
	  (format "@@html:<span style=\"color: %s; background-color: %s\">%s</span>@@"
		  fg-hex
		  bg-hex
		  s))
	(string-split (buffer-substring-no-properties (car content-bounds)
						      (cdr content-bounds))
		      "\n")
	"@@html:<br>@@"))))))

;; these should get removed when a region is unfontified.
(add-to-list 'font-lock-extra-managed-props 'sem-content)
(add-to-list 'font-lock-extra-managed-props 'sem-marker)
(add-to-list 'font-lock-extra-managed-props 'local-map)


(defun sem-font-lock-keywords ()
  "Return the font-lock keywords for all the editmarks in `sem-editmarks'."
  (mapcar
   (lambda (editmark)
     (let* ((type (car editmark))
	    (properties (cdr editmark))
	    (open-marker (plist-get properties :open-marker))
	    (close-marker (plist-get properties :close-marker))
	    (map (or (plist-get properties :keymap) sem-editmark-content-map))
	    (regexp (eval `(rx
			    (group-n 1 ,open-marker)
			    ;; this is an author
			    (optional blank (group-n 4 "@" (1+ (not space)) blank))
			    ;; the content
			    (group-n 2 (+? (or ascii nonascii)))
			    (group-n 3 ,close-marker)))))
       (if (symbolp map)
	   (setq map (symbol-value map)))
       (list regexp
	     `(0 ',(list 'face nil 'sem-editmark t 'sem-type type 'font-lock-multiline t))
	     ;; open-marker
	     `(1 ',(list 'face (plist-get properties :marker-face)
			 'sem-marker 'open
			 'rear-nonsticky t
			 ;; 'local-map sem-speed-map
			 'help-echo (plist-get properties :help-echo)))
	     ;; content
	     `(2 ',(list 'face (plist-get properties :face)
			 'mouse-face (plist-get properties :mouse-face)
			 'sem-content t
			 'local-map map
			 'help-echo (plist-get properties :help-echo)))
	     ;; close-marker
	     `(3 ',(list 'face (plist-get properties :marker-face)
			 'sem-marker 'close
			 'rear-nonsticky t
			 'help-echo (plist-get properties :help-echo)))
	     ;; author. This is sometimes missing and it is a problem when it is for fontification. I am not sure how to make this conditional.
	     ;; `(4 ',(list 'face nil
	     ;; 		 'sem-author t
	     ;; 		 'help-echo (plist-get properties :help-echo)))
	     )))
   sem-editmarks))

(defun sem-set-editmark-parameters (type &rest parameters)
  "Add or update an editmark.
TYPE is a symbol for the name of the editmark
PARAMETERS is a set of keyword value pairs
"
  (let ((data (assoc type sem-editmarks)))
    (if data
	;; update the editmark
	(setcdr data (org-combine-plists (cdr data) parameters))
      ;; New editmark
      (cl-pushnew `(,type ,@parameters) sem-editmarks)
      (font-lock-remove-keywords nil (sem-font-lock-keywords))
      (font-lock-add-keywords nil (sem-font-lock-keywords)))))

(when (featurep 'org-db)
  (defvar sem-editmark-contact-map
    (let ((map (copy-keymap org-mode-map)))
      (define-key map (kbd "<return>") 'sem-contact/body)
      (define-key map (kbd "s-e") 'sem-contact-email)
      (define-key map (kbd "s-t") 'sem-contact-email-to)
      (define-key map (kbd "s-f") 'sem-contact-email-from)
      (define-key map (kbd "s-r") 'sem-contact-related)
      map)
    "Map for actions on editmark contact.")


  (sem-set-editmark-parameters 'contact
			       :open-marker "{@>" :close-marker "<@}"
			       :marker-face '(:foreground "OrangeRed1" :weight ultra-light)
			       :face '(:foreground "OrangeRed1" :weight bold)
			       :keymap 'sem-editmark-contact-map
			       :help-echo "An editmark contact."
			       :export nil)


  (defun sem-contact-insert ()
    "Insert a contact edit mark"
    (interactive)
    (let* ((contacts (org-db-contacts-candidates))
	   (choice (cdr (assoc (ivy-read "Contact: "  contacts) contacts))))
      (insert (format "%s %s :email %s %s"
		      (plist-get (cdr (assoc 'contact sem-editmarks)) :open-marker)
		      (plist-get choice :title)
		      (plist-get choice :email)
		      (plist-get (cdr (assoc 'contact sem-editmarks)) :close-marker)))))


  (defun sem-contact-email ()
    "Open an email buffer to the contact."
    (interactive)
    (let* ((plist (sem-editmark-plist))
	   (email (plist-get (cdr plist) :email)))
      (compose-mail)
      (message-goto-to)
      (insert email)
      (message-goto-subject)))


  (defun sem-contact-email-from ()
    "Open mu4e showing emails from the candidate."
    (interactive)
    (let* ((plist (sem-editmark-plist))
	   (email (plist-get (cdr plist) :email)))
      (org-link-open-from-string
       (format "[[mu4e:query:from:%s]]"
	       email))))


  (defun sem-contact-email-to ()
    "Open mu4e showing emails to the candidate."
    (interactive)
    (let* ((plist (sem-editmark-plist))
	   (email (plist-get (cdr plist) :email)))
      (org-link-open-from-string
       (format "[[mu4e:query:tofrom:%s]]"
	       email))))


  (defun sem-contact-related ()
    "Completion to choose documents with this contact email in them.
This uses org-db-contacts, not editmark contacts right now."
    (interactive)
    (let* ((plist (sem-editmark-plist))
	   (email (plist-get (cdr plist) :email))
	   (link-candidates (cl-loop
			     for (rl fn bg) in
			     (with-org-db
			      (sqlite-select org-db "select raw_link,filename,begin from links
left join files on links.filename_id = files.rowid where links.type = \"contact\" and links.path = ? order by filename"
					     (list email))) 
			     collect
			     ;; (candidate :filename :begin)
			     (list (format "%s | %s" rl fn) :filename fn :begin bg)))

	   (results (with-org-db
		     (sqlite-select org-db "select headlines.title,properties.property,headline_properties.value,files.filename,files.last_updated,headlines.begin
from headlines
inner join headline_properties on headlines.rowid = headline_properties.headline_id
inner join properties on properties.rowid = headline_properties.property_id
inner join files on files.rowid = headlines.filename_id
where properties.property = \"ASSIGNEDTO\" and headline_properties.value like ?"
				    (list email))))

	   (assigned-candidates (cl-loop for (title property value fname last-updated begin) in results
					 collect
					 (list (format "%s | %s=%s | %s" title property value fname)
					       :filename fname :begin begin)))
	   (results (with-org-db
		     (sqlite-select org-db "select headlines.title, properties.property, headline_properties.value, files.filename, files.last_updated, headlines.begin
from headlines
inner join headline_properties on headlines.rowid = headline_properties.headline_id
inner join properties on properties.rowid = headline_properties.property_id
inner join files on files.rowid = headlines.filename_id
where properties.property = \"EMAIL\" and headline_properties.value like ?"
				    (list email))))
	   (email-candidates (cl-loop for (title property value fname last-updated begin) in results
				      collect
				      (list (format "%s | %s=%s | %s" title property value fname)
					    :filename fname :begin begin))))
      (ivy-read "Choose: " (append assigned-candidates email-candidates link-candidates)
		:action (lambda (x)
			  (let ((candidate (cdr x)))
			    (find-file (plist-get candidate :filename))
			    (goto-char (plist-get candidate :begin)))))))


  (defhydra sem-contact (:color blue :hint nil)
    "Editmark contact"
    ("o" sem-contact-open "open")
    ("e" sem-contact-email "Email contact")
    ("r" sem-contact-related "Related documents")
    ("t" sem-contact-email-to "Open emails to contact")
    ("f" sem-contact-email-from "Open emails from contact")))

(define-minor-mode sem-mode
  "A minor mode for editmarks."
  :lighter " sem"
  (if (not sem-mode)
      (progn
	(font-lock-remove-keywords
	 nil
	 (sem-font-lock-keywords))
	(remove-hook 'org-export-before-processing-hook 'sem-editmarks-to-org t))
    (font-lock-add-keywords
     nil
     (sem-font-lock-keywords))
    (add-hook 'org-export-before-processing-hook 'sem-editmarks-to-org nil t))
  (font-lock-ensure))

(defun sem-content-bounds ()
  "Return a cons cell of (start . end) of editmark content."
  (cond
   ;; on a marker
   ((eq (get-text-property (point) 'sem-marker) 'open)
    (let (b e)
      (setq b (or (next-single-property-change (point) 'sem-content))
	    e (or (next-single-property-change b 'sem-content)))
      (cons b e)))
   ((eq (get-text-property (point) 'sem-marker) 'close)
    (let (b e)
      (setq e (or (previous-single-property-change
		   (if (get-text-property (- (point) 1) 'sem-content)
		       (+ (point) 1)
		     (point))
		   'sem-content))
	    b (or (previous-single-property-change e 'sem-content)))
      (cons b e)))
   ;; in the content, but at the beginning
   ((and (get-text-property (point) 'sem-content)
	 (not (get-text-property (- (point) 1) 'sem-content)))
    (cons (point)
	  (or (next-single-property-change (point) 'sem-content) (point))))

   ((get-text-property (point) 'sem-content)
    (cons (or (previous-single-property-change (point) 'sem-content) (point))
	  (or (next-single-property-change (point) 'sem-content) (point))))
   (t
    (error "Not on an editmark?"))))


(defun sem-editmark-bounds ()
  "Return a cons cell of (start . end) of editmark.
Return nil if not on an editmark."
  (when (get-text-property (point) 'sem-editmark)
    (cond
     ;; At the very beginning of the buffer
     ((bobp)
      (cons (point) (next-single-property-change (point) 'sem-editmark)))
     ;; at beginning of an editmark
     ((null (get-text-property (- (point) 1) 'sem-editmark))
      (cons (point) (or (next-single-property-change (point) 'sem-editmark) (point))))

     ;; at end
     ((and (not (eobp))
	   (null (get-text-property (+ (point) 1) 'sem-editmark)))
      (cons (or (previous-single-property-change (point) 'sem-editmark) (point))
	    (point)))
     ;; in the middle
     (t
      (cons (or (previous-single-property-change (point) 'sem-editmark) (point))
	    (or (next-single-property-change (point) 'sem-editmark) (point)))))))

(defun sem-editmark-info ()
  "Give a message with some details."
  (interactive)
  (let ((bounds (sem-editmark-bounds))
	(cbounds (sem-content-bounds)))
    (message (s-format "type: ${type}
start: ${start}
end: ${end}
all: ${editmark}
c-start: ${content-start}
c-end  : ${content-end}
content: ${content}"
		       'aget
		       (list
			(cons "type" (get-text-property (point) 'sem-type))
			(cons "start" (car bounds))
			(cons "end" (cdr bounds))
			(cons "editmark" (buffer-substring-no-properties (car bounds) (cdr bounds)))
			(cons "content" (buffer-substring-no-properties (car cbounds) (cdr cbounds)))
			(cons "content-start" (car cbounds))
			(cons "content-end" (cdr cbounds)))))))

(defun sem-author ()
  "Return an author string"
  (format "@%s" (s-join "" (mapcar (lambda (s)
				     (downcase
				      (substring s 0 1)))
				   (split-string (or (user-full-name) "Not a name"))))))

(defun sem-insert (type)
  "Insert an editmark of TYPE.
TYPE should be a symbol corresponding to the car of an entry in `sem-editmarks'."
  (interactive (list (completing-read "Type: " (mapcar 'car sem-editmarks))))
  (if (not sem-mode) (sem-mode))
  (when (get-text-property (point) 'sem-type)
    (error "You are in an editmark. Nesting editmarks is not allowed."))

  (let ((entry (assoc (intern-soft type) sem-editmarks))
	(inhibit-modification-hooks t))
    ;; we do not track changes when inserting so we don't trigger nested
    ;; editmarks when editing editmarks.
    (cond
     ;; this is a special case
     ((eq type 'audio)
      (sem-audio-insert))
     ((eq type 'video)
      (sem-video-insert))
     ;; We have an active region we want to apply
     ((region-active-p)
      (let* ((bounds (list (region-beginning) (region-end)))
	     (start (apply 'min bounds))
	     (end (apply 'max bounds))
	     (lines))
	;; make sure we are not crossing any existing markups
	(when (or (get-text-property (region-beginning) 'sem-type)
		  (get-text-property (region-end) 'sem-type)
		  (not (= (region-end)
			  (next-single-property-change
			   (region-beginning)
			   'sem-type
			   nil
			   (region-end)))))
	  (error "You are in an editmark. Nesting editmarks is not allowed."))

	(cl--set-buffer-substring
	 start end
	 (concat (plist-get (cdr entry) :open-marker)
		 (when (plist-get (cdr entry) :include-author)
		   (concat " " (sem-author) " "))
		 (buffer-substring start end)
		 (plist-get (cdr entry) :close-marker)))))
     ;; We are on a word with no region selected
     ((thing-at-point 'word)
      (cond
       ;; beginning of a word
       ((looking-back "\\<" 1)
	(insert (plist-get (cdr entry) :open-marker)
		(when (plist-get (cdr entry) :include-author)
		  (concat " " (sem-author) " ")))
	(re-search-forward "\\>")
	(insert (plist-get (cdr entry) :close-marker)))
       ;; end of a word
       ((looking-back "\\>" 1)
	(insert (concat (plist-get (cdr entry) :open-marker)
			(when (plist-get (cdr entry) :include-author)
			  (concat " " (sem-author) " "))
			(plist-get (cdr entry) :close-marker)))
	(backward-char (length (plist-get (cdr entry) :close-marker))))
       ;; somewhere else in a word
       (t
	(re-search-backward "\\<")
	(insert (plist-get (cdr entry) :open-marker)
		(if (plist-get (cdr entry) :include-author)
		    (concat " " (sem-author) " ")
		  ""))
	(re-search-forward "\\>")
	(insert (plist-get (cdr entry) :close-marker)))))
     ;; not at a word or region, insert markers and put point between
     ;; them.
     (t
      (insert (concat (plist-get (cdr entry) :open-marker)
		      (when (plist-get (cdr entry) :include-author)
			(concat " " (sem-author) " "))
		      (plist-get (cdr entry) :close-marker)))
      ;; goto middle
      (backward-char (length (plist-get (cdr entry) :close-marker))))))
  ;; Should we add a local variable so the file opens in sem-mode?
  (hack-local-variables)
  ;; This is more complicated than I thought it should be. When I try to just
  ;; add a file-local variable, it often fails on new files because of some
  ;; weird issue in comment-region. I hacked this together, and it seems more
  ;; reliable.
  ;; (when (null file-local-variables-alist)
  ;;   (let ((mode major-mode))
  ;;     (save-excursion
  ;;   (save-restriction
  ;;     (widen)
  ;;     (goto-char (point-max))
  ;;     (unless (bolp)
  ;;       (insert "\n\n"))
  ;;     (when (eq mode 'org-mode)
  ;;       (insert "* Local Variables :noexport:\n\n"))
  ;;     (insert (with-temp-buffer
  ;;   	    (funcall mode) 
  ;;   	    (insert "Local Variables:\nEnd:\n")
  ;;   	    (comment-region (point-min) (point-max))
  ;;   	    (buffer-string)))))))
  ;; (when (not (member '(eval sem-mode) file-local-variables-alist))
  ;;   (save-excursion
  ;;     (add-file-local-variable 'eval '(sem-mode))))
  )

(defun sem-delete-editmark ()
  "Remove the editmark, markers and content."
  (interactive)
  (let ((bounds (sem-editmark-bounds)))
    (when bounds
      (cl--set-buffer-substring (car bounds) (cdr bounds) ""))))


(defun sem-delete-and-next-editmark ()
  "Remove the editmark, markers and content and go to the next one."
  (interactive)
  (let ((bounds (sem-editmark-bounds)))
    (when bounds
      (cl--set-buffer-substring (car bounds) (cdr bounds) ""))
    (sem-next-editmark)))


(defun sem-clear-editmark ()
  "Remove the markers but keep the content."
  (interactive)
  (let ((bounds (sem-editmark-bounds))
	(content-bounds (sem-content-bounds)))
    (when bounds
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (buffer-substring-no-properties (car content-bounds) (cdr content-bounds))))))


(defun sem-clear-and-next-editmark ()
  "Remove the markers but keep the content."
  (interactive)
  (let ((bounds (sem-editmark-bounds))
	(content-bounds (sem-content-bounds)))
    (when bounds
      (cl--set-buffer-substring
       (car bounds) (cdr bounds)
       (buffer-substring-no-properties
	(car content-bounds) (cdr content-bounds))))
    (sem-next-editmark)))

(defun sem-clear-all-editmarks ()
  "Clear all editmarks in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (sem-next-editmark)
      (sem-clear-editmark))))


(defun sem-delete-all-editmarks ()
  "Delete all editmarks in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (sem-next-editmark)
      (sem-delete-editmark))))

(defun sem-accept-editmark ()
  "Accept the current editmark."
  (interactive)
  (let* ((type (get-text-property (point) 'sem-type))
	 (func (plist-get (cdr (assoc type sem-editmarks)) :accept-func)))
    (if func
	(funcall func)
      (message "no :accept-func found for %s" type))))


(defun sem-accept-and-next-editmark ()
  "Accept the current editmark and move to the next one."
  (interactive)
  (sem-accept-editmark)
  (sem-next-editmark))


(defun sem-accept-all-editmarks ()
  "Accept all edtimarks."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (sem-next-editmark)
      (sem-accept-editmark))))

(defun sem-reject-editmark ()
  "Reject the current editmark."
  (interactive)
  (let* ((type (get-text-property (point) 'sem-type))
	 (func (plist-get (cdr (assoc type sem-editmarks)) :reject-func)))
    (if func
	(funcall func)
      (message "no :reject-func found for %s." type))))


(defun sem-reject-and-next-editmark ()
  "Reject the current editmark and move to the next one."
  (interactive)
  (sem-reject-editmark)
  (sem-next-editmark))


(defun sem-reject-all-editmarks ()
  "Reject all editmarks in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (sem-next-editmark)
      (sem-reject-editmark))))

(defun sem-next-editmark ()
  "Move point to the next editmark."
  (interactive)
  (when (get-text-property (point) 'sem-editmark)
    ;; we are on an editmark. first get out of it.
    (goto-char (next-single-property-change (point) 'sem-editmark)))
  (let ((next-em (next-single-property-change (point) 'sem-editmark)))
    (when next-em
      (goto-char next-em)
      next-em)))


(defun sem-previous-editmark ()
  "Move point to the previous editmark."
  (interactive)
  (when (get-text-property (point) 'sem-editmark)
    ;; we are on an editmark. first get out of it.
    (goto-char (previous-single-property-change (point) 'sem-editmark)))
  (let ((previous-em (previous-single-property-change (point) 'sem-editmark)))
    (when previous-em
      (goto-char previous-em)
      previous-em)))

(defun sem-jump-to-visible-editmark ()
  "Use avy to jump to a visible editmark."
  (interactive)
  (avy-with sem-editmark-jumper
    (avy-process
     ;; These are the points to process.
     (let ((editmarks '())
	   (start (window-start))
	   (end (window-end)))
       (save-excursion
	 (goto-char start)
	 (while (and (< (point) end) (sem-next-editmark))
	   (push (point) editmarks))
	 (reverse editmarks)))
     (avy--style-fn avy-style))))

(defun sem-jump-to-editmark ()
  "Jump to an editmark with completion."
  (interactive)
  ;; Get candidates
  (let ((candidates '())
	pos content content-bounds
	candidate)
    (save-excursion
      (goto-char (point-min))
      (while (sem-next-editmark)
	(setq pos (point)
	      content-bounds (sem-content-bounds)
	      content (buffer-substring (car content-bounds) (cdr content-bounds)))
	(push (cons content pos) candidates)))
    (setq candidate (completing-read "editmark: " (reverse candidates)))
    (goto-char (cdr (assoc candidate candidates)))))

(defun sem-get-editmarks ()
  "Return a list of the editmarks in the buffer.
Each element of the list is (type (start . end) editmark).
editmark is the full text including the markers."
  (save-excursion
    (goto-char (point-min))
    (let ((editmarks '())
	  bounds
	  cem)
      ;; when an editmark is at the beginning of the buffer
      (when (get-text-property (point) 'sem-type)
	(push (list (get-text-property (point) 'sem-type)
		    (current-buffer)
		    (setq bounds (sem-editmark-bounds))
		    (buffer-substring-no-properties (car bounds) (cdr bounds)))
	      editmarks))

      (while (setq cem (sem-next-editmark))
	(setq bounds (sem-editmark-bounds))
	(push (list (get-text-property (point) 'sem-type)
		    (current-buffer)
		    bounds
		    (buffer-substring-no-properties (car bounds) (cdr bounds)))
	      editmarks))
      editmarks)))

(defvar sem-editmark-source nil
  "Holds source buffer that the editmarks came from.")


(defun sem-editmark-display ()
  "Display the current editmarks in a tabulated list."
  (interactive)
  (save-buffer)
  (let ((buf (current-buffer)))
    (setq sem-editmark-source buf)
    (switch-to-buffer-other-window
     (get-buffer-create "*sem-editmarks*"))
    (sem-editmark-list-mode)
    (sem-editmark-refresh-list)))


(defun sem-editmark-refresh-list ()
  "Refresh the list of editmarks."
  (let ((editmarks)
	(entries))
    (with-current-buffer sem-editmark-source
      (setq editmarks (sem-get-editmarks))
      (setq entries (reverse (cl-loop for em in editmarks
				      collect
				      (list
				       nil ;id
				       (vector
					(cons (symbol-name (cl-first em))
					      (list
					       'face (plist-get (cdr (assoc (cl-first em) sem-editmarks)) :face)
					       'buffer (cl-second em)
					       'bounds (cl-third em)))
					(cons (cl-fourth em)
					      (list 'face (plist-get (cdr (assoc (cl-first em) sem-editmarks)) :face)))))))))
    (setq tabulated-list-entries entries
	  tabulated-list-format (vector '("Type" 20 t) '("Content" 40 t)))
    (tabulated-list-init-header)
    (tabulated-list-print)))


(defun sem-editmark-list-jump ()
  "In list mode, jump to the editmark back in the originating buffer."
  (interactive)
  (let ((buf (get-text-property (line-beginning-position) 'buffer))
	(pos (car (get-text-property (line-beginning-position) 'bounds))))
    (when pos
      (switch-to-buffer-other-window buf)
      (goto-char pos)
      (org-show-entry))))

(defvar sem-editmark-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "<return>") 'sem-editmark-list-jump)
    (define-key map (kbd "[mouse-1]") 'sem-editmark-list-jump)
    (define-key map (kbd "o") 'sem-editmark-list-jump)
    (define-key map (kbd "r") (lambda ()
				"Refresh the list."
				(interactive)
				(sem-editmark-refresh-list)))

    (define-key map (kbd "a") (lambda ()
				(interactive)
				"Accept the editmark"
				(save-window-excursion
				  (sem-editmark-list-jump)
				  (sem-accept-editmark))
				(sem-editmark-refresh-list)))

    (define-key map (kbd "c") (lambda ()
				"Clear the editmark"
				(interactive)
				(save-window-excursion
				  (sem-editmark-list-jump)
				  (sem-clear-editmark))
				(sem-editmark-refresh-list)))

    (define-key map (kbd "d") (lambda ()
				"Delete the editmark"
				(interactive)
				(save-window-excursion
				  (sem-editmark-list-jump)
				  (sem-delete-editmark))
				(sem-editmark-refresh-list)))

    (define-key map (kbd "u") (lambda ()
				"Undo in the source buffer"
				(interactive)
				(with-current-buffer sem-editmark-source
				  (undo))
				(sem-editmark-refresh-list)))

    (define-key map (kbd "4") (lambda ()
				"spellcheck the editmark"
				(interactive)
				(save-window-excursion
				  (sem-editmark-list-jump)
				  (sem-editmark-spellcheck-typo))
				(sem-editmark-refresh-list)))

    (define-key map (kbd "?") (lambda ()
				"Show keymap help."
				(interactive)
				(describe-keymap 'sem-editmark-list-mode-map)))
    map)
  "Local keymap for `sem-editmark-list-mode'.")

(define-derived-mode sem-editmark-list-mode
  tabulated-list-mode "sem-editmarks"
  "Mode for viewing editmarks as a tabular list.
\\{sem-editmark-list-mode-map}"
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook
	    #'sem-editmark-refresh-list))

(defun sem-editmark-spellcheck ()
  "Spell check the content of the editmark."
  (interactive)
  (let* ((bounds (sem-content-bounds))
	 (start (car bounds))
	 (end (cdr bounds)))
    (ispell-region start end)))


(defun sem-editmark-spellcheck-typo ()
  "Spell check the typo."
  (interactive)
  (let ((bounds (sem-content-bounds)))
    (goto-char (car bounds))
    (flyspell-correct-at-point)
    ;; This seems to be important to get the text properties fixed up before
    ;; clearing the editmark
    (save-excursion
      (font-lock-fontify-region (car bounds) (cdr bounds)))
    (sem-clear-editmark)))

(defhydra sem-insert (:color blue :hint nil :columns 3)
     "Editmark insert"
     ("s" (sem-insert 'audio) "audio")
     ("v" (sem-insert 'video) "video")
     ("m" (sem-insert 'comment) "comment")
     ("r" (sem-insert 'reply) "reply")
     ("i" (sem-insert 'insert) "insert")
     ("d" (sem-insert 'delete) "delete")
     ("f" (sem-insert-file-editmark) "file")
     ("t" (sem-insert 'typo) "typo")
     ("k" (sem-insert 'task) "task")
     ("c" (insert "✓") "checkmark")
     ("2" (sem-insert 'contact) "contact")
     ("hb" (sem-insert 'blue-highlight) "blue")
     ("hg" (sem-insert 'green-highlight) "green")
     ("hy" (sem-insert 'yellow-highlight) "yellow")
     ("hr" (sem-insert 'red-highlight) "red")
     ("hp" (sem-insert 'purple-highlight) "purple")
     ("ay" (sem-insert 'yellow-annotation) "yellow")
     ("ab" (sem-insert 'blue-annotation) "blue")
     ("ag" (sem-insert 'green-annotation) "green")
     ("ar" (sem-insert 'red-annotation) "red")
     ("ap" (sem-insert 'purple-annotation) "purple")
     ("n" sem-next-editmark "next")
     ("p" sem-previous-editmark "previous")
     ("g" sem-track-change-mode "toggle track changes")
     ("l" sem-editmark-display "List all")
     ("q" sem-jump-to-editmark "Jump to editmark")
     ("V" sem-jump-to-visible-editmark "Jump to visible")
     ("A" sem-action/body "action menu"))


   (defhydra sem-action (:color red :hint nil :columns 3)
     "Editmark action"
     ("a" sem-accept-editmark "accept")
     ("A" sem-accept-and-next-editmark "accept and next")
     ("C-a" sem-accept-all-editmarks "accept all")
     ("r" sem-reject-editmark "reject")
     ("R" sem-reject-and-next-editmark "reject and next")
     ("C-r" sem-reject-all-editmarks "reject all")
     ("c" sem-clear-editmark "clear")
     ("C" sem-clear-and-next-editmark "clear and next")
     ("C-c" sem-clear-all-editmarks "clear all")
     ("d" sem-delete-editmark "delete")
     ("D" sem-delete-and-next-editmark "delete and next")
     ("C-d" sem-delete-all-editmarks "Delete all")
     ("l" sem-editmark-display "List all")
     ("n" sem-next-editmark "next")
     ("p" sem-previous-editmark "previous")
     ("4" sem-editmark-spellcheck-typo "spellcheck typo")
     ("q" sem-jump-to-editmark "Jump to editmark")
     ("v" sem-jump-to-visible-editmark "Jump to visible")
     ("g" sem-track-change-mode "toggle track changes"))


   (defun sem-hydra ()
     "Open the editmark hydras depending on context of point.
   On an editmark open the action menu, otherwise the insert menu."
     (interactive)
     (if (get-text-property (point) 'sem-type)
         (sem-action/body)
       (sem-insert/body)))

(global-set-key (kbd "C-c M-v") 'sem-hydra)

(defun sem-editmarks-to-org (&optional backend)
  "Convert sem editmarks in an org-file to org syntax for BACKEND.
Inserts some headers at the top for todonotes and ulem, and the
LaTeX markup commands. This is not super robust, but works for
simple changes. There are issues with changes in citations,
tables, and other changes that cross org-element boundaries.

Note this function changes the buffer, so you may want to use it
in a copy of the buffer."
  (interactive)
  (goto-char (point-min))
  (sem-mode)
  (when
      (and
       (save-excursion (sem-next-editmark))
       (eq 'latex backend))
    (insert "
  #+latex_header: \\usepackage[normalem]{ulem}
  #+latex_header: \\usepackage{todonotes}
  #+latex_header: \\usepackage[usenames, dvipsnames]{color}
  \\listoftodos\n"))

  (while (sem-next-editmark)
    (let ((export-func (plist-get (cdr (assoc (get-text-property (point) 'sem-type) sem-editmarks)) :export)))
      (if export-func
	  (funcall export-func backend)
	(sem-export-default backend)))))

(defcustom sem-wdiff-cmd
  "wdiff -w \"{>-\" -x \"-<}\" -y \"{>+\" -z \"+<}\" "
  "Command to run wdiff with.")

(defun sem-wdiff-buffer-with-file ()
  "Do a wdiff of the buffer with the last saved version.
For line-based diff use `diff-buffer-with-file'."
  (interactive)
  (let ((contents (buffer-string))
	(tempf (make-temp-file "wdiff-"))
	(fname (buffer-file-name)))
    (with-temp-file tempf
      (insert contents))

    (switch-to-buffer "*wdiff-buffer*")
    (insert
     (shell-command-to-string
      (format "%s %s %s"
	      sem-wdiff-cmd
	      fname
	      tempf)))
    (delete-file tempf)
    (goto-char (point-min))
    (sem-mode)))

(defun sem-wdiff-git (commit)
  "Perform a wdiff between HEAD and a git commit.
An ivy selection is used to choose the commit.

If you choose one commit, the wdiff is between that commit and
the current version. Returns the buffer."
  (interactive
   (list (let ((candidates (mapcar (lambda (s)
				     (let ((commit
					    (nth
					     0
					     (split-string s))))
				       (cons s
					     commit)))
				   (split-string
				    (shell-command-to-string
				     "git log --pretty=format:\"%h %ad | %s%d [%an]\" --date=relative") "\n"))))
	   (cdr (assoc (ivy-read
			"commit: "
			candidates)
		       candidates)))))
  (let* ((buf (get-buffer-create
	       "*org-wdiff-git*"))
	 (curbuf (current-buffer))
	 (mmode major-mode)
	 (git-root (vc-git-root
		    (buffer-file-name)))
	 (fname
	  (file-relative-name
	   (buffer-file-name)
	   (vc-git-root (buffer-file-name))))
	 (cmd (format "%s <(git show %s:%s) %s"
		      sem-wdiff-cmd
		      commit fname
		      fname)))

    (switch-to-buffer-other-window buf)
    (let ((inhibit-read-only t))
      (erase-buffer))

    ;; Try to keep same major mode
    (funcall mmode)

    ;; get the wdiff. we do this in git-root so the paths are all correct.
    (let ((default-directory git-root))
      (insert (shell-command-to-string cmd)))
    (goto-char (point-min))
    ;; save fname as buffer local variable to save back later.
    (with-current-buffer buf
      (make-local-variable '*sem-wdiff-git-source*)
      (setq *sem-wdiff-git-source* curbuf))
    buf))

(defun sem-wdiff-save ()
  "Save changes.
If there is an *org-wdiff-git* buffer, then we copy that content
to the buffer visiting `*cm-wdiff-git-source*'. You may use
*org-wdiff-git* to accept/reject changes, and then put it back to
where it came from. Otherwise we just save the buffer."
  (interactive)
  (if (get-buffer "*org-wdiff-git*")
      (progn
	(switch-to-buffer *sem-wdiff-git-source*)
	(erase-buffer)
	(insert-buffer-substring "*org-wdiff-git*")
	(kill-buffer "*org-wdiff-git*"))
    (save-buffer)))

(define-minor-mode sem-track-change-mode
  "A minor mode for tracking changes."
  :lighter " tc"
  (if sem-track-change-mode
      (progn
	(add-to-list 'before-change-functions 'sem-before-change t)
	(add-to-list 'after-change-functions 'sem-after-change)
	(message "Track changes mode activated."))
    (setq before-change-functions (delq 'sem-before-change before-change-functions))
    (setq after-change-functions (delq 'sem-after-change after-change-functions))
    (message "Track changes mode deactivated.")))

(defvar sem-current-deletion nil
  "The deleted text in track changes mode.
The value is a list consisting of the text and a flag
indicating whether the deletion was done with the backspace
key.")


(defun sem-before-change (beg end)
  "Function to execute before a buffer change.
BEG and END are the beginning and the end of the region to be
changed."
  (unless (or undo-in-progress
              (and (= beg (point-min)) (= end (point-max)))) ; this happens on buffer switches
    (if (= beg end)			; this means we are inserting.
	(let ((inhibit-modification-hooks t))
	  ;; An insertion. There are a bunch of corner cases to handle
	  (cond
	   ;; We are on an open marker. Move in. (ref:insert-open)
	   ((eq (get-text-property (point) 'sem-marker) 'open)
	    ;; Move to beginning of content
	    (goto-char (car (sem-content-bounds))))

	   ;; On a close marker, move in and insert (ref:insert-close)
	   ((eq (get-text-property (point) 'sem-marker) 'close)
	    ;; Move to end of content
	    (goto-char (cdr (sem-content-bounds))))

	   ;; One character after an insert, merge back (ref:insert-merge-back)
	   ((and (not (get-text-property (point) 'sem-type))
		 (eq 'insert (get-text-property (- (point) 1) 'sem-type)))
	    (backward-char (+ 1 (length (plist-get
					 (cdr (assoc 'insert sem-editmarks))
					 :close-marker)))))

	   ;; one character in front of an insert, merge in (ref:insert-merge-forward)
	   ((and (not (get-text-property (point) 'sem-type))
		 (eq 'insert (get-text-property (+ (point) 1) 'sem-type)))
	    (forward-char (+ 1 (length (plist-get
					(cdr (assoc 'insert sem-editmarks))
					:open-marker)))))

	   ;; in an editmark, no need to do anything, just insert like normal. (ref:insert-content)
	   ((get-text-property (point) 'sem-content)
	    nil)

	   ;; The simplest is we are just inserting away from other editmarks. In this case, we just insert
	   ;; the insertion markers and put point in the middle. (ref:insert-simple)
	   ((not (get-text-property (point) 'sem-type))
	    (insert (plist-get (cdr (assoc 'insert sem-editmarks)) :open-marker))
	    (insert (plist-get (cdr (assoc 'insert sem-editmarks)) :close-marker))
	    (backward-char (length (plist-get (cdr (assoc 'insert sem-editmarks)) :close-marker))))

	   ;; what is the fall through case? Warning? do nothing?
	   ;; These are insertions on other editmarks.
	   (t
	    (message "Inserting in an unhandled state. Are you sure this makes sense?"))))
      ;; Not an insertion, we have a deletion to handle. This is usually done in `sem-after-change'.
      (cond
       (t
	(setq sem-current-deletion (list (buffer-substring beg end) (= (point) end))))))))

(defun sem-after-change (beg end length)
  "Function to execute after a buffer change.
This function marks deletions.  See `sem-before-change' for details.
BEG and END mark the region to be changed, LENGTH is the length
of the affected text."
  (unless (or undo-in-progress
              (not sem-current-deletion))
    (let ((inhibit-modification-hooks t))
      (cond
       ;; deletion by C-d, kill, etc. (ref:delete-1)
       ((null (cl-second sem-current-deletion))
	(message "C-d, kill, delete")
	(cond
	 ;; just mark for deletion
	 ;; (ref:delete-1-add-mark)
	 ((and (not (get-text-property (point) 'sem-type))
	       (not (get-text-property (- (point) 1) 'sem-type)))
	  (insert (plist-get (cdr (assoc 'delete sem-editmarks)) :open-marker))
	  (insert (plist-get (cdr (assoc 'delete sem-editmarks)) :close-marker)))
	 ;; On content, just let deletions happen. Note we don't check
	 ;; if open/close markers are present (ref:delete-1-content)
	 ((get-text-property (point) 'sem-content)
	  nil)

	 ;; We are on an open marker. ignore. (ref:delete-1-open)
	 ((eq (get-text-property (point) 'sem-marker) 'open)
	  (insert (cl-first sem-current-deletion)))

	 ;; On a close marker, ignore (ref:delete-1-close)
	 ((eq (get-text-property (point) 'sem-marker) 'close)
	  (insert (cl-first sem-current-deletion)))

	 (t
	  (message "Unhandled C-d/kill delete. did this make sense?"))))

       ;; backspace cases. (ref:delete-2)
       (t
	(cond

	 ;; empty editmark, we delete it. (ref:delete-2-empty)
	 ((and (looking-at (regexp-opt (mapcar (lambda (em)
						 (plist-get (cdr em) :close-marker))
					       sem-editmarks)))
	       (looking-back (regexp-opt (mapcar (lambda (em)
						   (plist-get (cdr em) :open-marker))
						 sem-editmarks))
			     (apply 'max (mapcar
					  'length
					  (mapcar (lambda (em)
						    (plist-get (cdr em) :open-marker))
						  sem-editmarks)))))
	  (re-search-forward (regexp-opt (mapcar (lambda (em)
						   (plist-get (cdr em) :close-marker))
						 sem-editmarks)))
	  (replace-match "")
	  (re-search-backward (regexp-opt (mapcar (lambda (em)
						    (plist-get (cdr em) :open-marker))
						  sem-editmarks)))
	  (replace-match ""))

	 ;; Simplest case, in text away from editmarks (ref:delete-2-new)
	 ((and (not (get-text-property (point) 'sem-type))
	       (not (get-text-property (+ (point) 1) 'sem-type)))
	  (insert (plist-get (cdr (assoc 'delete sem-editmarks)) :open-marker))
	  (insert (plist-get (cdr (assoc 'delete sem-editmarks)) :close-marker))
	  (backward-char (length (plist-get (cdr (assoc 'delete sem-editmarks))
					    :close-marker)))
	  (insert (cl-first sem-current-deletion))
	  ;; now go back to front of the mark.
	  (backward-char (length (cl-first sem-current-deletion)))
	  (backward-char (length (plist-get (cdr (assoc 'delete sem-editmarks))
					    :open-marker))))

	 ;; (ref:delete-2-front)
	 ;; at the front of delete but not at the end of any other editmark
	 ((and (looking-at (plist-get (cdr (assoc 'delete sem-editmarks)) :open-marker))
	       (not (string= "}" (cl-first sem-current-deletion))))
	  (forward-char (length (plist-get
				 (cdr (assoc 'delete sem-editmarks)) :open-marker)))
	  (insert (cl-first sem-current-deletion))
	  (backward-char (length (cl-first sem-current-deletion)))
	  (backward-char (length (plist-get
				  (cdr (assoc 'delete sem-editmarks)) :close-marker))))

	 ;; between two delete edit marks, merge them and jump to the front
	 ;; (ref:delete-2-merge)
	 ((and (looking-at (plist-get (cdr (assoc 'delete sem-editmarks)) :open-marker))
	       (string= "}" (cl-first sem-current-deletion)))
	  (insert "}")
	  (if (not (looking-back (plist-get (cdr (assoc 'delete sem-editmarks)) :close-marker)
				 (length (plist-get
					  (cdr (assoc 'delete sem-editmarks)) :close-marker))))
	      (progn
		;; this means it is some other kind of mark.
		(re-search-backward (regexp-opt (mapcar (lambda (em)
							  (plist-get (cdr em) :open-marker))
							sem-editmarks))))


	    (message "case 2 - merging delete")
	    (delete-char (length (plist-get
				  (cdr (assoc 'delete sem-editmarks)) :close-marker)))
	    (delete-char (* -1 (length (plist-get
					(cdr (assoc 'delete sem-editmarks)) :open-marker))))
	    (re-search-backward (plist-get (cdr (assoc 'delete sem-editmarks)) :open-marker))))


	 ;; At end of a mark, and beginning of delete. we should jump
	 ;; to end of previous content? (ref:delete-2-end+mark)
	 ((and (looking-at (plist-get (cdr (assoc 'delete sem-editmarks)) :open-marker))
	       (string= "}" (cl-first sem-current-deletion)))
	  (message "case 2 - front of delete and at end of a mark.")
	  (insert "}")
	  (if (not (looking-back (regexp-opt (mapcar (lambda (em)
						       (plist-get (cdr em) :close-marker))
						     sem-editmarks))
				 (apply 'max (mapcar 'length
						     (mapcar
						      (lambda (em)
							(plist-get (cdr em) :close-marker))
						      sem-editmarks)))))
	      (delete-char -1)
	    ;; put char back
	    (forward-char (length
			   (plist-get (cdr (assoc 'delete sem-editmarks)) :open-marker)))
	    (insert (cl-first sem-current-deletion))
	    (backward-char (length (cl-first sem-current-deletion)))
	    (re-search-backward (regexp-opt (mapcar (lambda (em)
						      (plist-get (cdr em) :open-marker))
						    sem-editmarks)))))

	 ;; at the back-end of a delete but not looking at the front
	 ;; of a delete, probably we should jump to the front to
	 ;; extend. (ref:delete-2-end-extend)
	 ((or (eq 'close (get-text-property (- (point) 1) 'sem-marker))
	      (eq 'open (get-text-property (- (point) 1) 'sem-marker)))
	  (message "case 2 - deleting mark")
	  (backward-char)
	  (goto-char (car (sem-editmark-bounds))))

	 ;; At front of some other mark
	 ((looking-at (regexp-opt (mapcar (lambda (em)
					    (plist-get (cdr em) :open-marker))
					  sem-editmarks)))
	  (insert (plist-get (cdr (assoc 'delete sem-editmarks)) :open-marker))
	  (insert (cl-first sem-current-deletion))
	  (insert (plist-get (cdr (assoc 'delete sem-editmarks)) :close-marker))
	  (re-search-backward (regexp-opt (mapcar (lambda (em)
						    (plist-get (cdr em) :open-marker))
						  sem-editmarks))))

	 (t
	  (message "after: why aren't you caught?>")))))))
  (setq sem-current-deletion nil)
  (save-excursion (font-lock-fontify-region (line-beginning-position) (line-end-position))))

(defmacro sem-without-following-changes (&rest body)
  "Execute BODY without following changes."
  (declare (indent defun))
  `(let ((inhibit-modification-hooks t))
     ,@body))

(provide 'scimax-editmarks)

;;; scimax-editmarks.el ends here
