; -*- mode: Emacs-lisp; -*-

;;{{{ TODO

;; TODO: Add folding sections for everything.
;; TODO: Make explicit your key binding hierarchy.
;; TODO: Make this work: (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;;}}}
;;{{{ General Emacs tweaks

;; https://www.emacswiki.org/emacs/MELPA
(require 'package)

(add-to-list
 'package-archives
 (quote ("melpa-stable" . "https://stable.melpa.org/packages/"))
 t)

(package-initialize)

;; https://masteringemacs.org/article/disabling-prompts-emacs
(fset 'yes-or-no-p 'y-or-n-p)

;; https://masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

;;{{{ Variables

;; TODO: Document anything non-trivial.
(setq enable-recursive-minibuffers t)
(setq sentence-end-double-space nil)
(setq async-shell-command-buffer 'new-buffer)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
	kill-buffer-query-functions))
(setq column-number-mode t)
(setq find-function-C-source-directory "~/src/emacs29/emacs-29.1/src/")
(setq confirm-nonexistent-file-or-buffer nil)

;;}}}
;;{{{ Previously disabled commands

(put 'downcase-region 'disabled nil)

;;}}}
;;}}}
;;{{{ Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (wheatgrass)))
 '(org-export-backends (quote (ascii html icalendar latex md)))
 '(package-selected-packages
   '(magit auctex haskell-mode folding buffer-move gnu-elpa-keyring-update disable-mouse go-mode))
 '(safe-local-variable-values
   (quote ((indent-tabs-mode nil)
	   (backup-inhibited . t)
	   (org-pretty-entities . t)))))

;;}}}
;;{{{ Global keys

(global-set-key (kbd "M-DEL") nil)
(global-set-key (kbd "M-`") #'jump-to-mark)
(global-set-key (kbd "<M-backspace>") #'kill-region)
(global-set-key (kbd "C-`") #'push-mark-no-activate)
(global-set-key (kbd "C-c g e") #'open-emacs)
(global-set-key (kbd "C-c g k") #'kill-restart-emacs)
(global-set-key (kbd "C-c g l") #'LaTeX-copy-ling-template-and-visit)
(global-set-key (kbd "C-c g m") #'LaTeX-copy-mwe-and-visit)
(global-set-key (kbd "C-c g s") #'eshell)
(global-set-key (kbd "C-c g v") #'vocabularium-fasciculos-agere)
(global-set-key (kbd "C-c g w") #'erase-kill-ring)
(global-set-key (kbd "C-c s o") #'owd)
(global-set-key (kbd "C-w") #'backward-kill-word)
;; https://emacs.stackexchange.com/a/3471
(global-set-key (kbd "C-x o") nil)
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-'") #'open-text-right)
(global-set-key (kbd "M-z") #'zap-up-to-char)

;;}}}
;;{{{ Trivial convenience functions

(defun kill-restart-emacs (arg)
  "With ARG set, kill and restart emacs, otherwise just kill."
  (interactive "P")

  (if arg
      (kill-emacs nil t)
    (kill-emacs)))

(defun open-emacs ()
  "Open ~/.emacs."
  (interactive)
  (find-file "~/.emacs"))

;;}}}
;;{{{ Less trivial convenience functions

(defun open-text-right ()
  "Inspired by open-line. Insert a space to the right of point while keeping
point in the same place."
  (interactive)

  (save-excursion
    (insert " ")))

(defun external-paste ()
  "Divide the region in two and call `paste (1)` on the two halves.
If the use of another delimiter or more well-defined behavior
upon unbalanced input is desired, use `paste (1)` directly."
  (interactive)

  (kill-region (mark) (point))

  (with-temp-buffer
    (yank)
    (delete-trailing-whitespace)
    (goto-char (point-min))

    (let ((mid (/ (count-lines (point-min) (point-max)) 2)))
      (forward-line mid)
      (shell-command-on-region (point-min) (point) "cat > /tmp/a" nil t)
      (shell-command-on-region (point-min) (point-max) "cat > /tmp/b" nil t)
      (shell-command "paste /tmp/a /tmp/b" t)
      (kill-new (buffer-string))))

  (yank))

(defun owd ()
  "From a shell: Get pwd from the other window and cd there."
  (interactive)

  (other-window 1)
  (with-temp-buffer
    (pwd t)
    (kill-new (buffer-string))
  (other-window 1)

  (insert (concat "cd " (current-kill 0 t)))
  (if (eq major-mode 'eshell-mode)
      (eshell-send-input)
    (comint-send-input))))
;;}}}
;;{{{ Logic

(defun insert-logical-operator (arg)
  "Insert the ARGth logical operator from a predefined list.
Calling the function with \"0\" prints the list."
  (interactive "*N")
  (let ((operator-list (list "∧" "∨" "¬" "⊃" "≡" "∃" "∀" "∅" "〈〉" "′")))
    (if (= 0 arg)
	(message "%s" operator-list)
      (insert (nth (- arg 1) operator-list)))))
(global-set-key (kbd "<f5>") 'insert-logical-operator)

;; http://xahlee.info/comp/unicode_circled_numbers.html
(defun insert-predicate-number (arg)
  "Insert the ARGth predicate number."
  (interactive "*N")
  (let ((predicate-list
	 (list "⓪" "①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨")))
    (insert (nth arg predicate-list))))
(global-set-key (kbd "<f6>") 'insert-predicate-number)

;;}}}

(add-hook 'write-file-functions 'delete-trailing-whitespace)

;; https://github.com/purcell/disable-mouse
(global-disable-mouse-mode)
(global-unset-key (kbd "<C-down-mouse-1>"))

(setq backup-by-copying t)

;; https://melpa.org/#/buffer-move
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(global-set-key (kbd "C-x g") 'magit-status)

;;{{{ Previously disabled commands

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)

;;}}}
;;{{{ Org

(require 'org)

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)

(setq org-agenda-files '("~/org/"))

(setq org-capture-templates '(("t" "Create a TODO item."
			       entry
			       (file "~/org/todo.org")
			       (file "~/org/todo-template"))))

(add-to-list 'org-file-apps '("\\.pdf\\'" . "okular %s"))
(add-to-list 'org-file-apps '("\\.epub\\'" . "calibre %s"))

;;}}}
;;{{{ Linguae

(defun lang-oe ()
  "\'ǿ\' et \'Ǿ\' in registra ?ø et ?Ø servāre."
  (interactive)

  (set-register ?ø "ǿ")
  (set-register ?Ø "Ǿ"))

(defun o-med-kvist ()
  (interactive)

  (set-register ?, "ǫ")
  (set-register ?< "Ǫ"))

;;}}}

;; https://stackoverflow.com/a/47587185
(add-to-list 'display-buffer-alist
	     (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;;{{{ LaTeX
(setq bibtex-dialect 'biblatex
      LaTeX-electric-left-right-brace t
      LaTeX-csquotes-open-quote "\\enquote{"
      LaTeX-csquotes-close-quote "}"
      LaTeX-indent-level 4
      TeX-engine 'luatex
      TeX-auto-save t
      TeX-parse-self t)

(setenv "PATH" "/usr/local/texlive/2022/bin/x86_64-linux:$PATH" t)

(defun LaTeX-mode-hook-customizations ()
  (add-to-list 'TeX-view-program-selection '(output-pdf "Okular"))
  (add-to-list 'LaTeX-item-list '("outline" .
				  LaTeX-insert-outline-level))
  (keymap-set LaTeX-mode-map "M-RET"
	      #'LaTeX-insert-item-line-empty-p)
  (keymap-set LaTeX-mode-map "C-c l" #'LaTeX-outline-change-level)
  (keymap-set LaTeX-mode-map "C-c r" #'LaTeX-compile-from-scratch))

(add-hook 'LaTeX-mode-hook #'LaTeX-mode-hook-customizations)

(defun LaTeX-insert-item-line-empty-p ()
  "Insert a new item in an environment.
You may use `LaTeX-item-list' to change the routines used to insert the item.

Changed from the original, `LaTeX-insert-item', to add a newline only if the
current line isn't empty."
  (interactive "*")

  (let ((environment (LaTeX-current-environment)))
    (when (and (TeX-active-mark)
               (> (point) (mark)))
      (exchange-point-and-mark))
    (unless (line-empty-p) (LaTeX-newline))
    (if (assoc environment LaTeX-item-list)
        (funcall (cdr (assoc environment LaTeX-item-list)))
      (TeX-insert-macro "item"))
    (indent-according-to-mode)))

(defun LaTeX-beginning-of-outline-p ()
  "Return t if point is on the line that begins an outline environment."
  (save-excursion
    (forward-line 0)
    (looking-at-p (rx bol (zero-or-more space) "\\begin{outline}"))))

(defun LaTeX-find-outline-level ()
  "Find the current level in an outline environment.

Since this is ultimately called from `LaTeX-insert-item-line-empty-p', which
knows the current environment, we don't need to initially check that we're in an
outline environment."
  (save-excursion
    (forward-line 0)
    (cond ((LaTeX-beginning-of-outline-p) "1")
	  ((looking-at (rx bol (zero-or-more space) "\\"
			   (group (= 1 digit))))
	   (substring-no-properties (match-string 1)))
	  (t (forward-line -1)
	     (LaTeX-find-outline-level)))))

(defun LaTeX-insert-outline-level ()
  "Insert the current outline level. Stored in `LaTeX-item-list' so as to be
called by `LaTeX-insert-item-line-empty-p'."
  (let ((level (LaTeX-find-outline-level)))
    (TeX-insert-macro (concat level " "))))

(defun LaTeX-outline-change-level (&optional decrease)
  "Increase the level in an outline environment by default, increase with the
prefix arg set."
  (interactive "P")

  (unless (string-equal (LaTeX-current-environment) "outline")
    (error "Not in an outline environment."))

  (let ((cur-level-int (string-to-number (LaTeX-find-outline-level)))
	(step)
	(new-level-str)
	(outline-item-rx (rx (group bol (zero-or-more space) "\\")
			     (= 1 digit)
			     (group (zero-or-more print) eol)))
	(current-outline-pos (save-excursion
			       (re-search-backward
				(rx bol (zero-or-more space)
				    "\\begin{outline}")))))

    (if decrease
	(setq step -1)
      (setq step 1))

    (setq new-level-str (int-to-string (+ cur-level-int step)))

    (save-excursion
      (end-of-line)
      (if (re-search-backward outline-item-rx current-outline-pos t)
	  (replace-match (concat "\\1" new-level-str "\\2"))
	(message "No outline item to in-/decrease.")))))

(defun LaTeX-copy-ling-template-and-visit ()
  "Copy your linguistics template to the current directory and visit the new file."
  (interactive)

  (let* ((basename (read-from-minibuffer "Basename: "))
	 (full-name (concat basename ".tex")))
    (copy-file "~/Dokumente/latex/linguistics-template/linguistics-template.tex" full-name)
    (find-file full-name)))

(defun LaTeX-copy-mwe-and-visit ()
  "Copy your MWE to the current directory and visit the new file."
  (interactive)

  (let* ((basename (read-from-minibuffer "Basename: "))
	 (full-name (concat basename ".tex")))
    (copy-file "~/Dokumente/latex/mwe/mwe.tex" full-name)
    (find-file full-name)))

(defun LaTeX-not-tex-sty-p (x)
  "Return t if the file is regular and not a .tex or .sty file."
  (and (file-regular-p x)
       (not (string-match-p (rx line-start
				(one-or-more anychar)
				?.
				(or "tex" "sty")
				line-end)
			    x))))

(defun LaTeX-compile-from-scratch ()
  "Delete all regular, non-tex/sty files and recompile."
  (interactive)
  (let* ((files (seq-filter #'LaTeX-not-tex-sty-p (directory-files "." t))))

    (dolist (file files)
      (delete-file file))

    (TeX-command-run-all nil)))

;;}}}

;; TODO: Document this beyond a link to SE.
;; https://emacs.stackexchange.com/a/21119
(require 'comint)
(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\|^\\[sudo\\] Passwort für .*:\\s *\\'"))

;; Fix error wherein visiting a .gpg file (occasionally) failed while gpg on
;; the command line worked without problem.
;; https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
(setq epg-pinentry-mode 'loopback)

(setq fill-column 80)

;; TODO: Wrap all such calls to packages.
(if (require 'folding nil 'noerror)
    (folding-mode-add-find-file-hook)
  (message "Library `folding' not found."))

(setq tramp-default-method "ssh")

(setq require-final-newline t)

;;{{{ eshell

;; https://emacs.stackexchange.com/a/54769
(defun eshell/bcat (&rest args)
  (if (bufferp (car args))
      (with-current-buffer (car args)
        (buffer-string))
    (apply #'eshell/cat args)))

;; https://stackoverflow.com/a/27908343
(defun eshell/clear-buffer ()
  "Erase buffer contents and load new prompt."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell/ff (path)
  (find-file path))

(defun eshell/insert-buffer-name-syntax ()
  "For quicker typing of buffer names in eshell commands."
  (interactive)
  (insert "#<>")
  (backward-char))

;; https://www.emacswiki.org/emacs/EshellFunctions
(defun eshell/maybe-bol ()
  "Move to beginning of prompt/line."
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(defun eshell/mcd (path)
  "mkdir PATH && cd PATH"
  (mkdir path t)
  (cd path))

(defun signum (fasciculus)
  "Fasciculum de gpg in ~/pw/ aperīre."
  (interactive "sFasciculus: ")

  (find-file (concat "~/pw/" fasciculus ".gpg")))

(defun eshell/pw (fasciculus)
  (signum fasciculus))

(defun eshell/pwcat (path)
  (eshell/cat (concat "~/pw/" path ".gpg")))

(add-hook 'eshell-mode-hook
          (lambda () (define-key eshell-mode-map (kbd "C-a")
			'eshell/maybe-bol)))
(add-hook 'eshell-mode-hook
          (lambda () (define-key eshell-mode-map (kbd "C-c b")
			'eshell/insert-buffer-name-syntax)))
(add-hook 'eshell-mode-hook
	  (lambda () (define-key eshell-mode-map (kbd "C-c l")
			'eshell/clear-buffer)))
(add-hook 'eshell-mode-hook
	  (lambda () (define-key eshell-mode-map (kbd "C-c s o")
			'owd)))

;;}}}


(defun count-occurrences (p1 p2 s)
  "Count occurrences of S in the region."
  (interactive "r\nsString: ")

  (let ((count (count-occurrences-rec p1 p2 s)))
    (message "%s occurrence(s) of %s in the region." count s)))

(defun count-occurrences-rec (start end s)
  "Count occurrences of S between START and END."

  (save-excursion
    (goto-char start)
    (if (or (= (point) (point-max))
	    (not (re-search-forward s end t)))
        0
      (+ 1 (count-occurrences-rec (point) end s)))))

(defun dagens-ord ()
  "Få dagens ord frå Aasen."
  (interactive)

  (let* ((side (1+ (random 965)))
	(spalte (1+ (random 2)))
	(mod-time (file-attribute-modification-time (file-attributes "~/tmp/dagens-ord")))
	(mod-time-day (decoded-time-day (decode-time mod-time)))
	(mod-time-month (decoded-time-month (decode-time mod-time)))
	(mod-time-year (decoded-time-year (decode-time mod-time)))
	(cur-time (decode-time))
	(cur-time-day (decoded-time-day cur-time))
	(cur-time-month (decoded-time-month cur-time))
	(cur-time-year (decoded-time-year cur-time))
	(update (or (not mod-time)
		    (< mod-time-year cur-time-year)
		    (< mod-time-month cur-time-month)
		    (< mod-time-day cur-time-day))))

    (with-temp-buffer
      (if update
	  (progn
	    (insert (format "Side %d, spalte %d" side spalte))
	    (write-region nil nil "~/tmp/dagens-ord"))
	(insert-file-contents "~/tmp/dagens-ord"))

      (buffer-string))))

(defun vocabularium-fasciculos-agere ()
  "Fasciculos vocabularii agere."
  (interactive)

  (let* ((index "~/Dokumente/språk/vocabularium/")
	 (fasciculi (directory-files index
				     nil
				     directory-files-no-dot-files-regexp))
	 (index-exitus "/tmp/"))

    (dolist (d fasciculi)
      (vocabularium-par-creare (concat index d)
			       (concat index-exitus "anki-vocabularium-" d)))))

(defun vocabularium-par-creare (fasciculus exitus)
  "Fasciculum visere et par verbiorum creare."
  (interactive "fFasciculus: \nFExitus: ")

  (write-region 1 1 exitus)

  (with-temp-buffer
    (insert-file-contents fasciculus)

    (while (save-excursion
	     (forward-line)
	     (< (point) (point-max)))

      (let* ((lineae (vocabularium-lineas-copiare))
	     (line1 (vocabularium-lineam-convertere (car lineae)))
	     (line2 (vocabularium-lineam-convertere (cadr lineae)))
	     (fasc (file-name-nondirectory fasciculus))
	     (series-exitus (concat line1 line2 "vocabularium::" fasc "\n")))

	(write-region series-exitus 0 exitus t)))))

(defun vocabularium-lineam-copiare ()
  "Lineam praesentem ut series reddere."
  (let ((finis (save-excursion
		 (end-of-line)
		 (point))))
    (buffer-substring (point) finis)))

(defun vocabularium-lineas-copiare ()
  "Duas lineas proximas fasciculi copiare et quas ut index reddere."
  (list (vocabularium-lineam-copiare) (progn (forward-line)
					     (vocabularium-lineam-copiare))))

(defun vocabularium-lineam-convertere (series)
  "Lineam divisam a virgula, SERIES, partire et ad `verbum|lingua|` convertere."
  (let ((split-s (split-string series "|")))
    (concat (cadr split-s) "|" (car split-s) "|")))

;; Skeletons
(setq skeleton-pair t
      skeleton-pair-alist (list (quote (?„ _ ?“))))

(global-set-key (kbd "'") #'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") #'skeleton-pair-insert-maybe)
(global-set-key (kbd "(") #'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") #'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") #'skeleton-pair-insert-maybe)
(global-set-key (kbd "„") #'skeleton-pair-insert-maybe)
(global-set-key (kbd "«") #'skeleton-pair-insert-maybe)

;; Dired
(defun dired-set-shell-alist ()
  "Set preferred programs for shell commands in dired."
  (add-to-list 'dired-guess-shell-alist-user
	       (quote ("\\.pdf\\'" "okular")))
  (add-to-list 'dired-guess-shell-alist-user
	       (quote ("\\.doc\\(x\\)?\\'" "soffice")))
  (add-to-list 'dired-guess-shell-alist-user
	       (quote ("\\.epub\\'" "calibre"))))

(add-hook 'dired-mode-hook #'dired-set-shell-alist)

(defun erase-kill-ring ()
  "Set the kill ring to nil."
  (interactive)
  (setq kill-ring nil))

(defun line-empty-p ()
  "Return t if the line at point is empty."
  (save-excursion
    (forward-line 0)
    (looking-at (rx line-start (zero-or-more space) line-end))))

;; Scheme

(defun scheme-send-buffer ()
  "Send the current buffer to the inferior Scheme process."
  (interactive)
  (scheme-send-region (point-min) (point-max)))

(defun scheme-send-buffer-and-go ()
  "Send the current buffer to the inferior Scheme process.
Then switch to the process buffer. "
  (interactive)
  (scheme-send-region-and-go (point-min) (point-max)))

(defun scheme-mode-hook-customizations ()
  (keymap-set scheme-mode-map "C-c C-b" #'scheme-send-buffer)
  (keymap-set scheme-mode-map "C-c M-b" #'scheme-send-buffer-and-go))

(add-hook 'scheme-mode-hook #'scheme-mode-hook-customizations)
