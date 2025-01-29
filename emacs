; -*- mode: Emacs-lisp; folded-file: t -*-

;;; Code:

;;{{{ TODO

;; TODO: Add folding sections for everything.
;; TODO: Make explicit your key binding hierarchy.
;; TODO: Make this work: (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;;}}}
;;{{{ General Emacs tweaks

(setq load-prefer-newer t)

;; https://www.emacswiki.org/emacs/MELPA
(require 'package)

(add-to-list
 'package-archives
 (quote ("melpa" . "https://melpa.org/packages/"))
 t)

(package-initialize)

(setq-default backup-by-copying-when-linked t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(require 'tramp)     ; tramp-default-method
(require 'find-func) ; find-function-C-source-directory

(setq indent-line-function 'insert-tab
      require-final-newline t
      tramp-default-method "ssh")

;; https://masteringemacs.org/article/disabling-prompts-emacs
(fset 'yes-or-no-p 'y-or-n-p)

;; https://masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
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
(setq find-function-C-source-directory "/usr/local/src/emacs-29.4/src")
(setq confirm-nonexistent-file-or-buffer nil)

;;}}}
;;{{{ Previously disabled commands

(put 'downcase-region 'disabled nil)

;;}}}

;;}}}
;;{{{ Flycheck

(require 'flycheck)
(global-flycheck-mode)
(setq flycheck-emacs-lisp-load-path 'inherit)

;;}}}
;;{{{ Customize

;; Set but don't load.
(setq custom-file "~/.emacs.d/custom.el")

(setq safe-local-variable-values
      '((folded-file . t)
        (indent-tabs-mode nil)
        (backup-inhibited . t)
        (org-pretty-entities . t)
        (TeX-master . t)))

(load-theme 'wheatgrass)

;;}}}
;;{{{ Global keys

(global-set-key (kbd "M-DEL") nil)
(global-set-key (kbd "M-`") #'jump-to-mark)
(global-set-key (kbd "<M-backspace>") #'kill-region)
(global-set-key (kbd "C-`") #'push-mark-no-activate)
(global-set-key (kbd "C-c g e") #'open-dot-emacs)
(global-set-key (kbd "C-c g i") #'qp-imperium-cursūs-acquīrere)
(global-set-key (kbd "C-c g k") #'kill-restart-emacs)
(global-set-key (kbd "C-c g l") #'LaTeX-copy-ling-template-and-visit)
(global-set-key (kbd "C-c g m") #'LaTeX-copy-mwe-and-visit)
(global-set-key (kbd "C-c g s") #'eshell)
(global-set-key (kbd "C-c g t") #'qp-toggle-skeletons)
(global-set-key (kbd "C-c g v") #'vocabularium-fasciculos-agere)
(global-set-key (kbd "C-c g w") #'erase-kill-ring)
(global-set-key (kbd "C-c s o") #'owd)
(global-set-key (kbd "C-w") #'backward-kill-word)
;; https://emacs.stackexchange.com/a/3471
(global-set-key (kbd "C-x o") nil)
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-'") #'qp-open-text-right)
(global-set-key (kbd "M-z") #'zap-up-to-char)

;;}}}
;;{{{ Convenience functions

;;{{{ Trivial

(defun kill-restart-emacs (arg)
  "With ARG set, kill and restart Emacs, otherwise just kill."
  (interactive "P")

  (if arg
      (kill-emacs nil t)
    (kill-emacs)))

(defun open-dot-emacs ()
  "Open ~/.emacs."
  (interactive)
  (find-file "~/.emacs"))

(defun qp-file-empty-p (x)
  "Return t if file X is empty."

  (eq 0 (file-attribute-size (file-attributes x))))

(defun qp-imperium-cursūs-acquīrere ()
  "Imperium cursūs acquīrere."
  (interactive)
  (message "%s" (process-command (get-buffer-process (buffer-name)))))

(defun qp-open-text-right ()
  "Insert a space to the right of point.
Inspired by `open-line'."
  (interactive)

  (save-excursion
    (insert ? )))

;;}}}
;;{{{ Not-so trivial

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

(require 'esh-mode)

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

(defun qp-litterās-fortuītās (n)
  "Litterās ASCII imprimendās longitūdine N forte parere."
  (let ((s "")
        (x 0))

    (while (< (length s) n)
      (setf x (mod (random) 127))
      (when (> x 32)
          (setf s (concat s (format "%c" x)))))
    s))

;;}}}

;;}}}
;;{{{ Previously disabled commands

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)

;;}}}
;;{{{ Linguae

(defun lang-oe ()
  "\'ǿ\' et \'Ǿ\' in registra ?ø et ?Ø servāre."
  (interactive)

  (set-register ?ø "ǿ")
  (set-register ?Ø "Ǿ"))

(defun o-med-kvist ()
"Load the ogonek into two registers."
  (interactive)

  (set-register ?, "ǫ")
  (set-register ?< "Ǫ"))

;;{{{ APL

(add-to-list 'load-path "~/src/gnu-apl-mode")

(require 'gnu-apl-mode)
(setq gnu-apl-show-keymap-on-startup t)

;;}}}
;;{{{ Scheme

(require 'cmuscheme)

(defun scheme-send-buffer ()
  "Send the current buffer to the inferior Scheme process."
  (interactive)
  (scheme-send-region (point-min) (point-max)))

(defun scheme-send-buffer-and-go ()
  "Send the current buffer to the inferior Scheme process.
Then switch to the process buffer."
  (interactive)
  (scheme-send-region-and-go (point-min) (point-max)))

(defun scheme-mode-hook-customizations ()
  "Scheme mode customizations."
  (define-key scheme-mode-map "C-c C-b" #'scheme-send-buffer)
  (define-key scheme-mode-map "C-c M-b" #'scheme-send-buffer-and-go))

(add-hook 'scheme-mode-hook #'scheme-mode-hook-customizations)

;;}}}

;;}}}
;;{{{ Modes (hooks, customizations, etc.)

;; Not technically a mode hook, but I want it to apply for Fundamental Mode
;; (which has no hooks).
(electric-indent-mode -1)

(defun qp-disable-electric-indent ()
  (electric-indent-local-mode -1))

(add-hook 'prog-mode-hook #'qp-disable-electric-indent)

;;{{{ disable-mouse

;; TODO: Re-enable when magit/transient is > 0.7.4.
;; https://melpa.org/#/disable-mouse
;; (global-disable-mouse-mode)
;; (global-unset-key (kbd "<C-down-mouse-1>"))

;; (setq disable-mouse-mode-lighter nil
;;       disable-mouse-mode-global-lighter nil)

;;}}}
;;{{{ Dired

(require 'dired)

(defun qp-dired-beginning-of-buffer ()
  "Move point to the first line of the directory. If already there, move to
the beginning of the buffer."
  (interactive)

  (let ((first-dired-line-p (and (not (bobp))
                                (save-excursion (dired-previous-line 2)
                                                (bobp)))))
    (if first-dired-line-p
        (dired-previous-line 2)
      (goto-char (point-min))
      (dired-next-line 2))))

(defun qp-dired-ctrl-a ()
  "C-a moves point to the beginning of the file name, unless already there,
in which case it moves to the beginning of the line."
  (interactive)
  (let ((file-pos (save-excursion (dired-move-to-filename) (point))))

    (if (= (point) file-pos)
        (move-beginning-of-line nil)
      (dired-move-to-filename))))

(defun qp-dired-end-of-buffer ()
  "Move point to the last line of the directory. If already there, move to
the end of the buffer."
  (interactive)

  (let ((last-dired-line-p (and (not (eobp))
                                (save-excursion (dired-next-line 1)
                                                (eobp)))))
    (if last-dired-line-p
        (dired-next-line 1)
      (goto-char (point-max))
      (dired-previous-line 1))))

(defun qp-dired-mark-empty ()
  "Mark empty files/directories in Dired mode."
  (interactive)

  (dired-mark-if
   (let ((fn (dired-get-filename nil t)))
     (and fn
          (or (directory-empty-p fn)
              (qp-file-empty-p fn))))
   "empty file"))

(defun dired-set-shell-alist ()
  "Set preferred programs for shell commands in dired."
  (add-to-list 'dired-guess-shell-alist-user
               (quote ("\\.pdf\\'" "xreader")))
  (add-to-list 'dired-guess-shell-alist-user
               (quote ("\\.doc\\(x\\)?\\'" "soffice")))
  (add-to-list 'dired-guess-shell-alist-user
               (quote ("\\.epub\\'" "calibre")))
  (add-to-list 'dired-guess-shell-alist-user
               (quote ("\\.djvu\\'" "xreader")))
  (add-to-list 'dired-guess-shell-alist-user
               (quote ("\\.png\\'" "xviewer")))
  (add-to-list 'dired-guess-shell-alist-user
               (quote ("\\.mp4\\'" "vlc")))
  (add-to-list 'dired-guess-shell-alist-user
               (quote ("\\.mkv\\'" "vlc")))
  (add-to-list 'dired-guess-shell-alist-user
               (quote ("\\.jpg\\'" "xviewer"))))

(define-key dired-mode-map "% e" #'qp-dired-mark-empty)
(define-key dired-mode-map [remap move-beginning-of-line] #'qp-dired-ctrl-a)
(define-key dired-mode-map [remap beginning-of-buffer] #'qp-dired-ctrl-a)
(define-key dired-mode-map [remap end-of-buffer] #'qp-dired-ctrl-a)

(add-hook 'dired-mode-hook #'dired-set-shell-alist)

;;}}}
;;}}}
;;{{{ mu4e

;; Helpful link(s).
;; https://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and/

(require 'mu4e)

;; TODO: Do you want this everywhere in dired-mode?
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(add-hook 'message-mode-hook #'turn-off-auto-fill)
(add-hook 'message-mode-hook #'use-hard-newlines)

(setq mail-user-agent 'mu4e-user-agent
      message-dont-reply-to-names #'mu4e-personal-or-alternative-address-p
      message-kill-buffer-on-exit t
      message-send-mail-function #'message-send-mail-with-sendmail
      mu4e-attachment-dir "~/Downloads"
      mu4e-compose-context-policy 'ask-if-none
      mu4e-compose-format-flowed t
      mu4e-context-policy 'pick-first
      mu4e-headers-fields '((:human-date . 12)
			                (:flags . 6)
			                (:mailing-list . 10)
			                (:from-or-to . 22)
			                (:thread-subject))
      mu4e-modeline-support nil
      read-mail-command 'mu4e
      sendmail-program "/usr/bin/msmtp")

;; Hat tip to Dalker, adapted from https://emacs.stackexchange.com/a/58461.
(defun qp-mu4e-switch-mail-account ()
  "Quit and reload mu4e to properly change context."
  (interactive)
  (mu4e-context-switch)
  (mu4e-quit)
  (sit-for .5)
  (mu4e))

;; Redefine context switching in a few places.
(define-key mu4e-main-mode-map (kbd ";") #'qp-mu4e-switch-mail-account)
(define-key mu4e-headers-mode-map (kbd ";") #'qp-mu4e-switch-mail-account)

;;}}}
;;{{{ LaTeX

(require 'bibtex)
(require 'latex)
(require 'tex)

(setq bibtex-dialect 'biblatex
      LaTeX-electric-left-right-brace t
      LaTeX-csquotes-open-quote "\\enquote{"
      LaTeX-csquotes-close-quote "}"
      LaTeX-indent-level 4
      TeX-engine 'luatex
      TeX-auto-save t
      TeX-parse-self t)

(setenv "PATH" "/usr/local/texlive/2024/bin/x86_64-linux:$PATH" t)

(defun LaTeX-mode-hook-customizations ()
  (add-to-list 'TeX-view-program-selection '(output-pdf "Xreader"))
  (add-to-list 'LaTeX-item-list '("outline" .
				  LaTeX-insert-outline-level))
  (define-key LaTeX-mode-map "M-RET"
	      #'LaTeX-insert-item-line-empty-p)
  (define-key LaTeX-mode-map "C-c l" #'LaTeX-outline-change-level)
  (define-key LaTeX-mode-map "C-c r" #'LaTeX-compile-from-scratch))

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
    (error "Not in an outline environment"))

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
  "Return t if X is a regular file and not a .tex or .sty file."
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
  (let ((files (seq-filter #'LaTeX-not-tex-sty-p (directory-files "." t))))

    (dolist (file files)
      (delete-file file))

    (TeX-command-run-all nil)))

;;}}}
;;{{{ Org

(require 'org)
(require 'org-capture)

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
(define-key org-mode-map "C-'" #'qp-open-text-right)

(setq org-agenda-files '("~/org/")
      org-capture-templates '(("t" "Create a TODO item."
			                   entry
			                   (file "~/org/todo.org")
                               "* TODO %?")
                              ("v" "Verbum novum aut ignotum.")
                              ("vg" "Lingua Graeca."
                               entry
                               (file+olp "~/org/språk.org" "Vocābulārium" "Lingua Graeca")
                               "*** %?")
                              ("vl" "Lingua Lātīna."
                               entry
                               (file+olp "~/org/språk.org" "Vocābulārium" "Lingua Lātīna")
                               "*** %?")
                              ("vn" "Lingua gesticulatoria norvegica."
                               entry
                               (file+olp "~/org/språk.org" "Vocābulārium" "Lingua gesticulatoria norvegica")
                               "*** %?")
                              ("vs" "Verbum substituendum."
                               entry
                               (file+olp "~/org/språk.org" "Vocābulārium" "Verbum substituendum")
                               "*** %?")
                              ("vå" "Lingua Aasensis."
                               entry
                               (file+olp "~/org/språk.org" "Vocābulārium" "Lingua Aasensis")
                               "*** %?"))
      org-export-backends '(ascii html icalendar latex md man)
      org-special-ctrl-a/e t)

(add-to-list 'org-file-apps '("\\.pdf\\'" . "xreader %s"))
(add-to-list 'org-file-apps '("\\.epub\\'" . "calibre %s"))
;;}}}
;;{{{ Shells

;; Extend the password prompt regex to include German.
;; https://emacs.stackexchange.com/a/21119
(require 'comint)
(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\|^\\[sudo\\] Passwort für .*:\\s *\\'"))

;;}}}
;;{{{ Skeletons

(require 'skeleton)

(setq skeleton-pair t
      skeleton-pair-alist (list (quote (?„ _ ?“))))

(defvar qp-skeletons t "Set to t if user-defined skeletons are enabled.")

(defun qp-skeletons (arg)
  "Enable skeletons if ARG, otherwise disable them."

  (let ((chars (list "'" "\"" "(" "[" "{" "„" "«" "`" "%")))
    (if arg
        (dolist (char chars)
          (global-set-key (kbd char) #'skeleton-pair-insert-maybe))
      (dolist (char chars)
        (global-set-key (kbd char) #'self-insert-command))))

  (setq qp-skeletons arg))

(defun qp-toggle-skeletons ()
  "Interactive command to toggle your skeletons."
  (interactive)
  (if qp-skeletons
      (progn
        (qp-skeletons nil)
        (message "Die Skelette sind ausgeschaltet."))
    (qp-skeletons t)
    (message "Die Skelette sind eingeschaltet.")))

(qp-skeletons t)

;;}}}

(add-hook 'write-file-functions #'delete-trailing-whitespace)

(setq backup-by-copying t)

;; https://melpa.org/#/buffer-move
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(global-set-key (kbd "C-x g") 'magit-status)

;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)
(setq window-sides-slots '(1 0 2 0))

(add-to-list 'display-buffer-alist
	         '("\\*Async Shell Command\\*"
               display-buffer-no-window))
(add-to-list 'display-buffer-alist
             `(,(rx (| "*info*"
                       "*Help*"
                       "*Compile-Log*"
                       "*Shortdoc"
                       "*Man"
                       "*Flycheck"
                       "*Apropos*"))
               (display-buffer-reuse-window
                display-buffer-in-previous-window
                display-buffer-in-side-window)
               (side . right)
               (window-width . 0.35)))
(add-to-list 'display-buffer-alist
             `(,(rx (| "*xref*"
                       "*grep*"
                       "*Occur*"))
               (display-buffer-reuse-window
                display-buffer-in-previous-window)))
(add-to-list 'display-buffer-alist
             '("\\*SQLite"
               (display-buffer-in-direction)
               (direction . bottom)
               (window-height . 0.5)
               (window . root)))
(add-to-list 'display-buffer-alist
             '("\\*e?shell"
               (display-buffer-in-direction)
               (direction . bottom)
               (window-height . 0.40)))

(defun qp-display-help-buffer ()
  "Display the *Help* buffer, creating a blank buffer if it doesn't exist.
Intended to allow for quick switching back to the *Help* buffer."
  (interactive)
  (display-buffer (get-buffer-create "*Help*")))

(global-set-key (kbd "C-h C-h") #'qp-display-help-buffer)

(require 'comint)

(defun qp-comment-and-send-input ()
  "Comment the current line and return, as `insert-comment' in bash."
  (interactive)

  (let ((bol (save-excursion
               (move-beginning-of-line nil)
               (point)))
        (eol (save-excursion
               (move-end-of-line nil)
               (point))))
    (comment-or-uncomment-region bol eol)
    (comint-send-input)))

(define-key comint-mode-map "M-#" #'qp-comment-and-send-input)

;; Fix error wherein visiting a .gpg file (occasionally) failed while gpg on
;; the command line worked without issue.
;; https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
;; TODO: See if this breaks anything.
;; Commented out - Sa 9. Nov 20:57:50 CET 2024
;; (setq epg-pinentry-mode 'loopback)

(setq fill-column 80)

;; TODO: Wrap all such calls to packages.
;; https://raw.githubusercontent.com/jaalto/project-emacs--folding-mode/master/folding.el
(require 'folding)

(if (require 'folding nil 'noerror)
    (folding-mode-add-find-file-hook)
  (message "Library `folding' not found."))

;;{{{ eshell

(require 'em-unix)

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
  "Make and create PATH.
Equivalent to mkdir PATH && cd PATH."
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

(defun erase-kill-ring ()
  "Set the kill ring to nil."
  (interactive)
  (setq kill-ring nil))

(defun line-empty-p ()
  "Return t if the line at point is empty."
  (save-excursion
    (forward-line 0)
    (looking-at (rx line-start (zero-or-more space) line-end))))

(defun latin-postfix-customizations ()
  (interactive)
  (set-input-method "latin-postfix")
  (deactivate-input-method)

  (let ((quail-current-package (assoc "latin-postfix" quail-package-alist)))
    (quail-define-rules ((append . t))
			("a," ?ą)
			("&-" ["&" "⁊"])
			("y-" ?ȳ)
			("Y-" ?Ȳ)
			("ø'" ?ǿ)
			("Ø'" ?Ǿ))))

(defun norrønt-input ()
  "Define my own input-method for writing Old Norse."
  (interactive)
  (load-file "~/Links/norrønt.el")
  (set-input-method "norrønt"))

(defun text-mode-hook-customization ()
  "Customizations for text mode."
  (latin-postfix-customizations))

(add-hook 'text-mode-hook #'text-mode-hook-customization)

(defun count-occurrences (p1 p2 regexp)
  "Count occurrences of REGEXP in the region."
  (interactive "r\nsString: ")

  (let ((count (count-occurrences-regexp p1 p2 regexp)))
    (message "%s occurrence(s) of %s in the region." count regexp)))

(defun count-occurrences-regexp (start end regexp)
  "Count occurrences of REGEXP between START and END."

  (length (matching-strings-regexp start end regexp)))

(defun matching-strings (start end regexp)
  "Return a list of the strings matching REGEXP in the region."
  (interactive "r\nsString (regexp): ")

  (let ((strings (matching-strings-regexp start end regexp)))
    (message "Found %s in region." strings)))

(defun matching-strings-regexp (start end regexp)
  "Return a list of the strings matching REGEXP between START and END."

  (save-excursion
    (goto-char start)

    (cl-loop while (and (< (point) end)
			(re-search-forward regexp end t))
	     collect (substring-no-properties (match-string 0)))))

(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)

(when (file-exists-p "~/.emacs-local.el")
  (load-file "~/.emacs-local.el"))
