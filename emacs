; -*- mode: Emacs-lisp; -*-

;;{{{ TODO

;; TODO: Add folding sections for everything.
;; TODO: Make explicit your key binding hierarchy.
;; TODO: Make this work: (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;;}}}
;;{{{ General Emacs tweaks

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
   (quote
    (folding buffer-move gnu-elpa-keyring-update disable-mouse go-mode)))
 '(safe-local-variable-values (quote ((backup-inhibited . t) (org-pretty-entities . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;}}}
;;{{{ Global keys

(global-set-key (kbd "M-DEL") nil)
(global-set-key (kbd "M-`") 'jump-to-mark)
(global-set-key (kbd "M-[") 'insert-brackets)
(global-set-key (kbd "<M-backspace>") 'kill-region)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "C-c g e") 'open-emacs)
(global-set-key (kbd "C-c g s") 'eshell)
(global-set-key (kbd "C-c s o") 'owd)
(global-set-key (kbd "C-w") 'backward-kill-word)
;; https://emacs.stackexchange.com/a/3471
(global-set-key (kbd "C-x o") nil)
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-'") 'open-text-right)
(global-set-key (kbd "M-z") 'zap-to-char-before)

;;}}}
;;{{{ Trivial convenience functions

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

;; https://www.emacswiki.org/emacs/InsertPair
(defun insert-brackets (&optional arg)
  "Enclose following ARG sexps in brackets.
Leave point after open-paren."
  (interactive "*P")
  (insert-pair nil ?\[ ?\]) [])

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

(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; http://ergoemacs.org/emacs/emacs_package_system.html
;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t))

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
;;}}}
;;{{{ Linguae

(defun lang-oe ()
  (interactive)

  (set-register ?ø "ǿ")
  (set-register ?Ø "Ǿ"))

(defun o-med-kvist ()
  (interactive)

  (set-register ?, "ǫ")
  (set-register ?< "Ǫ"))

(defun latin-macrons ()
  "Place macrons over Latin vowels."
  (interactive)

  (let ((current-char (char-after (point)))
	;; ((a . ā) (e . ē) (i . ī) (o . ō) (u . ū))
	(v-to-m '((97 . 257) (101 . 275) (105 . 299) (111 . 333) (117 . 363))))
    (delete-forward-char 1)
    (insert (alist-get current-char v-to-m current-char))))

;; TODO: Combine this with declinatio-macrons?
(defun куриллическое-ударение ()
  """Replace vowels with their stressed counterpart."""
  (interactive)

  (let
      ((vowels (list 1040 1045 1048 1054 1059 1067 1069 1070 1071 1072 1077 1080 1086 1091 1099 1101 1102 1103)))
    (if (member (char-after (point)) vowels)
	(progn
	  (forward-char)
	  (insert-string "́"))
      (forward-char))))

;; TODO: Account for line endings.
(defun russian-stress (&optional point mark)
  """Find the next vowel for optional overwriting."""
  (interactive "r")

  (save-mark-and-excursion

   (let ((response nil)
	 (vowels "[АЕИОУЫЭЮЯаеиоуыэюя]")
	 (start
	  (if (region-active-p) (region-beginning) (point)))
	 (end
	  (if (region-active-p) (region-end) (point-max))))

     (deactivate-mark nil)
     (goto-char start)

     (while (< (point) end)
       (search-forward-regexp vowels)
       (backward-char)
       (while (not (member response '("y" "n" "s" "b")))
	 (setq response (read-string "Replace? <y[es]/n[o]/s[kip]>: n" nil nil "n" nil)))
       (cond ((equal response "y")
	      (куриллическое-ударение)
	      (search-forward " "))
	     ((equal response "s") (forward-word))
	     ((equal response "n") (forward-char))
	     ((equal response "b") (search-backward-regexp vowels)))
       (setq response nil)))))

;;}}}

;; https://stackoverflow.com/a/47587185
(add-to-list 'display-buffer-alist
	     (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;;{{{ LaTeX

(setq bibtex-dialect 'biblatex)
;; (add-hook 'text-mode-hook 'auto-fill-mode)
(setq TeX-engine 'luatex)

;; Use okular instead of evince for viewing pdf documents w/ View in AUCTeX.
;; https://emacs.stackexchange.com/a/3402
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (add-to-list 'TeX-view-program-selection '(output-pdf "Okular"))))

(setenv "PATH" "/usr/local/texlive/2022/bin/x86_64-linux:$PATH" t)

(put 'TeX-narrow-to-group 'disabled nil)

;;}}}

;; https://emacs.stackexchange.com/a/21119
(require 'comint)
(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\|^\\[sudo\\] Passwort für .*:\\s *\\'"))

;; Fix error wherein visiting a .gpg file (occasionally) failed while gpg on
;; the command line worked without problem
;; https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
(setq epg-pinentry-mode 'loopback)

(setq fill-column 80)

(if (require 'folding nil 'noerror)
    (folding-mode-add-find-file-hook)
  (message "Library `folding' not found"))

(setq tramp-default-method "ssh")

(setq require-final-newline t)
(put 'LaTeX-narrow-to-environment 'disabled nil)

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

(defun eshell/pw (path)
  (find-file (concat "~/pw/" path ".gpg")))

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


(defun zap-to-char-before (arg char)
  "Kill up to but not including the ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char "Zap to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	(setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (progn
			 (search-forward (char-to-string char) nil nil arg)
			 (backward-char)
			 (point))))
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
