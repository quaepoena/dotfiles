; -*- mode: Emacs-lisp; -*-

(package-initialize)

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

;; https://masteringemacs.org/article/disabling-prompts-emacs
(fset 'yes-or-no-p 'y-or-n-p)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
	kill-buffer-query-functions))

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

(setq enable-recursive-minibuffers t)
(put 'downcase-region 'disabled nil)
(setq sentence-end-double-space nil)
(setq async-shell-command-buffer 'new-buffer)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "<C-backspace>") 'kill-region)
(global-set-key (kbd "<M-backspace>") 'kill-region)
(global-set-key (kbd "M-DEL") nil)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)
(global-set-key (kbd "M-[") 'insert-brackets)

(setq column-number-mode t)
(setq find-function-C-source-directory "~/src/emacs25-25.2+1/src/")

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

;; https://emacs.stackexchange.com/a/3471
(global-set-key (kbd "C-x o") nil)
(global-set-key (kbd "C-.") #'other-window)

(add-hook 'write-file-hooks 'delete-trailing-whitespace)


;; http://ergoemacs.org/emacs/emacs_package_system.html
;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "https://melpa.org/packages/")
   t))

;; https://wiki.archlinux.org/title/Mozc#Mozc_for_Emacs
(require 'mozc)
(setq default-input-method "japanese-mozc")
(setq mozc-candidate-style 'overlay)

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
;; Recommended org mode key bindings from the info page
(require 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files '("~/org/"))

(setq org-capture-templates '(("t" "Create a TODO item."
			       entry
			       (file "~/org/todo.org")
			       (file "~/org/todo-template"))))

(add-to-list 'org-file-apps '("\\.pdf\\'" . "okular %s"))
;;}}}

;; TODO: This can be simplified by using a combining macron.
(defun declinatio-macrons ()
  """Replace a/e/i/o/u with their respective variants with macrons."""
  (interactive)

  (cond ((eq (char-after (point)) 97)
	 (progn (delete-forward-char 1)
		(insert-char ?ā)))
	((eq (char-after (point)) 101)
	 (progn (delete-forward-char 1)
		(insert-char ?ē)))
	((eq (char-after (point)) 105)
	 (progn (delete-forward-char 1)
		(insert-char ?ī)))
	((eq (char-after (point)) 111)
	 (progn (delete-forward-char 1)
		(insert-char ?ō)))
	((eq (char-after (point)) 117)
	 (progn (delete-forward-char 1)
		(insert-char ?ū)))
	(t (forward-char))))

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
	  (if (region-active-p) (min (point) (mark)) (point)))
	 (end
	  (if (region-active-p) (max (point) (mark)) (point-max))))

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


;; https://stackoverflow.com/a/47587185
(add-to-list 'display-buffer-alist
	     (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;;{{{ LaTeX
(setq bibtex-dialect 'biblatex)
(add-hook 'text-mode-hook 'auto-fill-mode)
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
(setq epa-pinentry-mode 'loopback)

(setq fill-column 80)

(if (require 'folding nil 'noerror)
    (folding-mode-add-find-file-hook)
  (message "Library `folding' not found"))

(defun o-med-kvist ()
  (interactive)
     (progn (set-register ?o "ǫ")
	    (set-register ?O "Ǫ")))

(defun open-text-right ()
  "Inspired by open-line, the result is an inserted space
   with point in the same place."
  (interactive)
  (insert " ")
  (backward-char))

(global-set-key (kbd "C-'") 'open-text-right)

(setq tramp-default-method "ssh")

;; TODO: fix this
(setq require-final-newline 'visit-save)
(put 'LaTeX-narrow-to-environment 'disabled nil)

(defun external-paste ()
  "Divide the region in two and call `paste (1)` with the two halves.
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
