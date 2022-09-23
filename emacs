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
 '(package-selected-packages
   (quote
    (buffer-move gnu-elpa-keyring-update disable-mouse go-mode)))
 '(safe-local-variable-values (quote ((org-pretty-entities . t)))))
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


;; Recommended org mode key bindings from the info page
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files '("~/org/"))


(put 'upcase-region 'disabled nil)

(setq org-capture-templates '(("t" "Create a TODO item."
			       entry
			       (file "~/org/todo.org")
			       (file "~/org/todo-template"))))
(put 'set-goal-column 'disabled nil)

;; TODO: Move this to forgotten code.
;; Keyboard macros for quick creation of cloze cards
(fset 'cloze-multi
   "\C-e\342{{c\C-xria::\C-e}}\C-a\C-n\C-xr+a")
(fset 'cloze-single
   "{{c\C-xria::\346}}\C-f\C-f\C-f\C-xr+a")

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

;; Swap rows such that accusative is under nominative.
(fset 'n-g
   "\C-n\C-n\C-n\C-u1\C-k\C-p\C-p\C-y\C-p\C-p")

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

;; Unfinished.
;; TODO: Finish this function or delete it.
(defun deutsche-dingwoerter (&optional point mark)
  """Clean up German noun entries from Wiktionary."""
  (interactive "r")

  (let ((start (point))
	(start
	 (if (region-active-p) (min (point) (mark)) (point)))
	(end
	 (if (region-active-p) (max (point) (mark)) (point-max))))

    (while (search-forward-regexp "[[:space:]]+")
      (replace-match " ")
      (forward-char))

    (goto-char start)
    (delete-trailing-whitespace)))

;; TODO: Is this still relevant?
;; TODO: Macro for creating cloze cards, needs to be improved.
(fset 'anki-cloze-latin
   [?\{ ?\{ ?c ?1 ?: ?: ?\M-f ?: ?: ?: ?: backspace backspace ?\} ?\} ?\C-a ?\C-n ?\C-k ?\C-p ?\C-s ?: ?: ?\C-s return ?\C-y ?\C-a])

;; https://stackoverflow.com/a/47587185
(add-to-list 'display-buffer-alist
	     (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(defun count-overdue (start end)
  """Format the overdue values from Org Agenda for summing and insertion into the kill ring. """
  (interactive "r")

  (let ((oldbuf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring oldbuf start end)

      (goto-char (point-min))
      (while (re-search-forward ".*?\\([[:digit:]]+\\).*" nil t)
	(replace-match "\\1"))

      (goto-char (point-min))
      (subst-char-in-region (point-min) (point-max) ?\n ?\s)

      (fixup-whitespace)
      (insert "(+ ")
      (delete-trailing-whitespace)

      (goto-char (point-max))
      (insert ")")

      (kill-new
       (number-to-string
	(eval (car (read-from-string
		    (buffer-substring-no-properties (point-min) (point-max))))))))))
