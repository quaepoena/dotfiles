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
    (buffer-move gnu-elpa-keyring-update disable-mouse go-mode))))
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

;; Originally added for fold-lines.
(require 'cl)

(setq enable-recursive-minibuffers t)
(put 'downcase-region 'disabled nil)
(setq sentence-end-double-space nil)
(setq backup-by-copying-when-linked t)
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

(defun fold-lines (point mark)
  "Join lines together.
Let l be the number of lines in a region, such that l % 2 == 0.
Join line 1 with line l / 2 + 1, line 2 with l / 2 + 2, etc."
  (interactive "r")

  (assert (= (% (count-lines point mark) 2) 0)
	  t "fold-lines must be used on an even number of lines.")

  (let* ((lines (count-lines point mark))
	 (lines-remaining (/ lines 2))
	 (lines-to-jump
	  (if (= lines 2)
	      1 (- (/ lines 2) 1))))

    (while (> lines-remaining 0)
      (goto-char mark)
      (move-beginning-of-line nil)
      (kill-line)
      (delete-char -1 nil)
      (forward-line (* lines-to-jump -1))
      (move-end-of-line nil)
      (insert " ")
      (yank)
      (setq lines-remaining (1- lines-remaining)))
    ))

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


;; Recommended org key bindings from the info page
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

;; Keyboard macro for quick creation of cloze cards
(fset 'cloze
   "\C-e\342{{c\C-xria::\C-e}}\C-a\C-n\C-xr+a")
