;; .emacs

(if (load (expand-file-name "~/quicklisp/slime-helper.el") t)
    ;; Replace "sbcl" with the path to your implementation
    (setq inferior-lisp-program "sbcl"))

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; javascript tab-width
(defun my-edit-tab-width (width)
  "Change the width of tabs"
  (interactive "nwidth: \n")
  (setq javascript-indent-level width))

;; load packages using marmalade
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(global-set-key [delete] 'delete-char)
(global-set-key [M-delete] 'kill-word)

(setq-default indent-tabs-mode nil)
(column-number-mode 1)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x t") 'dirtree)

;; search for word at cursor
(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun my-isearch-yank-word-hook ()
  (when (equal this-command 'my-isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))

(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)

(add-to-list 'load-path (expand-file-name "~/.custom_emacs"))
(load "python-mode.el")
(load "javascript.el")
(load "psvn.el")
(load "fast-resize.el")
(load "highlight-80+.el")
(load "number-sequence.el")
(load "php-mode.el")

;; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; tree
(add-to-list 'load-path (expand-file-name "~/.custom_emacs/tree"))
(load "imenu-tree.el")
(load "tags-tree.el")
(load "tree-mode.el")
(autoload 'imenu-tree "imenu-tree" "Imenu tree" t)
(autoload 'tags-tree "tags-tree" "TAGS tree" t)
(eval-after-load "tree-widget"
  '(if (boundp 'tree-widget-themes-load-path)
       (add-to-list 'tree-widget-themes-load-path "~/.custom_emacs/tree")))

;; dirtree
(require 'windata)
(autoload 'dirtree "dirtree" "Add directory to tree view" t)

;; color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-euphoria)

;; highlight mode
(highlight-80+-mode 1)

;; auto load python mode
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                     interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; auto load javascript mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;; auto load nxml mode
(load "~/.nxml-mode-20041004/rng-auto.el")
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|tmpl\\)\\'" . nxml-mode)
            auto-mode-alist))

;; sort python definitions in a source file
;; requires python-mode
(defun python-sort-defs (reverse beg end)
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr reverse
                 (function
                  (lambda ()
                    (while (and (not (eobp)) (looking-at paragraph-separate))
                      (forward-line 1))))
                 'python-end-of-block-must-move))))

(defun python-end-of-block-must-move ()
  (when (eq (point) (progn
                      (py-end-of-def-or-class)
                      (point)))
    (forward-line 1)))

;; auto load flymake mode with pyflakes
;; (require 'compile)
;; (require 'flymake)
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pyflakes" (list local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))

;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; In XEmacs syntax highlighting should be enabled automatically.  In GNU
;; Emacs you may have to add these lines to your ~/.emacs file:
;(when (fboundp 'global-font-lock-mode)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
; '(auto-compression-mode t nil (jka-compr))
; '(case-fold-search t)
; '(current-language-environment "UTF-8")
; '(default-input-method "rfc1345")
; '(global-font-lock-mode t nil (font-lock)))
;(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
; )
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
