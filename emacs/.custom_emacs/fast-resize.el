;; fast window resize
;; from http://emacswiki.org/emacs/WindowResize

(defun xor (b1 b2)
  "Exclusive or of its two arguments."
  (or (and b1 b2)
      (and (not b1) (not b2))))
     
(defun move-border-left-or-right (arg dir)
  "General function covering move-border-left and move-border-right. If DIR is
     t, then move left, otherwise move right."
  (interactive)
  (if (null arg) (setq arg 20))
  (let ((left-edge (nth 0 (window-edges))))
    (if (xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))
     
(defun move-border-left (arg)
  "If this is a window with its right edge being the edge of the screen, enlarge
     the window horizontally. If this is a window with its left edge being the edge
     of the screen, shrink the window horizontally. Otherwise, default to enlarging
     horizontally.
     
     Enlarge/Shrink by ARG columns, or 5 if arg is nil."
  (interactive "P")
  (move-border-left-or-right arg t))
     
(defun move-border-right (arg)
  "If this is a window with its right edge being the edge of the screen, shrink
     the window horizontally. If this is a window with its left edge being the edge
     of the screen, enlarge the window horizontally. Otherwise, default to shrinking
     horizontally.
     
     Enlarge/Shrink by ARG columns, or 5 if arg is nil."
  (interactive "P")
  (move-border-left-or-right arg nil))
     
(global-set-key (kbd "C-x {") 'move-border-left)
(global-set-key (kbd "C-x }") 'move-border-right)
