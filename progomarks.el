;; -*- lexical-binding: t
;; This is progomarks: like the built-in bookmarks support but way
;; simpler and faster to access.

;; Installation:
;; - place this file under your Emacs' load-path.
;; - (require 'progomarks)
;;
;; In more detail:
;; - place this file in "~/.emacs.d/elisp"
;; - in your ".emacs" add the following:
;;     (add-to-list 'load-path  "~/.emacs.d/elisp")
;;     (require 'progomarks)

(defvar progomarks-file "~/.emacs.d/progomarks"
  "The location of a file containing stored progomarks.")

(defvar progomarks-mode-map nil
  "Keymap for progomarks major mode.")

(defvar progomarks-mode-hook nil)

;; Functionality

(defun progomarks--mode ()
  "Init major mode `progomarks'. Not for direct use: activate
with command `progomarks'."
  (interactive)
  (progn
    (kill-all-local-variables)
    (setq major-mode 'progomarks-mode)
    (setq mode-name "progomarks")
    (use-local-map progomarks-mode-map)
    (run-hooks 'progomarks-mode-hook)))

(defun progomarks--shortcut (num)
  "Turn a number to a Vimperator-like ascii key sequence."
  (let* ((alphabet "jkfalg")
         (base (length alphabet))
         (shortcut ""))
    (while (progn
             (setq shortcut
                   (concat (string (aref alphabet (mod num base)))
                           shortcut))
             (setq num (floor (/ num base)))
             (> num 0)))
    shortcut))

(defun progomarks--initialize ()
  "Read the marks from the file. And map the keys."
  (setq progomarks-mode-map (make-sparse-keymap))
  (suppress-keymap progomarks-mode-map)
  (define-key progomarks-mode-map (kbd "q") 'bury-buffer)
  (let ((counter 1))
    (when (file-readable-p progomarks-file)
      (with-temp-buffer
        (insert-file-contents progomarks-file)
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (let* ((keystr (progomarks--shortcut counter))
                 (uri (substring (thing-at-point 'line) 0 -1))
                 (opener (lambda ()
                           (interactive)
                           (find-file uri))))
            (define-key progomarks-mode-map
              (read-kbd-macro keystr)
              opener)
            (insert keystr ": ")
            (forward-line)
            (setq counter (+ 1 counter))))
        (buffer-string)))))

(defun progomarks ()
  "Pop out the progomarks buffer."
  (interactive)
  (let ((pm-buffer (get-buffer-create "*progomarks*")))
    (with-current-buffer pm-buffer
      (erase-buffer)
      (insert "Progomarks\n"
              "----------\n")
      (insert (progomarks--initialize) "\n")
      (progomarks--mode))
    (switch-to-buffer pm-buffer)))
  
(defun progomarks-mark-current-file ()
  "Insert the current buffer's file to progomarks file."
  (interactive)
  (let ((buf-file buffer-file-name))
    (with-temp-buffer
      (insert-file-contents progomarks-file)
      (end-of-buffer)
      (insert buf-file "\n")
      (write-region (point-min)
                    (point-max)
                    progomarks-file))))

;; evil integration
(when (and (boundp 'evil-emacs-state-modes)
           (listp evil-emacs-state-modes)
           (not (memq 'progomarks-mode evil-emacs-state-modes)))
  (add-to-list 'evil-emacs-state-modes
               'progomarks-mode))

(provide 'progomarks)
;; The end.
