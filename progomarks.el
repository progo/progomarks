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

(defvar progomarks-alphabet "jklfa"
  "The key pool from which the shortcuts are drawn.")

(defvar progomarks-endkey "w"
  "The key that finishes a shortcut. Must not be present in the
  alphabet. If you use a non-single-letter key, please prefix by
  space so that Emacs' key reader understands it correctly.")

(defvar progomarks-mode-hook nil)

;; Functionality

;; straigth from ElispCookbook
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

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
  "Turn a number to a Vimperator-like ascii key sequence. To satisfy
Emacs' submap requirements we insert another key at the end."
  (let* ((alphabet progomarks-alphabet)
         (finisher progomarks-endkey)
         (base (length alphabet))
         (shortcut ""))
    (while (progn
             (setq shortcut
                   (concat (string (aref alphabet (mod num base)))
                           shortcut))
             (setq num (floor (/ num base)))
             (> num 0)))
    (concat shortcut finisher)))

(defun progomarks--initialize ()
  "Read the marks from the file. And map the keys."
  (setq progomarks-mode-map (make-sparse-keymap))
  (suppress-keymap progomarks-mode-map)
  (define-key progomarks-mode-map (kbd "q") 'bury-buffer)
  (let ((counter 0))
    (when (file-readable-p progomarks-file)
      (with-temp-buffer
        (insert-file-contents progomarks-file)
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (let* ((keystr (progomarks--shortcut counter))
                 (uri (chomp (thing-at-point 'line)))
                 (opener (lambda ()
                           (interactive)
                           (bury-buffer)
                           (find-file uri))))
            (define-key progomarks-mode-map
              (read-kbd-macro keystr)
              opener)
            (insert (format "%-20s" keystr))
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

(defun progomarks--bookmarks-list ()
  "Collect a list of bookmarked paths."
  (with-temp-buffer
    (insert-file-contents progomarks-file)
    (split-string (buffer-string) "\n" t)))

(defun progomarks-ido ()
  "Use Ido for visiting a bookmarked file."
  (interactive)
  (let ((file-to-open (ido-completing-read "open: "
                                           (progomarks--bookmarks-list))))
    (find-file file-to-open)))

(defun progomarks-mark-current-file ()
  "Insert the current buffer's file to progomarks file."
  (interactive)
  (let ((buf-file buffer-file-name))
    (with-temp-buffer
      (insert-file-contents progomarks-file)
      (end-of-buffer)
      (insert "\n" buf-file)
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
