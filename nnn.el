;;; nnn.el --- TODO -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Yoav Marco
;;
;; Author: Yoav Marco <http://github/ymarco>
;; Maintainer: Yoav Marco <yoavm448@gmail.com>
;; Created: October 30, 2020
;; Modified: October 30, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/ymarco/nnn
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(unless module-file-suffix
  (error "nnn needs module support.  Please compile Emacs with the --with-modules option!"))

(module-load (concat default-directory "nnn-module.so"))
(let ((ctx (nnn-make-context default-directory)))
  (nnn-populate-context ctx)
  (nnn-get-file-at-index ctx 0)
  (nnn-get-entry-index ctx "LICENSE"))



(defvar-local nnn-ctx nil
  "nnn directory context for current buffer")

(defun nnn-fontify-region (beg end)
  (save-excursion
    (goto-char beg)
    (while (not (>= (point) end))
      (let ((lbp (line-beginning-position))
            (lep (line-end-position)))
        (if (= (char-before lep) ?/)
            (put-text-property (1+ lbp) (1- lep)
                               'face 'font-lock-function-name-face)
          (pcase (char-before lep)
            (?*
             (put-text-property (1+ lbp) (1- lep)
                                'face 'font-lock-string-face)))
          (goto-char lep)
          (when (search-backward "." lbp t)
            (put-text-property (point) lep
                               'face 'font-lock-comment-delimiter-face))))
      (forward-line))))



(defvar nnn-buffers-alist nil
  "A global alist of directories and the nnn buffers corresponding to them.")

(defun nnn-generate (dir)
  "Return a new filled-out nnn buffer in `default-directory'."
  (let ((buffer (generate-new-buffer dir)))
    (with-current-buffer buffer
      (setq default-directory (expand-file-name
                               (file-name-as-directory dir)))
      (add-hook 'kill-buffer-hook (lambda () (assoc-delete-all dir nnn-buffers-alist))
                nil t)
      (nnn-init-buffer))
    buffer))

(defun nnn-goto-dir (dir)
  "Return a buffer in which DIR is opened in nnn."
  (setq dir (file-name-as-directory dir))
  (let ((buffer (cdr (assoc dir nnn-buffers-alist))))
    (if buffer
        (message "cached nnn")
      (setq buffer (nnn-generate dir))
      (push (cons dir buffer) nnn-buffers-alist))
    buffer))

(defun nnn-kill-all-buffers ()
  "Kill all nnn buffers."
  (interactive)
  (mapc (lambda (dir.buf) (kill-buffer (cdr dir.buf)))
        nnn-buffers-alist)
  (setq nnn-buffers-alist nil))

(defun nnn-open-entry (num)
  "Open the NUMth file of current `nnn-ctx'."
  (interactive (list (1- (line-number-at-pos (point)))))
  (let ((file (concat default-directory
                      (nnn-get-file-at-index nnn-ctx num)
                      "/")))
    (if (file-directory-p file)
        (pop-to-buffer-same-window
         (nnn-goto-dir file))
      (find-file file))))

(defun nnn-goto-parent-dir ()
  "Go to the parent dir of `current-directory'."
  (interactive)
  (let* ((old-dir-fname (directory-file-name default-directory))
         (dir (file-name-directory old-dir-fname)))
    (pop-to-buffer-same-window (nnn-goto-dir dir))
    (goto-char (point-min))
    (nnn-scroll-to-file (file-name-nondirectory old-dir-fname))))

(defun nnn-scroll-to-file (filename)
  "Scroll point in the current buffer to start on the line containing FILENAME.

FILENAME should be as short as possible, i.e no parent dirs and
no ending slash."
  (goto-char (point-min))
  (forward-line (nnn-get-entry-index nnn-ctx filename)))

(defun nnn ()
  "Open nnn in the defalut directory."
  (interactive)
  (let ((old-buffer-file-name (buffer-file-name))
        (buffer (nnn-goto-dir default-directory)))
    (set-buffer buffer)
    (when old-buffer-file-name
      (nnn-scroll-to-file (file-name-nondirectory old-buffer-file-name)))
    (pop-to-buffer-same-window buffer)))

(defvar nnn-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; TODO
    (evil-define-key* 'normal keymap
      "q" #'nnn-kill-all-buffers
      "l" #'nnn-open-entry
      "h" #'nnn-goto-parent-dir)
    keymap)
  "TODO")

;; TODO make this not interactive. There shouldn't be any way to activate
;; nnn-mode in an already-existing buffer.
(defun nnn-init-buffer ()
  (setq major-mode 'nnn-mode
        mode-name "n³"
        nnn-ctx (nnn-make-context default-directory)
        font-lock-defaults nil)
  (nnn-populate-context nnn-ctx)
  (erase-buffer)
  (insert (nnn-ls nnn-ctx))
  (run-mode-hooks 'special-mode-hook)
  (use-local-map nnn-mode-map)
  (nnn-fontify-region (point-min) (progn
                                    (goto-char (point-min))
                                    (end-of-line (1+ (window-height)))
                                    (prog1 (point)
                                      (goto-char (1+ (point-min))))))

  (setq buffer-read-only t))

(define-derived-mode nnn-mode special-mode "n³"
  "TODO"
  :group 'nnn
  ;; TODO set `revert-buffer-function'
  (setq nnn-ctx (nnn-make-context default-directory))
  (setq font-lock-defaults nil)
  (nnn-populate-context nnn-ctx)
  (erase-buffer)
  (insert (nnn-ls nnn-ctx))
  (goto-char (1+ (point-min)))

  ;; TODO these advanced ways to fontify the buffer that I copied from
  ;; tree-sitter.el don't really work
  ;; (setq-local
  ;;  font-lock-fontify-region-function #'nnn-fontify-region
  ;;  font-lock-fontify-buffer-function (lambda ()
  ;;                                      (nnn-fontify-region (point-min) (point-max)))
  ;;  font-lock-unfontify-region-function nil)
  ;; (font-lock-refresh-defaults)
  ;; (unless font-lock-set-defaults
  ;;   (font-lock-turn-on-thing-lock))
  (nnn-fontify-region (point-min) (point-max))
  )

(add-load-path! default-directory)
(provide 'nnn)
;;; nnn.el ends here
