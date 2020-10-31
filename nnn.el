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

(load! "nnn-module.so")

(defvar-local nnn-ctx nil
  "nnn directory context for current buffer")

(define-derived-mode nnn-mode special-mode "nnn"
  "TODO"
  :group 'nnn
  (let ((buffer (get-buffer-create "* nnn")))
    (with-current-buffer buffer
      (setq nnn-ctx (nnn-make-context default-directory))
      (nnn-populate-context nnn-ctx)
      (erase-buffer)
      (insert (nnn-ls nnn-ctx)))
    (pop-to-buffer-same-window buffer)))

(add-load-path! default-directory)
(provide 'nnn)
;;; nnn.el ends here
