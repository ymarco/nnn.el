;;; fired.el --- TODO -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Yoav Marco
;;
;; Author: Yoav Marco <http://github/ymarco>
;; Maintainer: Yoav Marco <yoavm448@gmail.com>
;; Created: October 30, 2020
;; Modified: October 30, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/ymarco/fired
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
  (error "Fired needs module support.  Please compile Emacs with the --with-modules option!"))

(define-derived-mode fired-mode special-mode "Fired"
  "TODO"
  :group 'fired)

(add-load-path! default-directory)
(provide 'fired)
;;; fired.el ends here
