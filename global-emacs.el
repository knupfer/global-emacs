;;; global-emacs.el --- Unify multiple emacs instances

;; Copyright (C) 2014 Florian Knupfer

;; Authors: Florian Knupfer <fknupfer@gmail.com>

;; Version: 0.1
;; Keywords: concurrent async
;; X-URL: https://github.com/knupfer/global-emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use it, put this in your .emacs:
;;
;;     (require 'global-emacs)
;;

;;; Code:


(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defvar knu/idle t)
(when (not (file-exists-p "~/global-emacs"))
      (write-region "0" nil "~/global-emacs"))

(add-hook 'pre-command-hook '(lambda () (when (equal (eval knu/idle) t)
                                     (setq knu/idle nil)
                                     (write-region 
                                      (number-to-string
                                       (+
                                        (string-to-number
                                         (get-string-from-file "~/global-emacs"))
                                        1))
                                      nil "~/global-emacs")
                                     )))

(add-hook 'kill-emacs-hook '(lambda () (when (equal (eval knu/idle) nil)
                                    (setq knu/idle "exit")
                                    (write-region 
                                     (number-to-string
                                      (-
                                       (string-to-number
                                        (get-string-from-file "~/global-emacs"))
                                       1))
                                     nil "~/global-emacs")
                                    )))

(run-with-idle-timer 10 t '(lambda () (when (equal (eval knu/idle) nil)
                                   (setq knu/idle t)
                                     (write-region 
                                      (number-to-string
                                       (-
                                        (string-to-number
                                         (get-string-from-file "~/global-emacs"))
                                        1))
                                      nil "~/global-emacs")
                                     )))

(define-minor-mode dired-async-mode
    "Notify mode-line that an async process run."
  :group 'dired-async
  :global t
  :lighter (:eval (propertize (format " [%s Async job(s) running]"
                                      (+ (length (dired-async-processes))
                                         (string-to-number (get-string-from-file "~/global-emacs"))))
                              'face 'dired-async-mode-message))
  (unless dired-async-mode
    (let ((visible-bell t)) (ding))))

(provide 'global-emacs)

;;; global-emacs.el ends here
