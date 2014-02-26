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


(defgroup global-emacs nil
  "Unify multiple emacsen by sharing killrings and showing if
some are busy.")

(defcustom global-emacs-idle-time 2
  "Time to wait until an idle emacs is declared as idle.
Lower values are more precise but less friendly for the CPU."
  :group 'global-emacs
  :type 'integer)

(defcustom global-emacs-process-file "~/.emacs.d/global-emacs-emacsen"
  "File which saves emacs working states."
  :group 'global-emacs
  :type 'string)

(defcustom global-emacs-kill-ring-file "~/.emacs.d/global-emacs-kills.el"
  "File which saves emacs kill-ring."
  :group 'global-emacs
  :type 'string)

(defface global-emacs-mode-line
    '((t ()))
  "Face used for the mode line (this is incompatibel
with smart-mode-line)."
  :group 'global-emacs)

(defvar global-emacs-buffer-message nil)
(defvar global-emacs-emacsen nil)
(defvar global-emacs-idle nil)
(defvar global-emacs-mode-line-message nil)
(defvar global-emacs-kill-ring-tmp nil)
(defvar global-emacs-kill-ring-to-be-read nil)
(defvar global-emacs-mode-line-lock nil)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun global-emacs-change-count (counter change)
  "Take counter and change number of processes accordingly.
Take change and set global-emacs-idle to change."
  (setq global-emacs-buffer-message (current-message))
  (setq global-emacs-idle change)
  (setq global-emacs-emacsen (+ counter (string-to-number
                              (get-string-from-file global-emacs-process-file))))
  (write-region (number-to-string global-emacs-emacsen)
   nil global-emacs-process-file)
  (global-emacs-update-mode-line nil)
  (message global-emacs-buffer-message))

(defun global-emacs-update-mode-line (special-msg)
  "Updates the message of the modeline."
  (when special-msg 
    (setq global-emacs-mode-line-lock t)
    (setq global-emacs-mode-line-message (format "  [%s] " special-msg))
    (run-with-timer 4 nil '(lambda () (setq global-emacs-mode-line-lock nil)
                                 (global-emacs-update-mode-line nil))))
  (when (not global-emacs-mode-line-lock)
    (if (= global-emacs-emacsen 1)
        (setq global-emacs-mode-line-message "  [one emacs busy] ")
      (if (= global-emacs-emacsen 0) 
          (setq global-emacs-mode-line-message "  [no emacs works] ")
        (setq global-emacs-mode-line-message (format "  [%s emacsen busy] " global-emacs-emacsen))))))

(defun global-emacs-kill-ring-save ()
  "Exchanges the kill-ring between all emacsen."
  (interactive)
  (when (not (equal global-emacs-kill-ring-tmp kill-ring))
    (write-region (concat "(setq kill-ring '" (format "%S" (mapcar 'substring-no-properties kill-ring)) ")") nil global-emacs-kill-ring-file nil 'ignore)
    (setq global-emacs-kill-ring-tmp kill-ring)
    (global-emacs-update-mode-line "save kill-ring")) )

(defun global-emacs-kill-ring-read ()
  "Reads shared kill-ring."
  (interactive)
  (load global-emacs-kill-ring-file t t)
  (when (not (equal global-emacs-kill-ring-tmp kill-ring))
    (setq global-emacs-kill-ring-tmp kill-ring)
    (global-emacs-update-mode-line "load kill-ring")))

(define-minor-mode global-emacs-mode
  "Notify mode-line that an async process run."
  :group 'global-emacs
  :global t
  :lighter (:eval (propertize global-emacs-mode-line-message 'face 'global-emacs-mode-line))
  
  (if (file-exists-p global-emacs-process-file)
      (global-emacs-change-count 1 nil)
    (write-region "1" nil global-emacs-process-file))    
  (add-hook 'pre-command-hook '(lambda () (when global-emacs-idle (global-emacs-change-count 1 nil))
                                 (when global-emacs-kill-ring-to-be-read 
                                   (setq global-emacs-kill-ring-to-be-read nil)
                                   (global-emacs-kill-ring-read))))
  (add-hook 'kill-emacs-hook '(lambda () (when (not global-emacs-idle) (global-emacs-change-count -1 t))))
  (run-with-idle-timer global-emacs-idle-time t '(lambda () (when (not global-emacs-idle) (global-emacs-change-count -1 t))
                                                   (global-emacs-kill-ring-save)
                                                   (setq global-emacs-kill-ring-to-be-read t)))
  (run-with-idle-timer 15 t '(lambda () (setq global-emacs-mode-line-message "  [ disconnected ] "))))

(provide 'global-emacs)

;;; global-emacs.el ends here
