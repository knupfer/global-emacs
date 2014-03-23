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

(defcustom ge-idle-time 2
  "Time to wait until an idle emacs is declared as idle.
Lower values are more precise but less friendly for the CPU."
  :group 'global-emacs
  :type 'integer)

(defcustom ge-process-file "/tmp/ge-emacsen"
  "File which saves emacs working states."
  :group 'global-emacs
  :type 'string)

(defcustom ge-kill-ring-file "~/.emacs.d/ge-kills.el"
  "File which saves emacs kill-ring."
  :group 'global-emacs
  :type 'string)

(defcustom ge-kill-ring-use-properties-p t
  "Use string properties in kill-ring.
Disabling this will result in less CPU and IO."
  :group 'global-emacs
  :type 'boolean)

(defcustom ge-auto-redraw-p t
  "Automatically redraws the display after mode-line change.
This is over vnc quite costly."
  :group 'global-emacs
  :type 'boolean)

(defcustom ge-share-kill-ring-p t
  "Share kill-ring with all emacsen."
  :group 'global-emacs
  :type 'boolean)

(defface ge-mode-line
  '((t ()))
  "Face used for the mode line (this is incompatibel
with smart-mode-line)."
  :group 'global-emacs)

(defvar ge-emacsen 0)
(defvar ge-idle-p nil)
(defvar ge-mode-line-message nil)
(defvar ge-kill-ring-tmp nil)
(defvar ge-read-kill-ring-p nil)
(defvar ge-mode-line-lock-p nil)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer (insert-file-contents filePath)
                    (buffer-string)))

(defun ge-change-count (counter change)
  "Take counter and change number of processes accordingly.
Take change and set ge-idle-p to change."
  (setq ge-idle-p change)
  (setq ge-emacsen (+ counter
                      (string-to-number (get-string-from-file ge-process-file))))
  (write-region (number-to-string ge-emacsen) nil ge-process-file nil 'ignore)
  (ge-update-mode-line nil))

(defun ge-update-mode-line (special-msg)
  "Updates the message of the modeline."
  (when special-msg
    (setq ge-mode-line-lock-p t)
    (setq ge-mode-line-message (format "  [%s] " special-msg))
    (run-with-timer 4 nil '(lambda () (setq ge-mode-line-lock-p nil)
                             (ge-update-mode-line nil))))
  (when (not ge-mode-line-lock-p)
    (if (= ge-emacsen 1) (setq ge-mode-line-message "  [one emacs busy] ")
      (if (= ge-emacsen 0) (setq ge-mode-line-message "  [no emacs works] ")
        (setq ge-mode-line-message (format "  [%s emacsen busy] " ge-emacsen)))))
  (when ge-auto-redraw-p (redraw-modeline)))

(defun ge-kill-ring-save ()
  "Exchanges the kill-ring between all emacsen."
  (when (not (equal ge-kill-ring-tmp kill-ring))
    (write-region (concat "(setq kill-ring '" 
                          (if ge-kill-ring-use-properties-p 
                              (format "%S" kill-ring) 
                            (format "%S"
                                    (mapcar 'substring-no-properties kill-ring)))
                          ")")
                  nil ge-kill-ring-file nil 'ignore)
    (setq ge-kill-ring-tmp kill-ring)
    (ge-update-mode-line "save kill-ring")))

(defun ge-kill-ring-read ()
  "Reads shared kill-ring."
  (load ge-kill-ring-file t t)
  (when (not (equal ge-kill-ring-tmp kill-ring))
    (setq ge-kill-ring-tmp kill-ring)
    (ge-update-mode-line "load kill-ring")))

(define-minor-mode global-emacs-mode
  "Notify mode-line that an async process run."
  :group 'global-emacs
  :global t
  :lighter (:eval (propertize ge-mode-line-message 'face 'ge-mode-line))
  
  (when ge-share-kill-ring-p (ge-kill-ring-read))
  (if (file-exists-p ge-process-file)
      (ge-change-count 1 nil)
    (write-region "1" nil ge-process-file))    
  (add-hook 'pre-command-hook '(lambda () (when ge-idle-p (ge-change-count 1 nil))
                                 (when (and ge-read-kill-ring-p
                                            ge-share-kill-ring-p)
                                   (setq ge-read-kill-ring-p nil)
                                   (ge-kill-ring-read))))
  (add-hook 'kill-emacs-hook '(lambda () (when (not ge-idle-p)
                                           (ge-change-count -1 t))))
  (run-with-idle-timer ge-idle-time t '(lambda () (when (not ge-idle-p)
                                                    (ge-change-count -1 t))
                                         (when ge-share-kill-ring-p
                                           (ge-kill-ring-save)
                                           (setq ge-read-kill-ring-p t))))
  (run-with-idle-timer 15 t '(lambda () (setq ge-mode-line-message
                                              "  [ disconnected ] ")))) 

(provide 'global-emacs)

;;; global-emacs.el ends here 
