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
