
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defvar knu/foobar 1)
(defvar knu/idle t)
;(run-with-idle-timer 1 t '(lambda () (setq knu/foobar (+ (eval knu/foobar) 1))))
;(add-hook 'pre-command-hook '(lambda () (setq knu/idle nil)))
;(add-hook 'post-command-hook '(lambda () (setq knu/idle t)))


;(add-hook 'pre-command-hook '(lambda () (setq knu/idle nil)))
;(add-hook 'post-command-hook '(lambda () (append-to-file (number-to-string (eval knu/foobar)) nil "~/ubl")))

(when (file-exists-p "~/global-emacs")
      (write-region "0" nil "~/global-emacs"))

(add-hook 'post-command-hook '(lambda () (write-region 
                                     (number-to-string
                                      (+
                                       (string-to-number
                                        (get-string-from-file "~/global-emacs"))
                                       1))
                                     nil "~/global-emacs")))
