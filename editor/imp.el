(require 'websocket)

(defvar imp-ws nil)
(defvar imp-timer nil)
(defvar imp-buffer nil)

(defun imp-connect ()
  (interactive)
  (when imp-ws (websocket-close imp-ws))
  (message "Connecting to Imp...")
  (setq imp-ws (websocket-open
                "ws://127.0.0.1:8081"
                :on-message (lambda (_websocket frame)
                              (message "imp frame: %S" (websocket-frame-text frame)))
                :on-close (lambda (_websocket)
                            (message "imp socket closed")
                            (setq imp-ws nil)
                            (cancel-timer imp-timer)
                            (setq imp-timer nil)
                            (setq imp-buffer nil))))
  (setq imp-timer (run-with-idle-timer 0.1 t 'imp-send-code-buffer (current-buffer)))
  (setq imp-buffer (current-buffer))
  (message "Connected to Imp")
  nil)

(defun imp-send-code (code)
  (websocket-send-text imp-ws (json-encode (list (cons "code" code)))))

(defun imp-send-code-buffer (buffer)
  (with-current-buffer imp-buffer
    (imp-send-code (buffer-string))))

;; (cancel-function-timers 'imp-send-code-buffer)
;; (setq timer-idle-list (car timer-idle-list))
