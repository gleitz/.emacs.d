;; I would like to modify this function.
;; Right now it takes the current region and passes it to `llm`.
;; If a prefix argument is used, I would like to prompt for a message
;; and pass that message as a system message like this `llm -s "<PROMPT>"
;; (defun llm-query-and-insert (start end command)
;;   (interactive
;;    (let* ((prompt "Enter system message: ")
;;           (system-message (if current-prefix-arg "" (shell-quote-argument (read-string prompt))))
;;           (message (format "llm -m gpt-3.5-turbo -s \"%s\"" system-message))
;;           (command (format "llm -m gpt-3.5-turbo -s \"%s\"" system-message)))
;;      (if (use-region-p)
;;          (list (region-beginning) (region-end) command)
;;        (list (line-beginning-position) (line-end-position) command))))
;;   (message command)
;;   (let ((response (shell-command-on-region-to-string start end command)))
;;     (kill-new response)
;;     (save-excursion
;;       (end-of-region-or-visual-line)
;;       (newline)
;;       (insert response))))

;; (defun llm-gpt4-query-and-insert (start end command)
;;   (interactive
;;    (let* ((prompt "Enter system message: ")
;;           (system-message (if current-prefix-arg "" (shell-quote-argument (read-string prompt))))
;;           (message (format "llm -m gpt-4 -s \"%s\"" system-message))
;;           (command (format "llm -m gpt-4 -s \"%s\"" system-message)))
;;      (if (use-region-p)
;;          (list (region-beginning) (region-end) command)
;;        (list (line-beginning-position) (line-end-position) command))))
;;   (message command)
;;   (let ((response (shell-command-on-region-to-string start end command)))
;;     (kill-new response)
;;     (save-excursion
;;       (end-of-region-or-visual-line)
;;       (newline)
;;       (insert response))))

;; (defun llm-orca-query-and-insert (start end command)
;;   (interactive
;;    (let* ((prompt "Enter system message: ")
;;           (system-message (if current-prefix-arg "" (shell-quote-argument (read-string prompt))))
;;           (message (format "llm -m orca-2-13b -s \"%s\"" system-message))
;;           (command (format "llm -m orca-2-13b -s \"%s\"" system-message)))
;;      (if (use-region-p)
;;          (list (region-beginning) (region-end) command)
;;        (list (line-beginning-position) (line-end-position) command))))
;;   (message command)
;;   (let ((response (shell-command-on-region-to-string start end command)))
;;     (kill-new response)
;;     (save-excursion
;;       (end-of-region-or-visual-line)
;;       (newline)
;;       (insert response))))

(defun llm-query-and-insert (start end command model) ;; model is unused here
  (message command)
  (let ((response (shell-command-on-region-to-string start end command)))
    (kill-new response)
    (save-excursion
      (end-of-region-or-visual-line)
      (newline)
      (insert response))))

(defun llm-gpt35-turbo-query-and-insert ()
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p) (region-end) (line-end-position)))
         (prompt "Enter system message: ")
         (system-message (if current-prefix-arg "" (shell-quote-argument (read-string prompt))))
         (command (format "llm -m gpt-3.5-turbo -s \"%s\"" system-message)))
    (llm-query-and-insert start end command "gpt-3.5-turbo")))

(defun llm-gpt4-query-and-insert ()
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p) (region-end) (line-end-position)))
         (prompt "Enter system message: ")
         (system-message (if current-prefix-arg "" (shell-quote-argument (read-string prompt))))
         (command (format "llm -m gpt-4 -s \"%s\"" system-message)))
    (llm-query-and-insert start end command "gpt-4")))

;; doesn't work, currently
(defun llm-orca-2-13b-query-and-insert ()
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p) (region-end) (line-end-position)))
         (prompt "Enter system message: ")
         (system-message (if current-prefix-arg "" (shell-quote-argument (read-string prompt))))
         (command (format "llm -m orca-2-13b -s \"%s\"" system-message)))
    (llm-query-and-insert start end command "orca-2-13b")))

(defun shell-command-with-region-arg (command beginning end)
  "Prompt for a shell COMMAND, and use the region as an argument.

Buffer text from BEGINNING to END is passed as a single argument to COMMAND."
  (interactive (list (read-shell-command "Shell command: ")
                     (region-beginning)
                     (region-end)))
  (let ((region (buffer-substring-no-properties beginning end)))
    ;; (message (concat command " " (shell-quote-argument region)))
    (shell-command (concat command " " (shell-quote-argument region)) standard-output)))

(defun shell-command-with-region-arg-to-string (start end command)
  (with-output-to-string
    (shell-command-with-region-arg command start end)))

(defun llm-deepseek-coder-6.7b-query-and-insert ()
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p) (region-end) (line-end-position)))
         (prompt "Enter system message: ")
         (system-message (if current-prefix-arg "" (shell-quote-argument (read-string prompt))))
         ;; (command "llm -m gguf -o path /Users/gleitz/.cache/lm-studio/models/TheBloke/deepseek-coder-6.7B-instruct-GGUF/deepseek-coder-6.7b-instruct.Q5_K_M.gguf -s 'You are an AI programming assistant, utilizing the Deepseek Coder model, developed by Deepseek Company, and you only answer questions related to computer science. For politically sensitive questions, security and privacy issues, and other non-computer science questions, you will refuse to answer.'"))
         (command (format "llm -m gguf -o path /Users/gleitz/.cache/lm-studio/models/TheBloke/deepseek-coder-6.7B-instruct-GGUF/deepseek-coder-6.7b-instruct.Q5_K_M.gguf -t deepseek -p prompt \"%s\" -p code" system-message)))
    ;; (llm-query-and-insert start end command "deepseek-coder-6.7b")
    (message command)
    (let ((response (shell-command-with-region-arg-to-string start end command)))
      (kill-new response)
      (save-excursion
        (end-of-region-or-visual-line)
        (newline)
        (insert response)))))

;; Whisper
(require 'whisper)

;; whisper microphone setup
(defun rk/get-ffmpeg-device ()
  "Gets the list of devices available to ffmpeg.
The output of the ffmpeg command is pretty messy, e.g.
  [AVFoundation indev @ 0x7f867f004580] AVFoundation video devices:
  [AVFoundation indev @ 0x7f867f004580] [0] FaceTime HD Camera (Built-in)
  [AVFoundation indev @ 0x7f867f004580] AVFoundation audio devices:
  [AVFoundation indev @ 0x7f867f004580] [0] Cam Link 4K
  [AVFoundation indev @ 0x7f867f004580] [1] MacBook Pro Microphone
so we need to parse it to get the list of devices.
The return value contains two lists, one for video devices and one for audio devices.
Each list contains a list of cons cells, where the car is the device number and the cdr is the device name."
  (unless (string-equal system-type "darwin")
    (error "This function is currently only supported on macOS"))

  (let ((lines (string-split (shell-command-to-string "ffmpeg -list_devices true -f avfoundation -i dummy || true") "\n")))
    (cl-loop with at-video-devices = nil
             with at-audio-devices = nil
             with video-devices = nil
             with audio-devices = nil
             for line in lines
             when (string-match "AVFoundation video devices:" line)
             do (setq at-video-devices t
                      at-audio-devices nil)
             when (string-match "AVFoundation audio devices:" line)
             do (setq at-audio-devices t
                      at-video-devices nil)
             when (and at-video-devices
                       (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
             do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) video-devices)
             when (and at-audio-devices
                       (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
             do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) audio-devices)
             finally return (list (nreverse video-devices) (nreverse audio-devices)))))

(defun rk/find-device-matching (string type)
  "Get the devices from `rk/get-ffmpeg-device' and look for a device
matching `STRING'. `TYPE' can be :video or :audio."
  (let* ((devices (rk/get-ffmpeg-device))
         (device-list (if (eq type :video)
                          (car devices)
                        (cadr devices))))
    (cl-loop for device in device-list
             when (string-match-p string (cdr device))
             return (car device))))

(defcustom rk/default-audio-device nil
  "The default audio device to use for whisper.el and outher audio processes."
  :type 'string)

(defun rk/select-default-audio-device (&optional device-name)
  "Interactively select an audio device to use for whisper.el and other audio processes.
If `DEVICE-NAME' is provided, it will be used instead of prompting the user."
  (interactive)
  (let* ((audio-devices (cadr (rk/get-ffmpeg-device)))
         (indexes (mapcar #'car audio-devices))
         (names (mapcar #'cdr audio-devices))
         (name (or device-name (completing-read "Select audio device: " names nil t))))
    (setq rk/default-audio-device (rk/find-device-matching name :audio))
    (when (boundp 'whisper--ffmpeg-input-device)
      (setq whisper--ffmpeg-input-device (format ":%s" rk/default-audio-device)))))

(when (string= (system-name) "Repli-Benjamin-Gleitzman")
  (setq whisper-model "medium.en")
  (setq whisper--ffmpeg-input-device ":1"))

(defvar llm-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'copilot-mode)
    (define-key map (kbd "d") #'copilot-diagnose)
    (define-key map (kbd "w") #'whisper-run)
    (define-key map (kbd "d") #'llm-deepseek-coder-6.7b-query-and-insert)
    (define-key map (kbd "l") #'llm-gpt35-turbo-query-and-insert)
    (define-key map (kbd "o") #'llm-orca-2-13b-query-and-insert)
    (define-key map (kbd "4") #'llm-gpt4-query-and-insert)
    map)
  "Keymap for LLM commands.")
(fset 'llm-command-map llm-command-map)
(define-key projectile-mode-map (kbd "C-c l") 'llm-command-map)

(provide 'setup-llms)
