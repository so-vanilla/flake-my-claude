;;; claude-code-modeline.el --- Modeline display for Claude Code sessions -*- lexical-binding: t; -*-

;; Author: so-vanilla
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience

;;; Commentary:

;; Claude Code の statusline 機能が書き出す中間ファイルを読み取り、
;; claude-code-ide のセッションバッファの modeline にセッション情報
;; （モデル名、コンテキスト使用率、コスト、セッション時間）を表示する
;; グローバル minor-mode。

;;; Code:

(require 'json)

(declare-function claude-code-ide--get-buffer-name "claude-code-ide")
(defvar claude-code-ide--processes)

;;;; Customization

(defgroup claude-code-modeline nil
  "Modeline display for Claude Code sessions."
  :group 'claude-code-ide
  :prefix "claude-code-modeline-")

(defcustom claude-code-modeline-data-directory "/tmp/claude-code/"
  "Directory where statusline.sh writes intermediate JSON files."
  :type 'directory)

(defcustom claude-code-modeline-update-interval 3.0
  "Polling interval in seconds for reading intermediate files."
  :type 'number)

(defcustom claude-code-modeline-format-string " %m │ Ctx:%c%% │ $%d │ %t "
  "Format string for modeline display.
%m = model name, %c = context used percentage,
%d = cost in USD, %t = session duration."
  :type 'string)

;;;; Faces

(defface claude-code-modeline-normal
  '((t :inherit mode-line))
  "Face for normal context usage (< 75%).")

(defface claude-code-modeline-context-warning
  '((t :inherit warning))
  "Face for warning context usage (75% - 89%).")

(defface claude-code-modeline-context-critical
  '((t :inherit error))
  "Face for critical context usage (>= 90%).")

;;;; Internal variables

(defvar claude-code-modeline--data-cache (make-hash-table :test 'equal)
  "Cache of parsed JSON data, keyed by project directory.")

(defvar claude-code-modeline--timer nil
  "Timer for periodic data file polling.")

(defvar claude-code-modeline--original-mode-line-formats (make-hash-table :test 'equal)
  "Original mode-line-format values, keyed by buffer name.")

;;;; Data reading

(defun claude-code-modeline--data-file (project-dir)
  "Return the intermediate data file path for PROJECT-DIR."
  (expand-file-name
   (concat (md5 (directory-file-name project-dir)) ".json")
   claude-code-modeline-data-directory))

(defun claude-code-modeline--read-data (project-dir)
  "Read and parse the intermediate JSON file for PROJECT-DIR.
Returns parsed data as alist, or nil on failure."
  (let ((file (claude-code-modeline--data-file project-dir)))
    (when (file-readable-p file)
      (condition-case nil
          (let ((json-object-type 'alist)
                (json-key-type 'symbol))
            (json-read-file file))
        (error nil)))))

(defun claude-code-modeline--poll ()
  "Poll all active sessions and update the data cache."
  (when (and (boundp 'claude-code-ide--processes)
             (hash-table-p claude-code-ide--processes))
    (maphash
     (lambda (dir _process)
       (when-let ((data (claude-code-modeline--read-data dir)))
         (puthash dir data claude-code-modeline--data-cache)))
     claude-code-ide--processes))
  (claude-code-modeline--update-all-buffers)
  (force-mode-line-update t))

;;;; Formatting

(defun claude-code-modeline--format-duration (ms)
  "Format MS milliseconds as human-readable duration string."
  (let* ((total-seconds (/ ms 1000))
         (hours (/ total-seconds 3600))
         (minutes (/ (mod total-seconds 3600) 60))
         (seconds (mod total-seconds 60)))
    (cond
     ((>= hours 1) (format "%dh%02dm" hours minutes))
     ((>= minutes 1) (format "%dm%02ds" minutes seconds))
     (t (format "%ds" seconds)))))

(defun claude-code-modeline--format-cost (cost)
  "Format COST in USD for display."
  (if (< cost 0.01)
      (format "%.4f" cost)
    (format "%.2f" cost)))

(defun claude-code-modeline--context-face (percentage)
  "Return the appropriate face for context PERCENTAGE."
  (cond
   ((>= percentage 90) 'claude-code-modeline-context-critical)
   ((>= percentage 75) 'claude-code-modeline-context-warning)
   (t 'claude-code-modeline-normal)))

(defun claude-code-modeline--build-string (model context-used cost duration-ms)
  "Build modeline string from MODEL, CONTEXT-USED, COST, DURATION-MS."
  (let ((result claude-code-modeline-format-string))
    (setq result (string-replace "%m" (format "%s" model) result))
    (setq result (string-replace "%c" (format "%.0f" (float context-used)) result))
    (setq result (string-replace "%d" (claude-code-modeline--format-cost cost) result))
    (setq result (string-replace "%t" (claude-code-modeline--format-duration duration-ms) result))
    result))

(defun claude-code-modeline--format-data (data)
  "Format DATA alist according to `claude-code-modeline-format-string'."
  (let* ((model (or (alist-get 'model data) "?"))
         (context-used (or (alist-get 'context_used data) 0))
         (cost (or (alist-get 'cost_usd data) 0))
         (duration-ms (or (alist-get 'duration_ms data) 0))
         (face (claude-code-modeline--context-face context-used))
         (formatted (claude-code-modeline--build-string
                     model context-used cost duration-ms)))
    (propertize formatted 'face face)))

;;;; Project directory detection

(defun claude-code-modeline--buffer-project-dir (buffer)
  "Find the project directory associated with BUFFER.
Walk `claude-code-ide--processes' and match buffer names."
  (when (and (boundp 'claude-code-ide--processes)
             (hash-table-p claude-code-ide--processes))
    (let ((buf-name (buffer-name buffer))
          (result nil))
      (maphash
       (lambda (dir _process)
         (when (string= buf-name (claude-code-ide--get-buffer-name dir))
           (setq result dir)))
       claude-code-ide--processes)
      result)))

;;;; Modeline construction

(defun claude-code-modeline--mode-line-format (data)
  "Build a complete mode-line-format list showing DATA."
  (list (claude-code-modeline--format-data data)))

(defun claude-code-modeline--apply-to-buffer (buffer)
  "Apply claude-code modeline to BUFFER if it has associated session data."
  (when (buffer-live-p buffer)
    (when-let ((dir (claude-code-modeline--buffer-project-dir buffer)))
      (when-let ((data (gethash dir claude-code-modeline--data-cache)))
        (with-current-buffer buffer
          (unless (gethash (buffer-name buffer) claude-code-modeline--original-mode-line-formats)
            (puthash (buffer-name buffer) mode-line-format
                     claude-code-modeline--original-mode-line-formats))
          (if (featurep 'doom-modeline)
              (claude-code-modeline--apply-doom buffer)
            (setq-local mode-line-format
                        (claude-code-modeline--mode-line-format data))))))))

(defun claude-code-modeline--restore-buffer (buffer)
  "Restore original modeline for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let ((original (gethash (buffer-name buffer)
                                    claude-code-modeline--original-mode-line-formats)))
        (setq-local mode-line-format original)
        (remhash (buffer-name buffer) claude-code-modeline--original-mode-line-formats)))))

(defun claude-code-modeline--update-all-buffers ()
  "Update modeline for all Claude Code session buffers."
  (dolist (buffer (buffer-list))
    (claude-code-modeline--apply-to-buffer buffer)))

;;;; doom-modeline integration

(with-eval-after-load 'doom-modeline
  (when (fboundp 'doom-modeline-def-segment)
    (doom-modeline-def-segment claude-code-info
      "Claude Code session information segment."
      (when-let ((dir (claude-code-modeline--buffer-project-dir (current-buffer))))
        (if-let ((data (gethash dir claude-code-modeline--data-cache)))
            (claude-code-modeline--format-data data)
          (propertize " Claude Code ... " 'face 'claude-code-modeline-normal)))))

  (when (fboundp 'doom-modeline-def-modeline)
    (doom-modeline-def-modeline 'claude-code
      '(bar claude-code-info)
      '())))

(defun claude-code-modeline--apply-doom (buffer)
  "Apply doom-modeline claude-code modeline to BUFFER."
  (with-current-buffer buffer
    (when (fboundp 'doom-modeline-set-modeline)
      (doom-modeline-set-modeline 'claude-code))))

;;;; Advice for session start

(defun claude-code-modeline--after-start-session (&rest _args)
  "Advice function to apply modeline after a Claude Code session starts.
Runs with a short delay to allow the buffer to be fully set up."
  (run-at-time 1.0 nil #'claude-code-modeline--update-all-buffers))

;;;; Minor mode

;;;###autoload
(define-minor-mode claude-code-modeline-mode
  "Global minor mode to display Claude Code session info in modeline."
  :global t
  :lighter nil
  (if claude-code-modeline-mode
      (progn
        (setq claude-code-modeline--timer
              (run-with-timer 0 claude-code-modeline-update-interval
                              #'claude-code-modeline--poll))
        (with-eval-after-load 'claude-code-ide
          (advice-add 'claude-code-ide--start-session :after
                      #'claude-code-modeline--after-start-session))
        (claude-code-modeline--update-all-buffers))
    (when claude-code-modeline--timer
      (cancel-timer claude-code-modeline--timer)
      (setq claude-code-modeline--timer nil))
    (when (featurep 'claude-code-ide)
      (advice-remove 'claude-code-ide--start-session
                     #'claude-code-modeline--after-start-session))
    (dolist (buffer (buffer-list))
      (claude-code-modeline--restore-buffer buffer))
    (clrhash claude-code-modeline--data-cache)
    (clrhash claude-code-modeline--original-mode-line-formats)))

(provide 'claude-code-modeline)
;;; claude-code-modeline.el ends here
