;;; copilot-sdk.el --- Elisp client for the GitHub Copilot SDK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edd Wilder-James
;; Author: Edd Wilder-James <https://github.com/ewilderj>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (jsonrpc "1.0.25"))
;; Keywords: tools, ai
;; URL: https://github.com/ewilderj/emacs-copilot-sdk

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Thin elisp client for the GitHub Copilot SDK.  Talks directly to the
;; Copilot CLI (Node.js) over JSON-RPC 2.0 with Content-Length framing —
;; the same protocol eglot uses for LSP.  No Python bridge needed.
;;
;; This package provides the SDK primitives:
;; - Connection management (start/stop the CLI subprocess)
;; - Session lifecycle (create, resume, destroy, list)
;; - Tool definition and dispatch
;; - Permission and user-input request handling
;; - Session event notifications
;;
;; It is intentionally unopinionated about UI or Emacs integration.
;; Build your application on top of this (see `emacs-copilot' for an
;; example of a full Emacs assistant built on copilot-sdk).
;;
;; Usage:
;;   (require 'copilot-sdk)
;;
;;   ;; Start connection
;;   (copilot-sdk-start)
;;
;;   ;; Define a tool
;;   (copilot-sdk-define-tool
;;    "get_time" "Get the current time."
;;    (lambda (_args)
;;      `(:textResultForLlm ,(current-time-string)
;;        :resultType "success")))
;;
;;   ;; Create a session
;;   (copilot-sdk-create-session
;;    :model "claude-sonnet-4-5"
;;    :tools (copilot-sdk-tool-defs)
;;    :system-message "You are a helpful assistant.")
;;
;;   ;; Send a message
;;   (copilot-sdk-send "current-session-id" "What time is it?"
;;    :on-message (lambda (text) (message "Response: %s" text))
;;    :on-idle (lambda () (message "Done.")))

;;; Code:

(require 'jsonrpc)
(require 'cl-lib)
(require 'json)

;;;; --- Customization ---

(defgroup copilot-sdk nil
  "Copilot SDK client."
  :group 'tools
  :prefix "copilot-sdk-")

(defcustom copilot-sdk-cli-path (or (getenv "COPILOT_CLI_PATH") "copilot")
  "Path to the Copilot CLI binary."
  :type 'string)

(defcustom copilot-sdk-log-level "info"
  "Log level for the CLI subprocess."
  :type '(choice (const "none") (const "error") (const "warning")
                 (const "info") (const "debug") (const "all")))

;;;; --- Internal State ---

(defvar copilot-sdk--connection nil
  "The variable `jsonrpc-process-connection' to the Copilot CLI.")

(defvar copilot-sdk--tool-handlers (make-hash-table :test #'equal)
  "Map of tool-name → handler function.")

(defvar copilot-sdk--tool-defs nil
  "List of tool definition plists registered via `copilot-sdk-define-tool'.")

(defvar copilot-sdk--event-handlers nil
  "Alist of (session-id . handler) for session event dispatch.
Each handler is called with (EVENT-TYPE DATA) where EVENT-TYPE is a
string like \"assistant.message\" and DATA is a plist.")

(defvar copilot-sdk--permission-handler nil
  "Function called for permission requests.
Receives (KIND REQUEST) and should return a result plist.
If nil, all permissions are denied.")

(defvar copilot-sdk--user-input-handler nil
  "Function called for user input requests.
Receives (QUESTION CHOICES ALLOW-FREEFORM) and should return
\\='(:answer STRING :wasFreeform BOOL).
If nil, raises an error.")

(defconst copilot-sdk--protocol-version 2
  "Expected SDK protocol version.")

;;;; --- Connection Management ---

(defun copilot-sdk-start ()
  "Start the Copilot CLI and establish the JSON-RPC connection.
Does nothing if already connected."
  (interactive)
  (when (copilot-sdk-connected-p)
    (user-error "Already connected"))
  (let* ((process-environment
          (append
           (when (getenv "GH_TOKEN")
             (list (format "GH_TOKEN=%s" (getenv "GH_TOKEN"))))
           process-environment))
         (process (make-process
                   :name "copilot-cli"
                   :command (list copilot-sdk-cli-path
                                 "--server"
                                 "--log-level" copilot-sdk-log-level
                                 "--stdio")
                   :connection-type 'pipe
                   :coding 'utf-8-unix
                   :noquery t
                   :sentinel #'copilot-sdk--process-sentinel)))
    (setq copilot-sdk--connection
          (jsonrpc-process-connection
           :name "copilot-sdk"
           :process process
           :request-dispatcher #'copilot-sdk--handle-request
           :notification-dispatcher #'copilot-sdk--handle-notification))
    ;; Verify protocol version
    (let* ((ping (copilot-sdk-ping))
           (version (plist-get ping :protocolVersion)))
      (unless (eql version copilot-sdk--protocol-version)
        (copilot-sdk-stop)
        (error "Copilot SDK protocol mismatch: expected v%d, got v%s"
               copilot-sdk--protocol-version version)))
    (message "Copilot SDK: connected (protocol v%d)"
             copilot-sdk--protocol-version)))

(defun copilot-sdk-stop ()
  "Stop the Copilot CLI connection."
  (interactive)
  (when copilot-sdk--connection
    (ignore-errors
      (when (jsonrpc-running-p copilot-sdk--connection)
        (jsonrpc-shutdown copilot-sdk--connection)))
    (setq copilot-sdk--connection nil)
    (message "Copilot SDK: disconnected.")))

(defun copilot-sdk-connected-p ()
  "Return non-nil if the CLI connection is active."
  (and copilot-sdk--connection
       (jsonrpc-running-p copilot-sdk--connection)))

(defun copilot-sdk-ensure-connected ()
  "Ensure the CLI connection is active, starting if needed."
  (unless (copilot-sdk-connected-p)
    (copilot-sdk-start)))

(defun copilot-sdk--process-sentinel (_process event)
  "Handle CLI process state change described by EVENT."
  (when (and copilot-sdk--connection
             (not (string-match-p "open" event)))
    (setq copilot-sdk--connection nil)
    (message "Copilot SDK: CLI exited (%s)" (string-trim event))))

;;;; --- JSON-RPC Dispatch ---

(defun copilot-sdk--handle-request (_conn method params)
  "Handle incoming JSON-RPC requests from the CLI.
METHOD is a symbol (interned by jsonrpc), PARAMS is a plist."
  (pcase method
    ('tool.call
     (copilot-sdk--dispatch-tool-call params))
    ('permission.request
     (copilot-sdk--dispatch-permission params))
    ('userInput.request
     (copilot-sdk--dispatch-user-input params))
    ('hooks.invoke
     '(:output nil))))

(defun copilot-sdk--handle-notification (_conn method params)
  "Handle incoming notifications from the CLI.
METHOD is the notification method string, PARAMS is a plist."
  (when (string= method "session.event")
    (let* ((session-id (plist-get params :sessionId))
           (event (plist-get params :event))
           (type (plist-get event :type))
           (data (plist-get event :data)))
      (dolist (entry copilot-sdk--event-handlers)
        (when (string= (car entry) session-id)
          (ignore-errors
            (funcall (cdr entry) type data)))))))

;;;; --- Tool Call Dispatch ---

(defun copilot-sdk--dispatch-tool-call (params)
  "Dispatch a tool.call request.  PARAMS is the request plist."
  (let* ((tool-name (plist-get params :toolName))
         (arguments (plist-get params :arguments))
         (handler (gethash tool-name copilot-sdk--tool-handlers)))
    `(:result
      ,(if handler
           (condition-case err
               (funcall handler arguments)
             (error `(:textResultForLlm
                      ,(format "Tool error: %s" (error-message-string err))
                      :resultType "failure")))
         `(:textResultForLlm
           ,(format "Unknown tool: %s" tool-name)
           :resultType "failure")))))

;;;; --- Permission Dispatch ---

(defun copilot-sdk--dispatch-permission (params)
  "Dispatch a permission.request.  PARAMS is the request plist."
  (let* ((req (plist-get params :permissionRequest))
         (kind (plist-get req :kind)))
    `(:result
      ,(if copilot-sdk--permission-handler
           (condition-case _err
               (funcall copilot-sdk--permission-handler kind req)
             (error '(:kind "denied-no-approval-rule-and-could-not-request-from-user")))
         '(:kind "denied-no-approval-rule-and-could-not-request-from-user")))))

;;;; --- User Input Dispatch ---

(defun copilot-sdk--dispatch-user-input (params)
  "Dispatch a userInput.request.  PARAMS is the request plist."
  (if copilot-sdk--user-input-handler
      (funcall copilot-sdk--user-input-handler
               (plist-get params :question)
               (plist-get params :choices)
               (plist-get params :allowFreeform))
    (error "User input requested but no handler registered")))

;;;; --- Tool Definition API ---

(defun copilot-sdk-define-tool (name description handler &optional params-schema)
  "Define a tool with NAME, DESCRIPTION, and HANDLER.
HANDLER is a function receiving an ARGS plist and returning a ToolResult
plist (:textResultForLlm STRING :resultType STRING).
PARAMS-SCHEMA is an optional JSON Schema plist for the tool's parameters."
  (puthash name handler copilot-sdk--tool-handlers)
  ;; Replace existing def if present
  (setq copilot-sdk--tool-defs
        (cl-remove-if (lambda (d) (string= (plist-get d :name) name))
                      copilot-sdk--tool-defs))
  (let ((tool `(:name ,name :description ,description)))
    (when params-schema
      (setq tool (plist-put tool :parameters params-schema)))
    (push tool copilot-sdk--tool-defs))
  name)

(defun copilot-sdk-tool-defs ()
  "Return the current tool definitions as a vector (for JSON serialization)."
  (vconcat copilot-sdk--tool-defs))

(defun copilot-sdk-tool-success (text &rest format-args)
  "Build a successful tool result.
TEXT is the result string; FORMAT-ARGS are passed to `format'."
  (let ((msg (if format-args (apply #'format text format-args) text)))
    `(:textResultForLlm ,msg :resultType "success")))

(defun copilot-sdk-tool-failure (text &rest format-args)
  "Build a failed tool result.
TEXT is the error string; FORMAT-ARGS are passed to `format'."
  (let ((msg (if format-args (apply #'format text format-args) text)))
    `(:textResultForLlm ,msg :resultType "failure")))

(defun copilot-sdk-tool-rejected (text &rest format-args)
  "Build a rejected (permission denied) tool result.
TEXT is the explanation; FORMAT-ARGS are passed to `format'."
  (let ((msg (if format-args (apply #'format text format-args) text)))
    `(:textResultForLlm ,msg :resultType "rejected")))

(defun copilot-sdk-tool-denied (text &rest format-args)
  "Build a denied tool result.
TEXT is the explanation; FORMAT-ARGS are passed to `format'."
  (let ((msg (if format-args (apply #'format text format-args) text)))
    `(:textResultForLlm ,msg :resultType "denied")))

(defun copilot-sdk-clear-tools ()
  "Remove all registered tools."
  (clrhash copilot-sdk--tool-handlers)
  (setq copilot-sdk--tool-defs nil))

;;;; --- Event Handler API ---

(defun copilot-sdk-on (session-id handler)
  "Register an event HANDLER for SESSION-ID.
HANDLER is called with (EVENT-TYPE DATA) for each session event.
Returns an unsubscribe function."
  (let ((entry (cons session-id handler)))
    (push entry copilot-sdk--event-handlers)
    (lambda ()
      (setq copilot-sdk--event-handlers
            (delq entry copilot-sdk--event-handlers)))))

;;;; --- Session API ---

(defun copilot-sdk-create-session (&rest args)
  "Create a new Copilot session.
ARGS is a plist with keys:
  :model        — model string (default: `copilot-sdk-model')
  :tools        — tool defs vector (default: `copilot-sdk-tool-defs')
  :system-message — system prompt string
  :streaming    — t or nil (default nil)
  :skill-directories — list of paths (default \\='(\"/dev/null\"))
  :working-directory — string
Returns the session ID string."
  (copilot-sdk-ensure-connected)
  (let* ((tools (or (plist-get args :tools) (copilot-sdk-tool-defs)))
         (sys-msg (plist-get args :system-message))
         (streaming (plist-get args :streaming))
         (skill-dirs (or (plist-get args :skill-directories) '("/dev/null")))
         (work-dir (plist-get args :working-directory))
         (payload `(:model ,(or (plist-get args :model) "claude-sonnet-4-5")
                    :tools ,tools
                    :requestPermission t
                    :requestUserInput t
                    :streaming ,(if streaming t :json-false)
                    :skillDirectories ,(vconcat skill-dirs))))
    (when sys-msg
      (setq payload (plist-put payload :systemMessage `(:content ,sys-msg))))
    (when work-dir
      (setq payload (plist-put payload :workingDirectory work-dir)))
    (let ((response (jsonrpc-request copilot-sdk--connection
                                      :session.create payload)))
      (plist-get response :sessionId))))

(defun copilot-sdk-resume-session (session-id &rest args)
  "Resume an existing session by SESSION-ID.
ARGS is an optional plist with keys:
  :tools        — tool defs vector
  :streaming    — t or nil
  :skill-directories — list of paths
Returns the (possibly new) session ID, or signals an error."
  (copilot-sdk-ensure-connected)
  (let* ((tools (or (plist-get args :tools) (copilot-sdk-tool-defs)))
         (streaming (plist-get args :streaming))
         (skill-dirs (or (plist-get args :skill-directories) '("/dev/null")))
         (payload `(:sessionId ,session-id
                     :tools ,tools
                     :requestPermission t
                     :requestUserInput t
                     :streaming ,(if streaming t :json-false)
                     :skillDirectories ,(vconcat skill-dirs)))
         (response (jsonrpc-request copilot-sdk--connection
                                     :session.resume payload)))
    (plist-get response :sessionId)))

(defun copilot-sdk-destroy-session (session-id)
  "Destroy session SESSION-ID."
  (copilot-sdk-ensure-connected)
  (jsonrpc-request copilot-sdk--connection
                    :session.destroy `(:sessionId ,session-id)))

(defun copilot-sdk-delete-session (session-id)
  "Permanently delete session SESSION-ID and all its history."
  (copilot-sdk-ensure-connected)
  (jsonrpc-request copilot-sdk--connection
                    :session.delete `(:sessionId ,session-id)))

(defun copilot-sdk-list-sessions ()
  "List all sessions known to the server.
Returns a list of plists with :sessionId, :startTime, :modifiedTime, etc."
  (copilot-sdk-ensure-connected)
  (let ((response (jsonrpc-request copilot-sdk--connection
                                    :session.list nil)))
    (append (plist-get response :sessions) nil)))

;;;; --- Sending Messages ---

(defun copilot-sdk-send (session-id prompt &rest args)
  "Send PROMPT to SESSION-ID.
ARGS is an optional plist with keys:
  :on-message — callback (TEXT) for assistant.message events
  :on-delta   — callback (CHUNK) for streaming delta events
  :on-idle    — callback () when session goes idle
  :on-error   — callback (MESSAGE) on session error
  :on-tool    — callback (TOOL-NAME) on tool execution start
Returns the message ID string.

Events are delivered via the registered callbacks.  If you want raw
event access, use `copilot-sdk-on' instead."
  (copilot-sdk-ensure-connected)
  (let* ((on-message (plist-get args :on-message))
         (on-delta (plist-get args :on-delta))
         (on-idle (plist-get args :on-idle))
         (on-error (plist-get args :on-error))
         (on-tool (plist-get args :on-tool))
         (unsubscribe nil))
    ;; Register event handler if any callbacks provided
    (when (or on-message on-delta on-idle on-error on-tool)
      (setq unsubscribe
            (copilot-sdk-on
             session-id
             (lambda (type data)
               (pcase type
                 ("assistant.message"
                  (when on-message
                    (funcall on-message (plist-get data :content))))
                 ("assistant.message_delta"
                  (when on-delta
                    (funcall on-delta (plist-get data :delta_content))))
                 ("tool.execution_start"
                  (when on-tool
                    (funcall on-tool (plist-get data :toolName))))
                 ("session.error"
                  (when on-error
                    (funcall on-error (plist-get data :message))))
                 ("session.idle"
                  (when unsubscribe (funcall unsubscribe))
                  (when on-idle (funcall on-idle))))))))
    ;; Send the message
    (let ((response (jsonrpc-request
                     copilot-sdk--connection
                     :session.send
                     `(:sessionId ,session-id :prompt ,prompt))))
      (plist-get response :messageId))))

(defun copilot-sdk-send-and-wait (session-id prompt &optional timeout)
  "Send PROMPT to SESSION-ID and wait for the response.
TIMEOUT is in seconds (default 120).
Returns the assistant's response text, or nil."
  (copilot-sdk-ensure-connected)
  (let ((response nil)
        (done nil)
        (deadline (+ (float-time) (or timeout 120))))
    (copilot-sdk-send session-id prompt
                      :on-message (lambda (text) (setq response text))
                      :on-idle (lambda () (setq done t))
                      :on-error (lambda (msg)
                                  (setq done t)
                                  (error "Session error: %s" msg)))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (unless done
      (error "Timeout waiting for session idle"))
    response))

;;;; --- Convenience Queries ---

(defun copilot-sdk-ping (&optional message)
  "Ping the CLI server with optional MESSAGE.
Return a plist with :message, :timestamp, :protocolVersion."
  (copilot-sdk-ensure-connected)
  (jsonrpc-request copilot-sdk--connection :ping `(:message ,(or message "ping"))))

(defun copilot-sdk-status ()
  "Get CLI status.  Return a plist with :version, :protocolVersion."
  (copilot-sdk-ensure-connected)
  (jsonrpc-request copilot-sdk--connection :status.get nil))

(defun copilot-sdk-auth-status ()
  "Get authentication status.
Return a plist with :isAuthenticated, :login, etc."
  (copilot-sdk-ensure-connected)
  (jsonrpc-request copilot-sdk--connection :auth.getStatus nil))

(defun copilot-sdk-list-models ()
  "List available models.  Return a list of model info plists."
  (copilot-sdk-ensure-connected)
  (let ((response (jsonrpc-request copilot-sdk--connection :models.list nil)))
    (append (plist-get response :models) nil)))

;;;; --- Handler Registration ---

(defun copilot-sdk-set-permission-handler (handler)
  "Set HANDLER for permission requests.
HANDLER receives (KIND REQUEST-PLIST) and returns a result plist
like (:kind \"approved\")."
  (setq copilot-sdk--permission-handler handler))

(defun copilot-sdk-set-user-input-handler (handler)
  "Set HANDLER for user input requests.
HANDLER receives (QUESTION CHOICES ALLOW-FREEFORM) and returns
a plist of the form \\=(:answer STRING :wasFreeform BOOL)."
  (setq copilot-sdk--user-input-handler handler))

(provide 'copilot-sdk)
;;; copilot-sdk.el ends here
