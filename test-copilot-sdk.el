;;; test-copilot-sdk.el --- Tests for copilot-sdk.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edd Wilder-James

;;; Commentary:

;; ERT test suite for copilot-sdk.  Tests everything that can be tested
;; without a live CLI subprocess: tool definition, tool dispatch,
;; permission handling, event routing, session payload construction,
;; state management, and edge cases.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load copilot-sdk from the same directory as this test file
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "copilot-sdk" dir)))

;;;; === Test Helpers ===

(defmacro copilot-sdk-test-with-clean-state (&rest body)
  "Run BODY with a fresh copilot-sdk state, restoring afterward."
  (declare (indent 0))
  `(let ((copilot-sdk--connection nil)
         (copilot-sdk--tool-handlers (make-hash-table :test #'equal))
         (copilot-sdk--tool-defs nil)
         (copilot-sdk--event-handlers nil)
         (copilot-sdk--permission-handler nil)
         (copilot-sdk--user-input-handler nil))
     ,@body))

;;;; ============================================================
;;;; Tool Definition API
;;;; ============================================================

(ert-deftest copilot-sdk-test-define-tool-basic ()
  "Define a tool and verify it's registered."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool "my_tool" "Does a thing." #'ignore)
    (should (gethash "my_tool" copilot-sdk--tool-handlers))
    (should (= 1 (length copilot-sdk--tool-defs)))
    (let ((def (car copilot-sdk--tool-defs)))
      (should (string= "my_tool" (plist-get def :name)))
      (should (string= "Does a thing." (plist-get def :description))))))

(ert-deftest copilot-sdk-test-define-tool-with-schema ()
  "Define a tool with parameter schema."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool
     "search" "Search for things." #'ignore
     '(:type "object"
       :properties (:query (:type "string" :description "Search query"))
       :required ["query"]))
    (let ((def (car copilot-sdk--tool-defs)))
      (should (plist-get def :parameters))
      (should (string= "object" (plist-get (plist-get def :parameters) :type))))))

(ert-deftest copilot-sdk-test-define-tool-no-schema ()
  "Tool without schema should not have :parameters key set."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool "simple" "No params." #'ignore)
    (let ((def (car copilot-sdk--tool-defs)))
      (should-not (plist-get def :parameters)))))

(ert-deftest copilot-sdk-test-define-tool-returns-name ()
  "define-tool should return the tool name."
  (copilot-sdk-test-with-clean-state
    (should (string= "foo" (copilot-sdk-define-tool "foo" "Foo." #'ignore)))))

(ert-deftest copilot-sdk-test-define-tool-replaces-existing ()
  "Redefining a tool should replace the old definition."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool "dup" "First." #'ignore)
    (copilot-sdk-define-tool "dup" "Second." #'identity)
    ;; Should have exactly one definition
    (should (= 1 (length copilot-sdk--tool-defs)))
    (should (string= "Second." (plist-get (car copilot-sdk--tool-defs) :description)))
    ;; Handler should be the new one
    (should (eq #'identity (gethash "dup" copilot-sdk--tool-handlers)))))

(ert-deftest copilot-sdk-test-define-multiple-tools ()
  "Multiple tools should all be registered."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool "a" "Tool A." #'ignore)
    (copilot-sdk-define-tool "b" "Tool B." #'ignore)
    (copilot-sdk-define-tool "c" "Tool C." #'ignore)
    (should (= 3 (length copilot-sdk--tool-defs)))
    (should (= 3 (hash-table-count copilot-sdk--tool-handlers)))))

(ert-deftest copilot-sdk-test-clear-tools ()
  "clear-tools should remove all tools."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool "a" "A." #'ignore)
    (copilot-sdk-define-tool "b" "B." #'ignore)
    (copilot-sdk-clear-tools)
    (should (= 0 (length copilot-sdk--tool-defs)))
    (should (= 0 (hash-table-count copilot-sdk--tool-handlers)))))

(ert-deftest copilot-sdk-test-tool-defs-returns-vector ()
  "tool-defs should return a vector for JSON serialization."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool "x" "X." #'ignore)
    (let ((defs (copilot-sdk-tool-defs)))
      (should (vectorp defs))
      (should (= 1 (length defs))))))

(ert-deftest copilot-sdk-test-tool-defs-empty ()
  "tool-defs with no tools should return empty vector."
  (copilot-sdk-test-with-clean-state
    (let ((defs (copilot-sdk-tool-defs)))
      (should (vectorp defs))
      (should (= 0 (length defs))))))

;;;; ============================================================
;;;; Tool Dispatch
;;;; ============================================================

(ert-deftest copilot-sdk-test-dispatch-tool-success ()
  "Dispatch a tool call to a registered handler."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool
     "greet" "Say hello." 
     (lambda (args)
       `(:textResultForLlm ,(format "Hello, %s!" (plist-get args :name))
         :resultType "success"))
     '(:type "object" :properties (:name (:type "string"))))
    (let ((result (copilot-sdk--dispatch-tool-call
                   '(:toolName "greet"
                     :arguments (:name "World")))))
      (should (plist-get result :result))
      (let ((inner (plist-get result :result)))
        (should (string= "Hello, World!" (plist-get inner :textResultForLlm)))
        (should (string= "success" (plist-get inner :resultType)))))))

(ert-deftest copilot-sdk-test-dispatch-tool-unknown ()
  "Dispatch to an unknown tool should return failure."
  (copilot-sdk-test-with-clean-state
    (let* ((result (copilot-sdk--dispatch-tool-call
                    '(:toolName "nonexistent" :arguments nil)))
           (inner (plist-get result :result)))
      (should (string= "failure" (plist-get inner :resultType)))
      (should (string-match-p "Unknown tool" (plist-get inner :textResultForLlm))))))

(ert-deftest copilot-sdk-test-dispatch-tool-handler-error ()
  "A handler that signals an error should return failure, not crash."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool
     "explode" "Boom."
     (lambda (_args) (error "Kaboom!")))
    (let* ((result (copilot-sdk--dispatch-tool-call
                    '(:toolName "explode" :arguments nil)))
           (inner (plist-get result :result)))
      (should (string= "failure" (plist-get inner :resultType)))
      (should (string-match-p "Kaboom" (plist-get inner :textResultForLlm))))))

(ert-deftest copilot-sdk-test-dispatch-tool-nil-arguments ()
  "Handler should receive nil arguments gracefully."
  (copilot-sdk-test-with-clean-state
    (let ((received nil))
      (copilot-sdk-define-tool
       "nilargs" "Test nil."
       (lambda (args) (setq received args)
               '(:textResultForLlm "ok" :resultType "success")))
      (copilot-sdk--dispatch-tool-call
       '(:toolName "nilargs" :arguments nil))
      (should (null received)))))

(ert-deftest copilot-sdk-test-dispatch-tool-empty-name ()
  "Tool call with nil toolName should return failure."
  (copilot-sdk-test-with-clean-state
    (let* ((result (copilot-sdk--dispatch-tool-call
                    '(:toolName nil :arguments nil)))
           (inner (plist-get result :result)))
      (should (string= "failure" (plist-get inner :resultType))))))

(ert-deftest copilot-sdk-test-dispatch-tool-complex-arguments ()
  "Handler should receive nested plist arguments correctly."
  (copilot-sdk-test-with-clean-state
    (let ((received nil))
      (copilot-sdk-define-tool
       "complex" "Test complex args."
       (lambda (args) (setq received args)
               '(:textResultForLlm "ok" :resultType "success")))
      (copilot-sdk--dispatch-tool-call
       '(:toolName "complex"
         :arguments (:nested (:deep "value") :list ["a" "b"])))
      (should (string= "value" (plist-get (plist-get received :nested) :deep))))))

(ert-deftest copilot-sdk-test-dispatch-tool-handler-returns-string ()
  "Handler returning a plain string instead of plist — should not crash."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool
     "bad_return" "Returns wrong type."
     (lambda (_args) "just a string"))
    ;; Should not signal an error — the dispatch wraps the result
    (let ((result (copilot-sdk--dispatch-tool-call
                   '(:toolName "bad_return" :arguments nil))))
      ;; The result should be something (not crash)
      (should result))))

;;;; ============================================================
;;;; Permission Dispatch
;;;; ============================================================

(ert-deftest copilot-sdk-test-permission-no-handler ()
  "Without a handler, permissions should be denied."
  (copilot-sdk-test-with-clean-state
    (let* ((result (copilot-sdk--dispatch-permission
                    '(:permissionRequest (:kind "shell"))))
           (inner (plist-get result :result)))
      (should (string-match-p "denied" (plist-get inner :kind))))))

(ert-deftest copilot-sdk-test-permission-approved ()
  "Permission handler that approves."
  (copilot-sdk-test-with-clean-state
    (setq copilot-sdk--permission-handler
          (lambda (_kind _req) '(:kind "approved")))
    (let* ((result (copilot-sdk--dispatch-permission
                    '(:permissionRequest (:kind "write"))))
           (inner (plist-get result :result)))
      (should (string= "approved" (plist-get inner :kind))))))

(ert-deftest copilot-sdk-test-permission-denied ()
  "Permission handler that denies."
  (copilot-sdk-test-with-clean-state
    (setq copilot-sdk--permission-handler
          (lambda (_kind _req) '(:kind "denied-interactively-by-user")))
    (let* ((result (copilot-sdk--dispatch-permission
                    '(:permissionRequest (:kind "shell"))))
           (inner (plist-get result :result)))
      (should (string= "denied-interactively-by-user" (plist-get inner :kind))))))

(ert-deftest copilot-sdk-test-permission-handler-receives-kind ()
  "Permission handler should receive the correct kind string."
  (copilot-sdk-test-with-clean-state
    (let ((received-kind nil))
      (setq copilot-sdk--permission-handler
            (lambda (kind _req) (setq received-kind kind) '(:kind "approved")))
      (copilot-sdk--dispatch-permission
       '(:permissionRequest (:kind "mcp")))
      (should (string= "mcp" received-kind)))))

(ert-deftest copilot-sdk-test-permission-handler-error ()
  "If permission handler signals, should deny gracefully."
  (copilot-sdk-test-with-clean-state
    (setq copilot-sdk--permission-handler
          (lambda (_kind _req) (error "Handler crashed")))
    (let* ((result (copilot-sdk--dispatch-permission
                    '(:permissionRequest (:kind "shell"))))
           (inner (plist-get result :result)))
      (should (string-match-p "denied" (plist-get inner :kind))))))

(ert-deftest copilot-sdk-test-permission-nil-request ()
  "Permission dispatch with nil permissionRequest should not crash."
  (copilot-sdk-test-with-clean-state
    (let ((result (copilot-sdk--dispatch-permission '(:permissionRequest nil))))
      (should result))))

;;;; ============================================================
;;;; User Input Dispatch
;;;; ============================================================

(ert-deftest copilot-sdk-test-user-input-no-handler ()
  "Without a handler, user input should signal an error."
  (copilot-sdk-test-with-clean-state
    (should-error
     (copilot-sdk--dispatch-user-input
      '(:question "What is your name?" :choices nil :allowFreeform t)))))

(ert-deftest copilot-sdk-test-user-input-with-handler ()
  "User input handler should receive question and return answer."
  (copilot-sdk-test-with-clean-state
    (setq copilot-sdk--user-input-handler
          (lambda (question _choices _freeform)
            `(:answer ,(format "Answer to: %s" question) :wasFreeform t)))
    (let ((result (copilot-sdk--dispatch-user-input
                   '(:question "favorite color?" :choices nil :allowFreeform t))))
      (should (string-match-p "favorite color" (plist-get result :answer))))))

(ert-deftest copilot-sdk-test-user-input-handler-receives-choices ()
  "User input handler should receive choices list."
  (copilot-sdk-test-with-clean-state
    (let ((received-choices nil))
      (setq copilot-sdk--user-input-handler
            (lambda (_q choices _f) (setq received-choices choices)
                    '(:answer "red" :wasFreeform nil)))
      (copilot-sdk--dispatch-user-input
       '(:question "pick one" :choices ("red" "blue") :allowFreeform nil))
      (should (equal '("red" "blue") received-choices)))))

;;;; ============================================================
;;;; Event Handler API
;;;; ============================================================

(ert-deftest copilot-sdk-test-event-handler-registration ()
  "Register an event handler and verify it's stored."
  (copilot-sdk-test-with-clean-state
    (let ((unsub (copilot-sdk-on "session-1" #'ignore)))
      (should (= 1 (length copilot-sdk--event-handlers)))
      (should (functionp unsub)))))

(ert-deftest copilot-sdk-test-event-handler-unsubscribe ()
  "Unsubscribe function should remove the handler."
  (copilot-sdk-test-with-clean-state
    (let ((unsub (copilot-sdk-on "session-1" #'ignore)))
      (funcall unsub)
      (should (= 0 (length copilot-sdk--event-handlers))))))

(ert-deftest copilot-sdk-test-event-handler-dispatch ()
  "Events should be dispatched to the correct handler."
  (copilot-sdk-test-with-clean-state
    (let ((received nil))
      (copilot-sdk-on "s1" (lambda (type data)
                             (push (list type data) received)))
      ;; Simulate a notification
      (copilot-sdk--handle-notification
       nil "session.event"
       '(:sessionId "s1"
         :event (:type "assistant.message"
                 :data (:content "Hello!"))))
      (should (= 1 (length received)))
      (should (string= "assistant.message" (car (car received)))))))

(ert-deftest copilot-sdk-test-event-handler-wrong-session ()
  "Events for a different session should not trigger handler."
  (copilot-sdk-test-with-clean-state
    (let ((called nil))
      (copilot-sdk-on "s1" (lambda (_type _data) (setq called t)))
      (copilot-sdk--handle-notification
       nil "session.event"
       '(:sessionId "s2"
         :event (:type "assistant.message"
                 :data (:content "Not for you"))))
      (should-not called))))

(ert-deftest copilot-sdk-test-event-handler-multiple ()
  "Multiple handlers for the same session should all fire."
  (copilot-sdk-test-with-clean-state
    (let ((count 0))
      (copilot-sdk-on "s1" (lambda (_type _data) (cl-incf count)))
      (copilot-sdk-on "s1" (lambda (_type _data) (cl-incf count)))
      (copilot-sdk--handle-notification
       nil "session.event"
       '(:sessionId "s1"
         :event (:type "session.idle" :data nil)))
      (should (= 2 count)))))

(ert-deftest copilot-sdk-test-event-handler-error-isolation ()
  "A failing handler should not prevent other handlers from running."
  (copilot-sdk-test-with-clean-state
    (let ((second-called nil))
      (copilot-sdk-on "s1" (lambda (_type _data) (error "Handler 1 explodes")))
      (copilot-sdk-on "s1" (lambda (_type _data) (setq second-called t)))
      (copilot-sdk--handle-notification
       nil "session.event"
       '(:sessionId "s1"
         :event (:type "session.idle" :data nil)))
      (should second-called))))

(ert-deftest copilot-sdk-test-event-non-session-notification ()
  "Notifications that aren't session.event should be silently ignored."
  (copilot-sdk-test-with-clean-state
    (let ((called nil))
      (copilot-sdk-on "s1" (lambda (_type _data) (setq called t)))
      ;; Should not crash or dispatch
      (copilot-sdk--handle-notification
       nil "some.other.method" '(:foo "bar"))
      (should-not called))))

(ert-deftest copilot-sdk-test-event-missing-fields ()
  "Notification with missing event fields should not crash."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-on "s1" #'ignore)
    ;; Missing :event key entirely
    (copilot-sdk--handle-notification
     nil "session.event" '(:sessionId "s1"))
    ;; Missing :type in event
    (copilot-sdk--handle-notification
     nil "session.event" '(:sessionId "s1" :event (:data nil)))
    ;; Missing :data in event
    (copilot-sdk--handle-notification
     nil "session.event" '(:sessionId "s1" :event (:type "session.idle")))))

;;;; ============================================================
;;;; Request Dispatch (top-level router)
;;;; ============================================================

(ert-deftest copilot-sdk-test-handle-request-tool-call ()
  "Top-level request handler routes tool.call correctly."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool
     "echo" "Echo."
     (lambda (args) `(:textResultForLlm ,(plist-get args :text)
                      :resultType "success")))
    (let ((result (copilot-sdk--handle-request
                   nil 'tool.call
                   '(:toolName "echo" :arguments (:text "hi")))))
      (should (string= "hi" (plist-get (plist-get result :result)
                                       :textResultForLlm))))))

(ert-deftest copilot-sdk-test-handle-request-hooks-invoke ()
  "hooks.invoke should return (:output nil)."
  (copilot-sdk-test-with-clean-state
    (let ((result (copilot-sdk--handle-request
                   nil 'hooks.invoke
                   '(:sessionId "s1" :hookType "preToolUse" :input nil))))
      (should (null (plist-get result :output))))))

(ert-deftest copilot-sdk-test-handle-request-unknown-method ()
  "Unknown request methods should return nil (no match in pcase)."
  (copilot-sdk-test-with-clean-state
    (let ((result (copilot-sdk--handle-request
                   nil "totally.unknown" '(:foo "bar"))))
      (should (null result)))))

;;;; ============================================================
;;;; Connection State
;;;; ============================================================

(ert-deftest copilot-sdk-test-connected-p-nil ()
  "connected-p should be nil when no connection."
  (copilot-sdk-test-with-clean-state
    (should-not (copilot-sdk-connected-p))))

(ert-deftest copilot-sdk-test-start-when-connected ()
  "Starting when already connected should signal user-error."
  (copilot-sdk-test-with-clean-state
    ;; Simulate a connected state by setting connection to a truthy value
    ;; that also makes jsonrpc-running-p return t
    ;; We can't fully test this without a live process, but we test the guard
    (cl-letf (((symbol-function 'copilot-sdk-connected-p) (lambda () t)))
      (should-error (copilot-sdk-start) :type 'user-error))))

(ert-deftest copilot-sdk-test-stop-when-not-connected ()
  "Stopping when not connected should be a no-op."
  (copilot-sdk-test-with-clean-state
    ;; Should not error
    (copilot-sdk-stop)
    (should-not copilot-sdk--connection)))

;;;; ============================================================
;;;; Handler Registration API
;;;; ============================================================

(ert-deftest copilot-sdk-test-set-permission-handler ()
  "set-permission-handler should store the handler."
  (copilot-sdk-test-with-clean-state
    (let ((handler (lambda (_k _r) '(:kind "approved"))))
      (copilot-sdk-set-permission-handler handler)
      (should (eq handler copilot-sdk--permission-handler)))))

(ert-deftest copilot-sdk-test-set-permission-handler-nil ()
  "Setting permission handler to nil should clear it."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-set-permission-handler #'ignore)
    (copilot-sdk-set-permission-handler nil)
    (should-not copilot-sdk--permission-handler)))

(ert-deftest copilot-sdk-test-set-user-input-handler ()
  "set-user-input-handler should store the handler."
  (copilot-sdk-test-with-clean-state
    (let ((handler (lambda (_q _c _f) '(:answer "x" :wasFreeform t))))
      (copilot-sdk-set-user-input-handler handler)
      (should (eq handler copilot-sdk--user-input-handler)))))

;;;; ============================================================
;;;; Edge Cases & Adversarial Inputs
;;;; ============================================================

(ert-deftest copilot-sdk-test-tool-name-special-chars ()
  "Tool names with special characters should work."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool
     "my-tool.v2/search" "Special name."
     (lambda (_args) '(:textResultForLlm "found" :resultType "success")))
    (should (gethash "my-tool.v2/search" copilot-sdk--tool-handlers))
    (let* ((result (copilot-sdk--dispatch-tool-call
                    '(:toolName "my-tool.v2/search" :arguments nil)))
           (inner (plist-get result :result)))
      (should (string= "found" (plist-get inner :textResultForLlm))))))

(ert-deftest copilot-sdk-test-tool-name-empty-string ()
  "Empty string tool name should be registerable and dispatchable."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool "" "Empty name." #'ignore)
    (should (gethash "" copilot-sdk--tool-handlers))))

(ert-deftest copilot-sdk-test-tool-description-empty ()
  "Empty description should work."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool "nodesc" "" #'ignore)
    (let ((def (car copilot-sdk--tool-defs)))
      (should (string= "" (plist-get def :description))))))

(ert-deftest copilot-sdk-test-tool-handler-returns-nil ()
  "Handler returning nil should not crash dispatch."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool "nil_return" "Returns nil." (lambda (_) nil))
    (let ((result (copilot-sdk--dispatch-tool-call
                   '(:toolName "nil_return" :arguments nil))))
      (should result))))

(ert-deftest copilot-sdk-test-tool-unicode-arguments ()
  "Tool handler should receive Unicode arguments correctly."
  (copilot-sdk-test-with-clean-state
    (let ((received nil))
      (copilot-sdk-define-tool
       "unicode" "Unicode test."
       (lambda (args) (setq received (plist-get args :text))
               '(:textResultForLlm "ok" :resultType "success")))
      (copilot-sdk--dispatch-tool-call
       '(:toolName "unicode" :arguments (:text "こんにちは 🌍 Ω")))
      (should (string= "こんにちは 🌍 Ω" received)))))

(ert-deftest copilot-sdk-test-tool-very-large-result ()
  "Handler returning a large result should not crash."
  (copilot-sdk-test-with-clean-state
    (let ((big-string (make-string 100000 ?x)))
      (copilot-sdk-define-tool
       "big" "Returns large output."
       (lambda (_args) `(:textResultForLlm ,big-string :resultType "success")))
      (let* ((result (copilot-sdk--dispatch-tool-call
                      '(:toolName "big" :arguments nil)))
             (inner (plist-get result :result)))
        (should (= 100000 (length (plist-get inner :textResultForLlm))))))))

(ert-deftest copilot-sdk-test-define-tool-overwrite-different-schema ()
  "Redefining a tool with a different schema should update the schema."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool "evolve" "V1." #'ignore
                             '(:type "object" :properties (:a (:type "string"))))
    (copilot-sdk-define-tool "evolve" "V2." #'ignore
                             '(:type "object" :properties (:a (:type "string")
                                                           :b (:type "integer"))))
    (should (= 1 (length copilot-sdk--tool-defs)))
    (let* ((def (car copilot-sdk--tool-defs))
           (props (plist-get (plist-get def :parameters) :properties)))
      (should (plist-get props :b)))))

(ert-deftest copilot-sdk-test-event-handler-unsubscribe-idempotent ()
  "Calling unsubscribe multiple times should not error."
  (copilot-sdk-test-with-clean-state
    (let ((unsub (copilot-sdk-on "s1" #'ignore)))
      (funcall unsub)
      (funcall unsub)  ; Second call should be safe
      (should (= 0 (length copilot-sdk--event-handlers))))))

(ert-deftest copilot-sdk-test-event-handler-unsubscribe-correct-one ()
  "Unsubscribe should only remove the specific handler, not others."
  (copilot-sdk-test-with-clean-state
    (let ((calls nil))
      (copilot-sdk-on "s1" (lambda (_type _data) (push "first" calls)))
      (let ((unsub2 (copilot-sdk-on "s1"
                                    (lambda (_type _data) (push "second" calls)))))
        (funcall unsub2))
      ;; Only first handler should remain
      (should (= 1 (length copilot-sdk--event-handlers)))
      (copilot-sdk--handle-notification
       nil "session.event"
       '(:sessionId "s1" :event (:type "session.idle" :data nil)))
      (should (equal '("first") calls)))))

(ert-deftest copilot-sdk-test-many-event-handlers ()
  "Registering many handlers should work without issues."
  (copilot-sdk-test-with-clean-state
    (let ((count 0)
          (unsubs nil))
      (dotimes (_i 100)
        (push (copilot-sdk-on "s1" (lambda (_type _data) (cl-incf count)))
              unsubs))
      (copilot-sdk--handle-notification
       nil "session.event"
       '(:sessionId "s1" :event (:type "session.idle" :data nil)))
      (should (= 100 count))
      ;; Unsubscribe all
      (mapc #'funcall unsubs)
      (should (= 0 (length copilot-sdk--event-handlers))))))

(ert-deftest copilot-sdk-test-concurrent-tool-define-and-dispatch ()
  "Defining a tool while dispatching should not corrupt state."
  (copilot-sdk-test-with-clean-state
    (copilot-sdk-define-tool
     "sneaky" "Defines another tool during execution."
     (lambda (_args)
       (copilot-sdk-define-tool "injected" "Injected." #'ignore)
       '(:textResultForLlm "done" :resultType "success")))
    (copilot-sdk--dispatch-tool-call
     '(:toolName "sneaky" :arguments nil))
    ;; Both tools should exist
    (should (gethash "sneaky" copilot-sdk--tool-handlers))
    (should (gethash "injected" copilot-sdk--tool-handlers))
    (should (= 2 (length copilot-sdk--tool-defs)))))

(ert-deftest copilot-sdk-test-permission-handler-receives-full-request ()
  "Permission handler should receive the full request plist."
  (copilot-sdk-test-with-clean-state
    (let ((received-req nil))
      (setq copilot-sdk--permission-handler
            (lambda (_kind req) (setq received-req req) '(:kind "approved")))
      (copilot-sdk--dispatch-permission
       '(:permissionRequest (:kind "shell" :toolCallId "tc-42" :command "rm -rf /")))
      (should (string= "tc-42" (plist-get received-req :toolCallId))))))

(ert-deftest copilot-sdk-test-process-sentinel-clears-connection ()
  "Process sentinel should clear connection on non-open events."
  (copilot-sdk-test-with-clean-state
    (setq copilot-sdk--connection 'fake-connection)
    (copilot-sdk--process-sentinel nil "finished\n")
    (should-not copilot-sdk--connection)))

(ert-deftest copilot-sdk-test-process-sentinel-ignores-open ()
  "Process sentinel should not clear connection on open events."
  (copilot-sdk-test-with-clean-state
    (setq copilot-sdk--connection 'fake-connection)
    (copilot-sdk--process-sentinel nil "open from localhost\n")
    (should (eq 'fake-connection copilot-sdk--connection))))

(ert-deftest copilot-sdk-test-ensure-connected-calls-start ()
  "ensure-connected should call start when not connected."
  (copilot-sdk-test-with-clean-state
    (let ((start-called nil))
      (cl-letf (((symbol-function 'copilot-sdk-connected-p) (lambda () nil))
                ((symbol-function 'copilot-sdk-start) (lambda () (setq start-called t))))
        (copilot-sdk-ensure-connected)
        (should start-called)))))

(ert-deftest copilot-sdk-test-ensure-connected-skips-when-connected ()
  "ensure-connected should not call start when already connected."
  (copilot-sdk-test-with-clean-state
    (let ((start-called nil))
      (cl-letf (((symbol-function 'copilot-sdk-connected-p) (lambda () t))
                ((symbol-function 'copilot-sdk-start) (lambda () (setq start-called t))))
        (copilot-sdk-ensure-connected)
        (should-not start-called)))))

(provide 'test-copilot-sdk)
;;; test-copilot-sdk.el ends here
