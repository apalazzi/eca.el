;;; eca-chat-info-test.el --- tests for read-only info buffers -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Regression tests for the info buffers: rendering must work on
;;  the read-only info buffers (erase/insert without
;;  inhibit-read-only used to signal "Buffer is read-only", which
;;  also aborted server-message processing when called from a
;;  process filter).
;;
;;; Code:

(require 'buttercup)
(require 'eca-chat-info)

(describe "eca-chat--render-server-info"
  (let (chat-buffer info-buffer)
    (before-each
      (setq chat-buffer (generate-new-buffer " *info-test-chat*"))
      (setq info-buffer (generate-new-buffer " *info-test-server*"))
      (with-current-buffer chat-buffer
        (setq-local eca-chat--selected-model "model-a")
        (setq-local eca-chat--selected-agent "agent-a")
        (setq-local eca-chat--selected-variant "low")
        (setq-local eca-chat--server-version "1.2.3"))
      (with-current-buffer info-buffer
        (eca-chat-server-info-mode)
        (setq-local eca-chat-info--chat-buffer chat-buffer)))
    (after-each
      (let ((inhibit-read-only t))
        (with-current-buffer info-buffer (erase-buffer)))
      (kill-buffer info-buffer)
      (kill-buffer chat-buffer))

    (it "renders without error into a read-only buffer"
      (with-current-buffer info-buffer
        (expect buffer-read-only :to-be-truthy)
        ;; Used to signal "Buffer is read-only".
        (expect (ignore-errors
                  (eca-chat--render-server-info) t)
                :to-be-truthy)))

    (it "shows the chat model/agent/variant values"
      (with-current-buffer info-buffer
        (eca-chat--render-server-info)
        (expect (buffer-string) :to-match "Model: model-a")
        (expect (buffer-string) :to-match "Agent: agent-a")
        (expect (buffer-string) :to-match "Variant: low")
        (expect (buffer-string) :to-match "Server: 1\.2\.3")))

    (it "re-renders on refresh picking up new values"
      (with-current-buffer info-buffer
        (eca-chat--render-server-info)
        (with-current-buffer chat-buffer
          (setq-local eca-chat--selected-model "model-b"))
        (eca-chat-server-info-refresh)
        (expect (buffer-string) :to-match "Model: model-b")))

    (it "makes the whole model/agent/variant row clickable"
      (with-current-buffer info-buffer
        (eca-chat--render-server-info)
        ;; Users click anywhere on the row: an overlay with a
        ;; mouse-1/down-mouse-1 keymap must cover label, separator
        ;; and value.
        (dolist (label '("Model" "Agent" "Variant"))
          (let ((bol (save-excursion
                       (goto-char (point-min))
                       (search-forward (concat label ":"))
                       (line-beginning-position))))
            (dolist (pos (list bol
                               (+ bol (length label))
                               (+ bol (length label) 2)))
              (let ((maps (append
                           (mapcar (lambda (ov) (overlay-get ov 'keymap))
                                   (overlays-at pos))
                           (list (get-text-property pos 'keymap)))))
                (expect (seq-some
                         (lambda (map)
                           (and map
                                (lookup-key map [mouse-1])
                                (lookup-key map [down-mouse-1])))
                         maps)
                        :to-be-truthy)))))))))

;;; eca-chat-info-test.el ends here
