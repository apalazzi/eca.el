;;; eca-chat-layout.el --- Four-window stacked chat layout -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Builds the four stacked windows (top to bottom) when a chat
;;  is opened: server-info / chat / prompt / workspace-info.
;;  The chat window is the large one; the others are small.
;;  All four are normal, resizable windows.
;;
;;  On exit the original window configuration is restored.
;;
;;; Code:

(require 'eca-util)
(require 'eca-chat)
(require 'eca-chat-prompt)
(require 'eca-chat-info)

;; Variables

(defvar-local eca-chat--layout-saved-cwc nil
  "Saved window configuration before the four-zone layout.")

;; Internal — buffer helpers

(defun eca-chat-layout--get-prompt-buffer (chat-buffer)
  "Return the prompt buffer for CHAT-BUFFER, creating if needed."
  (let ((name (format "*eca-prompt:%s*" (buffer-name chat-buffer))))
    (if (get-buffer name)
        (get-buffer name)
      (let ((buf (generate-new-buffer name))
            (session (with-current-buffer chat-buffer
                       (eca-session))))
        (with-current-buffer buf
          (eca-chat-prompt-mode)
          (setq eca-chat-prompt--target-buffer chat-buffer)
          (setq-local eca--session-id-cache
                      (eca--session-id session))
          (when-let* ((dir (car (eca--session-workspace-folders
                                 session))))
            (setq-local default-directory dir))
          (setq-local eca-chat--id
                      (buffer-local-value
                       'eca-chat--id chat-buffer)))
        buf))))

(defun eca-chat-layout--get-server-info-buffer (chat-buffer)
  "Return the server-info buffer for CHAT-BUFFER, creating if needed."
  (let ((name (format "*eca-server-info:%s*"
                     (buffer-name chat-buffer))))
    (if (get-buffer name)
        (get-buffer name)
      (let ((buf (generate-new-buffer name)))
        (with-current-buffer buf
          (eca-chat-server-info-mode)
          (setq eca-chat-info--chat-buffer chat-buffer)
          (eca-chat--render-server-info))
        buf))))

(defun eca-chat-layout--get-workspace-info-buffer (chat-buffer)
  "Return the workspace-info buffer for CHAT-BUFFER, creating if needed."
  (let ((name (format "*eca-workspace-info:%s*"
                     (buffer-name chat-buffer))))
    (if (get-buffer name)
        (get-buffer name)
      (let ((buf (generate-new-buffer name)))
        (with-current-buffer buf
          (eca-chat-workspace-info-mode)
          (setq eca-chat-info--chat-buffer chat-buffer)
          (eca-chat--render-workspace-info))
        buf))))

;; Layout setup

(defun eca-chat--layout-setup (_session chat-buffer)
  "Build the four stacked windows around CHAT-BUFFER.
Top to bottom: server-info, chat, prompt, workspace-info.
The chat window takes the space of the old side window."
  (unless (memq eca-chat-window-side '(left right))
    (user-error "Four-zone layout requires left or right side"))
  ;; Save current window configuration for teardown.
  (eca-chat--with-current-buffer chat-buffer
    (setq-local eca-chat--layout-saved-cwc
                (current-window-configuration)))
  ;; Display the chat in the side window using existing logic.
  (eca-chat--display-buffer chat-buffer)
  (let ((chat-win (get-buffer-window chat-buffer)))
    (unless (window-live-p chat-win)
      (user-error "Failed to display chat window"))
    (select-window chat-win)
    ;; `split-window-below'/`split-window-above' act on the
    ;; selected window; SIZE is the height the original window
    ;; keeps, the new one gets the rest.  Bind
    ;; `window-min-height' so the small info windows can exist.
    (let* ((window-min-height 1)
           (h (window-total-height chat-win))
           ;; Split 11 lines below the chat window; that new
           ;; window becomes the prompt window...
           (prompt-win (split-window-below (max 1 (- h 11))))
           ;; ...split again below: prompt keeps 6 lines, the
           ;; new lower window (5) is workspace-info.
           (ws-win (progn
                     (select-window prompt-win)
                     (split-window-below 6)))
           ;; Finally split 3 lines above the chat window for
           ;; server-info.  (`split-window-above' does not exist;
           ;; the generic `split-window' with SIDE 'above keeps
           ;; SIZE lines in the original window.)
           (info-win (progn
                       (select-window chat-win)
                       (split-window
                        chat-win
                        (max 1 (- (window-total-height chat-win)
                                  3))
                        'above))))
          ;; Set buffers in the three new windows.
          (set-window-buffer info-win
                             (eca-chat-layout--get-server-info-buffer
                              chat-buffer))
          (set-window-buffer prompt-win
                             (eca-chat-layout--get-prompt-buffer
                              chat-buffer))
          (set-window-buffer ws-win
                             (eca-chat-layout--get-workspace-info-buffer
                              chat-buffer))
          ;; Suppress header-line and mode-line on chat buffer.
          (eca-chat--with-current-buffer chat-buffer
            (setq-local header-line-format nil)
            (setq-local mode-line-format nil))
          ;; Focus the prompt window (user input).
          (select-window prompt-win))))

;; Layout teardown

(defun eca-chat--layout-teardown (chat-buffer)
  "Restore the window configuration saved before the layout."
  (eca-chat--with-current-buffer chat-buffer
    (when (window-configuration-p eca-chat--layout-saved-cwc)
      (ignore-errors
        (set-window-configuration eca-chat--layout-saved-cwc))
      (setq-local eca-chat--layout-saved-cwc nil)
      ;; Re-enable header-line and mode-line.
      (when eca-chat-override-mode-line
        (setq-local mode-line-format
                    `(t (:eval (eca-chat--mode-line-string
                                (eca-session))))))
      (unless (listp header-line-format)
        (setq-local header-line-format
                    (list header-line-format)))
      (add-to-list 'header-line-format
                   `(t (:eval (eca-chat--header-line-string
                               (eca-session)))))
      (force-mode-line-update))))

(provide 'eca-chat-layout)
;;; eca-chat-layout.el ends here
