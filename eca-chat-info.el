;;; eca-chat-info.el --- Read-only chat info buffers -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Two read-only info buffers per chat:
;;  - server-info: model, agent, variant (clickable), server
;;    version, and trust indicator.
;;  - workspace-info: workspace paths, context usage with the
;;    colored bar, MCP servers, and skills token footprint.
;;
;;  Both refresh automatically via
;;  `eca-chat-session-status-changed-functions'.
;;
;;; Code:

(require 'eca-util)
(require 'eca-chat)

;; Variables

(defvar-local eca-chat-info--chat-buffer nil
  "The chat buffer this info buffer tracks.")

;; Internal

(defun eca-chat--info-row (label value &optional keymap)
  "Return a formatted \"LABEL: VALUE\" string.
When KEYMAP is non-nil the value is clickable (mouse-1)."
  (let ((val (or value "-"))
        (face 'eca-chat-option-value-face))
    (if keymap
        (concat
         (propertize label 'font-lock-face 'eca-chat-option-key-face)
         ": "
         (propertize val 'font-lock-face face
                     'pointer 'hand
                     'keymap keymap))
      (concat
       (propertize label 'font-lock-face 'eca-chat-option-key-face)
       ": "
       (propertize val 'font-lock-face face)))))

;; Server-info

(eca-chat-define-derived-mode eca-chat-server-info-mode
  "eca-chat-server-info"
  "Read-only buffer showing chat server information.
Displays the current model, agent, variant (clickable to
change), the ECA server version, and the trust indicator.

\\{eca-chat-server-info-mode-map}"
  (setq header-line-format
        (substitute-command-keys
         "Server info: \\[eca-chat-server-info-refresh] to refresh"))
  (setq buffer-read-only t)
  (read-only-mode 1))

(defun eca-chat--render-server-info ()
  "Rebuild the server-info buffer from the target chat state."
  (unless (buffer-live-p eca-chat-info--chat-buffer)
    (user-error "Target chat buffer no longer exists"))
  (let* ((info-buffer (current-buffer))
         (chat eca-chat-info--chat-buffer)
         (model-keymap (make-sparse-keymap))
         (agent-keymap (make-sparse-keymap))
         (variant-keymap (make-sparse-keymap)))
    (define-key model-keymap (kbd "<mouse-1>") #'eca-chat-select-model)
    (define-key agent-keymap (kbd "<mouse-1>") #'eca-chat-select-agent)
    (define-key variant-keymap (kbd "<mouse-1>") #'eca-chat-select-variant)
    (with-current-buffer chat
      (let ((model (eca-chat--model))
            (agent (eca-chat--agent))
            (variant (eca-chat--variant))
            (version eca-chat--server-version)
            (trust (eca-chat--trust)))
        (with-current-buffer info-buffer
          (erase-buffer)
          (insert
           (eca-chat--info-row "Model" model model-keymap) "\n"
           (eca-chat--info-row "Agent" agent agent-keymap) "\n"
           (eca-chat--info-row "Variant" variant variant-keymap) "\n"
           (eca-chat--info-row "Server" version) "\n"
           (let* ((graphic? (display-graphic-p))
                  (face (if trust
                            'eca-chat-trust-on-face
                          'eca-chat-trust-off-face))
                  (symbol (if trust
                              (if graphic? eca-chat-trust-on-symbol
                                eca-chat-trust-on-symbol-tty)
                            (if graphic? eca-chat-trust-off-symbol
                              eca-chat-trust-off-symbol-tty))))
             (concat
              (propertize "Trust"
                          'font-lock-face 'eca-chat-option-key-face)
              ": "
              (propertize symbol 'face face)
              (propertize (if trust " (on)" " (off)")
                          'face face)))
           "\n")
          (goto-char (point-min)))))))

(defun eca-chat-server-info-buffer ()
  "Get or create the server-info buffer for the current chat.
When called from a chat buffer the info targets that chat,
otherwise it targets the session's last used chat."
  (interactive)
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (let ((target (if (derived-mode-p 'eca-chat-mode)
                      (current-buffer)
                    (eca-chat--get-last-buffer session))))
      (unless (buffer-live-p target)
        (user-error "No chat buffer found for server info"))
      (let ((name (format "*eca-server-info:%s*"
                          (buffer-name target))))
        (if (get-buffer name)
            (progn
              (pop-to-buffer (get-buffer name))
              (with-current-buffer (get-buffer name)
                (eca-chat--render-server-info)))
          (let ((buffer (generate-new-buffer name)))
            (with-current-buffer buffer
              (eca-chat-server-info-mode)
              (setq eca-chat-info--chat-buffer target)
              (eca-chat--render-server-info))
            (pop-to-buffer buffer)))))))

;; Workspace-info

(eca-chat-define-derived-mode eca-chat-workspace-info-mode
  "eca-chat-workspace-info"
  "Read-only buffer showing chat workspace information.
Displays workspace paths, context usage with the colored bar,
MCP server status, and the skills token footprint.

\\{eca-chat-workspace-info-mode-map}"
  (setq header-line-format
        (substitute-command-keys
         "Workspace info: \
\\[eca-chat-workspace-info-refresh] to refresh"))
  (setq buffer-read-only t)
  (read-only-mode 1))

(defun eca-chat--render-workspace-info ()
  "Rebuild the workspace-info buffer from the target chat state."
  (unless (buffer-live-p eca-chat-info--chat-buffer)
    (user-error "Target chat buffer no longer exists"))
  (let* ((info-buffer (current-buffer))
         (chat eca-chat-info--chat-buffer)
         (session (with-current-buffer chat (eca-session))))
    (with-current-buffer chat
      (let* ((folders (eca--session-workspace-folders session))
            (tokens eca-chat--session-tokens)
            (limit eca-chat--session-limit-context)
            (bar (eca-chat--context-bar))
            (breakdown eca-chat--context-breakdown)
            (mcps (eca-mcp-servers session))
            (skills-tokens
             (when breakdown
               (plist-get (plist-get breakdown :categories)
                          "Skills"))))
        (with-current-buffer info-buffer
          (erase-buffer)
          ;; Workspace paths
          (dolist (dir folders)
            (insert (eca-chat--info-row "Workspace" dir) "\n"))
          ;; Context usage
          (insert (eca-chat--info-row
                   "Context"
                   (when (and tokens limit)
                     (format "%s / %s"
                             (eca-chat--number->friendly-number
                              tokens)
                             (eca-chat--number->friendly-number
                              limit)))))
          (when bar
            (insert "\n" bar))
          (insert "\n")
          ;; MCP servers
          (if mcps
              (let ((summary (eca-chat--mcps-summary session)))
                (insert (eca-chat--info-row "MCPs" summary) "\n"))
            (insert (eca-chat--info-row "MCPs" "none") "\n"))
          ;; Skills footprint
          (insert (eca-chat--info-row
                   "Skills tokens"
                   (when skills-tokens
                     (eca-chat--number->friendly-number
                      skills-tokens))))
          (goto-char (point-min)))))))

(defun eca-chat-workspace-info-buffer ()
  "Get or create the workspace-info buffer for the current chat.
When called from a chat buffer the info targets that chat,
otherwise it targets the session's last used chat."
  (interactive)
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (let ((target (if (derived-mode-p 'eca-chat-mode)
                      (current-buffer)
                    (eca-chat--get-last-buffer session))))
      (unless (buffer-live-p target)
        (user-error "No chat buffer found for workspace info"))
      (let ((name (format "*eca-workspace-info:%s*"
                          (buffer-name target))))
        (if (get-buffer name)
            (progn
              (pop-to-buffer (get-buffer name))
              (with-current-buffer (get-buffer name)
                (eca-chat--render-workspace-info)))
          (let ((buffer (generate-new-buffer name)))
            (with-current-buffer buffer
              (eca-chat-workspace-info-mode)
              (setq eca-chat-info--chat-buffer target)
              (eca-chat--render-workspace-info))
            (pop-to-buffer buffer)))))))

;; Refresh

(defun eca-chat-server-info-refresh ()
  "Re-render the current server-info buffer."
  (interactive)
  (eca-chat--render-server-info))

(defun eca-chat-workspace-info-refresh ()
  "Re-render the current workspace-info buffer."
  (interactive)
  (eca-chat--render-workspace-info))

(defun eca-chat-info--refresh (session)
  "Refresh all info buffers belonging to SESSION."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'eca-chat-server-info-mode
                                 'eca-chat-workspace-info-mode)
                 eca-chat-info--chat-buffer
                 (buffer-live-p eca-chat-info--chat-buffer))
        (let ((buf-session (with-current-buffer
                               eca-chat-info--chat-buffer
                             (eca-session))))
          (when (eq buf-session session)
            (cond
             ((derived-mode-p 'eca-chat-server-info-mode)
              (eca-chat--render-server-info))
             ((derived-mode-p 'eca-chat-workspace-info-mode)
              (eca-chat--render-workspace-info)))))))))

(add-hook 'eca-chat-session-status-changed-functions
          #'eca-chat-info--refresh)

(provide 'eca-chat-info)
;;; eca-chat-info.el ends here
