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
(require 'eca-chat-context)

;; Variables

(defvar-local eca-chat-info--chat-buffer nil
  "The chat buffer this info buffer tracks.")

;; Internal

(defun eca-chat-info--row-map (command)
  "Return a mouse-1 keymap invoking COMMAND on the whole row.
Mirrors the chat header-line approach in
`eca-chat--header-line-string' (the original selection method):
the command is bound directly, without a wrapper.  The selection
commands resolve the session themselves and route to the session's
active chat buffer, so they work from any buffer."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] command)
    map))

(defun eca-chat-info--overlay-clickable-rows (maps)
  "Cover each info row with a clickable overlay.
MAPS is an alist mapping row labels (strings) to commands.  Text
property keymaps can lose mouse clicks to global `down-mouse-1'
handlers (`mouse-drag-region', evil's mouse setup); overlay
keymaps sit at the top of the lookup chain and binding
`down-mouse-1' as well as `mouse-1' makes the row fire on press
regardless.  Overlays are wiped by the next render's
`erase-buffer'."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((label (car (split-string (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position))
                                       ":" t)))
             (command (and label (cdr (assoc label maps)))))
        (when command
          (let ((ov (make-overlay (line-beginning-position)
                                  (line-end-position))))
            (overlay-put ov 'eca-chat-info-row t)
            (overlay-put ov 'pointer 'hand)
            (overlay-put ov 'mouse-face 'highlight)
            (let ((map (make-sparse-keymap)))
              (define-key map [mouse-1] command)
              (define-key map [down-mouse-1] command)
              (overlay-put ov 'keymap map)))))
      (forward-line 1))))

(defun eca-chat--info-row (label value &optional keymap)
  "Return a formatted \"LABEL: VALUE\" string.
When KEYMAP is non-nil the whole row is clickable (mouse-1);
users naturally click anywhere on the row, not only on the
value, so the keymap covers label, separator and value."
  (let ((val (or value "-"))
        (face 'eca-chat-option-value-face))
    (if keymap
        (propertize
         (concat
          (propertize label 'font-lock-face 'eca-chat-option-key-face)
          ": "
          (propertize val 'font-lock-face face))
         'pointer 'hand
         'keymap keymap)
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

(define-key eca-chat-server-info-mode-map (kbd "m")
  #'eca-chat-select-model)
(define-key eca-chat-server-info-mode-map (kbd "a")
  #'eca-chat-select-agent)
(define-key eca-chat-server-info-mode-map (kbd "v")
  #'eca-chat-select-variant)
(define-key eca-chat-server-info-mode-map (kbd "g")
  #'eca-chat-server-info-refresh)
(define-key eca-chat-server-info-mode-map (kbd "r")
  #'eca-chat-server-info-refresh)

(defun eca-chat--render-server-info ()
  "Rebuild the server-info buffer from the target chat state."
  (unless (buffer-live-p eca-chat-info--chat-buffer)
    (user-error "Target chat buffer no longer exists"))
  (let* ((info-buffer (current-buffer))
         (chat eca-chat-info--chat-buffer)
         (model-keymap (eca-chat-info--row-map #'eca-chat-select-model))
         (agent-keymap (eca-chat-info--row-map #'eca-chat-select-agent))
         (variant-keymap (eca-chat-info--row-map #'eca-chat-select-variant)))
    (with-current-buffer chat
      (let ((model (eca-chat--model))
            (agent (eca-chat--agent))
            (variant (eca-chat--variant)))
        ;; Info buffers are read-only between renders; every
        ;; erase/insert must inhibit that or it signals
        ;; "Buffer is read-only", which (when called from a
        ;; process filter) also aborts handling of the server
        ;; message that triggered the refresh.
        (with-current-buffer info-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert
             (eca-chat--info-row "Model" model model-keymap) "\n"
             (eca-chat--info-row "Agent" agent agent-keymap) "\n"
             (eca-chat--info-row "Variant" variant variant-keymap) "\n")
            (eca-chat-info--overlay-clickable-rows
             (list (cons "Model" #'eca-chat-select-model)
                   (cons "Agent" #'eca-chat-select-agent)
                   (cons "Variant" #'eca-chat-select-variant)))
            (goto-char (point-min))))))))

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
              (setq-local eca--session-id-cache (eca--session-id session))
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

(defun eca-chat-info--action-row (label command)
  "Return a clickable action row showing LABEL.
COMMAND (interactive) runs on mouse-1; the row is also covered by
an overlay keymap in the render, see
`eca-chat-info--overlay-clickable-rows'."
  (propertize (concat label)
              'font-lock-face 'eca-chat-option-value-face
              'pointer 'hand
              'keymap (eca-chat-info--row-map command)))

(defun eca-chat--render-workspace-info ()
  "Rebuild the workspace-info buffer from the target chat state."
  (unless (buffer-live-p eca-chat-info--chat-buffer)
    (user-error "Target chat buffer no longer exists"))
  (let* ((info-buffer (current-buffer))
         (chat eca-chat-info--chat-buffer))
    (with-current-buffer chat
      (let* ((session (eca-session))
             (folders (eca--session-workspace-folders session))
             (tokens eca-chat--session-tokens)
             (limit eca-chat--session-limit-context)
             (bar (eca-chat--context-bar))
             (breakdown eca-chat--context-breakdown)
             (mcps (eca-mcp-servers session))
             (skills-tokens
              (when breakdown
                (plist-get (plist-get breakdown :categories)
                           "Skills")))
             (context-strs
              (mapcar (lambda (c) (eca-chat--context->str c t))
                      (append eca-chat--context nil))))
        ;; Same inhibit as the server-info render: the buffer is
        ;; read-only between renders.
        (with-current-buffer info-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            ;; Workspace paths
            (dolist (dir folders)
              (insert (eca-chat--info-row "Workspace" dir) "\n"))
            (insert (eca-chat-info--action-row "[+] add workspace"
                                               (lambda ()
                                                 (interactive)
                                                 (call-interactively
                                                  #'eca-chat-add-workspace-root)
                                                 (when (derived-mode-p
                                                        'eca-chat-workspace-info-mode)
                                                   (eca-chat--render-workspace-info)))))
            (insert "\n")
            (insert (eca-chat-info--action-row "[-] remove workspace"
                                               (lambda ()
                                                 (interactive)
                                                 (call-interactively
                                                  #'eca-chat-remove-workspace-root)
                                                 (when (derived-mode-p
                                                        'eca-chat-workspace-info-mode)
                                                   (eca-chat--render-workspace-info)))))
            (insert "\n")
            ;; Context usage (used / available tokens)
            (insert (eca-chat--info-row
                     "Context"
                     (let ((nf #'eca-chat--number->friendly-number))
                       (cond
                        ((and tokens limit)
                         (format "%s / %s"
                                 (funcall nf tokens)
                                 (funcall nf limit)))
                        (tokens (funcall nf tokens))
                        (t nil)))))
            (when bar
              (insert "\n" bar))
            (insert "\n")
            ;; Attached references (@file, @cursor, ... in the chat),
            ;; all on one line without a count.
            (insert
             (eca-chat--info-row
              "Refs"
              (and context-strs
                   (mapconcat #'identity context-strs " "))))
            (insert "\n")
            ;; MCP servers, listed by name.
            (let ((names (delq nil
                               (mapcar (lambda (s) (plist-get s :name))
                                       (append mcps nil)))))
              (insert (eca-chat--info-row
                       "MCPs"
                       (if names
                           (mapconcat #'identity names ", ")
                         "none"))
                      "\n"))
            ;; Skills footprint
            (insert (eca-chat--info-row
                     "Skills tokens"
                     (when skills-tokens
                       (eca-chat--number->friendly-number
                        skills-tokens))))
            (eca-chat-info--overlay-clickable-rows
             (list
              (cons "[+] add workspace"
                    (lambda ()
                      (interactive)
                      (call-interactively #'eca-chat-add-workspace-root)
                      (when (derived-mode-p 'eca-chat-workspace-info-mode)
                        (eca-chat--render-workspace-info))))
              (cons "[-] remove workspace"
                    (lambda ()
                      (interactive)
                      (call-interactively #'eca-chat-remove-workspace-root)
                      (when (derived-mode-p 'eca-chat-workspace-info-mode)
                        (eca-chat--render-workspace-info))))))
            (goto-char (point-min))))))))

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
            ;; Demote errors: this hook runs from process filters and
            ;; a render failure must not abort message processing.
            (cond
             ((derived-mode-p 'eca-chat-server-info-mode)
              (with-demoted-errors "eca-chat-info refresh: %S"
                (eca-chat--render-server-info)))
             ((derived-mode-p 'eca-chat-workspace-info-mode)
              (with-demoted-errors "eca-chat-info refresh: %S"
                (eca-chat--render-workspace-info))))))))))

(add-hook 'eca-chat-session-status-changed-functions
          #'eca-chat-info--refresh)

(provide 'eca-chat-info)
;;; eca-chat-info.el ends here
