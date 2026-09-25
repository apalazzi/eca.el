;;; eca-chat-prompt.el --- Persistent per-chat prompt buffer -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A persistent, dedicated prompt buffer per chat.  Unlike the
;;  one-shot compose buffer (`eca-chat-compose'), this buffer
;;  survives after sending and is the primary input surface when
;;  the four-window layout is active (Phase 3).  `eca-chat-prompt'
;;  opens (or reuses) the prompt buffer for the current chat;
;;  `C-c C-c' sends the buffer content and clears it; `C-c C-k'
;;  discards the current content.  @context and #filepath mentions
;;  complete against the ECA server just like in the inline chat
;;  prompt.
;;
;;; Code:

(require 'eca-util)
(require 'eca-chat)

;; Variables

(defvar-local eca-chat-prompt--target-buffer nil
  "The chat buffer the prompt is sent to.")

;; Internal

(defun eca-chat-prompt--yank-image-handler (type data)
  "Save clipboard image DATA of mime TYPE and insert an @file mention.
Writes the image to a temporary eca-screenshot file, like the eca
chat buffer does, and inserts \"@/path/to/file \" at point so the
server picks it up as a file context when the prompt is sent."
  (when-let* ((output-path (eca-chat-media--save-clipboard-image type data)))
    (insert eca-chat-context-prefix output-path " ")
    (eca-info "Image added, size: %s"
              (file-size-human-readable
               (file-attribute-size (file-attributes output-path))))))

(defun eca-chat-prompt--buffer-name (chat-buffer)
  "Return the prompt buffer name for CHAT-BUFFER."
  (format "*eca-prompt:%s*" (buffer-name chat-buffer)))

;; Public

(defun eca-chat-prompt-tab ()
  "Complete the @/# mention at point, else do markdown cycling."
  (interactive)
  (if (eca-chat--completion-type-at-point)
      (completion-at-point)
    (call-interactively #'markdown-cycle)))

(defun eca-chat-prompt-yank ()
  "Yank into the prompt buffer, routing images through `yank-media'."
  (interactive)
  (if (and (fboundp 'yank-media)
           (boundp 'yank-media--registered-handlers)
           yank-media--registered-handlers
           (eca-chat--clipboard-image-p))
      (call-interactively #'yank-media)
    (call-interactively #'yank)))

(eca-chat-define-derived-mode eca-chat-prompt-mode "eca-chat-prompt"
  "Major mode for the persistent ECA chat prompt buffer.
The target chat is captured when the buffer is created by
`eca-chat-prompt'.  Yanking a clipboard image inserts an @file
mention pointing at a temporary screenshot file.

\\{eca-chat-prompt-mode-map}"
  (setq header-line-format
        (substitute-command-keys
         "Prompt: \\[eca-chat-prompt-send] to send, \
\\[eca-chat-prompt-cancel] to clear"))
  (setq-local completion-at-point-functions
              (list #'eca-chat-completion-at-point))
  (setq-local completion-category-defaults
              (cons '(eca-capf (styles basic substring))
                    completion-category-defaults))
  (setq-local completion-ignore-case t)
  (when (fboundp 'yank-media-handler)
    (setq-local yank-media--registered-handlers nil)
    (yank-media-handler "image/png"
                        #'eca-chat-prompt--yank-image-handler)
    (yank-media-handler "image/jpeg"
                        #'eca-chat-prompt--yank-image-handler)
    (yank-media-handler "image/jpg"
                        #'eca-chat-prompt--yank-image-handler)
    (yank-media-handler "image/gif"
                        #'eca-chat-prompt--yank-image-handler)
    (yank-media-handler "image/webp"
                        #'eca-chat-prompt--yank-image-handler)))

(define-key eca-chat-prompt-mode-map (kbd "C-c C-c") #'eca-chat-prompt-send)
(define-key eca-chat-prompt-mode-map (kbd "C-c C-k") #'eca-chat-prompt-cancel)
(define-key eca-chat-prompt-mode-map [remap yank] #'eca-chat-prompt-yank)
(define-key eca-chat-prompt-mode-map (kbd "TAB") #'eca-chat-prompt-tab)
(define-key eca-chat-prompt-mode-map (kbd "<tab>") #'eca-chat-prompt-tab)

;;;###autoload
(defun eca-chat-prompt ()
  "Open the persistent prompt buffer for the current chat.
When called from a chat buffer the prompt targets that chat,
otherwise it targets the session's last used chat.
\\<eca-chat-prompt-mode-map>\\[eca-chat-prompt-send] sends the \
buffer content as a prompt to that chat;
\\[eca-chat-prompt-cancel] clears the buffer without sending."
  (interactive)
  (let ((session (eca-session)))
    (eca-assert-session-running session)
    (let ((target (if (derived-mode-p 'eca-chat-mode)
                      (current-buffer)
                    (eca-chat--get-last-buffer session))))
      (unless (buffer-live-p target)
        (user-error "No chat buffer found for prompt"))
      (let ((name (eca-chat-prompt--buffer-name target)))
        (if (get-buffer name)
            (pop-to-buffer (get-buffer name))
          (let ((buffer (generate-new-buffer name)))
            (with-current-buffer buffer
              (eca-chat-prompt-mode)
              (setq eca-chat-prompt--target-buffer target)
              (setq-local eca--session-id-cache
                          (eca--session-id session))
              (when-let* ((dir (car (eca--session-workspace-folders
                                     session))))
                (setq-local default-directory dir))
              (setq-local eca-chat--id
                          (buffer-local-value
                           'eca-chat--id target))))
            (pop-to-buffer buffer))))))

(defun eca-chat-prompt-send ()
  "Send the prompt buffer content to the target chat.
Clears the buffer afterwards but keeps it alive."
  (interactive)
  (let ((text (string-trim
               (buffer-substring-no-properties (point-min) (point-max))))
        (prompt-buffer (current-buffer))
        (target eca-chat-prompt--target-buffer)
        (session (eca-session)))
    (eca-assert-session-running session)
    (when (string-empty-p text)
      (user-error "Nothing to send"))
    (unless (buffer-live-p target)
      (user-error "The target chat buffer no longer exists"))
    (setf (eca--session-last-chat-buffer session) target)
    (eca-chat--with-current-buffer target
      (eca-chat--send-prompt session text prompt-buffer))
    (erase-buffer)
    (eca-info "Prompt sent to %s" (buffer-name target))))

(defun eca-chat-prompt-cancel ()
  "Clear the prompt buffer without sending."
  (interactive)
  (erase-buffer)
  (eca-info "Prompt cleared"))

(provide 'eca-chat-prompt)
;;; eca-chat-prompt.el ends here
