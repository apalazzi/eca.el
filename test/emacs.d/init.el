;; -*- lexical-binding: t -*-
;(prefer-coding-system 'utf-8)

;;----------------------------------------------------------------------------
;; Adjust garbage collection
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 20 1024 1024))

;; -----------------------
;; use-package
;; -----------------------
(setq load-prefer-newer t)              ; Don't load outdated byte code

;; Ensure project dependencies are on the load path.
(dolist (dep '("dash-20260221.1346" "f-20241003.1131"
               "s-20220902.1511" "compat-31.0.0.1"
               "ht-20230703.558" "markdown-mode-20260425.954"))
  (add-to-list 'load-path (expand-file-name
                           (concat "~/.emacs.d/elpa/" dep))))

;; -------
;; eca
;; -------
(use-package eca
  :load-path "/home/andrea/src/eca.el"
  :config
  (setq eca-chat-use-side-window t)
  (setq eca-chat-window-side 'right)
  (setq eca-chat-hide-markdown-markup nil)
  ;; DEBUG scrolling sluggishness: plain-text chat rendering.
  (setq eca-chat-enable-markdown-formatting nil)
  ;; Chat window read-only: interactive edits outside the prompt
  ;; area are refused.
  (setq eca-chat-read-only-buffer t)
  )
;; (setq eca-extra-args '("--log-level" "debug"))
;; (setq eca-extra-args '("--verbose"))
(setq eca-api-response-timeout 300)
(with-eval-after-load 'eca-chat
  (define-key eca-chat-mode-map (kbd "<return>") nil)  ;; unbind return
  (define-key eca-chat-mode-map (kbd "RET") nil)        ;; unbind RET
  (define-key eca-chat-mode-map (kbd "S-<return>") #'eca-chat--key-pressed-return)
  (define-key eca-chat-mode-map (kbd "<escape>") #'eca-chat-stop-prompt)
)
