;;; eca-chat-layout-test.el --- Tests for eca-chat-layout -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for the four-zone layout window placement, in particular
;; `eca-chat-layout--display-splittable': side windows cannot be
;; split, so the layout must never keep the chat in one.
;;
;;; Code:

(require 'buttercup)
(require 'eca-chat-layout)

(describe "eca-chat-layout--display-splittable"
  (it "replaces a side window with a splittable regular window"
    (let ((saved-cwc (current-window-configuration))
          (buf (generate-new-buffer " *layout-test-chat*")))
      (unwind-protect
          (let ((eca-chat-window-side 'right)
                (eca-chat-use-side-window t)
                (eca-chat-focus-on-open t))
            (with-current-buffer buf
              (setq-local eca-chat--id "layout-test"))
            ;; Mimic `eca-chat-open': pop-window first leaves the
            ;; chat displayed in a dedicated side window.
            (eca-chat--display-buffer buf)
            (let* ((chat-win (progn
                               (eca-chat-layout--display-splittable
                                buf)
                               (get-buffer-window buf)))
                   (window-min-height 1)
                   (split-height-threshold nil)
                   (split-width-threshold 0))
              (expect (window-live-p chat-win) :to-be-truthy)
              (expect (window-parameter chat-win 'window-side)
                      :to-be nil)
              ;; The four-zone layout now splits the chat window.
              (select-window chat-win)
              (let ((prompt-win (split-window-below -11)))
                (expect (window-live-p prompt-win) :to-be-truthy)
                (let ((ws-win (progn
                                (select-window prompt-win)
                                (split-window-below 6))))
                  (expect (window-live-p ws-win) :to-be-truthy)))))
        ;; Restore the frame and drop every trace of the test.
        (dolist (w (and (buffer-live-p buf) (get-buffer-window-list buf)))
          (delete-window w))
        (set-window-configuration saved-cwc)
        (kill-buffer buf)))))

;;; eca-chat-layout-test.el ends here
