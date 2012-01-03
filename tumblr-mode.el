;;; -*- indent-tabs-mode: t; tab-width: 8 -*-
;;;
;;; tumblr-mode.el --- Major mode for Tumblr

;; Copyright (C) 2011 Julian Qian

;; Author: Julian Qian <junist@gmail.com>
;; Created: Dec 25, 2011
;; Keywords: tumblr blog writer

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; tumblr-mode.el is a major mode for tumblr.com.
;; You can write and manage your tumblr posts in Emacs.

;; - `tumblr-new-post' create a buffer to post
;; - `tumblr-save-post' save buffer to tumblr.com
;; - `tumblr-list-posts' list tumblr posts in a buffer

;;; Feature Request:


;;; Code:

(require 'url)
(require 'xml)

(defcustom tumblr-hostname nil
  "Your blog host name, such as test.tumblr.com")
(defcustom tumblr-email nil
  "tumblr login email")
(defcustom tumblr-password nil
  "tumblr login password")
(defcustom tumblr-post-type "regular"
  "tumblr post type")
(defcustom tumblr-post-format "markdown"
  "tumblr post format")
(defcustom tumblr-retrieve-posts-num-total 20
  "the number of posts to be listed")

(defvar tumblr-retrieve-posts-num-once 20)
(defvar tumblr-retrieve-posts-list nil)
(defvar tumblr-mode-map (make-sparse-keymap))

;; utilities
(defun assocref (item alist)
  (cdr (assoc item alist)))

(defun assqref (item alist)
  (cdr (assq item alist)))

(defun tumblr-get-posts-count ()
  (let* ((root (tumblr-http-get-xml-root
                (format "http://%s/api/read?num=1&filter=text"
                        tumblr-hostname)))
         (tumblr (car root))
         (posts (car (xml-get-children tumblr 'posts)))
         (attrs (xml-node-attributes posts))
         (total (assqref 'total attrs)))
    (string-to-number total)))


(defun tumblr-query-string (args)
  (mapconcat (lambda (arg)
               (concat (url-hexify-string (car arg))
                       "="
                       (url-hexify-string (cdr arg))))
             args "&"))

(defun tumblr-authenticated-read-xml-root (hostname params)
  (let* ((url-request-method "POST")
         (url-http-attempt-keepalives nil)
         (url-mime-charset-string "utf-8;q=0.7,*;q=0.7")
         (url-request-data (tumblr-query-string
                            (append `(("email" . ,tumblr-email)
                                      ("password" . ,tumblr-password)
                                      ("type" . ,tumblr-post-type)) params)))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (buffer (url-retrieve-synchronously (format "http://%s/api/read" hostname)))
         (root (with-current-buffer buffer
                 (set-buffer-multibyte t)
                 ;; (write-file "/tmp/tumblr.txt" nil)
                 ;; (decode-coding-region (point-min) (point-max) 'utf-8)
                 (goto-char (point-min))
                 (if (search-forward "<?xml" nil t) ; skip HTTP header
                     (xml-parse-region (match-beginning 0) (point-max))
                   (error "Failed to read from tumblr.")))))
    (kill-buffer buffer)
    root))

(defun tumblr-write-post (hostname params)
  "PARAMS is a list "
  (let* ((url-request-method "POST")
         (url-http-attempt-keepalives nil)
         (url-mime-charset-string "utf-8;q=0.7,*;q=0.7")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (tumblr-query-string
                            (append `(("email" . ,tumblr-email)
                                      ("password" . ,tumblr-password)
                                      ("group" . ,hostname)
                                      ("type" . ,tumblr-post-type)
                                      ("format" . ,tumblr-post-format)
                                      ("generator" . "tumblr-mode.el")) params)))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (buffer (url-retrieve
                  "http://www.tumblr.com/api/write"
                  (lambda (&rest args)
                    (let ((buffer (current-buffer)))
                      (switch-to-buffer buffer)
                      (with-current-buffer buffer
                        (goto-char (point-min))
                        ;; take from twittering-mode.el
                        (when (search-forward-regexp
                               "\\`\\(\\(HTTP/1\.[01]\\) \\([0-9][0-9][0-9]\\) \\(.*?\\)\\)\r?\n"
                               nil t)
                          (let ((status-line (match-string 1))
                                (http-version (match-string 2))
                                (status-code (match-string 3))
                                (reason-phrase (match-string 4)))
                            (cond
                             ((string= "201" status-code)
                              (search-forward-regexp "\r?\n\r?\n" nil t)
                              (let* ((beg (match-end 0))
                                     (end (point-max))
                                     (post-id (string-to-number (buffer-substring beg end))))
                                ;; get post-id
                                (message "Post OK. return id: %d" post-id)))
                             ((string= "403" status-code)
                              (message "Failed. Your email address or password were incorrect."))
                             ((string= "400" status-code)
                              (message "Failed. There was at least one error while trying to save your post."))
                             (t
                              (error (format "Unknown failure, maybe network exception :(")))))))
                      (kill-buffer buffer))))))
    (kill-buffer buffer)))

(defun tumblr-list-posts-internal (retrieving tagged state)
  (let* ((root (tumblr-authenticated-read-xml-root
                tumblr-hostname
                `(("num" . ,(number-to-string retrieving))
                  ("filter" . "none")
                  ("tagged" . tagged)
                  ("state" . state))))
         (tumblr (car root))
         (posts (car (xml-get-children tumblr 'posts)))
         (post-list (xml-get-children posts 'post)))
    (mapcar (lambda (post)
              (let* ((attrs (xml-node-attributes post))
                     (timestamp (assqref 'unix-timestamp attrs))
                     (date (assqref 'date attrs))
                     (id (assqref 'id attrs))
                     (slug (assqref 'slug attrs))
                     (url (assqref 'url attrs))
                     (title (cddar (xml-get-children post 'regular-title)))
                     (tags (mapcar (lambda (tag) (caddr tag))
                                   (xml-get-children post 'tag))))
                ;; return a list of alist
                `((timestamp    .   ,timestamp)
                  (date         .   ,date)
                  (id           .   ,id)
                  (slug         .   ,slug)
                  (url          .   ,url)
                  (title        .   ,title)
                  (tags         .   ,tags))))
            post-list)))

(defun tumblr-list-posts (&optional tagged state)
  (interactive)
  (let* ((total (tumblr-get-posts-count))
           (total-retrieving (or (if (> tumblr-retrieve-posts-num-total total)
                                     total
                                   tumblr-retrieve-posts-num-total) total))
           (remaining total-retrieving)
           (tumblr-retrieve-posts-list nil))
      (while (> remaining 0)
        (let ((retrieving (if (> remaining tumblr-retrieve-posts-num-once)
                              tumblr-retrieve-posts-num-once
                            remaining)))
          (setq remaining (- remaining retrieving))
          (setq tumblr-retrieve-posts-list
                (append tumblr-retrieve-posts-list
                        (tumblr-get-posts-list-internal retrieving tagged state)))))
      ;; list all posts
      (with-current-buffer (get-buffer-create "*tumblr-mode*")
        (tumblr-mode-setup)
        ;; (toggle-read-only 1)
        (goto-char (point-min))
        (save-excursion
          (kill-region (point-min) (point-max))
          ;; header
          (save-excursion
            (insert "         Title                           Tags         Date     \n"))
          (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put ov 'face 'header-line))
          (forward-line)
          ;; list
          (mapc (lambda (post)
                  (insert (assqref 'title post))
                  (insert (mapconcat (lambda (tag) (format "#%s" tag))
                                     (assqref 'tags post) ", "))
                  (insert (assqref 'date post))
                  (make-text-button (line-beginning-position) (line-end-position)
                                      'id (assqref 'id post)
                                      'action 'tumblr-get-post-edit
                                      'face 'default)
                  (insert "\n"))
                tumblr-retrieve-posts-list))

        ;; skip header
        (forward-line)
        (toggle-read-only t)
        (set-window-buffer nil (current-buffer)))))

(defun tumblr-get-post (post-id)
  (let* ((root (tumblr-authenticated-read-xml-root
                tumblr-hostname
                `(("post-id" . ,post-id)
                  ("filter" . "none"))))
         (tumblr (car root))
         (posts (car (xml-get-children tumblr 'posts)))
         (post (car (xml-get-children posts 'post)))
         (attrs (xml-node-attributes post))
         (title (cddar (xml-get-children post 'regular-title)))
         (body (cddar (xml-get-children post 'regular-body))) ; post content
         (tags (mapcar (lambda (tag) (caddr tag))
                       (xml-get-children post 'tag))))
    ;; edit post
    (with-current-buffer (get-buffer-create (format "*tumbr: %s*" title))
      (tumblr-mode-post-setup)
      (goto-char (point-min))
      (save-excursion
        (kill-region (point-min) (point-max))
        ;; insert meta info, as octopress does
        (tumblr-insert-post-template title attrs)
        ;; insert content
        (insert body)))))

(defun tumblr-insert-post-template (title &optional attrs)
  (insert "--\n")
  (let ((date (assqref 'date attrs))
        (id (assqref 'id attrs))
        (slug (assqref 'slug attrs))
        (tags (assqref 'tags attrs))
        (state (assqref 'state attrs)))
    (and id (insert (format "id: %s\n" id)))
    (and title (insert (format "title: %s\n" title)))
    (and slug (insert (format "slug: %s\n" slug)))
    (and tags (insert (format "tags: %s\n" tags)))
    (and state (insert (format "state: %s\n" state)))
    (and date (insert (format "date: %s\n" date))))
  (insert "--\n\n"))

(defun tumblr-get-post-edit (button)
  (tumblr-get-post (button-get button 'id)))

(defun tumblr-save-post (&optional id)
  (interactive)
  (let* ((body-start (point-min))
         ;; get meta properities
         (props (save-excursion
                  ;; only search meta info in 0~10 lines
                  (let ((bound (progn (goto-char (point-min))
                                      (forward-line 10)
                                      (point)))
                        beg end)
                    (goto-char (point-min))           ; start to search
                    (when (search-forward-regexp "--\r?\n" bound t)
                      (setq beg (match-end 0))        ; meta info begin point
                      (when (search-forward-regexp "--\r?\n" bound t)
                        (setq end (match-beginning 0)) ; meta info end point
                        (search-forward-regexp "[^\r\n]") ; search body
                        (setq body-start (match-beginning 0))))
                    (when (< beg end)                 ; found meta info
                      (let* ((lines-text (buffer-substring-no-properties beg end))
                             (lines (split-string lines-text "\r?\n" t))
                             prop)
                        (mapcar (lambda (line)
                                  (let ((meta (split-string line "[ :]" t)))
                                    `(,(car meta) . ,(cadr meta))))
                                lines))))))
         ;; get body content
         (body (buffer-substring-no-properties body-start (point-max)))
         (id (assocref "id" props))
         (title (assocref "title" props))
         (tags (assocref "tags" props)))
    (if (y-or-n-p (format "%s post %s? tags: %s."
                          (if id "Save" "Create") title tags))
        (tumblr-write-post
         tumblr-hostname
         `(("post-id" . ,id)
           ("title" . ,title)
           ("body" . ,body)
           ("tags" . ,tags))))))

(defun tumblr-new-post (title)
  (interactive "sCreate post title: \n")
  (switch-to-buffer (format "*Tumblr: %s*" title))
  (tumblr-insert-post-template title
                               '((slug . " ")
                                 (tags . " ")
                                 (state . "published")))
  (if (fboundp 'markdown-mode)
      (markdown-mode)
    (message "Recommand to apply markdown-mode for tumblr post writing.")))

(defun tumblr-mode-setup ()
  (kill-all-local-variables)
  (setq major-mode 'tumblr-mode)
  ;; (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq mode-name "tumblr-mode")
  (setq mode-line-buffer-identification
        (append (default-value 'mode-line-buffer-identification)
                '((format " [%s] " tumblr-hostname))))
  (make-local-variable 'tumblr-retrieve-posts-list)
  (use-local-map tumblr-mode-map)
  (run-hooks 'tumblr-mode-hook))

(if tumblr-mode-map
    (let ((tm tumblr-mode-map))
      (define-key tm (kbd "q") 'bury-buffer)
      nil))

(provide 'tumblr-mode)
;;; tumblr.el ends here
