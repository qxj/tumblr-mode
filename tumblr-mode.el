;;; tumblr-mode.el --- Major mode for Tumblr
;;
;; Copyright (C) 2011, 2012 Julian Qian
;;
;; Author: Julian Qian <junist@gmail.com>
;; Created: Dec 25, 2011
;; Version: 0.2
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; tumblr-mode.el is a major mode for tumblr.com.
;;
;; You can write and manage your tumblr posts in Emacs with this mode.
;; It only supports to create regular(text) type in tumblr.com now,
;; because I think it's the most suitable type that can be edited in
;; Emacs.
;;
;; - `tumblr-new-post' create a buffer to post
;; - `tumblr-save-post' save buffer to tumblr.com
;; - `tumblr-list-posts' list tumblr posts in a buffer
;;
;;; Install:
;;
;; Download and put `tumblr-mode.el' into your load-path, then use it in
;; your ~/.emacs:
;;
;; (require 'tumblr-mode)
;;

;;; Code:

(require 'url)
(require 'xml)

(defgroup tumblr-mode nil
  "A major mode for Tumblr.com")

(defcustom tumblr-hostnames '()
  "a list containing all your tumblr hosts, eg:

\(setq tumblr-hostnames
      '\(\"myblog.tumblr.com\" \"myphoto.tumblr.com\"\)\)"
  :type '(repeat string)
  :group 'tumblr-mode)
(defcustom tumblr-email ""
  "tumblr login email"
  :type 'string
  :group 'tumblr-mode)
(defcustom tumblr-password ""
  "tumblr login password"
  :type 'string
  :group 'tumblr-mode)
(defcustom tumblr-post-type "text"
  "tumblr post type"
  :type 'string
  :group 'tumblr-mode)
(defcustom tumblr-post-format "markdown"
  "tumblr post format"
  :type 'string
  :group 'tumblr-mode)
(defcustom tumblr-retrieve-posts-num-total 20
  "the number of posts to be listed"
  :type 'integer
  :group 'tumblr-mode)

(defvar tumblr-current-hostname nil)
(defvar tumblr-current-tag nil)
(defvar tumblr-current-state nil)

(defvar tumblr-post-header-delimiters '("--" . "--")
  "A cons containing two flags, to indicate the header, aka. meta info section, of a post.

Default are two \"--\", but you can replace them with \"<!--\"
and \"-->\" for better markdown preview, eg:

(setq tumblr-post-header-delimiters '(\"<!--\" . \"-->\"))
")

(defvar tumblr-post-status '("published" "draft"))
(defvar tumblr-post-types '("text" "quote" "photo" "link" "chat" "video" "audio"))
(defvar tumblr-post-formats '("markdown" "html"))
(defvar tumblr-request-filters '("text" "none"))
(defvar tumblr-retrieve-posts-num-once 20)
(defvar tumblr-retrieve-posts-list nil)

(defvar tumblr-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "g") 'tumblr-list-posts)
    map)
  "keymap for `tumblr-mode'.")

(defvar tumblr-post-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-s") (lambda ()
                                      (interactive)
                                      (call-interactively 'tumblr-save-post)
                                      (set-buffer-modified-p nil)))
    map)
  "keymap for `tumblr-post-mode'.")

;; utilities
(defun assocref (item alist)
  (cdr (assoc item alist)))

(defun assqref (item alist)
  (cdr (assq item alist)))

(defun tumblr-format-timestamp (secs)
  (format-time-string "%F" (seconds-to-time (if (stringp secs)
                                                (string-to-number secs)
                                              secs))))

(defun tumblr-query-string (args)
  (mapconcat (lambda (arg)
               (concat (url-hexify-string (car arg))
                       "="
                       (url-hexify-string (cdr arg))))
             args "&"))

(defun tumblr-get-hostname ()
  (setq tumblr-current-hostname
        (if (and (listp tumblr-hostnames) (car tumblr-hostnames))
            (funcall (if (fboundp 'ido-completing-read)
                         'ido-completing-read
                       'completion-read)
                     "Choose hostname: " tumblr-hostnames nil nil nil nil
                     (car tumblr-hostnames))
          (read-string "Tumblr hostname: "))))

(defun tumblr-get-tag ()
  (setq tumblr-current-tag
        (read-string "Choose a tag: " nil '(tumblr-current-tag))))

(defun tumblr-get-state ()
  (setq tumblr-current-state
        (funcall (if (fboundp 'ido-completing-read)
                     'ido-completing-read
                   'completion-read)
                 "Choose state: " tumblr-post-status nil nil nil nil
                 (car tumblr-post-status))))

(defun tumblr-get-email ()
  (or tumblr-email (setq tumblr-email (read-string "Tumblr email: "))))

(defun tumblr-get-password ()
  (or tumblr-password (setq tumblr-password (read-passwd "Tumblr password: "))))

;; tumblr format
(defun tumblr-list-posts-format (width content)
  (let ((fmt (format "%%-%d.%ds" width (decf width))))
    (format fmt content)))

(defun tumblr-mode-line-title-format (title)
  (format "*tumblr: %s*" title))

;; http functions
(defun tumblr-authenticated-read-xml-root (hostname params)
  (let* ((url-request-method "POST")
         ;; (url-http-attempt-keepalives nil)
         (url-mime-charset-string "utf-8;q=0.7,*;q=0.7")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (tumblr-query-string
                            (append params
                                    `(("email" . ,(tumblr-get-email))
                                      ("password" . ,(tumblr-get-password))))))
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
  "Post data to HOSTNAME, PARAMS is alist containing request data."
  (let* ((url-request-method "POST")
         (url-max-redirecton -1)
         ;; (url-http-attempt-keepalives nil)
         (url-mime-charset-string "utf-8;q=0.7,*;q=0.7")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (tumblr-query-string
                            (append params
                                    `(("email" . ,(tumblr-get-email))
                                      ("password" . ,(tumblr-get-password))
                                      ("group" . ,hostname)
                                      ("type" . ,tumblr-post-type)
                                      ("format" . ,tumblr-post-format)
                                      ("generator" . "tumblr-mode.el")))))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))
    (url-retrieve
     "http://www.tumblr.com/api/write"
     (lambda (&rest args)
       (message "url-retrieve callback status")
       (let ((buffer (current-buffer)))
         (set-window-buffer nil buffer)
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
                        (post-id (buffer-substring beg end)))
                   ;; get post-id
                   (message "Post OK. return id: %s" post-id)))
                ((string= "403" status-code)
                 (message "Failed. Your email address or password were incorrect."))
                ((string= "400" status-code)
                 (message "Failed. There was at least one error while trying to save your post."))
                (t
                 (error (format "Unknown failure, maybe network exception :(")))))))
         (kill-buffer buffer))))))

;; tumblr functions
(defun tumblr-get-posts-count (hostname &optional tagged state)
  (let* ((root (tumblr-authenticated-read-xml-root
                hostname
                `(("num" . "1")
                  ("filter" . "text")
                  ("tagged" . ,tagged)
                  ("state" . ,state))))
         (tumblr (car root))
         (posts (car (xml-get-children tumblr 'posts)))
         (attrs (xml-node-attributes posts))
         (total (assqref 'total attrs)))
    (string-to-number total)))

(defun tumblr-list-posts-internal (hostname retrieving &optional tagged state)
  (let* ((root (tumblr-authenticated-read-xml-root
                hostname
                `(("num" . ,(number-to-string retrieving))
                  ("filter" . "text")
                  ("tagged" . ,tagged)
                  ("state" . ,state))))
         (tumblr (car root))
         (posts (car (xml-get-children tumblr 'posts)))
         (post-list (xml-get-children posts 'post)))
    (mapcar (lambda (post)
              (let* ((attrs (xml-node-attributes post))
                     (timestamp (assqref 'unix-timestamp attrs))
                     ;; (date (assqref 'date attrs))
                     (id (assqref 'id attrs))
                     (slug (assqref 'slug attrs))
                     (url (assqref 'url attrs))
                     (title (caddar (xml-get-children post 'regular-title)))
                     (tags (mapcar (lambda (tag) (caddr tag))
                                   (xml-get-children post 'tag))))
                ;; return a list of alist
                `((timestamp    .   ,timestamp)
                  (id           .   ,id)
                  (slug         .   ,slug)
                  (url          .   ,url)
                  (title        .   ,title)
                  (tags         .   ,tags))))
            post-list)))

(defun tumblr-list-posts (choose)
  "List all regular posts of your hostname.

Default hostname/tag/state can be specified with
`tumblr-default-hostname', `tumblr-default-tag' and
`tumblr-default-state'.

If \\[tumblr-list-posts] is called with a argument, other
hostname/tag/state can also be specified to override default
settings temporarily.

\\[tumblr-list-posts] retrieves posts from tumblr.com
synchronously, during this period, Emacs will seems to hang up
some minutes."
  (interactive "P")
  (let* ((hostname (if choose
                       (tumblr-get-hostname)
                     (or tumblr-current-hostname
                         (tumblr-get-hostname))))
         (tagged (if choose
                     (tumblr-get-tag)
                   (or tumblr-current-tag
                       (tumblr-get-tag))))
         (state (if choose
                    (tumblr-get-state)
                  (or tumblr-current-state
                      (tumblr-get-state))))
         (total (tumblr-get-posts-count hostname tagged state))
         (total-retrieving (or (if (> tumblr-retrieve-posts-num-total total)
                                   total
                                 tumblr-retrieve-posts-num-total) total))
         (remaining total-retrieving)
         (tumblr-retrieve-posts-list nil))
    ;; retrieve posts
    (while (> remaining 0)
      (let ((retrieving (if (> remaining tumblr-retrieve-posts-num-once)
                            tumblr-retrieve-posts-num-once
                          remaining)))
        (setq remaining (- remaining retrieving))
        (setq tumblr-retrieve-posts-list
              (append tumblr-retrieve-posts-list
                      (tumblr-list-posts-internal hostname retrieving tagged state)))))
    ;; list all posts
    (with-current-buffer (get-buffer-create "*tumblr-mode*")
      (goto-char (point-min))
      (save-excursion
        (kill-region (point-min) (point-max))
        (let ((title-len 48)            ; keep less than 80 columns
              (tags-len  20)
              (date-len  11))
          ;; header
          (save-excursion
            (insert (tumblr-list-posts-format title-len "Title"))
            (insert (tumblr-list-posts-format tags-len "Tags"))
            (insert (tumblr-list-posts-format date-len "Date"))
            (insert "\n"))
          (overlay-put (make-overlay (line-beginning-position) (line-end-position))
                       'face 'header-line)
          (forward-line)
          ;; list posts
          (mapc (lambda (post)
                  (insert (tumblr-list-posts-format
                           title-len (assqref 'title post)))
                  (insert (tumblr-list-posts-format
                           tags-len (mapconcat (lambda (tag) (format "#%s" tag))
                                               (assqref 'tags post) ", ")))
                  (insert (tumblr-list-posts-format
                           date-len (tumblr-format-timestamp (assqref 'timestamp post))))
                  (make-text-button (line-beginning-position) (line-end-position)
                                    'id (assqref 'id post)
                                    'group hostname
                                    'action 'tumblr-get-post-edit
                                    'face 'default)
                  (insert "\n"))
                tumblr-retrieve-posts-list)))
      ;; skip header
      (forward-line)
      (tumblr-mode hostname)
      (set-window-buffer nil (current-buffer)))

    (message "Retrieved total %d posts on %s, tagged %s, state: %s."
             total-retrieving hostname tagged state)))

(defun tumblr-get-post (post-id hostname)
  (let* ((root (tumblr-authenticated-read-xml-root
                hostname
                `(("id" . ,post-id)
                  ("filter" . "none"))))
         (tumblr (car root))
         (posts (car (xml-get-children tumblr 'posts)))
         (post (car (xml-get-children posts 'post)))
         (attrs (xml-node-attributes post))
         (title (caddar (xml-get-children post 'regular-title)))
         (body (caddar (xml-get-children post 'regular-body))) ; post content
         (tags (mapcar (lambda (tag) (caddr tag))
                       (xml-get-children post 'tag)))
         (buffer (get-buffer-create (tumblr-mode-line-title-format title))))
    ;; edit post
    (with-current-buffer buffer
      (goto-char (point-min))
      (save-excursion
        (kill-region (point-min) (point-max))
        ;; insert meta info, as octopress does
        (tumblr-insert-post-template title attrs tags hostname)
        ;; insert content
        (insert body))
      (tumblr-prepare-post-edit))
    (set-window-buffer nil buffer)))

(defun tumblr-insert-post-template (title &optional attrs-alist tags-list group)
  (if (car tumblr-post-header-delimiters)
      (insert (format "%s\n" (car tumblr-post-header-delimiters))))
  (let ((date (assqref 'date attrs-alist))
        (id (assqref 'id attrs-alist))
        (slug (assqref 'slug attrs-alist))
        (state (assqref 'state attrs-alist))
        (format (assqref 'format attrs-alist))
        (tags (mapconcat (lambda (tag) tag) tags-list ", ")))
    (and id (insert (format "id: %s\n" id)))
    (and title (insert (format "title: %s\n" title)))
    (and slug (insert (format "slug: %s\n" slug)))
    (and group (insert (format "group: %s\n" group)))
    (and tags (insert (format "tags: %s\n" tags)))
    (and format (insert (format "format: %s\n" format)))
    (and state (insert (format "state: %s\n" state)))
    (and date (insert (format "date: %s\n" date))))

  (if (cdr tumblr-post-header-delimiters)
      (insert (format "%s\n\n" (cdr tumblr-post-header-delimiters)))
    (error "cdr of `tumblr-post-header-delimiters' is a must,
otherwise it cannot identify what's header of the post.")))

(defun tumblr-get-post-edit (button)
  (tumblr-get-post (button-get button 'id) (button-get button 'group)))

(defun tumblr-save-post ()
  "Posting current buffer to tumblr.com. Below options are
accepted headers for posting:

- `title' Post's title, required
- `post-id' Post's identity, required when editing an existed post
- `group' The hostname the post will be posted to, it will
   override `tumblr-default-hostname', optional
- `tags' Tags seperated by comma, optional
- `slug' Slug for friendly url, optional
- `state' published/draft/submission/queue, optional
- `date' When the post is posted, optional

One example of the post buffer:

--
title: a post writed by tumblr-mode.el
group: test.tumblr.com
tags: tumblr, emacs
--

blah..blah..blah
"
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

                    ;; meta info begin point
                    (if (and (car tumblr-post-header-delimiters)
                             (search-forward-regexp
                              (format "%s\r?\n" (car tumblr-post-header-delimiters)) bound t))
                        (setq beg (match-end 0))
                      (setq beg (point-min)))

                    ;; meta info end point
                    (if (and (cdr tumblr-post-header-delimiters)
                             (search-forward-regexp
                              (format "%s\r?\n" (cdr tumblr-post-header-delimiters)) bound t))
                        (progn
                          (setq end (match-beginning 0))
                          )
                      (error "Can't found the delimiter of post's header and body"))

                    ;; meanwhile, we can get body start point
                    (if (search-forward-regexp "[^\r\n]" nil t)
                        (setq body-start (match-beginning 0))
                      (setq body-start (point)))

                    ;; found meta info
                    (if (and (< beg end)
                               (< end body-start))
                      (let* ((lines-text (buffer-substring-no-properties beg end))
                             (lines (split-string lines-text "\r?\n" t))
                             prop)
                        (mapcar (lambda (line)
                                  (when (string-match "\\s-*\\([^:]+\\)\\s-*:\\s-*\\(.+\\)" line)
                                    `(,(match-string 1 line)
                                      .
                                      ,((lambda (str) ; strip tail white space
                                          (if (string-match "\\s-+$" str)
                                              (replace-match "" t t str)
                                            str))
                                        (match-string 2 line)))))
                                lines))
                      (error "Failed to parse the post, maybe format is wrong.")))))
         ;; get body content
         (body (buffer-substring-no-properties body-start (point-max)))
         (id (assocref "id" props))
         (title (assocref "title" props))
         (tags (assocref "tags" props))
         (date (assocref "date" props))
         (group (assocref "group" props))
         (state (assocref "state" props))
         (slug (assocref "slug" props)))
    (when (y-or-n-p (format "%s post [ %s ]?%s"
                            (if id "Save" "Create")
                            title
                            (if tags (format " tags: %s." tags) "")))
      (tumblr-write-post
       (if (or (null group) (string= "" group))
           (tumblr-get-hostname)
         group)
       `(("post-id" . ,id)            ; WTF..api/read is "id", but api/write is "post-id"
         ("title" . ,title)
         ("body" . ,body)
         ("tags" . ,tags)
         ("date" . ,date)
         ("slug" . ,slug)
         ("state" . ,state)))
      (set-buffer-modified-p nil))))

(defun tumblr-prepare-post-edit ()
  (if (fboundp 'markdown-mode)
      (markdown-mode)
    (message "Recommand to apply markdown-mode for tumblr post writing."))
  (tumblr-post-mode 1)
  (set-buffer-modified-p nil))

(defun tumblr-new-post (title)
  (interactive "sCreate post title: \n")
  (switch-to-buffer (tumblr-mode-line-title-format title))
  (tumblr-insert-post-template title
                               '((slug . " ")
                                 (state . "published"))
                               nil (or tumblr-current-hostname (tumblr-get-hostname)))
  (tumblr-prepare-post-edit))

;; (put 'tumblr-mode 'mode-class 'special)

(defun tumblr-mode (&optional hostname)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'tumblr-mode
        mode-name "tumblr-mode"
        mode-line-buffer-identification
        (append (default-value 'mode-line-buffer-identification)
                `(,(format " [%s] " hostname))))
  (make-local-variable 'tumblr-retrieve-posts-list)
  (use-local-map tumblr-mode-map)
  (run-mode-hooks 'tumblr-mode-hook))

(define-minor-mode tumblr-post-mode
  "A minor mode for tumblr post, see \\[tumblr-new-post] and
\\[tumblr-save-post]"
  :init-value nil
  :lighter " Tumblr"
  :keymap tumblr-post-mode-map
  :group 'tumblr-mode)

(provide 'tumblr-mode)

;;; tumblr-mode.el ends here