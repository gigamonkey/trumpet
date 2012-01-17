;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :trumpet)

;; Given a directory containing the feed.sexp and entries.sexp files
;; and a content/ directory feed up.
;;
;; 1. The Atom feed.
;;
;; 2. A main HTML page with a certain number of articles.
;;
;; 3. Permalink pages for individual articles.
;;
;; 4. A list of archived articles in various ways.
;;
;; Should also provide commenting on individual articles (with
;; information about the number of comments on the main page).

(defclass blog-handler ()
  ((root :initarg :root :accessor root)
   (comment-db :accessor comment-db)
   (feed :accessor the-feed)))

(defmethod initialize-instance :after ((blog blog-handler) &key &allow-other-keys)
  (with-slots (feed root comment-db) blog
    (setf root (merge-pathnames root))
    (setf feed (parse-feed (merge-pathnames "content/feed.sexp" root)))
    (setf comment-db (open-comments-db (merge-pathnames "comments/" root) #'extract-features))))

(defmethod generate-response ((blog blog-handler) request &key what year month date name category)
  (let ((feed (the-feed blog)))
    (ecase what
      (:feed
       (with-response-body (s request :content-type "application/atom+xml")
         (with-html-output (s) (feed feed))))

      (:index
       (with-response-body (s request)
         (with-html-output (s)
           (index-page
            :title (title feed)
            :feed-url (feed-url feed)
            :entries (entries feed)
            :categories (categories feed)))))

      (:by-category
       (with-response-body (s request)
         (with-html-output (s)
           (index-page
            :title (title feed)
            :feed-url (feed-url feed)
            :entries (entries feed)
            :category category
            :categories (categories feed)))))

      (:article
       (destructuring-bind (year month date) (mapcar #'parse-integer (list year month date))
         (let ((entry (find-entry feed year month date name)))
           (with-response-body (s request)
             (with-html-output (s)
               (article-page
                :blog blog
                :blog-title (title feed)
                :feed-url (feed-url feed)
                :title (title entry)
                :file (file entry)
                :published (published entry)
                :updated (updated entry)
                :path (request-path request)
                :user-name (cookie-value "comment_name" request)))))))

      (:post-comment (post-comment blog request))

      (:classify-comment (spam-classifier blog request))

      (:declassify-comment (spam-declassifier blog request))

      (:explain-comment (spam-explainer blog request))

      (:spam-db (spam-db blog request))

      (:spam-admin
       (with-response-body (s request)
         (with-html-output (s)
           (spam-admin-page))))

      (:spam-admin-batch (spam-admin/next-batch blog request)))))

(defun find-entry (feed year month date name)
  (let ((key (list year month date name)))
    (find key (entries feed) :key #'entry-key :test #'equal)))

(defun entry-key (entry)
  (when entry
    (with-accessors ((file file) (published published)) entry
      (with-time (year month date) published
        (let ((key (list year month date (pathname-name file))))
          key)))))