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
   (renderer :accessor renderer)
   (comment-db :accessor comment-db)
   (feed :accessor the-feed)))

(defmethod register-plugin ((blog blog-handler) server)
  (with-slots (root) blog
    (let ((blog-static (make-instance 'whistle::whistle-static-file-handler :root (merge-pathnames "static/" root))))
      (add-url server "^/blog/$" blog :what :index)
      (add-url server "^/blog/feed.atom$" blog :what :feed)
      (add-url server "^/blog/(\\w+)/$" blog :what :by-category :category '$1)
      (add-url server "^/blog/(\\d+)/(\\d+)/(\\d+)/(.*)$" blog :what :article :year '$1 :month '$2 :date '$3 :name '$4)
      (add-url server "^/blog/(.*)" blog-static :path '$1)

      (add-url server "^/comments/spam/classify$" blog :what :classify-comment)
      (add-url server "^/comments/spam/declassify$" blog :what :declassify-comment)
      (add-url server "^/comments/spam/explain$" blog :what :explain-comment)
      (add-url server "^/comments/spam/db$" blog :what :spam-db)
      (add-url server "^/comments/spam/admin$" blog :what :spam-admin)
      (add-url server "^/comments/spam/admin/batch$" blog :what :spam-admin-batch)
      (add-url server "^/comments/(.*)$" blog :what :post-comment))))

(defmethod initialize-instance :after ((blog blog-handler) &key &allow-other-keys)
  (with-slots (root renderer feed comment-db) blog
    (setf root (truename (merge-pathnames root)))
    (setf renderer (second (assoc :renderer (file->list (merge-pathnames "config.sexp" root)))))
    (setf feed (parse-feed (merge-pathnames "content/feed.sexp" root)))
    (setf comment-db (open-comments-db (merge-pathnames "comments/" root) #'extract-features))))


(defgeneric render-page (renderer page feed &key &allow-other-keys)
  (:documentation "Render one of the blog pages."))

(defmethod generate-response ((blog blog-handler) request &key what year month date name category)
  (let ((feed (the-feed blog)))
    (ecase what
      (:feed
       (with-response-body (s request :content-type "application/atom+xml")
         (with-html-output (s) (feed feed))))

      (:index
       (with-response-body (s request)
         (with-html-output (s)
           (render-page (renderer blog) 'index-page feed))))

      (:by-category
       (with-response-body (s request)
         (with-html-output (s)
           (render-page (renderer blog) 'category-page feed :category category))))

      (:article
       (destructuring-bind (year month date) (mapcar #'parse-integer (list year month date))
         (let ((entry (find-entry feed year month date name)))
           (with-response-body (s request)
             (with-html-output (s)
               (render-page (renderer blog) 'article-page feed
                            :blog blog
                            :entry entry
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

      (:spam-admin-batch
       (spam-admin/next-batch blog request)))))

(defun find-entry (feed year month date name)
  (let ((key (list year month date name)))
    (find key (entries feed) :key #'entry-key :test #'equal)))

(defun entry-key (entry)
  (when entry
    (with-accessors ((file file) (published published)) entry
      (with-time (year month date) published
        (let ((key (list year month date (pathname-name file))))
          key)))))