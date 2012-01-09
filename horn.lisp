;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :horn)

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
;; Sholud also provide commenting on individual articles (with
;; information about the number of comments on the main page).

(defclass horn ()
  ((root :initarg :root :accessor root)
   (comment-db :accessor comment-db)
   (feed :accessor the-feed)))

(defmethod initialize-instance :after ((horn horn) &key &allow-other-keys)
  (with-slots (feed root comment-db) horn
    (setf feed (parse-feed (merge-pathnames "feed.sexp" root)))
    (setf comment-db (make-instance 'comments :root (merge-pathnames "comments/" root)))))

(defmethod generate-response ((horn horn) request &key what year month date name category)
  (let ((feed (the-feed horn)))
    (ecase what
      (:feed
       (with-response-body (s request :content-type "application/atom+xml")
         (with-html-output (s) (feed feed))))

      (:index
       (with-response-body (s request)
         (with-html-output (s) (render-index-html feed))))

      (:by-category
       (with-response-body (s request)
         (with-html-output (s) (render-index-html feed :category category))))

      (:article
       (destructuring-bind (year month date) (mapcar #'parse-integer (list year month date))
         (with-response-body (s request)
           (with-html-output (s)
             (render-article-html
              horn
              feed year month date name
              (request-path request)
              (let ((x (cookie-value "comment_name" request)))
                x))))))

      (:post-comment (post-comment horn request)))))

(defun render-index-html (feed &key category)
  (with-accessors ((title title) (entries entries) (feed-url feed-url)) feed
    (html
      (:html
        (:head
         (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
         (:title title)
         (:link :rel "stylesheet" :type "text/css" :href "/css/blog.css")
         (:link :rel "alternate" :type "application/atom+xml" :title title :href feed-url))
        (:body
         (:h1 title)
         (dolist (entry entries)
           (when (or (not category) (member category (categories entry) :test #'string=))
             (with-accessors ((file file)
                              (title title)
                            (published published)
                              (updated updated)
                              (categories categories))
                 entry
               (with-time (year month date) published
                 (html
                   (:a :href (:format "/blog/~a" (permalink (pathname-name file) year month date)) (:h2 (emit-html title)))
                   (:p :class "dateline" (:print (human-date published)))
                   (render-body file)
                   (:p :class "updated" (:i "Last updated " (:print (format-iso-8601-time updated)) "."))))))))))))

(defun render-article-html (horn feed year month date name path user-name)
  (with-accessors ((blog-title title) (feed-url feed-url) (index-url index-url)) feed
    (with-accessors ((file file)
                     (title title)
                     (published published)
                     (updated updated)
                     (categories categories)
                     (body body))
        (find-entry feed year month date name)
      (html
        (:html
          (:head
           (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
           (:title (:print (just-text title)))
           (:link :rel "stylesheet" :type "text/css" :href "/css/blog.css")
           (:link :rel "alternate" :type "application/atom+xml" :href feed-url))
          (:body
           (:a :href "/blog/" (:h1 blog-title))
           (:h2 (emit-html title))
           (:p :class "dateline" (:print (human-date published)))
           (render-body file)
           (:p :class "updated" (:i "Last updated " (:print (format-iso-8601-time updated)) "."))
           (render-comments horn path t)
           (comment-form path user-name)))))))

(defun human-date (utc)
  (with-time (year month date hour minute day) utc
    (let ((pm-p (> hour 12)))
      (format nil "~a, ~d ~a ~d, ~d:~2,'0d ~:[am~;pm~] in Berkeley, California, USA"
              (day-name day) date (month-name month) year (mod hour 12) minute pm-p))))

(defun find-entry (feed year month date name)
  (let ((key (list year month date name)))
    (find key (entries feed) :key #'entry-key :test #'equal)))

(defun entry-key (entry)
  (when entry
    (with-accessors ((file file) (published published)) entry
      (with-time (year month date) published
        (let ((key (list year month date (pathname-name file))))
          key)))))