;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :trumpet)

(defparameter *google-analytics-code*
  "var _gaq=[['_setAccount','UA-23885112-1'],['_trackPageview'],['_trackPageLoadTime']];
    (function(d,t){var g=d.createElement(t),s=d.getElementsByTagName(t)[0];g.async=1;
    g.src=('https:'==location.protocol?'//ssl':'//www')+'.google-analytics.com/ga.js';
    s.parentNode.insertBefore(g,s)}(document,'script'));")

(defparameter *ie-gunk*
  "<!--[if lt IE 9]><script src=\"//html5shim.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->")

(define-html-macro stylesheet (href)
  `(:link :href ,href :media "all" :rel "stylesheet" :type "text/css"))

(define-html-macro javascript (src)
  `(:script :src ,src :type "text/javascript"))

(define-html-macro header ()
  `((:header)
    ((:a :href "/blog/")
     ((:hgroup :class "branding")
      (:h1 "A billion monkeys can't be wrong")
      (:h2 "If you put a billion monkeys at a billion typewriters ...")))
    #+(or)(:nav
     #+(or)(:ul
      (:li (:a :href "/about/" "About"))
      (:li (:a :href "/write/" "Submissions")))
     (:input :type "text" :placeholder "Search" :class "search"))))

(define-html-macro footer ()
  `(:footer (:p "Copyright 2007-2012, Peter Seibel")))

(define-html-macro sidebar ()
  `((:aside :class "sidebar")
    (:h1 "Me")
    (:p (:img :src "/img/headshot.jpg"))
    (:p (:a :href "mailto:peter@gigamonkeys.com" "peter@gigamonkeys.com"))
    (:p (:a :href "http://twitter.com/peterseibel" "twitter: peterseibel"))
    (:p (:a :href "http://github.com/gigamonkey" "github: gigamonkey"))
    (:h1 "My books")
    (:p (:img :src "/img/pcl-cover.gif"))
    (:p (:img :src "/img/coders-at-work-cover.gif"))
    (:h1 "Categories")
    (:h2 (:a :href "/blog/writing/" "writing"))
    (:h2 (:a :href "/blog/parenthood/" "parenthood"))
    (:h2 (:a :href "/blog/lisp/" "lisp"))))

(define-html-macro standard-page ((&key
                                   author
                                   description
                                   title
                                   sidebar
                                   (extra-head '(:progn)))
                                  &body body)
  `(:progn
     (:noescape "<!doctype html>")
     (:newline)
     ((:html :lang "en")
      (:head
       (:meta :http-equiv "content-type" :content "text/html; charset=UTF-8")
       (:meta :http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1")
       (:meta :name "description" :content ,description)
       (:meta :name "author" :content ,author)
       (:title ,title)
       (stylesheet "http://fonts.googleapis.com/css?family=Droid+Serif:regular,italic,bold&v1")
       (stylesheet "/css/new-blog.css")
       (:noescape *ie-gunk*)
       ,extra-head)
      (:body
       ((:div :class "wrap")
        (header)
        (when ,sidebar (html (sidebar)))
        (:div :class "contents"
        ,@body)
        (footer))
       (:script :type "text/javascript" (:noescape  *google-analytics-code*))))))

(define-template index-page (title feed-url entries categories &optional category)
  "The main HTML page of the blog."
  (standard-page (:title title
                         :extra-head
                         (:progn (:link :rel "alternate" :type "application/atom+xml" :title title :href feed-url))
                         :author "Peter Seibel"
                         :description "Peter Seibel's blog"
                         :sidebar t)
                 (dolist (entry entries)
                   (when (or (not category) (member category (categories entry) :test #'string=))
                     (with-time (year month date) (published entry)
                       (article-html
                        :title-link (format nil "/blog/~a" (permalink (pathname-name (file entry)) year month date))
                        :file (file entry)
                        :title (title entry)
                        :published (published entry)
                        :updated (updated entry)))))))


(define-template article-page (blog-title feed-url title file published updated blog path user-name)
  "One article."
  (declare (ignore blog-title feed-url))
  (standard-page (:title (:print (just-text title))
                         :author "Peter Seibel"
                         :description "Peter Seibel's blog"
                         :sidebar t)
    (article-html
     :title title
     :file file
     :published published
     :updated updated)
    ((:div :class "comments")
     (article-comments
      :comments (sort (collect-comments (comment-db blog) :path path) #'< :key #'utc))
     (:h2 "Leave a comment")
     (comment-form
      :submit-to (format nil "/comments~a" path)
      :user-name user-name))))

(define-template article-html (title file published updated &optional title-link)
  "A single article."
  (:article
   (if title-link
       (html (:h2 (:a :href title-link (emit-html title))))
       (html (:h2 (emit-html title))))
   (:div :class "dateline" (human-date published))
   (render-body file)
   #+(or)(:div :class "updated" (:i "Last updated " (:print (format-iso-8601-time updated)) "."))))

(define-template article-comments (comments)
  (:h2
   (:format "~[No comments yet~:;~:*~@(~r~) comment~:p~]" (length comments)))
   ((:div :id "comments")
    (loop for comment in comments
       for even-p = t then (not even-p)
       do
         (article-comment
          :even-p even-p
          :name (getf (data comment) :name)
          :utc (utc comment)
          :text (getf (data comment) :text)))))

(define-template article-comment (even-p name utc text)
  ((:div :class (:format "comment ~[even~;odd~]" even-p))
   (:p :class "comment-author" name)
   ((:p :class "comment-date")
    (:span :class "actual-date" (format-comment-time utc))
    " "
    (:span :class "time-ago" "(" (time-ago utc) ")"))
   ((:div :class "comment-text")
    (loop for p in (paragraphize text) do (emit-html p)))))

(define-template comment-form (submit-to user-name)
  ((:form :id "comment-form" :method "post" :action submit-to)
   (:p
    (:label :for "comment-name" "Name (required)")(:br)
    (:input :id "comment-name" :type "text" :name "name" :tabindex "1" :value (:print (or user-name "")))
    (:input :type "checkbox" :name "remember_me" :value "yes" :checked "on") "Remember me")
   (:p (:label :for "comment-text" "Comment")(:br)
       (:textarea :id "comment-text" :name "text" :tabindex 4))
   (:p (:input :type "submit" :value "Submit"))))

(define-template spam-admin-page ()
  "Render the admin page which will request a batch of comments via Javascript."
  (:html
    (:head
     (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
     (:title "Spam Admin")
     (:script :src "/js/jquery-1.7.1.js")
     (:script :src "/js/spam.js")
     (:link :rel "stylesheet" :type "text/css" :href "/css/blog.css"))
    (:body
     (:h1 :id "header" "Comments")
     (:div :id "comments"))))

(define-template spam-admin-comment-html (id name text utc spamminess classification)
  ((:div :id id :class "comment")
   ((:div :class "comment-footer")
    ((:span :class "classifiers")
     (:span :class "classification" (:print (string-capitalize classification)))
     " "
     (:span :class "spamminess" (:format "(~,2f)" spamminess))
     " "
     (:span :class "comment-id" id)))
   (:p :class "comment-author" name)

   ((:p :class "comment-date")
    (:span :class "actual-date" (format-comment-time utc))
    " "
    (:span :class "time-ago" "(" (time-ago utc) ")"))
   ((:div :class "comment-text")
    (loop for p in (paragraphize text) do (emit-html p)))))

(define-template spam-db-page (title trained-count untrained-count features)
  (:html
    (:head
     (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
     (:title title)
     (:link :rel "stylesheet" :type "text/css" :href "/css/blog.css"))
    (:body
     (:h1 title)
     ((:div :class "spam-explanation")
      (:h1 (:format "~d feature~:p (~:d trained; ~:d untrained)"
                    (+ trained-count untrained-count) trained-count untrained-count))
      (feature-table :features features)))))

(define-template spam-explanation-snippet (classification score features)
  "The snippet of HTML sent back to explain the score of a comment on
the spam admin page."
  ((:div :class "spam-explanation")
   (:h1 (:format "Classification: ~a; score: ~,5f~%" classification score))
   (feature-table :features features)
   (:p :class "click-instructions" "Click anywhere in box to hide.")))

(define-template feature-table (features)
  "A table of spam features and their statistics."
  (:table
   (:tr
    (:th "Type")
    (:th "Value")
    (:th "# Hams")
    (:th "# Spams")
    (:th "Spamminess"))
   (loop for (feature . spamminess) in (features-with-spamminess features)
      do (with-slots (id hams spams) feature
           (html
             (:tr
              (:td (:format "~a" (first id)))
              (:td (:format "~a" (limit-string (format nil "~s" (unlist (rest id))) 40)))
              (:td hams)
              (:td spams)
              (:td (:format "~:[-~;~:*~,f~]" spamminess))))))))

(defun features-with-spamminess (features)
  (sort
   (loop for f in features collect (cons f (feature-spamminess f)))
   #'(lambda (a b) (> (or a 0) (or b 0)))
   :key #'cdr))

(defun limit-string (s limit)
  (cond
    ((<= (length s) limit) s)
    ((> limit 3)
     (format nil "~a..." (subseq s 0 (- limit 3))))
    (t (subseq s 0 limit))))

;;; Small bits of formatted output

(defun human-date (utc)
  (with-time (year month date hour minute day) utc
    (let ((pm-p (> hour 12)))
      (html
        (:format
         "~a, ~d ~a ~d, ~d:~2,'0d ~:[am~;pm~] in Berkeley, California, USA"
         (day-name day) date (month-name month) year (mod hour 12) minute pm-p)))))

(defun format-comment-time (time-value)
  (with-time (date month year hour minute zone) time-value
    (html
      (:format "~d ~a ~4,'0d ~d:~2,'0d ~:[am~;pm~] ~a"
                date (month-name month) year (mod hour 12) minute (> hour 12)
                (reverse-translate-zone (- zone))))))

(defun time-ago (utc)
  (let ((seconds (- (now) utc)))
    (if (zerop seconds)
        "now"
        (multiple-value-bind (minutes seconds) (floor (round seconds) 60)
          (multiple-value-bind (hours minutes) (floor minutes 60)
            (multiple-value-bind (days hours) (floor hours 24)
              (multiple-value-bind (weeks days) (floor days 7)
                (html
                  (:format "~{~[~*~:;~:*~:d ~(~a~2:*~p~*~)~^, ~]~} ago"
                           (loop for (amount unit) on
                                (list weeks :week
                                      days :day
                                      hours :hour
                                      minutes :minute
                                      seconds :second)
                                by #'cddr
                                if (plusp amount) collect amount and collect unit))))))))))


;;; Simple minded comment rendering.

(defun paragraphize (string &optional text-limit)
  "Make text into paragraphs, optionally limiting the amount of text shown."
  (when (and text-limit (< text-limit (length string)))
    (setf string (format nil "~a ..." (subseq string 0 text-limit))))
  (setf string (regex-replace-all "\\r\\n"  string (string #\Newline)))
  (mapcar #'(lambda (x) (list :p x)) (split "[\\r\\n]{2,}" string)))
