;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :trumpet)

(define-template spam-admin-page ()
  "Render the admin page which will request a batch of comments via Javascript."
  (:html
    (:head
     (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
     (:title "Spam Admin")
     (:script :src "/blog/js/jquery-1.7.1.js")
     (:script :src "/blog/js/spam.js")
     (:link :rel "stylesheet" :type "text/css" :href "/blog/css/spam.css"))
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
