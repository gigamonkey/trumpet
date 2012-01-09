;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :horn)

(defun comment-submit-url (request-path)
  (format nil "/comments~a" request-path))

(defun comment-form (path &optional name)
  "Render a form for submitting a comment, optionally providing a user
name to pre-populate the Name input."
  (html
    ((:form :id "comment-form" :method "post" :action (:print (comment-submit-url path)))
     (:p
      (:label :for "comment-name" "Name (required)")(:br)
      (:input :id "comment-name" :type "text" :name "name" :tabindex "1" :value (:print (or name "")))
      (:input :type "checkbox" :name "remember_me" :value "yes" :checked "on") "Remember me")
     (:p (:label :for "comment-text" "Comment")(:br)
         (:textarea :id "comment-text" :name "text" :tabindex 4))
     (:p (:input :type "submit" :value "Submit")))))

(defun render-comments (handler request-path administrator-p)
  "Render all the non-spam comments for the given request-path."
  (with-slots (comment-db) handler
    (let ((comments (comments-for-path comment-db request-path :filter #'exclude-probable-spam)))
      (setf comments (sort comments #'< :key #'utc))
      (html
        ((:div :class "comments")
         ((:h2 :class "comment-count")
          (:format "~[No comments yet~:;~:*~@(~r~) comment~:p~]" (length comments)))
         ((:div :id "comments")
          (loop for comment in comments
             for i from 0
             do (emit-comment comment i administrator-p))))))))

(defun emit-comment (comment i administrator-p)
  (let ((comment-id (id comment))
        (div-id (format nil "spamspan_~d" i)))
    (multiple-value-bind (classification spamminess explicit) (spamminess comment)
      (with-slots (utc data) comment
        (destructuring-bind (&key name text &allow-other-keys) data
          (html
            ((:div :id div-id :class (:format "comment ~[even~;odd~]" (mod i 2)))
             (:p :class "comment-author" name)
             ((:p :class "comment-date")
              (:span :class "actual-date" (:print (format-comment-time utc)))
              " "
              (:span :class "time-ago" (:format "(~a)" (time-ago utc))))
             ((:div :class "comment-text")
              (loop for p in (paragraphize text) do (emit-html p)))
             (when (and administrator-p (not explicit))
               (html
                 ((:div :class "comment-footer")
                  ((:span :class "classifiers")
                   ((:a :href (:format "javascript:explain('~a', '~a')" comment-id div-id))
                    (:span :class "spamminess" (:format "(Spamminess: ~,2f)" spamminess))) " "
                   "Mark as: "
                   ((:a :class (:print (if (eql classification :spam) "predicted" "not-predicted"))
                        :href (:format "javascript:classify('spam', '~a', '~a')" comment-id div-id))
                    "Spam"))
                   " | "
                  ((:a :class (:print (if (eql classification :ham) "predicted" "not-predicted"))
                       :href (:format "javascript:classify('ham', '~a', '~a')" comment-id div-id))
                   "Ham")))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handlers

;;; Accept a new comment

(defun for-page (request-path)
  (subseq request-path (position #\/ request-path :start 1)))

(defun post-comment (handler request)
  ;; Comments for page /foo/bar/baz/ are submitted to
  ;; /comments/foo/bar/baz/. We store the comment and then redirect to
  ;; the corresponding page.
  (let ((path (for-page (request-path request)))
        (name (parameter "name" request))
        (text (parameter "text" request)))

    ;; Save the comment in the filesystem.
    (save-comment
     (comment-db handler)
     (list :text text :name name :cookies (cookies-in request))
     path)

    ;; Set or clear the cookie that stores the user's name.
    (if (string= (parameter "remember_me" request) "yes")
        (set-cookie "comment_name" request :value name :path "/")
        (set-cookie "comment_name" request :path "/" :expires 0))

    ;; And redirect the user to the
    (redirect request path)))

;;; AJAX thingy for classifying a comment and returning a JSON snip
;;; saying that it has been classified.

;;; AJAX thingy for getting an explanation of the spam score of a
;;; comment.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic formatting -- turning submitted text into Markup and
;;; generating dates in various ways.

(defun paragraphize (string &optional text-limit)
  "Make text into paragraphs, optionally limiting the amount of text shown."
  (when (and text-limit (< text-limit (length string)))
    (setf string (format nil "~a ..." (subseq string 0 text-limit))))
  (setf string (regex-replace-all "\\r\\n"  string (string #\Newline)))
  (mapcar #'(lambda (x) (list :p x)) (split "[\\r\\n]{2,}" string)))

(defun format-comment-time (time-value)
  (with-time (date month year hour minute zone) time-value
    (with-output-to-string (s)
      (format s "~d ~a ~4,'0d ~d:~2,'0d ~:[am~;pm~] ~a"
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
                (format nil "~{~[~*~:;~:*~:d ~(~a~2:*~p~*~)~^, ~]~} ago"
                        (loop for (amount unit) on
                             (list weeks :week
                                   days :day
                                   hours :hour
                                   minutes :minute
                                   seconds :second)
                           by #'cddr
                           if (plusp amount) collect amount and collect unit)))))))))