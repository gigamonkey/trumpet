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
      (with-slots (utc path data) comment
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
;;; saying that it has been classified. The page will submit the
;;; comment's path, identifier, and current classification (usually
;;; "new") as well as an id of the div in the page containing the
;;; comment. If the comment is still to be found under the given
;;; classification, we reclassify it and send back a snip of JSON data
;;; specifying the new classification and the div id so the page can
;;; adjust the display.

(defun spam-classifier (blog request)
  "Classify a new comment."
  (with-parameters ((as keyword)
                    (comment_id string)
                    (div_id string)) request
    (let ((comment (find-comment (comment-db blog) comment_id)))
      (classify-comment comment as)
      (with-response-body (s request :content-type "application/json")
        (let ((resp `(:as ,(string-downcase as) :div_id ,div_id)))
          ;;(break "About to send json response ~a" (json resp))
          (write-json resp s))))))


;;; AJAX thingy for getting an explanation of the spam score of a
;;; comment.

(defun features-with-spamminess (features spam)
  (loop for f in features collect (cons f (spam::bayesian-spam-probability f spam))))

(defun spam-explainer (blog request)
  (with-slots (comment-db) blog
    (with-slots (spam) comment-db
      (with-parameters ((comment_id string)) request
        (let* ((comment (find-comment comment-db comment_id))
               (features (remove-if #'spam::untrained-p (intern-features comment))))
          (multiple-value-bind (classification score) (spamminess comment)
            (with-response-body (s request)
              (with-html-output (s)
                ((:div :class "spam-explanation")
                 (:h1 (:format "Classification: ~a; score: ~,5f~%" classification score))
                 (if features
                     (feature-table (features-with-spamminess features spam))
                     (html (:h2 "No features of message found in database")))
                 (:p :class "click-instructions" "Click anywhere in box to hide."))))))))))


(defun spam-db (blog request)
  (let ((trained-features ())
        (trained-count 0)
        (untrained-count 0)
        (title "Spam DB")
        (spam (spam (comment-db blog))))
    (loop for f being the hash-values of (spam::features spam)
       when (spam::untrained-p f) do
         (incf untrained-count)
       else do
         (push f trained-features)
         (incf trained-count))

    (with-response-body (s request)
      (with-html-output (s)
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
            (feature-table (features-with-spamminess trained-features spam)))))))))


(defun spam-admin (handler request)
  "Render all the non-spam comments for the given request-path."
  (with-slots (comment-db) handler
    (multiple-value-bind (ham spam unsure)
        (loop for c in (sort (comments-for-category comment-db :new) #'> :key #'utc)
           for prediction = (spamminess c)
           when (eql prediction :ham) collect c into ham
           when (eql prediction :spam) collect c into spam
           when (eql prediction :unsure) collect c into unsure
           finally (return (values ham spam unsure)))
      (let ((i 0))
        (with-response-body (s request)
          (with-html-output (s)
            (:html
              (:head
               (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
               (:title "Spam Admin")
               (:script :src "/js/jquery-1.7.1.js")
               (:script :src "/js/spam.js")
               (:link :rel "stylesheet" :type "text/css" :href "/css/blog.css"))
              (:body
               (:h1 "Ham" (:format " (~:d comment~:p)" (length ham)))
               (loop for comment in ham do (emit-comment/spam-admin comment (incf i)))

               (:h1 "Unsure" (:format " (~:d comment~:p)" (length unsure)))
               (loop for comment in unsure do (emit-comment/spam-admin comment (incf i)))

               (:h1 "Spam" (:format " (~:d comment~:p)" (length spam)))
               (loop for comment in spam do (emit-comment/spam-admin comment (incf i)))))))))))

(defun emit-comment/spam-admin (comment i)
  (let ((comment-id (id comment))
        (div-id (format nil "spamspan_~d" i)))
    (multiple-value-bind (classification spamminess) (spamminess comment)
      (with-slots (utc path data) comment
        (destructuring-bind (&key name text &allow-other-keys) data
          (html
            ((:div :id div-id :class (:format "comment ~[even~;odd~]" (mod i 2)))
             (:p :class "comment-author" name)
             ((:p :class "comment-date")
              (:span :class "actual-date" (:print (format-comment-time utc)))
              " "
              (:span :class "time-ago" (:format "(~a)" (time-ago utc))))
             ((:p :class "comment-path") "On: " path)
             ((:div :class "comment-text")
              (loop for p in (paragraphize text 200) do (emit-html p)))
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
               "Ham")))))))))

(defun feature-table (features-with-spamminess)
  (html
    (:table
     (:tr
      (:th "Type")
      (:th "Value")
      (:th "# Hams")
      (:th "# Spams")
      (:th "Spamminess"))
     (dolist (f (sort features-with-spamminess #'> :key #'cdr))
       (destructuring-bind (feature . spamminess) f
         (with-slots (spam::id spam::hams spam::spams) feature
           (html
             (:tr
              (:td (:format "~a" (first spam::id)))
              (:td (:format "~s" (unlist (rest spam::id))))
              (:td spam::hams)
              (:td spam::spams)
              (:td (:format "~,f" spamminess))))))))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spam features

(defun extract-features (comment)
  (destructuring-bind (&key name text cookies &allow-other-keys) (data comment)
    (let ((features ()))
      (push `(:name ,name) features)
      (loop for word in (extract-words text) do (push `(:word ,word) features))
      (loop for (key . value) in cookies do (push `(:cookie ,key ,value) features))
      features)))

(defparameter *word-regex* (create-scanner "[a-z]{3,}" :case-insensitive-mode t))

(defun extract-words (text)
  "Simple function to extract words from a text."
  (delete-duplicates
   (mapcar #'string-downcase (all-matches-as-strings *word-regex* text))
   :test #'string=))
