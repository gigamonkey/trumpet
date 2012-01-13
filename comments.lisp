;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :trumpet)

(defparameter *admin-sessions* (make-hash-table))

(defclass admin-session ()
  ((unsent :initarg :unsent :accessor unsent)
   (last-hit :initform (get-universal-time) :accessor last-hit)))

(defun post-comment (blog request)
  "Handler for the comment submit form. Comments for page
/foo/bar/baz/ are submitted to /comments/foo/bar/baz/. We store the
comment and then redirect to the corresponding page where the comment
will be displayed unless it is classified as spam."
  (let ((path (for-page (request-path request)))
        (name (parameter "name" request))
        (text (parameter "text" request)))

    ;; Save the comment in the filesystem.
    (save-comment
     (comment-db blog)
     (list :text text :name name :cookies (cookies-in request))
     path)

    ;; Set or clear the cookie that stores the user's name.
    (if (string= (parameter "remember_me" request) "yes")
        (set-cookie "comment_name" request :value name :path "/")
        (set-cookie "comment_name" request :path "/" :expires 0))

    ;; And redirect the user to the
    (redirect request path)))

(defun for-page (request-path)
  (subseq request-path (position #\/ request-path :start 1)))

(defun spam-classifier (blog request)
  "Handler for AJAX requests to classify a comment. Responds with a
bit of JSON the admin page can use to remove the comment from the
page."
  (with-parameters ((as keyword) (id string)) request
    (when-let ((comment (find-comment (comment-db blog) id)))
      (classify-comment comment as)
      (with-response-body (s request :content-type "application/json")
        (write-json `(:as ,(string-downcase as) :id ,id) s)))))

(defun spam-declassifier (blog request)
  "Handler for AJAX requests to declassify a comment. Responds with a
snippet of HTML containing the comment so it can be put back on the
page."
  (with-parameters ((id string)) request
    (when-let ((comment (find-comment (comment-db blog) id)))
      (declassify-comment comment)
      (with-response-body (s request)
        (with-html-output (s)
          (multiple-value-bind (prediction score) (spamminess comment)
            (emit-spam-admin-comment-html comment score prediction)))))))

(defun spam-explainer (blog request)
  "Handler for AJAX requests to get the explanation of the spam score
of a single comment. Responds with HTML to be displayed."
  (let ((comment (find-comment (comment-db blog) (parameter "id" request))))
    (multiple-value-bind (classification score) (spamminess comment)
      (with-response-body (s request)
        (with-html-output (s)
          (spam-explanation-snippet
           :classification classification
           :score score
           :features (spam-features comment)))))))

(defun spam-db (blog request)
  "Handler to generate the spam database page."
  (loop with title = "Spam DB"
     for f in (all-spam-features (comment-db blog))
     when (spam::untrained-p f)
       count t into untrained-count
     else
       count t into trained-count and
       collect f into features
     finally
     (with-response-body (s request)
       (with-html-output (s)
         (spam-db-page
          :title title
          :trained-count trained-count
          :untrained-count untrained-count
          :features features)))))

(defun spam-admin/next-batch (handler request)
  "Render all the non-spam comments for the given request-path."
  (with-parameters ((session keyword)
                    (n integer)) request
    (let ((admin-session (gethash session *admin-sessions*))
          (db (comment-db handler)))
      (unless admin-session
        (setf admin-session
              (setf (gethash session *admin-sessions*)
                    (make-instance 'admin-session
                      :unsent (collect-comments db :classification :new)))))

      (with-slots (comment-db) handler
        (with-response-body (out request)
          (with-html-output (out)
            (:div
             (loop for (comment spamminesss classification) in (next-n-comments n admin-session)
                do (emit-spam-admin-comment-html comment spamminesss classification)))))))))

(defun next-n-comments (n admin-session)
  ;; Keep a list of unsent comments in the session. Each time we
  ;; request a batch of comments sort the unsent comments and send the
  ;; next N leaving unsent set to the remainder.
  (let ((comments-with-scores (comments-with-scores (unsent admin-session))))
    (let ((to-keep (nthcdr n comments-with-scores)))
      (setf (unsent admin-session) (mapcar #'first to-keep))
      (setf (last-hit admin-session) (get-universal-time))
      (ldiff comments-with-scores to-keep))))

(defun comments-with-scores (comments)
  (sort
   (mapcar
    #'(lambda (c)
        (multiple-value-bind (prediction score) (spamminess c)
          (list c score prediction)))
    comments)
   #'spam-admin-<))

(defun spam-admin-< (a b)
  (destructuring-bind (a-comment a-score a-prediction) a
    (declare (ignore a-prediction))
    (destructuring-bind (b-comment b-score b-prediction) b
      (declare (ignore b-prediction))
      (cond
        ((< a-score b-score) t)
        ((> a-score b-score) nil)
        (t (< (utc a-comment) (utc b-comment)))))))

(defun emit-spam-admin-comment-html (comment spamminess classification)
  (spam-admin-comment-html
   :id (id comment)
   :name (getf (data comment) :name)
   :text (getf (data comment) :text)
   :utc (utc comment)
   :spamminess spamminess
   :classification classification))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spam features

(defun extract-features (comment)
  (destructuring-bind (&key name text cookies &allow-other-keys) (data comment)
    (let ((features ()))
      (push `(:name ,name) features)
      (loop for word in (extract-words text) do (push `(:word ,word) features))
      (loop for url in (extract-urls text) do (push `(:url ,url) features))
      (loop for (key . value) in cookies do (push `(:cookie ,key ,value) features))
      features)))

(defparameter *word-regex* (create-scanner "\\w{3,}" :case-insensitive-mode t))

(defun extract-words (text)
  "Simple function to extract words from a text."
  (delete-duplicates
   (mapcar #'string-downcase (all-matches-as-strings *word-regex* text))
   :test #'string=))

(defparameter *url-regex* (create-scanner "http://\\w+" :case-insensitive-mode t))

;; From: http://daringfireball.net/2010/07/improved_regex_for_matching_urls
(defparameter *url-regex* (create-scanner "(?i)\\b((?:[a-z][\\w-]+:(?:/{1,3}|[a-z0-9%])|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:'\".,<>?«»“”‘’]))"))

(defun extract-urls (text)
  "Extract things that look like URLS from text."
  (delete-duplicates (all-matches-as-strings *url-regex* text) :test #'string=))
