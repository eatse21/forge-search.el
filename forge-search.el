;;; Magit Forge search add-on
(require 's)
(require 'magit)
(require 'forge)
(require 'emacsql)

(defcustom forge-search-date-type "created"
  "Choose which date is displayed on search results
- \"created\": date of creation of the issue/pr/message
- \"updated\": date of last change"
  :type '(choice (const :tag "creation" "created")
                 (other :tag "last update" "updated"))
  :group 'forge)

(defun forge-search (search-string)
  "Perform a SQLite full text search of SEARCH-STRING in current repository’s
issues and pull requests (with their replies)"
  (interactive "sSearch in repository: ")
  (when-let* ((db (forge-db))
              (repo (forge-get-repository 'full))
              (repoid (slot-value repo 'id)))
    (emacsql-with-transaction db
      (emacsql db [:create-virtual-table :if :not :exists search
		   :using :fts5
		   ([id haystack author date type title body])])

      ;; Is there a way to make those queries with Emacsql syntax?
      ;; Manual SQL query string building could be avoided
      ;; SQLite’s || (concatenation) operator doesn't seem to be supported
      ;; https://github.com/magit/emacsql/issues/33
      (emacsql db
               (concat "insert into search "
                       "select id, "
                       "('\"' || replace(title, '\"', ' ') || ' ' || replace(body, '\"', ' ') || '\"') as haystack, "
                       "author, " forge-search-date-type " as date, "
                       "'\"issue\"' as type, title, body "
                       "from issue where repository = '\"" repoid "\"';"))
      (emacsql db
               (concat "insert into search "
                       "select issue_post.issue, "
                       "('\"' || replace(issue_post.body, '\"', ' ') || '\"') as haystack, "
                       "issue_post.author, "
                       "issue_post." forge-search-date-type " as date, "
                       "'\"issue_msg\"' as type, '\"\"' as title, issue_post.body "
                       "from issue_post "
                       "inner join issue on issue.id = issue_post.issue "
                       "where issue.repository = '\"" repoid "\"';"))


      (emacsql db
               (concat "insert into search "
                       "select id, "
                       "('\"' || replace(title, '\"', ' ') || ' ' || replace(body, '\"', ' ') || '\"') as haystack, "
                       "author, " forge-search-date-type " as date, "
                       "'\"pr\"' as type, title, body "
                       "from pullreq where repository = '\"" repoid "\"';"))
      (emacsql db
               (concat "insert into search "
                       "select pullreq_post.pullreq, "
                       "('\"' || replace(pullreq_post.body, '\"', ' ') || '\"') as haystack, "
                       "pullreq_post.author, "
                       "pullreq_post." forge-search-date-type " as date, "
                       "'\"pr_msg\"' as type, '\"\"' as title, pullreq_post.body "
                       "from pullreq_post "
                       "inner join pullreq on pullreq.id = pullreq_post.pullreq "
                       "where repository = '\"" repoid "\"';"))

      (message "Searching '%s'" search-string)
      (let* ((matches
              ;; haystack contains both post's title and body
              (emacsql db [:select [id author date type title body]
                           :from search
                           :where haystack :match $r1]
                       search-string))
             (magit-generate-buffer-name-function (lambda (_mode _value) (format "*Forge Search Results*" search-string))))
        (if matches
            (magit-setup-buffer #'forge-search-mode t
              (matches matches)
              (search-string search-string))
          (message "No result")))
      (emacsql db [:drop-table search]))))

(defun forge-search-refresh-buffer ()
  ;; magit-setup-buffer sets matches and search-string automatically from above
  (magit-set-header-line-format (concat "Results for query \"" search-string "\""))
  (magit-insert-section (_note)
    (-each matches
      (lambda (el)
        (let* ((id (car el))
               (author (cadr el))
               (date (caddr el))
               (type (cadddr el))
               (title (cadddr (cdr el)))
               (body (cadddr (cddr el)))
               (heading (concat type ": by " author " " date))
               (body-rendered (concat
                               (if (or (equal "issue" type) (equal "pr" type))
                                   (forge--fontify-markdown (concat "# " title "\n\n"))
                                 "")
                               (forge--fontify-markdown body) "\n\n")))

          ;; Adding id text property so we can do visit/browse at point afterwards
          (add-text-properties 0 (length heading) (list 'id id) heading)
          (add-text-properties 0 (length body-rendered) (list 'id id) body-rendered)

          (magit-insert-section (_note)
            (magit-insert-heading heading)
            (insert body-rendered)))))))



(defvar-keymap forge-search-mode-map
  :doc "Local keymap for Forge Search buffers."
  :parent magit-mode-map
  "b"      #'forge-search-browse-at-point
  "RET"      #'forge-search-visit-at-point
  "<return>" #'forge-search-visit-at-point)

(define-derived-mode forge-search-mode magit-mode "forge-search"
  "Major mode for Forge Search. Simple placeholder for now")

(defun forge--search-do-at-point (fn-issue fn-pullreq)
  (when-let ((id (get-text-property (point) 'id)))
    ;; Forge issue/pr msgs ids contain an extra :[0-9]+ at the end
    ;; get rid of it so we can use forge-visit-X
    (let ((decoded-id (replace-regexp-in-string ":[0-9]+$" "" (base64-decode-string id))))
      (if (s-contains-p "issue" decoded-id)
          (funcall fn-issue (base64-encode-string decoded-id))
        (funcall fn-pullreq (base64-encode-string decoded-id)))))
  )

(defun forge-search-visit-at-point ()
"View the issue/pull request under point"
  (interactive)
  (forge--search-do-at-point 'forge-visit-issue 'forge-visit-pullreq)
)

(defun forge-search-browse-at-point ()
"View the issue/pull request under point with the browser"
  (interactive)
  (forge--search-do-at-point 'forge-browse-issue 'forge-browse-pullreq)
)
