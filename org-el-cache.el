;;; Directory Setup

(defcustom org-el-cache-directories '("~/org/")
  "List of directories to scan for org files")

(defcustom org-el-cache-include-archives nil
  "Include org archives in cache.")

(defun org-el-cache-member-p (file)
  "Check if FILE is managed by the cache."
  (if (or (string= (file-name-extension file) "org")
          (and
           org-el-cache-include-archives
           (string= (file-name-extension file) "org_archive")))
      (some (lambda (folder) (string-prefix-p (expand-file-name folder) file))
            org-el-cache-directories)))

;;; Helper Functions

(defun org-el-cache--merge-plist (plist1 plist2)
  "Merge two plists."
  (loop for (key value) on plist2 by 'cddr do
        (setq plist1 (plist-put plist1 key value)))
  plist1)

;;; Cache

(defun make-org-el-cache ()
  "Create a now object to be used as cache table"
  (make-hash-table :test 'equal :size 1000))

(defvar org-el-cache--table (make-org-el-cache)
  "Hash table used to store cached files")

(defun org-el-cache-get (file)
  "Get the entry for FILE."
  (gethash file org-el-cache--table))

(defun org-el-cache-file-property (file property)
  "Read PROPERTY from the entry for FILE"
  (org-el-cache-entry-property (org-el-cache-get file) property))

(defun org-el-cache-entry-property (entry property)
  "Read PROPERTY from ENTRY"
  (plist-get entry property))

(defun org-el-cache-remove (file)
  "Delete the entry for the file at PATH from the cache."
  (remhash file org-el-cache--table))

(defun org-el-cache-clear ()
  "Clear the org-element cache"
  (interactive)
  (clrhash org-el-cache--table))
;;; Shell File Indexing / Hashing

(defun org-el-cache--find (paths)
  "Generate shell code to search PATHS for org files."
  (if org-el-cache-include-archives
    (format
       "find %s -name \"[a-Z0-9_]*.org\" -o -name \"[a-Z0-9_]*.org_archive\" "
       (mapconcat 'identity paths " "))
    (format
       "find %s -name \"[a-Z0-9_]*.org\" "
       (mapconcat 'identity paths " "))))

(defun org-el-cache-file-hashes ()
  "Generate a list of (path . hash) pairs for each file in
`org-el-cache-directories'."
  (mapcar
   (lambda (line)
  (let ((parts (split-string line "  ")))
       (cons (cadr parts) (car parts))))
   (split-string
    (shell-command-to-string
     (concat
      (org-el-cache--find org-el-cache-directories)
      " | xargs sha1sum"))
    "\n"
    t)))
;;; Persistence

(defcustom org-el-cache-persist-file "~/org/.org-el-cache"
  "File to use for storing / persisting the cache on disk.")

(defcustom org-el-cache-persist-interval (* 10 60)
  "Store the cache on disk every n seconds.")

;; TODO: Don't go through string
(defun org-el-cache-persist ()
  "Store the cache on disk."
  (interactive)
  (with-temp-buffer
    (insert (with-output-to-string (prin1 org-el-cache--table)))
    (write-file org-el-cache-persist-file)))

(defun org-el-cache-load ()
  "Try loading the cache from disk."
  (interactive)
  (if (file-exists-p org-el-cache-persist-file)
      (with-temp-buffer
        (insert-file-contents org-el-cache-persist-file)
        (goto-char (point-min))
        (let ((table (read (current-buffer))))
          (if (hash-table-p table)
              table
            (error "Malformed org-el-cache file"))))))

;;; Initialization / Updating

(defun org-el-cache-init ()
  "Initialize the cache, loading it from disk if there is a cache
    file."
  (interactive)
  (let ((persisted (org-el-cache-load)))
    (if persisted
        (setq org-el-cache--table persisted)
      (setq org-el-cache--table
            (make-hash-table :test 'equal :size 1000))))
  (org-el-cache-update))

;; TODO: Remove files in the cache but not in the file list
;; needs to store find results in a hash-table for quick membership tests
(defun org-el-cache-update ()
  "Checks all files in `org-el-cache-directories' for changes
and updates their entries. If there is no entry for a file,
a new one is created."
  (interactive)
  (loop for (file . hash) in (org-el-cache-file-hashes) do
        (let ((entry (org-el-cache-get file)))
          (unless (and entry (string= (plist-get entry :hash) hash))
            (org-el-cache--process-file file)))))

;;;; Advice / Hooks

(defun org-el-cache--remove-file-advice (file &optional _trash)
  (let ((truename (file-truename file)))
    (message "Removing cached file %s" truename)
    (org-el-cache-remove truename)))

(advice-add 'delete-file :before #'org-el-cache--remove-file-advice)

;; TODO Can this be rewritten with defadvice?
(defun org-el-cache--rename-file-advice (file-old file-new &optional _ok-if-already-exists)
  (org-el-cache-remove file-old)
  (org-el-cache-process-file (file-truename file-new)))

(advice-add 'rename-file :before #'org-el-cache--rename-file-advice)

;; Index current buffer on focus change
;; (defadvice switch-to-buffer (before save-buffer-now activate)
;;   (when (buffer-file-name) (org-el-cache-process-buffer)))

;; (defadvice other-window (before other-window-now activate)
;;   (when (buffer-file-name) (org-el-cache-process-buffer)))

;; (defadvice other-frame (before other-frame-now activate)
;;   (when (buffer-file-name) (org-el-cache-process-buffer)))

;; In org mode files, update cache entries on save
(add-hook 'org-mode-hook
  (lambda ()
    (add-hook 'before-save-hook
      (lambda () (org-el-cache-process-buffer))
      nil t)))

;;;; File Processing

;; This is the fastest way of parsing the contents of a file into an
;; org-element I've found so far.
(defun org-el-cache--process-file (file)
  "Process a FILE."
  (with-temp-buffer
  (insert-file-contents file)
  (delay-mode-hooks
  (org-mode)
  (org-el-cache--process-buffer file))))

(defun org-el-cache-process-file (file)
  "Process a FILE, checking if it is managed by the cache."
  (if (org-el-cache-member-p file)
    (org-el-cache--process-file file)))

(defun org-el-cache-process-buffer ()
  "Process the current buffer"
  (interactive)
  (if (org-el-cache-member-p (buffer-file-name))
    (org-el-cache--process-buffer (buffer-file-name))))

(defun org-el-cache--process-buffer (file)
  "Process the contents of the current buffer and use them to create a cache entry for FILE.
 Assumes FILE is managed by the cache.  FILE is an argument
because of the way files are processed in the background."
  (interactive)
  (message "Processing file %s" file)
  (org-el-cache--process-element file (buffer-hash) (org-element-parse-buffer)))

(defun org-el-cache--process-element (file hash element)
  (let ((cache-file (list :file file :hash hash :headlines nil)))
    (loop for (_ hook) on org-el-cache-file-hooks by 'cddr do
          (setq cache-file
                (org-el-cache--merge-plist
                 cache-file
                 (funcall hook file element))))
    (puthash file cache-file org-el-cache--table)))

;;; Helper Functions
(defun org-el-cache-interpret-data (data)
  "Interpret DATA stripping positional / style information."
  (let ((string (org-element-interpret-data data)))
    (substring-no-properties string)))
;;; Hooks
;;;; File Hooks

(defvar org-el-cache-file-hooks '())

(defmacro def-org-el-cache-file-hook (name args &rest body)
  "Helper macro for registering a cache file hook.
ARGS should be of the form (var1 var2), var1 being used to pass
in the files file, var2 used to pass in the parsed element."
  (unless (and (listp args) (= (length args) 2))
    (error "org-el-cache file hooks must take exactly one argument"))
  (unless (symbolp name)
    (error "org-el-cache file hooks must have a symbol as their name"))
  ;; TODO, check if name is a symbol
  `(setq
    org-el-cache-file-hooks
    (plist-put
     org-el-cache-file-hooks
     ',name
     (lambda ,args ,@body))))

(def-org-el-cache-file-hook headlines (file el)
  (let (headlines)
    (org-element-map el 'headline
      (lambda (child)
        (let ((hl `(:file ,file)))
          (loop for (nil hook) on org-el-cache-headline-hooks by 'cddr do
                (setq hl (org-el-cache--merge-plist
                          hl
                          (funcall hook child))))
          (push hl headlines))))
    `(:headlines ,headlines)))

(def-org-el-cache-file-hook links (_file el)
  (let (links)
    (org-element-map el 'link
      (lambda (child)
        (push (list :type (org-element-property :type child)
                    :path (org-element-property :path child)
                    :text (org-el-cache-interpret-data (org-element-contents child)))
              links)))
    `(:links ,links)))

(def-org-el-cache-file-hook keywords (_file el)
  (let ((first (car (org-element-contents el))))
    (if (eq (org-element-type first) 'section)
        `(:keywords
          ,(org-element-map first 'keyword
             (lambda (e) (cons (org-element-property :key e)
                          (org-element-property :value e))))))))

(defun org-el-cache-file-keyword (file keyword &optional default)
  (org-el-cache-entry-keyword
   (org-el-cache-get file)
   keyword
   default))

(defun org-el-cache-entry-keyword (entry keyword &optional default)
  (or (if-let ((keywords (org-el-cache-entry-property entry :keywords)))
          (alist-get keyword keywords nil nil 'string=))
      default))

;;;; Headline Hooks

(defvar org-el-cache-headline-hooks '())

(defmacro def-org-el-cache-headline-hook (name args &rest body)
  "Helper macro for registering a cache headline hook.
ARGS should be of the form (var1), var1 being used to pass in the
parsed headline element."
  (unless (and (listp args) (= (length args) 1))
    (error "org-el-cache headline hooks must take exactly one argument"))
  (unless (symbolp name)
    (error "org-el-cache headline hooks must have a symbol as their name"))
  ;; TODO, check if name is a symbol
  `(setq
    org-el-cache-headline-hooks
    (plist-put
     org-el-cache-headline-hooks
     ',name
     (lambda ,args ,@body))))


(def-org-el-cache-headline-hook headline-properties (el)
  `(:begin ,(org-element-property :begin el)
           :deadline ,(org-element-property :deadline el)
           :scheduled ,(org-element-property :scheduled el)
           :level ,(org-element-property :level el)
           :priority ,(org-element-property :priority el)
           :tags ,(org-element-property :tags el)
           :todo-keyword ,(org-element-property :todo-keyword el)
           :todo-type ,(org-element-property :todo-type el)
           :title ,(org-element-property :raw-value el)
           :effort ,(org-element-property :EFFORT el)
           :style ,(org-element-property :STYLE el)))

;;;; Filtering / Mapping

(defun org-el-cache-filter-files (pred)
  "Collect a list of (file cached-file) matching the predicate PRED.
FN is called with two arguments: the file file and the cached file object"
  (let (entries)
    (maphash
     (lambda (_key value)
       (if (funcall pred value)
           (push value entries)))
     org-el-cache--table)
    entries))

(defun org-el-cache-mapcan-files (fn)
  "Collect a list of results by calling FN on each entry,
concatenating the lists it returns. FN is called with two
arguments: the file file and the cached file object"
  (let (results)
    (maphash
     (lambda (_key value)
       (setq results (nconc (funcall fn value) results)))
     org-el-cache--table)
    results))

(defun org-el-cache-filter-headlines (pred)
  "Collect a list of cached-headlines matching the predicate PRED.
FN is called with two arguments: the parent cached file object
and the cached headline object."
  (let (entries)
    (maphash
     (lambda (key value)
       (dolist (headline (plist-get value :headlines))
         (if (funcall pred value headline)
             (push headline entries))))
     org-el-cache--table)
    entries))

(defun org-el-cache-mapcan-headlines (fn)
  "Collect a list of (file cached-file cached-headline) matching the predicate PRED.
FN is called with two arguments: the parent cached file object and the cached headline object."
  (let (entries)
    (maphash
     (lambda (_key value)
       (dolist (headline (plist-get value :headlines))
         (setq entries (nconc entries (funcall fn value headline)))))
     org-el-cache--table)
    entries))

(defun org-el-cache-reduce-headlines (fn acc)
  "Reduce over all headlines in the cache.
FN is called with three arguments, the parent cached file, the
current headline and the current value of acc."
  (maphash
   (lambda (_key value)
     (dolist (headline (plist-get value :headlines))
       (setq acc (funcall fn value headline acc))))
   org-el-cache--table)
  acc)

;;; Exports

(provide 'org-el-cache)
