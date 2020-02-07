;;; org-el-cache.el --- Persistent cache for org-element data -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Package-requires: ((emacs "26.3"))
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'cl)

;;; Configuration

(defcustom org-el-cache-persist-interval (* 10 60)
  "Store the cache on disk every n seconds.")

;;; Creating Caches

(defclass org-el-cache ()
  ((folders :initarg :folders)
   (file :initarg :file)
   (hooks :initform '())
   (table :initarg :table)
   (include-archives :initarg :include-archives)))

(defvar org-el-caches '()
  "List of all org-el-caches.")

(defun org-el-all-caches ()
  (loop for (k v) on org-el-caches by #'cddr collecting v))

(defun make-org-el-cache (folders file &optional include-archives)
  "Create a new org-el-cache object."
  (let ((cache
         (make-instance
          'org-el-cache
          :folders folders
          :file file
          :include-archives include-archives
          :table (make-hash-table :test 'equal :size 1000))))
    ;; Try loading the cache from disk
    (org-el-cache-load cache)
    cache))

(defmethod org-el-cache-get ((cache org-el-cache) file)
  "Get the entry for FILE from CACHE."
  (gethash file (oref cache table)))

(defmethod org-el-cache-file-property ((cache org-el-cache) file prop &optional default)
  "Get the value of PROP for FILE in CACHE.
If the property has no value, return DEFAULT,
if FILE is not part of the cache, return nil."
  (if-let ((entry (org-el-cache-get cache file)))
      (or (plist-get entry prop) default)))

(defmethod org-el-cache-member-p ((cache org-el-cache) file)
  "Check if FILE is part of the files in CACHE."
  (with-slots (folders include-archives) cache
    (let ((extension (file-name-extension file))
          (file (expand-file-name file)))
      (and (or (string= extension "org")
               (and include-archives (string= extension "org_archive")))
           (some
            (lambda (folder) (string-prefix-p (expand-file-name folder) file))
            folders)))))

(defmethod org-el-cache-remove ((cache org-el-cache) file)
  (remhash
   (expand-file-name file)
   (oref cache table)))

(defmethod org-el-cache-clear ((cache org-el-cache))
  "Clear CACHE."
  (interactive)
  (clrhash (oref cache table)))

(defmethod org-el-cache-count ((cache org-el-cache))
  "Get the number of files in CACHE."
  (hash-table-count (oref cache table)))

(defmethod org-el-cache-files ((cache org-el-cache))
  "List of all files managed by CACHE."
  (hash-table-keys (oref cache table)))

(defmacro def-org-el-cache (name folders file &optional include-archives)
  "Create a new org-el-cache and add it to the list of active caches.
NAME should be a symbol."
  ;; (unless (symbolp name)
  ;;   (error "org-el-cache: cache name should be a symbol, was %s" name))
  `(progn
     (let ((cache (make-org-el-cache ,folders ,file ,include-archives))
           (var (quote ,name)))
       (setq org-el-caches (plist-put org-el-caches var cache))
       (setq ,name cache))))

(defmethod org-el-cache-add-hook ((cache org-el-cache) prop hook)
  (with-slots (hooks) cache
    (setf hooks (plist-put hooks prop hook))))

;;; Helper Functions

(defun org-el-cache-interpret-data (data)
  "Interpret DATA stripping positional / style information."
  (let ((string (org-element-interpret-data data)))
    (substring-no-properties string)))

;;; Processing Files / Buffers, Per Cache

;; This is the fastest way of parsing the contents of a file into an
;; org-element I've found so far.
(defmethod org-el-cache--process-file ((cache org-el-cache) file)
  "Process FILE for CACHE."
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks
      (org-mode)
      (org-el-cache--process-buffer cache file))))

(defmethod org-el-cache--process-buffer ((cache org-el-cache) filename)
  "Update the cache entry for FILE using the contents of the
current buffer."
  (message "Processing file %s" filename)
  (org-el-cache--process-root
   cache
   filename
   (buffer-hash)
   (org-element-parse-buffer)))

(defmethod org-el-cache--process-root ((cache org-el-cache) filename hash el)
  "Process the org-element root element EL."
  (with-slots (hooks table) cache
    (let ((entry `(:file ,filename :hash ,hash)))
      (loop for (prop hook) on hooks by 'cddr do
            (setq entry
                  (plist-put
                   entry
                   prop
                   (funcall hook filename el))))
      (puthash filename entry table))))

;;; Processing Files / Buffers, Global

;; TODO: Check if there is a buffer visiting this file,
;; use it to parse the file instead.
(defun org-el-cache--parse-file (file)
  "Parse the contents of FILE into an org-element."
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks
      (org-mode)
      (cons
       (buffer-hash)
       (org-element-parse-buffer)))))

(defun org-el-cache-process-file (file)
  (let (el hash)
    ;; Hacky way to only parse the buffer if it its file is member
    ;; of at least one cache
    (dolist (cache (org-el-all-caches))
      (when (org-el-cache-member-p cache file)
        (unless el
          (let ((res (org-el-cache--parse-file file)))
            (setq el (cdr res))
            (setq hash (car res))))
        (org-el-cache--process-root cache file hash el)))))

(defun org-el-cache-process-buffer ()
  ;; TODO
  (if-let ((filename (buffer-file-name)))
      ;; Hacky way to only parse the buffer if it its file is member
      ;; of at least one cache
      (let (el hash)
        (dolist (cache (org-el-all-caches))
          (when (org-el-cache-member-p cache filename)
            (unless el
              (setq el (org-element-parse-buffer))
              (setq hash (buffer-hash)))
            (org-el-cache--process-root cache filename hash el))))))

(defmethod org-el-cache--rename-file ((cache org-el-cache) from to)
  (if (org-el-cache-member-p cache from)
   (with-slots (table) cache
     (if (org-el-cache-member-p cache to)
         (puthash to (gethash from table) table))
     (remhash from table))))

(defun org-el-cache-rename-file (from to)
  (message "org-el-cache: Renaming entries for %s to %s" from to)
  (dolist (cache (org-el-all-caches))
    (org-el-cache--rename-file cache from to)))

(defun org-el-cache-delete-file (file)
  (dolist (cache (org-el-all-caches))
    (org-el-cache-remove cache file)))

;;; Advice / Hooks

;; TODO: rewrite using defadvice
(defun org-el-cache--delete-file-advice (file &optional _trash)
  (let ((truename (file-truename file)))
    (org-el-cache-delete-file truename)))

(advice-add 'delete-file :after #'org-el-cache--delete-file-advice)

;; TODO: rewrite using defadvice
(defun org-el-cache--rename-file-advice (file-old file-new &optional _ok-if-already-exists)
  (org-el-cache-delete-file file-old)
  (org-el-cache-process-file (file-truename file-new)))

(advice-add 'rename-file :after #'org-el-cache--rename-file-advice)

;; In org mode, update cache entries when saving files
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda () (org-el-cache-process-buffer))
                      nil t)))

;;; Initializing / Updating Caches

(defun org-el-cache--find (paths &optional include-archives)
  "Generate shell code to search PATHS for org files."
  (if include-archives
      (format
       "find %s -name \"[a-Z0-9_]*.org\" -o -name \"[a-Z0-9_]*.org_archive\" "
       (mapconcat 'identity paths " "))
    (format
     "find %s -name \"[a-Z0-9_]*.org\" "
     (mapconcat 'identity paths " "))))

(defmethod org-el-cache--file-hashes ((cache org-el-cache))
  "List all files managed by CACHE together with their hashes."
  (with-slots (folders include-archives) cache
    (mapcar
     (lambda (line)
       (let ((parts (split-string line "  ")))
         (cons (cadr parts) (car parts))))
     (split-string
      (shell-command-to-string
       (concat
        (org-el-cache--find folders include-archives)
        " | xargs sha1sum"))
      "\n"
      t))))

(defmethod org-el-cache-update ((cache org-el-cache))
  "Update all outdated entries in CACHE."
  (loop for (file . hash) in (org-el-cache--file-hashes cache) do
        (let ((entry (org-el-cache-get cache file)))
          (unless (and entry (string= (plist-get entry :hash) hash))
            (org-el-cache--process-file cache file)))))

(defmethod org-el-cache-force-update ((cache org-el-cache))
  "Re-initialize CACHE."
  (org-el-cache-clear cache)
  (org-el-cache-update cache))

;;; Persisting / Loading Caches

  (defmethod org-el-cache-persist ((cache org-el-cache))
    "Store CACHE on disk."
    (with-slots (table file) cache
      (with-temp-buffer
        (insert (with-output-to-string (prin1 table)))
        (write-file file))))

(defmethod org-el-cache-load ((cache org-el-cache))
  "Try loading CACHE from disk."
  (with-slots (table file) cache
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((table_ (read (current-buffer))))
            (if (hash-table-p table_)
                (setf table table_)
              (error "org-el-cache: malformed cache file")))))))

(defun org-el-cache-persist-all ()
  "Persist all caches to disk."
  (interactive)
  (dolist (cache (org-el-all-caches))
    (org-el-cache-persist cache)))

(run-at-time
 nil
 org-el-cache-persist-interval
 #'org-el-cache-persist-all)

;;; Mapping / Reduction Functions

(defmethod org-el-cache-reduce ((cache org-el-cache) fn acc)
  "Reduce over each entry of CACHE using FN.
FN is called with three arguments:
1. the cache key (filename)
2. the cache entry
3. the accumulator, initially set to ACC "
  (maphash
   (lambda (key value) (setq acc (funcall fn key value acc)))
   (oref cache table))
  acc)

(defmethod org-el-cache-map ((cache org-el-cache) mapfn)
  "Map over each entry in CACHE, collecting the results of calling
FN with two arguments:
1. the cache key (filename)
2. the cache entry"
  (org-el-cache-reduce
   cache
   (lambda (key value acc) (cons (funcall mapfn key value) acc))
   '()))

(defmethod org-el-cache-select ((cache org-el-cache) filterfn)
  "Map over each entry in CACHE, collecting all files for which FN evaluates to a non-nil value.
FN is with two arguments:
1. the cache key (filename)
2. the cache entry"
  (org-el-cache-reduce
   cache
   (lambda (key value acc)
     (if (funcall filterfn key value)
         (cons value acc)
       acc))
   '()))

(defmethod org-el-cache-each ((cache org-el-cache) fn)
  "Call FN for each entry in CACHE.
FN is called with two arguments:
1. the cache key (filename)
2. the cache entry"
  (maphash fn (oref cache table)))

;;; Exports

(provide 'org-el-cache)
