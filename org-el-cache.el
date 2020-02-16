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

(defgroup org-el-cache nil
  "Persistent cache for data derived from org-elements."
  :group 'external
  :group 'text)

(defcustom org-el-cache-directory (expand-file-name "org-el-caches/" user-emacs-directory)
  "Directory to store cache files in."
  :type 'path
  :group 'org-el-cache)

(defcustom org-el-cache-persist t
  "Enable persisting caches to disk"
  :type 'boolean
  :group 'org-el-cache)

;;; Creating Caches

(defclass org-el-cache ()
  ((folders :initarg :folders)
   (file :initarg :file)
   (hook :initarg :hook)
   (table :initarg :table)
   (include-archives :initarg :include-archives)
   (name :initarg :name)))

(defvar org-el-caches (make-hash-table :test 'eq :size 10)
  "Table of all org-el-caches.")

(defun org-el-all-caches ()
  "Get a list of all active caches."
  (hash-table-values org-el-caches))

(defun org-el-cache-remove-cache (cache)
  "Remove CACHE from the list of active caches."
  (remhash (oref cache name) org-el-caches))

(cl-defun make-org-el-cache (name folders hook &key file include-archives)
  "Create a new org-el-cache and add it to the list of active caches.
NAME should be a symbol."
  (unless (symbolp name)
    (error "org-el-cache: Cache name must be a symbol, was %s" name))
  (when (null file)
    (setq file (expand-file-name (format "%s.el" name) org-el-cache-directory)))
  (make-directory (file-name-directory file) t)
  (let ((cache
         (make-instance
          'org-el-cache
          :name name
          :folders (mapcar #'expand-file-name folders)
          :file file
          :hook hook
          :include-archives include-archives
          :table (make-hash-table :test 'equal :size 1000))))
    (if org-el-cache-persist
        (org-el-cache-load cache))
    (puthash name cache org-el-caches)
    cache))

(cl-defmacro def-org-el-cache (name folders hook &key file include-archives)
  "Convenience macro around `make-org-el-cache' that creates a
cache and binds it to a variable named NAME."
  ;; Make sure to evaluate name only once
  (unless (symbolp name)
    (error "org-el-cache: Cache name must be a symbol, was %s" name))
  `(setq ,name
         (make-org-el-cache (quote ,name) ,folders ,hook
                            :file ,file
                            :include-archives ,include-archives)))

(defun org-el-cache-get (cache file)
  "Get the value for FILE from CACHE."
  (if-let ((entry (gethash file (oref cache table))))
      (plist-get entry :data)))

(defun org-el-cache-member-p (cache file)
  "Check if FILE is part of the files in CACHE."
  (with-slots (folders include-archives) cache
    (let ((extension (file-name-extension file))
          (file (expand-file-name file)))
      (and (or (string= extension "org")
               (and include-archives (string= extension "org_archive")))
           (some
            (lambda (folder) (string-prefix-p folder file))
            folders)))))

(defun org-el-cache-remove (cache file)
  (remhash
   (expand-file-name file)
   (oref cache table)))

(defun org-el-cache-clear (cache)
  "Clear CACHE."
  (interactive)
  (clrhash (oref cache table)))

(defun org-el-cache-count (cache)
  "Get the number of files in CACHE."
  (hash-table-count (oref cache table)))

(defun org-el-cache-files (cache)
  "List of all files managed by CACHE."
  (hash-table-keys (oref cache table)))

;;; Helper Functions

(defun org-el-cache-interpret-data (data)
  "Interpret DATA stripping positional / style information."
  (let ((string (org-element-interpret-data data)))
    (substring-no-properties string)))

;;; Processing Files / Buffers, Per Cache

;; This is the fastest way of parsing the contents of a file into an
;; org-element I've found so far.
(defun org-el-cache--process-file (cache file)
  "Process FILE for CACHE."
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks
      (org-mode)
      (org-el-cache--process-buffer cache file))))

(defun org-el-cache--process-buffer (cache filename)
  "Update the cache entry for FILE using the contents of the
current buffer."
  (message "Processing file %s" filename)
  (org-el-cache--process-root
   cache
   filename
   (secure-hash 'sha1 (current-buffer))
   (org-element-parse-buffer)))

(defun org-el-cache--process-root (cache filename hash el)
  "Process the org-element root element EL."
  (with-slots (hook table) cache
    (puthash
     filename
     (list :file filename
           :hash hash
           :data (funcall hook filename el))
     table)))

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
       (secure-hash 'sha1 (current-buffer))
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
              (setq hash (secure-hash 'sha1 (current-buffer))))
            (org-el-cache--process-root cache filename hash el))))))

(defun org-el-cache--rename-file (cache from to)
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

(defun org-el-cache--file-hashes (cache)
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

(defun org-el-cache-update (cache)
  "Update all outdated entries in CACHE."
  (loop for (file . hash) in (org-el-cache--file-hashes cache) do
        (let ((entry (gethash file (oref cache table))))
          (unless (and entry (string= (plist-get entry :hash) hash))
            (org-el-cache--process-file cache file)))))

(defun org-el-cache-force-update (cache)
  "Re-initialize CACHE."
  (org-el-cache-clear cache)
  (org-el-cache-update cache))

;;; Persisting / Loading Caches

(defun org-el-cache-persist (cache)
  "Store CACHE on disk."
  (if org-el-cache-persist
      (with-slots (table file) cache
        (with-temp-buffer
          (insert (with-output-to-string (prin1 table)))
          (write-file file)))
    (message "org-el-cache: Persisting caches is not enabled")))

(defun org-el-cache-load (cache)
  "Try loading CACHE from disk."
  (if org-el-cache-persist
      (with-slots (table file) cache
        (if (file-exists-p file)
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (let ((table_ (read (current-buffer))))
                (if (hash-table-p table_)
                    (setf table table_)
                  (error "org-el-cache: malformed cache file"))))))
    (message "org-el-cache: Persisting caches is not enabled")))

(defun org-el-cache-persist-all ()
  "Persist all caches to disk."
  (interactive)
  (if org-el-cache-persist
      (dolist (cache (org-el-all-caches))
        (org-el-cache-persist cache))))

(add-hook 'kill-emacs-hook 'org-el-cache-persist-all)

;;; Mapping / Reduction Functions

(defun org-el-cache-reduce (cache fn acc)
  "Reduce over each entry of CACHE using FN.
FN is called with three arguments:
1. the cache key (filename)
2. the cache entry
3. the accumulator, initially set to ACC "
  (maphash
   (lambda (key entry) (setq acc (funcall fn key (plist-get entry :data) acc)))
   (oref cache table))
  acc)

(defun org-el-cache-map (cache mapfn)
  "Map over each entry in CACHE, collecting the results of calling
FN with two arguments:
1. the cache key (filename)
2. the cache entry"
  (org-el-cache-reduce
   cache
   (lambda (key value acc) (cons (funcall mapfn key value) acc))
   '()))

(defun org-el-cache-select (cache filterfn)
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

(defun org-el-cache-each (cache fn)
  "Call FN for each entry in CACHE.
FN is called with two arguments:
1. the cache key (filename)
2. the cache entry"
  (maphash
   (lambda (filename entry)
     (funcall fn filename (plist-get entry :data)))
   (oref cache table)))

;;; Exports

(provide 'org-el-cache)
