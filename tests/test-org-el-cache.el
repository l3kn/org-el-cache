;;; test-org-el-cache.el --- Tests for org-el-cache -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>

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

(require 'ert)
(require 'org-el-cache)

(defun org-el-cache-test-make-dir ()
  "Create a unique temporary directory for tests."
  (let* ((parent (temporary-file-directory))
         (tempdir (expand-file-name (concat (symbol-name (gensym)) "/") parent)))
    (make-directory tempdir)
    tempdir))

(defun org-el-cache-test-make-file (dir filename title)
  (let ((filename (expand-file-name filename dir)))
    (with-current-buffer (find-file-noselect filename)
      (insert "#+TITLE: " title "\n")
      (save-buffer))
    filename))

(defun org-el-cache-test--extract-title (filename el)
  (org-element-map el 'keyword
    (lambda (kw)
      (if (string= (org-element-property :key kw) "TITLE")
          (org-element-property :value kw)))
    :first-match t))

(ert-deftest integration-test ()
  (let* ((dir (org-el-cache-test-make-dir))
         (f1 (org-el-cache-test-make-file dir "foo.org" "Foo"))
         (f2 (org-el-cache-test-make-file dir "bar.org" "Bar"))
         (new-name (expand-file-name "baz.org" dir)))
    (def-org-el-cache
      org-el-test-cache
      (list dir)
      #'org-el-cache-test--extract-title
      :file (expand-file-name "cache.el" dir))

    (org-el-cache-update org-el-test-cache)
    (should (= (org-el-cache-count org-el-test-cache) 2))

    ;; Hook Data
    (should (equal (org-el-cache-get org-el-test-cache f1) "Foo"))
    (should (equal (org-el-cache-get org-el-test-cache f2) "Bar"))

    (when org-el-cache-persist
      ;; Persistence
      (org-el-cache-persist org-el-test-cache)
      (org-el-cache-clear org-el-test-cache)
      (should (= (org-el-cache-count org-el-test-cache) 0))
      (org-el-cache-load org-el-test-cache)
      (should (= (org-el-cache-count org-el-test-cache) 2))
      (should (equal (org-el-cache-get org-el-test-cache f1) "Foo"))
      (should (equal (org-el-cache-get org-el-test-cache f2) "Bar")))

    ;; File Updating
    (with-current-buffer (find-file-noselect f1)
      (erase-buffer)
      (insert "#+TITLE: Baz\n")
      (save-buffer))
    (should (equal (org-el-cache-get org-el-test-cache f1) "Baz"))

    ;; Deleting Files
    (delete-file f1)
    (should (= (org-el-cache-count org-el-test-cache) 1))
    (should (equal (org-el-cache-files org-el-test-cache) (list f2)))

    ;; Renaming Files
    (rename-file f2 new-name)
    (should (= (org-el-cache-count org-el-test-cache) 1))
    (should (equal (org-el-cache-files org-el-test-cache) (list new-name)))

    (org-el-cache-remove-cache org-el-test-cache)))
