;;; file-selector.el --- File selector example for org-el-cache -*- lexical-binding: t; -*-

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

;; Extract a files title by mapping over all keywords
;; in it, returning the value of the ~#+TITLE~ keyword
(defun file-selector--extract-title (filename el)
  (org-element-map el 'keyword
    (lambda (kw)
      (if (string= (org-element-property :key kw) "TITLE")
          (org-element-property :value kw)))
    :first-match t))

;; Define a new cache.  It is automatically updated and loaded if a
;; persisted file exists.
(def-org-el-cache
  file-selector-cache
  '("~/org")
  #'file-selector--extract-title)

;; Update / Initialize the cache
(org-el-cache-update file-selector-cache)

;; Define a file selector using ivy from https://github.com/abo-abo/swiper/
(defun file-selector ()
  (interactive)
  (ivy-read
   "File: "
   (org-el-cache-map
    file-selector-cache
    (lambda (filename entry)
      (cons entry filename)))
   :action
   (lambda (selection)
     (find-file (cdr selection)))))
