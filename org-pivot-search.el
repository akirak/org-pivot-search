;;; org-pivot-search.el --- Experimental search command for Org files -*- lexical-binding: t -*-

;; Copyright (C) 2023 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org-ql "0.6"))
;; Keywords: outlines convenience
;; URL: https://github.com/akirak/org-pivot-search

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a search command for relatively large Org files.

;; The primary entry point is `org-pivot-search-from-files' which takes a list
;; of Org files as arguments and perform search on them.

;; The implementation is partly based on org-ql-completing-read.el by Adam
;; Porter (alphapapa) which is available at
;; <https://github.com/alphapapa/org-ql>.

;;; Code:

(require 'pcase)
(require 'map)
(require 'org-ql)
(require 'org-nlink nil t)
(require 'org-dog nil t)

(defgroup org-pivot-search nil
  "Experimental search command for Org files."
  :group 'org
  :group 'org-ql
  :group 'org-dog)

(defconst org-pivot-search-nlink-group "Internal targets")

(defconst org-pivot-search-headline-group "Headline")

(defcustom org-pivot-search-display-action
  '(display-buffer-same-window
    (inhibit-same-window . nil))
  "Display action used for an entry in interactive mode.

When `org-pivot-search-from-files' is run interactively, this will
be used as the display action of `display-buffer', which see."
  :type 'sexp)

(defcustom org-pivot-search-fallback-function
  #'org-pivot-search-fallback-1
  "Function to run when the user input does not match a candidate.

The function takes two arguments: the user input, and a list of Org files."
  :type 'function)

(defcustom org-pivot-search-entry-display-hook nil
  "Hook run after an Org entry is displayed in `org-pivot-search-from-files'."
  :type 'hook)

(defcustom org-pivot-search-width-function #'frame-width
  "Function to determine the maximum width of completion candidates.

The function is called without arguments, and the result is
passed to `org-format-outline-path'.

The default value is `frame-width', but you can use
`window-width', which is more suitable if you use
`vertico-buffer-mode' <https://github.com/oantolin/vertico> in
completion UI."
  :type 'function
  :options '((const :tag "The width of the frame" frame-width)
             (const :tag "The width of the normal window" window-width)))

;;;###autoload
(cl-defun org-pivot-search-from-files (files &key display-action (indirect t))
  "Perform search of items from a given set of Org files."
  (interactive (list (list (buffer-file-name (org-base-buffer (current-buffer))))
                     :display-action org-pivot-search-display-action
                     :indirect nil)
               org-mode)
  (let* ((files (ensure-list files))
         (multi-p (> (length files) 1))
         (style (make-symbol "org-pivot-search--completion-style"))
         (width (funcall org-pivot-search-width-function))
         (nlink-items (org-pivot-search--nlink-candidates files))
         (table (make-hash-table :test #'equal :size 200))
         ;; The completion table is usually called more than once, e.g. for
         ;; `all-completions' and `try-completion', so it will be more efficient
         ;; to memorize dynamic candidates.
         ql-candidates)
    (cl-labels
        ((ql-candidates-for-file (predicate file)
           (org-ql-select file
             predicate
             :action (apply-partially #'org-pivot-search--entry-candidate
                                      width
                                      (when multi-p
                                        (org-pivot-search--candidate-prefix-1 file)))))

         (process-candidates (items)
           (dolist (x items)
             (puthash x x table))
           items)

         ;; Completion style

         (try (input _table _pred point &optional _metadata)
           (cons input point))

         (all (input table pred _point)
           (all-completions input table pred))

         (ql-candidates (input)
           (mapcan (apply-partially #'ql-candidates-for-file
                                    (org-ql--query-string-to-sexp input))
                   files))

         (completions (input pred action)
           (pcase action
             (`metadata
              (cons 'metadata
                    (list (cons 'category 'multi-category)
                          (cons 'group-function #'org-pivot-search--group)
                          (cons 'annotation-function #'org-pivot-search--annotate))))
             (`t
              (process-candidates (append (all-completions input nlink-items pred)
                                          (setq ql-candidates (ql-candidates input)))))
             (`nil
              (try-completion input nlink-items pred))
             (`lambda
               (when (member input nlink-items)
                 t))
             (`(boundaries . ,_suffix)
              nil))))
      (let* ((completion-ignore-case t)
             (completion-styles `(,style))
             (completion-styles-alist (cons (list style #'try #'all)
                                            completion-styles-alist))
             (input (completing-read (format "Org search (%s): "
                                             (mapconcat #'file-name-nondirectory
                                                        files ", "))
                                     #'completions)))
        (if-let (choice (gethash input table))
            (org-pivot-search--run-choice choice
                                          :display-action display-action
                                          :indirect indirect)
          (funcall org-pivot-search-fallback-function input files))))))

;;;; Running the choice

(cl-defun org-pivot-search--run-choice (choice &key display-action indirect)
  ;; Separate functions mostly for ease of debugging.
  (pcase (org-pivot-search--category choice)
    (`org-nlink-target
     (pcase (gethash choice org-nlink-target-cache)
       ((map :radio :marker)
        (org-dog-link-target-occur choice
                                   (buffer-file-name (marker-buffer marker))
                                   :radio radio))))
    (otherwise
     (let ((marker (get-text-property 0 'org-marker choice)))
       (pop-to-buffer (if indirect
                          (org-with-point-at marker
                            (org-tree-to-indirect-buffer))
                        (with-current-buffer (marker-buffer marker)
                          (goto-char marker)
                          (current-buffer)))
                      display-action)
       (run-hooks 'org-pivot-search-entry-display-hook)))))

(defun org-pivot-search-fallback-1 (input files)
  (if (string-empty-p input)
      (let ((org-agenda-files files))
        (org-agenda nil "a"))
    (org-ql-search files input)))

;;;; Completion groups and annotations

(defun org-pivot-search--category (candidate)
  (car-safe (get-text-property 0 'multi-category candidate)))

(defun org-pivot-search--group (candidate transform)
  (if transform
      candidate
    (cl-case (org-pivot-search--category candidate)
      (org-nlink-target org-pivot-search-nlink-group)
      (otherwise org-pivot-search-headline-group))))

(defun org-pivot-search--annotate (candidate)
  (while-no-input
    (cl-case (org-pivot-search--category candidate)
      (org-nlink-target (org-nlink-annotate-target candidate))
      (otherwise nil))))

;;;; Building candidates

(defun org-pivot-search--entry-candidate (&optional width prefix)
  (let* ((element (org-element-at-point))
         (headline (org-link-display-format
                    (org-element-property :raw-value element)))
         (candidate (org-format-outline-path
                     (org-pivot-search--olp-from-element element)
                     width prefix))
         (beg-marker (copy-marker (org-element-property :begin element)))
         (end-marker (copy-marker (org-element-property :end element)))
         (item (cons 'org-headline (list headline beg-marker end-marker))))
    (add-text-properties 0 (length candidate)
                         (list 'element element
                               'multi-category item
                               'org-marker beg-marker)
                         candidate)
    candidate))

(defun org-pivot-search--olp-from-element (element)
  "Return a reversed outline path of an ELEMENT."
  (let (olp)
    (while (and element
                ;; The top-level element is `org-data'.
                (eq 'headline (org-element-type element)))
      (push (thread-last
              (org-element-property :raw-value element)
              (org-no-properties)
              (org-link-display-format))
            olp)
      (setq element (org-element-property :parent element)))
    olp))

(defun org-pivot-search--candidate-prefix-1 (file)
  (let ((filename (abbreviate-file-name file)))
    (if-let (obj (and (featurep 'org-dog)
                      (org-dog-file-object filename :allow-missing t)))
        ;; oref doesn't work if this library is byte-compiled without org-dog
        (slot-value obj 'relative)
      filename)))

(defun org-pivot-search--nlink-candidates (files)
  (when (featurep 'org-nlink)
    (thread-last
      (plist-get (org-nlink-build-cache :files files :skip-headings t)
                 :targets)
      (mapcar (lambda (item)
                (put-text-property 0 (length item)
                                   'multi-category `(org-nlink-target . ,item)
                                   item)
                item)))))

(provide 'org-pivot-search)
;;; org-pivot-search.el ends here
