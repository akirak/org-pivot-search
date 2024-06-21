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

(defcustom org-pivot-search-default-arguments
  #'org-pivot-search-default-arguments-1
  "Function used to determine the arguments of `org-pivot-search-from-files'.

The function takes one argument, the current prefix argument. It
should return a list, which is used as the argument of
`org-pivot-search-from-files' function, which see."
  :type 'function)

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

(defcustom org-pivot-search-query-prefix nil
  "Default query prefix of org-ql search.

This is prepended to the user input in
`org-pivot-search-from-files' to build a query."
  :type '(choice (string :tag "Non-sexp org-ql query")
                 (const nil)))

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

(defcustom org-pivot-search-annotation-function nil
  "Function used to generate annotation of each completion candidate.

The function takes the marker of the headline as an argument."
  :type '(choice function (const nil)))

(defvar org-pivot-search-gc-threshold (* 64 1024 1024)
  "Large GC threshold for temporary increase.")

(defvar org-pivot-search-gc-percentage 0.5
  "Large GC percentage for temporary increase.")

;;;; Macro

(defmacro org-pivot-search--with-increased-gc (&rest body)
  (cl-with-gensyms (overwrite)
    `(let* ((,overwrite (> org-pivot-search-gc-threshold gc-cons-threshold))
            (gc-cons-threshold (if ,overwrite
                                   org-pivot-search-gc-threshold
                                 gc-cons-threshold))
            (gc-cons-percentage (if ,overwrite
                                    org-pivot-search-gc-percentage
                                  gc-cons-percentage)))
       ,@body)))

;;;; Interactive commands

;;;###autoload
(cl-defun org-pivot-search-from-files (files &key display-action (indirect t)
                                             noninteractive
                                             prompt
                                             query-prefix
                                             query-filter)
  "Perform search of items from a given set of Org files.

If NONINTERACTIVE is non-nil, it returns a cons cell of (CATEGORY
. STRING) instead of opening the target location in a window.

PROMPT is the prompt of the `completing-read' session.

QUERY-PREFIX should be, like in `org-ql-completing-read', a string
prepended to the plain query typed by the user.

QUERY-FILTER should be, like in `org-ql-completing-read', a function
that transforms the plain query just before it is parsed."
  (declare (indent 1))
  (interactive (funcall org-pivot-search-default-arguments)
               org-mode)
  (org-pivot-search--with-increased-gc
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
         ((process-candidates (items)
            (dolist (x items)
              (puthash x x table))
            items)

          ;; Completion style

          (try (input _table _pred point &optional _metadata)
            (cons input point))

          (all (input table pred _point)
            (all-completions input table pred))

          (ql-candidates (input)
            (let (result
                  (query (org-ql--query-string-to-sexp
                          (funcall (or query-filter
                                       #'identity)
                                   (concat (or org-pivot-search-query-prefix "")
                                           (or query-prefix "")
                                           input)))))
              ;; `mapcan' seems to create a circular list, which makes completion
              ;; freeze. I will use `append' as an alternative here. I am not
              ;; sure if this is a proper way to avoid the issue.
              (dolist (file files)
                (setq org-outline-path-cache nil)
                (setq result (append result (org-ql-select file query
                                              :action
                                              (apply-partially
                                               #'org-pivot-search--entry-candidate
                                               width
                                               (when multi-p
                                                 (org-pivot-search--candidate-prefix-1 file)))))))
              result))

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
              (input (completing-read (or prompt
                                          (format "Org search (%s): "
                                                  (mapconcat #'file-name-nondirectory
                                                             files ", ")))
                                      #'completions)))
         (if noninteractive
             (if-let (choice (gethash input table))
                 (cons (org-pivot-search--category choice)
                       choice)
               (cons nil input))
           (if-let (choice (gethash input table))
               (org-pivot-search--run-choice choice
                                             :display-action display-action
                                             :indirect indirect)
             (funcall org-pivot-search-fallback-function input files))))))))

(defun org-pivot-search-default-arguments-1 (&optional arg)
  "Arguments specification.

See `org-pivot-search-default-arguments'."
  (list (list (buffer-file-name (org-base-buffer (current-buffer))))
        :display-action org-pivot-search-display-action
        :indirect nil))

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
     (let* ((marker (get-text-property 0 'org-marker choice))
            (ret (if indirect
                     (org-with-point-at marker
                       (org-tree-to-indirect-buffer))
                   (with-current-buffer (marker-buffer marker)
                     (goto-char marker)
                     (current-buffer)))))
       (cl-etypecase ret
         (string (pop-to-buffer ret display-action))
         (buffer (pop-to-buffer ret display-action))
         (window (select-window ret)))
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
      (org-headline (when org-pivot-search-annotation-function
                      (pcase-exhaustive (cdr (get-text-property 0 'multi-category candidate))
                        (`(,_headline . (,beg-marker . ,_end-marker))
                         (when (functionp org-pivot-search-annotation-function)
                           (funcall org-pivot-search-annotation-function beg-marker))))))
      (otherwise nil))))

;;;; Building candidates

(defun org-pivot-search--entry-candidate (&optional width prefix)
  (let* ((element (org-element-headline-parser))
         (headline (org-link-display-format
                    (org-element-property :raw-value element)))
         (olp (thread-last
                (org-get-outline-path 'with-self 'use-cache)
                (mapcar #'org-no-properties)
                (mapcar #'org-link-display-format)))
         (candidate (org-format-outline-path olp width prefix))
         (beg-marker (copy-marker (org-element-property :begin element)))
         (end-marker (copy-marker (org-element-property :end element)))
         (item (cons 'org-headline (cons headline (cons beg-marker end-marker)))))
    (add-text-properties 0 (length candidate)
                         (list 'org-element (org-pivot-search--circular-free element)
                               'multi-category item
                               'org-marker beg-marker)
                         candidate)
    candidate))

(defun org-pivot-search--circular-free (element)
  (thread-first
    element
    (org-element-put-property :parent nil)
    (org-element-put-property :title nil)))

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
