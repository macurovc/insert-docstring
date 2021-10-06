;;; insert-docstring.el --- Python Docstring Inserter
;;
;; Copyright (c) 2021 Marco Vocialta
;;
;; Author: Marco Vocialta <macurovc@tutanota.com>
;; URL: https://github.com/macurovc/insert-docstring
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-macs)
(require 'cl-seq)

(setq insert-docstring--python-tab-width
      (if (boundp 'python-tab-width) python-tab-width 4)
  )
(setq insert-docstring--default-python-indentation
      (make-string insert-docstring--python-tab-width ? )
      )

(setq insert-docstring--python-function-indentation-regex
      (rx line-start (group (* blank)) "def" (or blank "\n"))
      )
(setq insert-docstring--python-function-name-regex
      (rx "def" (+ (or blank "\n"))
          (group (+ (not whitespace)))
          (* (or blank "\n")) "(")
      )

(setq insert-docstring--python-function-arguments-regex
      (rx "(" (group (* (not (any "(" ")")))) ")")
      )
(setq insert-docstring--python-function-end-regex
      (rx ")" (* (not (any ":"))) ":")
      )


(cl-defstruct insert-docstring--argument-data
  "Data associated to a function argument."
  name description
  )

(cl-defstruct insert-docstring--function-data
  "Data associated to a function."
  indentation-string short-description long-description arguments return
  )


(defun python-insert-google-docstring-at-point ()
  "Insert a Google docstring for the Python function at point."
  (interactive)
  (let* ((function-data (insert-docstring--function-data-at-point))
         (google-docstring (insert-docstring--python-google-docstring function-data))
         )
    (insert-docstring--insert-python-docstring-with-indentation google-docstring)
    )
  )


(defun insert-docstring--function-data-at-point ()
  "Return a function-data struct with the data of the function at
  point."
  (let ((indentation-string (insert-docstring--match-string-for-python-function-at-point
                             insert-docstring--python-function-indentation-regex))
        (function-name (insert-docstring--match-string-for-python-function-at-point
                       insert-docstring--python-function-name-regex))
        (arguments-string (insert-docstring--match-string-for-python-function-at-point
                          insert-docstring--python-function-arguments-regex))
        )
    (insert-docstring--make-function-data
     indentation-string
     function-name
     (insert-docstring--get-python-arguments-names-from-string arguments-string)
     )
    )
  )


(defun insert-docstring--match-string-for-python-function-at-point (regex)
  "Match REGEX for the python function at point and return its first group."
  (save-excursion
    (re-search-forward insert-docstring--python-function-end-regex)
    (re-search-backward regex)
    (match-string 1)
    )
  )


(defun insert-docstring--get-python-arguments-names-from-string (arguments-string)
  "Parse the argument names contained in ARGUMENTS-STRING and
return them in a list."
  (if (string-equal "" arguments-string) nil
    (cl-remove-if
     (lambda (string)
       (string-match-p (rx (or "[" "]")) string)
       )
     (mapcar (lambda (single-argument-string)
               (car (split-string single-argument-string ":" t
                                  (rx (+ (or blank "\n")))))
               )
             (split-string arguments-string ",")
             )
     )
    )
  )


(defun insert-docstring--make-function-data (indentation-string function-name argument-names)
  "Create a function-data struct for a function with indentation
  INDENTATION-STRING, with name FUNCTION-NAME and arguments
  ARGUMENT-NAMES."
  (make-insert-docstring--function-data
   :indentation-string indentation-string
   :short-description (insert-docstring--get-short-description-from-user function-name)
   :long-description (insert-docstring--get-long-description-from-user function-name)
   :arguments (insert-docstring--make-arguments-data argument-names)
   :return (insert-docstring--get-return-description-from-user)
   )
  )


(defun insert-docstring--make-arguments-data (argument-names)
  "Ask the user for the description of each argument in the
ARGUMENT-NAMES list and return them as arguments-data."
  (when argument-names
    (let ((name (car argument-names)))
      (cons
       (make-insert-docstring--argument-data
        :name name
        :description (read-string (format "Enter the description for argument '%s': " (car argument-names))))
       (insert-docstring--make-arguments-data (cdr argument-names))
       )
      )
    )
  )


(defun insert-docstring--get-short-description-from-user (function-name)
  "Ask the user for the short description of the function with name
FUNCTION_NAME."
  (read-string (format "Enter the short description for the function '%s': " function-name))
  )


(defun insert-docstring--get-long-description-from-user (function-name)
  "Ask the user for the long description of the function with name
FUNCTION_NAME."
  (let ((description
         (read-string
          (format
           "Enter the long description for the function '%s' (leave empty to omit it): "
           function-name))))
    (if (string-equal description "") nil description)
    )
  )


(defun insert-docstring--get-return-description-from-user ()
  "Ask the user of the description of the returned data."
  (let ((description (read-string "Enter the return description (leave empty to omit it): ")))
    (if (string-equal description "") nil description)
    )
  )


(defun insert-docstring--prefix-lines (lines prefix)
  "Prepend each string in the LINES list with the PREFIX string
and return them in a list.

If a string is empty, PREFIX doesn't get prepended."
  (mapcar (lambda (line)
            (if (string-equal "" line) line (concat prefix line))
            )
          lines)
  )


(defun insert-docstring--insert-python-docstring-with-indentation (docstring-lines)
  "Insert the DOCSTRING-LINES in the buffer."
  (save-excursion
    (re-search-forward insert-docstring--python-function-end-regex)
    (insert "\n")
    (insert (string-join docstring-lines "\n"))
    (insert "\n")
    )
  )


(defun insert-docstring--python-google-docstring (function-data)
  "Return the Google docstring lines corresponding to FUNCTION-DATA."
  (let ((docstring-indentation
         (concat (insert-docstring--function-data-indentation-string function-data)
                 insert-docstring--default-python-indentation))
        (short-description (insert-docstring--function-data-short-description function-data))
        (long-description (insert-docstring--function-data-long-description function-data))
        (arguments (insert-docstring--function-data-arguments function-data))
        (return (insert-docstring--function-data-return function-data))
        )
    (append (insert-docstring--line-to-indented-paragraph (format "\"\"\"%s" short-description) docstring-indentation)
            (insert-docstring--python-google-docstring-long-description long-description docstring-indentation)
            (insert-docstring--python-google-docstring-arguments arguments docstring-indentation)
            (insert-docstring--python-google-docstring-returns return docstring-indentation)
            (insert-docstring--prefix-lines '("" "\"\"\"") docstring-indentation)
            )
    )
  )


(defun insert-docstring--python-google-docstring-long-description (long-description docstring-indentation)
  "Return the docstring lines about the ARGUMENTS with the given
DOCSTRING-INDENTATION."
  (if long-description
      (append
       (list "")
       (insert-docstring--line-to-indented-paragraph long-description docstring-indentation)
       )
    )
  )


(defun insert-docstring--python-google-docstring-arguments (arguments docstring-indentation)
  "Return the docstring lines about the ARGUMENTS with the given
DOCSTRING-INDENTATION."
  (append
   (if arguments (append (list "")
                         (insert-docstring--prefix-lines '("Args:") docstring-indentation))
     )
   (insert-docstring--python-google-docstring-arguments-list
    arguments
    (concat docstring-indentation insert-docstring--default-python-indentation)
    )
   )
  )


(defun insert-docstring--python-google-docstring-arguments-list (arguments arguments-docstring-indentation)
  "Return the docstring lines about the ARGUMENTS list with the
given DOCSTRING-INDENTATION."
  (when arguments
    (let* ((argument (car arguments))
           (name (insert-docstring--argument-data-name argument))
           (description (insert-docstring--argument-data-description argument)))
      (append
       (insert-docstring--line-to-indented-paragraph (format "%s: %s" name description)
                                                     arguments-docstring-indentation
                                                     insert-docstring--default-python-indentation)
       (insert-docstring--python-google-docstring-arguments-list
        (cdr arguments) arguments-docstring-indentation)
       )
      )
    )
  )


(defun insert-docstring--python-google-docstring-returns (return-description docstring-indentation)
  "Return the docstring lines about the returned data using the
RETURN-DESCRIPTION string with the DOCSTRING-INDENTATION
provided."
  (if return-description
      (append (list "")
              (insert-docstring--prefix-lines '("Returns:") docstring-indentation)
              (insert-docstring--line-to-indented-paragraph
               return-description
               (concat insert-docstring--default-python-indentation docstring-indentation))
              )
    )
  )


(defun insert-docstring--line-to-indented-paragraph (line indentation &optional new-line-prefix column-width)
  "Split a line multiple ones with the given indentation and
  global line width."
  (let ((max-length
         (- (or column-width (insert-docstring--get-fill-column))
            (insert-docstring--indentation-length indentation))))
    (insert-docstring--prefix-lines
     (insert-docstring--append-words-to-paragraph () max-length (split-string line) new-line-prefix)
     indentation
     )
    )
  )


(defun insert-docstring--get-fill-column ()
  "Return the fill column value."
  (if (boundp 'insert-docstring--fill-column) insert-docstring--fill-column
    (if (boundp 'python-fill-column) python-fill-column
      fill-columnll-column)
    )
  )


(defun insert-docstring--indentation-length (indentation-string &optional num-tab-chars)
  "Compute the number of character in INDENTATION-STRING if tabs
were converted to spaces. NUM-TAB-CHARS corresponds to the spaces
in a tab (default: tab-width)."
  (cl-reduce
   '+
   (mapcar
    (lambda (char) (if (char-equal char ?\t)
                       (or num-tab-chars tab-width)
                     1))
    indentation-string)
   )
  )


(defun insert-docstring--append-words-to-paragraph (lines max-line-width words &optional new-line-prefix)
  "Append the WORDS to the strings in LINES. A new line is
appended whenever MAX-LINE-WIDTH is reached. New lines can be
optionally prefixed with NEW-LINE-PREFIX."
  (if words
      (insert-docstring--append-words-to-paragraph
       (let ((next-word (car words))
             (next-line (car (last lines))))
         (if next-line
             (if (<= (+ (length next-line) (length next-word) 1) max-line-width)
                 (append (nbutlast lines) (list (concat next-line " " next-word)))
               (append lines (list (concat new-line-prefix next-word)))
               )
           (list next-word)
           )
         )
       max-line-width (cdr words) new-line-prefix
       )
    lines
    )
  )

;; End:

(provide 'insert-docstring)

;;; insert-docstring.el ends here
