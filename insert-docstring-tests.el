;;; insert-docstring-tests.el --- Tests for the Python Google docstring inserter -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Marco Vocialta

;; Author: Marco Vocialta <macurovc@tutanota.com>
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/macurovc/insert-docstring
;; Version: 1.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains tests for the functions in insert-docstring.el that don't
;; require user inputs and don't modify a buffer.

;;; Code:

(defconst insert-docstring--fill-column 79)

(ert-deftest python-python-function-name-test ()
  "Match the name of a simple function."
  (let ((string "def something()"))
    (should (equal (string-match insert-docstring--python-function-name-regex string) 0))
    (should (equal (match-string 1 string) "something"))))

(ert-deftest python-function-name-with-indent-test ()
  "Match the name of a function with some indentation."
  (let ((string "\n \t   def    something  ("))
    (should (equal (string-match insert-docstring--python-function-name-regex string) 6))
    (should (equal (match-string 1 string) "something"))))

(ert-deftest python-function-name-with-underscores-test ()
  "Match the name of a function with underscores."
  (let ((string "\n \t   def    _something_else  ("))
    (should (equal (string-match insert-docstring--python-function-name-regex string) 6))
    (should (equal (match-string 1 string) "_something_else"))))

(ert-deftest python-end-of-function-test ()
  "Match the end of a function definition."
  (should (equal (string-match insert-docstring--python-function-end-regex "):") 0))
  (should (equal (string-match insert-docstring--python-function-end-regex ")  ->  None : ") 0)))

(ert-deftest python-end-of-function-with-newline-test ()
  "Match the end of a function definition with a None return type and newlines."
  (let ((string ") \n ->\n \n\nNone:\n"))
    (should (equal (string-match insert-docstring--python-function-end-regex string) 0))))

(ert-deftest python-end-of-function-with-type-test ()
  "Match the end of a function definition with an elaborate return type."
  (let ((string "  ) \n ->\n \n\nMap[int, int]:\n"))
    (should (equal (string-match insert-docstring--python-function-end-regex string) 2))))

(ert-deftest python-argument-string-regex-test ()
  "Match the arguments string."
  (let ((string "def toto_blabla(hey1):"))
    (should (equal (string-match insert-docstring--python-function-arguments-regex string) 15))
    (should (equal (match-string 1 string) "hey1"))))

(ert-deftest python-argument-string-regex-test ()
  "Match the arguments string."
  (let ((string "\ndef\n\ntoto(  bubu : List[str] ,   lala   )  -> \n\nNone : "))
    (should (equal (string-match insert-docstring--python-function-arguments-regex string) 10))
    (should (equal (match-string 1 string) "  bubu : List[str] ,   lala   "))))

(ert-deftest python-argument-string-regex-with-newlines-test ()
  "Match the arguments string."
  (let ((string "\ndef\n\ntoto(  bubu : List[str] ,\n   lala   )  -> \n\nNone : "))
    (should (equal (string-match insert-docstring--python-function-arguments-regex string) 10))
    (should (equal (match-string 1 string) "  bubu : List[str] ,\n   lala   "))))

(ert-deftest python-function-indentation-test ()
  "Match the arguments string."
  (let ((string "\ndef\n\ntoto()  -> \n\nNone : "))
    (should (equal (string-match insert-docstring--python-function-indentation-regex string) 1))
    (should (equal (match-string 1 string) ""))))

(ert-deftest python-function-indentation-with-tabs-and-spaces-test ()
  "Match the arguments string."
  (let ((string "\n  \t def\n\ntoto()  -> \n\nNone : "))
    (should (equal (string-match insert-docstring--python-function-indentation-regex string) 1))
    (should (equal (match-string 1 string) "  \t "))))

(ert-deftest python-parse-argument-list-test ()
  "Test the arguments list parser function."
  (progn
    (should (equal (insert-docstring--get-python-arguments-names-from-string "") nil))
    (should (equal (insert-docstring--get-python-arguments-names-from-string
                    "one") '("one")))
    (should (equal (insert-docstring--get-python-arguments-names-from-string
                    "  first,  second, third  ") '("first" "second" "third")))
    (should (equal (insert-docstring--get-python-arguments-names-from-string
                    "  first: Type,  second: Map[some, other], third : Dict[str, List[str]] ")
                   '("first" "second" "third")))
    (should (equal (insert-docstring--get-python-arguments-names-from-string
                    "  first: Type\n,\n  second:\n Map[some, other]") '("first" "second")))
    (should (equal (insert-docstring--get-python-arguments-names-from-string
                    "  first: Type = 1,\n  second: Map[some, other] = 2") '("first" "second")))
    (should (equal (insert-docstring--get-python-arguments-names-from-string
                    "  first = 1,\n  second = 2") '("first" "second")))))

(ert-deftest prefix-lines-test ()
  "Test the function to prefix lines with blanks."
  (should (equal (insert-docstring--prefix-lines '("some" "lines") "  ") '("  some" "  lines")))
  (should (equal (insert-docstring--prefix-lines '("some" "") "  ") '("  some" ""))))

(ert-deftest python-google-docstring-arguments-test ()
  "Test the generation of the argument list in the docstring."
  (should (equal (insert-docstring--python-google-docstring-arguments () "") nil))
  (let ((arguments (list (make-insert-docstring--argument-data :name "one" :description "this one")
                         (make-insert-docstring--argument-data :name "two" :description "this two"))))
    (should (equal (insert-docstring--python-google-docstring-arguments arguments "pre")
                   '("" "preArgs:" "pre    one: this one" "pre    two: this two")))))

(ert-deftest python-google-docstring-returns-test ()
  "Test the generation of the return statement in the docstring."
  (should (equal (insert-docstring--python-google-docstring-returns nil "") nil))
  (should (equal (insert-docstring--python-google-docstring-returns "something" "")
                 '("" "Returns:" "    something")))
  (should (equal (insert-docstring--python-google-docstring-returns "something" "  ")
                 '("" "  Returns:" "      something"))))

(ert-deftest python-google-docstring-title-only ()
  "Test the generation of the entire docstring."
  (should (equal
           (insert-docstring--python-google-docstring
            (make-insert-docstring--function-data
             :indentation-string ""
             :short-description "title"
             :long-description nil
             :arguments nil
             :return nil))
           '("    \"\"\"title"
             ""
             "    \"\"\""))))

(ert-deftest python-google-docstring-title-and-return ()
  "Test the generation of the entire docstring."
  (should (equal
           (insert-docstring--python-google-docstring
            (make-insert-docstring--function-data
             :indentation-string ""
             :short-description "title"
             :long-description nil
             :arguments nil
             :return "something"))
           '("    \"\"\"title"
             ""
             "    Returns:"
             "        something"
             ""
             "    \"\"\""))))

(ert-deftest python-google-docstring-title-args-and-return ()
  "Test the generation of the entire docstring."
  (should (equal
           (insert-docstring--python-google-docstring
            (make-insert-docstring--function-data
             :indentation-string ""
             :short-description "title"
             :long-description nil
             :arguments (list (make-insert-docstring--argument-data :name "one" :description "this one")
                              (make-insert-docstring--argument-data :name "two" :description "this two"))
             :return "something"))
           '("    \"\"\"title"
             ""
             "    Args:"
             "        one: this one"
             "        two: this two"
             ""
             "    Returns:"
             "        something"
             ""
             "    \"\"\""))))

(ert-deftest python-google-docstring-title-and-args ()
  "Test the generation of the entire docstring."
  (should (equal
           (insert-docstring--python-google-docstring
            (make-insert-docstring--function-data
             :indentation-string ""
             :short-description "title"
             :long-description nil
             :arguments (list (make-insert-docstring--argument-data :name "one" :description "this one")
                              (make-insert-docstring--argument-data :name "two" :description "this two"))
             :return nil))
           '("    \"\"\"title"
             ""
             "    Args:"
             "        one: this one"
             "        two: this two"
             ""
             "    \"\"\""))))

(ert-deftest python-google-docstring-all-fields ()
  "Test the generation of the entire docstring."
  (should (equal
           (insert-docstring--python-google-docstring
            (make-insert-docstring--function-data
             :indentation-string "    "
             :short-description "this is a very very very very very very very very very very very very long title"
             :long-description "this is a very very very very very very very very very very very very long description"
             :arguments (list (make-insert-docstring--argument-data
                               :name "one"
                               :description "this one is very very very very very very very very very very long")
                              (make-insert-docstring--argument-data :name "two" :description "this two"))
             :return "what a very very very very very very very very very very very very long return"))
           '("        \"\"\"this is a very very very very very very very very very very very"
             "        very long title"
             ""
             "        this is a very very very very very very very very very very very very"
             "        long description"
             ""
             "        Args:"
             "            one: this one is very very very very very very very very very very"
             "                long"
             "            two: this two"
             ""
             "        Returns:"
             "            what a very very very very very very very very very very very very"
             "            long return"
             ""
             "        \"\"\""))))


(ert-deftest indentation-length ()
  "Test the computation of the indentation length."
  (should (equal (insert-docstring--indentation-length "") 0))
  (should (equal (insert-docstring--indentation-length " ") 1))
  (should (equal (insert-docstring--indentation-length "   ") 3))
  (should (equal (insert-docstring--indentation-length " \t  " 2) 5))
  (should (equal (insert-docstring--indentation-length " \t  \t  " 3) 11)))


(ert-deftest append-words-to-paragraph ()
  "Test the function to append words to a paragraph."
  (should (equal (insert-docstring--append-words-to-paragraph () 5 '("one" "two" "three"))
                 '("one" "two" "three")))
  (should (equal (insert-docstring--append-words-to-paragraph () 7 '("one" "two" "three"))
                 '("one two" "three")))
  (should (equal (insert-docstring--append-words-to-paragraph () 13 '("one" "two" "three"))
                 '("one two three")))
  (should (equal (insert-docstring--append-words-to-paragraph '("one") 7 '("two" "three"))
                 '("one two" "three")))
  (should (equal (insert-docstring--append-words-to-paragraph '("one") 7 '("two" "three") "  ")
                 '("one two" "  three")))
  (should (equal (insert-docstring--append-words-to-paragraph () 7 '("two" "three") "  ")
                 '("two" "  three"))))


(ert-deftest line-to-indented-paragraph ()
  "Test splitting a line into a paragraph."
  (should (equal (insert-docstring--line-to-indented-paragraph "this is a line" "  " " " 9)
                 '("  this is" "   a line"))))

;; End:

;;; insert-docstring-tests.el ends here
