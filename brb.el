;;; brb.el --- Barberry Garden utilities and helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/brb
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.3") (emacs "29.3") (dash "2.19.1") (s "1.13.0") (vulpea "0.3.0") (vino) (widget-extra))
;; Keywords:

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Barberry Garden utilities and helpers.
;;
;; This module contains only common things used in other modules.
;; Refer to them for more information.
;;

;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)
(require 'vulpea)

(defconst brb-currency "UAH"
  "Main currency for Barberry Garden.

Most of the utilities assume that the price is in this currency and if
not, their behaviour is either not well defined or they no-op.")

;;; * Price

(defun brb-price-format (amount)
  "Format AMOUNT as price.

The price is rounded up and grouped by thousands. If the amount is nil,
- character is displayed."
  (format
   "%s %s"
   (if amount (brb-string-group-number (ceiling amount))
     "–")
   brb-currency))

(defun brb-price-to-number (price)
  "Convert PRICE to number.

Returns nil if PRICE is of different currency than `brb-currency'.

  (brb-price-to-number (brb-price-format X)) returns X."
  (cond
   ((string-equal price (brb-price-format nil)) nil)
   ((s-suffix-p brb-currency price)
    (string-to-number
     (s-replace " " "" (s-chop-suffix brb-currency price))))))

(defun brb-string-group-number (num &optional size char)
  "Format NUM as string grouped to SIZE with CHAR."
  ;; Based on code for `math-group-float' in calc-ext.el
  (let* ((size (or size 3))
         (char (or char " "))
         (str (if (stringp num)
                  num
                (number-to-string num)))
         ;; omit minus sign
         (neg (string-prefix-p "-" str))
         (str (s-chop-prefix "-" str))
         ;; omitting any trailing non-digit chars
         ;; NOTE: Calc supports BASE up to 36 (26 letters and 10 digits ;)
         (pt (or (string-match "[^0-9a-zA-Z]" str) (length str))))
    (while (> pt size)
      (setq str (concat (substring str 0 (- pt size))
                        char
                        (substring str (- pt size)))
            pt (- pt size)))
    (if neg
        (concat "-" str)
      str)))

;;; * Compatibility / code migration
;;
;; The following code exists simply to ease the migration from private
;; configurations to public repository.
;;

(cl-defun brb-string-table (&key
                            data
                            header
                            header-sep
                            header-sep-start
                            header-sep-conj
                            header-sep-end
                            pad-type
                            pad-str
                            sep
                            row-start
                            row-end
                            width)
  "Format DATA as a table.

HEADER is optional. When present HEADER-SEP, HEADER-SEP-START,
HEADER-SEP-CONJ, HEADER-SEP-END control line between header and
data.

DATA is list of lists. Each column is aligned by padding with
PAD-STR either on left or right depending on value of PAD-TYPE.

The width of columns is controlled by WIDTH. If it\\='s nil, each
column takes full width. If it\\='s a list, each element must be
either \\='full or integer enabling truncation.

Each row begins with ROW-START and ends with ROW-END. Each value
in row is separated by SEP."
  (let* ((all (if header (cons header data) data))
         (n (seq-reduce
             (lambda (r v)
               (min r (if (eq 'sep v) r (seq-length v))))
             all
             (seq-length (car all))))
         (widths (seq-reduce
                  (lambda (r v)
                    (if (eq 'sep v) r
                      (seq-map-indexed
                       (lambda (a i)
                         (max
                          (pcase (or (and width
                                          (listp width)
                                          (nth i width))
                                     'full)
                            (`full (length (brb-string-from a)))
                            (n n))
                          (or (nth i r)
                              0)))
                       v)))
                  all
                  nil))
         (pad-str (or pad-str " "))
         (pad-str-props (text-properties-at 0 pad-str))
         (pad-fns (seq-map
                   (lambda (i)
                     (pcase (or (and pad-type
                                     (listp pad-type)
                                     (nth i pad-type))
                                pad-type
                                'left)
                       (`left (lambda (len padding s)
                                (let ((extra (max 0 (- len (length s)))))
                                  (concat
                                   (apply #'propertize
                                          (make-string extra (string-to-char padding))
                                          pad-str-props)
                                   s))))
                       (`right (lambda (len padding s)
                                 (let ((extra (max 0 (- len (length s)))))
                                   (concat
                                    s
                                    (apply #'propertize
                                           (make-string extra (string-to-char padding))
                                           pad-str-props)))))))
                   (-iota n)))
         (row-start (or row-start ""))
         (row-end (or row-end ""))
         (sep (or sep " ")))
    (concat
     ;; header
     (when header
       (brb-string-table--format-line header
         :sep sep
         :pad-fns pad-fns
         :pad-str pad-str
         :widths widths
         :row-start row-start
         :row-end row-end))
     (when header "\n")
     (when (and header header-sep)
       (brb-string-table--format-line (-repeat n "")
         :sep (or header-sep-conj sep)
         :pad-fns pad-fns
         :pad-str header-sep
         :widths widths
         :row-start (or header-sep-start row-start)
         :row-end (or header-sep-end row-end)))
     (when (and header header-sep) "\n")
     ;; data
     (mapconcat
      (lambda (v)
        (if (and (eq 'sep v) header header-sep)
            (brb-string-table--format-line (-repeat n "")
              :sep (or header-sep-conj sep)
              :pad-fns pad-fns
              :pad-str header-sep
              :widths widths
              :row-start (or header-sep-start row-start)
              :row-end (or header-sep-end row-end))
          (brb-string-table--format-line (seq-take v n)
            :sep sep
            :pad-fns pad-fns
            :pad-str pad-str
            :widths widths
            :row-start row-start
            :row-end row-end)))
      data
      "\n"))))

(cl-defun brb-string-table--format-line (values
                                         &key
                                         sep
                                         pad-fns
                                         pad-str
                                         widths
                                         row-start
                                         row-end)
  "Format lines consisting of VALUES.

Line begins with optional ROW-START and ends with optional
ROW-END.

Each value is padded with PAD-STR using PAD-FNS to achieve cell
WIDTHS. Each value is separated by SEP."
  (declare (indent 1))
  (concat
   row-start
   (string-join
    (seq-map-indexed
     (lambda (a i)
       (let* ((width-max (nth i widths))
              (str (brb-string-from a)))
         (s-truncate
          width-max
          (funcall (nth i pad-fns)
                   width-max
                   pad-str
                   str)
          (propertize "..."
                      'help-echo str))))
     values)
    sep)
   row-end))

(defun brb-string-from (value)
  "Convert VALUE to string."
  (cond
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((symbolp value) (symbol-name value))
   ((vulpea-note-p value) (brb-vulpea-buttonize value))
   (t (user-error
       "Unsupported type of \"%s\"" value))))

(defun brb-vulpea-buttonize (note &optional title-fn)
  "Create a link to `vulpea' NOTE.

Title is calculated based on TITLE-FN (takes note as a single
parameter), defaulting to `vulpea-note-title'."
  (buttonize (funcall (or title-fn #'vulpea-note-title)
                      note)
             #'vulpea-visit
             (vulpea-note-id note)))

(provide 'brb)
;;; brb.el ends here
