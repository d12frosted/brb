;;; brb.el --- Barberry Garden utilities and helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/brb
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.3") (dash "2.19.1") (s "1.13.0") (vulpea "0.3.0") (vino "0.4.0") (widget-extra "1.0.0"))
;; Keywords: extensions

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

(require 'vulpea)

(require 'cl-lib)
(require 'calc)
(require 'calc-stat)

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

(defun brb-to-price (string)
  "Convert STRING to price object.

Result is an alist ((amount . float) (currency . string))."
  (let ((pieces (s-split " " string))
        (amount 0))
    (unless (= (seq-length pieces) 2)
      (user-error "Not valid price string: `%s'" string))
    (setq amount (string-to-number (nth 0 pieces)))
    (unless (>= amount 0)
      (error "Negative price is not supported: `%d'" amount))
    `((amount . ,amount)
      (currency . ,(nth 1 pieces)))))

(defun brb-price (wine)
  "Return price of WINE entry.

Result is an alist ((amount . float) (currency . string))."
  (when-let* ((prices (->> (vulpea-note-meta-get-list wine "price")
                           (--remove (s-prefix-p "XXX" it)))))
    (when (> (seq-length prices) 1)
      (user-error "%S has multiple prices, which is not supported"
                  (vulpea-note-title wine)))
    (let ((pieces (s-split " " (nth 0 prices)))
          (amount 0))
      (unless (= (seq-length pieces) 2)
        (user-error "%S has unsupported price: `%s'"
                    (vulpea-note-title wine)
                    (nth 0 prices)))
      (setq amount (string-to-number (nth 0 pieces)))
      (unless (> amount 0)
        (error "%S has unsupported price amount: `%d'" (vulpea-note-title wine) amount))
      `((amount . ,amount)
        (currency . ,(nth 1 pieces))))))

;;; * QPR

(defun brb-qpr (price score wine)
  "Calculate QPR.

SCORE is a rational number in [0, 5].
PRICE is a positive number in `brb-currency'.
WINE is a note representing wine.

QPR is adjusted to account for VOLUME."
  (when (and score price (> price 0) (> score 0))
    (let* ((volume (or (vulpea-note-meta-get wine "volume" 'number) 750))
           (appellation (vulpea-note-meta-get wine "appellation"))
           (multiplier (if (and appellation
                                (seq-contains-p
                                 '("Champagne AOC")
                                 (nth 3 (s-match org-link-any-re appellation))))
                           2500
                         1600))
           (price (* price (/ 750.0 volume)))
           (p (calc-from-number (float price)))
           (s (calc-from-number (float score))))
      (calc-to-number
       (math-div
        (math-sqrt
         (math-div
          (math-mul
           multiplier
           (math-mul (math-pow (calcFunc-fact s) (math-add 1 (math-phi)))
                     (calcFunc-ln (math-add (calc-from-number 1.1) s))))
          p))
        100)))))

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

(defun calc-from-number (number)
  "Convert NUMBER to Calc format."
  (math-read-number (number-to-string number)))

(defun calc-to-number (number)
  "Convert NUMBER from Calc format."
  (read (math-format-number number)))

(provide 'brb)
;;; brb.el ends here
