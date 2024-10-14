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

(provide 'brb)
;;; brb.el ends here
