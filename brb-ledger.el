;;; brb-ledger.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>

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
;; NB! These functions are tied to specific use case. Feel free to
;; copy them, and modify as you wish. But don't expect stability or
;; new features.
;;
;; Requires hledger command line tool.
;;
;; A set of helpers to maintain Barberry Garden ledger. There are the
;; following interactive functions of interest:
;;
;; - `brb-ledger-display' - display ledger. Includes total balance,
;;   balance for each participant and list of transactions. Many other
;;   interactive functions work in a DWIM fashion when used in this
;;   buffer.
;;
;; - `brb-ledger-spend' - spend some amount. Modifies total balance.
;;
;; - `brb-ledger-spend-personal' - spend some amount from personal
;;   account. Modified total balance.
;;
;; - `brb-ledger-charge' - charge a person for some amount. Modifies
;;   balance of specific participant (subtract). Useful to track who
;;   paid what.
;;
;; - `brb-ledger-deposit' - deposit some amount for a person. Modifies
;;   balance of specific participant AND total balance.
;;
;; P.S. Person, participant and convive are used as synonyms in this
;; file.

;;; Code:

(require 's)
(require 'vulpea)
(require 'brb)
(require 'brb-widget)
(require 'widget-extra)

;;; * Configurations

(defvar brb-ledger-file nil
  "Path to Barberry Garden ledger file.

Can be a string or list of two strings, where first one is for reading
and the second one for writing.")

(defvar brb-ledger-buffer-name "*Barberry Garden Balance*"
  "Name of balance buffer.")

;;; * File resolution

(defun brb-ledger-file--read (&optional path)
  "Return path to ledger file for reading.

Unless specified, `brb-ledger-file' is used as PATH."
  (setq path (or path brb-ledger-file))
  (cond
   ((stringp path) path)
   ((functionp path) (funcall path))
   ((and (listp path) (= 2 (seq-length path))) (brb-ledger-file--read (nth 0 path)))
   (t (user-error "Invalid path to ledger file: %s" path))))

(defun brb-ledger-file--write (&optional path)
  "Return path to ledger file for reading.

Unless specified, `brb-ledger-file' is used as PATH."
  (setq path (or path brb-ledger-file))
  (cond
   ((stringp path) path)
   ((functionp path) (funcall path))
   ((and (listp path) (= 2 (seq-length path))) (brb-ledger-file--write (nth 1 path)))
   (t (user-error "Invalid path to ledger file: %s" path))))

;;; * Transaction API

(cl-defun brb-ledger-record-txn (&key date code comment account-to account-from amount)
  "Record transaction.

DATE (can be nil) is a time object as returned by `current-time'.

CODE (can be nil) is idempotency key. When non-nil, any transaction
sharing provided CODE is replaced.

COMMENT (can be nil) is a transaction description.

ACCOUNT-TO is account that receives AMOUNT.

ACCOUNT-FROM is account that spends AMOUNT.

AMOUNT is number in `brb-currency'.

Transaction is recorded into `brb-ledger-file'."
  (when code
    (with-current-buffer (find-file-noselect (brb-ledger-file--write) t)
      (revert-buffer t t t)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward (concat "(" code ")") nil 'noerror)
          (forward-line -1)
          (--dotimes 4
            (delete-region (line-beginning-position) (1+ (line-end-position))))))
      (save-buffer)))
  (let* ((cmd (format
               "echo '\n%s%s%s\n    %s  %s %s\n    %s' >> '%s'"
               (format-time-string "%Y/%m/%d" date)
               (if code
                   (concat " (" (s-replace "'" "\\'" code) ")")
                 "")
               (if comment
                   (concat " " (s-replace "'" "\\'" comment))
                 "")
               account-to
               amount
               brb-currency
               account-from
               (brb-ledger-file--write)))
         (res (shell-command-to-string cmd)))
    (message res)))

;;; * Convive-related API

;;;###autoload
(defun brb-ledger-deposit ()
  "Deposit an amount for convive."
  (interactive)
  (let* ((name (brb-ledget-buffer-convive-at-point))
         (convive (vulpea-select-from
                   "People"
                   (vulpea-db-query-by-tags-some '("people"))
                   :require-match t
                   :initial-prompt name))
         (data (brb-ledger-data-read))
         (balance (assoc-default (vulpea-note-id convive) (brb-ledger-data-balances data)))
         (amount (read-number "Amount: " (when balance (* -1 balance))))
         (date (org-read-date nil t))
         (comment (read-string "Comment: ")))
    (brb-ledger-record-txn
     :date date
     :comment (concat "deposit: " comment)
     :account-to (concat "balance:" (vulpea-note-id convive))
     :account-from (concat "convive:" (vulpea-note-id convive))
     :amount amount)
    (when (get-buffer brb-ledger-buffer-name)
      (brb-ledger-display))))

;;;###autoload
(cl-defun brb-ledger-charge (&key convive amount date comment code)
  "Charge an amount from convive.

CONVIVE, AMOUNT, DATE and COMMENT are optional arguments. Unless
specified, user is asked to provide them interactively.

CODE can be passed only in non-interactive usage. See
`brb-ledger-record-txn' to learn about this argument."
  (interactive)
  (let* ((name (unless convive
                 (brb-ledget-buffer-convive-at-point)))
         (convive (or convive
                      (vulpea-select-from
                       "People"
                       (vulpea-db-query-by-tags-some '("people"))
                       :require-match t
                       :initial-prompt name)))
         (amount (or amount (read-number "Amount: ")))
         (date (or date (org-read-date nil t)))
         (comment (or comment (read-string "Comment: "))))
    (brb-ledger-record-txn
     :date date
     :code code
     :comment (concat "charge: " comment)
     :account-to "balance:assets"
     :account-from (concat "balance:" (vulpea-note-id convive))
     :amount amount)
    (when (get-buffer brb-ledger-buffer-name)
      (brb-ledger-display))))

;;; * Spending API

;;;###autoload
(cl-defun brb-ledger-spend (&key amount date comment code)
  "Spend an amount on event.

AMOUNT, DATE and COMMENT are optional arguments. Unless
specified, user is asked to provide them interactively.

CODE can be passed only in non-interactive usage. See
`brb-ledger-record-txn' to learn about this argument."
  (interactive)
  (let ((amount (or amount (read-number "Amount: ")))
        (date (or date (org-read-date nil t)))
        (comment (or comment (read-string "Comment: "))))
    (brb-ledger-record-txn
     :date date
     :code code
     :comment (concat "spend: " comment)
     :account-to "expenses"
     :account-from "balance:assets"
     :amount amount)
    (when (get-buffer brb-ledger-buffer-name)
      (brb-ledger-display))))

;;;###autoload
(cl-defun brb-ledger-spend-personal (&key amount date comment code)
  "Spend from personal account.

AMOUNT, DATE and COMMENT are optional arguments. Unless
specified, user is asked to provide them interactively.

CODE can be passed only in non-interactive usage. See
`brb-ledger-record-txn' to learn about this argument."
  (interactive)
  (let ((amount (or amount (read-number "Amount: ")))
        (date (or date (org-read-date nil t)))
        (comment (or comment (read-string "Comment: "))))
    (brb-ledger-record-txn
     :date date
     :code code
     :comment (concat "spend personal: " comment)
     :account-to "spending:wines"
     :account-from "personal:account"
     :amount amount)
    (when (get-buffer brb-ledger-buffer-name)
      (brb-ledger-display))))

(cl-defun brb-ledger-buy-wine (&key wine price date code)
  "Buy a WINE from personal account.

PRICE is the actual purchase price. It must be in `brb-currency'.

DATE is purchase date (internal time).

See `brb-ledger-record-txn' to learn about CODE."
  (let* ((wine-id (if (vulpea-note-p wine)
                      (vulpea-note-id wine)
                    wine)))
    (brb-ledger-record-txn
     :date date
     :code code
     :comment (concat "[" wine-id "]")
     :account-to "spending:wines"
     :account-from "personal:account"
     :amount price)))

;;; * Balance API

;;;###autoload
(cl-defun brb-ledger-balance-of (convive &optional date)
  "Return balance of a given CONVIVE.

Optionally return the balance on DATE (inclusive).

Result is a number in `brb-currency'."
  (let* ((id (if (vulpea-note-p convive) (vulpea-note-id convive) convive))
         (cmds (if date
                   (let* ((time0 (if (stringp date) (date-to-time date) date))
                          (time1 (time-add time0 (* 60 60 24))))
                     (list
                      (format "hledger -f %s balance balance:%s -e %s"
                              (brb-ledger-file--read)
                              id
                              (format-time-string "%Y-%m-%d" time0))
                      (format "hledger -f %s balance balance:%s -b %s -e %s 'not:desc:charge'"
                              (brb-ledger-file--read)
                              id
                              (format-time-string "%Y-%m-%d" time0)
                              (format-time-string "%Y-%m-%d" time1))))
                 (list
                  (format "hledger -f %s balance balance:%s" (brb-ledger-file--read) id)))))
    (->> cmds
         (--map
          (->> it
               (shell-command-to-string)
               (s-trim)
               (s-lines)
               (-last-item)
               (string-to-number)))
         (--reduce-from (+ acc it) 0))))

;;; * Various balance-changing utils

(cl-defun brb-ledger-buy-wines-for (&key convive
                                         spend-amount
                                         charge-amount
                                         date)
  "Buy wines for CONVIVE.

Spend SPEND-AMOUNT on DATE and charge CHARGE-AMOUNT said CONVIVE.

Basically a convenient shortcut for charge + spend."
  (interactive)
  (let* ((name (unless convive
                 (seq-find
                  (lambda (str)
                    (and (not (s-matches-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" str))
                         (not (s-suffix-p brb-currency str))))
                  (s-split
                   "  "
                   (s-chop-prefix "- " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                   t))))
         (convive (or convive
                      (vulpea-select-from
                       "People"
                       (vulpea-db-query-by-tags-some '("people"))
                       :require-match t
                       :initial-prompt name)))
         (spend-amount (or spend-amount (read-number "Spend amount: ")))
         (charge-amount (or charge-amount (read-number "Charge amount: ")))
         (date (or date (org-read-date nil t))))
    (brb-ledger-spend :amount spend-amount
                      :date date
                      :comment (concat "Wine for " (vulpea-note-title convive)))
    (brb-ledger-charge :convive convive
                       :amount charge-amount
                       :date date)))

(cl-defun brb-ledger-receive-present (&optional source amount date)
  "Receive AMOUNT as a present from SOURCE on a DATE."
  (interactive)
  (let* ((source (or source
                     (vulpea-select "Source" :require-match t)))
         (amount (or amount (read-number "Amount: ")))
         (date (or date (org-read-date nil t))))
    (brb-ledger-record-txn
     :date date
     :comment "present"
     :account-to "balance:assets"
     :account-from (concat "source:" (vulpea-note-id source))
     :amount amount)
    (brb-ledger-display)))

;;; * Data reading
;;
;; Necessary structures and utilities to read them from hledger.
;;

(cl-defstruct brb-ledger-data
  total
  convives
  balances
  postings)

(cl-defstruct brb-ledger-personal-data total postings)
(cl-defstruct brb-ledger-convive-data total postings)

(cl-defstruct brb-ledger-posting
  date
  description
  account
  amount
  total)

(defun brb-ledger-data-read ()
  "Read balance data from `brb-ledger-file'."
  (let* ((prefix "balance:")
         (ignored '("assets"))

         (cmd-bal (format "hledger -f '%s' balance '%s'" (brb-ledger-file--read) prefix))
         (res-bal (split-string (shell-command-to-string cmd-bal) "\n" t " +"))
         (balances (->> (-drop-last 2 res-bal)
                        (--map
                         (let ((vs (split-string it prefix t " +")))
                           (cons
                            (cadr vs)
                            (string-to-number (car vs)))))
                        (--remove (seq-contains-p ignored (car it)))))
         (total (string-to-number (-last-item res-bal)))

         (cmd-accs (format "hledger -f '%s' accounts '%s'" (brb-ledger-file--read) prefix))
         (res-accs (shell-command-to-string cmd-accs))
         (convives (->> (split-string res-accs "\n" t)
                        (--map (string-remove-prefix prefix it))
                        (--remove (-contains-p ignored it))
                        (--map (if-let ((convive (vulpea-db-get-by-id it)))
                                   convive
                                 (user-error "Could not find convive with id %s" it)))))

         (postings (brb-ledger-postings-read prefix)))
    (make-brb-ledger-data
     :total total
     :convives convives
     :balances balances
     :postings postings)))

(defun brb-ledger-personal-data-read ()
  "Read personal balance data from `brb-ledger-file'."
  (let* ((prefix "personal:")
         (cmd-bal (format "hledger -f '%s' balance '%s'" (brb-ledger-file--read) prefix))
         (res-bal (split-string (shell-command-to-string cmd-bal) "\n" t " +"))
         (total (string-to-number (-last-item res-bal)))
         (postings (brb-ledger-postings-read prefix)))
    (make-brb-ledger-personal-data
     :total total
     :postings postings)))

(defun brb-ledger-convive-data-read (convive)
  "Read balance data from `brb-ledger-file' for given CONVIVE."
  (let* ((prefix (concat "balance:" (vulpea-note-id convive)))
         (cmd-bal (format "hledger -f '%s' balance '%s'" (brb-ledger-file--read) prefix))
         (res-bal (split-string (shell-command-to-string cmd-bal) "\n" t " +"))
         (total (string-to-number (-last-item res-bal)))
         (postings (brb-ledger-postings-read prefix)))
    (make-brb-ledger-convive-data
     :total total
     :postings postings)))

(defun brb-ledger-postings-read (prefix)
  "Read postings for PREFIX."
  (let* ((cmd-register (format "hledger -f '%s' register -O csv -H '%s'"
                               (brb-ledger-file--read) prefix))
         (res-register (shell-command-to-string cmd-register)))
    (->> (split-string res-register "\n" t)
         (cdr)
         (--map (let* ((parts (split-string-and-unquote it ","))
                       (account (string-remove-prefix prefix (nth 4 parts)))
                       (account (or (vulpea-db-get-by-id account)
                                    account))
                       (description (->> (nth 3 parts)
                                         (s-chop-prefix "[")
                                         (s-chop-suffix "]")))
                       (description (or (when (string-match org-uuid-regexp description)
                                          (vulpea-db-get-by-id description))
                                        description)))
                  (make-brb-ledger-posting
                   :date (nth 1 parts)
                   :description description
                   :account account
                   :amount (string-to-number (nth 5 parts))
                   :total (string-to-number (nth 6 parts))))))))

;;; * Ledger UI
;;
;; A very simple and naïve ledger UI.
;;

;;;###autoload
(defun brb-ledger-display ()
  "Display Barberry Garden ledger."
  (interactive)
  (let* ((data (brb-ledger-data-read))
         (data-personal (brb-ledger-personal-data-read))
         (brb-total (brb-ledger-data-total data))
         (brb-uncleared (--reduce-from
                         (+ acc
                            (or (assoc-default (vulpea-note-id it) (brb-ledger-data-balances data)) 0))
                         0
                         (brb-ledger-data-convives data)))
         (brb-cleared (- brb-total brb-uncleared))
         (personal-total (brb-ledger-personal-data-total data-personal))
         (personal-cleared (+ (brb-ledger-data-total data) (brb-ledger-personal-data-total data-personal))))
    (widget-buffer-setup brb-ledger-buffer-name
      (widget-create 'title "Barberry Garden Ledger")
      (widget-insert "\n")

      (widget-create 'heading-1 "Balances")
      (apply
       (-partial
        #'widget-create
        'table
        :padding-type '((1 . left) (2 . left))
        :row-start "- "
        :row-conj "  "
        :row-end ""
        `(row
          (label :value "Barberry Garden")
          (balance-label :value ,brb-total)
          (balance-label :value ,brb-cleared))
        `(row
          (label :value "Personal")
          (balance-label :value ,personal-total)
          (balance-label :value ,personal-cleared)))
       (->> (brb-ledger-data-convives data)
            (--sort
             (< (or (assoc-default (vulpea-note-id it) (brb-ledger-data-balances data)) 0)
                (or (assoc-default (vulpea-note-id other) (brb-ledger-data-balances data)) 0)))
            (--remove (= 0 (or (assoc-default (vulpea-note-id it) (brb-ledger-data-balances data)) 0)))
            (--map
             (list 'row
                   (list 'push-button
                         :value (vulpea-note-title it)
                         :notify (lambda (&rest _)
                                   (funcall-interactively #'brb-ledger-convive-display-balance it)))
                   `(balance-reversed-label
                     :value ,(or (assoc-default (vulpea-note-id it) (brb-ledger-data-balances data)) 0))))))
      (widget-insert "\n")

      (widget-create 'heading-1 "Latest Transactions / Barberry Garden")
      (apply
       (-partial
        #'widget-create
        'table
        :padding-type '((2 . left) (4 . left))
        :trunace '((1 . 70))
        :row-start ""
        :row-conj "  "
        :row-end "")
       (->> (brb-ledger-data-postings data)
            (--remove (and (not (vulpea-note-p (brb-ledger-posting-description it)))
                           (s-prefix-p "charge" (brb-ledger-posting-description it))))
            (seq-reverse)
            (-take 36)
            (--map
             (list
              'row
              `(label :face shadow :value ,(brb-ledger-posting-date it))
              (brb-ledger--posting-desc-widget it)
              `(balance-label :value ,(brb-ledger-posting-amount it))
              '(label :face shadow :value "->")
              `(balance-label :value ,(brb-ledger-posting-total it))))))
      (widget-insert "\n")

      (widget-create 'heading-1 "Latest Transactions / Personal")
      (apply
       (-partial
        #'widget-create
        'table
        :padding-type '((2 . left) (4 . left))
        :trunace '((1 . 70))
        :row-start ""
        :row-conj "  "
        :row-end "")
       (->> (brb-ledger-personal-data-postings data-personal)
            (seq-reverse)
            (-take 36)
            (--map
             (list
              'row
              `(label :face shadow :value ,(brb-ledger-posting-date it))
              (brb-ledger--posting-desc-widget it)
              `(balance-label :value ,(brb-ledger-posting-amount it))
              '(label :face shadow :value "->")
              `(balance-label :value ,(brb-ledger-posting-total it)))))))))

(defun brb-ledger--posting-desc-widget (posting)
  "Return a widget for POSTING."
  (cond
   ((vulpea-note-p (brb-ledger-posting-account posting))
    (list 'push-button
          :value (vulpea-note-title (brb-ledger-posting-account posting))
          :notify (lambda (&rest _)
                    (funcall-interactively #'brb-ledger-convive-display-balance
                                           (brb-ledger-posting-account posting)))))

   ((vulpea-note-p (brb-ledger-posting-description posting))
    (list 'push-button
          :value (vulpea-note-title (brb-ledger-posting-description posting))
          :notify (lambda (&rest _)
                    (funcall-interactively #'vulpea-visit (brb-ledger-posting-description posting)))))

   (t (list 'label
            :value (s-chop-prefixes '("spend: " "charge: ") (brb-ledger-posting-description posting))))))

;;; ** Utils

(defun brb-ledget-buffer-convive-at-point ()
  "Return convive name at point.

This function is a bit wacky and needs to be replaced with something
more robust."
  (when (string= (buffer-name) brb-ledger-buffer-name)
    (->> (buffer-substring-no-properties (line-beginning-position) (line-end-position))
         (s-chop-prefix "- ")
         (s-split "  ")
         (-filter #'identity)
         (--find (and (not (s-matches-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" it))
                      (not (s-suffix-p brb-currency it))))
         (s-chop-prefix "[")
         (s-chop-suffix "]"))))

;; * Convive view
;;
;; Balance of specific convive
;;

;;;###autoload
(defun brb-ledger-convive-display-balance (&optional convive point)
  "Display balance of CONVIVE.

When POINT is non-nil, jump to it."
  (interactive)
  (let* ((convive (or convive (vulpea-select-from
                              "People"
                              (vulpea-db-query-by-tags-some '("people"))
                              :require-match t)))
         (data (brb-ledger-convive-data-read convive)))
    (widget-buffer-setup (concat "*" (vulpea-note-title convive) " Ledger*")
      (widget-create 'title (concat (vulpea-note-title convive) " - Ledger"))
      (widget-create 'balance-label :value (brb-ledger-convive-data-total data) :tag "Balance:")
      (widget-insert "\n\n")

      (widget-create 'heading-1 "Records")
      (apply
       #'widget-create
       (-concat
        (list 'table
              :padding-type '((2 . left)
                              (3 . left))
              :truncate '((1 . 70))
              '(hline)
              '(row :padding-type 'right
                (label :value "date")
                (label :value "description")
                (label :value "amount")
                (label :value "balance"))
              '(hline))
        (->> (brb-ledger-convive-data-postings data)
             (reverse)
             (--map
              `(row
                (label :value ,(brb-ledger-posting-date it))
                (label :value ,(s-chop-prefixes
                                '("deposit: " "deposit" "charge: " "charge")
                                (brb-ledger-posting-description it)))
                (balance-label :value ,(brb-ledger-posting-amount it))
                (balance-label :value ,(brb-ledger-posting-total it)))
              ))
        '((hline)))))
    (when point
      (goto-char point))))

(provide 'brb-ledger)
;;; brb-ledger.el ends here
