;;; brb-event-plan.el --- Event planning UI with vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Boris Buliga

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
;; Declarative UI for planning and managing wine tasting events.
;; Built with vui.el - a React-like component library for Emacs.
;;
;; Main entry point is `brb-event-plan' which opens an interactive buffer
;; for the selected event with tabs for different aspects:
;; - plan: budget, wines, participants, shared spending
;; - scores: live scoring during event
;; - order: personal food/drink orders
;; - extra: extra wine sales (by-the-glass)
;; - invoices: invoice generation and ledger integration
;;
;; Architecture:
;; - Context-based state management (React-like)
;; - Declarative components with props and state
;; - Automatic re-rendering on state changes
;;

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'vulpea)
(require 'vui)
(require 'brb)
(require 'brb-event)
(require 'brb-ledger)


;;; Contexts

;; Event context - the vulpea-note for the event
(defcontext brb-plan-event nil
  "The event vulpea-note being planned.")

;; Data context - event data from .data.el file
(defcontext brb-plan-data nil
  "Event data alist from .data.el file.")

;; Host context - the host vulpea-note
(defcontext brb-plan-host nil
  "The event host vulpea-note.")

;; Participants context - list of participant vulpea-notes
(defcontext brb-plan-participants nil
  "List of participant vulpea-notes.")

;; Waiting list context
(defcontext brb-plan-waiting nil
  "List of vulpea-notes on the waiting list.")

;; Wines context - list of wine vulpea-notes
(defcontext brb-plan-wines nil
  "List of wine vulpea-notes for the event.")

;; Balances context - hash-table of participant-id -> balance
(defcontext brb-plan-balances nil
  "Hash-table mapping participant ID to their balance.")

;; Actions context - plist of action functions for state updates
(defcontext brb-plan-actions nil
  "Plist of action functions for updating state and persisting changes.")

;;; Helper functions

(defun brb-plan--wine-data (data wine-id)
  "Get wine data for WINE-ID from DATA."
  (--find (string-equal wine-id (alist-get 'id it))
          (alist-get 'wines data)))

(defun brb-plan--participant-data (data pid)
  "Get participant data for PID from DATA."
  (--find (string-equal pid (alist-get 'id it))
          (alist-get 'participants data)))

(defun brb-plan--glass-price (wine-data)
  "Calculate glass price for extra wine from WINE-DATA."
  (let ((asking (or (alist-get 'price-asking wine-data)
                    (alist-get 'price-public wine-data)
                    0))
        (participants (alist-get 'participants wine-data)))
    (if (and participants (> (length participants) 0))
        (ceiling (/ (float asking) (length participants)))
      0)))


;;; Root Component

(defcomponent brb-event-plan-app (event)
  :state ((event event)
          (tab "plan")
          (data nil)
          (host nil)
          (participants nil)
          (waiting nil)
          (wines nil)
          (balances nil))

  :on-mount
  ;; Initialize state from event
  (let* ((event-data (brb-event-data-read event))
         (event-host (vulpea-note-meta-get event "host" 'note))
         (event-participants (brb-event-participants event))
         (event-waiting (vulpea-note-meta-get-list event "waiting" 'note))
         (event-wines (brb-event-wines event))
         (event-date (brb-event-date-string event))
         (event-balances (let ((tbl (make-hash-table :test 'equal)))
                           (--each event-participants
                             (puthash (vulpea-note-id it)
                                      (brb-ledger-balance-of (vulpea-note-id it) event-date)
                                      tbl))
                           tbl)))
    (vui-batch
     (vui-set-state :data event-data)
     (vui-set-state :host event-host)
     (vui-set-state :participants event-participants)
     (vui-set-state :waiting event-waiting)
     (vui-set-state :wines event-wines)
     (vui-set-state :balances event-balances)))

  :render
  (let* (;; Create action functions that update state and persist
         (actions
          (list
           ;; Update data and save to file
           :update-data
           (lambda (key value)
             (let ((new-data (copy-alist data)))
               (setf (alist-get key new-data) value)
               (brb-event-data-write event new-data)
               (vui-set-state :data new-data)))

           ;; Update nested wine data
           :update-wine-data
           (lambda (wine-id key value)
             (let* ((new-data (copy-alist data))
                    (wines-data (alist-get 'wines new-data))
                    (wine-data (--find (string-equal wine-id (alist-get 'id it)) wines-data)))
               (if wine-data
                   (setf (alist-get key wine-data) value)
                 ;; Create new entry
                 (push `((id . ,wine-id) (,key . ,value)) wines-data)
                 (setf (alist-get 'wines new-data) wines-data))
               (brb-event-data-write event new-data)
               (vui-set-state :data new-data)))

           ;; Add pays-for relationship (payer-id pays for payee-id)
           ;; Stored in participants: ((id . "payer-id") (pays-for . ("payee-id1" "payee-id2")))
           :add-pays-for
           (lambda (payer-id payee-id)
             (let* ((new-data (copy-alist data))
                    (participants-data (or (alist-get 'participants new-data) nil))
                    (existing (--find (string-equal payer-id (alist-get 'id it)) participants-data)))
               (if existing
                   ;; Add to existing payer's pays-for list
                   (unless (-contains-p (alist-get 'pays-for existing) payee-id)
                     (setf (alist-get 'pays-for existing)
                           (-uniq (append (alist-get 'pays-for existing) (list payee-id)))))
                 ;; Create new participant entry with pays-for
                 (push `((id . ,payer-id) (pays-for . (,payee-id))) participants-data)
                 (setf (alist-get 'participants new-data) participants-data))
               (brb-event-data-write event new-data)
               (vui-set-state :data new-data)))

           ;; Remove pays-for relationship
           :remove-pays-for
           (lambda (payer-id payee-id)
             (let* ((new-data (copy-alist data))
                    (participants-data (alist-get 'participants new-data))
                    (existing (--find (string-equal payer-id (alist-get 'id it)) participants-data)))
               (when existing
                 (setf (alist-get 'pays-for existing)
                       (--remove (string-equal payee-id it) (alist-get 'pays-for existing))))
               (brb-event-data-write event new-data)
               (vui-set-state :data new-data)))

           ;; Set event metadata
           :set-event-meta
           (lambda (key value)
             (vulpea-utils-with-note event
               (vulpea-buffer-meta-set key value 'append)
               (save-buffer))
             (vulpea-db-update-file (vulpea-note-path event))
             (vui-set-state :event (vulpea-db-get-by-id (vulpea-note-id event))))

           ;; Set host
           :set-host
           (lambda (new-host)
             (vui-set-state :host new-host)
             (vulpea-utils-with-note event
               (vulpea-buffer-meta-set "host" new-host 'append)
               (save-buffer))
             (vulpea-db-update-file (vulpea-note-path event))
             (vui-set-state :event (vulpea-db-get-by-id (vulpea-note-id event))))

           ;; Add wine
           :add-wine
           (lambda (wine price-public price-real)
             (let* ((new-wines (append wines (list wine)))
                    (new-data (copy-alist data))
                    (wines-data (alist-get 'wines new-data))
                    (entry `((id . ,(vulpea-note-id wine))
                             (type . "normal")
                             (price-public . ,price-public)
                             (price-real . ,price-real))))
               (push entry wines-data)
               (setf (alist-get 'wines new-data) wines-data)
               (vui-batch
                (vui-set-state :wines new-wines)
                (vui-set-state :data new-data))
               (vulpea-utils-with-note event
                 (vulpea-buffer-meta-set "wines" new-wines 'append)
                 (save-buffer))
               (brb-event-data-write event new-data)))

           ;; Remove wine
           :remove-wine
           (lambda (wine-id)
             (let* ((new-wines (--remove (string-equal wine-id (vulpea-note-id it)) wines))
                    (new-data (copy-alist data)))
               (setf (alist-get 'wines new-data)
                     (--remove (string-equal wine-id (alist-get 'id it))
                               (alist-get 'wines new-data)))
               (vui-batch
                (vui-set-state :wines new-wines)
                (vui-set-state :data new-data))
               (vulpea-utils-with-note event
                 (vulpea-buffer-meta-set "wines" new-wines 'append)
                 (save-buffer))
               (brb-event-data-write event new-data)))

           ;; Add participant
           :add-participant
           (lambda (person)
             (let ((new-participants (append participants (list person))))
               (vui-set-state :participants new-participants)
               (vulpea-utils-with-note event
                 (vulpea-buffer-meta-set "participants" new-participants 'append)
                 (save-buffer))))

           ;; Remove participant
           :remove-participant
           (lambda (pid)
             (let* ((new-participants (--remove (string-equal pid (vulpea-note-id it)) participants))
                    (new-data (copy-alist data)))
               (setf (alist-get 'participants new-data)
                     (--remove (string-equal pid (alist-get 'id it))
                               (alist-get 'participants new-data)))
               (vui-batch
                (vui-set-state :participants new-participants)
                (vui-set-state :data new-data))
               (vulpea-utils-with-note event
                 (vulpea-buffer-meta-set "participants" new-participants 'append)
                 (save-buffer))
               (brb-event-data-write event new-data)))

           ;; Add to waiting list
           :add-waiting
           (lambda (person)
             (let ((new-waiting (append waiting (list person))))
               (vui-set-state :waiting new-waiting)
               (vulpea-utils-with-note event
                 (vulpea-buffer-meta-set "waiting" new-waiting 'append)
                 (save-buffer))))

           ;; Remove from waiting list
           :remove-waiting
           (lambda (pid)
             (let ((new-waiting (--remove (string-equal pid (vulpea-note-id it)) waiting)))
               (vui-set-state :waiting new-waiting)
               (vulpea-utils-with-note event
                 (vulpea-buffer-meta-set "waiting" new-waiting 'append)
                 (save-buffer))))

           ;; Promote from waiting to participant
           :promote-waiting
           (lambda (pid)
             (let* ((person (--find (string-equal pid (vulpea-note-id it)) waiting))
                    (new-waiting (--remove (string-equal pid (vulpea-note-id it)) waiting))
                    (new-participants (append participants (list person))))
               (vui-batch
                (vui-set-state :waiting new-waiting)
                (vui-set-state :participants new-participants))
               (vulpea-utils-with-note event
                 (vulpea-buffer-meta-set "waiting" new-waiting 'append)
                 (vulpea-buffer-meta-set "participants" new-participants 'append)
                 (save-buffer))))

           ;; Refresh balances from ledger (as of event date)
           :refresh-balances
           (lambda ()
             (let* ((date (brb-event-date-string event))
                    (new-balances (make-hash-table :test 'equal)))
               (--each participants
                 (puthash (vulpea-note-id it)
                          (brb-ledger-balance-of (vulpea-note-id it) date)
                          new-balances))
               (vui-set-state :balances new-balances)))

           ;; Set tab
           :set-tab
           (lambda (new-tab)
             (vui-set-state :tab new-tab)))))

    ;; Provide all contexts and render
    (brb-plan-event-provider event
      (brb-plan-data-provider data
        (brb-plan-host-provider host
          (brb-plan-participants-provider participants
            (brb-plan-waiting-provider waiting
              (brb-plan-wines-provider wines
                (brb-plan-balances-provider balances
                  (brb-plan-actions-provider actions
                    (vui-vstack
                     ;; Header with tabs
                     (vui-component 'brb-plan-header :tab tab)
                     ;; Tab content
                     (pcase tab
                       ("plan" (vui-component 'brb-plan-tab-plan))
                       ("scores" (vui-component 'brb-plan-tab-scores))
                       ("order" (vui-component 'brb-plan-tab-order))
                       ("extra" (vui-component 'brb-plan-tab-extra))
                       ("invoices" (vui-component 'brb-plan-tab-invoices))
                       (_ (vui-text (format "Unknown tab: %s" tab)))))))))))))))

;;; Header Component

(defcomponent brb-plan-header (tab)
  :render
  (let ((event (use-brb-plan-event))
        (actions (use-brb-plan-actions)))
    (vui-vstack
     ;; Event title
     (vui-text (vulpea-note-title event) :face 'org-level-1)
     (vui-newline)
     ;; Tab buttons
     (vui-hstack
      :spacing 1
      (vui-button (if (string= tab "plan") "*plan*" "plan")
        :face (when (string= tab "plan") 'bold)
        :on-click (lambda () (funcall (plist-get actions :set-tab) "plan")))
      (vui-button (if (string= tab "scores") "*scores*" "scores")
        :face (when (string= tab "scores") 'bold)
        :on-click (lambda () (funcall (plist-get actions :set-tab) "scores")))
      (vui-button (if (string= tab "order") "*order*" "order")
        :face (when (string= tab "order") 'bold)
        :on-click (lambda () (funcall (plist-get actions :set-tab) "order")))
      (vui-button (if (string= tab "extra") "*extra*" "extra")
        :face (when (string= tab "extra") 'bold)
        :on-click (lambda () (funcall (plist-get actions :set-tab) "extra")))
      (vui-button (if (string= tab "invoices") "*invoices*" "invoices")
        :face (when (string= tab "invoices") 'bold)
        :on-click (lambda () (funcall (plist-get actions :set-tab) "invoices"))))
     (vui-newline)
     ;; Action buttons
     (vui-hstack :spacing 1
                 (vui-button "Open note"
                   :on-click (lambda () (vulpea-visit event)))
                 (vui-button "Refresh"
                   :on-click (lambda ()
                               ;; TODO: reload from disk
                               (message "Refresh not yet implemented"))))
     (vui-newline))))


;;; Plan Tab Components

(defcomponent brb-plan-tab-plan ()
  :render
  (vui-vstack
   (vui-component 'brb-plan-general)
   (vui-component 'brb-plan-forecast)
   (vui-component 'brb-plan-finances)
   (vui-component 'brb-plan-wines-table)
   (vui-component 'brb-plan-shared-table)
   (vui-component 'brb-plan-expense-wines-table)
   (vui-component 'brb-plan-participants-table)
   (vui-component 'brb-plan-waiting-list)))

;;; General Section

(defcomponent brb-plan-general ()
  :render
  (let ((event (use-brb-plan-event))
        (host (use-brb-plan-host))
        (participants (use-brb-plan-participants))
        (actions (use-brb-plan-actions)))
    (vui-vstack
     (vui-text "General" :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '(()
                 (:width 5 :grow t) ; just for extra spacing
                 (:align :left))
      :rows (list
             (list
              (vui-text "Host: ")
              ""
              (vui-button
                  (if host (vulpea-note-title host) "_____")
                :on-click (lambda ()
                            (let* ((candidates (or participants
                                                   (vulpea-db-query-by-tags-every '("people"))))
                                   (new-host (vulpea-select-from "Host" candidates :require-match t)))
                              (when new-host
                                (funcall (plist-get actions :set-host) new-host))))))
             (list
              (vui-text "Price: ")
              ""
              (vui-button
                  (brb-price-format (or (vulpea-note-meta-get event "price" 'number) 0))
                :on-click (lambda ()
                            (let ((price (read-number "Price: "
                                                      (or (vulpea-note-meta-get event "price" 'number) 0))))
                              (funcall (plist-get actions :set-event-meta) "price" price)))))))
     (vui-newline))))

;;; Forecast Section

(defcomponent brb-plan-forecast ()
  :render
  (let* ((event (use-brb-plan-event))
         (data (use-brb-plan-data))
         (actions (use-brb-plan-actions))
         (price (or (vulpea-note-meta-get event "price" 'number) 0))
         (planned (or (alist-get 'planned-participants data) 0))
         (debit (* planned price)))
    (vui-vstack
     (vui-text "Forecast" :face 'org-level-2)
     (vui-newline)
     (vui-hstack
      (vui-text "Participants: ")
      (vui-button (if (> planned 0) (number-to-string planned) "__")
        :on-click (lambda ()
                    (let ((num (read-number "Planned participants: " planned)))
                      (funcall (plist-get actions :update-data)
                               'planned-participants num)))))
     (vui-hstack
      (vui-text "Debit: ")
      (vui-text (brb-price-format debit)))
     (vui-newline))))


;;; Finances Section

(defcomponent brb-plan-finances ()
  :render
  (let* ((event (use-brb-plan-event))
         (price (vulpea-note-meta-get event "price" 'number))
         (data (use-brb-plan-data))
         (participants (use-brb-plan-participants))
         (wines (use-brb-plan-wines))
         (host (use-brb-plan-host))
         (balances (or (use-brb-plan-balances) (make-hash-table :test 'equal)))
         (statement (brb-event-statement event
                      :data data
                      :participants participants
                      :wines wines
                      :host host
                      :balances balances)))
    (vui-vstack
     (vui-text "Finances" :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "" :width 20 :grow t)
                 (:header "Public" :width 10 :grow t :align :right)
                 (:header "Real" :width 10 :grow t :align :right))
      :rows `(("Spending (shared)"
               ,(brb-price-format (alist-get 'spending-shared statement))
               "")
              ("Spending (wines)"
               ,(brb-price-format (alist-get 'spending-wines-public statement))
               ,(brb-price-format (alist-get 'spending-wines-real statement)))
              ("Spending (extra)"
               ,(brb-price-format (alist-get 'spending-extra-public statement))
               ,(brb-price-format (alist-get 'spending-extra-real statement)))
              ("Spending (total)"
               ,(brb-price-format (alist-get 'credit-public statement))
               ,(brb-price-format (alist-get 'credit-real statement)))
              :separator
              ("Debit (base)"
               ,(brb-price-format (alist-get 'debit-base statement))
               "")
              ("Debit (extra)"
               ,(brb-price-format (alist-get 'debit-extra statement))
               "")
              ("Debit (total)"
               ,(brb-price-format (alist-get 'debit statement))
               "")
              :separator
              (,(vui-text "Gain" :face (if (>= (alist-get 'balance-public statement) 0) 'success 'error))
               ,(vui-text (brb-price-format (alist-get 'balance-public statement))
                 :face (if (>= (alist-get 'balance-public statement) 0) 'success 'error))
               ,(vui-text (brb-price-format (alist-get 'balance-real statement))
                 :face (if (>= (alist-get 'balance-real statement) 0) 'success 'error))))
      :border :ascii)
     (vui-newline))))


;;; Wines Table

(defcomponent brb-plan-wines-table ()
  :render
  (let* ((data (use-brb-plan-data))
         (wines (use-brb-plan-wines))
         (actions (use-brb-plan-actions)))
    (vui-vstack
     (vui-text "Wines" :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "" :width 3 :grow t)
                 (:header "Producer" :width 20 :grow t :truncate t)
                 (:header "Wine" :width 36 :grow t :truncate t)
                 (:header "Year" :width 4 :grow t)
                 (:header "P.Pub" :width 10 :grow t :align :right)
                 (:header "P.Real" :width 10 :grow t :align :right)
                 (:header "Type" :width 8 :grow t))
      :rows (append
             ;; Wine rows
             (--map
              (let* ((wine it)
                     (wine-id (vulpea-note-id wine))
                     (wine-data (brb-plan--wine-data data wine-id))
                     (producer (vulpea-note-meta-get wine "producer" 'note))
                     (producer-name (if producer (vulpea-note-title producer) ""))
                     (name (or (vulpea-note-meta-get wine "name") ""))
                     (vintage (or (vulpea-note-meta-get wine "vintage") "NV"))
                     (price-public (or (alist-get 'price-public wine-data) 0))
                     (price-real (or (alist-get 'price-real wine-data) 0))
                     (wine-type (or (alist-get 'type wine-data) "normal"))
                     (price-real-min (-min (or (->> (vino-inv-query-available-bottles-for wine)
                                                    (-map #'vino-inv-bottle-price)
                                                    (--filter (s-suffix-p brb-currency it))
                                                    (-map #'string-to-number))
                                               (list price-real)))))
                (list
                 ;; Remove button
                 (vui-button "x"
                   :on-click (lambda ()
                               (funcall (plist-get actions :remove-wine) wine-id)))
                 ;; Producer
                 producer-name
                 ;; Wine name (clickable)
                 (vui-button name
                   :on-click (lambda () (vulpea-visit wine)))
                 ;; Vintage
                 (if (numberp vintage) (number-to-string vintage) vintage)
                 ;; Public price (editable)
                 (vui-button (brb-price-format price-public)
                   :on-click (lambda ()
                               (let ((price (read-number "Price public: " (alist-get 'amount (brb-price wine)))))
                                 (funcall (plist-get actions :update-wine-data)
                                          wine-id 'price-public price))))
                 ;; Real price (editable)
                 (vui-button (brb-price-format price-real)
                   :on-click (lambda ()
                               (let ((price (read-number "Price real: " price-real-min)))
                                 (funcall (plist-get actions :update-wine-data)
                                          wine-id 'price-real price))))
                 ;; Type (editable)
                 (vui-button wine-type
                   :on-click (lambda ()
                               (let ((type (completing-read "Type: "
                                                            '("normal" "welcome" "bonus" "extra")
                                                            nil t)))
                                 (funcall (plist-get actions :update-wine-data)
                                          wine-id 'type type))))))
              wines)
             ;; Add button row
             (list
              :separator
              (list
               (vui-button "+"
                 :on-click (lambda ()
                             (let* ((wine (vulpea-select-from
                                           "Wine"
                                           (--remove
                                            (-contains-p (-map #'vulpea-note-id wines)
                                                         (vulpea-note-id it))
                                            (vulpea-db-query-by-tags-every '("wine" "cellar")))
                                           :require-match t)))
                               (when wine
                                 (let ((price-pub (read-number "Price public: " 0))
                                       (price-real (read-number "Price real: " 0)))
                                   (funcall (plist-get actions :add-wine)
                                            wine price-pub price-real))))))
               "" "" "" "" "" "")))
      :border :ascii)
     (vui-newline))))


;;; Shared Spending Table

(defcomponent brb-plan-shared-table ()
  :render
  (let* ((data (use-brb-plan-data))
         (actions (use-brb-plan-actions))
         (shared (alist-get 'shared data)))
    (vui-vstack
     (vui-text "Shared Spending" :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "" :width 3 :grow t)
                 (:header "Item" :width 20 :grow t)
                 (:header "Price" :width 10 :grow t :align :right)
                 (:header "Amount" :width 6 :grow t :align :right)
                 (:header "Total" :width 10 :grow t :align :right))
      :rows (append
             ;; Item rows
             (--map-indexed
              (let* ((item it)
                     (idx it-index)
                     (item-name (or (alist-get 'item item) ""))
                     (price (or (alist-get 'price item) 0))
                     (amount (or (alist-get 'amount item) 1))
                     (total (ceiling (* amount price))))
                (list
                 ;; Remove button
                 (vui-button "x"
                   :on-click (lambda ()
                               (let* ((new-shared (-remove-at idx shared)))
                                 (funcall (plist-get actions :update-data)
                                          'shared new-shared))))
                 ;; Item name
                 item-name
                 ;; Price (editable)
                 (vui-button (brb-price-format price)
                   :on-click (lambda ()
                               (let* ((new-price (read-number "Price: " price))
                                      (new-item (copy-alist item))
                                      (new-shared (copy-sequence shared)))
                                 (setf (alist-get 'price new-item) new-price)
                                 (setf (nth idx new-shared) new-item)
                                 (funcall (plist-get actions :update-data)
                                          'shared new-shared))))
                 ;; Amount (editable)
                 (vui-button (number-to-string amount)
                   :on-click (lambda ()
                               (let* ((new-amount (read-number "Amount: " amount))
                                      (new-item (copy-alist item))
                                      (new-shared (copy-sequence shared)))
                                 (setf (alist-get 'amount new-item) new-amount)
                                 (setf (nth idx new-shared) new-item)
                                 (funcall (plist-get actions :update-data)
                                          'shared new-shared))))
                 ;; Total
                 (brb-price-format total)))
              shared)
             ;; Add button row
             (list
              :separator
              (list
               (vui-button "+"
                 :on-click (lambda ()
                             (let* ((item-name (read-string "Item: "))
                                    (new-entry `((item . ,item-name) (amount . 1) (price . 0)))
                                    (new-shared (append shared (list new-entry))))
                               (funcall (plist-get actions :update-data)
                                        'shared new-shared))))
               "" "" "" "")))
      :border :ascii)
     (vui-newline))))


;;; Expense Wines Table

(defcomponent brb-plan-expense-wines-table ()
  :render
  (let* ((data (use-brb-plan-data))
         (actions (use-brb-plan-actions))
         (expense (alist-get 'expense-wines data)))
    (vui-vstack
     (vui-text "Expense Wines" :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "" :width 3 :grow t)
                 (:header "Item" :width 20 :grow t)
                 (:header "Price" :width 10 :grow t :align :right)
                 (:header "Amount" :width 6 :grow t :align :right)
                 (:header "Total" :width 10 :grow t :align :right))
      :rows (append
             ;; Item rows
             (--map-indexed
              (let* ((item it)
                     (idx it-index)
                     (item-name (or (alist-get 'item item) ""))
                     (price (or (alist-get 'price item) 0))
                     (amount (or (alist-get 'amount item) 1))
                     (total (ceiling (* amount price))))
                (list
                 ;; Remove button
                 (vui-button "x"
                   :on-click (lambda ()
                               (let* ((new-expense (-remove-at idx expense)))
                                 (funcall (plist-get actions :update-data)
                                          'expense-wines new-expense))))
                 ;; Item name
                 item-name
                 ;; Price (editable)
                 (vui-button (brb-price-format price)
                   :on-click (lambda ()
                               (let* ((new-price (read-number "Price: " price))
                                      (new-item (copy-alist item))
                                      (new-expense (copy-sequence expense)))
                                 (setf (alist-get 'price new-item) new-price)
                                 (setf (nth idx new-expense) new-item)
                                 (funcall (plist-get actions :update-data)
                                          'expense-wines new-expense))))
                 ;; Amount (editable)
                 (vui-button (number-to-string amount)
                   :on-click (lambda ()
                               (let* ((new-amount (read-number "Amount: " amount))
                                      (new-item (copy-alist item))
                                      (new-expense (copy-sequence expense)))
                                 (setf (alist-get 'amount new-item) new-amount)
                                 (setf (nth idx new-expense) new-item)
                                 (funcall (plist-get actions :update-data)
                                          'expense-wines new-expense))))
                 ;; Total
                 (brb-price-format total)))
              expense)
             ;; Add button row
             (list
              :separator
              (list
               (vui-button "+"
                 :on-click (lambda ()
                             (let* ((item-name (read-string "Item: "))
                                    (new-entry `((item . ,item-name) (amount . 1) (price . 0)))
                                    (new-expense (append expense (list new-entry))))
                               (funcall (plist-get actions :update-data)
                                        'expense-wines new-expense))))
               "" "" "" "")))
      :border :ascii)
     (vui-newline))))


;;; Participants Table

(defcomponent brb-plan-participants-table ()
  :render
  (let* ((event (use-brb-plan-event))
         (data (use-brb-plan-data))
         (participants (use-brb-plan-participants))
         (host (use-brb-plan-host))
         (actions (use-brb-plan-actions))
         (price (or (vulpea-note-meta-get event "price" 'number) 0))
         (host-id (when host (vulpea-note-id host))))
    (vui-vstack
     (vui-text (format "Participants (%d)" (length participants)) :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "" :width 3 :grow t)
                 (:header "Participant" :width 20 :grow t)
                 (:header "Mode" :width 8 :grow t)
                 (:header "Fee" :width 10 :grow t :align :right))
      :rows (append
             ;; Participant rows
             (--map
              (let* ((person it)
                     (pid (vulpea-note-id person))
                     (name (vulpea-note-title person))
                     (p-data (brb-plan--participant-data data pid))
                     (host-p (and host-id (string-equal pid host-id)))
                     (custom-fee (alist-get 'fee p-data))
                     (fee (cond (host-p 0)
                                (custom-fee custom-fee)
                                (t price)))
                     (mode (cond (host-p "host")
                                 (custom-fee "custom")
                                 (t "normal"))))
                (list
                 ;; Remove button
                 (vui-button "x"
                   :on-click (lambda ()
                               (funcall (plist-get actions :remove-participant) pid)))
                 ;; Name
                 name
                 ;; Mode
                 mode
                 ;; Fee (editable unless host)
                 (if host-p
                     "0"
                   (vui-button (brb-price-format fee)
                     :on-click (lambda ()
                                 (let* ((new-fee (read-number "Fee: " fee))
                                        (new-data (copy-alist data))
                                        (p-list (alist-get 'participants new-data))
                                        (p-entry (--find (string-equal pid (alist-get 'id it)) p-list)))
                                   (if p-entry
                                       (setf (alist-get 'fee p-entry) (unless (= new-fee price) new-fee))
                                     (push `((id . ,pid) (fee . ,(unless (= new-fee price) new-fee))) p-list)
                                     (setf (alist-get 'participants new-data) p-list))
                                   (funcall (plist-get actions :update-data)
                                            'participants (alist-get 'participants new-data))))))))
              participants)
             ;; Add button row
             (list
              :separator
              (list
               (vui-button "+"
                 :on-click (lambda ()
                             (let ((person (vulpea-select-from
                                            "Participant"
                                            (--remove
                                             (-contains-p (-map #'vulpea-note-id participants)
                                                          (vulpea-note-id it))
                                             (vulpea-db-query-by-tags-every '("people")))
                                            :require-match t)))
                               (when person
                                 (funcall (plist-get actions :add-participant) person)))))
               "" "" "")))
      :border :ascii)
     (vui-newline))))


;;; Waiting List

(defcomponent brb-plan-waiting-list ()
  :render
  (let ((waiting (use-brb-plan-waiting))
        (actions (use-brb-plan-actions)))
    (vui-vstack
     (vui-text (format "Waiting List (%d)" (length waiting)) :face 'org-level-2)
     (vui-newline)
     (if (null waiting)
         (vui-text "(empty)")
       (vui-list waiting
                 (lambda (person)
                   (let ((pid (vulpea-note-id person))
                         (name (vulpea-note-title person)))
                     (vui-hstack
                      (vui-button "x"
                        :on-click (lambda ()
                                    (funcall (plist-get actions :remove-waiting) pid)))
                      (vui-button "↑"
                        :on-click (lambda ()
                                    (funcall (plist-get actions :promote-waiting) pid)))
                      (vui-text name))))
                 #'vulpea-note-id))
     (vui-hstack
      (vui-button "+"
        :on-click (lambda ()
                    (let ((person (vulpea-select-from
                                   "Person"
                                   (vulpea-db-query-by-tags-every '("people"))
                                   :require-match t)))
                      (when person
                        (funcall (plist-get actions :add-waiting) person))))))
     (vui-newline))))


;;; Scores Tab Components

(defcomponent brb-plan-tab-scores ()
  :render
  (vui-vstack
   (vui-component 'brb-plan-scores-summary)
   (vui-component 'brb-plan-wines-ranking-table)
   (vui-component 'brb-plan-scores-matrix)))


;;; Scores Summary

(defcomponent brb-plan-scores-summary ()
  :render
  (let* ((event (use-brb-plan-event))
         (data (use-brb-plan-data))
         (wines (use-brb-plan-wines))
         (participants (use-brb-plan-participants))
         (summary (brb-event-summary event :data data :wines wines :participants participants))
         (event-rms (alist-get 'rms summary))
         (event-wavg (alist-get 'wavg summary))
         (event-qpr (alist-get 'qpr summary))
         (price-harmonic (alist-get 'wines-price-harmonic summary))
         (price-median (alist-get 'wines-price-median summary)))
    (vui-vstack
     (vui-text "Summary" :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:width 16 :grow t)
                 (:width 10 :grow t :align :right))
      :rows `(("RMS:" ,(if event-rms (format "%.4f" event-rms) "—"))
              ("WAVG:" ,(if event-wavg (format "%.4f" event-wavg) "—"))
              ("QPR:" ,(if event-qpr (format "%.4f" event-qpr) "—"))
              ("Price (harmonic):" ,(if price-harmonic (brb-price-format price-harmonic) "—"))
              ("Price (median):" ,(if price-median (brb-price-format price-median) "—"))))
     (vui-newline))))


;;; Wines Ranking Table

(defun brb-plan--compute-places (wines-data)
  "Compute places for WINES-DATA based on WAVG with ties."
  (let* ((places (make-hash-table :test 'equal))
         (wines-sorted (->> wines-data
                            (--filter (alist-get 'wavg it))
                            (--sort (> (alist-get 'wavg it)
                                       (alist-get 'wavg other)))))
         (current-place 1)
         (prev-wavg nil))
    (--each-indexed wines-sorted
      (let* ((wine (alist-get 'wine it))
             (wavg (alist-get 'wavg it)))
        (unless (and prev-wavg (= wavg prev-wavg))
          (setq current-place (1+ it-index)))
        (puthash (vulpea-note-id wine) current-place places)
        (setq prev-wavg wavg)))
    places))

(defcomponent brb-plan-wines-ranking-table ()
  :render
  (let* ((event (use-brb-plan-event))
         (data (use-brb-plan-data))
         (wines (use-brb-plan-wines))
         (participants (use-brb-plan-participants))
         (actions (use-brb-plan-actions))
         (raw-wines-data (alist-get 'wines data))
         (summary (brb-event-summary event :data data :wines wines :participants participants))
         (wines-data (alist-get 'wines summary))
         (wines-display (--remove (alist-get 'ignore-scores it) wines-data))
         (places (brb-plan--compute-places wines-display)))
    (vui-vstack
     (vui-text "Wines Ranking" :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "#" :width 3 :grow t)
                 (:header "##" :width 3 :grow t)
                 (:header "Producer" :width 16 :grow t :truncate t)
                 (:header "Wine" :width 28 :grow t :truncate t)
                 (:header "Year" :width 4 :grow t)
                 (:header "WAVG" :width 6 :grow t :align :right)
                 (:header "Sdev" :width 6 :grow t :align :right)
                 (:header "QPR" :width 6 :grow t :align :right)
                 (:header "Fav" :width 3 :grow t :align :right)
                 (:header "Out" :width 3 :grow t :align :right)
                 (:header "Inc" :width 3 :grow t :align :center)
                 (:header "Rev" :width 3 :grow t :align :center))
      :rows (--map-indexed
             (let* ((wine-data it)
                    (order (1+ it-index))
                    (wine (alist-get 'wine wine-data))
                    (wine-id (vulpea-note-id wine))
                    (raw-wine-data (--find (string-equal wine-id (alist-get 'id it)) raw-wines-data))
                    (ignore-scores (alist-get 'ignore-scores raw-wine-data))
                    (revealed (not (alist-get 'blind raw-wine-data)))
                    (place (gethash wine-id places))
                    (producer (vulpea-note-meta-get wine "producer" 'note))
                    (producer-name (if producer (vulpea-note-title producer) ""))
                    (name (or (vulpea-note-meta-get wine "name") ""))
                    (vintage (or (vulpea-note-meta-get wine "vintage") "NV"))
                    (wavg (alist-get 'wavg wine-data))
                    (sdev (alist-get 'sdev wine-data))
                    (qpr (alist-get 'qpr wine-data))
                    (fav (alist-get 'fav wine-data))
                    (out (alist-get 'out wine-data)))
               (list
                (number-to-string order)
                (if place (number-to-string place) "")
                (if revealed producer-name "****")
                (vui-button (if revealed name "****")
                  :on-click (lambda () (vulpea-visit wine)))
                (if revealed
                    (if (numberp vintage) (number-to-string vintage) vintage)
                  "****")
                (if wavg (format "%.4f" wavg) "—")
                (if sdev (format "%.4f" sdev) "—")
                (if qpr (format "%.4f" qpr) "—")
                (number-to-string (or fav 0))
                (number-to-string (or out 0))
                ;; Include toggle (inverted: checked = included, unchecked = ignored)
                (vui-checkbox :checked (not ignore-scores)
                              :on-change (lambda (v)
                                           (funcall (plist-get actions :update-wine-data)
                                                    wine-id 'ignore-scores (not v))))
                ;; Reveal toggle
                (vui-checkbox :checked revealed
                              :on-change (lambda (v)
                                           (funcall (plist-get actions :update-wine-data)
                                                    wine-id 'blind (not v))))))
             wines-data)
      :border :ascii)
     (vui-newline))))


;;; Personal Scores Matrix

(defcomponent brb-plan-scores-matrix ()
  :render
  (let* ((data (use-brb-plan-data))
         (participants (use-brb-plan-participants))
         (wines (use-brb-plan-wines))
         (actions (use-brb-plan-actions))
         (wines-data (alist-get 'wines data)))
    (vui-vstack
     (vui-text "Personal Scores" :face 'org-level-2)
     (vui-newline)
     (if (or (null participants) (null wines))
         (vui-text "(no participants or wines)")
       (vui-table
        :columns (cons '(:header "" :width 16 :truncate t)
                       (--map-indexed
                        `(:header ,(format "#%d" (1+ it-index)) :width 6)
                        wines))
        :rows (--map
               (let ((person it)
                     (pid (vulpea-note-id it))
                     (name (vulpea-note-title it)))
                 (cons (vui-button name :on-click (lambda () (vulpea-visit it)))
                       (--map
                        (let* ((wine it)
                               (wid (vulpea-note-id wine))
                               (wine-data (--find (string-equal wid (alist-get 'id it)) wines-data))
                               (score-data (when wine-data
                                             (--find (string-equal pid (alist-get 'participant it))
                                                     (alist-get 'scores wine-data))))
                               (score (when score-data (alist-get 'score score-data)))
                               (sentiment (when score-data (alist-get 'sentiment score-data)))
                               (sentiment-char (pcase sentiment
                                                 ("favourite" "♥")
                                                 ("outcast" "✗")
                                                 (_ (if score " " ""))))
                               (display (format "%s%s"
                                                (if score (format "%.1f" score) "—")
                                                sentiment-char)))
                          (vui-button display
                            :on-click (lambda ()
                                        (brb-plan--score-edit
                                         actions data wid pid score sentiment))))
                        wines)))
               participants)
        :border :ascii))
     (vui-newline))))

(defun brb-plan--score-edit (actions data wine-id participant-id current-score current-sentiment)
  "Edit score for WINE-ID and PARTICIPANT-ID.
ACTIONS is the actions plist. DATA is the current data.
CURRENT-SCORE and CURRENT-SENTIMENT are current values."
  (let* ((prompt (format "Score (0-5, f=♥, o=✗, c=clear): "))
         (input (read-string prompt)))
    (cond
     ((string-equal "f" input)
      (brb-plan--sentiment-set actions data wine-id participant-id "favourite"))
     ((string-equal "o" input)
      (brb-plan--sentiment-set actions data wine-id participant-id "outcast"))
     ((string-equal "c" input)
      (brb-plan--sentiment-set actions data wine-id participant-id nil))
     ((string-empty-p input) nil)
     (t
      (let ((score (string-to-number input)))
        (when (and (>= score 0) (<= score 5))
          ;; Score 0 means "clear score" (nil)
          (brb-plan--score-set actions data wine-id participant-id
                               (if (zerop score) nil score))))))))

(defun brb-plan--score-set (actions data wine-id participant-id score)
  "Set SCORE for WINE-ID and PARTICIPANT-ID using ACTIONS with DATA."
  (let* ((wines-data (copy-alist (alist-get 'wines data)))
         (wine-data (--find (string-equal wine-id (alist-get 'id it)) wines-data)))
    (if wine-data
        (let ((scores (alist-get 'scores wine-data))
              (score-data (--find (string-equal participant-id (alist-get 'participant it))
                                  (alist-get 'scores wine-data))))
          (if score-data
              (setf (alist-get 'score score-data) score)
            (push `((participant . ,participant-id) (score . ,score) (sentiment . nil)) scores)
            (setf (alist-get 'scores wine-data) scores)))
      (push `((id . ,wine-id)
              (scores . (((participant . ,participant-id) (score . ,score) (sentiment . nil)))))
            wines-data))
    (funcall (plist-get actions :update-data) 'wines wines-data)))

(defun brb-plan--sentiment-set (actions data wine-id participant-id sentiment)
  "Set SENTIMENT for WINE-ID and PARTICIPANT-ID using ACTIONS with DATA."
  (let* ((wines-data (copy-alist (alist-get 'wines data)))
         (wine-data (--find (string-equal wine-id (alist-get 'id it)) wines-data)))
    (if wine-data
        (let ((scores (alist-get 'scores wine-data))
              (score-data (--find (string-equal participant-id (alist-get 'participant it))
                                  (alist-get 'scores wine-data))))
          (if score-data
              (setf (alist-get 'sentiment score-data) sentiment)
            (push `((participant . ,participant-id) (score . nil) (sentiment . ,sentiment)) scores)
            (setf (alist-get 'scores wine-data) scores)))
      (push `((id . ,wine-id)
              (scores . (((participant . ,participant-id) (score . nil) (sentiment . ,sentiment)))))
            wines-data))
    (funcall (plist-get actions :update-data) 'wines wines-data)))


;;; Order Tab Components

(defcomponent brb-plan-tab-order ()
  :render
  (vui-vstack
   (vui-component 'brb-plan-order-summary)
   (vui-component 'brb-plan-order-personal)))


;;; Order Summary

(defcomponent brb-plan-order-summary ()
  :render
  (let* ((data (use-brb-plan-data))
         (participants (use-brb-plan-participants))
         (actions (use-brb-plan-actions))
         (personal (alist-get 'personal data)))
    (vui-vstack
     (vui-text "All Orders" :face 'org-level-2)
     (vui-newline)
     (if (null personal)
         (vui-text "No orders yet.")
       (let* ((grand-total-qty
               (-sum (--map
                      (-sum (--map (alist-get 'amount it) (alist-get 'orders it)))
                      personal)))
              (grand-total-price
               (-sum (--map
                      (* (alist-get 'price it)
                         (-sum (--map (alist-get 'amount it) (alist-get 'orders it))))
                      personal))))
         (vui-table
          :columns '((:header "" :width 3)
                     (:header "Item" :width 38 :truncate t)
                     (:header "Price" :width 10 :align :right)
                     (:header "Qty" :width 4 :align :right)
                     (:header "Total" :width 10 :align :right)
                     (:header "Participants" :width 34 :truncate t))
          :rows (append
                 (--map-indexed
                  (let* ((item it)
                         (idx it-index)
                         (item-name (or (alist-get 'item item) ""))
                         (price (or (alist-get 'price item) 0))
                         (orders (alist-get 'orders item))
                         (total-qty (-sum (--map (alist-get 'amount it) orders)))
                         (total-price (* price total-qty))
                         (participant-names
                          (string-join
                           (--map
                            (let ((pid (alist-get 'participant it)))
                              (vulpea-note-title
                               (--find (string= (vulpea-note-id it) pid) participants)))
                            orders)
                           ", ")))
                    (list
                     (vui-button "x"
                       :on-click (lambda ()
                                   (let ((new-personal (-remove-at idx personal)))
                                     (funcall (plist-get actions :update-data)
                                              'personal new-personal))))
                     item-name
                     (brb-price-format price)
                     (number-to-string total-qty)
                     (brb-price-format total-price)
                     participant-names))
                  personal)
                 (list
                  :separator
                  (list "" "" "" (number-to-string grand-total-qty)
                        (brb-price-format grand-total-price) "")))
          :border :ascii)))
     (vui-newline))))


;;; Per-Participant Orders

(defcomponent brb-plan-order-personal ()
  :render
  (let* ((data (use-brb-plan-data))
         (participants (use-brb-plan-participants))
         (actions (use-brb-plan-actions))
         (personal (alist-get 'personal data)))
    (vui-vstack
     (vui-text "Orders by Participant" :face 'org-level-2)
     (vui-newline)
     (vui-list participants
               (lambda (person)
                 (let* ((pid (vulpea-note-id person))
                        (name (vulpea-note-title person))
                        (participant-orders
                         (->> personal
                              (--map
                               (let* ((order-entry (--find (string= pid (alist-get 'participant it))
                                                           (alist-get 'orders it)))
                                      (amount (when order-entry (alist-get 'amount order-entry))))
                                 (when (and amount (> amount 0))
                                   `((item . ,(alist-get 'item it))
                                     (price . ,(alist-get 'price it))
                                     (amount . ,amount)
                                     (total . ,(* amount (alist-get 'price it)))))))
                              (-non-nil)))
                        (participant-total (-sum (--map (alist-get 'total it) participant-orders))))
                   (vui-vstack
                    (vui-text name :face 'bold)
                    (if (null participant-orders)
                        (vui-hstack
                         (vui-text "No orders. ")
                         (vui-button "+ Add"
                           :on-click (lambda ()
                                       (brb-plan--order-add actions data personal pid))))
                      (vui-table
                       :columns '((:width 3)
                                  (:width 24 :truncate t)
                                  (:width 10 :align :right)
                                  (:width 5 :align :center)
                                  (:width 10 :align :right))
                       :rows (append
                              (--map
                               (let ((item-name (alist-get 'item it))
                                     (price (alist-get 'price it))
                                     (amount (alist-get 'amount it))
                                     (total (alist-get 'total it)))
                                 (list
                                  (vui-button "x"
                                    :on-click (lambda ()
                                                (brb-plan--order-remove-participant
                                                 actions data personal item-name pid)))
                                  item-name
                                  (brb-price-format price)
                                  (vui-button (format "%d" amount)
                                    :on-click (lambda ()
                                                (let ((new-amount (read-number "Amount: " amount)))
                                                  (brb-plan--order-update-qty
                                                   actions data personal item-name pid new-amount))))
                                  (brb-price-format total)))
                               participant-orders)
                              (list
                               (list
                                (vui-button "+"
                                  :on-click (lambda ()
                                              (brb-plan--order-add actions data personal pid)))
                                ""
                                ""
                                "Total:"
                                (brb-price-format participant-total))))))
                    (vui-newline))))
               #'vulpea-note-id))))

(defun brb-plan--order-add (actions data personal pid)
  "Add order for participant PID using ACTIONS with DATA and PERSONAL."
  (let* ((item-name (completing-read "Item: " (--map (alist-get 'item it) personal)))
         (existing (--find (string= item-name (alist-get 'item it)) personal))
         (price (or (when existing (alist-get 'price existing))
                    (read-number "Price: " 0)))
         (amount (read-number "Amount: " 1))
         (new-order `((participant . ,pid) (amount . ,amount))))
    (if existing
        (let* ((orders (alist-get 'orders existing))
               (order-entry (--find (string= pid (alist-get 'participant it)) orders)))
          (if order-entry
              (setf (alist-get 'amount order-entry) amount)
            (setf (alist-get 'orders existing) (append orders (list new-order)))))
      (let ((new-item `((item . ,item-name) (price . ,price) (orders . (,new-order)))))
        (setq personal (append personal (list new-item)))))
    (funcall (plist-get actions :update-data) 'personal personal)))

(defun brb-plan--order-remove-participant (actions data personal item-name pid)
  "Remove PID from item ITEM-NAME using ACTIONS with DATA and PERSONAL."
  (let ((item (--find (string= item-name (alist-get 'item it)) personal)))
    (when item
      (setf (alist-get 'orders item)
            (--remove (string= pid (alist-get 'participant it))
                      (alist-get 'orders item)))
      (when (null (alist-get 'orders item))
        (setq personal (--remove (string= item-name (alist-get 'item it)) personal))))
    (funcall (plist-get actions :update-data) 'personal personal)))

(defun brb-plan--order-update-qty (actions data personal item-name pid amount)
  "Update AMOUNT for PID in item ITEM-NAME using ACTIONS."
  (let* ((item (--find (string= item-name (alist-get 'item it)) personal))
         (order-entry (when item
                        (--find (string= pid (alist-get 'participant it))
                                (alist-get 'orders item)))))
    (when order-entry
      (setf (alist-get 'amount order-entry) amount)
      (funcall (plist-get actions :update-data) 'personal personal))))


;;; Extra Tab Components

(defcomponent brb-plan-tab-extra ()
  :render
  (let* ((data (use-brb-plan-data))
         (wines (use-brb-plan-wines))
         (participants (use-brb-plan-participants))
         (actions (use-brb-plan-actions))
         (wines-data (alist-get 'wines data))
         (extra-wines (--filter
                       (let* ((wid (vulpea-note-id it))
                              (wd (--find (string= wid (alist-get 'id it)) wines-data)))
                         (string= "extra" (alist-get 'type wd)))
                       wines)))
    (vui-vstack
     (vui-text "Extra Wines" :face 'org-level-2)
     (vui-newline)
     (if (null extra-wines)
         (vui-text "No extra wines. Mark a wine as type \"extra\" in the Plan tab.")
       (vui-list extra-wines
                 (lambda (wine)
                   (let* ((wine-id (vulpea-note-id wine))
                          (wine-data (--find (string= wine-id (alist-get 'id it)) wines-data))
                          (price-public (or (alist-get 'price-public wine-data) 0))
                          (price-real (or (alist-get 'price-real wine-data) 0))
                          (price-asking (or (alist-get 'price-asking wine-data) price-public))
                          (wine-participants (alist-get 'participants wine-data))
                          (participant-count (length wine-participants))
                          (glass-price (brb-plan--glass-price wine-data)))
                     (vui-vstack
                      (vui-text (format "--- %s ---" (vulpea-note-title wine)) :face 'bold)
                      (vui-hstack (vui-text "Price public: ")
                                  (vui-text (brb-price-format price-public)))
                      (vui-hstack (vui-text "Price real:   ")
                                  (vui-text (brb-price-format price-real)))
                      (vui-hstack (vui-text "Asking price: ")
                                  (vui-button (brb-price-format price-asking)
                                    :on-click (lambda ()
                                                (let ((price (read-number "Asking price: " price-asking)))
                                                  (funcall (plist-get actions :update-wine-data)
                                                           wine-id 'price-asking price)))))
                      (vui-hstack (vui-text "Participants: ")
                                  (vui-text (number-to-string participant-count)))
                      (vui-hstack (vui-text "Glass price:  ")
                                  (vui-text (brb-price-format glass-price)))
                      (vui-newline)
                      ;; Participant toggles
                      (vui-list participants
                                (lambda (person)
                                  (let* ((pid (vulpea-note-id person))
                                         (name (vulpea-note-title person))
                                         (included (member pid wine-participants)))
                                    (vui-hstack
                                     (vui-button (if included "X" " ")
                                       :on-click (lambda ()
                                                   (brb-plan--extra-toggle
                                                    actions data wines-data wine-id pid)))
                                     (vui-text name))))
                                #'vulpea-note-id)
                      (vui-newline))))
                 #'vulpea-note-id))
     (vui-newline))))

(defun brb-plan--extra-toggle (actions data wines-data wine-id pid)
  "Toggle PID for extra wine WINE-ID using ACTIONS."
  (let ((wine-data (--find (string= wine-id (alist-get 'id it)) wines-data)))
    (when wine-data
      (let ((participants (alist-get 'participants wine-data)))
        (if (member pid participants)
            (setf (alist-get 'participants wine-data)
                  (--remove (string= it pid) participants))
          (setf (alist-get 'participants wine-data)
                (cons pid participants))))
      (funcall (plist-get actions :update-data) 'wines wines-data))))


;;; Invoices Tab Components

(defcomponent brb-plan-tab-invoices ()
  :render
  (vui-vstack
   (vui-component 'brb-plan-invoice-actions)
   (vui-component 'brb-plan-invoice-settings)
   (vui-component 'brb-plan-invoices-personal)))


;;; Invoice Actions

(defcomponent brb-plan-invoice-actions ()
  :render
  (let ((actions (use-brb-plan-actions))
        (event (use-brb-plan-event))
        (data (use-brb-plan-data))
        (participants (use-brb-plan-participants))
        (wines (use-brb-plan-wines))
        (host (use-brb-plan-host))
        (balances (or (use-brb-plan-balances) (make-hash-table :test 'equal))))
    (vui-vstack
     (vui-text "Actions" :face 'org-level-2)
     (vui-newline)
     (vui-hstack :spacing 1
                 (vui-button "Refresh Balances"
                   :on-click (lambda ()
                               (funcall (plist-get actions :refresh-balances))
                               (message "Balances refreshed")))
                 (vui-button "Charge All"
                   :on-click (lambda ()
                               (brb-plan--charge-all event data participants wines host balances)
                               (message "All participants charged")))
                 (vui-button "Record Spendings"
                   :on-click (lambda ()
                               (brb-plan--record-spendings event data participants wines host balances)
                               (message "Spendings recorded"))))
     (vui-newline))))

(defun brb-plan--charge-all (event data participants wines host balances)
  "Record charges for all participants."
  (let ((date (vulpea-utils-with-note event
                (vulpea-buffer-prop-get "date"))))
    (--each participants
      (unless (and host (string= (vulpea-note-id it) (vulpea-note-id host)))
        (let ((st (brb-event-statement-for
                   event it
                   :data data
                   :host host
                   :wines wines
                   :balances balances
                   :participants participants)))
          (brb-ledger-charge
           :convive it
           :code (concat (vulpea-note-id event) ":" (vulpea-note-id it))
           :amount (alist-get 'total st)
           :date (date-to-time date)
           :comment (vulpea-note-title event)))))))

(defun brb-plan--record-spendings (event data participants wines host balances)
  "Record event spendings to ledger."
  (let* ((statement (brb-event-statement event
                      :data data
                      :participants participants
                      :wines wines
                      :host host
                      :balances balances))
         (date (vulpea-utils-with-note event
                 (vulpea-buffer-prop-get "date"))))
    (brb-ledger-record-txn
     :amount (alist-get 'spending-wines-real statement)
     :date (date-to-time date)
     :comment (format "%s: wines" (vulpea-note-title event))
     :code (concat (vulpea-note-id event) ":wines")
     :account-to "personal:account"
     :account-from "balance:assets")
    (brb-ledger-record-txn
     :amount (alist-get 'spending-extra-real statement)
     :date (date-to-time date)
     :comment (format "%s: wines extra" (vulpea-note-title event))
     :code (concat (vulpea-note-id event) ":wines-extra")
     :account-to "personal:account"
     :account-from "balance:assets")
    (brb-ledger-spend
     :amount (alist-get 'spending-shared statement)
     :date (date-to-time date)
     :comment (format "%s: shared" (vulpea-note-title event))
     :code (concat (vulpea-note-id event) ":shared"))
    (when (> (alist-get 'spending-expense-wines statement) 0)
      (brb-ledger-record-txn
       :amount (alist-get 'spending-expense-wines statement)
       :date (date-to-time date)
       :comment (format "%s: expense wines" (vulpea-note-title event))
       :code (concat (vulpea-note-id event) ":expense-wines")
       :account-to "personal:account"
       :account-from "balance:assets"))
    (brb-ledger-spend
     :amount (alist-get 'spending-order statement)
     :date (date-to-time date)
     :comment (format "%s: delivery" (vulpea-note-title event))
     :code (concat (vulpea-note-id event) ":delivery"))))


;;; Invoice Settings

(defcomponent brb-plan-invoice-settings ()
  :render
  (let* ((event (use-brb-plan-event))
         (actions (use-brb-plan-actions))
         (use-balance (or (vulpea-note-meta-get event "use balance") "true"))
         (pay-url (vulpea-note-meta-get event "pay url" 'link)))
    (vui-vstack
     (vui-text "Settings" :face 'org-level-2)
     (vui-newline)
     (vui-hstack
      (vui-text "Use balance: ")
      (vui-button (if (string= use-balance "true") "X" " ")
        :on-click (lambda ()
                    (let ((new-val (if (string= use-balance "true") "false" "true")))
                      (funcall (plist-get actions :set-event-meta) "use balance" new-val)))))
     (vui-hstack
      (vui-text "Pay URL: ")
      (vui-button (or pay-url "not set")
        :on-click (lambda ()
                    (let ((url (read-string "Pay URL: " (or pay-url ""))))
                      (funcall (plist-get actions :set-event-meta) "pay url" url)))))
     (vui-newline))))


;;; Per-Participant Invoices

(defcomponent brb-plan-invoices-personal ()
  :render
  (let* ((event (use-brb-plan-event))
         (data (use-brb-plan-data))
         (participants (use-brb-plan-participants))
         (wines (use-brb-plan-wines))
         (host (use-brb-plan-host))
         (actions (use-brb-plan-actions))
         (balances (or (use-brb-plan-balances) (make-hash-table :test 'equal)))
         (use-balance (string= "true" (or (vulpea-note-meta-get event "use balance") "true"))))
    (vui-vstack
     (vui-text "Invoices" :face 'org-level-2)
     (vui-newline)
     (vui-list participants
               (lambda (person)
                 (let* ((pid (vulpea-note-id person))
                        (name (vulpea-note-title person))
                        (st (brb-event-statement-for
                             event person
                             :data data
                             :host host
                             :wines wines
                             :balances balances
                             :participants participants))
                        (paid-by-id (alist-get 'paid-by st)))
                   ;; Skip participants who are paid for by someone else
                   (unless paid-by-id
                     (let* ((fee (alist-get 'fee st))
                            (base-fee (alist-get 'base-fee st))
                            (paying-for (alist-get 'paying-for st))
                            (paying-for-fees (alist-get 'paying-for-fees st))
                            (order (alist-get 'order st))
                            (extra (alist-get 'extra st))
                            (paying-for-orders (alist-get 'paying-for-orders st))
                            (paying-for-extras (alist-get 'paying-for-extras st))
                            (total (alist-get 'total st))
                            (balance (alist-get 'balance st))
                            (balance-final (alist-get 'balance-final st))
                            (due (alist-get 'due st)))
                       (vui-vstack
                        (vui-text name :face 'org-level-3)
                        (vui-newline)
                        ;; Pays for section
                        (vui-hstack
                         (vui-text "Pays for ")
                         (vui-button "+"
                           :on-click (lambda ()
                                       (brb-plan--add-pays-for actions participants pid paying-for))))
                        (when paying-for
                          (vui-list paying-for
                                    (lambda (payee-id)
                                      (let ((payee (--find (string-equal payee-id (vulpea-note-id it)) participants)))
                                        (vui-hstack
                                         (vui-text "  ")
                                         (vui-button "x"
                                           :on-click (lambda ()
                                                       (funcall (plist-get actions :remove-pays-for) pid payee-id)))
                                         (vui-space)
                                         (vui-text (if payee (vulpea-note-title payee) payee-id)))))
                                    #'identity
                                    :vertical t))
                        (vui-newline)
                        ;; Invoice breakdown as table
                        (vui-table
                         :columns '((:width 44 :truncate t)
                                    (:width 12 :align :right))
                         :rows (append
                                ;; Starting balance (if applicable)
                                (when (and use-balance (not (= balance 0)))
                                  (list (list "Starting balance" (brb-price-format balance))))
                                ;; Event fee
                                (if (and paying-for (> paying-for-fees 0))
                                    (list
                                     (list "Event fee (self)" (brb-price-format base-fee))
                                     (list (format "Event fee (%d others)" (length paying-for))
                                           (brb-price-format paying-for-fees)))
                                  (list (list "Event fee" (brb-price-format fee))))
                                ;; Orders
                                (--map (list (alist-get 'item it)
                                             (brb-price-format (alist-get 'total it)))
                                       order)
                                ;; Extra wines
                                (--map (list (format "Extra: %s"
                                                     (vulpea-note-title (alist-get 'wine it)))
                                             (brb-price-format (alist-get 'total it)))
                                       extra)
                                ;; Orders for people we're paying for
                                (--map (list (format "Order (others): %s" (alist-get 'item it))
                                             (brb-price-format (alist-get 'total it)))
                                       paying-for-orders)
                                ;; Extras for people we're paying for
                                (--map (list (format "Extra (others): %s"
                                                     (vulpea-note-title (alist-get 'wine it)))
                                             (brb-price-format (alist-get 'total it)))
                                       paying-for-extras)
                                ;; Separator and totals
                                (list :separator)
                                (list (list (vui-text "Total" :face 'bold)
                                            (vui-text (brb-price-format total) :face 'bold)))
                                ;; Balance info (if applicable)
                                (when (and use-balance (not (= balance 0)))
                                  (list
                                   (list "Final balance" (brb-price-format balance-final))
                                   (list (vui-text "Due" :face (if (> due 0) 'error 'success))
                                         (vui-text (brb-price-format due)
                                           :face (if (> due 0) 'error 'success)))))))
                        ;; Copy button
                        (vui-newline)
                        (vui-button "Copy Invoice"
                          :on-click (lambda ()
                                      (brb-plan--copy-invoice event person st use-balance balance)))
                        (vui-newline)
                        (vui-newline))))))
               #'vulpea-note-id))))

(defun brb-plan--add-pays-for (actions participants payer-id current-payees)
  "Add someone for PAYER-ID to pay for.
ACTIONS is the actions plist. PARTICIPANTS is list of all participants.
CURRENT-PAYEES is list of IDs already being paid for."
  (let* ((available (--remove (or (string-equal payer-id (vulpea-note-id it))
                                  (-contains-p current-payees (vulpea-note-id it)))
                              participants))
         (choice (completing-read "Pay for: "
                                  (--map (vulpea-note-title it) available)
                                  nil t)))
    (when-let ((person (--find (string-equal choice (vulpea-note-title it)) available)))
      (funcall (plist-get actions :add-pays-for) payer-id (vulpea-note-id person)))))

(defun brb-plan--copy-invoice (event person statement use-balance balance)
  "Copy invoice for PERSON to clipboard."
  (let* ((name (vulpea-note-title person))
         (event-name (vulpea-note-title event))
         (fee (alist-get 'fee statement))
         (order (alist-get 'order statement))
         (extra (alist-get 'extra statement))
         (total (alist-get 'total statement))
         (due (alist-get 'due statement))
         (lines (list (format "Invoice for %s - %s" name event-name)
                      "")))
    (when (and use-balance (not (= balance 0)))
      (push (format "Starting balance: %s" (brb-price-format balance)) lines))
    (push (format "Event fee: %s" (brb-price-format fee)) lines)
    (--each order
      (push (format "Order %s: %s" (alist-get 'item it) (brb-price-format (alist-get 'total it))) lines))
    (--each extra
      (push (format "Extra %s: %s" (vulpea-note-title (alist-get 'wine it))
                    (brb-price-format (alist-get 'total it))) lines))
    (push "" lines)
    (push (format "Total: %s" (brb-price-format total)) lines)
    (when (and use-balance (not (= balance 0)))
      (push (format "Due: %s" (brb-price-format due)) lines))
    (kill-new (string-join (nreverse lines) "\n"))
    (message "Invoice copied to clipboard")))


;;; Entry Point

;;;###autoload
(defun brb-event-plan (&optional event)
  "Open planning UI for EVENT using vui.el.

If EVENT is nil, interactively select one."
  (interactive)
  (let* ((event (or event (brb-event-select)))
         (buffer-name (format "*Event: %s*" (vulpea-note-title event))))
    (vui-mount (vui-component 'brb-event-plan-app :event event) buffer-name)
    (switch-to-buffer buffer-name)))

(provide 'brb-event-plan)
;;; brb-event-plan.el ends here
