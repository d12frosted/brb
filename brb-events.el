;;; brb-events.el --- Multi-event dashboard with vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/brb

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
;; Multi-event dashboard for Barberry Garden wine club.
;; Built with vui.el - a React-like component library for Emacs.
;;
;; Main entry point is `brb-events' which opens an interactive buffer
;; listing events in a time range with two tabs:
;; - overview: event listing with action buttons
;; - stats: aggregated statistics across events
;;

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'vulpea)
(require 'vui)
(require 'brb)
(require 'brb-event)
(require 'brb-event-plan)


;;; Contexts

(vui-defcontext brb-events-all nil
  "Filtered events list.")

(vui-defcontext brb-events-actions nil
  "Plist of action functions.")


;;; Action helpers

(defun brb-events--set-date (event reload-fn)
  "Prompt for date and update EVENT, then call RELOAD-FN."
  (let ((date (with-temp-buffer
                (let ((date (org-read-date nil t nil "Date: ")))
                  (org-insert-timestamp date nil t)
                  (buffer-substring (point-min) (point-max))))))
    (vulpea-utils-with-note (vulpea-db-get-by-id (vulpea-note-id event))
      (vulpea-buffer-prop-set "date" date)
      (save-buffer))
    (vulpea-db-update-file (vulpea-note-path event))
    (funcall reload-fn)))

(defun brb-events--set-location (event reload-fn)
  "Prompt for location and update EVENT, then call RELOAD-FN."
  (let ((location (vulpea-select-from
                   "Location"
                   (vulpea-db-query-by-tags-every '("places"))
                   :require-match t)))
    (vulpea-utils-with-note (vulpea-db-get-by-id (vulpea-note-id event))
      (vulpea-buffer-meta-set "location" location)
      (save-buffer))
    (vulpea-db-update-file (vulpea-note-path event))
    (funcall reload-fn)))

(defun brb-events--set-host (event reload-fn)
  "Prompt for host and update EVENT, then call RELOAD-FN."
  (let ((host (vulpea-select-from
               "Host"
               (vulpea-db-query-by-tags-every '("people"))
               :require-match t)))
    (vulpea-utils-with-note (vulpea-db-get-by-id (vulpea-note-id event))
      (vulpea-buffer-meta-set "host" host)
      (save-buffer))
    (vulpea-db-update-file (vulpea-note-path event))
    (funcall reload-fn)))


;;; Root Component

(vui-defcomponent brb-events-app (frame range)
  :state ((events-all nil)
          (filter "internal")
          (tab "overview"))

  :on-mount
  (let ((events (-concat (apply #'brb-events-from-range range)
                          (brb-events-without-date))))
    (vui-set-state :events-all events))

  :render
  (let* ((filtered (pcase filter
                     ("all" events-all)
                     ("internal" (--filter
                                  (not (vulpea-note-tagged-all-p it "external"))
                                  events-all))
                     ("external" (--filter
                                  (vulpea-note-tagged-all-p it "external")
                                  events-all))
                     (_ events-all)))
         (actions
          (list
           :set-tab
           (lambda (new-tab)
             (vui-set-state :tab new-tab))

           :set-filter
           (lambda (new-filter)
             (vui-set-state :filter new-filter))

           :reload-events
           (lambda ()
             (let ((events (-concat (apply #'brb-events-from-range range)
                                     (brb-events-without-date))))
               (vui-set-state :events-all events))))))

    (brb-events-all-provider filtered
      (brb-events-actions-provider actions
        (vui-vstack
         (vui-component 'brb-events-header
           :tab tab :filter filter :range range)
         (pcase tab
           ("overview" (vui-component 'brb-events-tab-overview))
           ("stats" (vui-component 'brb-events-tab-stats))
           (_ (vui-text (format "Unknown tab: %s" tab)))))))))


;;; Header Component

(vui-defcomponent brb-events-header (tab filter range)
  :render
  (let ((actions (use-brb-events-actions)))
    (vui-vstack
     (vui-text "Barberry Garden Events" :face 'org-level-1)
     (vui-text (format "from %s to %s" (nth 0 range) (nth 1 range)))
     (vui-newline)
     ;; Filter buttons
     (vui-hstack
      :spacing 1
      (vui-button (if (string= filter "all") "*all*" "all")
        :face (when (string= filter "all") 'bold)
        :on-click (lambda () (funcall (plist-get actions :set-filter) "all")))
      (vui-button (if (string= filter "internal") "*internal*" "internal")
        :face (when (string= filter "internal") 'bold)
        :on-click (lambda () (funcall (plist-get actions :set-filter) "internal")))
      (vui-button (if (string= filter "external") "*external*" "external")
        :face (when (string= filter "external") 'bold)
        :on-click (lambda () (funcall (plist-get actions :set-filter) "external"))))
     (vui-newline)
     ;; Tab buttons
     (vui-hstack
      :spacing 1
      (vui-button (if (string= tab "overview") "*overview*" "overview")
        :face (when (string= tab "overview") 'bold)
        :on-click (lambda () (funcall (plist-get actions :set-tab) "overview")))
      (vui-button (if (string= tab "stats") "*stats*" "stats")
        :face (when (string= tab "stats") 'bold)
        :on-click (lambda () (funcall (plist-get actions :set-tab) "stats"))))
     (vui-newline))))


;;; Overview Tab

(vui-defcomponent brb-events-tab-overview ()
  :render
  (let* ((events (use-brb-events-all))
         (actions (use-brb-events-actions))
         (reload-fn (plist-get actions :reload-events))
         (today (format-time-string "%F" (current-time)))
         (memo-key (mapcar #'vulpea-note-id events))
         (statement-tbl (vui-use-memo (memo-key)
                          (let ((tbl (make-hash-table :test 'equal)))
                            (--each events
                              (puthash (vulpea-note-id it)
                                       (brb-event-statement it
                                         :balances (make-hash-table))
                                       tbl))
                            tbl)))
         (render-event
          (lambda (event &optional show-gain)
            (let* ((wines (brb-event-wines event))
                   (date (brb-event-date-string event))
                   (gain (when show-gain
                           (alist-get 'balance-real
                                      (gethash (vulpea-note-id event) statement-tbl)))))
              (list
               (vui-button (or date "-")
                 :on-click (lambda ()
                             (brb-events--set-date event reload-fn)))
               (vui-button "P"
                 :on-click (lambda () (brb-event-plan event)))
               (concat
                (if (vulpea-note-tagged-any-p event "external") "E" "I")
                (if (string-equal "true" (vulpea-note-meta-get event "publish")) "+" "-"))
               (vui-button (vulpea-note-title event)
                 :on-click (lambda () (vulpea-visit event)))
               (vui-button
                   (if-let* ((location (vulpea-note-meta-get event "location" 'note)))
                       (vulpea-note-title location)
                     "<unknown>")
                 :on-click (lambda ()
                             (brb-events--set-location event reload-fn)))
               (vui-button
                   (if-let* ((host (vulpea-note-meta-get event "host" 'note)))
                       (or (vulpea-note-meta-get host "public name")
                           (vulpea-note-title host))
                     "<unknown>")
                 :on-click (lambda ()
                             (brb-events--set-host event reload-fn)))
               (number-to-string (seq-length (brb-event-participants event)))
               (number-to-string (seq-length wines))
               (brb-price-format (vulpea-note-meta-get event "price" 'number))
               (if gain (brb-price-format gain) "")))))

         (splitted (--split-with
                    (let ((date (brb-event-date-string it)))
                      (and date (string< date today)))
                    events))
         (events-past (nth 0 splitted))
         (events-future (nth 1 splitted))
         (total-participants (--reduce-from
                              (+ acc (seq-length (brb-event-participants it)))
                              0 events))
         (total-wines (--reduce-from
                       (+ acc (seq-length (brb-event-wines it)))
                       0 events))
         (total-gain (->> events-past
                          (--reduce-from
                           (+ acc (or (alist-get 'balance-real
                                                 (gethash (vulpea-note-id it) statement-tbl))
                                      0))
                           0)))
         (table-rows (-concat
                      (--map (funcall render-event it t) events-past)
                      (when (and events-past events-future) '(:separator))
                      (--map (funcall render-event it nil) events-future)
                      '(:separator)
                      (list (list "" "" "" "" "" ""
                                  (number-to-string total-participants)
                                  (number-to-string total-wines)
                                  ""
                                  (brb-price-format total-gain))))))
    (vui-vstack
     (vui-hstack
      (vui-text (format "Events (%d) " (seq-length events)) :face 'org-level-2)
      (vui-button "+"
        :on-click (lambda ()
                    (brb-event-create)
                    (funcall reload-fn))))
     (vui-newline)
     (vui-table
      :columns '((:header "date" :min-width 10)
                 (:header "" :min-width 3)
                 (:header "" :min-width 3)
                 (:header "event" :width 30)
                 (:header "location" :min-width 10)
                 (:header "host" :min-width 10)
                 (:header "folks" :min-width 5 :align :right)
                 (:header "wines" :min-width 5 :align :right)
                 (:header "price" :min-width 10 :align :right)
                 (:header "gain" :min-width 10 :align :right))
      :rows table-rows
      :border :ascii)
     (vui-newline))))


;;; Stats Tab

(vui-defcomponent brb-events-tab-stats ()
  :render
  (let* ((events (use-brb-events-all))
         (memo-key (mapcar #'vulpea-note-id events))
         (summary-tbl (vui-use-memo (memo-key)
                        (let ((tbl (make-hash-table :test 'equal)))
                          (--each events
                            (puthash (vulpea-note-id it)
                                     (brb-event-summary it)
                                     tbl))
                          tbl)))
         (statement-tbl (vui-use-memo (memo-key)
                          (let ((tbl (make-hash-table :test 'equal)))
                            (--each events
                              (puthash (vulpea-note-id it)
                                       (brb-event-statement it
                                         :balances (make-hash-table))
                                       tbl))
                            tbl))))
    (vui-vstack
     (vui-component 'brb-events-stats-events
       :events events :summary-tbl summary-tbl :statement-tbl statement-tbl)
     (vui-component 'brb-events-stats-locations
       :events events)
     (vui-component 'brb-events-stats-participants
       :events events)
     (vui-component 'brb-events-stats-wines
       :events events :summary-tbl summary-tbl)
     (vui-component 'brb-events-stats-producers
       :events events)
     (vui-component 'brb-events-stats-grapes
       :events events)
     (vui-component 'brb-events-stats-countries
       :events events)
     (vui-component 'brb-events-stats-regions
       :events events)
     (vui-component 'brb-events-stats-colours
       :events events))))


;;; Stats: Events

(vui-defcomponent brb-events-stats-events (events summary-tbl statement-tbl)
  :render
  (let* ((today (format-time-string "%F" (current-time)))
         (events-past (--filter (string> today (brb-event-date-string it)) events))
         (lines (--map
                 (let* ((summary (gethash (vulpea-note-id it) summary-tbl))
                        (date (brb-event-date-string it)))
                   (list (or date "-")
                         (vui-button (vulpea-note-title it)
                           :on-click (lambda () (vulpea-visit it)))
                         (number-to-string (seq-length (brb-event-participants it)))
                         (number-to-string (seq-length (brb-event-wines it)))
                         (if-let* ((wavg (alist-get 'wavg summary)))
                             (format "%.4f" wavg)
                           "-")
                         (brb-price-format (vulpea-note-meta-get it "price" 'number))
                         (if-let* ((qpr (alist-get 'qpr summary)))
                             (format "%.4f" qpr)
                           "-")))
                 events))
         (splitted (--split-with (string< (nth 0 it) today) lines))
         (total-participants (--reduce-from
                              (+ acc (seq-length (brb-event-participants it)))
                              0 events))
         (total-wines (--reduce-from
                       (+ acc (seq-length (brb-event-wines it)))
                       0 events))
         (total-gain (->> events-past
                          (--reduce-from
                           (+ acc (or (alist-get 'balance-real
                                                 (gethash (vulpea-note-id it) statement-tbl))
                                      0))
                           0))))
    (vui-vstack
     (vui-text (format "Events (%d)" (seq-length events)) :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "date" :min-width 10)
                 (:header "event" :width 30)
                 (:header "folks" :min-width 5 :align :right)
                 (:header "wines" :min-width 5 :align :right)
                 (:header "wavg" :min-width 6 :align :right)
                 (:header "price" :min-width 10 :align :right)
                 (:header "qpr" :min-width 6 :align :right))
      :rows (-concat
             (nth 0 splitted)
             (when (nth 1 splitted) '(:separator))
             (nth 1 splitted)
             '(:separator)
             (list (list ""
                         ""
                         (number-to-string total-participants)
                         (number-to-string total-wines)
                         ""
                         ""
                         (brb-price-format total-gain))))
      :border :ascii)
     (vui-newline))))


;;; Stats: Locations

(vui-defcomponent brb-events-stats-locations (events)
  :render
  (let* ((locs (--map (if-let* ((loc (vulpea-note-meta-get it "location" 'note)))
                          (vulpea-note-title loc)
                        "unknown")
                      events))
         (locs-uniq (brb-count-unique locs))
         (locs-sorted (--sort (> (cdr it) (cdr other)) locs-uniq)))
    (vui-vstack
     (vui-text (format "Locations (%d)" (seq-length locs-uniq)) :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:min-width 20)
                 (:min-width 5 :align :right))
      :rows (--map (list (car it) (number-to-string (cdr it))) locs-sorted))
     (vui-newline))))


;;; Stats: Participants

(vui-defcomponent brb-events-stats-participants (events)
  :render
  (let* ((today (format-time-string "%F" (current-time)))
         (events-past (--filter (string> today (brb-event-date-string it)) events))
         (participants-past (->> events-past
                                 (--map (brb-event-participants it))
                                 (-flatten)))
         (participants-all (->> events
                                (--map (brb-event-participants it))
                                (-flatten)))
         (participants (->> participants-all
                            (-map #'vulpea-note-id)
                            (vulpea-db-query-by-ids)
                            (-remove #'vulpea-note-primary-title))))
    (vui-vstack
     (vui-text (format "Participants (%s)" (seq-length participants)) :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "participant" :min-width 20)
                 (:header "level" :min-width 5 :align :right)
                 (:header "wix" :min-width 5 :align :right)
                 (:header "past" :min-width 4 :align :right)
                 (:header "all" :min-width 4 :align :right))
      :rows (->> participants
                 (--map
                  (list (vui-button (vulpea-note-title it)
                          :on-click (lambda () (vulpea-visit it)))
                        (or (vulpea-note-meta-get it "tasting level") "")
                        (or (vulpea-note-meta-get it "wix") "")
                        (number-to-string
                         (-count (lambda (other)
                                   (string-equal (vulpea-note-id it)
                                                 (vulpea-note-id other)))
                                 participants-past))
                        (number-to-string
                         (-count (lambda (other)
                                   (string-equal (vulpea-note-id it)
                                                 (vulpea-note-id other)))
                                 participants-all))))
                 (--sort (> (string-to-number (nth 3 it))
                            (string-to-number (nth 3 other)))))
      :border :ascii)
     (vui-newline))))


;;; Stats: Wines

(vui-defcomponent brb-events-stats-wines (events summary-tbl)
  :render
  (let* ((today (format-time-string "%F" (current-time)))
         (events-past (--filter (string> today (brb-event-date-string it)) events))
         (wines (->> events-past
                     (--map (brb-event-wines it))
                     (-flatten)
                     (-distinct)))
         (wine-rows (->> events-past
                         (--map
                          (let ((summary (alist-get 'wines (gethash (vulpea-note-id it) summary-tbl)))
                                (event it))
                            (->> (brb-event-wines it)
                                 (--map-indexed
                                  (let* ((wine it)
                                         (wine-summary (nth it-index summary))
                                         (producer (vulpea-note-meta-get wine "producer" 'note)))
                                    (list
                                     (brb-event-date-string event)
                                     (vui-button (vulpea-note-title event)
                                       :on-click (lambda () (vulpea-visit event))
                                       :max-width 16)
                                     (if producer
                                         (vui-button (vulpea-note-title producer)
                                           :on-click (lambda () (vulpea-visit producer))
                                           :max-width 18)
                                       "")
                                     (vui-button (or (vulpea-note-meta-get wine "name") "")
                                       :on-click (lambda () (vulpea-visit wine))
                                       :max-width 20)
                                     (let ((v (vulpea-note-meta-get wine "vintage")))
                                       (if (numberp v) (number-to-string v) (or v "NV")))
                                     (if-let* ((wavg (alist-get 'wavg wine-summary)))
                                         (format "%.4f" wavg) "-")
                                     (if-let* ((sdev (alist-get 'sdev wine-summary)))
                                         (format "%.4f" sdev) "-")
                                     (if-let* ((price (alist-get 'amount (alist-get 'price wine-summary))))
                                         (number-to-string price) "-")
                                     (if-let* ((qpr (alist-get 'qpr wine-summary)))
                                         (format "%.4f" qpr) "-")))))))
                         (-flatten-n 1)
                         (--sort (string> (nth 5 it) (nth 5 other))))))
    (vui-vstack
     (vui-text (format "Wines (%s)" (seq-length wines)) :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "date" :min-width 10)
                 (:header "event" :width 16)
                 (:header "producer" :width 18)
                 (:header "wine" :width 20)
                 (:header "year" :min-width 4)
                 (:header "wavg" :min-width 6 :align :right)
                 (:header "sdev" :min-width 6 :align :right)
                 (:header "price" :min-width 6 :align :right)
                 (:header "qpr" :min-width 6 :align :right))
      :rows wine-rows
      :border :ascii)
     (vui-newline))))


;;; Stats: Producers

(vui-defcomponent brb-events-stats-producers (events)
  :render
  (let* ((today (format-time-string "%F" (current-time)))
         (events-past (--filter (string> today (brb-event-date-string it)) events))
         (producers-all (->> events
                             (--map (brb-event-wines it))
                             (-flatten)
                             (--map (vulpea-note-meta-get it "producer" 'note))
                             (-flatten)))
         (producers-past (->> events-past
                              (--map (brb-event-wines it))
                              (-flatten)
                              (--map (vulpea-note-meta-get it "producer" 'note))
                              (-flatten)))
         (producers (->> producers-all
                         (-map #'vulpea-note-id)
                         (vulpea-db-query-by-ids)
                         (--filter (null (vulpea-note-primary-title it))))))
    (vui-vstack
     (vui-text (format "Producers (%s)" (seq-length producers)) :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "producer" :min-width 20)
                 (:header "past" :min-width 4 :align :right)
                 (:header "all" :min-width 4 :align :right))
      :rows (->> producers
                 (--map
                  (list (vui-button (vulpea-note-title it)
                          :on-click (lambda () (vulpea-visit it)))
                        (number-to-string
                         (-count (lambda (other)
                                   (string-equal (vulpea-note-id it)
                                                 (vulpea-note-id other)))
                                 producers-past))
                        (number-to-string
                         (-count (lambda (other)
                                   (string-equal (vulpea-note-id it)
                                                 (vulpea-note-id other)))
                                 producers-all))))
                 (--sort (> (string-to-number (nth 1 it))
                            (string-to-number (nth 1 other)))))
      :border :ascii)
     (vui-newline))))


;;; Stats: Grapes

(vui-defcomponent brb-events-stats-grapes (events)
  :render
  (let* ((today (format-time-string "%F" (current-time)))
         (events-past (--filter (string> today (brb-event-date-string it)) events))
         (grapes-all (->> events
                          (--map (brb-event-wines it))
                          (-flatten)
                          (--map (vulpea-note-meta-get-list it "grapes" 'note))
                          (-flatten)))
         (grapes-past (->> events-past
                           (--map (brb-event-wines it))
                           (-flatten)
                           (--map (vulpea-note-meta-get-list it "grapes" 'note))
                           (-flatten)))
         (grapes (->> grapes-all
                      (-map #'vulpea-note-id)
                      (vulpea-db-query-by-ids)
                      (--filter (null (vulpea-note-primary-title it))))))
    (vui-vstack
     (vui-text (format "Grapes (%s)" (seq-length grapes)) :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "grape" :min-width 20)
                 (:header "past" :min-width 4 :align :right)
                 (:header "all" :min-width 4 :align :right))
      :rows (->> grapes
                 (--map
                  (list (vui-button (vulpea-note-title it)
                          :on-click (lambda () (vulpea-visit it)))
                        (number-to-string
                         (-count (lambda (other)
                                   (string-equal (vulpea-note-id it)
                                                 (vulpea-note-id other)))
                                 grapes-past))
                        (number-to-string
                         (-count (lambda (other)
                                   (string-equal (vulpea-note-id it)
                                                 (vulpea-note-id other)))
                                 grapes-all))))
                 (--sort (> (string-to-number (nth 1 it))
                            (string-to-number (nth 1 other)))))
      :border :ascii)
     (vui-newline))))


;;; Stats: Countries

(vui-defcomponent brb-events-stats-countries (events)
  :render
  (let* ((today (format-time-string "%F" (current-time)))
         (events-past (--filter (string> today (brb-event-date-string it)) events))
         (wines-all (->> events
                         (--map (brb-event-wines it))
                         (-flatten)))
         (wines-past (->> events-past
                          (--map (brb-event-wines it))
                          (-flatten)))
         (countries-all (->> wines-all
                             (--map (vulpea-note-meta-get
                                     (or (vulpea-note-meta-get it "region" 'note)
                                         (vulpea-note-meta-get it "appellation" 'note))
                                     "country" 'note))
                             (-flatten)))
         (countries-past (->> wines-past
                              (--map (vulpea-note-meta-get
                                      (or (vulpea-note-meta-get it "region" 'note)
                                          (vulpea-note-meta-get it "appellation" 'note))
                                      "country" 'note))
                              (-flatten)))
         (countries (->> countries-all
                         (-map #'vulpea-note-id)
                         (vulpea-db-query-by-ids)
                         (--filter (null (vulpea-note-primary-title it))))))
    (vui-vstack
     (vui-text (format "Countries (%s)" (seq-length countries)) :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "country" :min-width 20)
                 (:header "past" :min-width 4 :align :right)
                 (:header "all" :min-width 4 :align :right))
      :rows (->> countries
                 (--map
                  (list (vui-button (vulpea-note-title it)
                          :on-click (lambda () (vulpea-visit it)))
                        (number-to-string
                         (-count (lambda (other)
                                   (string-equal (vulpea-note-id it)
                                                 (vulpea-note-id other)))
                                 countries-past))
                        (number-to-string
                         (-count (lambda (other)
                                   (string-equal (vulpea-note-id it)
                                                 (vulpea-note-id other)))
                                 countries-all))))
                 (--sort (> (string-to-number (nth 1 it))
                            (string-to-number (nth 1 other)))))
      :border :ascii)
     (vui-newline))))


;;; Stats: Regions

(vui-defcomponent brb-events-stats-regions (events)
  :render
  (let* ((today (format-time-string "%F" (current-time)))
         (events-past (--filter (string> today (brb-event-date-string it)) events))
         (wines-all (->> events
                         (--map (brb-event-wines it))
                         (-flatten)))
         (wines-past (->> events-past
                          (--map (brb-event-wines it))
                          (-flatten)))
         (roas-all (->> wines-all
                        (--map (or (vulpea-note-meta-get it "region" 'note)
                                   (vulpea-note-meta-get it "appellation" 'note)))
                        (-flatten)))
         (roas-past (->> wines-past
                         (--map (or (vulpea-note-meta-get it "region" 'note)
                                    (vulpea-note-meta-get it "appellation" 'note)))
                         (-flatten)))
         (roas (->> roas-all
                    (-map #'vulpea-note-id)
                    (vulpea-db-query-by-ids)
                    (--filter (null (vulpea-note-primary-title it))))))
    (vui-vstack
     (vui-text (format "Regions (%s)" (seq-length roas)) :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "region" :min-width 20)
                 (:header "past" :min-width 4 :align :right)
                 (:header "all" :min-width 4 :align :right))
      :rows (->> roas
                 (--map
                  (list (vui-button (vulpea-note-title it)
                          :on-click (lambda () (vulpea-visit it)))
                        (number-to-string
                         (-count (lambda (other)
                                   (string-equal (vulpea-note-id it)
                                                 (vulpea-note-id other)))
                                 roas-past))
                        (number-to-string
                         (-count (lambda (other)
                                   (string-equal (vulpea-note-id it)
                                                 (vulpea-note-id other)))
                                 roas-all))))
                 (--sort (> (string-to-number (nth 1 it))
                            (string-to-number (nth 1 other)))))
      :border :ascii)
     (vui-newline))))


;;; Stats: Colours

(vui-defcomponent brb-events-stats-colours (events)
  :render
  (let* ((today (format-time-string "%F" (current-time)))
         (events-past (--filter (string> today (brb-event-date-string it)) events))
         (colours-all (->> events
                           (--map (brb-event-wines it))
                           (-flatten)
                           (--map (vulpea-note-meta-get it "colour" 'string))
                           (-flatten)))
         (colours-past (->> events-past
                            (--map (brb-event-wines it))
                            (-flatten)
                            (--map (vulpea-note-meta-get it "colour" 'string))
                            (-flatten)))
         (colours (-uniq colours-all)))
    (vui-vstack
     (vui-text (format "Colours (%s)" (seq-length colours)) :face 'org-level-2)
     (vui-newline)
     (vui-table
      :columns '((:header "colour" :min-width 20)
                 (:header "past" :min-width 4 :align :right)
                 (:header "all" :min-width 4 :align :right))
      :rows (->> colours
                 (--map
                  (list it
                        (number-to-string
                         (-count (lambda (other)
                                   (string-equal it other))
                                 colours-past))
                        (number-to-string
                         (-count (lambda (other)
                                   (string-equal it other))
                                 colours-all))))
                 (--sort (> (string-to-number (nth 1 it))
                            (string-to-number (nth 1 other)))))
      :border :ascii)
     (vui-newline))))


;;; Entry Point

;;;###autoload
(defun brb-events (&optional arg)
  "Display vui-based dashboard for managing events.

By default this full year is used as a time frame.  Pass a universal
ARG to override and query for specific frame."
  (interactive "P")
  (let* ((frame (if arg nil 'this-year-full))
         (frame (or
                 frame
                 (intern
                  (completing-read
                   "Time frame: " (cons 'custom brb-time-frames)
                   nil 'require-match))))
         (range (pcase frame
                  (`custom (list
                            (org-read-date nil nil nil "From (inclusive)")
                            (org-read-date nil nil nil "To (exclusive)")))
                  (_ (brb-time-frame-range frame))))
         (buffer-name (format "*barberry garden events*")))
    (vui-mount (vui-component 'brb-events-app :frame frame :range range)
               buffer-name)
    (switch-to-buffer buffer-name)))

(provide 'brb-events)
;;; brb-events.el ends here
