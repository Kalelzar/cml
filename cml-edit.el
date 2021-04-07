;;; cml-edit -- Edit functions for cml
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'cml)

(defun read-language (mode &optional initial-input)
  "Pick a language.
When MODE is nil, show the iso-639-2 language codes.
When MODE is 0, show the English name of the language.
When MODE is anything else show both.
This function will always return the iso-639-2 language code."
  (cond
   ((null mode)
    (completing-read "Pick language: "
                     (-map #'car iso-639)
                     nil
                     t
                     initial-input))
   ((eql mode 0)
    (car
     (let ((picked (completing-read "Pick language: "
                                    (-map #'cdr iso-639)
                                    nil
                                    t
                                    initial-input)))
       (--find
        (string= (cdr it) picked)
        iso-639))))
   (t
    (second (s-match ".*\\[\\(.*\\)\\]"
                     (completing-read "Pick language: "
                                      (--map (format "%s [%s]" (cdr it) (car it))
                                             iso-639)
                                      nil
                                      t
                                      initial-input))))))

(defun cme-gen-title-sort (title)
  "Generate a sortable title string from a TITLE."
  (if-let* ((eradicate '("A" "The" "An"))
            (eradicated (-first-item
                         (--filter
                          (string-prefix-p (concat it
                                                   " ")
                                           title
                                           t)
                          eradicate))))
      (concat (s-replace-regexp (concat "^"
                                        eradicated
                                        " ")
                                ""
                                title)
              ", "
              eradicated)
    (if (string= (calibre-metadata-title_sort cme-metadata)
                 (calibre-metadata-title cme-metadata))
        nil
      (calibre-metadata-title cme-metadata))))

(defun cme-gen-title (title-sort)
  "Generate a title string from a TITLE-SORT."
  (if-let* ((eradicate '("A" "The" "An"))
            (eradicated (-first-item
                         (--filter
                          (string-suffix-p (concat ", "
                                                   it)
                                           title-sort
                                           t)
                          eradicate))))
      (concat eradicated
              " "
              (s-replace-regexp (concat ", "
                                        eradicated
                                        "$")
                                ""
                                title-sort))
    (if (string= (calibre-metadata-title_sort cme-metadata)
                 (calibre-metadata-title cme-metadata))
        nil
      (calibre-metadata-title_sort cme-metadata))))

(defun cme-gen-author-sort (author)
  (s-join " & "
          (--map
           (s-concat (s-join " " (cdr it)) ", " (car it))
           (--map
            (s-split " " it)
            (s-split ", " author)))))

(defun cme-gen-author (author-sort)
  (s-join ", "
          (--map
           (s-concat (s-join " " (cdr it)) " " (car it))
           (--map
            (s-split ", " it)
            (s-split " & " author-sort)))))

(defun cme-edit-author (&optional no-prompt)
  "Edit the author of the current book.
Don't prompt for updating the author sort if NO-PROMPT is non-nil."
  (setf (calibre-metadata-author cme-metadata)
        (read-string "New Author: "
                     (calibre-metadata-author cme-metadata)))
  (when-let* (((not no-prompt))
              (author-sort (cme-gen-author-sort
                            (calibre-metadata-author cme-metadata)))
              ((not (string= author-sort (calibre-metadata-author_sort cme-metadata))))
              (answer (completing-read (format "Update author sort to '%s': " author-sort)
                                       '("Yes"
                                         "No"
                                         "Set manually")
                                       #'identity
                                       t)))
    (cond
     ((string= answer "Yes")
      (setf (calibre-metadata-author_sort cme-metadata)
            author-sort))
     ((string= answer "Set manually")
      (cme-edit-author-sort t)))))

(defun cme-edit-author-sort (&optional no-prompt)
  "Edit the author sort of the current book.
Don't prompt for updating the author if NO-PROMPT is non-nil."
  (setf (calibre-metadata-author_sort cme-metadata)
        (read-string "New Author Sort: "
                     (calibre-metadata-author_sort cme-metadata)))
  (when-let* (((not no-prompt))
              (author (cme-gen-author
                       (calibre-metadata-author_sort cme-metadata)))
              ((not (string= author (calibre-metadata-author cme-metadata))))
              (answer (completing-read (format "Update author to '%s': " author)
                                       '("Yes"
                                         "No"
                                         "Set manually")
                                       #'identity
                                       t)))
    (cond
     ((string= answer "Yes")
      (setf (calibre-metadata-author cme-metadata)
            author))
     ((string= answer "Set manually")
      (cme-edit-author t)))
    ))

(defun cme-edit-creator ()
  "Edit the creator of the current book."
  (setf (calibre-metadata-creator cme-metadata)
        (read-string "New Creator: "
                     (calibre-metadata-creator cme-metadata))))

(defun cme-edit-series ()
  "Edit the series of the current book."
  (setf (calibre-metadata-series cme-metadata)
        (read-string "New Series: "
                     (calibre-metadata-series cme-metadata))))

(defun cme-edit-publisher ()
  "Edit the publisher of the current book."
  (setf (calibre-metadata-publisher cme-metadata)
        (read-string "New Publisher: "
                     (calibre-metadata-publisher cme-metadata))))

(defun cme-edit-language ()
  "Edit the language of the current book."
  (setf (calibre-metadata-language cme-metadata)
        (read-language t
                       (calibre-metadata-language cme-metadata))))

(defun cme-edit-series-index ()
  "Edit the series index of the current book."
  (setf (calibre-metadata-series_index cme-metadata)
        (format "%s"
                (read-number "New Series Index: "
                             (string-to-number
                              (calibre-metadata-series_index cme-metadata))))))

(defun cme-edit-rating ()
  "Edit the rating of the current book."
  (setf (calibre-metadata-rating cme-metadata)
        (completing-read "New Rating: "
                         (-map #'number-to-string (cl-loop for i from 10 downto 1 collect i))
                         #'identity
                         t)))

(defun cme-edit-title (&optional no-prompt)
  "Edit the title of the current book.
Don't prompt for updating the title sort if NO-PROMPT is non-nil."
  (setf (calibre-metadata-title cme-metadata)
        (read-string "New Title: "
                     (calibre-metadata-title cme-metadata)))
  (when-let* (((not no-prompt))
              (title-sort (cme-gen-title-sort
                           (calibre-metadata-title cme-metadata)))
              ((not (string= title-sort (calibre-metadata-title_sort cme-metadata))))
              (answer (completing-read (format "Update title sort to '%s': " title-sort)
                                       '("Yes"
                                         "No"
                                         "Set manually")
                                       #'identity
                                       t)))
    (cond
     ((string= answer "Yes")
      (setf (calibre-metadata-title_sort cme-metadata)
            title-sort))
     ((string= answer "Set manually")
      (cme-edit-title-sort t)))))

(defun cme-edit-tag ()
  (when-let* ((selected (completing-read "Edit tag: "
                                         (calibre-metadata-tag cme-metadata)))
              (edited (read-string "Change to: "
                                   selected)))
    (setf (calibre-metadata-tag cme-metadata)
          (append (remove selected
                          (calibre-metadata-tag cme-metadata))
                  (list edited)))))

(defun cme-edit-add-tag ()
  (when-let ((new (read-string "Add tag: "))
             ((> (length new) 0)))
    (setf (calibre-metadata-tag cme-metadata)
          (append (calibre-metadata-tag cme-metadata)
                  (list new)))))

(defun cme-edit-remove-tag ()
  (when-let* ((selected (completing-read "Remove tag: "
                                         (calibre-metadata-tag cme-metadata))))
    (setf (calibre-metadata-tag cme-metadata)
          (remove selected
                  (calibre-metadata-tag cme-metadata)))))

(defun cme-edit-title-sort (&optional no-prompt)
  "Edit the title sort of the current book.
Don't prompt for updating the title if NO-PROMPT is non-nil."
  (setf (calibre-metadata-title_sort cme-metadata)
        (read-string "New Title Sort: "
                     (calibre-metadata-title_sort cme-metadata)))
  (when-let* (((not no-prompt))
              (title (cme-gen-title
                      (calibre-metadata-title_sort cme-metadata)))
              ((not (string= title (calibre-metadata-title cme-metadata))))
              (answer (completing-read (format "Update title to '%s': " title)
                                       '("Yes"
                                         "No"
                                         "Set manually")
                                       #'identity
                                       t)))
    (cond
     ((string= answer "Yes")
      (setf (calibre-metadata-title cme-metadata)
            title))
     ((string= answer "Set manually")
      (cme-edit-title t)))
    ))

(defvar date-result "")

(defun date-write-back ()
  (interactive)
  (calendar-exit)
  (setq date-result
        (let ((date
               (with-temp-buffer
                 (org-date-from-calendar))))
          (s-concat (car (s-split " "
                                  (s-replace-all '(("<" . "") (">" . ""))
                                                 date)))
                    "T00:00:00Z")))
  (funcall (with-current-buffer calendar-buffer
             read-date-callback)
           date-result))

(defun read-date (callback)
  (calendar)
  (with-current-buffer calendar-buffer
    (let ((map (copy-keymap calendar-mode-map)))
      (bind-key "RET" #'date-write-back map)
      (use-local-map map)
      (setq-local read-date-callback callback))))

(defun cme-date-callback (date)
  (setf (calibre-metadata-date cme-metadata)
        date)
  (cme-hydra-pop))

(defun cme-edit-date ()
  (read-date #'cme-date-callback))

(defun cme-write-back ()
  (interactive)
  (let* ((content-1 (buffer-substring-no-properties (buffer-end -1)
                                                    (buffer-end 1)))
         (content (s-join " " (cdr (s-lines content-1)))))
    (switch-to-buffer (marker-buffer cme-write-back-marker))
    (bury-buffer "*cme*")
    (setf (calibre-metadata-description cme-metadata)
          content)
    (cme-hydra-pop)
    ))

(defun cme-edit-identifier ()
  "Edit the current book's identifier."
  (let* ((identifier-plist (calibre-metadata-identifier cme-metadata))
         (identifiers (--map (s-replace ":" "" (format "%S = %s" (car it) (cadr it)))
                             (-partition 2 identifier-plist)))
         (identifier (completing-read "Add or edit identifier: " identifiers))
         (key (car (read-from-string (concat ":" (first (s-split " " identifier))))))
         (current-value (plist-get identifier-plist key))
         (new-value (read-string (concat "Set " (car (s-split " " identifier)) ": ") current-value)))
    (setf (calibre-metadata-identifier cme-metadata)
          (plist-put identifier-plist key new-value))))

(defun cme-edit-description ()
  "Edit the current book's description."
  (let ((text (calibre-metadata-description cme-metadata))
        (marker (point-marker)))
    (with-current-buffer (get-buffer-create "*cme*")
      (text-mode)
      (kill-region (buffer-end -1) (buffer-end 1))
      (let ((map (copy-keymap text-mode-map)))
        (bind-key "C-c C-c" #'cme-write-back map)
        (use-local-map map)
        (setq-local cme-write-back-marker marker)
        )
      (insert "# 'C-c C-c' to save\n")
      (insert text))
    (switch-to-buffer "*cme*")
    (goto-char 21)))


(provide 'cml-edit)
;;; cml-edit.el ends here
