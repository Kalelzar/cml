;;; cml --- Package for manipulating Calibre Metadata
;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'f)
(require 's)
(require 'dash)
(require 'hydra)

(cl-defstruct calibre-metadata
  "Calibre metadata."
  (author "Unknown"
          :type string
          :docstring "The author")
  (author_sort "Unknown"
               :type string
               :docstring "The author name to sort by.")
  (creator "Unknown"
           :type string
           :docstring "No idea what the difference is between this and author.")
  (date "1900-01-01"
        :type string
        :docstring "The date of publishing in ISO-8601 format.")
  (description "None"
               :type string
               :docstring "A summary of the book.")
  (id nil
      :type string
      :docstring "The id of the book in the calibre library.")
  (identifier '()
              :type list
              :docstring "A plist of various identifiers (isbn, google, etc).")
  (language "eng"
            :type string
            :docstring "The language in ISO-639 format.")
  (publisher "Unknown"
             :type string
             :docstring "The publisher of the book.")
  (rating ""
          :type string
          :docstring "A number from 1-10 or empty.")
  (series ""
          :type string
          :docstring "The series the book belongs to if any.")
  (series_index ""
                :type string
                :docstring "The index of the book in the series.")
  (tag '()
       :type list
       :docstring "A list of tags.")
  (timestamp ""
             :type string
             :docstring "Some calibre date I am too lazy to look-up. Ignored")
  (title "Unknown"
         :type string
         :docstring "Title of the book")
  (title_sort "Unknown"
              :type string
              :docstring "The title to use for sorting"))

(defvar cml-dir "~/Code/cml"
  "The directory where cml sources are stored.")

(defvar iso-639 (eval (car (read-from-string (f-read (f-join cml-dir "iso639.sexp")))))
  "An alist of iso-639-2 language codes and their corresponding English name.")

(defun cme-pick-book ()
  (interactive)
  (let* ((books (--map
                (format "%s: %s" (car it) (s-join " | " (cdr it)))
                (--map
                  (s-split "" it)
                  (s-split ""
                           (calibredb-query "SELECT id,title,author_sort FROM books")))))
        (selected (cadr (s-match "^\\([0-9]+\\):.*"  (completing-read "Select Book: " books nil t)))))
    (setq calibre-metadata-edit-metadata (load-cml (string-to-number selected)))
    (calibre-metadata-edit/body)))

(defun load-cml (id)
  "Load a 'cml' file with name ID as a `calibre-metadata' struct."
  (eval (car
         (read-from-string
          (f-read
           (f-join cml-dir
                   (format "%d.cml"
                           id)))))
        t))

(defun cml-to-org (id)
  "Convert a 'cml' file with name ID to an `org-mode' file."
  (let ((meta (load-cml id)))
    (with-temp-file (f-join cml-dir
                            (format "%d.org"
                                    id))
      (insert (format "#+TITLE: %s\n" (calibre-metadata-title meta)))
      (insert (format "#+AUTHOR: %s\n" (calibre-metadata-author meta)))
      (insert (format "#+SERIES: %s\n" (calibre-metadata-series meta)))
      (insert (format "#+SERIES_INDEX: %s\n"
                      (calibre-metadata-series_index meta)))
      (insert (format "#+DATE: %s\n" (calibre-metadata-date meta)))
      (insert (format "#+LANGUAGE: %s\n" (calibre-metadata-language meta)))
      (insert (format "#+ID: %s\n" (calibre-metadata-id meta)))
      (insert (format "#+PUBLISHER: %s\n" (calibre-metadata-publisher meta)))
      (insert (format "#+TITLE_SORT: %s\n" (calibre-metadata-title_sort meta)))
      (insert (format "#+AUTHOR_SORT: %s\n" (calibre-metadata-author_sort meta)))
      (insert (format "#+CREATOR: %s\n" (calibre-metadata-creator meta)))
      (insert (format "#+RATING: %s\n" (calibre-metadata-rating meta)))
      (insert (format "#+TAGS: %s\n" (s-join " "
                                             (--map
                                              (format "\"%s\"" it)
                                              (calibre-metadata-tag meta)))))
      (insert "\n* Description\n\n")
      (let ((position (buffer-end 1)))
        (insert (s-replace "." ".\n\n" (calibre-metadata-description meta)))
        (fill-region position (buffer-end 1)))

      )))

(defgroup calibre-metadata-mode-group
  '()
  "TODO")

(define-derived-mode calibre-metadata-mode
  fundamental-mode
  "cmm"
  "A mode for editing calibre metadata"
  :group 'calibre-metadata-mode-group)

(defvar cme-metadata nil
  "Stores the `calibre-metadata' struct for the current book.")

;(setq cme-metadata (load-cml 1))
(provide 'cml)
;;; cml.el ends here
