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



(defun cme-query-metadata (id)
  (let* ((tags-query (format "SELECT group_concat(DISTINCT tags.name) \
FROM books_tags_link
LEFT JOIN tags ON books_tags_link.tag = tags.id
WHERE books_tags_link.book = %s
GROUP BY books_tags_link.book" id))
         (tags (s-split "," (s-replace "" "" (calibredb-query tags-query))))

         (title-query (format "SELECT title,sort FROM books where id = %s" id))
         (title+sort (s-split "" (s-replace "" "" (calibredb-query title-query))))
         (title (car title+sort))
         (title-sort (cadr title+sort))

         (author-id-query (format "SELECT author FROM books_authors_link WHERE book = %s" id))
         (author-id (s-replace "" "" (calibredb-query author-id-query)))

         (author-query (format "SELECT name,sort FROM authors WHERE id = %s" author-id))
         (author+sort (if (string= author-id "")
                          '("Unknown Unknown")
                          (s-split "" (s-replace "" "" (calibredb-query author-query)))))
         (author (car author+sort))
         (author-sort (cadr author+sort))

         (series-id-query (format "SELECT series FROM books_series_link WHERE book = %s" id))
         (series-id (s-replace "" "" (calibredb-query series-id-query)))

         (series-query (format "SELECT name,sort FROM series WHERE id = %s" series-id))
         (series+sort (if (string= series-id "")
                          '("" "")
                        (s-split "" (s-replace "" "" (calibredb-query series-query)))))
         (series (car series+sort))

         (series-index-query (format "SELECT series_index FROM books where id = %s" id))
         (series-index (s-replace "" "" (calibredb-query series-index-query)))

         (pubdate-query (format "SELECT pubdate FROM books where id = %s" id))
         (pubdate (s-replace "" "" (calibredb-query pubdate-query)))

         (description-query (format "SELECT text FROM comments where book = %s" id))
         (description (s-replace "" "" (calibredb-query description-query)))

         (identifiers-query (format "SELECT type,val FROM identifiers where book = %s" id))
         (identifiers-unform (--map
                              (destructuring-bind (type val) (s-split "" it)
                                (list (make-symbol (concat ":" type)) val))
                              (s-split "" (calibredb-query identifiers-query) t)))
         (identifiers (-flatten identifiers-unform))

         (language-id-query (format "SELECT lang_code FROM books_languages_link WHERE book = %s" id))
         (language-id (s-replace "" "" (calibredb-query language-id-query)))

         (language-query (format "SELECT lang_code FROM languages WHERE id = %s" language-id))
         (language (if (string= language-id "")
                       ""
                     (s-replace "" "" (calibredb-query language-query))))

         (publisher-id-query (format "SELECT publisher FROM books_publishers_link WHERE book = %s" id))
         (publisher-id (s-replace "" "" (calibredb-query publisher-id-query)))

         (publisher-query (format "SELECT name FROM publishers WHERE id = %s" publisher-id))
         (publisher (if (string= publisher-id "")
                        "Unknown"
                      (s-replace "" "" (calibredb-query publisher-query))))

         (rating-id-query (format "SELECT rating FROM books_ratings_link WHERE book = %s" id))
         (rating-id (s-replace "" "" (calibredb-query rating-id-query)))

         (rating-query (format "SELECT rating FROM ratings WHERE id = %s" rating-id))
         (rating (if (string= rating-id "")
                     ""
                     (s-replace "" "" (calibredb-query rating-query))))
         )
    (make-calibre-metadata :author author
                           :author_sort author-sort
                           :creator "KILL"
                           :date pubdate
                           :description description
                           :id id
                           :identifier identifiers
                           :language language
                           :publisher publisher
                           :rating rating
                           :series series
                           :series_index series-index
                           :tag tags
                           :timestamp "KILL"
                           :title title
                           :title_sort title-sort)
    ))

(defun cme-pick-book ()
  (interactive)
  (let* ((books (--map
                (format "%s: %s" (car it) (s-join " | " (cdr it)))
                (--map
                  (s-split "" it)
                  (s-split ""
                           (calibredb-query "SELECT id,title,author_sort FROM books")))))
        (selected (cadr (s-match "^\\([0-9]+\\):.*"  (completing-read "Select Book: " books nil t)))))
    (setq cme-metadata (cme-query-metadata (string-to-number selected)))
    (cme-hydra/body)))

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
