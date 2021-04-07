;;; cml-format -- String formatting for cml
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'cml)

(defcustom cme-hydra-title-format
  "~t (~T)"
  "A format string used by `cme-hydra-title'."
  :type 'string
  :safe t
  :group 'calibre-metadata-mode-group
  :package-version "1")

(defcustom cme-hydra-tag-format
  "[~w]"
  "A format string used by `cme-hydra-tag'."
  :type 'string
  :safe t
  :group 'calibre-metadata-mode-group
  :package-version "1")

(defcustom cme-hydra-author-format
  "~a (~A) [~c]"
  "A format string used by `cme-hydra-author'."
  :type 'string
  :safe t
  :group 'calibre-metadata-mode-group
  :package-version "1")

(defcustom cme-hydra-series-format
  "~s ~S"
  "A format string used by `cme-hydra-series'."
  :type 'string
  :safe t
  :group 'calibre-metadata-mode-group
  :package-version "1")

(defcustom cme-hydra-rating-format
  "~r"
  "A format string used by `cme-hydra-rating'."
  :type 'string
  :safe t
  :group 'calibre-metadata-mode-group
  :package-version "1")

(defcustom cme-hydra-description-format
  "~d"
  "A format string used by `cme-hydra-description'."
  :type 'string
  :safe t
  :group 'calibre-metadata-mode-group
  :package-version "1")

(defcustom cme-hydra-publishing-format
  "Published by ~p on ~D"
  "A format string used by `cme-hydra-publishing'."
  :type 'string
  :safe t
  :group 'calibre-metadata-mode-group
  :package-version "1")

(defcustom cme-hydra-identifiers-format
  "~i"
  "A format string used by `cme-hydra-identifiers'."
  :type 'string
  :safe t
  :group 'calibre-metadata-mode-group
  :package-version "1")


(defcustom cme-hydra-language-format
  "~l"
  "A format string used by `cme-hydra-language'."
  :type 'string
  :safe t
  :group 'calibre-metadata-mode-group
  :package-version "1")

(defun cme-format (format)
  "Format FORMAT.
~t Title
~T Title Sort
~w Tags
~a Author
~A Author sort
~c creator
~s Series
~S Series index
~r Rating
~d Description
~D Publish Date
~p Publisher
~i Identifiers
~l Language"
  (let (
        (title (calibre-metadata-title cme-metadata))
        (author (calibre-metadata-author cme-metadata))
        (author-sort (calibre-metadata-author_sort cme-metadata))
        (creator (calibre-metadata-creator cme-metadata))
        (title-sort (calibre-metadata-title_sort cme-metadata))
        (tags (calibre-metadata-tag cme-metadata))
        (series (calibre-metadata-series cme-metadata))
        (series-index (calibre-metadata-series_index cme-metadata))
        (rating (calibre-metadata-rating cme-metadata))
        (description (s-replace-regexp "<.*?>" "" (calibre-metadata-description cme-metadata)))
        (date (calibre-metadata-date cme-metadata))
        (publisher (calibre-metadata-publisher cme-metadata))
        (identifiers (calibre-metadata-identifier cme-metadata))
        (language (calibre-metadata-language cme-metadata))
        )
    (s-replace-regexp "~[tTaAcwsSrdDpil]"
                      (lambda (match)
                        (cond
                         ((string= match "~t") title)
                         ((string= match "~s") series)
                         ((string= match "~r") rating)
                         ((string= match "~l") (if-let ((lang (alist-get language iso-639 nil nil #'string=)))
                                                   (format "%s [%s]" lang language)
                                                   language))
                         ((string= match "~D") date)
                         ((string= match "~p") publisher)
                         ((string= match "~i")
                          (s-replace ":" ""
                                     (s-join " "
                                             (--map
                                              (destructuring-bind (key val) it
                                                (format "[%S %s]" key val))
                                              (-partition 2 identifiers)))))
                         ((string= match "~S") series-index)
                         ((string= match "~T")
                          (if (string= title title-sort)
                              ""
                            title-sort))
                         ((string= match "~c") creator)
                         ((string= match "~a") author)
                         ((string= match "~A")
                          (if (string= author author-sort)
                              ""
                            author-sort))
                         ((string= match "~w")
                          (s-join ", " tags))
                         ((string= match "~d")
                          description)
                         ))
                      format)))

(defun cme-hydra-format (format)
  "Same as `cme-format' but pad the string with whitespace until it is
the correct size for the hydra docstrings and cap it with a unicode vertical bar."
  (s-truncate (- (frame-width) 17)
              (s-pad-right (frame-width) " "
                           (cme-format format))
              "┃"))

(defun cme-hydra-title ()
  "Return a formatted view of the book title."
  (cme-hydra-format cme-hydra-title-format))

(defun cme-hydra-tag ()
  "Return a formatted view of the book tags."
  (cme-hydra-format cme-hydra-tag-format))

(defun cme-hydra-author ()
  "Return a formatted view of the book authors."
  (cme-hydra-format cme-hydra-author-format))

(defun cme-hydra-series ()
  "Return a formatted view of the book series."
  (cme-hydra-format cme-hydra-series-format))

(defun cme-hydra-rating ()
  "Return a formatted view of the book ratings."
  (cme-hydra-format cme-hydra-rating-format))

(defun cme-hydra-description ()
  "Return a formatted view of the book descriptions."
  (cme-hydra-format cme-hydra-description-format))

(defun cme-hydra-publishing ()
  "Return a formatted view of the book publishings."
  (cme-hydra-format cme-hydra-publishing-format))

(defun cme-hydra-identifiers ()
  "Return a formatted view of the book identifierss."
  (cme-hydra-format cme-hydra-identifiers-format))

(defun cme-hydra-language ()
  "Return a formatted view of the book languages."
  (cme-hydra-format cme-hydra-language-format))



(provide 'cml-format)
;;; cml-format.el ends here
