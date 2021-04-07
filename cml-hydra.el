;;; cml-hydra -- Hydra definitions for cml
;;; Commentary:
;;; Code:

(require 'hydra)
(require 's)
(require 'dash)
(require 'cml-format)
(require 'cml-edit)

(defvar cme-hydra-stack nil
  "A stack for the currently active hydras.
Allows you to return to previous hydra when you quit the current one.")

(defun cme-hydra-push (current-hydra)
  "Push CURRENT-HYDRA on `cme-hydra-stack'.
CURRENT-HYDRA must be a hydra body function."
  (push current-hydra cme-hydra-stack))

(defun cme-hydra-pop ()
  "Pop the top function from `cme-hydra-stack' and call it if it is non-nil."
  (interactive)
  (let ((prev (pop cme-hydra-stack)))
    (when prev
      (funcall prev))))

(defvar cme--frame-long-cache '(0 . "")
  "Don't remake the long lines if the frame size hasn't changed.")

(defun cme--make-frame-long ()
  "Generate a long line of unicode dashes that is dependent of `frame-width'.
This result is cached in `cme--frame-long-cache' and reused if the `frame-width'
hasn't changed."
  (if (= (car cme--frame-long-cache) (frame-width))
      (cdr cme--frame-long-cache)
    (cdr (setq cme--frame-long-cache
               (cons (frame-width) . (s-repeat (- (frame-width) 17) "━"))))))

(defun cme-make-menu (&rest elements)
  "Generate a menu-like thing from ELEMENTS.
This returns a symbol so don't put any characters that need escaping
such as whitespace in the elements."
  (let* ((keys
          (s-join ""
                  (--map
                   (format "┥%s┝" it)
                   elements)))
         (keylen (length keys))
         (spacers (- (/ (- (frame-width) keylen) 2.0) 1.5))
         (left-spacer (s-repeat (ceiling spacers) "━"))
         (right-spacer (s-repeat (floor spacers) "━")))
    (s-concat left-spacer keys right-spacer)
    ))


(defhydra cme-hydra/title (:hint none
                                 :color amaranth)
  "
┏━━━━━━━━━━━━━┯%s(cme--make-frame-long)┓
┃ _t_itle       ┆ ?t?
┃ _T_itle sort  ┆ ?T?
┣━━━━━━━━━━━━━┷%s(cme--make-frame-long)┫
┗%s(cme-make-menu \"quit\")┛
"
  ("t" (cme-edit-title) (cme-hydra-format "~t"))
  ("T" (cme-edit-title-sort) (cme-hydra-format "~T"))
  ("q" cme-hydra-pop "Quit" :exit t))

(defhydra cme-hydra/series (:hint none
                                              :color amaranth)
  "
┏━━━━━━━━━━━━━┯%s(cme--make-frame-long)┓
┃ _s_eries      ┆ ?s?
┃ _i_ndex       ┆ ?i?
┣━━━━━━━━━━━━━┷%s(cme--make-frame-long)┫
┗%s(cme-make-menu \"quit\")┛
"
  ("s" (cme-edit-series) (cme-hydra-format "~s"))
  ("i" (cme-edit-series-index) (cme-hydra-format "~S"))
  ("q" cme-hydra-pop "Quit" :exit t))

(defhydra cme-hydra/publisher (:hint none
                                              :color amaranth)
  "
┏━━━━━━━━━━━━━┯%s(cme--make-frame-long)┓
┃ _p_ublisher   ┆ ?p?
┃ _d_ate        ┆ ?d?
┣━━━━━━━━━━━━━┷%s(cme--make-frame-long)┫
┗%s(cme-make-menu \"quit\")┛
"
  ("p" (cme-edit-publisher) (cme-hydra-format "~p"))
  ("d" (progn
         (cme-hydra-push #'cme-hydra/publisher/body)
         (cme-edit-date))
   (cme-hydra-format "~D"))
  ("q" cme-hydra-pop "Quit" :exit t))

(defhydra cme-hydra/tags (:hint none
                                              :color amaranth)
  "
???
┏━━━━━━━━━━━━━┯%s(cme--make-frame-long)┓
┃ _a_dd         ┆ ?a?
┃ _r_emove      ┆ ?r?
┃ _e_dit        ┆ ?e?
┣━━━━━━━━━━━━━┷%s(cme--make-frame-long)┫
┗%s(cme-make-menu \"quit\")┛
"
  ("a" (cme-edit-add-tag) "Add tag")
  ("r" (cme-edit-remove-tag) "Remove tag")
  ("e" (cme-edit-tag) "Edit tag")
  ("q" cme-hydra-pop "Quit" :exit t)
  ("?" (identity nil) (s-join "\n" (--map
                                    (s-join " "
                                            (s-split "💩"
                                                     (-reduce
                                                      #'s-concat
                                                      it)))
                                    (-partition-all (- (frame-width) 2)
                                                    (s-split ""
                                                             (s-join "💩"
                                                                     (--map
                                                                      (s-wrap it "[" "]")
                                                                      (calibre-metadata-tag cme-metadata)))))))))

(defhydra cme-hydra/author (:hint none
                                             :color amaranth)
  "
┏━━━━━━━━━━━━━┯%s(cme--make-frame-long)┓
┃ _a_uthor      ┆ ?a?
┃ _A_uthor sort ┆ ?A?
┃ _c_reator     ┆ ?c?
┣━━━━━━━━━━━━━┷%s(cme--make-frame-long)┫
┗%s(cme-make-menu \"quit\")┛
"
  ("a" (cme-edit-author) (cme-hydra-format "~a"))
  ("A" (cme-edit-author-sort) (cme-hydra-format "~A"))
  ("c" (cme-edit-creator) (cme-hydra-format "~c"))
  ("q" cme-hydra-pop "Quit" :exit t))

(defhydra cme-hydra
  (:pre (setq cme-hydra-stack nil)
   :hint  none
   :color amaranth)
  "
┏━━━━━━━━━━━━━┯%s(cme--make-frame-long)┓
┃ _t_itle       ┆ ?t?
┃ _a_uthor      ┆ ?a?
┃ _s_eries      ┆ ?s?
┃ _T_ags        ┆ ?T?
┃ _r_ating      ┆ ?r?
┃ _d_escription ┆ ?d?
┃ _p_ublishing  ┆ ?p?
┃ _i_dentifiers ┆ ?i?
┃ _l_anguage    ┆ ?l?
┣━━━━━━━━━━━━━┷%s(cme--make-frame-long)┫
┗%s(cme-make-menu \"quit\" \"Revert\" \"undo\")┛
"
  ("t" (progn
         (cme-hydra/title/body)
         (cme-hydra-push #'cme-hydra/body))
   (cme-hydra-title)
   :color teal)
  ("T" (progn
         (cme-hydra/tags/body)
         (cme-hydra-push #'cme-hydra/body))
   (cme-hydra-tag)
   :color teal)
  ("r" (cme-edit-rating) (cme-hydra-rating))
  ("a" (progn
         (cme-hydra/author/body)
         (cme-hydra-push #'cme-hydra/body))
   (cme-hydra-author)
   :color teal)
  ("p" (progn
         (cme-hydra/publisher/body)
         (cme-hydra-push #'cme-hydra/body))
   (cme-hydra-publishing)
   :color teal)
  ("d" (progn
         (cme-edit-description)
         (cme-hydra-push #'cme-hydra/body))
   (cme-hydra-description)
   :color teal)
  ("s" (progn
         (cme-hydra/series/body)
         (cme-hydra-push #'cme-hydra/body))
   (cme-hydra-series)
   :color teal)
  ("l" (cme-edit-language) (cme-hydra-language))
  ("i" (cme-edit-identifier) (cme-hydra-identifiers))
  ("R" (message "Revert") "Revert" :exit t)
  ("u" (message "Undo last") "Undo last")
  ("q" (message "Done") "Quit" :exit t))

(bind-key "e" #'cme-hydra/body calibre-metadata-mode-map)

(provide 'cml-hydra)
;;; cml-hydra.el ends here
