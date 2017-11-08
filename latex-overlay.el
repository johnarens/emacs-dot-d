(setq org-latex-create-formula-image-program 'imagemagick)
(defvar org-latex-fragment-last nil
  "Holds last fragment/environment you were on.")

;; (defun element-at ()
;;   (interactive)
;;   (let* ((el (org-element-at-point))
;;    (el-type (org-element-type el)))
;;     (if (or (eq 'latex-fragment el-type)
;;       (eq 'latex-environment el-type))
;;   (message "True")
;;       (message "False"))))

(defun org-latex-fragment-toggle ()
  "Toggle a latex fragment image "
  ;; (let* ((el-at-point (org-element-at-point))
  ;;    (el-type (car el-at-point)))
  ;;   "")
  (and (eq 'org-mode major-mode)
       (let* ((el (org-element-context))
              (el-type (org-element-type el)))
         (cond
    ;; not on a fragment and now are not on a fragment
          ((and
            ;; we were not one one
            (not org-latex-fragment-last)
            ;; not on a fragment now
            (not (or
                  (eq 'latex-fragment el-type)
                  (eq 'latex-environment el-type))))
      nil)
    
          ;; were on a fragment and now on a new fragment
          ((and
            ;; fragment we were on
            org-latex-fragment-last
            ;; and are on a fragment now
            (or
             (eq 'latex-fragment el-type)
             (eq 'latex-environment el-type))
            ;; but not on the last one this is a little tricky. as you edit the
            ;; fragment, it is not equal to the last one. We use the begin
            ;; property which is less likely to change for the comparison.
            (not (= (org-element-property :begin el)
                    (org-element-property :begin org-latex-fragment-last))))
           ;; go back to last one and put image back
           (save-excursion
             (goto-char (org-element-property :begin org-latex-fragment-last))
             (org-preview-latex-fragment))
           ;; now remove current image
           (goto-char (org-element-property :begin el))
           (let ((ov (loop for ov in org-latex-fragment-image-overlays
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           return ov)))
             (when ov
               (delete-overlay ov)))
           ;; and save new fragment
           (setq org-latex-fragment-last el))

          ;; were on a fragment and now are not on a fragment
          ((and
            ;; not on a fragment now
            (not (or
                  (eq 'latex-fragment el-type)
                  (eq 'latex-environment el-type)))
            ;; but we were on one
            org-latex-fragment-last)
           ;; put image back on
           (save-excursion
             (goto-char (org-element-property :begin org-latex-fragment-last))
             (org-preview-latex-fragment))
           ;; unset last fragment
           (setq org-latex-fragment-last nil))

          ;; were not on a fragment, and now are
          ((and
            ;; we were not one one
            (not org-latex-fragment-last)
            ;; but now we are
            (or
             (eq 'latex-fragment el-type)
             (eq 'latex-environment el-type)))
           (goto-char (org-element-property :begin el))
           ;; remove image
           (let ((ov (loop for ov in org-latex-fragment-image-overlays
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           return ov)))
             (when ov
               (delete-overlay ov)))
           (setq org-latex-fragment-last el))))))

(add-hook 'post-command-hook 'org-latex-fragment-toggle)
