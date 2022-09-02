;;; citar-no-cache.el --- Description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Experiment in a cache less citar
;;
;;; Code:

(require 'parsebib)
(require 'citar)
(require 'citar-format)

(defvar citar-no-cache-max-entries 256)
(defvar citar-no-cache--completions nil)
(defvar citar-no-cache--last-query nil)

(defun citar-no-cache-matching-entries (buffer query)
  "Return approxinately `citar-no-cache-max-entries' from BUFFER matching QUERY."
  (let ((entries)
        (numcandidates 0)
        (start 0)
        (end))
    (with-current-buffer buffer
      (goto-char (point-min))
      (setq start (re-search-forward parsebib--entry-start nil 'move)))
    (with-temp-buffer
      (while (and (< numcandidates citar-no-cache-max-entries) start)
        (with-current-buffer buffer
          (dotimes (_ 64)
            (when start
              (setq end (re-search-forward parsebib--entry-start nil 'move))
              (push (buffer-substring (1- start)
                                      (if end (- end 2) (point)))
                    entries))
            (setq start end))
          (cl-callf2 completion-all-completions query entries nil nil)
          (setf (nthcdr (safe-length entries) entries) nil))
        (dolist (entry entries)
          (insert (substring-no-properties entry) "\n")
          (cl-incf numcandidates))
        (setq entries nil))
      (car (parsebib-parse-bib-buffer :fields (citar--fields-to-parse) :replace-TeX t)))))

(defun citar-no-cache--format-candidates (query buffer)
  "Format completion candidates from BUFFER matching QUERY.

Return a hash table with the keys being completion candidate
strings and values being citation keys.

Return nil if `citar-bibliographies' returns nil."
  (when (not (string= query citar-no-cache--last-query))
    (setq citar-no-cache--completions
          (make-hash-table :test #'equal :size (+ citar-no-cache-max-entries 64)))
    (setq citar-no-cache--last-query query)
    (let* ((citar--entries (citar-no-cache-matching-entries buffer query))
           (format (citar-format--parse (citar--get-template 'completion)))
           (hasfilesp (citar-has-files))
           (hasnotesp (citar-has-notes))
           (haslinksp (citar-has-links))
           (hasfilestag (propertize " has:files" 'invisible t))
           (hasnotestag (propertize " has:notes" 'invisible t))
           (haslinkstag (propertize " has:links" 'invisible t))
           (symbolswidth (string-width (citar--symbols-string t t t)))
           (width (- (frame-width) symbolswidth 2)))
      (maphash
       (lambda (citekey entry)
         (let* ((hasfiles (and hasfilesp (funcall hasfilesp citekey)))
                (hasnotes (and hasnotesp (funcall hasnotesp citekey)))
                (haslinks (and haslinksp (funcall haslinksp citekey)))
                (preform (citar-format--preformat format entry t citar-ellipsis))
                (display (citar-format--star-widths
                          (- width (car preform)) (cdr preform)
                          t citar-ellipsis))
                (tagged (if (not (or hasfiles hasnotes haslinks))
                            display
                          (concat display
                                  (when hasfiles hasfilestag)
                                  (when hasnotes hasnotestag)
                                  (when haslinks haslinkstag)))))
           (puthash tagged entry citar-no-cache--completions)))
       citar--entries)))
  citar-no-cache--completions)

(defun citar-no-cache-completion-table (buffer &optional filter &rest metadata)
  "Dyanmic cacheless completion table for citar from BUFFER using FILTER and METADATA."
  (let ((metadata `(metadata . ((category . citar-candidate)
                                . ((affixation-function . ,#'citar--ref-affix)
                                   . ,metadata)))))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          metadata
        (let ((predicate
               (when (or filter predicate)
                 (lambda (_ key)
                   (and (or (null filter) (funcall filter key))
                        (or (null predicate) (funcall predicate string)))))))
          (complete-with-action action
                                (citar-no-cache--format-candidates (minibuffer-contents) buffer)
                                string predicate))))))

(cl-defun citar-no-cache-select-ref ()
  "Select a bibliographic reference."
  (let ((bibs (or (citar--bibliography-files)
                  (user-error "No bibliography set")))
        (buffer (generate-new-buffer " *citar-bibliography*" t))
        (syntax-table (make-char-table 'syntax-table)))
    (modify-syntax-entry ?{ "(}" syntax-table)
    (modify-syntax-entry ?} "){" syntax-table)
    (unwind-protect
        (progn (with-current-buffer buffer
                 (set-syntax-table syntax-table)
                 (dolist (bib bibs)
                   (insert-file-contents-literally bib)))
               (let ((chosen (completing-read "Reference: " (citar-no-cache-completion-table buffer)
                                              nil nil nil 'citar-history citar-presets nil)))
                 (gethash chosen citar-no-cache--completions)))
      (setq citar-no-cache--completions nil)
      (setq citar-no-cache--last-query nil)
      (kill-buffer buffer))))

(provide 'citar-no-cache)
;;; citar-no-cache.el ends here
