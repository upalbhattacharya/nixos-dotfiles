;; Copyright © 2022  Yann Herklotz
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the “Software”), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

(defun acm-pdf-url (doi)
  "Retrieve a DOI pdf from the ACM."
  (concat "https://dl.acm.org/doi/pdf/" doi))

(defun ieee-pdf-url (doi)
  "Retrieve a DOI pdf from the IEEE."
  (when (string-match "\\.\\([0-9]*\\)$" doi)
    (let ((doi-bit (match-string 1 doi)))
      (concat "https://ieeexplore.ieee.org/stampPDF/getPDF.jsp?tp=&arnumber=" doi-bit "&ref="))))

(defun springer-pdf-url (doi)
  "Retrieve a DOI pdf from the Springer."
  (concat "https://link.springer.com/content/pdf/" doi ".pdf"))

(defun arxiv-pdf-url (eprint)
  "Download an arXiv pdf based on it's EPRINT number."
  (concat "https://arxiv.org/pdf/" eprint ".pdf"))

(defun download-pdf-from-doi (key &optional doi publisher eprint journal organization url)
  "Download pdf from DOI with KEY name."
  (let ((pub  (or publisher ""))
        (epr  (or eprint ""))
        (jour (or journal ""))
        (org  (or organization ""))
        (link (or url "")))
    (url-copy-file (cond
                    ((not doi) link)
                    ((or (string-match "ACM" (s-upcase pub))
                         (string-match "association for computing machinery" (s-downcase pub)))
                     (acm-pdf-url doi))
                    ((string-match "arxiv" (s-downcase pub))
                     (arxiv-pdf-url epr))
                    ((or (string-match "IEEE" (s-upcase pub))
                         (string-match "IEEE" (s-upcase jour))
                         (string-match "IEEE" (s-upcase org)))
                     (ieee-pdf-url doi))
                    ((string-match "springer" (s-downcase pub))
                     (springer-pdf-url doi))
                    (t (error "Cannot possibly find the PDF any other way")))
                   (concat (car ebib-file-search-dirs) "/" key ".pdf"))))

(defun download-pdf-from-link (link key)
  (url-copy-file link
                 (concat (car ebib-file-search-dirs) "/" key ".pdf")))

(defun download-pdf-from-downloads (key)
  (copy-file (concat "~/Downloads/" key ".pdf")
             (concat (car ebib-file-search-dirs) "/" key ".pdf") t))

(defun get-bib-from-doi (doi)
  "Get the bibtex from DOI."
  (shell-command (concat "curl -L -H \"Accept: application/x-bibtex; charset=utf-8\" "
                         "https://doi.org/" doi)))

(defun ebib-download-pdf-from-doi ()
  "Download a PDF for the current entry."
  (interactive)
  (let* ((key (ebib--get-key-at-point))
         (doi (ebib-get-field-value "doi" key ebib--cur-db 'noerror 'unbraced 'xref))
         (publisher (ebib-get-field-value "publisher" key ebib--cur-db 'noerror 'unbraced 'xref))
         (eprinttype (ebib-get-field-value "eprinttype" key ebib--cur-db 'noerror 'unbraced 'xref))
         (eprint (ebib-get-field-value "eprint" key ebib--cur-db 'noerror 'unbraced 'xref))
         (journal (ebib-get-field-value "journal" key ebib--cur-db 'noerror 'unbraced 'xref))
         (journaltitle (ebib-get-field-value "journaltitle" key ebib--cur-db 'noerror 'unbraced 'xref))
         (organization (ebib-get-field-value "organization" key ebib--cur-db 'noerror 'unbraced 'xref))
         (url (ebib-get-field-value "url" key ebib--cur-db 'noerror 'unbraced 'xref)))
    (unless key
      (error "[Ebib] No key assigned to entry"))
    (download-pdf-from-doi key doi (or publisher eprinttype) eprint (or journal journaltitle) organization url)))

(defun ebib-check-file ()
  "Download a PDF for the current entry."
  (interactive)
  (let ((key (ebib--get-key-at-point)))
    (unless (file-exists-p (concat (car ebib-file-search-dirs) "/" key ".pdf"))
      (error "[Ebib] No PDF found"))))
