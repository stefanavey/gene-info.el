;;; gene-info.el --- Provides a way to see gene summary information inside Emacs

;; Copyright (C) 2015 Stefan Avey

;; Author: Stefan Avey <stefan.avey@yale.edu>
;; Version: 0.0.1000
;; Package-Requires: ((shr))
;; Keywords: bioinformatics
;; URL: https://github.com/stefanavey/gene-info.el

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a way to see gene summary information inside Emacs

;; TODO: 
;; - Add abiilty to check if `thing-at-point` is already an Entrez Gene ID (if so, don't need first step)

;;; Code:

;; Return all matches to regexp in a string (e.g. that returned by buffer-string) as a list
;; Author: Alan Shutko
;; URL: http://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)
    )
)

;; Author: sds
;; URL: http://emacs.stackexchange.com/questions/8082/how-to-get-buffer-position-given-line-number-and-column-number
(defun pos-at-line-col (l c)
  (save-excursion
    (goto-char (point-min))
    (forward-line l)
    (move-to-column c)
    (point)))

(defun spa-find-gene-info ()
  "If an html file exists in the database for the geneID, open it in view mode inside emacs.
   Otherwise, open a browser to view gene annotation from Gene Cards for the gene symbol near the point"
  (interactive)
  (let (geneName geneID url buffList buffContents allNames buffName)
    (setq geneName (thing-at-point 'word))
    ;; Not a great solution becaus it is redundant but could make this part a function
    (if (and (equal (nth 0 (re-seq "[0-9]+" geneName)) geneName) (> (string-to-number geneName) 0) )
	(lambda ()
	  (setq geneID geneName)
	  (setq url2 (concatenate 'string "http://www.ncbi.nlm.nih.gov/gene/" 
				  geneID "?report=full_report&format=text"))
	  (describe-variable 'geneName)
	  ;; (describe-variable 'url2)))
	  (browse-url-emacs url2) ;(url-encode-url url2)) 
	  (setq buffName (current-buffer))
	  (kill-buffer "*html*")
	  ;; (pop-to-buffer "*html*")
	  ;; (csv-mode)       ; this won't turn off for some reason!
	  ;; (view-mode -1)
	  ;; (read-only-mode -1)
	  (shr-render-buffer buffName)
	  (kill-buffer buffName)
	  (setq buffName (concatenate 'string "*gene-info:" geneID "*"))
	  (if (get-buffer buffName)
	      (kill-buffer buffName))
	  (rename-buffer buffName)
	  (view-mode))
      (setq url (concatenate 'string 
			     "http://www.ncbi.nlm.nih.gov/gene?report=full_report&format=text&term=(" 
			     geneName ") AND human[Organism]" ))
      ;; (setq url (concatenate 'string "http://www.genecards.org/Search/Keyword?queryString=" geneName)) 
      ;; Find the file in my local database of HTML files it exists and open it in View Mode
      ;; other window; otherwise, just open the url in the browser                                     
      (require 'shr)
      
      ;; (url-retrieve-synchronously url)
      ;; (let (tmp (markup (current-buffer)))
      ;; 	(delete-region (point-min) (1+ url-http-end-of-headers))
      ;; 	;; Have to turn off view mode in order to erase buffer if *html* already exists
      ;; 	;; (pop-to-buffer "*html*")
      ;; 	;; (view-mode nil)
      ;; 	(shr-render-buffer markup)
      ;; 	(kill-buffer markup)
      ;; 	(csv-mode)
      ;; 	;; (view-mode)
      ;; 	(setq buffContents (buffer-substring-no-properties (pos-at-line-col 1 0) (point-max)))
      ;; 	;; (describe-variable 'buffContents)
      ;; 	(setq buffList (split-string buffContents "\n" t))))
      
      (url-retrieve       
       url
       (lambda (&optional status cbargs)
	 (let (tmp (markup (current-buffer)))
	   (delete-region (point-min) (1+ url-http-end-of-headers))
	   ;; Have to turn off read-only-mode in order to erase buffer if *html* already exists
	   (pop-to-buffer "*html*")
	   (read-only-mode -1)
	   ;; Renders HTML into buffer named (*html*)
	   ;; If already exists, it is overwritten
	   (shr-render-buffer markup)
	   (kill-buffer markup)
	   (csv-mode)
	   (view-mode)
	   (setq buffContents (buffer-substring-no-properties (pos-at-line-col 1 0) (point-max)))
	   ;; (describe-variable 'buffContents)
	   (setq buffList (split-string buffContents "\n" t)))
	 ;; (describe-variable 'buffList)))
	 (let ( (result ()) )
	   (dolist (line buffList)
	     (setq tmp (split-string line "\t" t))
	     (push (nth 2 tmp) result))
	   (setq result (nreverse result))
	   (setq geneID (ido-completing-read "Ambiguous Search Term, Select GeneID: " result nil t nil nil nil nil))
	   (setq url2 (concatenate 'string "http://www.ncbi.nlm.nih.gov/gene/" 
				   geneID "?report=full_report&format=text")))
	 ;; (describe-variable 'url2)))
	 (browse-url-emacs url2) ;(url-encode-url url2)) 
	 (setq buffName (current-buffer))
	 (kill-buffer "*html*")
	 ;; (pop-to-buffer "*html*")
	 ;; (csv-mode)       ; this won't turn off for some reason!
	 ;; (view-mode -1)
	 ;; (read-only-mode -1)
	 (shr-render-buffer buffName)
	 (kill-buffer buffName)
	 (setq buffName (concatenate 'string "*gene-info:" geneID "*"))
	 (if (get-buffer buffName)
	     (kill-buffer buffName))
	 (rename-buffer buffName)
	 (view-mode)
	 )))
    ))



;; 10
;;   )
;; ;; (browse-url url)
;; ))
;; (global-set-key "\C-cg" 'spa-find-gene-info)
;; Try to be smart about which gene information to show
;; Match this regular expression in the buffer string and return a list


;; (let( (result ()) )
;;      (push "hello" result)
;;      (describe-variable 'result))

;; Using NCBI
;; Put contents of buffer into a list



;; ;; Using Gene Cards
;; (setq temp (re-seq "data-ga-label=\".+\"" (buffer-substring-no-properties (point-min) (point-max))))
;; (setq geneName "IFNB")
;;  (let (url geneName (result  ()))
;;       ;; (describe-variable 'temp)
;;       (dolist ( word temp)
;; 	(push (substring word 15 -1) result))
;;       ;; (describe-variable 'result)
;;       ;; Choose from list using ido
;;       (setq geneName (ido-completing-read "Choose Gene: " result))
;;       (setq url (concatenate 'string "http://www.genecards.org/cgi-bin/carddisp.pl?gene="
;;       			     geneName "&amp;keywords=" geneName))
;;       (describe-variable 'url)
;;       (browse-url-emacs url))


(provide 'gene-info)

;;; gene-info.el ends here



;; IFNB1
