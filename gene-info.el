;;; gene-info.el --- Provides a way to see gene summary information inside Emacs

;; Copyright (C) 2015 Stefan Avey

;; Author: Stefan Avey <stefan.avey@yale.edu>
;; Version: 0.0.1001
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

;; Main function is `gene-info-search` which searches the thing at point against NCBI's gene database and returns suggestions for what gene you might mean. It then downloads the summary for that gene as plain text into a new buffer.

;; TODO: 
;; - Add abiilty to check if `thing-at-point` is already an Entrez Gene ID (if so, don't need first step)
;; - Could try using Entrez Programming Utlities http://www.ncbi.nlm.nih.gov/books/NBK25501/
 

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

(defun gene-info-search (&optional organism)
  "If the `thing-at-point` is a positive integer, search NCBI for gene annotations using the `thing-at-point` as the Entrez Gene ID. Otherwise, search NCBI genes with keyword given by `thing-at-point` and return a buffer with possible completions and prompt for correct gene ID.  In both cases, a new buffer named *gene-info:<ID>* (where <ID> is the Entrez Gene ID) is created and the full report on the gene is displayed.

Organism specifies what organism to search for genes in. This defaults to 'human' if no value is supplied
"
  (interactive)
  (if organism
      nil
    (defvar organism "human")
    )
  (defvar geneName (thing-at-point 'word))
  (let (geneID url buffList buffContents allNames buffName)
    ;; Not a great solution because it is redundant but could make this part a function to reduce redundancy
    ;; (if (string-match "\\`[1-9][0-9]*\\'" geneName) ; test whether geneName is a positive integer
        ;; (and (equal (nth 0 (re-seq "[0-9]+" geneName)) geneName) (> (string-to-number geneName) 0) )
    	;; (lambda ()
    	  ;; (setq geneID geneName)
    	  ;; (setq url2 (concatenate 'string "http://www.ncbi.nlm.nih.gov/gene/" 
    	  ;; 			  geneID "?report=full_report&format=text"))
    	  ;; (describe-variable 'geneID)
    	  ;; ;; (describe-variable 'url2)))
    	  ;; (browse-url-emacs url2) ;(url-encode-url url2)) 
    	  ;; (setq buffName (current-buffer))
    	  ;; (kill-buffer "*html*")
    	  ;; ;; (pop-to-buffer "*html*")
    	  ;; ;; (csv-mode)       ; this won't turn off for some reason!
    	  ;; ;; (view-mode -1)
    	  ;; ;; (read-only-mode -1)
    	  ;; (shr-render-buffer buffName)
    	  ;; (kill-buffer buffName)
    	  ;; (setq buffName (concatenate 'string "*gene-info:" geneID "*"))
    	  ;; (if (get-buffer buffName)
    	  ;;     (kill-buffer buffName))
    	  ;; (rename-buffer buffName)
    	  ;; (view-mode))
	;; )
      (setq url (concatenate 'string 
			     "http://www.ncbi.nlm.nih.gov/gene?report=tabular&format=text&term=(" 
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
	 (let ( (result ()) prompt)
	   (dolist (line buffList)
	     (setq tmp (split-string line "\t" t))
	     (push (nth 2 tmp) result))
	   (setq result (nreverse result))
	   (setq prompt (concatenate 'string "Ambiguous Search Term '" geneName "', Select GeneID: "))
	   (setq geneID (ido-completing-read prompt result nil t nil nil nil nil))
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
	 (makunbound 'geneName)
	 (makunbound 'organism)
	 )
       ))
    ;; )
)
(global-set-key "\C-cg" 'gene-info-search-test)

;; ;; Using Gene Cards instead of NCBI
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

(defun gene-info-search-test (&optional organism)
  "Debugging"
  (interactive)
  ;; (if organism
  ;;     nil
  ;;   (defvar organism "human")
  ;;   )
  (defvar geneName (thing-at-point 'word))
  (let (geneID url buffList buffContents allNames buffName)
    ;; Not a great solution becaus it is redundant but could make this part a function
    (if (string-match "\\`[1-9][0-9]*\\'" geneName) ; test whether geneName is a positive integer
	(lambda ()
    	  (setq geneID geneName)
    	  (setq url2 (concatenate 'string "http://www.ncbi.nlm.nih.gov/gene/" 
    	  			  geneID "?report=full_report&format=text"))
    	  ;; (describe-variable 'geneID)
    	  ;; (describe-variable 'url2)))
	  (message url2)
    	  ;; (browse-url-emacs url2) ;(url-encode-url url2)) 
    	  ;; (setq buffName (current-buffer))
    	  ;; (kill-buffer "*html*")
    	  ;; ;; (pop-to-buffer "*html*")
    	  ;; ;; (csv-mode)       ; this won't turn off for some reason!
    	  ;; ;; (view-mode -1)
    	  ;; ;; (read-only-mode -1)
    	  ;; (shr-render-buffer buffName)
    	  ;; (kill-buffer buffName)
    	  ;; (setq buffName (concatenate 'string "*gene-info:" geneID "*"))
    	  ;; (if (get-buffer buffName)
    	  ;;     (kill-buffer buffName))
    	  ;; (rename-buffer buffName)
    	  ;; (view-mode))
	  )
      nil
      )
        ;; (and (equal (nth 0 (re-seq "[0-9]+" geneName)) geneName) (> (string-to-number geneName) 0) )
))
(global-set-key "\C-cd" 'gene-info-search-test)

(provide 'gene-info)

;;; gene-info.el ends here
