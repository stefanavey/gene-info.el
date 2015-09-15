Provides a way to see gene summary information inside Emacs

Main function is `gene-info-search` which searches the thing at point against NCBI's gene database and returns suggestions for what gene you might mean. It then downloads the summary for that gene as plain text into a new buffer.

TODO:
- Add abiilty to check if `thing-at-point` is already an Entrez Gene ID (if so, don't need first step)
- Could try using Entrez Programming Utlities http://www.ncbi.nlm.nih.gov/books/NBK25501/
