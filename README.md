# semantic_sustainability_assessment
# repository still under construction! - please contact me in case of any questions
data and main code (except one last plotting code for cooccurrence) are already available, however, 
not diligently documented, yet

## Introduction
This repository contains the data and basic R code of a study conducted to assess the sustainability content and semantic societal networks in climate action plans of sixteen regional centers in the Federal State of Lower Saxony (Germany).

The background of the repository is, that the goal of the author was primarily to produce results for a scientific article which is currently under submission. The primary goal was not to develop a perfect code. Thus, the code is not optimized and may be advanced in many directions. The documentation and structuration for public use is in process. Please contact me, in case questions should arise before this process is finished.

In a nutshell text files are split into text windows and the (co-)occurrence of pre-defined categories in these windows is calculated on basis of categorized words for assessimg the main topics and semantic networks in the text files. The categorization of words has been performed semi-manually.

## Content
Structure and content of this repository are as follows:

### code
R code for separating text files into text windows on basis of repeating marker phrases, counting the occurrence of categorized words and creating networ data, plotting occurrence as box plot and and co-occurrence values as co-occurrence plot (chess pattern) - two different scaling options, variations of the degree centrality, are applied to the co-occurrence plot.

### data
Original pdf documents that were downloaded from the respective homepages of the municipalities or the public municipal information system and the .txt version of these files. The .txt files were manually adapted to allow extracting text windows and generate network data.

One sub-folder contains interim results such as the information on the occurrence of categorized words per document and another the final results mostly in form of figures.

### encoding
List of symbols in different encodings for harmonization of .txt files to a uniform encodig.

### thesauri wordlists
Collection of general German stopwords and stopwords defined for this study manually. 

The wordlists compiled for this study as thesauri. Wordlists are stored in folders that are named with the categories that the wordlists shall represent.

The nouns that have not been matched in the study and not yet been defined either as stopwords or assigned to categories (a quick manual screening shows that the largest part may be assumed as stopwords and therefore irrelevant for results of the study).

## Workflow
0. Check if your documents have a suitable structure for the analysis, i.e. text windows of similar structure that shall be used in the network analysis and that may be separated for example by marker phrases at their beginning and end or in the worst case by introducing suitable marker phrases manually

1. convert pdf files to txt files (see e.g. https://gist.github.com/benmarwick/11333467); depending on the desired output the different output formats of xpdf might be considered (e.g. "", "-table", "-layout")

2. Write the empty structure of the code for setting text processing markers at the file heads. It is assumed that documents to be analyzed have different formats, therefore extraction ranges, etc. might have to be set manually.

3. Manually define the page ranges containing the potential text windows and collect potential (repeating) marker phrases for the beginning and end of the text windows per document. (Might of course be known in advance allowing to skip this manual step.) Write the information into the text processing code at the file head.

4. tbc



