# Semantic Sustainability Assessment via interpretation network analysis applied on Lower Saxionan Climate Action Plans to reveal transition patterns of the German Energy Transition (Energiewende) 

## Introduction
This repository contains the data and basic R code of a study conducted to assess the sustainability content and semantic societal networks in climate action plans of sixteen regional centers in the Federal State of Lower Saxony (Germany) on basis of an semantic network analysis / interpretation network analysis.

In a nutshell, text files are split into text windows and the occurrence and co-occurrence of pre-defined categories in these windows is calculated on basis of categorized words. This serves for assessing the main topics and semantic networks in the text files. The categorization of words has been performed semi-manually. Hence, the code does not yet build on machine learning methods, which might, however, be used in combination with this code, for example, for creating thesauri.

The code may be used for other studies, however, a prerequisite is that the documents to be analyzed have a suitable structure. This means, that they include text windows of similar structure that can be used in a network analysis and that they may be separated, for example, by marker phrases at their beginning and end or in the worst case by introducing suitable marker phrases manually. Further, the wordlists in the thesauri created for the study are in German language and mostly limited to nouns - except the sustainability vocabulary.

It has to be noted, that the purpose of this repository was primarily to produce results for a scientific article which is currently under submission and not to develop a perfect code. Thus, the code is not optimized and may be advanced in many directions. Of course, there are already a lot of good packages for handling texts (see e.g. https://cran.r-project.org/web/views/NaturalLanguageProcessing.html). The code in this repository is not meant to replace or compete with these. It certainly has some overlaps, but is focussing on analyzing a specific data set. Known performance issues of the code concern tokenization and the matching algorithm, that are going to be solved in the next version by using a sparse Matrix appoach and a more efficient search algorithm including ngrams on basis of the pbapply and embed functions. However, beside the performance issues, the graphical representation of network data with the introduced scaling options might be an intereseting example how to analyze this kind of data.

For executing the code a commented meta-code file is available, that guides through the necessary steps and settings. For interested readers, after the README file the meta-code would be the next station:
**META_CODE_AND_INSTRUCTIONS.R**

## Content
Structure and content of this repository are as follows:

### code
R code for separating text files into text windows on basis of repeating marker phrases, counting the occurrence of categorized words and creating networl data, plotting occurrence as box plot and and co-occurrence values as co-occurrence plot (chess pattern) - two different scaling options, variations of the degree centrality, are applied to the co-occurrence plot.

### data
Original pdf documents that were downloaded from the respective homepages of the municipalities or the public municipal information system and the .txt version of these files. The .txt files were manually adapted to allow extracting text windows and generate network data.

One sub-folder contains interim results such as the information on the occurrence of categorized words per document and another the final results (figures and tables).

### encoding
List of symbols in different encodings for harmonization of .txt files to a uniform encoding.

### thesauri wordlists
Collection of general German stopwords and stopwords defined for this study manually. 

The wordlists compiled for this study as thesauri. Wordlists are stored in folders that are named with the categories that the wordlists shall represent.

The nouns that have not been matched in the study and not yet been defined either as stopwords or assigned to categories (a quick manual screening shows that the largest part may be assumed as stopwords and therefore irrelevant for results of the study).



