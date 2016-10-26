# semantic_sustainability_assessment
# !!incomplete - repository still under construction!!

## Introduction
This repository contains the data and basic R code of a study conducted to assess the sustainability content and semantic societal networks in climate action plans of sixteen regional centers in the Federal State of Lower Saxony (Germany).

The background of the repository is, that the goal of the author was primarily to produce results for a scientific article which is currently under submission. The primary goal was not to develop a perfect code. Thus, the code is not optimized and may be advanced in many directions. The documentation and structuration for public use is in process. Please contact me, in case questions should arise before this process is finished.

## Workflow
0. Check if your documents have a suitable structure for the analysis, i.e. text windows of similar structure that shall be used in the network analysis and that may be separated for example by marker phrases at their beginning and end or in the worst case by introducing suitable marker phrases manually

1. convert pdf files to txt files (see e.g. https://gist.github.com/benmarwick/11333467); depending on the desired output the different output formats of xpdf might be considered (e.g. "", "-table", "-layout")

2. Write the empty structure of the code for setting text processing markers at the file heads. It is assumed that documents to be analyzed have different formats, therefore extraction ranges, etc. might have to be set manually.

3. Manually define the page ranges containing the potential text windows and collect potential (repeating) marker phrases for the beginning and end of the text windows per document. (Might of course be known in advance allowing to skip this manual step.) Write the information into the text processing code at the file head.

4. tbc



