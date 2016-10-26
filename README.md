# semantic_sustainability_assessment

!!incomplete!! - repository still under construction

This repository contains the data and basic R code of a study conducted to assess the sustainability content and semantic societal networks in climate action plans of sixteen regional centers in the Federal State of Lower Saxony (Germany).

The background of the repository is, that the goal of the author was primarily to produce results for a scientific article which is currently under submission. The primary goal was not to develop a perfect code. Thus, the code is not optimized and may be advanced in many directions. The documentation and structuration for public use is in process. Please contact me, in case questions should arise before this process is finished.


The workflow is as follows:
0. Check if your documents have a suitable structure for the analysis, i.e. text windows of similar structure that shall be used in the network analysis and that may be separated for example by marker phrases at their beginning and end or in the worst case by introducing suitable marker phrases manually

1. convert pdf files to txt files (see e.g. https://gist.github.com/benmarwick/11333467); depending on the desired output the different output formats of xpdf might be considered (e.g. "", "-table", "-layout")

2. Write the empty structure of the textfile processing code at the file head (it is assumed that documents to be analyzed have different formats and therefore extraction ranges, etc. have to be set manually)

3. Manually define the page ranges containg the potential text windows

