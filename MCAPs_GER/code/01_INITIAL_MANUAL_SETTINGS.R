##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<< MANUAL SETTINGS (DIRECTORIES; FILENAME EXTENSIONS; ETC)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#the following setting are preset for the study of Lower Saxonian climate action plans
#for conduting other studies you might consider changing these lines

##+ OPTIONAL  MANUAL SETTINGS----------------------
#following settings concern (i) filenames, etc.

#storage file for words that have not been matched by the search algorithm
file.words.not.matched <- "words_not_matched.txt"

#empty processing code that is copied at the file head of txt files to be analyzed
#in case the file structure is known (e.g. marker phrases for beginning and end of text windows)
#this code may be adapted accordingly
file.empty.processing.code  <- "code_structure_for_setting_text_processing_markers.txt"

#manually identified marker phrases for splitting into text windows
#and deleting single lines like footnotes, etc.
file.split.pattern.identified <- "identified_extraction_patterns.txt"

#prefix for naming the sublist of the working data list that contains
#the stemmed unique words of the text windows
wordlists.prefix <- c("uniq.stem_")

#final filename extension for the result matrices of single cases
#"MR" should be kept, since an integer will be added after these letters
#that shows a rough estimation of the match rate (#tagged words/#total words)
occurrence.filename.tag <- c("__w_occ_mat__MR")
cooccurrence.filename.tag <- c("__cat_coocc_mat")

#results table for occurrence result
file.occurrence.results <- "results_occurrence_single_cases_and_mean.csv"

#filename beginning for the averaged results over all cases
#cooccurrence.filename.tag, ".csv" will be added at the end
filenamebeginning.cooccurrence.matrix.mean <- "GER__Lower_Saxony_regional_centers_mean"
##~---------------------------------------------

##+ OPTIONAL MANUAL SETTINGS FOR THE STUDY OF LOWER SAXONIAN CLIMATE ACTION PLANS------------
#changing these lines requires knowledge about the categories you expect to be displayed
#hence a preliminary plot should exist, which you then might want to change

#order of energy system categories for plotting
main.categories.energy <- c("<resources><unsp",
                            "(<resources>)(?!<unsp)",
                            "<conversion><unsp", 
                            "(<conversion>)(?!<unsp)",
                            "<distribution><unsp", 
                            "(<distribution>)(?!<unsp)",
                            "<sales_contracts><unsp",
                            "(<sales_contracts>)(?!<unsp)",
                            "<Technology_Option>",
                            "<Energy_Form>",
                            "<end_use><consumption>",
                            "<mobility>(?!<frei)",
                            "<mobility><frei",
                            "<building><unsp",
                            "(<building>)(?!<unsp)",
                            "<electric_application>",
                            "<local_administration_bodies>",
                            "<Mobility_Sector>",
                            "<Infrastructure>",
                            "<Residents>",
                            "<Food>",
                            "<Economy><unspec",
                            "<Economy><service>",
                            "<commerce>",
                            "<industry><unsp",
                            "(<industry>)(?!<unsp)"
)

#in graphical results draw separation line at border of following (meta)categories
main.categories.energy.reduced <- c( "resources",         
                                     
                                     "<conversion>", 
                                     "<distribution>", 
                                     "<sales_contracts>",
                                     "<Technology_Option>",
                                     "<Energy_Form>", 
                                     "<end_use><consumption>",
                                     "<mobility>",
                                     "<building>",
                                     "<electric_application>",
                                     "<local_administration_bodies>", 
                                     "<Mobility_Sector>",
                                     "<Infrastructure>",
                                     
                                     "<Residents>",
                                     "<Food>",
                                     "<Economy>"
                                     
)
##~----------------------------------------

##+ OPTIONAL SETTINGS ENCODING FILES (GENERALLYY NO ADAPTION REQUIRED)---------------------
#the code assumes a specific structure in the following files
#that contain tables of equivalent characters in different encodings
#only if you want to add files or REALLY want different filenames you might consider
#changing the following lines

#encoding tables and files to be loaded
file.encoding.table.win <- "tm_win1252_misinterpretation_encoding_table_UTF8.txt"
file.encoding.punctuation  <- "punct_meta_ANSI.txt"
##~-------------------------------

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<< MANUAL SETTINGS (DIRECTORIES; FILENAME EXTENSIONS; ETC)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


