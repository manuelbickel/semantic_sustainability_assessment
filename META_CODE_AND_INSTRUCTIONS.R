#****************** INTRODUCTORY COMMENTS ON THE CODE  ****************************************************--------

#This meta-code serves to execute code files step by step or all at once that are stored in the code/ folder.
#The code files are stored in ISO-8859-1 encoding. Please keep this encoding since the code files 
#contain some characters which require this encoding to be processed; otherwise parts of the code might not work.
#Please contact me, if you encounter difficulties concerning encoding of files.

#This meta code assumes a semi-automatic approach and has to be run stepwise if other documents than the ones for
#the study of municipal climate action plans of Lower Saxony shall be investigated. Further, it assumes GERMAN language
#files. A switch to English is of course possible but not implemented, yet. (-> translation required)
#For this study the code and data are prepared already and all code files in this meta code
#can be called without any manual action, despite setting the main working directory in the following.
#For each step that loads a specific code file (see below), a short explanation is given about the code being executed.
#".../semantic_sustainability_assessment/"
wd.main <- "M:/Science/Programmierung/Github/semantic_sustainability_assessment/"
setwd(wd.main)
#optionally you might also increase/decrease the amount currently executed code stored in the code files
#number of characters output for the deparse of a single expression:
deparse.length <- 10^6

#STEPWISE EXECUTION<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#HOWEVER, stepwise execution including manual entries is required for other data sets.
#The required manual actions are included as print("text....") in below lines of code.


#NOTES ON STRUCTURE OF WORKING DATA<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#the internal R workingdata.list structure which is used to handle processed text content
#is as follows
#(check e.g. str(workingdata.list) or text <- workingdata.list[[24]][[2]] after creating the list)
#name
#[[CASE]]                     name of a single case, here the document name
#[[CASE]][[1-FULL_TEXT]]      Dummy sentence (may contain the full content of the .txt file being processes; for saving memory in this script the full content is deleted) // character
#[[CASE]][[2-SPLIT_PATTERN]]  shows the occurrence of guessed split phrases for text windows in the text
#[[CASE]][[3-WINDOWS]]        separated text windows
#[[CASE]][[4-WINDOWS_WORDS]]  unique words in the single text windows


#NOTE ON THE WAY OF CODE COMMENTING<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#in general the following syntax is used for comments (please excuse some incosistencies)

# : short comments...

##+ DESCRIPTION TEXT : marks beginning of a code block
##~ : marks the end of a code block

##+ DESCRIPTION TEXT -------------- : marks the end of a long code block (may be collapsed, e.g. in R Studio)
##~-------------- : marks the end of a long code block (may be collapsed, e.g. in R Studio)

##START <<<<<<<<<<<<< DESCRITPTION TEXT : marks the beginning of a code file
##END <<<<<<<<<<<<< DESCRITPTION TEXT   : marks the end of a code file

#variables are named in lowercase letters, separating symbol is a dot, e.g.: variable.new
#functions start with an uppercase letter, most initial characters of words do also, e.g.: FunctionToDoSomething()

#ADDITIONAL NOTES<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#warnings concerning incomplete final lines may be neglected

#******************************************************************************************---------------


##+ INITIAL TIME COUNTER TO ASSESS WORKING/COMPUTING TIME------------
time.elapsed <- rbind(c(), proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "start"
##~----------------------------------


##+ 01 LOAD OPTIONAL MANUAL SETTINGS ------------------------------
print("Please adapt the file 01_INITIAL_MANUAL_SETTINGS.R in the code/ folder 
      if you want to customize filenames, etc.
      Further, you need to convert your documents into txt files
      and store them in the folder data/text_files.")

source(paste0(wd.main, "code/","01_INITIAL_MANUAL_SETTINGS.R"), 
       echo=T, max.deparse.length = deparse.length)
##~----------------------------------------------------------------------


##+ 02 LOAD DEFINITIONS AND FUNCTIONS -------------------------------------
source(paste0(wd.main, "code/", "02_LOAD_DEFINITIONS_OPTIONS_FUNCTIONS.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 03_ADD_TEXT_PROCESSING_CODE_STRUCTURE_AT_FILEHEADS -------------------------------------
source(paste0(wd.code, "03_ADD_TEXT_PROCESSING_CODE_STRUCTURE_AT_FILEHEADS.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------

##+ MANUAL SETTING OF PAGE RANGES AND POTENTIAL MARKER PHRASES FOR TEXT WINDOWS------------
print("Please check text files for potential repeating phrases that mark the beginning and end of desired text windows.
      Insert these guessed phrases as single lines at each file head in the text processing code between the lines desired_pattern_start and desired_pattern_end.
      The next source code file will check the number of occurrences of these phrases to define suitable
      start/end phrases of text windows. To be clear, it is assumed that each text window in a single file
      starts and ends with the same phrase. 

      If execution of source code 04 below shows that there are no
      suitable repeating phrases, these have to be inserted manually to mark the text windows.
      You might consider inserting phrases such as
      S~T~A~R~T~E~X~T~R~A~C~T~I~O~N
      E~N~D~E~X~T~R~A~C~T~I~O~N
      between the text windows and in the procsessing code at the file heads.
      
      The code also assumes that the text range that contains the text windows is marked via:
      ~PAGE~RANGE~START~
      ~PAGE~RANGE~END~
      These phrases have to be copied at the respective positions of the text files. This is done
      to make definition of splitting patterns easier due a lower amount of text to be split.
      
      Instead of marking the page ranges containing the text windows the full text might be used.
      Then a suitable expression with eval(parse(text=...)) could be introduced.")
##~------------------


##+ LOAD_TXT_FILES_AND_CHECK_TEXT_WINDOW_MARKER_PHRASES -------------------------------------
source(paste0(wd.code, "04_LOAD_TXT_FILES_AND_CHECK_TEXT_WINDOW_MARKER_PHRASES.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------

##+ MANUAL IDENTIFICATION OF SUITABLE SPLITTING PHRASES-------------------------------
#before running 05_SPLIT_TXT_FILES_INTO_TEXT_WINDOWS.R
#the occurrence of the potential repeating marker words to define text windows have to be checked
#It has to be decided whether there are suitable pairs of marker phrases.
#in the positive case these marker phrases have to be documented manually in the file
#wd.interim/identified_extraction_patterns.txt
##----------------------------------------

##+ SPLIT_TXT_FILES_INTO_TEXT_WINDOWS -------------------------------------
source(paste0(wd.code, "05_SPLIT_TXT_FILES_INTO_TEXT_WINDOWS.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ PREPARE_THESAURI_AND_STOPWORDS -------------------------------------
#additional stopwordlists or folders containing txt files with words (folder = category name of all words)
#might be introduced
source(paste0(wd.code, "06_PREPARE_THESAURI_AND_STOPWORDS.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ CLEAN_AND_STEM_TEXT_WINDOWS -------------------------------------
source(paste0(wd.code, "07_CLEAN_AND_STEM_TEXT_WINDOWS.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------

##+ 08_COUNT_OCCURRENCE_IN_TEXT_WINDOWS -------------------------------------
source(paste0(wd.code, "08_COUNT_OCCURRENCE_IN_TEXT_WINDOWS.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 09_CALCULATE_MATCH_RATE_TABLE -------------------------------------
source(paste0(wd.code, "09_CALCULATE_MATCH_RATE_TABLE.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 10_GENERATE_FINAL_OCCURRENCE_RESULTS_TABLE -------------------------------------
source(paste0(wd.code, "10_GENERATE_FINAL_OCCURRENCE_RESULTS_TABLE.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 11_GENERATE_NORMALIZED_COOCCURRENCE_MATRICES_AND_GET_TOP_MATCHED_WORDS -------------------------------------
source(paste0(wd.code, "11_GENERATE_NORMALIZED_COOCCURRENCE_MATRICES_AND_GET_TOP_MATCHED_WORDS.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 12_GENERATE_AVERAGE_COOCCURRENCE_MATRIX -------------------------------------
source(paste0(wd.code, "12_GENERATE_AVERAGE_COOCCURRENCE_MATRIX.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 13_PREPARE_OCCURRENCE_RESULTS_FOR_PLOTTING -------------------------------------
source(paste0(wd.code, "13_PREPARE_OCCURRENCE_RESULTS_FOR_PLOTTING.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 14_BOXPLOT_OCCURRENCE_ENERGY-------------------------------------
source(paste0(wd.code, "14_BOXPLOT_OCCURRENCE_ENERGY.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 15_BOXPLOT_OCCURRENCE_SOCIAL-------------------------------------
source(paste0(wd.code, "15_BOXPLOT_OCCURRENCE_SOCIAL.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 16_BOXPLOT_OCCURRENCE_SUSTAINABILITY -------------------------------------
source(paste0(wd.code, "16_BOXPLOT_OCCURRENCE_SUSTAINABILITY.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 17_PREPARE_COOCCURRENCE_RESULTS_FOR_PLOTTING -------------------------------------
source(paste0(wd.code, "17_PREPARE_COOCCURRENCE_RESULTS_FOR_PLOTTING.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 18_COOCCURRENCE_PLOT_ENERGY_AND_SOCIAL_SYSTEM -------------------------------------
source(paste0(wd.code, "18_COOCCURRENCE_PLOT_ENERGY_AND_SOCIAL_SYSTEM.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 19_COOCCURRENCE_ENERGY_SYSTEM_AND_SUSTAINABILITY -------------------------------------
source(paste0(wd.code, "19_COOCCURRENCE_ENERGY_SYSTEM_AND_SUSTAINABILITY.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


##+ 20_COOCCURRENCE_ALL_CATEGORIES_NO_SCALING_OR_CLASSIFICATION -------------------------------------
source(paste0(wd.code, "20_COOCCURRENCE_ALL_CATEGORIES_NO_SCALING_OR_CLASSIFICATION.R"), 
       echo=T, max.deparse.length = deparse.length)
##~--------------------------------------------------------------------


