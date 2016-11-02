

######  README  **************************************************************************************************

##code files are stored in ISO-8859-1 encoding
#please keep this encoding since the code files contain some characters which require
#this encoding to be processed; otherwise parts of the code might not work
#please contact me, if you encounter difficulties concerning encoding of files

#This code assumes a semi-automatic approach and has to be run stepwise if other documents than the ones for
#the study of municipal climate action plans of Lower Saxony shall be investigated
#for this study the code and data are prepared already and the code can be run entirely.
#the manual action that would be required for other data sets is described in short below

#MANUAL ACTION
#Manual Action 1
#the documents to be analyzed have to be converted to text files (via tesseract and xpdf) 
#and have to be manipulated by including a predefined analysis code at the file head (see separate R code)
#this analysis code has to be adapted manually according to content of the txt file (semi-automatic approach)
#assumptions for filling the code:
#page range to be analyzed has to be set (by copying the start marker/ end marker words (might be introduced manually into the text) at the respective location of the text into the code)
#single repeating lines containing a certain string (e.g. footnotes) can be deleted (if desired)
#page breaks are marked via: \f (via xpdf)
#range to be analyzed should be defined in a way so that it inlcudes an \f in the beginning and at the end

#Manual Action  2
#before running the section "##+ IDENTIFY EXTRACTION PATTERNS"
#potential repeating marker words to define text windows (beginning and end phrase) have to be guessed
#these are then tested in that section automatically
#the user will then have to decide if the marker phrases are suitable or not
#in the positive case these marker phrases have to be documented manually in the file
#wd.interim/identified_extraction_patterns.txt

#Manual Action 3
#in a separate step which may be supported by computer programs the nouns (or other words) of the
#text windows have to be tagged; also stopwords may be defined
#these word lists have to be stored in the respective folders to be read into R



#FURTHER NOTES

#the internal R workingdata.list structure which is used to handle the files is as follows
#(check e.g. str(workingdata.list) or text <- workingdata.list[[24]][[2]] after creating the list)
#name
#[[ENT]]             list ENTity (=document name)
#[[ENT]][[1-WT]]     Dummy sentence (before: Whole Text of the document including code at filehead)
#[[ENT]][[2-AEXP]]   Available EXtraction Pattern in the complete text, matrix
#[[ENT]][[3-MEAS]]   separated single measures (including linebreaks marked with "~~collapse~~")
#[[ENT]][[4-UnWinM]] Unique Words in the single Measures, words which exist in the available wordlists have an E~X~T at the beginning


#SYNTAX NOTES
# : comments...

##+ DESCRIPTION TEXT : marks beginning of a short code block
##~ : marks the end of a short code block

##+ DESCRIPTION TEXT -------------- : marks the end of a longer code block (may be collapsed, e.g. in R Studio)
##~-------------- : marks the end of a longer code block (may be collapsed, e.g. in R Studio)

##START <<<<<<<<<<<<< description text  : marks the beginning of a very large code block
##END <<<<<<<<<<<<<   : marks the end of a very large code block

#variables are named in lowercase letters only, separating symbol is a dot, e.g. variable.new
#Functions start with an uppercase letter, separating symboal is a hyphen, e.g. Function_new()

#ADDITONAL NOTES
#warnings concerning incomplete final lines may be neglected
#**************************************************************************************************





##+ OBLIGATORY MANUAL SETTINGS-----------------------------------------
#set main working directory: ".../semantic_sustainability_assessment/"
wd.main <- "M:/Science/Programmierung/Github/semantic_sustainability_assessment/"
setwd(wd.main)

#optional, if more or less processed source code shall be displayed:
deparse.length <- 10^6
##~-------------------------------------------------------------------

##+ INITIAL TIME COUNTER TO ASSESS COMPUTING TIME------------
time.elapsed <- rbind(c(), proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "start"
##~----------------------------------

##+ LOAD OPTIONAL MANUAL SETTINGS ------------------------------
print("Please adapt the file 01_INITIAL_MANUAL_SETTINGS.R in the code/ folder if you want to customize filenames, etc. This is optional, the file contains preset values.")

#load manual settings
source(paste0(wd.main, "code/","01_INITIAL_MANUAL_SETTINGS.R"), 
       echo=T, max.deparse.length = deparse.length)
##~----------------------------------------------------------------------

##+ LOAD DEFINITIONS AND FUNCTIONS -------------------------------------
source(paste0(wd.main, "code/", "02_LOAD_DEFINITIONS_OPTIONS_FUNCTIONS.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------


##+ LOAD DEFINITIONS AND FUNCTIONS -------------------------------------
source(paste0(wd.code, "03_ADD_TEXT_PROCESSING_CODE_STRUCTURE_AT_FILEHEADS.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
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
      between the text windows and in the procsessing code at the file heads.")

##~------------------

##+ LOAD_TXT_FILES_AND_CHECK_TEXT_WINDOW_MARKER_PHRASES -------------------------------------
source(paste0(wd.code, "04_LOAD_TXT_FILES_AND_CHECK_TEXT_WINDOW_MARKER_PHRASES.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------

##+ SPLIT_TXT_FILES_INTO_TEXT_WINDOWS -------------------------------------
source(paste0(wd.code, "05_SPLIT_TXT_FILES_INTO_TEXT_WINDOWS.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------


##+ PREPARE_THESAURI_AND_STOPWORDS -------------------------------------
source(paste0(wd.code, "06_PREPARE_THESAURI_AND_STOPWORDS.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------


##+ CLEAN_AND_STEM_TEXT_WINDOWS -------------------------------------
source(paste0(wd.code, "07_CLEAN_AND_STEM_TEXT_WINDOWS.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------

##+ 08_COUNT_OCCURRENCE_IN_TEXT_WINDOWS -------------------------------------
source(paste0(wd.code, "08_COUNT_OCCURRENCE_IN_TEXT_WINDOWS.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------

##+ 09_CALCULATE_MATCH_RATE_TABLE -------------------------------------
source(paste0(wd.code, "09_CALCULATE_MATCH_RATE_TABLE.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------

##+ 10_GENERATE_FINAL_OCCURRENCE_RESULTS_TABLE -------------------------------------
source(paste0(wd.code, "10_GENERATE_FINAL_OCCURRENCE_RESULTS_TABLE.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------



##+ 11_GENERATE_NORMALIZED_COOCCURRENCE_MATRICES_AND_GET_TOP_MATCHED_WORDS -------------------------------------
source(paste0(wd.code, "11_GENERATE_NORMALIZED_COOCCURRENCE_MATRICES_AND_GET_TOP_MATCHED_WORDS.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------

##+ 12_GENERATE_AVERAGE_COOCCURRENCE_MATRIX -------------------------------------
source(paste0(wd.code, "12_GENERATE_AVERAGE_COOCCURRENCE_MATRIX.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------


##+ 13_PREPARE_OCCURRENCE_RESULTS_FOR_PLOTTING -------------------------------------
source(paste0(wd.code, "13_PREPARE_OCCURRENCE_RESULTS_FOR_PLOTTING.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------

##+ 14_BOXPLOT_OCCURRENCE_ENERGY-------------------------------------
source(paste0(wd.code, "14_BOXPLOT_OCCURRENCE_ENERGY.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------


##+ 15_BOXPLOT_OCCURRENCE_SOCIAL-------------------------------------
source(paste0(wd.code, "15_BOXPLOT_OCCURRENCE_SOCIAL.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------

##+ 16_BOXPLOT_OCCURRENCE_SUSTAINABILITY -------------------------------------
source(paste0(wd.code, "16_BOXPLOT_OCCURRENCE_SUSTAINABILITY.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------

##+ 17_PREPARE_COOCCURRENCE_RESULTS_FOR_PLOTTING -------------------------------------
source(paste0(wd.code, "17_PREPARE_COOCCURRENCE_RESULTS_FOR_PLOTTING.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------


##+ 18_COOCCURRENCE_PLOT_ENERGY_AND_SOCIAL_SYSTEM -------------------------------------
source(paste0(wd.code, "18_COOCCURRENCE_PLOT_ENERGY_AND_SOCIAL_SYSTEM.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------


##+ 19_COOCCURRENCE_ENERGY_SYSTEM_AND_SUSTAINABILITY -------------------------------------
source(paste0(wd.code, "19_COOCCURRENCE_ENERGY_SYSTEM_AND_SUSTAINABILITY.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------


##+ 20_COOCCURRENCE_ALL_CATEGORIES_NO_SCALING_OR_CLASSIFICATION -------------------------------------
source(paste0(wd.code, "20_COOCCURRENCE_ALL_CATEGORIES_NO_SCALING_OR_CLASSIFICATION.R"), 
       echo=T, max.deparse.length = deparse.length)

#computing time to this step
time.elapsed
##~--------------------------------------------------------------------


