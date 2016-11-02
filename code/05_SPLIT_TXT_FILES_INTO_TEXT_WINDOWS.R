##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##START <<<<<<<<<<<<<<<<<<< SPLIT INTO TEXT WINDOWS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

wd.current <- getwd()
setwd(wd.interim)
##+ APPEND EXTRACTION PATTERNS IN FILEHEAD-------------------------
split.pattern.identified <-  readLines(paste(wd.interim, file.split.pattern.identified, sep=""))

#clean text from comments
split.pattern.identified <- gsub("#.*$", "", split.pattern.identified)
split.pattern.identified <- split.pattern.identified[split.pattern.identified != ""]

split.pattern.identified <- strsplit(split.pattern.identified, "\\|")
split.pattern.identified <- lapply(split.pattern.identified, function(x) {
  gsub("[[:blank:]]+$|^[[:blank:]]+","", x)})


for (i in 1:length(split.pattern.identified )) {#loop through the split.pattern.identified to write them in the fileheads
  
  #check which identified pattern refers to which workingdata.list element
  #and if a suitable element exists at all
  #(e.g. in case the order of documents changed or a additional document are stored in the folder)
  
  split.pattern.case <- eval(parse(text=split.pattern.identified[[i]][[1]]))
  
  pattern.id.in.workingdata.list <- grep(split.pattern.identified[[i]][[2]], names(workingdata.list), fixed = TRUE)
  
  
  if (sum(pattern.id.in.workingdata.list) > 0) { #check if the filename of the stored pattern matches the one in the workingdata.list, if yes, insert extraction pattern
    
    split.pattern.startstring <- workingdata.list[[pattern.id.in.workingdata.list]][[2]][[split.pattern.case[1],1]]
    
    workingdata.list[[pattern.id.in.workingdata.list]][[1]] <- append(workingdata.list[[pattern.id.in.workingdata.list]][[1]], split.pattern.startstring, after = grep("startextract \\|", workingdata.list[[pattern.id.in.workingdata.list]][[1]]))
    
    split.pattern.endstring <- workingdata.list[[pattern.id.in.workingdata.list]][[2]][[split.pattern.case[2],1]]
    
    workingdata.list[[pattern.id.in.workingdata.list]][[1]] <- append(workingdata.list[[pattern.id.in.workingdata.list]][[1]], split.pattern.endstring, after = grep("endextract \\|", workingdata.list[[pattern.id.in.workingdata.list]][[1]]))
    
    
  } else { #in case the filename of the extraction pattern is not matched in the workingdata.list
    print(paste("Not matched in workingdata.list:", split.pattern.identified[[i]][[2]]))
  }
}

##~-------------------------

##+ SPLIT TEXTS INTO TEXT WINDOWS-------------------

for (t in 1:length(workingdata.list)) {
  
  text <- workingdata.list[[t]][[1]]
  
  #read analysis code at filehead
  #it has been changed manually in the texts and is therefore read in again
  processing.code.start <- grep("!Analysis_Code_Start!",text)
  processing.code.end <- grep("!Analysis_Code_End!",text)
  code <- text[processing.code.start:processing.code.end]
  
  text <- text[-c(processing.code.start:processing.code.end)]
  
  #reading in the pages to be analyzed
  startpage <- code[grep("pagerange",code)+1]
  endpage <- code[grep("pagerange",code)+2]
  
  ##+ CHECK FOR MARKER WORDS F~A~L~S~E AND T~R~U~E
  #the words F~A~L~S~E and T~R~U~E are used in the code at some parts instead of the boolean TRUE/FALSE
  #as the latter might in some steps (e.g. by gsub) be converted to character and not fulfill their purpose
  #in order to avoid mistakes in processing the text, it may not contain these words, this is checked
  if (sum(grepl("F~A~L~S~E", text)) > 0) {
    text <- gsub("F~A~L~S~E", "FALSE", text)
    print(paste("The string F~A~L~S~E was replaced by FALSE in: ", names(workingdata.list)[t], sep=""))
  } 
  
  if (sum(grepl("T~R~U~E", text)) > TRUE) {
    text <- gsub("T~R~U~E", "TRUE", text)
    print(paste("The string T~R~U~E was replaced by TRUE in: ", names(workingdata.list)[t], sep=""))
  } 
  ##~
  
  
  ##+ CLEAN TEXT FROM FOOTNOTES; ETC:
  #manually identified single lines
  if (grep("single_line_deletion_end",code)-grep("single_line_deletion_start",code)-1 >= 1) {
    
    delete.single.lines.range <- grep("single_line_deletion_end",code)-grep("single_line_deletion_start",code)-1
    delete.single.lines <- code[grep("single_line_deletion_start",code)+rep(1:delete.single.lines.range)]
    
  } else {
    delete.single.lines <- "F~A~L~S~E"
  }
  ##~
  
  ##+ READ EXTRACTION PATTERN FROM ANALYSIS CODE AT FILE HEAD
  #if single string pattern (FROM X TO X TO X TO END) option is set in the first line of extraction pattern take this into account
  #or otherwise use two (or more) extraction words
  if (grepl("S~I~N~G~L~E~S~T~R~I~N~G~P~A~T~T~E~R~N:", code[(grep("startextract",code)+1)]) == TRUE) { 
    
    split.start <- gsub("S~I~N~G~L~E~S~T~R~I~N~G~P~A~T~T~E~R~N:", "", code[(grep("startextract",code)+1)] )
    split.end <- "F~A~L~S~E"
    
    delete.single.string.switch <- "T~R~U~E"
    
  } else {
    
    #defining the numbers of words and lines for extraction / deletion
    split.range <- grep("endextract",code)-grep("startextract",code)-1
    
    #defining end/start words based on above ranges to be extracted or deleted - stored in vectors
    split.start <- code[grep("startextract",code)+rep(1:split.range)]
    split.end <- code[grep("endextract",code)+rep(1:split.range)]
    
    delete.single.string.switch <- "F~A~L~S~E"
  }
  
  #clean the text from words of the analysis code
  split.start <- gsub("!~~.*~~!", "", split.start) 
  split.end <- gsub("!~~.*~~!", "", split.end)
  delete.single.lines <- gsub("!~~.*~~!", "", delete.single.lines)
  ##~
  
  
  
  ##+ EXTRACT PAGE RANGE TO BE ANALYZED AND SPLIT INTO TEXT WINDOWS
  text.windows <- text[grep(startpage,text):grep(endpage,text)]
  
  #find the lines to START / END extraction on basis of a search string / search cases
  #for checking the output connected to the lines, use this command: text.windows[startlines]
  startlines <- c(sapply(seq(length(split.start)), function(e) {
    
    grep(split.start[e], text.windows)
    
  }))
  
  
  ##+ FIND THE LINES FOR SPLITTING INTO TEXT WINDOWS 
  if (delete.single.string.switch == "T~R~U~E") {
    
    #all rows to be extracted from text.windows
    #set endlines for extraction from one element to the next and last element = last row
    endlines <- startlines
    
    #delete the first entry, hence extraction would start with startlines[1] and end with startlines[2]
    endlines <- endlines[-1]
    #however extraction should be only until stratlines[2]-1 therfore -1 on all rows 
    endlines <- endlines-1 
    
    #as the final element is missing now the end of the text is defined as last split.end line
    endlines <- c(endlines, length(text.windows)) #add the last row as last entry     
    
  } else {
    
    endlines <- c(sapply(seq(length(split.end)), function(e) {
      
      grep(split.end[e], text.windows)
      
    }))
  } #endif
  
  #all rows to be extracted from text.windows
  extractall <- cbind(startlines, endlines)
  ##~
  
  
  ##+ SPLIT INTO TEXT WINDOWS
  #explanation of the code - see deletall block
  
  ##+ CLEAN TEXT FROM ANALYSIS CODE WORDS
  #the extraction lines are fixed now therefore the extraction patterns (as single words) can be cleaned from the text
  #for singleline pattern there is only split.start which can be deleted
  if (delete.single.string.switch == "T~R~U~E") {
    text.windows <- gsub( split.start ,"",text.windows)
  } else {
    text.windows <- gsub( split.start ,"",text.windows)
    text.windows <- gsub( split.end ,"",text.windows)
  }
  ##~
  
  ##+ DO SPLITTING
  #different way of splitting for single string patterns and multiple string patterns
  if (delete.single.string.switch == "T~R~U~E") {
    
    #build a vector with each element including a single text.window - collapsed text from the rows to be extracted
    text.windows.split <- c(sapply(seq(nrow(extractall)), function(x) {
      rows <- (extractall[x,1]:extractall[x,2])
      text.window <- text.windows[rows] #extracted text 
      #for the current analysis it makes sense to collapse the text.window into plain text
      #it might also be stored as a list
      #in case the former structure shall be generated again the wording " ~~collapse~~ " is included at the collaped locations
      
      ##+DELETE FOOTNOTES
      #in case footnotes were defined for deletion
      if (delete.single.lines[1] == "F~A~L~S~E") {  #first if/else
        
        # print(paste("no deletelines, nothing deleted in: ", names(workingdata.list[t])))
        
      } else if  (delete.single.lines[1] != "F~A~L~S~E") { #if there is something to delete, loop through the vector of lines containing the deletion marker string
        
        for (z in 1:length(delete.single.lines)) { #for loop through delete.single.lines
          
          if (sum(c(grep(delete.single.lines[z], text.window))) == 0) {  # second first/ifelse  / in case matching for deletion fails and a integer(o) vector is returned, a safety net is simply not to delete
            
            # if (delete.single.lines[z] != "!~delete_line_~!") { #only print something if the deletline text is not a the dummy text 
            #  print(paste("failed to match deleteline", delete.single.lines[z] , "nothing deleted in: ", names(workingdata.list[t])))
            # }
            
          } else if (sum(c(grep(delete.single.lines[z], text.window))) > 0) {
            text.window <- text.window[-c(grep(delete.single.lines[z], text.window))]
          } #end second if/else
          
        }
        
      } #end first if/else    
      ##~   
      
      paste(text.window, collapse= " ~~collapse~~ ")
      
    }))
    
  } else {
    
    #check if number of start/split.endlines are equal
    if (length(endlines) != length(startlines)) {
      print(paste("ERROR - number of start/endextractlines not equal in", names(workingdata.list[t])))
      stop}
    
    #build a vector with each element including a single text.window - collapsed text from the rows to be extracted
    text.windows.split <- c(sapply(seq(nrow(extractall)), function(x) {
      rows <- (extractall[x,1]:extractall[x,2])
      text.window <- text.windows[rows] #extracted text 
      #for the current analysis it makes sense to collapse the text.window into plain text
      #it might also be stored as a list
      #in case the former structure shall be generated again the wording " ~~collapse~~ " is included at the collaped locations
      
      
      ##+DELETE FOOTNOTES
      if (delete.single.lines[1] == "F~A~L~S~E") {  #first if/else
        
        # print(paste("no deletelines, nothing deleted in: ", names(workingdata.list[t])))
        
      } else if  (delete.single.lines[1] != "F~A~L~S~E") { #if there is something to delete, loop through the vector of lines containing the deletion marker string
        
        for (z in 1:length(delete.single.lines)) { #for loop through delete.single.lines
          
          if (sum(c(grep(delete.single.lines[z], text.window))) == 0) {  # second first/ifelse  / in case matching for deletion fails and a integer(o) vector is returned, a safety net is simply not to delete
            
            # if (delete.single.lines[z] != "!~delete_line_~!") { #only print something if the deletline text is not a the dummy text 
            #  print(paste("failed to match deleteline", delete.single.lines[z] , "nothing deleted in: ", names(workingdata.list[t])))
            # }
            
          } else if (sum(c(grep(delete.single.lines[z], text.window))) > 0) {
            text.window <- text.window[-c(grep(delete.single.lines[z], text.window))]
          } #end second if/else
          
        } #end for loop through delete.single.lines
        
      } #end first if/else    
      ##~
      
      
      paste(text.window, collapse= " ~~collapse~~ ")
      
    }))
  } 
  ##~
  
  text.windows.split <- list(text.windows.split)
  names(text.windows.split) <- paste("text_windows_", names(workingdata.list[t]), sep="")
  workingdata.list[[t]] <- c(workingdata.list[[t]], text.windows.split)    
  
  #in order to save memory and speed up the following calculations the complete text is removed
  workingdata.list[[t]][[1]] <- c("Full text removed in order to free memory and improve calculation performance. Please load again by readLines if the full text is needed.")
  
}
##~-------------------


##+ COMBINE LISTS OF TEXT WINDOWS FOR HANNOVER-----------
#the action plan came in several files which are combined here
files.combine <- grep("GER__Hannover__", names(workingdata.list))

#only do something if there are more than one list entries with the same entity name
if (length(files.combine) > 1) { 
  
  #initial the combined list with the first element of elements to be combined
  case.combine <- workingdata.list[files.combine[1]]
  #get the number of first list levels which serve as loop variable
  num.listelements <- length(case.combine[[1]])
  
  #loop through all elements to combine except the intial one
  cases.add <- files.combine[-c(1)]
  
  for (t in cases.add) {
    
    for (u in 1:num.listelements) {
      
      case.combine[[1]][[u]] <- c(case.combine[[1]][[u]], workingdata.list[[t]][[u]])
      
    }
    
  }
  
  #reassign the names
  names(case.combine) <- "GER__Hannover__combined_files.txt"
  names(case.combine[[1]]) <- c("GER__Hannover__combined_files.txt",
                                "split_pattern_guessed_summary_GER__Hannover__combined_files.txt",
                                "text_windows__GER__Hannover__combined_files.txt")
  
  #add the combined files list and delete the single files
  workingdata.list <- c(workingdata.list,case.combine)
  workingdata.list <- workingdata.list[-c(files.combine)]
}
##~-------------

time.elapsed <- rbind(time.elapsed, proc.time())
row.names(time.elapsed)[nrow(time.elapsed)] <- "SPLIT_INTO_TEXT_WINDOWS"
setwd(wd.current)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##END <<<<<<<<<<<<<<<<<<< SPLIT INTO TEXT WINDOWS
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
