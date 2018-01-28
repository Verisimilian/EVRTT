require(rentrez)
library(rentrez)
library(xml2)
library(rvest)
library(stringr)

findLocation <- function(pubIDs){
  #entry / exit function!!!
  authorInfos <- getAuthInfo(pubIDs) #retrieves author information
  
  allMatchesTemp <- matrix(nrow=1)
  allMatchesTemp2 <- matrix(nrow=1)
  #attempts to grep locations using complex regex | see regexr.com <<<<<
  for(i in authorInfos){
    location <- str_match(string = i, pattern = "([A-Z])(?![A-Z])\\w+[\\s]?(\\w+)\\,[\\s][A-Z]\\w+")
    pattern = "([A-Z])(?![A-Z])\\w+[\\s]?(\\w+)\\,[\\s][A-Z]\\w+"
    matches <- getAllMatches(pattern, location)
    
    allMatchesTemp <- c(allMatchesTemp, matches)
  }
  
  for(i in authorInfos){
    location <- str_match(string = i, pattern = "[A-Z]{1}[a-z]+\\,\\s([A-Z]{2})")
    pattern = "[A-Z]{1}[a-z]+\\,\\s([A-Z]{2})"
    matches2 <- getAllMatches(pattern, location)
    
    allMatchesTemp2 <- c(allMatchesTemp2, matches2)
  }
  
  allMatchesFinal <- c(allMatchesTemp[-c(1)],allMatchesTemp2[-c(1)])
  allMatchesFinalReturned <- unique(allMatchesFinal)
  return(allMatchesFinalReturned)
  
  
}


getAllMatches <- function(regPattern, stringyboi){
  #initial vars
  tempResults <- matrix(nrow = 1)
  finalResults <- matrix(nrow = 1)
  currentMatch <- "."
  stringyboi2 <- stringyboi
  #find all matches in the string
  counter <- 1
  while(is.character(currentMatch) && length(currentMatch) > 0){
    currentMatch <- regmatches(stringyboi, 
                               regexpr(pattern = regPattern, 
                                       stringyboi, 
                                       perl = T))
    tempResults <- c(tempResults, currentMatch)#add match to tempResults
    counter <- counter + 1
    stringyboi <- gsub(pattern = currentMatch[1], replacement = "", x = stringyboi)#remove found match
  }
  #sanitize tempResults by removing univerities and other accidental matches (NOT PERFECT)
  for(i in tempResults){
    if(!(grepl(pattern = "University", x = i) || grepl(pattern = "of", x = i))&& !is.na(i)){
      finalResults <- c(finalResults, i)
    }
  }
  returnedResults <- finalResults[-c(1)]
  return(returnedResults)
}

getAuthInfo <- function(pubIDs){
  authorInfosTemp <- matrix(nrow= 1)
  for(i in pubIDs){
    htmlRead = read_html((paste0("https://www.ncbi.nlm.nih.gov/pubmed/",i)))#downloads html of pubmed page
    authorInfo <- as.character(xml_find_all(htmlRead, ".//dd")[1]) #finds author information
    #sanitize location string before grepping
    badChars = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                        'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                        'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                        'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                        'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
    
    authorInfo <- chartr(paste(names(badChars), collapse=''),
           paste(badChars, collapse=''),
           authorInfo)
    
    authorInfosTemp <- c(authorInfosTemp, authorInfo)
    
  }
  authorInfosReturned <- authorInfosTemp[-c(1)]
  return(authorInfosReturned)
}

test <- findLocation(10499451)
