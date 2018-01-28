.libPaths(.libPaths()[2])
.libPaths("C:/R_library/R_3.2")
if(!require(rentrez)){
  install.packages("rentrez")
  library(rentrez)
}
if(!require(xml2)){
  install.packages("xml2")
  library(xml2)
}
if(!require(rvest)){
  install.packages("rvest")
  library(rvest)
}
if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}



getAllMatches <- function(regPattern, stringyboi){
  #initial vars
  tempResults <- matrix(nrow = 1)
  finalResults <- matrix(nrow = 1)
  currentMatch <- "."
  
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
      returnedResults <- finalResults[-c(1)]
    }
  }
  return(returnedResults)
  
}





getAuthInfo <- function(genomeID, db){
  linked <- entrez_link(dbfrom = db, id = genomeID, db ="pubmed") #finds linked pubmed articles to genome entry
  pubID <- linked$links$genome_pubmed #retrieves pubmed ID for link hit
  htmlRead = read_html((paste0("https://www.ncbi.nlm.nih.gov/pubmed/",pubID)))#downloads html of pubmed page
  authorInfo <- as.character(xml_find_all(htmlRead, ".//dd")[1]) #finds author information
  
  #sanitize location string before grepping
  badChars = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                      'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                      'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                      'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                      'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  
  chartr(paste(names(badChars), collapse=''),
         paste(badChars, collapse=''),
         authorInfo)
  
  return(authorInfo)
}

findLocation <- function(genomeID, db){
  authorInfo <- getAuthInfo(genomeID, db) #retrieves author information
  
  #attempts to grep locations using complex regex | see regexr.com <<<<<
  location <- str_match(string = authorInfo, pattern = "([A-Z])(?![A-Z])\\w+[\\s]?(\\w+)\\,[\\s][A-Z]\\w+")
  pattern <- "([A-Z])(?![A-Z])\\w+[\\s]?(\\w+)\\,[\\s][A-Z]\\w+"
  returnedResults <- getAllMatches(pattern, location)
  print(returnedResults)
  
}
findLocation("10319", "genome")
findLocation("ABYD01000001","refseqViruses")
genomeID <- "ABYD01000001"
db <- "refseqViruses"
