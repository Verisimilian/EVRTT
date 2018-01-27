if(!require(seqinr)){
  install.packages("seqinr")
  library(seqinr)
}

create_trend <- function(db, name){
  choosebank(db)
  name <- paste("SP=", name)
  returned_query <- query(name)
  #all_annot <- sapply(returned_query$req[1:length(returned_query$req)], getAnnot)
  all_annot <- getAnnot(returned_query)
  
  date_df <- as.data.frame( matrix(ncol = 2 ))
  colnames(date_df) <-  c("Month", "Year")
  
  for(i in 1:length(all_annot)){
    line_one <- gsub("\\s+", " ", all_annot[[i]][1])
    split_line <- strsplit(line_one, " ")
    date <- split_line[[1]][length(split_line[[1]])]
    ac_num <- split_line[[1]][2]
    
    split_date <- strsplit(date, "-")
    month <- as.vector(split_date[[1]][2])
    year <- as.numeric(as.vector(split_date[[1]][3]))
    temp_df <- data.frame(Month = month, Year = year, row.names = ac_num)
    if(i ==1){
      date_df <- temp_df
    }else{
      date_df <- rbind(date_df, temp_df)
    }
  }
  
  date_df$Month <- factor(date_df$Month, levels = toupper(month.abb))
  date_df <- date_df[ order( date_df$Year, date_df$Month ) , ]
  print(date_df)
  plot(date_df)
}


db <- "refseqViruses"
name <- "Lentivirus"
create_trend(db, name)
