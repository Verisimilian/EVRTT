#if(!require(seqinr)){
#  install.packages("seqinr")
#}
#if(!require(zoo)){
#  install.packages("zoo")
#}
if(!require(XML)){
  install.packages("XML")
}
#if(!require(ggplot2)){
#  install.packages("ggplot2")
#} 
#INSTALLS ABOVE

#libs to include
library(ggplot2)
library(XML)
library(zoo)
library(seqinr)
library(shiny)
library(rentrez)
library(xml2)
library(rvest)
library(stringr)

#UI element#
ui <- fluidPage(
  titlePanel("EVRTT - Emerging Viral Research Tracking Tool"), #Name panel
  radioButtons(                                         #selection buttons, add new databases here
    inputId = "dbin",                                   #variable name for later use with input$dbin
    label = "Select a database:",                       #label so the user isnt clueless
    c("refseqViruses" = "refseqViruses",                #array of options, first "" is what text shows up in the buttons, second "" is what is passed to the function(s) in -server-
      "bacterial" = "bacterial")
  ),
  textInput(                                                         #simple text print function
    inputId = "queryin",                                             #variable name for later use in input$queryin
    label = "Enter a valid order, family, genus, or species",        #label for user
    placeholder = "Ex. Mononegavirales"),                            #that little greyed out portion that gives the user an example so they dont enter "Traffic Cone" or some shit
  
  actionButton(                                                      #button
    inputId = "buttonpress",                                        
    label = "EVR-it!"),                                              
  
  
  location_list <<- vector(),
  plotOutput("plot"),                                                 #output of func(create_trend)
  plotOutput("map")                                            #setup for Location when implemented///
  
  
  
  
  
)

server <- function(input, output) {
  observeEvent(
    eventExpr = input[["buttonpress"]],
    handlerExpr = {
      output$plot <- renderPlot({
        findLocation <- function(pubIDs){                                                                                #FUNCTION FIND LOCATION
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
        }                                                                                                                #FUNCTION FIND LOCATION
        
        getAllMatches <- function(regPattern, stringyboi){                                                              #FUNCTION GET ALL MATCHES
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
        }                                                                                                               #FUNCTION GET ALL MATCHES
        
        getAuthInfo <- function(pubIDs){                                                                                #FUNCTION GET AUTH INFO
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
        create_trend <- function(db, name){
          choosebank(input$dbin)
          sp_name <- isolate(paste0("SP=", input$queryin))
          returned_query <- query(sp_name)
          withProgress(
            message = 'Querying...',
            value = 0,
            {progBar <- txtProgressBar(min = 0, max = length(returned_query$req), style = 3)
            lst = list()
            woop <-length(returned_query$req)
            for(i in 1:length(returned_query$req)){
              one_annot <- getAnnot(returned_query$req[i])
              lst[[i]] <- one_annot[[1]]
              setTxtProgressBar(progBar, i)
              incProgress(1/woop)
            }})
          
          date_df <- as.data.frame( matrix(ncol = 2 ))
          
          colnames(date_df) <-  c("Month", "Year")
          
          for(i in 1:length(lst)){
            line_one <- gsub("\\s+", " ", lst[[i]][1])
            split_line <- strsplit(line_one, " ")
            date <- split_line[[1]][length(split_line[[1]])]
            ac_num <- split_line[[1]][2]
            
            split_date <- strsplit(date, "-")
            month <- as.vector(split_date[[1]][2])
            year <- as.numeric(as.vector(split_date[[1]][3]))
            
            temp_df <- data.frame(Month = month, Year = year,  row.names = ac_num)
            if(i ==1){
              date_df <- temp_df
            }else{
              date_df <- rbind(date_df, temp_df)
            }
          }
          date_df$Month <- factor(date_df$Month, levels = toupper(month.abb))
          date_df <- date_df[ order( date_df$Year, date_df$Month ) , ]
          
          
          for(j in 1:length(lst)){
            temp <- gsub("\\s+", " ", lst[[j]])
            for(i in 1:length(temp)){
              
              temp_id <- regmatches(temp[i], regexpr(pattern = "(?<=PUBMED\\s)\\d+", temp[i], perl = TRUE))
              if( !(identical(temp_id, character(0)))) {
                location_list <-c(location_list, findLocation(temp_id))
              }
            }
          }
          location_list <<- as.vector(location_list)
          
          
          
          
          df <- date_df
          first = df[1,]
          last = df[nrow(df),]
          ind = match(first[1,1], toupper(month.abb))
          fyr = first[1,2]
          lyr = last[1,2]
          hits_df = as.data.frame(matrix(nrow = 0, ncol = 2), col.names = c('Hits', 'Date'))
          for(i in fyr:lyr){
            for(j in ind:12){
              r = paste0(toupper(month.abb[j]), i)
              m = paste(toupper(month.abb[j]), i)
              tmp = data.frame(Hits = 0, Date = m, row.names = r)
              hits_df = rbind(hits_df, tmp)
            }
            ind = 1
          }
          
          for(i in 1:nrow(df)){
            tmp = paste0(df[i,1], df[i,2])
            hits_df[tmp, 1] = hits_df[tmp, 1] + 1
          }
          
          
          hits_df$Date <- as.yearmon(hits_df$Date)
          title_name <- isolate(paste( input$queryin, "Trends Over Time"))
          p <- ggplot(data=hits_df, mapping=aes(x=Date, y=Hits))+ scale_x_yearmon(format = "%b %Y") + geom_point() +geom_line(aes(group = 1))
          p <- p+ ggtitle(title_name) +theme( text = element_text(size = 16),
                                              axis.text.y = element_text(size = 16),
                                              plot.title = element_text(hjust = 0.5),
                                              axis.title.y = element_text(size = 18),
                                              axis.title.x  = element_text(size = 18),
                                              axis.text.x=element_text(size=16,hjust=1),
                                              axis.title=element_text(size=16,face="bold"))
          p
          return(p)
        }
        
        #instantiate
        create_trend(db, name)
        
        
        
      })
      output$map <- renderPlot({
        #/////PUT LOCATION CODE HERE STARTING WITH FUNCTION NAME, AS IN create_map <- function(db, name)///////
        #I will incorporate Reactivity (the nuts and bolts) in the morning
        # <3
        #any time you reference the db name directly, use input$dbin
        #any time you directly reference the name input by the user, use input$queryin
                                                                                     #FUNCTION GET AUTH INFO
        
        make_map <- function(visited){                                                                                  #FUNCTION MAKE MAP
          df = as.data.frame(matrix(nrow = 0, ncol = 3), col.names = c("lon", "lat", "occur"))
          for(i in visited){
            if(i %in% rownames(df)){
              df[i, 'occur'] = df[i, 'occur'] + 1
            }
            else{
              r = data.frame(lon = NA, lat = NA, occur = 1, row.names = i)
              df = rbind(df, r)
            }
          }
          
          ll.visited <- geocode(rownames(df))
          df$lon = ll.visited$lon
          df$lat = ll.visited$lat
          
          prob = df$occur / length(visited)
          df$Proportion = prob
          
          mp <- NULL
          mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
          mp <- ggplot(df) + mapWorld
          
          mp <- mp + geom_jitter(aes(x=lon, y=lat, col= Proportion), size = 2.5) +
            scale_color_gradient(low="pink",high='red',na.value="yellow") +
            theme(axis.text = element_blank(), axis.title = element_blank())
          mp
          return(mp)
        }                                                                                                               #FUNCTION MAKE MAP
        make_map(location_list)
        
        
      })
      
      
      
    },
    ignoreNULL = TRUE
  )
  
  
  
  
  
  #no touchy  
}

shinyApp(ui = ui, server = server)

