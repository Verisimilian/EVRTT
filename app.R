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

#UI element#
ui <- fluidPage(
  titlePanel("VIRAL - Temporal and Geographic analysis of Biological Database Entries"), #Name panel
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
    label = "QUEUE"),                                              
  
  plotOutput("plot")                                                 #output of func(create_trend)
  #plotOutput("map")                                            ///#setup for Location when implemented///
  
  
  

  
)

server <- function(input, output) {
  observeEvent(
    eventExpr = input[["buttonpress"]],
    handlerExpr = {
      output$plot <- renderPlot({
        create_trend <- function(db, name){
          choosebank(input$dbin)
          sp_name <- isolate(paste0("SP=", input$queryin))                                         #INSTEAD OF NAME USE input$queryin AND ENCAPSULATE EVERYTHING AFTER <- IN isolate()
          returned_query <- query(sp_name)
          withProgress(                                                                             #Progress Bar in UI
            message = 'Querying...',                                                                #Message for User
            value = 0,                                                                              #start
            {progBar <- txtProgressBar(min = 0, max = length(returned_query$req), style = 3)
            lst = list()
            woop <- length(returned_query$req)                                                      
            for(i in 1:length(returned_query$req)){
              one_annot <- getAnnot(returned_query$req[i])
              lst[[i]] <- one_annot[[1]]
              setTxtProgressBar(progBar, i)
              incProgress(1/woop)                                                                   #increments progress bar by percentage complete
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
          title_name <- isolate(paste( input$queryin, "Trends Over Time"))                                      #INSTEAD OF NAME USE input$queryin AND ENCAPSULATE EVERYTHING AFTER <- in isolate()
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
        } #/create trend
        
        #instantiate
        create_trend(db, name)
        
       
        
      })
      output$map <- renderPlot({
        #/////PUT LOCATION CODE HERE STARTING WITH FUNCTION NAME, AS IN create_map <- function(db, name)///////
        #I will incorporate Reactivity (the nuts and bolts) in the morning
        # <3
        #any time you reference the db name directly, use input$dbin
        #any time you directly reference the name input by the user, use input$queryin
        
        
        
        
      })
      
      
      
    },
    ignoreNULL = TRUE
  )
    
  
  
  
  
#no touchy  
}

shinyApp(ui = ui, server = server)