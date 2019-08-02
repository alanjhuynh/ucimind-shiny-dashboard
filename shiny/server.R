#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

if(!require(RCurl)){
  install.packages("RCurl")
}
if(!require(jsonlite)){
  install.package("jsonlite")
}
if(!require(ggplot2)){
  install.packages("ggplot2")
}
if(!require(plotly)){
  install.packages("plotly")
}
if(!require(flexdashboard)){
  install.packages("flexdashboard")
}
library(RCurl)                                  #load RCurl library
library(jsonlite)                               #load JSON library
library(gridExtra)
library(ggplot2)
library(plotly)
library(flexdashboard)
#library(bitops)

library(shiny)

#API function
getResult <- function(reportId)
{
  result <- postForm(                             #request patient cohort records
    uri='https://dmsc.mind.uci.edu/redcap/api/',
    token='insert api token here',
    content='report',
    format='json',
    report_id = reportId,
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    returnFormat='json'
  )
  return (result)
}  

# Define server logic required to draw plots
shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
    
    datasetInput <- reactive({                      #make reactive
      switch(input$dataset,
             "Patient Cohort" = 621,
             "Control Cohort" = 622,
             "Home Cohort" = 623
      )  
    })
    
    reportId <- datasetInput()                      #select from reactive

    withProgress(message = "Loading...", value = 0,{  #add progress bar
      
    incProgress(3/10, detail = "Pulling from API")  
    result <- getResult(reportId)                   #call API function
    
    patientDS <- fromJSON (result)                  #get JSON from request
    
    theme_update(plot.title = element_text(hjust = 0.5))
    
    ### SEX
    incProgress(1/10, detail = "Ploting Sex Data") 
    #Sex Data Setup
    sexTable <- table(as.factor(patientDS$sex))     #make table of sexes
    sexDf <- data.frame(sexTable)                   #make the table into a dataframe
    sexDf <- sexDf[-c(3),]                          # GET RID OF NULL ROW; null was 3rd row; "," removes row instead of column
    
    #Sex Bar Chart ggplot
    sexPlot <- ggplot(data = sexDf,                 #data frame  
                      aes(x = Var1, y = sexDf$Freq/sum(sexDf$Freq),   #set x and y
                          text = paste("Proportion:", sexDf$Freq/sum(sexDf$Freq), "", "Count:", sexDf$Freq))) + 
      geom_bar(stat = "identity", fill = c("darkgreen", "orchid4"), color = "black") +                 #set as bar chart
      labs (title = paste("Sex Chart n = ", sum(sexDf$Freq)), x = "Sex", y = "Proportion")    #labels
    
    #Sex Summary
    output$sexSummary <- renderPrint({
      sexDf
    })
    ###
    
    ### AGE
    incProgress(1/10, detail = "Ploting Age Data") 
    #Age Data Setup
    ageTable <- table(as.factor(patientDS$age))
    ageDf <- data.frame(ageTable)
    ageVector <- unlist(as.numeric(patientDS$age))
    ageDfgp <- data.frame(ageVector)
    
    #Age Boxplot ggplot
    agePlot <- ggplot(data = ageDfgp, aes(y = ageVector, x = "")) +
      geom_boxplot() +
      labs (title = "Age Boxplot", y = "Age", x = "") +
      coord_flip() 
    
    
    #Age Historgram ggplot
    ageHist <- ggplot (data = ageDfgp, aes(x = ageVector, y = ..count.. / sum(..count..))) +
      geom_histogram(fill = "white", col = "black", binwidth = 2) +
      labs(title = "Age Histogram", x = "Age", y = "Proportion")
    
    #Age Summary
    output$ageSummary <- renderPrint({
      summary(ageVector)
    })
    ###
    
    ### EDUCATION
    incProgress(1/10, detail = "Ploting Education Data") 
    #Education Data Setup
    eduTable <- table(as.factor(patientDS$eduyears))
    eduDf <- data.frame(eduTable)
    eduDf <- eduDf[-c(1),]
    
    #Education Bar Chart ggplot
    eduPlot <- ggplot(data = eduDf,                   
                      aes(x = Var1, y = eduDf$Freq/sum(eduDf$Freq))) +
      geom_bar(stat = "identity", fill = rainbow(13), color = "black") + 
      labs (title = "Years of Education", x = "Education Years", y = "Proportion") 
    
    #Education Summary
    eduVector <- unlist(as.numeric(patientDS$eduyears))
    output$eduSummary <- renderPrint({
      summary(eduVector)
    })
    
    ###
    
    ### RACE
    incProgress(1/10, detail = "Ploting Race Data") 
    #Race Data Setup
    raceTable <- table(as.factor(patientDS$race))
    raceDf <- data.frame(raceTable)
    raceDf <- raceDf[-c(3),] #fix
    
    #Race Bar Chart ggplot
    
    racePlot <- ggplot(data = raceDf,                   
                       aes(x = Var1, y = raceDf$Freq/sum(raceDf$Freq))) +
      geom_bar(stat = "identity") + 
      labs (title = "Race Chart", x = "Race", y = "Proportion") 
    
    ###
    
    ### ETHNICITY
    incProgress(1/10, detail = "Ploting Ethnicity Data") 
    #Ethnicity Data Setup
    ethnicTable <- table(as.factor(patientDS$ethnicitymod))
    ethnicDf <- data.frame(ethnicTable)
    ethnicDf <- ethnicDf[-c(8),] #fix
    
    #Ethnicity Bar Chart ggplot
    ethnicPlot <- ggplot(data = ethnicDf,                   
                         aes(x = Var1, y = ethnicDf$Freq/sum(ethnicDf$Freq))) +
      geom_bar(stat = "identity") + 
      labs (title = "Ethnicity Chart", x = "Ethnicity", y = "Proportion") 
    
    ###
    incProgress(1/10, detail = "Displaying All Data") 
    grid.arrange(sexPlot, agePlot, ageHist, eduPlot, racePlot, ethnicPlot, ncol = 1)
    
    })
  })
})
