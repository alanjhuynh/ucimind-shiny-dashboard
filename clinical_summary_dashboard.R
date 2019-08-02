#!/usr/bin/env Rscript
#rm(list=ls()) #clear environment

#install.packages(c("RCurl", "jsonlite"))
if(!require(RCurl)){
  install.packages("RCurl")
}
if(!require(jsonlite)){
  install.packages("jsonlite")
}
if(!require(ggplot2)){
  install.packages("ggplot2")
}
if(!require(plotly)){
  install.packages("plotly")
}

library(RCurl)                                  #load RCurl library
library(jsonlite)                               #load JSON library
library(ggplot2)
library(plotly)
#library(bitops)

test <- '622'

result <- postForm(                             #request patient cohort records
  uri='https://dmsc.mind.uci.edu/redcap/api/',
  token='insert API token here',
  content='report',
  format='json',
  report_id = test,
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  returnFormat='json'
)

#average.function <- function(totalCount, totalPatient){
#  average <- totalCount/totalPatient
#  print(average)
#}
getMode <- function(v) {                        #get mode function
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#get avg, median, mode, min, max, sd DON'T NEED THIS BECAUSE OF SUMMARY FUNCTION
getStat <- function(numericList)
{
  average <- mean (as.numeric(numericList))#result$age))
  median <- median (as.numeric(numericList))
  mode <- getMode (as.numeric(numericList))
  min <- min (as.numeric(numericList))
  max <- max (as.numeric(numericList))
  sd <- sd (as.numeric(numericList))
  
  print(average)
  print(median)
  print(mode)
  print(min)
  print(max)
  print(sd)
}

patientDS <- fromJSON (result)                  #get JSON from request
#class(result)                                  #check data type: "data.frame" - everything is "character w/in data frame"
#print (result)
#str(result)
#summary(result)
#print(nrow(result))                     
#print(ncol(result))

#Sex Data Setup
sexTable <- table(as.factor(patientDS$sex))     #make table of sexes
sexDf <- data.frame(sexTable)                   #make the table into a dataframe
sexDf <- sexDf[-c(3),]                          # GET RID OF NULL ROW; null was 3rd row; "," removes row instead of column
                                                #This is hard coded to specific row, so may cause error if more data is added
#testdf <- data.frame(table(patientDS$sex)) same thing?

#print(summary(sexDf))                           #get statistics summary for the data frame (min,max,mean,etc)


#Sex Bar Chart
sexProp <-(sexDf$Freq/sum(sexDf$Freq))          #get proportion for each sex

sexPlot <- barplot(sexProp,                     #bar plot for the proportion 
        main = paste("Sex Chart n = ", sum(sexDf$Freq)), #title + total patient
        ylab = "Proportion",                                 #y axis
        xlab = "Sex",                                        #x axis
        names.arg = paste(sexDf$Var1, "=", sexDf$Freq),      #title under each bar + patient count
        col = c("purple","green")                            #color for each column; can add [] to check for conditions
        )

#Sex Bar Chart ggplot
theme_update(plot.title = element_text(hjust = 0.5))

sexPlot <- ggplot(data = sexDf,                   
       aes(x = Var1, y = sexDf$Freq/sum(sexDf$Freq), 
           text = paste("Proportion:", sexDf$Freq/sum(sexDf$Freq), "", "Count:", sexDf$Freq))) + 
       geom_bar(stat = "identity") + 
       labs (title = paste("Sex Chart n = ", sum(sexDf$Freq)), x = "Sex", y = "Proportion") 

ggplotly(sexPlot, tooltip = "text")

###########

#Age Data Setup
ageTable <- table(as.factor(patientDS$age))
ageDf <- data.frame(ageTable)
ageVector <- unlist(as.numeric(patientDS$age))
ageDfgp <- data.frame(ageVector)

#Age Boxplot
summary (ageVector)
agePlot <- boxplot(ageVector,
        horizontal = TRUE,                                   #make boxplot horizontal
        main = "Age Boxplot",
        xlab = "Age"
        )

#Age BoxPlot ggplot
agePlot <- ggplot(data = ageDfgp, aes(y = ageVector, x = "")) +
                  geom_boxplot() +
                  labs (title = "Age Boxplot", y = "Age", x = "") +
                  coord_flip() 
                   

#Age Histogram
ageHist <- hist(ageVector,
     freq = FALSE,                                           #switch from freq to density
     main = "Age Histogram",
     xlab = "Age",
     ylab = "Proportion"
     )

#Age Historgram ggplot
ageHist <- ggplot (data = ageDfgp, aes(x = ageVector, y = ..count.. / sum(..count..))) +
                   geom_histogram(fill = "white", col = "black", binwidth = 2) +
                   labs(title = "Age Histogram", x = "Age", y = "Proportion")

###########

#Education Data Setup
eduTable <- table(as.factor(patientDS$eduyears))
eduDf <- data.frame(eduTable)
eduDf <- eduDf[-c(1),]

#Education Bar Chart
eduProp <-(eduDf$Freq/sum(eduDf$Freq))
eduPlot <- barplot(eduProp,
                   main = "Years of Education",
                   names.arg = (eduDf$Var1),
                   ylab = "Proportion",
                   xlab = "Education Years"
                   )


#Education Bar Chart ggplot
eduPlot <- ggplot(data = eduDf,                   
                  aes(x = Var1, y = eduDf$Freq/sum(eduDf$Freq))) +
                  geom_bar(stat = "identity") + 
                  labs (title = "Years of Education", x = "Education Years", y = "Proportion") 
                
###########

#Race Data Setup
raceTable <- table(as.factor(patientDS$race))
raceDf <- data.frame(raceTable)
raceDf <- raceDf[-c(3),] #fix

#Race Bar Chart
raceProp <-(raceDf$Freq/sum(raceDf$Freq))

racePlot <- barplot(raceProp,
                   main = "Race Chart",
                   names.arg = (raceDf$Var1),
                   ylab = "Proportion",
                   xlab = "Race",
                   ylim = c(0,1)
)

#Race Bar Chart ggplot

racePlot <- ggplot(data = raceDf,                   
                  aes(x = Var1, y = raceDf$Freq/sum(raceDf$Freq))) +
  geom_bar(stat = "identity") + 
  labs (title = "Race Chart", x = "Race", y = "Proportion") 

###########
#Ethnicity Data Setup
ethnicTable <- table(as.factor(patientDS$ethnicitymod))
ethnicDf <- data.frame(ethnicTable)
ethnicDf <- ethnicDf[-c(8),] #fix
print(ethnicDf)

ethnicProp <-(ethnicDf$Freq/sum(ethnicDf$Freq))
print(ethnicProp)
ethnicPlot <- barplot(ethnicProp,
                    main = "Ethnicity Chart",
                    names.arg = (ethnicDf$Var1),
                    ylab = "Proportion",
                    xlab = "Ethnicity",
                    col = rainbow(length(ethnicProp)),
                    cex.names = 0.8
)

#Ethnicity Bar Chart ggplot
ethnicPlot <- ggplot(data = ethnicDf,                   
                   aes(x = Var1, y = ethnicDf$Freq/sum(ethnicDf$Freq))) +
  geom_bar(stat = "identity") + 
  labs (title = "Ethnicity Chart", x = "Ethnicity", y = "Proportion") 
