pkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","raster","sf","ggspatial","cluster","factoextra",
              "NbClust","tidyr","forecast","semPlot","semTools","corrplot",
              "corrr","haven","psych","dplyr","lavaan","readr","cvms","tm",
              "NLP","SnowballC","RColorBrewer","wordcloud","wordcloud2",
              "RefManageR","bibliometrix","GGally","quanteda","ggplot2",
              "ggpubr","Factoshiny","syuzhet","tm","RColorBrewer","tokenizers",
              "stringr","sentimentr","stringi","stopwords","twitteR",
              "mscstexta4r","plyr","corrplot","psych","corrr","latticeExtra",
              "semPlot","lavaan","readr","lme4","sjPlot","gvlma","Rcmdr",
              "tidymodels","caret","lmtest", "rtweet")

pkg(packages)

#call my Api Key
create_token(app="ClasII",
             consumer_key = "DP7F7o2r8bS5hophlgGdi9Kqd",#api key
             consumer_secret = "OQcWOdJG2JqAa8370w5Zu0UseQHu3QM7ChJ9xUTNTXmbAMzGWc",#api secret key
             access_token ="284827529-LORYnWAHxeYBUpm3PqnQ3EqD5JCa46jfVaSFnv0F",#acces token
             access_secret ="TOoNhDxMcSnxlYBguc7xyIK4bAABnSwoSbiFuFRoERnps")#acces token secret

#Now i will make any call from the tweeter API
Col <- get_timeline(user = "@petrogustavo", n = 500, parse = TRUE, check = FALSE)

#i will extract the text form the dataframe 
texto<-Col$text
#Now i will split the text in words to analyse each one of them separated
tokens<-tokens(texto,what = "word",remove_punct = TRUE,remove_symbols =TRUE,remove_numbers =TRUE,remove_url =TRUE,remove_separators =TRUE,split_hyphens =TRUE)
#I will celan the words 
# remove rt
x = gsub("rt", "", tokens)
# remove at
x = gsub("@\\w+", "", x)
# remove punctuation
x = gsub("[[:punct:]]", "", x)
# remove numbers
x = gsub("[[:digit:]]", "", x)
# remove links http
x = gsub("http\\w+", "", x)
# remove tabs
x = gsub("[ |\t]{2,}", "", x)
# remove blank spaces at the beginning
x = gsub("^ ", "", x)
# remove blank spaces at the end
x = gsub(" $", "", x)
#more unusual characters 
a<-gsub("[^\x01-\x7F]", "", x)
#now i will get the sentiments
sentimientos_df <- get_nrc_sentiment(a, lang="spanish")
#now i will print the "head()" to see if everything is ok. 
head(sentimientos_df)
#now i will make my summary to see the results. 
summary<-summary(sentimientos_df)
#graphic emotions
barplot(
  colSums(prop.table(sentimientos_df[, 1:8])),
  space = 0.2,
  horiz = F,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "8 diferentes emociones",
  sub = "Emociones",
  xlab="emociones", ylab = NULL)
#graphic sentiments
barplot(
  colSums(prop.table(sentimientos_df[, 9:10])),
  space = 0.2,
  horiz = T,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 2, name = "Set3"),
  main = "Sentimiento positivo y negativo",
  sub = "Sentimientos",
  xlab="emociones", ylab = NULL)
#counting words 
#i will set all the words in frecuency order, from 0 to the maximum.
#you can repeat this process with each emotion you want: fear, disgust,
#anger, trust, negative, positive, etc. 
palabras_tristeza <- a[sentimientos_df$sadness> 0]
palabras_tristeza_orden <- sort(table(unlist(palabras_tristeza)), decreasing = TRUE)
head(palabras_tristeza_orden, n = 13)
#Now, in a graphic i will draw the way in chich the dialog has changed between 
#positive sentiments and negative ones
sentimientos_valencia <- (sentimientos_df$negative *-1) + sentimientos_df$positive
simple_plot(sentimientos_valencia)
