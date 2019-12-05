###############################################################
####                                                       ####
####   These are the variables and hyperparameters you     ####
####   will need to set to run the program. Follow the     ####
####   instructions given in the comments.                 ####
####                                                       ####
###############################################################


# set the working directory  - keep the quotation marks
setwd("C://Users//metl023//Desktop//simu")

# load data - insert the file name here in the form '[name].csv' - keep the quotation marks
data <- read.csv("review0-2.csv", header=TRUE)

# list the column where the text data lives in the form data$NAME (where [NAME] is the column name) - no quotation marks
myText = data$text

# set the columns names for the metadata - keep the quotation marks
myContinuousValue = 'state'

# set the value - this is the hyperparameter that can be changed - much like with k-means clustering
k = 4

# insert any common words you would like the model to ignore here wrap them in quotation marks. If there are none leave blank
customStopWords = c('said','told','didnt','called', 'back', 'call', 'asked', 'got','bonnets','broadband','get','dont','just','can','cant','service','will','now','customer', 'company','customers','thing','people','one','something','nothing','everything','going','well','another','really','tell','right','like','talk','make','someone','first','want','first','every')

###############################################################
####  End of variables section							   ####
###############################################################



###############################################################
####                                                       ####
####   This part imports the relevant libraries.           ####
####                                                       ####
###############################################################


# import libraries. You will need to select a mirror (list of countries) from the GUI list
packages <- c("stm", "Rtsne", "geometry", "RColorBrewer", "tm", "wordcloud")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(stm)
library(Rtsne)
library(geometry)
library(RColorBrewer)
library(tm)
library(wordcloud)

###############################################################
####  End of libraries section							   ####
###############################################################



###############################################################
####                                                       ####
####   This part cleans the data. See the slides from      ####
####   Wednesday and Friday for more information.          ####
####                                                       ####
###############################################################

# stemming/stopwords/etc.
processed <- textProcessor(documents=myText, metadata = data, customstopwords = customStopWords, stem = FALSE, striphtml = TRUE)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

#output will have object meta, Abstract, and vocab
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

###############################################################
####  End of data cleaning section      				   ####
###############################################################



###############################################################
####                                                       ####
####   This part builds the actual model. See the slides   ####
####   from Friday for more information.                   ####
####                                                       ####
###############################################################

# build the model
result <- stm(docs, vocab, K = k, prevalence=~ s(eval(as.name(myContinuousValue))), init.type = "LDA", data=meta)
###############################################################

####                                                       ####

####   These are the variables and hyperparameters you     ####

####   will need to set to run the program. Follow the     ####

####   instructions given in the comments.                 ####

####                                                       ####

###############################################################





# set the working directory  - keep the quotation marks

setwd("C://Users//metl023//Desktop//simu")



# load data - insert the file name here in the form '[name].csv' - keep the quotation marks

data <- read.csv("Reviews.csv", header=TRUE)



# list the column where the text data lives in the form data$NAME (where [NAME] is the column name) - no quotation marks

myText = data$text



# set the columns names for the metadata - keep the quotation marks

myContinuousValue = 'rating'



# set the value - this is the hyperparameter that can be changed - much like with k-means clustering

k = 4



# insert any common words you would like the model to ignore here wrap them in quotation marks. If there are none leave blank

customStopWords = c('said','told','didnt','called', 'back', 'call', 'asked', 'got','bonnets','broadband','get','dont','just','can','cant','service','will','now','customer', 'company','customers','thing','people','one','something','nothing','everything','going','well','another','really','tell','right','like','talk','make','someone','first','want','first','every')



###############################################################

####  End of variables section							   ####

###############################################################







###############################################################

####                                                       ####

####   This part imports the relevant libraries.           ####

####                                                       ####

###############################################################





# import libraries. You will need to select a mirror (list of countries) from the GUI list

packages <- c("stm", "Rtsne", "geometry", "RColorBrewer", "tm", "wordcloud")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)



library(stm)

library(Rtsne)

library(geometry)

library(RColorBrewer)

library(tm)

library(wordcloud)



###############################################################

####  End of libraries section							   ####

###############################################################







###############################################################

####                                                       ####

####   This part cleans the data. See the slides from      ####

####   Wednesday and Friday for more information.          ####

####                                                       ####

###############################################################



# stemming/stopwords/etc.

processed <- textProcessor(documents=myText, metadata = data, customstopwords = customStopWords, stem = FALSE, striphtml = TRUE)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)



#output will have object meta, Abstract, and vocab

docs <- out$documents

vocab <- out$vocab

meta <-out$meta



###############################################################

####  End of data cleaning section      				   ####

###############################################################







###############################################################

####                                                       ####

####   This part builds the actual model. See the slides   ####

####   from Friday for more information.                   ####

####                                                       ####

###############################################################



# build the model

result <- stm(docs, vocab, K = k, prevalence=~ s(eval(as.name(myContinuousValue))), init.type = "LDA", data=meta)



#display model results

plot(result)

labelTopics(result, topics = NULL, n = 7, frexweight = 0.5)



###############################################################

####  End of model build section         				   ####

###############################################################







###############################################################

####                                                       ####

####   This part builds the visualisation (tagclouds).     ####

####   See the slides from Thursday for more information.  ####

####                                                       ####

###############################################################



# make wordclouds

pdf("topic_cloud_review0-2.pdf")

for (i in 1:k){

  cloud(result, topic = i, max.words = 50, random.color = TRUE, colors = brewer.pal(6,"Dark2"))

  title(paste("Topic ", i))

}

dev.off() 



regress <- estimateEffect(c(1:k) ~ s(rating), result, metadata = meta)



# print a regression of topic to sentiment

for (i in c(1:k)) {  

  x = i

  title = paste("trend_topic_",x,".jpg", sep="")

  jpeg(title)

  plot(regress, "rating", method = "continuous", topics = x, model = result, ci.level = 0, printlegend = FALSE, xlab = "rating")

  dev.off()

}



#end
