##Jargon analysis project Summer 2019##
#Building the corpora: 
#English general terms and scraping science papers
library(pdftools)
library(tidyverse)
library(tidytext)
files <- list.files(pattern = "pdf$")
env_papers <- lapply(files, pdf_text)
length(env_papers)

library(tm) ##None of the below works. But the tibble method does!
#env_corp <- Corpus(URISource(files), readerControl = list(readPDF))
#env_corp <- tm_map(env_corp, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
#env_corp <- tm_map(env_corp, removePunctuation, ucp = TRUE)
#env_corp_TDM <- TermDocumentMatrix(env_corp, control = list(removePunctuation = TRUE,
# stopwords = FALSE,
#tolower = FALSE,
#stemming = FALSE,
#removeNumbers = TRUE,
#bounds = list(global = c(1, Inf))))

env_text <- pdf_text(files)
env_tibble <- tibble(text = env_text)
env_tidy <- env_tibble %>%
  unnest_tokens(word, text)

##Trying to read in standard corpus in txt files:
library(readtext)
#std_corp <- readtext("wlp_fiction_awq/*")
test_corp <- readtext("Test/*",
                      docvarsfrom = "filenames",
                      docvarnames = c("col1", "col2", "col3"),
                      dvsep="")
#this (below) doesn't work 
#test_corp2 <- read.table("Test/wlp_fic_2012.txt", sep = "\t", header = TRUE)
#OK this broke my computer last time, so don't run it.
test_tibble <- tibble(text = test_corp)
test_tidy <- test_tibble %>%
  unnest_tokens(word,text)
#alright, there is some weird stuff going on with the third
#column in the txt files.
#can I read it in delimited by tabs? And then delete the third col?
##Trying the blog corpus which is xml
library(XML)
library(methods)
blog_corpus <- xmlParse(file = "Test/7596.male.26.Internet.Scorpio.xml")
blog_dataframe <- xmlToDataFrame("Test/7596.male.26.Internet.Scorpio.xml")
blog_list <- xmlToList(blog_corpus)
blog_tibble <- tibble(blog_dataframe)
blog_char <- as.character(blog_tibble)
blog_tidy <- blog_dataframe %>%
  unnest_tokens(word, text, format = "xml")

#another method?
posts <- xpathApply(blog_corpus, "//post",xmlValue)
postWords <- lapply(posts,strsplit,"[[:space:]]")
#postWords <- tibble(postWords)
library(tm)
blogposts <- Corpus(DirSource("Test"))
blogposts <- tm_map(blogposts, removePunctuation)
blogposts <- tm_map(blogposts, content_transformer(tolower))
blogposts <- tm_map(blogposts, removeNumbers)
blogposts <- tm_map(blogposts, stripWhitespace)
blogDTM <- DocumentTermMatrix(blogposts)
#looking at word frequency:
blog_freq <- colSums(as.matrix(blogDTM))
#total number of terms:
length(blog_freq) #14515

#posttm <- readXML(type = "node", spec = "XPathExpression") 