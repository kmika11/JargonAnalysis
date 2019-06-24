##Jargon analysis project Summer 2019##
#Building the corpora: 
#English general terms and scraping science papers
library(pdftools)
library(tidyverse)
library(tidytext)
files <- list.files(pattern = "pdf$")
env_papers <- lapply(files, pdf_text)
length(env_papers)

library(tm) ## the tibble method works!
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
library(xml2)

##From here to indication below does not work.
list.files(pattern=".xml$")
# create a list from these files
list.filenames<-list.files(pattern=".xml$")
df_list <- lapply(list.filenames, function(f) {
  doc <- read_xml(f)
  setNames(data.frame(
    xml_attr(xml_find_all(doc, "//ns2:opendataField"), "key"),
    xml_attr(xml_find_all(doc, "//ns2:opendataField"), "value")
  ), c("key", f))
  
})
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

###Here. Below is what works, but directory must be set to the jargon master, not to test. 

##This results in however many xml documents in the folder being read in as a DTM.
library(tm)
blogposts <- Corpus(DirSource("Test/XML"))
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


##Working on some analysis/creating numbers methods: 
#methods used will be: Jargonness, LSA, lex tightness, Flesch-Kincaid, POS analysis

##Jargonness: 1. comparison of word frequency in corpora: count words in science corpora, count in normal
#make ratio of counts. 

##LSA: latent semantic analysis: 

##Flesch-Kincaid: 206.835 - 1.015(totalwords/totalsentences) - 84.6(totalsyllables/totalwords)
#so I need a corpus of abstracts that is not tidy. It needs punctuation and spaces. 
#there is actually a function: readability(txt.file, hyphen = NULL, index = "Flesch")
library(koRpus)
library(koRpus.lang.en)
#install.koRpus.lang(lang = "en") #Think I have this on machine now, so I don't need to run line again.
#k so the readability function takes .txt files. I want to look at my abstracts only, so maybe I save them as txt files...?
readability("Test/Ab1.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")
readability("Test/Abs2.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")
readability("Test/Abs3.txt", hyphen = NULL, index = "Flesch.Kincaid", tagger = "tokenize", force.lang = "en")


##Lexical Tightness: how inter-related words are in normal vs science language.
#is a mean of NPMI and is log2(p(a,b)/p(a)p(b))/-log2(p(a,b))