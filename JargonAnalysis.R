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

###Using blog posts to create general english corpus. Directory must be set to the jargon master, not to test. 
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

###Building the Environmental science corpus###
#### Creating a corpus from PLOS articles 
# Link to full download: https://www.plos.org/text-and-data-mining 
# Basically, I used the tm package to create a VCorpus from the XML files 
# that PLOS downloads as. DirSource() imports files from a directory. I created
# a custom reader (readPLOS) to handle the XML structure of the files 
# DIRECTORY IS TOO BIG FOR R - USE SUBSETS (I used 15 files with zero performance issues
# if you want to test with a more representative sample, I'm sure it could handle 50+)

#create custom reader
readPLOS <- readXML(
  spec = list(front = list("node", "front"),
              content = list("node", "body"),
              back = list("node", "back")),
  doc = PlainTextDocument())

#use DirSource to recursively import documents 
#note that the directory works when set to the main project directory, in my case, jargonanalysis-master.
plos <- VCorpus(DirSource("ploscorpus/plostest", mode = "text"), 
                readerControl = list(reader = readPLOS))

View(plos)
plos[["journal.pbio.0000001.xml"]][["content"]]

# tidy - got rid of all metadata except filename (as identifier?) (which is maybe a bad call, but idk where to store it)
#slice out content
plos[[4]][[1]]

ploscontent <- data.frame(sapply(plos,`[`,1)) %>%
  gather(ploscontent) %>%
  unnest_tokens("words", value)

##### Working on some analysis/creating numbers methods ####
#methods used will be: Jargonness, LSA, lex tightness, Flesch-Kincaid, POS analysis

##Jargonness: 1. comparison of word frequency in corpora: count words in science corpora, count in normal
#make ratio of counts. 

##LSA: latent semantic analysis: 
library(lsa)
library(LSAfun)
#creating the matrixes for the analysis
#a question: Should I create different matrixes for the env publications and the blog corups, or can it all go in one? 
#working directory is set to jargonAnalysis-master.
source_dir <- "Test/XML"
text_dir <- blogposts #it only wants a path, not an object. So I need to find a way to make this a path.
TDM <- textmatrix(source_dir, stopwords = stopwords_en, stemming = TRUE, removeXML = TRUE, removeNumber = T, minGlobFreq=1)
summary.textmatrix(TDM)
# creating weighted matrix TDM2 out of the original TDM. TDM2 is the term frequency times its inverse document frequency
TDM2 <- lw_tf(TDM) * gw_idf(TDM) 
LSAspace <- lsa(TDM2, dims=dimcalc_share())
as.textmatrix(LSAspace)
#lsa function above makes TDM2 into three matrices. tk (term matrix), dk (document matrix), and sk (singular val matrix)
tk2 <- t(LSAspace$sk *t(LSAspace$tk))
#can plot dimensions and terms.
plot(tk2[,1], y=tk2[,2], col="red", cex=.50, main="TK Plot")
text(tk2[,1], y=tk2[,2], labels=rownames(tk2), cex=.70)

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