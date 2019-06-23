library(tidyverse)
library(tm)
library(XML)
library(tidytext)

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
plos <- VCorpus(DirSource("plostest", mode = "text"), 
                readerControl = list(reader = readPLOS))

View(plos)
plos[["journal.pbio.0000001.xml"]][["content"]]

# tidy - got rid of all metadata except filename (as identifier?) (which is maybe a bad call, but idk where to store it)
#slice out content
plos[[4]][[1]]

ploscontent <- data.frame(sapply(plos,`[`,1)) %>%
  gather(ploscontent) %>%
  unnest_tokens("words", value)

#pretty dirty - you can clean it up a bit but since you're matching words, i'm not sure 
# how essential it is to have the cleanest, most beautiful corpus. 



#try it with the whole dump! <--- THIS IS A BAD IDEA

allofplos <- VCorpus(DirSource("allofplos_xml", mode = "text"),
                     readerControl = list(reader = readPLOS))



##### JUNK #####
custom.xml <- system.file("texts", "custom.xml", package = "tm")
print(readLines(custom.xml), quote = FALSE)

mySource <- function(x) +
  XMLSource(x, parser = xml2::xml_children, reader = myXMLReader)

myXMLReader <- readXML(
  spec = list(author = list("node", "writer"),
              content = list("node", "description"),
              heading = list("node", "caption")),
  doc = PlainTextDocument()
)

corpus <- VCorpus(mySource(custom.xml))  

corpus

  
  
