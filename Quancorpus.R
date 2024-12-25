# install.packages("devtools")
# library(devtools)
# devtools::install_github("quanteda/quanteda")

library(quanteda)
# install.packages("readtext")
library (readtext)
#step 1. Корпус и словарь
DATA_DIR_1 = "D:/0/"
authtexts <- readtext(paste0(DATA_DIR_1, "*.txt"), docvarsfrom = "filenames", 
                      docvarnames = "document", encoding = "cp1251") #проверить кодировку
authtexts$text<- gsub("=","", authtexts$text)
mycorp <- corpus(authtexts)
#summary(mycorp)

prepositions <- c('вPR','воPR','наPR','сPR','соPR','кPR','коPR',
                  'поPR','изPR','заPR','уPR', 'отPR','безPR','доPR',
                  'оPR','обPR','черезPR','приPR','надPR','подPR',   
                  'проPR','дляPR',
                  'иCONJ','чтоCONJ', 'ноCONJ','аCONJ','потомуADVPRO',
                  'поэтомуADVPRO','несмотряADV','хотяCONJ')

prep_corp <- dfm(mycorp, select = prepositions, tolower = TRUE, stem = FALSE)

x <- convert(prep_corp, to = "data.frame")

write.csv(x, "writers.csv")
prep_corp_base <- read.csv("writers.csv", header=TRUE, stringsAsFactors = TRUE)
