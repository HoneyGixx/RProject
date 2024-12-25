tmp0 <- unlist(strsplit(wrdLines[1], ","))
length(tmp0)
wrdLines <- readLines("di.txt", encoding = "UTF-8")
df = data.frame(word=tmp0[1], type = tmp0[2], emotT = tmp0[3], 
                emotPT=tmp0[4]) 
i=2
for (i in 1:length(wrdLines)) {#
  tmp0 <- unlist(strsplit(wrdLines[i], ","))
  if (length(tmp0) == 3){
    df = rbind(df,c(tmp0[1],0,tmp0[2:length(tmp0)]))
  }else if (length(tmp0) == 4){
    df = rbind(df,tmp0)
  }else if (length(tmp0) == 5){
    df = rbind(df,c(tmp0[1],paste(tmp0[c(2,3)], collapse =","),tmp0[4:5]))
  }else if (length(tmp0) == 6){
    df = rbind(df,c(tmp0[1],paste(tmp0[c(2,3,4)], collapse =","),tmp0[5:6]))
  }
}
write.csv(df, "emot_alph.csv")
df$PosNeg <-  NA
for (i in 1:length(df$emotT)) {
  if (grepl('2|4|5|6|11|12|17|18|19|20|21|27|28|30|31|32|33|
            36|37|38|39',df$emotT[i], ignore.case = T)==TRUE){
    df$PosNeg[i] <-"pos"
  }else{
    df$PosNeg[i] <-"neg"
  }
}
#}
tmp = df
write.csv(df, "emot_alph_PosNeg.csv")
df$emotT[1:5]






library(readxl)
#library(quanteda.sentiment)
library(quanteda)
# install.packages("remotes")
# remotes::install_github("quanteda/quanteda.sentiment")
tmp <- read_excel("full_word_rating_after_coding.xlsx", col_names = TRUE)

df #head body stem class

mycorp <- corpus(df, text_field = "stem", )



dict <- dictionary(list(negative = c(tmp$word[tmp$value==-1]),
                        neg_negative = c(tmp$word[tmp$value==-2]),
                        pos_positive = c(tmp$word[tmp$value==2]),
                        neutral = c(tmp$word[tmp$value==0]),
                        positive = c(tmp$word[tmp$value==1])))
valence(dict) <- list(negative = -1, neg_negative = -2, pos_positive = 2, neutral = 0, positive =1)

dict2 <- dictionary(list(neg = c(tmp$word[tmp$PosNeg == "neg"]),
                         pos = c(tmp$word[tmp$PosNeg=="pos"])))
valence(dict2) <- list(neg = -1, pos = 1, neut = 0)
polarity(data_dictionary_LSD2015) <- dict
# list(pos = c("positive", "neg_negative"), neg = c("negative", "neg_positive"))
sent_pres <- mycorp_vk %>%
  corpus_subset(gnd == "f")
sent_pres2 <- mycorp_vk %>%
  corpus_subset(gnd == "m")
summary(mycorp_vk)
x_m <- tokens_lookup(tokens(sent_pres), dictionary = dict2) %>%
  dfm()
x_f <- tokens_lookup(tokens(sent_pres2), dictionary = dict2) %>%
  dfm()
x_m <- dfm_weight(x_m, scheme = "prop")
x_f <- dfm_weight(x_f, scheme = "prop")
x_full <- tokens_lookup(tokens(mycorp_vk), dictionary = dict2) %>%
  dfm()

x_f <-convert(x_f, to = "data.frame")
x_m <-convert(x_m, to = "data.frame")
x_m$gnd <- "f"
x_f$gnd <- "m"
x_full_abs_vkwall <- rbind(x_f, x_m)
x_full_abs_vkwall$ComType <- "vkw"
write.csv(x_full_abs_vkwall, "sentiment_vkwall_gnd.csv")
library(ggplot2)
ggplot(x_full_abs_vkwall, aes(doc_id, neg, fill = gnd, group = gnd)) + 
  geom_bar(stat='identity', position = position_dodge(), size = 1) + scale_fill_brewer(palette = "Set1") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  ggtitle("Sentiment scores in twelve Sherlock Holmes novels") + xlab("")
x_m2 <-  convert(x_m, to = "data.frame")
x_m2 <- as.data.frame(x_m)
x_f2 <-  convert(x_f, to = "data.frame")
x_f2 <- as.data.frame(x_f)






shapiro.test(x_full_abs_vkwall$neg)
shapiro.test(x_full_abs_vkwall$pos)

t.test(x_full_abs_vkwall$neg ~ x_full_abs_vkwall$gnd)
wilcox.test(x_full_abs_vkwall$pos ~ x_full_abs_vkwall$gnd)
wilcox.test(x_full_abs_vkwall$neg ~ x_full_abs_vkwall$gnd)
tapply(x_full_abs_vkwall$neg, x_full_abs_vkwall$gnd, summary)
tapply(x_full_abs_vkwall$pos, x_full_abs_vkwall$gnd, summary)
tapply(x_full_abs_vkwall$neut, x_full_abs_vkwall$gnd, summary)
ks.test()
help(t.test)
library(ggplot2)
ggplot(x_full_abs_vkwall, aes(doc_id, neg, fill = gnd, group = gnd)) + 
  geom_bar(stat='identity', position = position_dodge(), size = 1) + scale_fill_brewer(palette = "Set1") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  ggtitle("Sentiment scores in twelve Sherlock Holmes novels") + xlab("")
# x_m2 <-  convert(x_m_int, to = "data.frame")
# x_m2 <- as.data.frame(x_m)
# x_f2 <-  convert(x_f_int, to = "data.frame")
# x_f2 <- as.data.frame(x_f)
# barplot(as.matrix(x_m2[2:4]),
#         main="Средняя частота местоимений \n в текстах устной коммуникации",
#         xlab="Эмоциональные ЛЕ",
#         ylab="ср.знач",
#         border="black",
#         col="blue",
#         density=9, #раст-ие м-у барами
#         cex.lab = 2, #размер шрифта имен абсцисс ординат
#         cex.names=1 ##изменяемый параметр шрифта текста оси Х
# )
# 
# barplot(as.matrix(x_f2[2:4]),
#         main="Средняя частота местоимений \n в текстах устной коммуникации",
#         xlab="группа местоимений",
#         ylab="ср.знач",
#         border="black",
#         col="blue",
#         density=9, #раст-ие м-у барами
#         cex.lab = 2, #размер шрифта имен абсцисс ординат
#         cex.names=1 ##изменяемый параметр шрифта текста оси Х
# )

library("ggplot2")
ggplot(x_full_abs_vkwall) +
  geom_point(aes(x = pos, y = reorder(doc_id, pos))) +
  ylab("")
ggplot(x_full_abs_vkwall) +
  geom_point(aes(x = neg, y = reorder(doc_id, neg))) +
  ylab("")

x_m2$gnd <- "m"
x_f2$gnd <- "f"
emot_int <-  rbind(x_m_int, x_f_int)
ggplot(x_full_abs_vkwall, aes(doc_id, pos, fill = gnd, group = gnd)) + 
  geom_bar(stat='identity', position = position_dodge(), size = 1) + scale_fill_brewer(palette = "Set1") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  ggtitle("Sentiment scores in twelve Sherlock Holmes novels") + xlab("")
ggplot(x_full_abs_vkwall, aes(doc_id, neg, group = gnd, col = gnd)) + 
  geom_line(size = 1) + 
  scale_colour_brewer(palette = "Set1") + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Trump vs. Clinton: Tweets per month (2015-2017)") + 
  xlab("")

ggplot(x_full_abs_vkwall, aes(gnd, neg, colour = gnd, fill = gnd)) +
  geom_bar(stat="identity") + scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Pastel1") + ggtitle("Sentiment shares in two subreddits") + 
  xlab("") + ylab("Comments")
#textstat_polarity(sent_pres, data_dictionary_LSD2015)
#?polarity
# w_dict_dfm_mycorp_dlgs2 <- w_dict_dfm_mycorp_dlgs
# w_dict_dfm_full2 <- w_dict_dfm_full
# w_dict_dfm_mycorp_vk2 <- w_dict_dfm_mycorp_vk
# dim(w_dict_dfm_mycorp_dlgs2)
# dim(w_dict_dfm_full2)
# dim(w_dict_dfm_mycorp_vk2)
# w_dict_dfm_mycorp_dlgs2$ComType <-"dlgs"
# w_dict_dfm_full2$ComType <- "intv"
# w_dict_dfm_mycorp_vk2$ComType <- "wllVK"
# w_dict_dfm_full <- rbind(w_dict_dfm_mycorp_dlgs2,w_dict_dfm_full2,w_dict_dfm_mycorp_vk2)

summary(emot_int)
tapply(emot_int$neg, emot_int$gnd, summary)
tapply(emot_int$neut, emot_int$gnd, summary)
tapply(emot_int$pos, emot_int$gnd, summary)


boxplot(emot_int$neg ~ emot_int$gnd, col=c("red","blue"))
boxplot(emot_int$pos ~ emot_int$gnd)
boxplot(emot_int$neut ~ emot_int$gnd)
shapiro.test(emot_int$neg)
shapiro.test(emot_int$pos)
shapiro.test(emot_int$neut)
wilcox.test(emot_int$neg ~ emot_int$gnd)
wilcox.test(emot_int$pos ~ emot_int$gnd)
wilcox.test(emot_int$neut ~ emot_int$gnd)

# boxplot(w_dict_dfm_full$we ~ w_dict_dfm_full$ComType)
library(outliers)
grubbs.test(emot_int$neg, type = 10)

grubbs.test(emot_int$pos, type = 10)
grubbs.test(emot_int$neut, type = 10)
help("grubbs.test")
library(ggplot2)
p = ggplot(emot_int[,-1], aes(x=pos))
(p <- p+geom_density(aes(fill=gnd), alpha=1/2))

# Sample data
data <- x_full_abs_vkwall[, 2:3] # Numerical variables
groups <- as.factor(x_full_abs_vkwall[, 5]) # Factor variable (groups)
# Plot correlation matrix
pairs(data)

# Equivalent with a formula
pairs(~ neg+pos, data = x_full_abs_vkwall)


pairs(data,                     # Data frame of variables
      labels = colnames(data),  # Variable names
      pch = 1,                 # Pch symbol
      bg = rainbow(2)[groups],  # Background color of the symbol (pch 21 to 25)
      col = rainbow(2)[groups], # Border color of the symbol
      main = "",    # Title of the plot
      row1attop = TRUE,         # If FALSE, changes the direction of the diagonal
      gap = 1,                  # Distance between subplots
      cex.labels = NULL,        # Size of the diagonal text
      font.labels = 1)          # Font style of the diagonal text

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  his <- hist(x, plot = FALSE)
  breaks <- his$breaks
  nB <- length(breaks)
  y <- his$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = rgb(0, 1, 1, alpha = 0.5), ...)
  # lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
}

# Creating the scatter plot matrix
pairs(data,
      upper.panel = NULL,         # Disabling the upper panel
      diag.panel = panel.hist)    # Adding the histograms
# Function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

# Plotting the correlation matrix
pairs(data,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Smoothed regression lines

# install.packages("gclus")
library(gclus)

# Correlation in absolute terms
corr <- abs(cor(data)) 

colors <- dmat.color(corr)
order <- order.single(corr)

cpairs(data,                    # Data frame of variables
       order,                   # Order of the variables
       panel.colors = colors,   # Matrix of panel colors
       border.color = "grey70", # Borders color
       gap = 0.45,              # Distance between subplots
       main = "Ordered variables colored by correlation", # Main title
       show.points = TRUE,      # If FALSE, removes all the points
       pch = 21,                # pch symbol
       bg = rainbow(2)[groups]) # Colors by group


#install.packages("psych")
library(psych)

pairs.panels(data,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

library(psych)

corPlot(data, cex = 1.2)

library(ggplot2)
# install.packages("ggExtra")
library(ggExtra)
p_base <- ggplot(emot_int,aes(x=neg,y=pos,color=gnd)) + geom_point()
ggExtra::ggMarginal(p_base, groupColour = TRUE, groupFill = TRUE)
library(otrimle)
clus <- otrimleg(emot_int[,c(1,2)], G=2:5, monitor=1) # параметр monitor позволяет видеть ход выполнения



# Equivalent but using the plot function
plot(data)
library(tidyverse)
cor(w_dict_dfm_full[,2:4] %>% w_dict_dfm_mycorp_vk$gnd=="m")
w_dict_dfm_mycorp_vk  %>% 
  group_by(gnd) %>%
  plot(w_dict_dfm_mycorp_vk$self)




w_dict_dfm_full$gnd <- as.factor(w_dict_dfm_full$gnd)
w_dict_dfm_full_01 <- w_dict_dfm_full
w_dict_dfm_full_01$gnd01[w_dict_dfm_full$gnd=="m"] <- 1
w_dict_dfm_full_01$gnd01[w_dict_dfm_full$gnd=="f"] <- 0

fitglm <- glm(w_dict_dfm_full_01$gnd01 ~ w_dict_dfm_full_01$personal - w_dict_dfm_full_01$you + 
                w_dict_dfm_full_01$we +w_dict_dfm_full_01$self)

summary(fitglm)
anova(fitglm, test = "Chisq")

summary(w_dict_dfm_full)
tapply(w_dict_dfm_full$personal, w_dict_dfm_full$gnd, summary)
tapply(w_dict_dfm_full$you, w_dict_dfm_full$gnd, summary)
tapply(w_dict_dfm_full$we, w_dict_dfm_full$gnd, summary)
tapply(w_dict_dfm_full$self, w_dict_dfm_full$gnd, summary)
shapiro.test(w_dict_dfm_full$personal)
shapiro.test(w_dict_dfm_full$you)
shapiro.test(w_dict_dfm_full$we)
shapiro.test(w_dict_dfm_full$self)
wilcox.test(w_dict_dfm_full$personal~w_dict_dfm_full$gnd)
wilcox.test(w_dict_dfm_full$you~w_dict_dfm_full$gnd)
wilcox.test(w_dict_dfm_full$we~w_dict_dfm_full$gnd)
wilcox.test(w_dict_dfm_full$self~w_dict_dfm_full$gnd)
wilcox.test(w_dict_dfm_full$self+w_dict_dfm_full$personal+w_dict_dfm_full$you+w_dict_dfm_full$we~w_dict_dfm_full$gnd)
shapiro.test(c(w_dict_dfm_full$self+w_dict_dfm_full$personal+w_dict_dfm_full$you+w_dict_dfm_full$we))

kruskal.test(w_dict_dfm_full$personal~w_dict_dfm_full$ComType)
kruskal.test(w_dict_dfm_full$you~w_dict_dfm_full$ComType)
kruskal.test(w_dict_dfm_full$we~w_dict_dfm_full$ComType)
kruskal.test(w_dict_dfm_full$self~w_dict_dfm_full$ComType)
shapiro.test(c(w_dict_dfm_full$self+w_dict_dfm_full$personal+w_dict_dfm_full$you+w_dict_dfm_full$we))
boxplot(stacked_df$values ~ stacked_df$ind,
        col = rainbow(ncol(trees)))

boxplot(w_dict_dfm_full$personal ~ w_dict_dfm_full$gnd)
boxplot(w_dict_dfm_full$we ~ w_dict_dfm_full$gnd)
boxplot(w_dict_dfm_full$self ~ w_dict_dfm_full$gnd)
boxplot(w_df_dict_dfm_mycorp_int$you ~ norm_df_dict_dfm_mycorp_int$gnd)
boxplot(w_df_dict_dfm_mycorp_int$self ~ norm_df_dict_dfm_mycorp_int$gnd)
boxplot(w_df_dict_dfm_mycorp_int$we ~ norm_df_dict_dfm_mycorp_int$gnd)

boxplot(w_df_dict_dfm_mycorp_dlgs$personal ~ norm_df_dict_dfm_mycorp_dlgs$gnd)
boxplot(w_df_dict_dfm_mycorp_dlgs$you ~ norm_df_dict_dfm_mycorp_dlgs$gnd)
boxplot(w_df_dict_dfm_mycorp_dlgs$self ~ norm_df_dict_dfm_mycorp_dlgs$gnd)
boxplot(w_df_dict_dfm_mycorp_dlgs$we ~ norm_df_dict_dfm_mycorp_dlgs$gnd)

boxplot(w_df_dict_dfm_mycorp_vk$personal ~ norm_df_dict_dfm_mycorp_vk$gnd)
boxplot(w_df_dict_dfm_mycorp_vk$you ~ norm_df_dict_dfm_mycorp_vk$gnd)
boxplot(w_df_dict_dfm_mycorp_vk$self ~ norm_df_dict_dfm_mycorp_vk$gnd)
boxplot(w_df_dict_dfm_mycorp_vk$we ~ norm_df_dict_dfm_mycorp_vk$gnd)
#Statistica
shapiro.test(norm_df_dict_dfm_mycorp_int$personal)
shapiro.test(norm_df_dict_dfm_mycorp_int$you)
shapiro.test(norm_df_dict_dfm_mycorp_int$self)
shapiro.test(norm_df_dict_dfm_mycorp_int$we)
wilcox.test(norm_df_dict_dfm_mycorp_int$personal ~ norm_df_dict_dfm_mycorp_int$gnd) #W = 3190, p-value = 0.0001323
wilcox.test(norm_df_dict_dfm_mycorp_int$you ~ norm_df_dict_dfm_mycorp_int$gnd) #W = 2748, p-value = 0.0548
wilcox.test(norm_df_dict_dfm_mycorp_int$self ~ norm_df_dict_dfm_mycorp_int$gnd) #W = 2709, p-value = 0.08177
wilcox.test(norm_df_dict_dfm_mycorp_int$we ~ norm_df_dict_dfm_mycorp_int$gnd) #W = 2911, p-value = 0.009087

shapiro.test(norm_df_dict_dfm_mycorp_vk$personal)
shapiro.test(norm_df_dict_dfm_mycorp_vk$you)
shapiro.test(norm_df_dict_dfm_mycorp_vk$self)
shapiro.test(norm_df_dict_dfm_mycorp_vk$we)
wilcox.test(norm_df_dict_dfm_mycorp_vk$personal ~ norm_df_dict_dfm_mycorp_vk$gnd) #W = 3190, p-value = 0.0001323
wilcox.test(norm_df_dict_dfm_mycorp_vk$you ~ norm_df_dict_dfm_mycorp_vk$gnd) #W = 2748, p-value = 0.0548
wilcox.test(norm_df_dict_dfm_mycorp_vk$self ~ norm_df_dict_dfm_mycorp_vk$gnd) #W = 2709, p-value = 0.08177
wilcox.test(norm_df_dict_dfm_mycorp_vk$we ~ norm_df_dict_dfm_mycorp_vk$gnd) #W = 2911, p-value = 0.009087
norm_df_dict_dfm_mycorp_vk$gnd <- as.factor(norm_df_dict_dfm_mycorp_vk$gnd)

library(caret)
validation_index <- createDataPartition(x_full_abs_int$gnd, 
                                        p=0.70, list=FALSE)
# select 20% of the data for validation
test <- x_full_abs_int[-validation_index,]
train <- x_full_abs_int[validation_index,]
test <- test[,-c(1)]
train <- train[,-c(1)]


##Trees
author.tree<-rpart(gnd~.,data=train)
plot(author.tree,margin=0.1,uniform=T)
text(author.tree,use.n=T)
printcp(author.tree)
plotcp(author.tree)

author.predict.train<-predict(author.tree,train,type = "class")
ttreetrain<-table(train$gnd,author.predict.train)
error(ttreetrain)

author.predict.test<-predict(author.tree,test,type = "class")
ttreetest<-table(test$gnd,author.predict.test)
error(ttreetest)

table(test$gnd)
LS.train<-subset(train,gnd%in%c("f","m"))
LS.train$gnd<-factor(LS.train$gnd)
LS.test<-subset(test,gnd%in%c("f","m"))
LS.test$gnd<-factor(LS.test$gnd)

library(randomForest)
LSRandomForest<-randomForest(LS.train$gnd~.,data=LS.train,ntree=5000,importance=TRUE)
print(LSRandomForest)

hist(treesize(LSRandomForest))
importance(LSRandomForest)
varImpPlot(LSRandomForest)

LS.predictrf<-predict(LSRandomForest,LS.train,type="class")
LSrftrain<-table(LS.train$gnd,LS.predictrf)
error(LSrftrain)

LS.predictrftest<-predict(LSRandomForest,test,type="class")
LSrftest<-table(test$gnd,LS.predictrftest)
error(LSrftest)

# Loading package
library(e1071)
library(caTools)
library(caret)
# Fitting Naive Bayes Model
# to training dataset
classifier_auth <- naiveBayes(gnd ~ ., data = train)

classifier_auth

# Predicting on test data'
y_pred <- predict(classifier_auth, newdata = test)

# Confusion Matrix
cm <- table(test$gnd, y_pred)
cm
error(cm)
library(MASS)
##LDA
author.lda<-lda(gnd~.,data=train)
print(author.lda$scaling)
summary(author.lda)
author.ldapredicttrain<-predict(author.lda,train)
tldatrain<-table(train$gnd,author.ldapredicttrain$class)
error(tldatrain)
f1(tldatrain)
author.ldapredicttest<-predict(author.lda,test)
summary(author.ldapredicttest)
tldatest<-table(test$gnd,author.ldapredicttest$class)
error(tldatest)

paste0(x1, x2)
error(tldatest)
plot(author.lda,col=author.train$colour)
plot(author.lda,dimen=1,type="both")
plot(author.lda,dimen=1,type="density")


require(nnet)
##Logistic Regression
lr<-multinom(gnd~.,data=train)
help(multinom)
lr.train<-predict(lr,train,type = "class")
error(table(train$gnd,lr.train))

lr.test<-predict(lr,test,type = "class")
error(table(test$gnd,lr.test))
##SVM
# author.numtr$colour = NA
# author.numtr$colour[author.numtr$Author == "Austen"] = 0
# author.numtr$colour[author.numtr$Author == "London"] = 1
# author.numtr$colour[author.numtr$Author == "Milton"] = 2
# author.numtr$colour[author.numtr$Author == "Shakespeare"] = 3
# author.numtr <- author.numtr[,-15]
train$gnd <- as.factor(train$gnd)
author.svm<-svm(gnd~.,data=train,
                kernel="linear")

summary(author.svm)

author.pred<-predict(author.svm,
                     train,decision.values=T)
ttrain<-table(train$gnd,author.pred)
error(ttrain)

author.predtestsvm<-predict(author.svm,test,decision.values=T)
ttest<-table(test$gnd,author.predtestsvm)
error(ttest)


row.names(emot_int) <- emot_int[,1]

tstat_dist <- dist(emot_int[,2:4], method = "maximum")
help(dist)
clust <- hclust(tstat_dist)
pheatmap(clust)
plot(clust, xlab = "Distance", ylab = NULL,hang = -5, cex = 0.4)

plot(as.phylo(clust), type = "unrooted", cex = 0.6,
     no.margin = TRUE)


HC = hclust(dist(emot_int[,2:4], method = "euclidean"))
plot(emot_int[,2:4], pch=20, col=cutree(HC,4))


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
df <- emot_int[,2:4]
df <- na.omit(df)
df <- scale(df)
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2 <- kmeans(df, centers = 5, nstart = 25)
str(k2)
k2$cluster
fviz_cluster(k2, data = df)

df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)
# Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final)
fviz_cluster(final, data = df)

set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)


library("ggplot2")
library("ggdendro")
ggdendrogram(clust)
ggdendrogram(clust, rotate = TRUE, theme_dendro = FALSE)
# Build dendrogram object from hclust results
dend <- as.dendrogram(clust)
# Extract the data (for rectangular lines)
# Type can be "rectangle" or "triangle"
dend_data <- dendro_data(dend, type = "rectangle")
# What contains dend_data
names(dend_data)
head(dend_data$segments)
head(dend_data$labels)
p <- ggplot(dend_data$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  geom_text(data = dend_data$labels, aes(x, y, label = label),
            hjust = 1, angle = 90, size = 1)+
  ylim(-3, 2)
print(p)



data <- scale(emot_int[,2:4])
dist.res <- dist(data)
hc <- hclust(dist.res, method = "ward.D2")
dend <- as.dendrogram(hc)
plot(dend,  cex.sub = 0.2,  # Subtitle size
     cex.lab = 0.3,    # X-axis and Y-axis labels size
     cex.axis = 0.5)

plot(dend, which.plots=2, cex=0.1)


library(cluster)
data(votes.repub)
agn1 <- agnes(emot_int[,2:4], metric = "manhattan", stand = TRUE)

plot(hc, which.plots=2)
plot(hc, which.plots=2, cex=0.1)




library("rjson")
result <- fromJSON(file = "D:/dusheyko/train_model_data/kosmos_valid.jsonl")
result$summary
json_data_frame <- as.data.frame(result)



tmp <- readLines("full.txt", encoding = "UTF-8")
tmp2 = data.frame(tmp)
tmp2$all = NA
i=1
j=1
tmp2$tmp[1]
x0 = unlist(strsplit(tmp2$tmp[1], " "))
grepl("вот_",x0) 
for (i in 1:length(x0)) {
  if (grepl("вот_",x0[i]) == TRUE){
    x = x0[i]
  }
}

for (i in 1:length(tmp2$tmp)) {
  x = unlist(strsplit(tmp2$tmp[i], " "))
  j=1
  for (j in 1:length(x)) {
    if (grepl("вот_",x[j]) == TRUE){
      tmp2$all[i] = x[j]
  }
  }
}  
}

tmp2$all <- gsub("…", "", tmp2$all)
tmp2 = tmp2[-256,]
write.csv(tmp2, "vot.csv")

set.seed(1)
a <- createDataPartition(tmp2$all, p = 0.8, list=FALSE)
training <- tmp2[a,]
test <- tmp2[-a,]

test$tmp <- gsub("вот_plus", "", test$tmp)
write.csv(training, "vottrain.csv")

out = read.csv("out_tok.csv", sep=",", encoding = "UTF-8")
# out$predicted = unlist(strsplit(out$predicted, " "))
out$predicted=gsub("\\[)", "", out$predicted)
out$predicted=gsub("\\])", "", out$predicted)
out$predicted =gsub("'", "", out$predicted) 
out$predicted =gsub("[0-9]", "", out$predicted) 
out$predicted =gsub("\\[", "", out$predicted) 
out$predicted =gsub("\\]", "", out$predicted) 
out$predicted =gsub("\\.", "", out$predicted) 
out$predicted =gsub(")", "", out$predicted) 
out$predicted =gsub("\\(", "", out$predicted) 
out$predicted =gsub(" ", "", out$predicted) 
out$predicted =gsub("-", "", out$predicted)
out$predicted =gsub(",", "", out$predicted)
out$predicted <-  as.factor(out$predicted)

out$all <-  as.factor(out$all)
table(out$all)
table(out$predicted)
out$all <- as.factor(out$all)
library(caret)
cm <- confusionMatrix(out$all, out$predicted, mode = "everything")
library(ConfusionTableR)
mc_df <- multi_class_cm(out$all, out$predicted,
                                         mode="everything")


library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL 
Server};server=localhost;database=dims;trusted_connection=true')
res <- sqlQuery(dbhandle, 'select * from dbo.Lexeme')




system(paste('"c:/Program Files/Mozilla Firefox/firefox.exe"',
             '-url cran.r-project.org'), wait = FALSE)

system('D:/mystem/mystem.exe -nid D:/mystem/test.txt D:/mystem/0test.txt', wait = T)

files = c("test1", "test2")


x=readline()
print(paste("Hello, ", x))

link = "www.vtomske.ru/sport/34234$p="
pg = "1"
paste0(link, pg, collapse = "")

system('mystem.exe -ld test2.txt stemmed/0_test2.txt')
length(files)
for (i in 1:length(files)){
  print(paste0('mystem.exe -ld ', files[i], '.txt', ' ', 'stemmed/', '0_', files[i], '.txt'),wait = T)
}


library(quanteda)

txt1 <- "круто я все понимать"
txt2 <- "ехать ужасный паз"
df = data.frame(text=c(txt1,txt2))
emot <- dictionary(list(pos=c("круто","здорово","веселый","прикольный"), 
                   neg=c("пдлохой","ужасный","паз","стремный")))

crp <- corpus(df)

mydfm <- tokens(crp) %>%
  dfm()%>%
  dfm_weight(scheme = "prop")%>%
  dfm_select(pattern = emot)
df2 = convert(mydfm, to="data.frame")
library(readxl)
exl = read_xlsx("babenko.xlsx")

vecP <- as.character(exl$word[exl$X=='pos'])
vecN <- as.character(exl$word[exl$X=='neg'])
emot <- dictionary(list(pos=c(vecP), 
                        neg=c(vecN)))
mydfm <- tokens(crp) %>%
  dfm()%>%
  dfm_weight(scheme = "prop")%>%
  dfm_select(pattern = emot)

library(readtext)
txt_pol = readtext(paste0( "D:/topici/politics/lemm/*.txt"),
               docvarsfrom = "filenames", 
               docvarnames = c("class", "pref", "lemm", "purl"),
               dvsep = "_", encoding = "UTF-8")
txt_pol = txt_pol[,-4]
txt_acc = readtext(paste0( "D:/topici/accidents/lemm/*.txt"),
              docvarsfrom = "filenames", 
              docvarnames = c("class", "lemm", "purl"),
              dvsep = "_", encoding = "UTF-8")
txt_soc = readtext(paste0( "D:/topici/society/lemm/*.txt"),
                  docvarsfrom = "filenames", 
                  docvarnames = c("class", "lemm", "purl"),
                  dvsep = "_", encoding = "UTF-8")
colnames(txt_pol)
colnames(txt_acc)
colnames(txt_soc)

txt = rbind(txt_pol, txt_soc)
txt$text=gsub('\\{', '', txt$text)
txt$text=gsub('\\}', '', txt$text)
txt$text=gsub('\\?', '', txt$text)
txt$text=gsub('\\:', '', txt$text)
txt$text=gsub('\\"', '', txt$text)
txt$text=gsub('[[:digit:]]', '', txt$text)
txt$text=gsub('-', '', txt$text)
#txt$text=gsub('[:upper:]', '', txt$text)
library(quanteda)
# quanteda_options("threads" = 10)
crp = corpus(txt)
summary(crp)
sum(ntoken(crp))
#docvars(crp, "Year") <- 2010
tks = tokens(crp, what = "word", remove_numbers = FALSE, remove_punct = FALSE, remove_separators = FALSE)
tokeninfo <- summary(crp)
tokeninfo$Year <- docvars(crp, "Year")
with(tokeninfo, plot(Year, Tokens, type = "b", pch = 19, cex = .7))


kwic(tks, pattern = "россия")
kwic(tks, pattern = "д?м", valuetype = "regex")

kwic(tks, pattern = phrase("россия федерация")) %>%
  head()

pol = corpus_subset(crp, class == "pol")
soc = corpus_subset(crp, class == "soc")


dfmat <- crp %>%
  tokens() %>%
  dfm()
dfmatdf = convert(dfmat[,1:10], to = "data.frame")
dfmatdf$class = txt$class

library("quanteda.textplots")
set.seed(23)
dfmat_inaug <- corpus_subset(crp, class == "pol") %>%
  tokens(remove_punct = TRUE)  %>%
  tokens_remove(pattern = stopwords('russian'))  %>%
  dfm()  %>%
  dfm_trim(min_termfreq = 5, max_termfreq = 80, verbose = FALSE)
textplot_wordcloud(dfmat_inaug)


corpus_subset(crp, 
              class %in% c("pol", "soc")) %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("russian")) %>%
  dfm() %>%
  dfm_group(groups = class) %>%
  dfm_trim(min_termfreq = 5, verbose = FALSE) %>%
  textplot_wordcloud(comparison = TRUE)


textplot_wordcloud(dfmat_inaug, min_count = 10,
                   color = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))


kwic(pol, pattern = "рф") %>%
  textplot_xray()

textplot_xray(
  kwic(pol, pattern = "человек"),
  kwic(pol, pattern = "страна"),
  kwic(pol, pattern = "демократия"),
  kwic(pol, pattern = "закон"),
  scale = "absolute"
)

png("123456789.png")
textplot_xray(
  kwic(tokens(pol), pattern = "страна"),
  kwic(tokens(pol), pattern = "закон")
) 
dev.off()


library("quanteda.textstats")
tstat_freq_inaug <- textstat_frequency(dfmat_inaug, n = 20)

library(ggplot2)
ggplot(tstat_freq_inaug, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() + 
  labs(x = "Frequency", y = "Feature")


freq_grouped <- textstat_frequency(dfm(tks), 
                                   groups = class)

# Filter the term "american"
freq_american <- subset(freq_grouped, freq_grouped$feature %in% "тасс")  

ggplot(freq_american, aes(x = frequency, y = group)) +
  geom_point() + 
  scale_x_continuous(limits = c(0, 14), breaks = c(seq(0, 14, 2))) +
  labs(x = "Frequency", y = NULL,
       title = "Frequency of 'РФ'")

dfmat_weight_pres <- crp %>%
  
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("russian")) %>%
  dfm() %>%
  dfm_weight(scheme = "prop")

# Calculate relative frequency by president
dat_freq_weight <- textstat_frequency(dfmat_weight_pres, n = 15, 
                                      groups = class)

ggplot(data = dat_freq_weight, aes(x = nrow(dat_freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(dat_freq_weight):1,
                     labels = dat_freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")



corp_pres <- corpus_subset(crp, 
                           class %in% c("pol", "acc"))

# Create a dfm grouped by president
dfmat_pres <- tokens(corp_pres, remove_punct = TRUE) %>%
  tokens_remove(c(stopwords("russian"), "это")) %>%
  tokens_group(groups = class) %>%
  dfm()

# Calculate keyness and determine pol as target group
tstat_keyness <- textstat_keyness(dfmat_pres, target = "pol")

# Plot estimated word keyness
textplot_keyness(tstat_keyness) 
textplot_keyness(tstat_keyness, show_reference = FALSE)


library("quanteda.textmodels")

# Transform corpus to dfm
dfmat_ie <- pol %>% 
  tokens() %>% 
  dfm()

# Set reference scores
refscores <- c(rep(NA, 4), 1, -1, rep(NA, 8))

# Predict Wordscores model
tmod_ws <- textmodel_wordscores(dfmat_ie, y = refscores, smooth = 1)

# Plot estimated word positions (highlight words and print them in red)
textplot_scale1d(tmod_ws,
                 highlighted = c("minister", "have", "our", "budget"), 
                 highlighted_color = "red")


dfmat_tag <- dfm_select(dfmat_ie)
toptag <- names(topfeatures(dfmat_tag, 50))
head(toptag)

library("quanteda.textplots")
fcmat_tag <- fcm(dfmat_tag)
head(fcmat_tag)
fcmat_topgat <- fcm_select(fcmat_tag, pattern = toptag)
textplot_network(fcmat_topgat, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5)
