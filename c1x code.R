setwd("C:\\intro_to_datascience\\competitions\\c1x")
df<- read.csv("user-ct-test-collection-02.txt", header= TRUE, sep='\t')
str(df)
df1<- subset(df,ItemRank!= 'NA')
df[df=='NA']<- NA
df2<- subset(df,is.na(df$ItemRank))
str(df1)
str(df2)
df1<- unique(df1)
df2<- unique(df2)
nrow(df1)
table(df1$ItemRank)
ranktab<-round(prop.table(table(df1$ItemRank))*100)

install.packages("data.table")
install.packages("dplyr")
install.packages("plyr")

remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)

library(data.table)
library(plyr)
library(dplyr)

dtbl<- tbl_df(df)

dtbl3<- dtbl %>% group_by(AnonID) %>% summarise(n=n_distinct(Query))
head(dtbl3,10)
val<- dtbl3[,2]
querytab<- round(prop.table(table(val))*100)
querytab<- as.data.frame(querytab)
querytab1<- querytab[1:50,]
querytab1
querytbl<- tbl_df(querytab1)
mutate(querytbl,cumulative=cumsum(querytbl$Freq))
ggplot(querytab1,aes(x=val,y=Freq))+geom_bar(stat = "identity")




library("magrittr")

#ggplot(dtbl3,aes(n))+geom_histogram
#ggplot()+aes(val)+geom_histogram(binwidth=50)+xlim(1,500)
library(ggplot2)
#hist(dtbl3$n,xlim = range(0,500))


dbtl4<- dtbl %>% select(AnonID,ItemRank)
dbtl4$rank<-dbtl4$ItemRank
dbtl4[dbtl4=='NA']<- NA

dbtl4$rank<- apply(dbtl4[,c("ItemRank")],1, function(i) ifelse((is.na(i)),'NA','CL'))
head(dbtl4,15)        

dbtl6<- dbtl4 %>% group_by(AnonID,rank) %>% summarise(n=n())
head(dbtl6)

library(tidyr)
dbtl7<- spread(dbtl6,rank,n)
head(dbtl7)
nrow(dbtl7)

dbtl9<- dbtl7[which(!is.na(dbtl7$CL)),]
nrow(dbtl9)
a<- data.frame(name=c("ID","Click","NoClick"))
colnames(dbtl9)<- a$name
dbtl10<- dbtl9 %>% mutate(Total = Click+NoClick) %>% mutate(clrate= round((Click/Total)*100)) %>% mutate(nonclrate= round((NoClick/Total)*100))

hist(dbtl10$clrate,breaks = 2)
hist(dbtl10$nonclrate)
mean(dbtl10$clrate,na.rm = TRUE)
mean(dbtl10$nonclrate,na.rm = TRUE)


dbtl8<- dbtl7[which(is.na(dbtl7$CL)),]
nrow(dbtl8)
dbtl8<- as.data.frame(dbtl8)
a<- data.frame(name=c("ID","Click","NoClick"))
colnames(dbtl8)<- a$name 
head(dbtl8)

tabdf<- table(dbtl8$NoClick)
tabdf<- as.data.frame(tabdf)
tabdf$Var1<- as.numeric(tabdf$Var1)
tabdf1<- tabdf[which(tabdf$Var1<=50),]
ggplot(tabdf1,aes(x=tabdf1$Var1,y=Freq))+geom_bar(stat = "identity")+geom_text(label = tabdf1$Freq)

#for click url study
str(df1)

dtb1<- tbl_df(df1)
dtb2<- count(dtb1,ClickURL)
head(dtb2)
dtb2<- arrange(dtb2,desc(n))
head(dtb2,10)

# time
df4<- df
dbtt3<- tbl_df(df4)
str(dbtt3)
dbtt3$Date<- as.Date(dbtt3$QueryTime)
dbtt3$Time<- format(as.POSIXct(dbtt3$QueryTime),format = "%H:%M:%S")

dbtt3 %>% group_by(AnonID,Date) %>% summarise(min(Time),max(Time))->dbtt4
head(dbtt4)
str(dbtt4)
library("chron")

dbtt4$MINTime<- chron(times=dbtt4$`min(Time)`)
dbtt4$MAXTime<- chron(times=dbtt4$`max(Time)`)        
dbtt4$timespent<- dbtt4$MAXTime-dbtt4$MINTime
head(dbtt4)
dim(dbtt4)


dbtt4$minutesspent<- 60*24* as.numeric(times(dbtt4$timespent))
head(dbtt4)
dbtt4$minutesspent<- round(dbtt4$minutesspent,0)
hist(dbtt4$minutesspent)

# query words that not leads to a click

rec<-df2$Query
head(rec)

library(tm)
library("wordcloud")
library("qdap")
rec_source<- VectorSource(rec)
rec_corpus<- VCorpus(rec_source)
rec_corpus[[1]][1]


clean_corpus<- function(corpus){
        corpus<- tm_map(corpus, content_transformer(replace_abbreviation))
        corpus<- tm_map(corpus, removePunctuation)
        corpus<- tm_map(corpus, removeNumbers)
        corpus<- tm_map(corpus, removeWords, c(stopwords("en"),"hpe"))
        corpus<- tm_map(corpus, content_transformer(tolower))
        return(corpus)
}

clean_rec<- clean_corpus(rec_corpus)
clean_rec[[1]][1]

rec_tdm<- TermDocumentMatrix(clean_rec)
rec_tdm

rec_m<- as.matrix(rec_tdm)
term_freqs<- rowSums(rec_m)
term_freqs

term_freqs<- sort(term_freqs,decreasing = TRUE)

wordfreq<- data.frame(term= names(term_freqs), num= term_freqs)
wordcloud(wordfreq$term,wordfreq$num, max.words = 100)

#query words that leads to a click
rec1<-df1$query
head(rec1)

library(tm)
library("wordcloud")
library("qdap")
rec_source1<- VectorSource(rec1)
rec_corpus1<- VCorpus(rec_source1)
rec_corpus1[[1]][1]


clean_corpus1<- function(corpus){
        corpus<- tm_map(corpus, content_transformer(replace_abbreviation))
        corpus<- tm_map(corpus, removePunctuation)
        corpus<- tm_map(corpus, removeNumbers)
        corpus<- tm_map(corpus, removeWords, c(stopwords("en"),"hpe"))
        corpus<- tm_map(corpus, content_transformer(tolower))
        return(corpus)
}

clean_rec1<- clean_corpus1(rec_corpus1)
clean_rec1[[1]][1]

rec_tdm1<- TermDocumentMatrix(clean_rec1)
rec_tdm1

rec_m1<- as.matrix(rec_tdm1)
term_freqs1<- rowSums(rec_m1)
term_freqs1

term_freqs1<- sort(term_freqs1,decreasing = TRUE)

wordfreq1<- data.frame(term= names(term_freqs1), num= term_freqs1)
wordcloud(wordfreq1$term,wordfreq1$num, max.words = 100)








#practise code below
a<- "manikandan"
nchar(a)

df3<- df
str(df3)
df3$Query<- as.character(df3$Query)
df3$querylength<- sapply(df3[,c("Query")], function(x) nchar(x))
head(df3)
df3[is.na(df3)] <- 5000
dbtt1<- tbl_df(df3)
dbtt1 %>% group_by(ItemRank) %>% summarise(mean(querylength))-> dbtt2
head(dbtt2)
tail(dbtt2)


ggplot(df3,aes(x=ItemRank,y=querylength))+geom_jitter()


library("tidyr")
install.packages("ggplot2")
library(ggplot2)

ranktabdf<- as.data.frame(ranktab)
str(ranktabdf)
ranktabdf1<- ranktabdf[2:16,]
ggplot(ranktabdf1,aes(x= Var1,y= Freq))+geom_bar(stat = "identity")





library(plyr)
c11<-count(df1,c("Query"))
head(c11)
C11<-sort(c11,decreasing = TRUE)
tail(c11)
prop.table(df1$QueryTime)


