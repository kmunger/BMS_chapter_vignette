
##Figures 3, 5 and 7: US data


library(sophistication)
library(quanteda)
library(stringi)
library(magrittr)
library(dplyr)

data("data_corpus_SOTU")


SOTU_stat <- textstat_readability(data_corpus_SOTU, measure = c("Flesch", "meanSentenceLength", "meanWordSyllables"))

SOTU_year <- lubridate::year(docvars(data_corpus_SOTU, "Date"))
SOTU_fre_df <- data.frame("year" = SOTU_year, "SOTU_stat" = SOTU_stat$Flesch)
SOTU_fre_sent <- data.frame("year" = SOTU_year, "SOTU_stat" = SOTU_stat$meanSentenceLength)
SOTU_fre_word <- data.frame("year" = SOTU_year, "SOTU_stat" = SOTU_stat$meanWordSyllables)


###EO

data("data_corpus_eo")


#drop the ones that are empty
clean1 <- data_corpus_eo$documents



##problems
clean1$texts<-stri_replace_all_fixed(clean1$texts, "A. D.", "AD")




#drop anything that's very long (95th percentile and above)
quant95 <- which( nchar(clean1$texts) >
                    quantile( nchar(clean1$texts), prob=.95) )
clean2 <- clean1[-quant95, c(1:2)]





eo_corp <- corpus(clean2, text_field = "texts")

##drop very short sentences
eo_corp <- corpus_trimsentences(eo_corp, min_length = 4)

eo_corp <- corpus_subset(eo_corp, ntoken(eo_corp) > 10)

eo_year <- docvars(eo_corp)


eo_stat <- textstat_readability(eo_corp$documents$texts, measure = c("Flesch", "meanSentenceLength", "meanWordSyllables"))

eo_fre_df <- data.frame("year" = eo_year$Year, "eo_stat" = eo_stat$Flesch)
eo_fre_sent <- data.frame("year" = eo_year$Year, "eo_stat" = eo_stat$meanSentenceLength)
eo_fre_word <- data.frame("year" = eo_year$Year, "eo_stat" = eo_stat$meanWordSyllables)


########SCOTUS
load("C:/Users/kevin/Documents/GitHub/BMS_chapter_replication/data/data_corpus_SCOTUS.rda")

#drop the ones that are empty
clean1 <- corpus_subset(data_corpus_SCOTUS, nchar(texts(data_corpus_SCOTUS)) > 0)


## implement fixes
texts(clean1) <- stri_replace_all_fixed(texts(clean1), "U. S.", "US")



#drop anything that's very long (95th percentile and above)
temp_lengths <- stri_length(texts(clean1))
clean2 <- corpus_subset(clean1, temp_lengths <quantile(temp_lengths, prob = .95))
scotus_lengths<-ntoken(clean2, removePunct = T)

clean3 <- corpus_trimsentences(clean2, min_length = 4)

scotus_year <- clean3$documents$Year

scotus_stat <- textstat_readability(clean3$documents$texts, measure = c("Flesch", "meanSentenceLength", "meanWordSyllables"))

scotus_fre_df <- data.frame("year" = scotus_year, "scotus_stat" = scotus_stat$Flesch)
scotus_fre_sent <- data.frame("year" = scotus_year, "scotus_stat" = scotus_stat$meanSentenceLength)
scotus_fre_word <- data.frame("year" = scotus_year, "scotus_stat" = scotus_stat$meanWordSyllables)






##too many to plot well, and very unbalanced (way more modern ones)--try for a more balanced sample

years<-unique(scotus_fre_df$year)


indices<-vector()
for(i in 1:length(years)){
  set<-which(scotus_fre_df[,"year"]==years[i])
  num<-min(length(set), 30)
  samp<-sample(set, num, replace = FALSE)
  indices<-c(indices, samp)
}


balanced_scotus_fre_df<-scotus_fre_df[indices,]
balanced_scotus_fre_sent<-scotus_fre_sent[indices,]
balanced_scotus_fre_word<-scotus_fre_word[indices,]




require(reshape2)


###Congress
load("C:/Users/kevin/Documents/GitHub/BMS_chapter_replication/data/data_corpus_CR.rdata")


#drop the ones that are by the speaker
speaker <- which( (data_corpus_CR$documents$name)=="Speaker" )
clean1 <- data_corpus_CR$documents[-speaker, ]



#drop anything that's very long (90th percentile and above)
quant95 <- which( nchar(clean1$texts) >
                    quantile( nchar(clean1$texts), prob=.95) )
clean2 <- clean1[-quant95, ]


##sync up year format

clean2$year[clean2$year==95]<-1995
clean2$year[clean2$year==96]<-1996
clean2$year[clean2$year==97]<-1997
clean2$year[clean2$year==98]<-1998
clean2$year[clean2$year==99]<-1999
clean2$year[clean2$year==0]<-2000
clean2$year[clean2$year==1]<-2001
clean2$year[clean2$year==2]<-2002
clean2$year[clean2$year==3]<-2003
clean2$year[clean2$year==4]<-2004
clean2$year[clean2$year==5]<-2005
clean2$year[clean2$year==6]<-2006
clean2$year[clean2$year==7]<-2007
clean2$year[clean2$year==8]<-2008


##take a very small sample--can't run readability on the whole thing
sample<-sample(seq(1,length(clean2$texts)), 10000, replace = F  )


sample_data<-clean2[sample,]


congress_year <- sample_data$year


congress_stat <- textstat_readability(sample_data$texts, measure = c("Flesch", "meanSentenceLength", "meanWordSyllables"))

congress_fre_df <- data.frame("year" = congress_year, "congress_stat" = congress_stat$Flesch)
congress_fre_sent <- data.frame("year" = congress_year, "congress_stat" = congress_stat$meanSentenceLength)
congress_fre_word <- data.frame("year" = congress_year, "congress_stat" = congress_stat$meanWordSyllables)




##melt together for plotting--Figure 3
require(reshape2)


df_US<-melt(list(SOTU=SOTU_fre_df, SCOTUS=balanced_scotus_fre_df, Congress = congress_fre_df,
                 ExecOrders = eo_fre_df), id.vars="year")



require(ggplot2)

linez<-c( "F1", "dashed",  "dotdash","solid")

p <- ggplot(data = df_US,
            aes(x = year, y = value, linetype = L1)) + #, group = delivery)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_smooth(alpha=0.2,  method = "loess", span = .34, color = "black", se = F) +
  coord_cartesian(ylim = c(-20, 65)) +
  theme(legend.position="none", axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15))+
  scale_linetype_manual(values = linez)+
  xlab("") +
  ylab("Flesch Reading Ease Score") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  annotate("text", label = "Congress Speech", x = 1995, y = 45, size = 6, colour = "black")+
  
  annotate("text", label = "SOTU", x = 1800, y = 20, size = 6, colour = "black")+
  annotate("text", label = "SCOTUS", x = 1810, y = 50, size = 6, colour = "black") +
  annotate("text", label = "Executive Orders", x = 1970, y = 0, size = 6, colour = "black")


print(p)



##melt together for plotting--Figure 5


df_US_sent<-melt(list(SOTU=SOTU_fre_sent, SCOTUS=balanced_scotus_fre_sent, Congress = congress_fre_sent,
                 ExecOrders = eo_fre_sent), id.vars="year")




linez<-c( "F1", "dashed",  "dotdash","solid")


p <- ggplot(data = df_US_sent,
            aes(x = year, y = value, linetype = L1)) + #, group = delivery)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_smooth(alpha=0.2,  method = "loess", span = .34, color = "black", se = F) +

  coord_cartesian(ylim = c(15, 65)) +
  theme(legend.position="none", axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15))+
  scale_linetype_manual(values = linez)+
  xlab("") +
  ylab("Mean Sentence Length") +

  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  annotate("text", label = "Congress Speech", x = 1960, y = 17, size = 6, colour = "black")+
  annotate("text", label = "SOTU", x = 1910, y = 25, size = 6, colour = "black")+
  annotate("text", label = "SCOTUS", x = 1810, y = 30, size = 6, colour = "black") +
  annotate("text", label = "Executive Orders", x = 1960, y = 50, size = 6, colour = "black")


print(p)


##melt together for plotting--Figure 7


df_US_word<-melt(list(SOTU=SOTU_fre_word, SCOTUS=balanced_scotus_fre_word, Congress = congress_fre_word,
                      ExecOrders = eo_fre_word), id.vars="year")


p <- ggplot(data = df_US_word,
            aes(x = year, y = value, linetype = L1)) + #, group = delivery)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_smooth(alpha=0.2,  method = "loess", span = .34, color = "black", se = F) +
coord_cartesian(ylim = c(1.4, 1.85)) +

theme(legend.position="none", axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15))+
  scale_linetype_manual(values = linez)+
  xlab("") +
  ylab("Mean Number of Syllables per Word") + theme(plot.title = element_text(lineheight=.8, face="bold")) +
  annotate("text", label = "SOTU", x = 1800, y = 1.73, size = 6, colour = "black")+
  annotate("text", label = "SCOTUS", x = 1810, y = 1.47, size = 6, colour = "black") +
  annotate("text", label = "Con. Speech", x = 1998, y = 1.61, size = 6, colour = "black")+
  
  annotate("text", label = "Executive Orders", x = 1950, y = 1.82, size = 6, colour = "black")

print(p)
