

library(sophistication)
library(quanteda)
library(stringi)
library(magrittr)
library(dplyr)

##Figures 4, 6 and 8: Comparative data

SOTU_stat <- textstat_readability(data_corpus_SOTU)

data("data_corpus_SOTU")


SOTU_stat <- textstat_readability(data_corpus_SOTU, measure = c("Flesch", "meanSentenceLength", "meanWordSyllables"))

SOTU_year <- lubridate::year(docvars(data_corpus_SOTU, "Date"))
SOTU_fre_df <- data.frame("year" = SOTU_year, "SOTU_stat" = SOTU_stat$Flesch)
SOTU_fre_sent <- data.frame("year" = SOTU_year, "SOTU_stat" = SOTU_stat$meanSentenceLength)
SOTU_fre_word <- data.frame("year" = SOTU_year, "SOTU_stat" = SOTU_stat$meanWordSyllables)

########nobel
#load("data_text/NobelLitePresentations/data_corpus_nobel.rdata")
load("C:/Users/kevin/Desktop/data_corpus_nobel.rdata")


temp_lengths <- stri_length(texts(data_corpus_nobel))
data_corpus_nobel <- corpus_subset(data_corpus_nobel, temp_lengths < quantile(temp_lengths, prob = .95))


nobel_corp <- corpus_trimsentences(data_corpus_nobel, min_length = 4)


nobel_lengths<-ntoken(nobel_corp, removePunct = T)



docvars(data_corpus_nobel)


nobel_stat <- textstat_readability(nobel_corp, measure = c("Flesch", "meanSentenceLength", "meanWordSyllables"))

nobel_year <- (docvars(data_corpus_nobel, "year"))
nobel_fre_df <- data.frame("year" = nobel_year, "nobel_stat" = nobel_stat$Flesch)
nobel_fre_sent <- data.frame("year" = nobel_year, "nobel_stat" = nobel_stat$meanSentenceLength)
nobel_fre_word <- data.frame("year" = nobel_year, "nobel_stat" = nobel_stat$meanWordSyllables)





########Party Broadcasts

data("data_corpus_partybroadcasts")
temp_lengths <- stri_length(texts(data_corpus_partybroadcasts))
data_corpus_pb <- corpus_subset(data_corpus_partybroadcasts, temp_lengths < quantile(temp_lengths, prob = .95))

pb_corp <- corpus_trimsentences(data_corpus_pb, min_length = 4)

pb_lengths<-ntoken(pb_corp, removePunct = T)

###need to manually fix the dates
xx<-substr(texts(pb_corp), 1, 30)
xxx<-str_extract_all(xx,"\\(?[0-9,.]+\\)?")


pb_year <- numeric()
for(i in 1:length(xxx)){
  
  pb_year[i]<- as.numeric((xxx[[i]][length(xxx[[i]])]))
}
pb_year[4]<-1964

pb_year[10]<-1970

pb_year[14]<-1974
pb_year[22]<-1979



pb_stat <- textstat_readability(pb_corp, measure = c("Flesch", "meanSentenceLength", "meanWordSyllables"))


pb_fre_df <- data.frame("year" = pb_year, "pb_stat" = pb_stat$Flesch)
pb_fre_sent <- data.frame("year" = pb_year, "pb_stat" = pb_stat$meanSentenceLength)
pb_fre_word <- data.frame("year" = pb_year, "pb_stat" = pb_stat$meanWordSyllables)



########UK Manifestos

load("C:/Users/kevin/Desktop/data_corpus_man.rdata")

temp_lengths <- stri_length(texts(data_corpus_man))
data_corpus_man <- corpus_subset(data_corpus_man, temp_lengths < quantile(temp_lengths, prob = .95))


man_corp <- corpus_trimsentences(data_corpus_man, min_length = 4)


docvars(data_corpus_man)


man_stat <- textstat_readability(man_corp, measure = c("Flesch", "meanSentenceLength", "meanWordSyllables"))

man_year <- (docvars(data_corpus_man, "Year"))
man_fre_df <- data.frame("year" = man_year, "man_stat" = man_stat$Flesch)
man_fre_sent <- data.frame("year" = man_year, "man_stat" = man_stat$meanSentenceLength)
man_fre_word <- data.frame("year" = man_year, "man_stat" = man_stat$meanWordSyllables)



##melt together for plotting--Figure 4
require(reshape2)


df_comp<-melt(list(SOTU=SOTU_fre_df, Manifestos=man_fre_df, Broadcasts = pb_fre_df,
                 Nobel = nobel_fre_df), id.vars="year")



require(ggplot2)

linez<-c( "F1", "dashed",  "dotdash","solid")


p <- ggplot(data = df_comp,
            aes(x = year, y = value, linetype = L1)) + #, group = delivery)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_smooth(alpha=0.2,  method = "loess", span = .34, color = "black", se = F) +
  geom_point(alpha=0.01) + 
  coord_cartesian(ylim = c(15, 70), xlim=c(1900, 2010)) +
  theme(legend.position="none")+
  scale_linetype_manual(values = linez)+
  xlab("") +
  ylab("Flesch Reading Ease Score") +
  #geom_line(aes(), alpha=0.3, size = 1) +
  # ggtitle("Text Complexity in State of the Union Addresses") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  annotate("text", label = "SOTU", x = 1995, y = 55, size = 6, colour = "black")+
  annotate("text", label = "UK Manifestos", x = 1925, y = 30, size = 6, colour = "black") +
  annotate("text", label = "Party Broadcasts", x = 1955, y = 65, size = 6, colour = "black") + 
  annotate("text", label = "Nobel Prize", x = 1910, y = 48, size = 6, colour = "black")


print(p)



##melt together for plotting--Figure 6



df_comp_sent<-melt(list(SOTU=SOTU_fre_sent, Manifestos=man_fre_sent, Broadcasts = pb_fre_sent,
                   Nobel = nobel_fre_sent), id.vars="year")



linez<-c( "F1", "dashed",  "dotdash","solid")



p <- ggplot(data = df_comp_sent,
            aes(x = year, y = value, linetype = L1)) + #, group = delivery)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_smooth(alpha=0.2,  method = "loess", span = .34, color = "black", se = F) +
  coord_cartesian(ylim = c(15, 35), xlim=c(1900, 2010)) +
  theme(legend.position="none")+
  scale_linetype_manual(values = linez)+
  xlab("") +
  ylab("Mean Sentence Length") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  annotate("text", label = "SOTU", x = 1912, y = 34, size = 6, colour = "black")+
  annotate("text", label = "UK Manifestos", x = 2005, y = 27, size = 6, colour = "black") +
  annotate("text", label = "Party Broadcasts", x = 1965, y = 15, size = 6, colour = "black") + 
  annotate("text", label = "Nobel Prize", x = 1910, y = 26, size = 6, colour = "black")



print(p)


##melt together for plotting--Figure 8



df_comp_word<-melt(list(SOTU=SOTU_fre_word, Manifestos=man_fre_word, Broadcasts = pb_fre_word,
                        Nobel = nobel_fre_word), id.vars="year")



p <- ggplot(data = df_comp_word,
            aes(x = year, y = value, linetype = L1)) + #, group = delivery)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_smooth(alpha=0.2,  method = "loess", span = .34, color = "black", se = F) +
  #geom_point(alpha=0.01) + 
  # coord_cartesian(ylim = c(1.45, 1.85)) +
  # coord_cartesian(ylim = c(-25, 70)) +
  coord_cartesian(ylim = c(1.4, 1.75), xlim=c(1900, 2010)) +
  theme(legend.position="none")+
  scale_linetype_manual(values = linez)+
  #scale_colour_manual(values = cols, guide = guide_legend(title = "Source"))+
  xlab("") +
  ylab("Mean Number of Syllables per Word") +
  #geom_line(aes(), alpha=0.3, size = 1) +
  # ggtitle("Text Complexity in State of the Union Addresses") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  annotate("text", label = "SOTU", x = 1903, y = 1.66, size = 6, colour = "black")+
  annotate("text", label = "UK Manifestos", x = 1985, y = 1.72, size = 6, colour = "black") +
  annotate("text", label = "Party Broadcasts", x = 1955, y = 1.45, size = 6, colour = "black") + 
  annotate("text", label = "Nobel Prize", x = 1910, y = 1.58, size = 6, colour = "black")



print(p)
