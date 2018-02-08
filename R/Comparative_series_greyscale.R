## Trend analysis on SOTU corpus for book chapter
##
##

########nobel
#load("data_text/NobelLitePresentations/data_corpus_nobel.rdata")
load("C:/Users/kevin/Desktop/data_corpus_nobel.rdata")


temp_lengths <- stri_length(texts(data_corpus_nobel))
data_corpus_nobel <- corpus_subset(data_corpus_nobel, temp_lengths < quantile(temp_lengths, prob = .95))


nobel_corp <- corpus_trimsentences(data_corpus_nobel, min_length = 4)


nobel_lengths<-ntoken(nobel_corp, removePunct = T)






nobel_stat <- textstat_readability(nobel_corp, measure = c("Flesch", "meanSentenceLength", "meanWordSyllables"))

nobel_year <- lubridate::year(docvars(data_corpus_nobel, "Date"))
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



###combine all together


require(reshape2)
df1<-melt(list( SOTU=SOTU_df, 
              Nobel = nobel_df, 
              Manifestos = man_df, Broadcasts = pb_df    ), id.vars="year")




df1<-filter(df1, is.na(df1$value)==F)

##

#df1<-filter(df1, value < 150)
#df1<-filter(df1, value > -100)




require(ggplot2)

linez<-c("twodash", "dotted", "dashed", "F1", 
         "dotdash", "solid")

p <- ggplot(data = df1,
            aes(x = year, y = value, linetype = L1)) + #, group = delivery)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    geom_smooth(alpha=0.2,  method = "loess", span = .34, color = "black", se = F) +
  geom_point(alpha=0.01) + 
 # coord_cartesian(ylim = c(1.45, 1.85)) +
   coord_cartesian(ylim = c(-25, 70)) +
  #coord_cartesian(ylim = c(15, 40), xlim=c(1900, 2010)) +
 theme(legend.position="none")+
    scale_linetype_manual(values = linez)+
#scale_colour_manual(values = cols, guide = guide_legend(title = "Source"))+
    xlab("") +
    ylab(measure) +
    #geom_line(aes(), alpha=0.3, size = 1) +
    # ggtitle("Text Complexity in State of the Union Addresses") +
    theme(plot.title = element_text(lineheight=.8, face="bold")) +
annotate("text", label = "SOTU", x = 1800, y = 22, size = 6, colour = "black")+
annotate("text", label = "SCOTUS", x = 1800, y = 49, size = 6, colour = "black") +
  annotate("text", label = "Executive Orders", x = 1970, y = -10, size = 6, colour = "black") +
  annotate("text", label = "UK Manifestos", x = 1960, y = 30, size = 6, colour = "black") +
   annotate("text", label = "Party Broadcasts", x = 1925, y = 65, size = 6, colour = "black") + 
    annotate("text", label = "Nobel Prize", x = 1900, y = 48, size = 6, colour = "black")
  


print(p)


pdf(file="C:/Users/kevin/OneDrive/Documents/GitHub/sophistication/manuscript_chapter/plots//all_series_greyscale.pdf", height=7, width=12)
print(p)

#print(p)
dev.off()
###################syllables per word 




p <- ggplot(data = df1,
            aes(x = year, y = value, linetype = L1)) + #, group = delivery)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_smooth(alpha=0.2,  method = "loess", span = .34, color = "black", se = F) +
  geom_point(alpha=0.01) + 
  coord_cartesian(ylim = c(1.45, 1.85)) +
  #coord_cartesian(ylim = c(15, 40), xlim=c(1900, 2010)) +
  theme(legend.position="none")+
  scale_linetype_manual(values = linez)+
  #scale_colour_manual(values = cols, guide = guide_legend(title = "Source"))+
  xlab("") +
  ylab(measure) +
  #geom_line(aes(), alpha=0.3, size = 1) +
  # ggtitle("Text Complexity in State of the Union Addresses") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ 
  annotate("text", label = "SOTU", x = 2005, y = 1.55, size = 6, colour = "black")+
  annotate("text", label = "SCOTUS", x = 1820, y = 1.45, size = 6, colour = "black") +
  annotate("text", label = "Pres Proclamations", x = 1860, y = 1.56, size = 6, colour = "black") + 
  annotate("text", label = "Executive Orders", x = 1970, y = 1.83, size = 6, colour = "black")
#  annotate("text", label = "UK Manifestos", x = 1985, y = 1.73, size = 6, colour = "black") +
#   annotate("text", label = "Party Broadcasts", x = 1950, y = 1.42, size = 6, colour = "black") + 
#  annotate("text", label = "Nobel Prize", x = 1925, y = 1.56, size = 6, colour = "black")
