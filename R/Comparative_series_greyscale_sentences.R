## Trend analysis on SOTU corpus for book chapter
##
##
devtools::install_github("kbenoit/quanteda", force = T ) 

devtools::install_github("kbenoit/sophistication", 
                         subdir = "R_package",
                         auth_token = "309171976db5586eab402a922604229cd5190c81")
library(sophistication)
library(quanteda)
library(magrittr)
# sets to each user's Dropbox root folder for the project
setwd(getOption("ROOT_DROPBOX"))

# library(devtools)
# library(quanteda)
# library(Rcpp)
# library(quantedaData)
# library(trend)
library(dplyr)
 library(stringi)
library(stringr)

# devtools::install_github("kbenoit/quantedaData")
# setwd("C:/Users/kevin/Dropbox/Benoit_Spirling_Readability/")


data("data_corpus_SOTU")

## readability for SOTU texts
#measure <- "Flesch"
measure <- "meanSentenceLength"
#measure <- "meanWordSyllables"


SOTU_stat <- textstat_readability(data_corpus_SOTU, measure)

SOTU_year <- lubridate::year(docvars(data_corpus_SOTU, "Date"))
SOTU_df <- data.frame("year" = SOTU_year, "SOTU_stat" = SOTU_stat)
plot(SOTU_df)


########nobel
load("data_text/NobelLitePresentations/data_corpus_nobel.rdata")

temp_lengths <- stri_length(texts(data_corpus_nobel))
data_corpus_nobel <- corpus_subset(data_corpus_nobel, temp_lengths < quantile(temp_lengths, prob = .95))


nobel_corp <- corpus_trimsentences(data_corpus_nobel, min_length = 4)


nobel_lengths<-ntoken(nobel_corp, removePunct = T)
summary(nobel_lengths)
summary(nobel_year)
nobel_stat <- textstat_readability(nobel_corp, measure)

nobel_year <- docvars(nobel_corp)

nobel_df<-data.frame("year" = nobel_year$year, "nobel_stat" = nobel_stat)

#nobel_df<-filter(nobel_df, nobel_stat> -50)

#nobel_df<-filter(nobel_df, nobel_stat< 150)


########Party Broadcasts
load("C:/Users/kevin/OneDrive/Documents/GitHub/sophistication/R_package/data/data_corpus_partybroadcasts.rdata")

temp_lengths <- stri_length(texts(data_corpus_partybroadcasts))
data_corpus_pb <- corpus_subset(data_corpus_partybroadcasts, temp_lengths < quantile(temp_lengths, prob = .95))

pb_corp <- corpus_trimsentences(data_corpus_pb, min_length = 4)

pb_lengths<-ntoken(pb_corp, removePunct = T)
summary(pb_lengths)
summary(pb_year)

pb_stat <- textstat_readability(pb_corp, measure)

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

pb_df<-data.frame("year" = pb_year, "pb_stat" = pb_stat)

#pb_df<-filter(pb_df, pb_stat> -50)

#pb_df<-filter(pb_df, pb_stat< 150)


########UK Manifestos
load("data_text/UK_manifestos/data_corpus_man.rdata")

temp_lengths <- stri_length(texts(data_corpus_man))
data_corpus_man <- corpus_subset(data_corpus_man, temp_lengths < quantile(temp_lengths, prob = .95))


man_corp <- corpus_trimsentences(data_corpus_man, min_length = 4)

man_lengths<-ntoken(man_corp, removePunct = T)
summary(man_lengths)
summary(man_year)



#corp <- corpus(clean2$texts)


man_stat <- textstat_readability(man_corp, measure)

man_year <- docvars(man_corp)

man_df<-data.frame("year" = man_year$Year, "man_stat" = man_stat)

#man_df<-filter(man_df, man_stat> -50)

#man_df<-filter(man_df, man_stat< 150)



###combine all together


require(reshape2)
df1<-melt(list( SOTU=SOTU_df, 
              Nobel = nobel_df, 
              Manifestos = man_df, Broadcasts = pb_df  ), id.vars="year")




df1<-filter(df1, is.na(df1$value)==F)

##

#df1<-filter(df1, value < 150)
#df1<-filter(df1, value > -100)




require(ggplot2)

linez<-c( "F1", "dashed",  "dotdash","solid")


p <- ggplot(data = df1,
            aes(x = year, y = value, linetype = L1)) + #, group = delivery)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    geom_smooth(alpha=0.2,  method = "loess", span = .34, color = "black", se = F) +
  #geom_point(alpha=0.01) + 
 # coord_cartesian(ylim = c(1.45, 1.85)) +
  # coord_cartesian(ylim = c(-25, 70)) +
  coord_cartesian(ylim = c(15, 35), xlim=c(1900, 2010)) +
 theme(legend.position="none")+
    scale_linetype_manual(values = linez)+
#scale_colour_manual(values = cols, guide = guide_legend(title = "Source"))+
    xlab("") +
    ylab("Mean Sentence Length") +
    #geom_line(aes(), alpha=0.3, size = 1) +
    # ggtitle("Text Complexity in State of the Union Addresses") +
    theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15)) +
annotate("text", label = "SOTU", x = 1912, y = 34, size = 6, colour = "black")+
  annotate("text", label = "UK Manifestos", x = 2000, y = 27, size = 6, colour = "black") +
   annotate("text", label = "Party Broadcasts", x = 1965, y = 15, size = 6, colour = "black") + 
    annotate("text", label = "Nobel Prize", x = 1910, y = 26, size = 6, colour = "black")
  


print(p)


pdf(file="C:/Users/kevin/Documents/GitHub/sophistication/manuscript_chapter/plots//Comparative_series_greyscale_sentences.pdf", height=7, width=12)
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
