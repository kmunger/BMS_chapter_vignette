## Trend analysis on SOTU corpus for book chapter
##
##

library(sophistication)
library(quanteda)

# sets to each user's Dropbox root folder for the project
setwd(getOption("ROOT_DROPBOX"))

# library(dplyr)
library(stringi)
# library(stringr)

## readability for SOTU texts
measure <- "Flesch"

SOTU_stat <- textstat_readability(data_corpus_SOTU, measure)

SOTU_year <- lubridate::year(docvars(data_corpus_SOTU, "Date"))
SOTU_df <- data.frame("year" = SOTU_year, "SOTU_stat" = SOTU_stat)
plot(SOTU_df)

###EO


load("data_text/executive_orders/data_corpus_eo.rdata")


#drop the ones that are empty
empty <- which( nchar(data_corpus_eo$documents$texts)==0 )
clean1 <- data_corpus_eo$documents[-empty, c(1:2)]

##problems
clean1$texts<-stri_replace_all_fixed(clean1$texts, "A. D.", "AD")

#clean1$texts<-stri_replace_all_regex(clean1$texts, " ([A-z])\\.", "")
#clean1$texts<-stri_replace_all_regex(clean1$texts, "\\.  ([0-9])\\.", "")




#drop anything that's very long (95th percentile and above)
quant95 <- which( nchar(clean1$texts) >
                    quantile( nchar(clean1$texts), prob=.95) )
clean2 <- clean1[-quant95, c(1:2)]





eo_corp <- corpus(clean2, text_field = "texts")

eo_corp <- corpus_trimsentences(eo_corp, min_length = 4)


eo_corp <- corpus_subset(eo_corp, ntoken(eo_corp) > 10)


eo_lengths <- ntoken(eo_corp, removePunct = TRUE)
summary(eo_lengths)
summary(eo_year)
eo_stat <- textstat_readability(eo_corp, measure)

eo_year <- docvars(eo_corp)

eo_df <- data.frame("year" = eo_year$Year, "eo_stat" = eo_stat)
#eo_df<-filter(eo_df, eo_stat> -50)

#eo_df<-filter(eo_df, eo_stat< 150)
########SCOTUS

data_dropbox(data_corpus_SCOTUS)
#drop the ones that are empty
clean1 <- corpus_subset(data_corpus_SCOTUS, nchar(texts(data_corpus_SCOTUS)) > 0)


## implement fixes
texts(clean1) <- stri_replace_all_fixed(texts(clean1), "U. S.", "US")



#drop anything that's very long (95th percentile and above)
#drop anything that's very long (95th percentile and above)
temp_lengths <- stri_length(texts(clean1))
clean2 <- corpus_subset(clean1, temp_lengths <quantile(temp_lengths, prob = .95))
scotus_lengths<-ntoken(clean2, removePunct = T)
summary(scotus_lengths)
table(scotus_lengths)

clean3 <- corpus_trimsentences(clean2, min_length = 4)





scotus_stat <- textstat_readability(clean3$documents$texts, measure)


scotus_year <- clean2$documents$Year
sort(unique(scotus_year))
scotus_df<-data.frame("year" = scotus_year, "scotus_stat" = scotus_stat)

#scotus_df<-filter(scotus_df, scotus_stat> -50)

#scotus_df<-filter(scotus_df, scotus_stat< 150)

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
load("C:/Users/kevin/Documents/GitHub/sophistication/R_package/data/data_corpus_partybroadcasts.rdata")

temp_lengths <- stri_length(texts(data_corpus_partybroadcasts))
data_corpus_pb <- corpus_subset(data_corpus_partybroadcasts, temp_lengths < quantile(temp_lengths, prob = .95))
library(stringi)
library(stringr)

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
##too many to plot well, and very unbalanced (way more modern ones)--try for a more balanced sample

years<-unique(scotus_df$year)


indices<-vector()
for(i in 1:length(years)){
  set<-which(scotus_df[,"year"]==years[i])
  num<-min(length(set), 30)
  samp<-sample(set, num, replace = FALSE)
  indices<-c(indices, samp)
}


balanced_scotus_df<-scotus_df[indices,]


###Congress

load("data_text/CR/data_corpus_CR.rdata")


#drop the ones that are by the speaker
speaker <- which( (data_corpus_CR$documents$name)=="Speaker" )
clean1 <- data_corpus_CR$documents[-speaker, ]



#drop anything that's very long (90th percentile and above)
quant95 <- which( nchar(clean1$texts) >
                    quantile( nchar(clean1$texts), prob=.95) )
clean2 <- clean1[-quant95, ]


table(clean2$year)

##need to add leading 0 to dates after 2000
#clean2$year<-str_pad(clean2$year, 2, pad = "0")


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


#stat <- textstat_readability(corp, measure)
congress_stat <- textstat_readability(sample_data$texts, measure)
congress_year <- sample_data$year

congress_df<-data.frame("year" = congress_year, "congress_stat" = congress_stat)



require(reshape2)
df1<-melt(list( SOTU=SOTU_df,
              Nobel = nobel_df,
              Manifestos = man_df, Broadcasts = pb_df  , SCOTUS=balanced_scotus_df,
              ExecOrders = eo_df   , Congress = congress_df  ), id.vars="year")



library(dplyr)
df1<-filter(df1, is.na(df1$value)==F)

##

#df1<-filter(df1, value < 150)
#df1<-filter(df1, value > -100)




require(ggplot2)

linez<-c("twodash", "dotted", "dashed", "F1",
         "dotdash", "solid", "4C88C488")

p <- ggplot(data = df1,
            aes(x = year, y = value, linetype = L1)) + #, group = delivery)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    geom_smooth(alpha=0.2,  method = "loess", span = .34, color = "black", se = F) +
 # geom_point(alpha=0.01) +
 # coord_cartesian(ylim = c(1.45, 1.85)) +
   coord_cartesian(ylim = c(-10, 70)) +
  #coord_cartesian(ylim = c(15, 40), xlim=c(1900, 2010)) +
 theme(legend.position="none")+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15)) +
    scale_linetype_manual(values = linez)+
#scale_colour_manual(values = cols, guide = guide_legend(title = "Source"))+
    xlab("") +
    ylab("Flesh Reading Ease Score") +
    #geom_line(aes(), alpha=0.3, size = 1) +
    # ggtitle("Text Complexity in State of the Union Addresses") +
    theme(plot.title = element_text(lineheight=.8, face="bold")) +
annotate("text", label = "SOTU", x = 1800, y = 22, size = 6, colour = "black")+
annotate("text", label = "SCOTUS", x = 1800, y = 49, size = 6, colour = "black") +
  annotate("text", label = "Executive Orders", x = 1970, y = -10, size = 6, colour = "black") +
  annotate("text", label = "UK Manifestos", x = 1959, y = 30, size = 6, colour = "black") +
   annotate("text", label = "Party Broadcasts", x = 1927, y = 65, size = 6, colour = "black") +
    annotate("text", label = "Nobel Prize", x = 1900, y = 48, size = 6, colour = "black") +
annotate("text", label = "Congress", x = 2000, y = 50, size = 6, colour = "black")



print(p)


pdf(file="C:/Users/kevin/Documents/GitHub/sophistication/manuscript_chapter/plots//all_series_greyscale.pdf", height=7, width=12)
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
