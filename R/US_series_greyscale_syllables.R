## Trend analysis on SOTU corpus for book chapter
##
##

library(sophistication)
library(quanteda)
library(stringi)
library(dplyr)

# sets to each user's Dropbox root folder for the project
#setwd(getOption("ROOT_DROPBOX"))

measure <- "meanWordSyllables"
SOTU_stat <- textstat_readability(data_corpus_SOTU, measure)

SOTU_year <- lubridate::year(docvars(data_corpus_SOTU, "Date"))
SOTU_df <- data.frame("year" = SOTU_year, "SOTU_stat" = SOTU_stat)
plot(SOTU_df)

###PP


###EO


load(paste0(getOption("ROOT_DROPBOX"), "data_text/executive_orders/data_corpus_eo.rdata"))


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


eo_lengths <- ntoken(eo_corp, removePunct = T)
summary(eo_lengths)
# summary(eo_year)
eo_stat <- textstat_readability(eo_corp, measure)

eo_year <- docvars(eo_corp)

eo_df<-data.frame("year" = eo_year$Year, "eo_stat" = eo_stat)
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
scotus_lengths <- ntoken(clean2, removePunct = T)
summary(scotus_lengths)
table(scotus_lengths)

clean3 <- corpus_trimsentences(clean2, min_length = 4)





scotus_stat <- textstat_readability(clean3, measure)


scotus_year <- docvars(clean2, "Year")
scotus_df <- data.frame("year" = scotus_year, "scotus_stat" = scotus_stat)

#scotus_df<-filter(scotus_df, scotus_stat> -50)

#scotus_df<-filter(scotus_df, scotus_stat< 150)



###combine all together
##too many to plot well, and very unbalanced (way more modern ones)--try for a more balanced sample

years <- unique(scotus_df$year)


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


df_US<-melt(list(SOTU=SOTU_df, SCOTUS=balanced_scotus_df, Congress = congress_df,
                 ExecOrders = eo_df), id.vars="year")








df1<-filter(df_US, is.na(df_US$value)==F)

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
<<<<<<< HEAD
#  geom_point(alpha=0.025) + 
  coord_cartesian(ylim = c(1.4, 1.85)) +
=======
  geom_point(alpha=0.025) +
  coord_cartesian(ylim = c(1.45, 1.85)) +
>>>>>>> origin/master
#   coord_cartesian(ylim = c(-20, 65)) +
  #coord_cartesian(ylim = c(15, 40), xlim=c(1900, 2010)) +
  theme(legend.position="none", axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15))+
    scale_linetype_manual(values = linez)+
#scale_colour_manual(values = cols, guide = guide_legend(title = "Source"))+
    xlab("") +
    ylab("Mean Number of Syllables per Word") +
    #geom_line(aes(), alpha=0.3, size = 1) +
    # ggtitle("Text Complexity in State of the Union Addresses") +
    theme(plot.title = element_text(lineheight=.8, face="bold")) +
annotate("text", label = "SOTU", x = 1800, y = 1.73, size = 6, colour = "black")+
annotate("text", label = "SCOTUS", x = 1810, y = 1.47, size = 6, colour = "black") +
  annotate("text", label = "Con. Speech", x = 1998, y = 1.61, size = 6, colour = "black")+
  
 annotate("text", label = "Executive Orders", x = 1950, y = 1.82, size = 6, colour = "black")


print(p)

##sorry about filepath
pdf(file = "manuscript_chapter/plots/US_series_greyscale_syllables.pdf", height=7, width=12)
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
