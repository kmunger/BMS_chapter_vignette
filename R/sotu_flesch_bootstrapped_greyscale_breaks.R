## Trend analysis on SOTU corpus for book chapter
## Currently Figure 1

library(sophistication)
library(quanteda)

## compute readability and bootstrap the sentences
set.seed(20170308)
results <- bootstrap_readability(data_corpus_SOTU, n = 100, measure = "Flesch", verbose = TRUE)
bs_sd <- results$bs_sd[,"Flesch"]
stat <- results$original[,"Flesch"]
year <- lubridate::year(docvars(data_corpus_SOTU, "Date"))


## calculate statistics for SOTU
library(trend)
library(strucchange)
sotu_mean <- mean(stat$Flesch)
sotu_var <- var(stat$Flesch)
sotu_cs_test <- cs.test(stat$Flesch)
sotu_ts <- as.ts(stat$Flesch)
sotu_breaks <- breakpoints(sotu_ts ~ 1 )
summary(sotu_breaks)

# looks like  3  breakpoints
fm0 <- lm(stat ~ breakfactor(sotu_breaks, breaks = 3))


sotu_breaks <- breakpoints(sotu_ts ~ 1, breaks = 3 )
year_breaks <- year[breakdates(sotu_breaks)]


## plot the trend
require(ggplot2)
partyColours <- c("blue", "blue", "black", "black", "red", "red")
p <- ggplot(data = docvars(data_corpus_SOTU),
            aes(x = year, y = stat)) + #, group = delivery)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    # geom_smooth(alpha=0.2, linetype=1, color="grey70", method = "loess", span = .34) +
    xlab("") +
    ylab("Flesch Reading Ease Score") +
    geom_errorbar(aes(ymin=stat-1.96*bs_sd, ymax=stat+1.96*bs_sd), colour="grey70", width=.1) +
    geom_point(aes(shape = party), size = 2, fill = "black") +
    #   geom_ribbon(aes(ymin=stat-1.96*bs_sd, ymax=stat+1.96*bs_sd), color="grey70", alpha=.4) +
    ##geom_line(aes(y = stat-1.96*bs_sd))+
    #geom_line(aes(y = stat+1.96*bs_sd))+
    scale_shape_manual(values = c(17, 8, 3, 19, 15, 20)) +
    geom_vline(aes(xintercept= year_breaks[1] ) ) +
    geom_vline(aes(xintercept= year_breaks[2] ) ) +
    geom_vline(aes(xintercept= year_breaks[3] ) ) +
    #  scale_colour_manual(values = partyColours) +
    # geom_line(aes(), alpha=0.3, size = 1) +
    # ggtitle("Text Complexity in State of the Union Addresses") +
    theme(plot.title = element_text(lineheight=.8, face="bold"),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15))
print(p)


pdf(file="C:/Users/kevin/OneDrive/Documents/GitHub/sophistication/manuscript_chapter/plots//sotu_flesch_bootstrapped_greyscale_breaks.pdf", height=7, width=12)

#pdf(file = "manuscript_chapter/plots/SOTU_flesch_bootstrapped_greyscale_breaks.pdf", height = 7, width = 12)
print(p)
dev.off()


## save the data
save.image(paste0(getOption("ROOT_DROPBOX"), "data_intermediate/SOTU_run.RData"))

