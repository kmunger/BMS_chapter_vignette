## Trend analysis on SOTU corpus for book chapter
## Currently Figure 1

library(sophistication)
library(quanteda)

## compute readability and bootstrap the sentences
stat <- textstat_readability(data_corpus_SOTU, measure = "Flesch")
year <- lubridate::year(docvars(data_corpus_SOTU, "Date"))


## calculate statistics for SOTU
library(trend)
library(strucchange)
sotu_mean <- mean(stat)
sotu_var <- var(stat)
sotu_cs_test <- cs.test(stat)
sotu_ts <- as.ts(stat)
sotu_breaks <- breakpoints(sotu_ts ~ 1 )
summary(sotu_breaks)

# looks like  3  breakpoints
fm0 <- lm(stat ~ breakfactor(sotu_breaks, breaks = 3))

year_breaks
plot(sotu_ts)
lines(ts(fitted(fm0), start = 1), col = 3)
sotu_breaks <- breakpoints(sotu_ts ~ 1, breaks = 3 )
year_breaks <- year[breakdates(sotu_breaks)]



## plot a boxplot

# compute the levels
period <- cut(year, breaks = c(min(year), year_breaks, max(year)), include.lowest = TRUE, dig.lab = 4)
# reformat the levels
levs <- stringi::stri_replace_all_fixed(levels(period), c("[", "]", "("), "", vectorize_all = FALSE)
levs <- strsplit(levs, ",")
levs[2:length(levs)] <- lapply(levs[2:length(levs)], function(x) {
    x[1] <- as.integer(x[1]) + 1
    x
})
levels(period) <- sapply(levs, paste, collapse = "-")

ggplot(aes(y = stat, x = period, fill = docvars(data_corpus_SOTU, "delivery")),
       data = NULL) +
    geom_boxplot() +
    scale_fill_grey("", start = .6, end = .9) +
    theme(plot.title = element_text(lineheight=.8, face="bold"),
           axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15)) +
    labs(y = "Flesch Reading Ease Score", x = "") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.key.size =  unit(2.5, "cm"),
          axis.line = element_line(colour = "black"))

dev.copy2pdf(file="C:/Users/kevin/OneDrive/Documents/GitHub/sophistication/manuscript_chapter/plots/SOTU_flesch_boxplot_breaks_x_delivery.pdf", height = 7, width = 12)


## table of results
tapply(stat, list(period, delivery = docvars(data_corpus_SOTU, "delivery")),
       function(x) format(median(x, na.rm = TRUE), digits = 3))
tapply(stat, list(period, delivery = docvars(data_corpus_SOTU, "delivery")), length)


data(data_corpus_SOTU, package = "quantedaData")
tmp <- textstat_readability(data_corpus_SOTU, c("meanSentenceLength", "meanWordSyllables"))
apply(tmp, 2, mean)
apply(tmp, 2, sd)
apply(tmp, 2, max)
apply(tmp, 2, min)
tmp$year <- lubridate::year(docvars(data_corpus_SOTU, "Date"))

require(ggplot2)
ggplot(data = tmp,
            aes(x = year, y = meanWordSyllables)) + #, group = delivery)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    geom_smooth(alpha=0.2,  method = "loess", span = .34, color = "black", se = FALSE) +
    geom_point(alpha=0.01) +
    theme(legend.position="none")

