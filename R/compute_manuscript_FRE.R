## estimate readability of our chapter
## Kevin will like this one :-)
## KB

require(quanteda)
require(sophistication)
require(magrittr)


# read in the plain text conversion of the LaTeX chapter document
# paste into a single document
# make into a corpus
# trim sentences < 5 tokens in length and containing URLs
# compute FRE

system2("pandoc",
        c("-f", "latex", "-t", "plain", "manuscript_chapter/BenoitMungerSpirling_SSRCchapter.tex"),
        stdout = TRUE) %>%
    paste(collapse = "  ") %>%
    corpus() %>%
    corpus_trimsentences(min_length = 5, exclude_pattern = "http") %>%
    textstat_readability(measure = "Flesch")

