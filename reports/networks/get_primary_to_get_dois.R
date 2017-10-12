### GET PRIMARY PAPERS TO GET DOIS ####

library(tidyverse)

source("../../../dashboard/global.R", chdir = TRUE)

meta_d <- all_data %>%
  filter(short_name == "statSeg") %>%
  select(study_ID, long_cite) %>%
  distinct()

df <- meta_d %>%
  mutate(ID = 1:n()) %>%
  select(ID, everything())
write_csv(df, "data/statseg_papers.csv")

# To get dois, copy-pasting 50 at a time into search engine (obtained from: https://apps.crossref.org/SimpleTextQuery/)

dois <- read.csv("dois2.csv") %>%
  filter(dataset == "Statistical Word Segmentation") %>%
  select(study_ID, doi) %>%
  filter(doi != "" & !is.na(doi)) %>%
  mutate(doi = ifelse(grepl(".org/", doi),
                      unlist(lapply(str_split(doi, ".org/"), 
                                    function(x) {x[2]})), as.character(doi)))

### Scrape data from web of science at paper level using bibliometrix package 
# (copy paste this string into WOS search engine)
# credentials: uchicago email; password: uchicago pw + 1
string = "DO = ("
for (i in 1:length(dois$doi)) {
  string = paste(string, dois$doi[i], sep = " OR ")
}
# add paren to

### search web of science-> save to marked list -> download marked list as .txt
wos.raw <- readFiles("data/savedrecs_stat_seg.txt")

# creates dataframe out of raw wos
wos <- convert2df(wos.raw, dbsource = "isi", format = "plaintext") %>% # creates 
  mutate(DI = tolower(DI),
         DI = str_replace_all(DI, "//", "/")) %>%
         #ID = str_replace_all(ID, ";;", ";")
  arrange(DI) %>%
  filter(!is.na(DI)) %>%
  mutate_each(funs(as.factor), -AU)

wos2 <- metaTagExtraction(wos[1,], Field = "CR_AU", sep = ";")
# write.csv(wos, "wos_bib1_6.csv")