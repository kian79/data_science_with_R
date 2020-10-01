library(ggplot2)
library(stringr)
library(gutenbergr)
library(tidytext)
library(dplyr)
library(wordcloud2)
devtools::install_github("lchiffon/wordcloud2")
novels_cloud <-
  words%>%
  group_by(word) %>%
  dplyr::count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(1:200) %>%
  select(word,freq = n) %>%
  wordcloud2(size = 1,
             color = "random-dark",
             minRotation = pi/2, maxRotation = -pi/2, minSize = 10,
             rotateRatio = 1, figPath = 'a.png')