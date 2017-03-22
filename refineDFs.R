refineDF <- function(df) {
  library(dplyr)
  
  df %>%
    filter(rating > 0, votes >= 500, !is.na(votes), !is.na(rating)) %>%
    arrange(desc(rating)) %>%
    mutate("ranking" = 1:n())
}

top100RatingMean <- function(df) {
  library(dplyr)
  tList <- df %>% filter(ranking <= 100)
  mean(tList$rating)
}

top100VotesMean <- function(df) {
  library(dplyr)
  tList <- df %>% filter(ranking <= 100)
  mean(tList$votes)
}