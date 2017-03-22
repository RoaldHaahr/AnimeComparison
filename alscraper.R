getALAnime <- function() {
  library(rvest)
  url <- "C:/Users/Roald/Documents/RData/IMDB/anilist/anilist.html"
  anilist <- read_html(url)
  
  titles <- anilist %>%
    html_nodes(".cover__data a") %>%
    html_text()
  
  ratings <- anilist %>%
    html_nodes("div[title='Average Score']") %>%
    html_text()
  ratings <- as.numeric(gsub('%', '', ratings)) / 10
  
  votes <- anilist %>%
    html_nodes("div[title='Popularity']") %>%
    html_text()
  votes <- as.numeric(votes)
  
  output <- data.frame(titles, ratings, votes)
  colnames(output) <- c("title", "rating", "votes")
  
  output #return
}
