getMALAnime <- function(){
  library(rvest)
  hasMore <- TRUE 
  allPageResults <- data.frame()
  urlLimit <- 0
  
  while(hasMore == TRUE){
    url <- paste("https://myanimelist.net/topanime.php?limit=", urlLimit, sep = "")
    animePage <- read_html(url)
    print(paste("Loading records",  urlLimit, "to", urlLimit + 50))
    
    titles <- animePage %>% html_nodes(xpath = '//*[contains(@class, "hoverinfo_trigger fl-l fs14 fw-b")]') %>% html_text(trim = TRUE)

    ratings <- animePage %>% html_nodes(xpath = '//*[contains(@class, "js-top-ranking-score-col di-ib al")]') %>% html_text(trim = TRUE)
    ratings <- as.numeric(ratings)
    
    votes <- animePage %>% html_nodes(xpath = '//*[contains(@class, "information di-ib mt4")]/text()[preceding-sibling::br[2]]') %>% html_text(trim = TRUE)
    for(i in 1:length(votes)) {
      votes[i] <- substring(votes[i], 0, nchar(votes[i]) - 8)
    }
    votes <- as.numeric(gsub(',', '', votes))
    
    pageResult <- data.frame(titles, ratings, votes)
    
    allPageResults <- rbind(allPageResults, pageResult)
    
    if(nrow(pageResult) < 50) { 
      hasMore <- FALSE 
    }
    
    urlLimit <- urlLimit + 50
  }
  data.frame(allPageResults)
  
}
