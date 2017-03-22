getIMDBAnime <- function(){
  library(rvest)
  hasMore <- TRUE 
  allPageResults <- data.frame()
  pageNo <- 1
  
  while(hasMore == TRUE){
    url <- paste("http://www.imdb.com/search/keyword?keywords=anime&sort=user_rating,desc&mode=detail&page=", pageNo, "&title_type=tvSeries&ref_=kw_ref_key", sep = "")
    animePage <- read_html(url)
    print(paste("Loading records",  (pageNo - 1) * 50, "to", (pageNo - 1) * 50  + 50))
    
    pageResult <- data.frame()
    
    titles <- animePage %>%
      html_nodes("div.lister-item-content > h3 > a") %>%
      html_text(trim = TRUE)
    
    ratings <- animePage %>%
      html_nodes("div.lister-item-content > div > div.inline-block.ratings-imdb-rating > strong") %>%
      html_text(trim = TRUE)
    ratings <- as.numeric(ratings)
    
    votes <- animePage %>%
      html_nodes("div.lister-item-content > p:nth-child(6) > span:nth-child(2)") %>%
      html_text(trim = TRUE)
    
    votes <- as.numeric(gsub(',', '', votes))
    
    if(length(titles) != length(ratings)) {
      length(ratings) <- max(length(titles))
      length(votes) <- max(length(titles))
    }
    
    pageResult <- data.frame(titles, ratings, votes)
    
    allPageResults <- rbind(allPageResults, pageResult)
    
    if(nrow(pageResult) < 50) { 
      hasMore <- FALSE 
    } else {
      pageNo <- pageNo + 1
    }
  }
  names(allPageResults) <- c("title", "rating", "votes")
  data.frame(allPageResults)
}