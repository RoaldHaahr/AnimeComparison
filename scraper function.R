getIMDBAnime <- function(selectors) {
  library(rvest)
  imdb <- read_html("http://www.imdb.com/search/keyword?keywords=anime&sort=user_rating,desc&mode=detail&page=1&title_type=tvSeries&ref_=kw_ref_key")
  recordCountNode <- html_node(imdb, "input[name=tvSeries]")
  recordCount <- html_attr(recordCountNode, "data-count")
  pageCount <- ceiling(as.integer(gsub(",", "", recordCount)) / 50)
  anime <- matrix(nrow = 0, ncol = length(selectors))
  for(x in 1:pageCount) {
    imdb <- read_html(paste("http://www.imdb.com/search/keyword?keywords=anime&sort=user_rating,desc&mode=detail&page=", x, "&title_type=tvSeries&ref_=kw_ref_key", sep = ""))
    temp <- matrix(nrow = 50, ncol = 0)
    for(i in 1:length(selectors)) {
      
      tempList <- imdb %>%
        html_nodes(selectors[i]) %>%
        html_text()
      if(length(tempList) > 0 && length(tempList) < 50) {
        for(z in 1:50) {
          if(is.na(tempList[z])) tempList[z] <- NA
        }
      }
      if(length(tempList) == 0) tempList = matrix(NA, nrow = 50)
      
      temp <- cbind(temp, tempList)
      
    }
    anime <- rbind(anime, temp)
  }
  data.frame(anime)
}

getIMDBAnime <- function(){
  library(rvest)
  hasMore <- TRUE 
  allPageResults <- data.frame()
  pageNo <- 1
  
  selectors <- c(
    "div.lister-item-content > h3 > a",
    "div.lister-item-content > div > div.inline-block.ratings-imdb-rating > strong",
    "div.lister-item-content > p:nth-child(6) > span:nth-child(2)"
  )
  
  while(hasMore == TRUE){
    url <- paste("http://www.imdb.com/search/keyword?keywords=anime&sort=user_rating,desc&mode=detail&page=", pageNo, "&title_type=tvSeries&ref_=kw_ref_key", sep = "")
    animePage <- read_html(url)
    print(paste("Loading records",  (pageNo - 1) * 50, "to", (pageNo - 1) * 50  + 50))
    
    pageResult <- data.frame()
    
    for(i in 1:length(selectors)) {
      if(i == 1) {
        pageResult <- data.frame(
          animePage %>%
            html_nodes(selectors[i]) %>%
            html_text()
        )
      } else {
        animeCol <- 
          animePage %>%
            html_nodes(selectors[i]) %>%
            html_text()
        
        
        if(length(pageResult[,1]) != length(animeCol)) {
          # lapply(animeCol, 'length<-', max(length(pageResult)))
          length(animeCol) <- max(length(pageResult[,1]))
        }
        
        pageResult <- cbind(
          pageResult,
          animeCol
        )
      }
    }
    # anime <- rbind(anime, temp)
    
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