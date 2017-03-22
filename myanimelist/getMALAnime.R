# 
# 
# myAniTitle <- html_nodes(myAnimeList, ".detail .hoverinfo_trigger.fl-l.fs14.fw-b") %>% html_text()
# 
# aniDataTitle <- data.frame(c(myAniTitle))
# 
# myAniRating <- html_nodes(myAnimeList, ".text.on") %>% html_text()
# aniDataRating <- data.frame(c(myAniRating))
# 
# aniTitleRating <- data.frame(c(aniDataTitle, aniDataRating))
# 
# 
# "https://myanimelist.net/topanime.php?limit=" + x
# 

getMALAnime <- function(){
  library(rvest)
  hasMore <- TRUE 
  allPageResults <- data.frame()
  urlLimit <- 0
  
  while(hasMore == TRUE){
    url <- paste("https://myanimelist.net/topanime.php?limit=", urlLimit, sep = "")
    animePage <- read_html(url)
    print(paste("Loading records",  urlLimit, "to", urlLimit + 50))

    pageResult <- data.frame(animePage %>% html_table(".top-ranking-table", header = TRUE))
    allPageResults <- rbind(allPageResults, pageResult)
    
    if(nrow(pageResult) < 50) { 
      hasMore <- FALSE 
    }
    
    urlLimit <- urlLimit + 50
  }
  data.frame(allPageResults)

}
