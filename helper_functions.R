
# Helper functions for scraping -------------------------------------------

wait_for_page <- function(remDr, timeout = 10){
  for (i in 1:timeout) {
    state <- remDr$executeScript("return document.readyState")[[1]]
    if (state == "complete") return(TRUE)
    Sys.sleep(1)
  }
  return(FALSE)
}

box_score_length <- function(site) {
  remDr$navigate(site)
  
  if (!wait_for_page(remDr, timeout = 10)){
    cat("Warning: Page took too long to load:", site, "\n")
  }
  
  boxscore_href <- remDr$findElements(using = "xpath", "//a[contains(@href, 'boxscore')]")
  boxscore_linktext <- remDr$findElements(using = "partial link text", "Box Score")
  
  href_length <- length(boxscore_href)
  linktext_length <- length(boxscore_linktext)
  
  if (href_length == linktext_length){
    return(href_length)
  } else if (href_length > 0 && linktext_length > 0) {
    min(href_length, linktext_length)
  } else if (href_length > 0 && linktext_length == 0){
    href_length
  } else if (href_length == 0 && linktext_length > 0){
    linktext_length
  } else {
    0
  }
}

# get boxscore href
get_boxscore <- function(site) {
  remDr$navigate(site)
  
  # My computer is slow. Need time to load elements
  while (remDr$executeScript("return document.readyState")[[1]] != "complete"){
    Sys.sleep(0.5)
  }
  
  remDr$setTimeout(type = "implicit", milliseconds = 5000) # Wait up to 5 sec for elements
  tryCatch({
    close_button <- remDr$findElement(using = "id", "polite-close")
    close_button$click()
    message("Pop-up closed")
  }, error = function(e) {
    message("No pop-up detected")
  })
  
  boxscore_href <- remDr$findElements(using = "xpath", "//a[contains(@href, 'boxscore')]")
  boxscore_linktext <- remDr$findElements(using = "partial link text", "Box Score")
  
  href_length <- length(boxscore_href)
  linktext_length <- length(boxscore_linktext)
  
  if (href_length == linktext_length){
    return(boxscore_href)
  } else if (href_length > 0 && linktext_length > 0) {
    if (which.min(c(href_length, linktext_length)) == 1){
      return(boxscore_href) 
    } else {
      return(boxscore_linktext)
    }
  } else if (href_length > 0 && linktext_length == 0){
    return(boxscore_href)
  } else if (href_length == 0 && linktext_length > 0){
    return(boxscore_linktext)
  } else {
    0
  }
}

# get game info
get_game_info <- function(links) {
  remDr$navigate(links)
  
  while (remDr$executeScript("return document.readyState")[[1]] != "complete"){
    Sys.sleep(0.5)
  }
  
  remDr$setTimeout(type = "implicit", milliseconds = 3000) # Wait up to 3 sec for elements
  tryCatch({
    close_button <- remDr$findElement(using = "id", "polite-close")
    close_button$click()
    message("Pop-up closed")
  }, error = function(e) {
    message("No pop-up detected")
  })
  
  remDr$executeScript("window.scrollBy(0, 500);")
  Sys.sleep(2)
  panel_button <- remDr$findElements(using = "id", "expansionPanelButton")
  info_container <- remDr$findElements(using = "xpath", "//dl[starts-with(@class, 'text-center')]")
  iframe <- remDr$findElements(using = "tag name", "iframe")
  
  if (length(panel_button) > 0) {
    panel_button[[1]]$clickElement()
    Sys.sleep(1)
    remDr$executeScript("arguments[0].scrollIntoView();", list(panel_button[[1]]))
    Sys.sleep(1)
    game_info_box <- remDr$findElement(using = "id", "expansionPanel")
    return(game_info_box$getElementText() %>% unlist())
  } else if (length(info_container) > 0){
    remDr$executeScript("arguments[0].scrollIntoView();", list(info_container[[1]])) # Might remove
    col_name <- info_container[[1]]$findChildElements(using = "tag name", "dt") %>%
      lapply(function(x) x$getElementText()) %>%
      unlist()
    col_info <- info_container[[1]]$findChildElements(using = "tag name", "dd") %>%
      lapply(function(x) x$getElementText()) %>% 
      unlist()
    str_c(col_name, col_info)
  } else if (length(iframe) > 0) {
    remDr$switchToFrame(iframe[[1]])
    content <- remDr$findElements(using = "xpath", "//div[contains(@class, 'box-score')]")
    stats <- remDr$findElements(using = "xpath", "//li[starts-with(@class, 'match-stats-header')]")
    if (length(content) > 0) {
      level_items <- content[[1]]$findChildElements(using = "xpath", ".//div[starts-with(@class, 'level-item')]")
      text <- lapply(level_items, function(x) x$getElementText()) %>% unlist()
      text <- gsub("\\\n", ":", text)
      remDr$switchToFrame(NULL)
      return(text)
    } else if (length(stats) > 0){
      text <- lapply(stats, function(x) x$getElementText()) %>% unlist()
      text <- gsub("\\\n", ":", text)
      remDr$switchToFrame(NULL)
      return(text)
    } else {
      remDr$switchToFrame(NULL)
      return(NULL)
    }
  }
}

get_iframe <- function(remDr){
  iframe <- remDr$findElements(using = "tag name", "iframe")
  if (length(iframe) > 0){
    remDr$switchToFrame(iframe[[1]])
    content <- remDr$findElements(using = "xpath", "//div[contains(@class, 'box-score')]")
    stats <- remDr$findElements(using = "xpath", "//li[starts-with(@class, 'match-stats-header')]")
    if(length(content) > 0){
      level_items <- content[[1]]$findChildElements(using = "xpath", ".//div[starts-with(@class, 'level-item')]")
      text <- lapply(level_items, function(x) x$getElementText()) %>% unlist()
      text <- gsub("\\\n", ":", text)
      remDr$switchToFrame(NULL)
      return(text)
    } else if (length(stats) > 0) {
      text <- lapply(stats, function(x) x$getElementText()) %>% unlist()
      text <- gsub("\\\n", ":", text)
      remDr$switchToFrame(NULL)
      return(text)
    } else {
      remDr$switchToFrame(NULL)
      return(NULL)
    } 
  }
}
