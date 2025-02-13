library(RSelenium)
library(tidyverse)
library(netstat)

# Scrape Go5 --------------------------------------------------------------
fbs_df <- read.csv("C:\\Users\\valen\\OneDrive\\Documents\\R_twitter\\fbs_df.csv")

# C-USA, MAC, Mountain West, Pac-12, Sun Belt
go5 <- c("C-USA", "MAC", "Mountain West", "Pac-12", "Sun Belt")
go5_attendance <- fbs_df %>% filter(Conference %in% go5)

rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "131.0.6778.265",
                             verbose = F,
                             port = free_port())

remDr <- rs_driver_object$client

# Get Go5 Lengths ---------------------------------------------------------
go5_ref <- go5_attendance$ref
go5_ref[[20]] <- "https://ksuowls.com/sports/football/schedule/2024"
ref <- go5_ref[-14]
ref[[13]] <- "https://fiusports.com/sports/football/schedule/2024"
ref[[21]] <- "https://libertyflames.com/sports/football/schedule/2024"
ref[[35]] <- "https://gobearkats.com/sports/football/schedule/2024"
ref <- ref[-39]

attendance <- list()

for (i in seq_along(ref)){
  html_elements <- get_boxscore(ref[[i]])
  boxscore_links <- lapply(html_elements, function(x) x$getElementAttribute("href")) %>%
    unlist() %>%
    unique()
  boxscore_links <- boxscore_links[!grepl("\\.pdf$", boxscore_links, ignore.case = TRUE)]
  attendance[[i]] <- lapply(boxscore_links, get_game_info)
}

# Clean data --------------------------------------------------------------
split_vec <- function(x){
  str_split(x, pattern = "\\\n") %>% unlist()
}

get_attendance <- function(x){
  values <- x[str_detect(x, pattern = regex("Attendance|Date", ignore_case = TRUE))] %>%
    str_to_title()
  mat <- str_split_fixed(values, pattern = ":", 2) %>% 
    t() %>%
    as.data.frame()
  colnames(mat) <- mat[1, ]
  mat <- mat[-1, , drop = FALSE]
  mat <- mat %>%
    mutate(across(everything(), trimws))
  return(mat)
}

# noticed a null value for this list
lapply(attendance[[1]], split_vec)

check_null <- list()
for (i in seq_along(attendance)){
  check_null[[i]] <- lapply(attendance[[i]], is.null) %>% unlist() %>% sum()
}

# Null values exist in elements: 1, 16, 25, 33
# Air Force
attendance[[1]]
af_baylor <- remDr$findElement(using = "xpath", "//div[@data-test-id = 's-list__root']")
af3 <- af_baylor$getElementText()

# Replace missing AF value
attendance[[1]][[3]] <- af3
attendance[[1]]

# Hawaii
lapply(attendance[[16]], is.null) %>%
  unlist() %>%
  which()
remDr$navigate(ref[16])
remDr$navigate("https://hawaiiathletics.com/sports/football/stats/2024/sam-houston/boxscore/27224")
hawaii_boxscore <- remDr$findElement(using = "xpath", "//dl[starts-with(@class, 'text-center')]")
hawaii_dt <- hawaii_boxscore$findChildElements(using = "tag name", "dt")
hw_dt <- lapply(hawaii_dt, function(x) x$getElementText()) %>% unlist()
hawaii_dd <- hawaii_boxscore$findChildElements(using = "tag name", "dd")
hw_dd <- lapply(hawaii_dd, function(x) x$getElementText()) %>% unlist()
hawaii_text <- str_c(hw_dt, hw_dd)
attendance[[16]][[3]] <- hawaii_text

# Marshall
remDr$navigate(ref[25])
lapply(attendance[[25]], is.null) %>% 
  unlist() %>% 
  which()
marshall_gc <- remDr$findElement(using = "xpath", "//div[starts-with(@class, 's-game-card__content__details')]")
marshall_text <- marshall_gc$getElementText()
attendance[[25]][[8]] <- marshall_text %>% unlist()

# old dominion
remDr$navigate(ref[[33]])
attendance[[33]] %>% 
  lapply(is.null) %>% 
  unlist() %>% 
  which()

odu_iframe <- remDr$findElement(using = "tag name", "iframe")
remDr$switchToFrame(odu_iframe)
odu_ul <- remDr$findElement(using = "xpath", "//ul[starts-with(@class, 'match-stats-header')]")
odu_items <- odu_ul$findChildElements(using = "tag name", "li")
odu_item_text <- odu_items %>% lapply(function(x) x$getElementText()) %>% unlist
odu_text <- gsub(pattern = "\\\n", replacement = ":", odu_item_text)
attendance[[33]][[2]] <- odu_text

# scraping southern miss and fresno state
# Southern Miss
sm_game_cards <- remDr$findElements(using = "xpath", "//div[@data-test-id = 's-list__root']")
sm_list <- lapply(sm_game_cards, function(x) x$getElementText()) %>% unlist()
sm_text <- lapply(sm_list, split_vec)

for (i in seq_along(sm_dates)){
  sm_text[[i]][[7]] <- sm_dates[[i]]
}

# Fresno State
remDr$navigate(go5_ref[14])
fresno_game_cards <- remDr$findElements(using = "xpath", "//div[@data-test-id = 's-list__root']")
fresno_list <- lapply(fresno_game_cards, function(x) x$getElementText()) %>% unlist()
fresno_text <- lapply(fresno_list, split_vec)

for (i in seq_along(fresno_dates)){
  fresno_text[[i]][[8]] <- fresno_dates[[i]]
}
# NA in element 9 is causing problemns, remove it
fresno_text[[9]] <- fresno_text[[9]][-7]


# making the tables
aac <- list()
for (i in seq_along(attendance)){
  aac[[i]] <- lapply(attendance[[i]], split_vec)
}

group_five <- go5_attendance %>%
  select(1, 2)
group_five <- group_five[-c(14, 40), ]

group_five_attendance <- data.frame(
  Date = as.character(),
  Attendance = as.numeric(),
  School = as.character(),
  Conference = as.character()
)

for (i in seq_along(aac)){
  df <- map_dfr(aac[[i]], get_attendance)
  df$School <- group_five$School[[i]]
  df$Conference <- group_five$Conference[[i]]
  group_five_attendance <- rbind(group_five_attendance, df)
}

sm_fresno_list <- list(fresno_text, sm_text)
sm_fresno_df <- go5_attendance[c(14, 40), c(1, 2)]

sm_fresno <- data.frame(
  Date = as.character(),
  Attendance = as.numeric(),
  School = as.character(),
  Conference = as.character()
)

for (i in seq_along(sm_fresno_list)){
  df <- map_dfr(sm_fresno_list[[i]], get_attendance)
  df$School <- sm_fresno_df$School[[i]]
  df$Conference <- sm_fresno_df$Conference[[i]]
  sm_fresno <- rbind(sm_fresno, df)
}

# impute date data
attendance_df <- rbind(group_five_attendance, sm_fresno) %>% arrange(School)
attendance_df$Date %>% is.na() %>% which()
# row 3 and 326,
# row 3 is the baylor vs air force game
attendance_df[3, 1] <- "09/14/2024"
# row 326 is marshall vs ULM
attendance_df %>% filter(School == "Marshall")
attendance_df[326, 1] <- "11/02/2024"

# impute attendance data
# 5 rows have zero attendance which is unusual 
attendance_df %>% filter(Attendance == 0)
# Schools: Eastern Michigan, Georgia State, Louisiana Tech, Miami (OH), New Mexico State
attendance_df %>% filter(School == "Eastern Michigan")
# Row 154, 194, 299, 343, 394
which(attendance_df[2] == 0)
# Used Western Michigan data
attendance_df[154, 2] <- 11263
# No attendance for this game.
attendance_df[194, ]
# Attendance: 9671 per ESPN: https://www.espn.com/college-football/game/_/gameId/401641010/louisiana-tech-new-mexico-st
attendance_df[299, 2] <- 9671
# Attendance: 9345 per ESPN: https://www.espn.com/college-football/game/_/gameId/401644691/miami-oh-bowling-green
attendance_df[343, 2] <- 9345
# Attendance: 13059
attendance_df[394, 2] <- 13059

# save df
write_csv(attendance_df, "C:/Users/valen/OneDrive/Documents/R_twitter/group_five_attendance.csv")

# terminate selenium server
system("taskkill /im java.exe /f")

# Marginalia --------------------------------------------------------------
# fresno state
remDr$navigate(go5_ref[[14]])
# 14
fresno_bulldogs <- remDr$findElements(using = "xpath", "//a[contains(@href, 'boxscore')]")
length(fresno_bulldogs)

# Lobos site is ok, need new scraping method
# Lobos:
remDr$navigate(ref[29])
iframe <- remDr$findElements(using = "tag name", "iframe")
remDr$switchToFrame(iframe[[1]])
content <- remDr$findElement(using = "xpath", "//div[contains(@class, 'box-score')]")
level_items <- content$findChildElements(using = "xpath", ".//div[starts-with(@class, 'level-item')]")
text <- lapply(level_items, function(x) x$getElementText()) %>% unlist()
remDr$switchToFrame(NULL)

# these two share the same html structure
remDr$navigate(ref[36])
iframe <- remDr$findElements(using = "tag name", "iframe")
remDr$switchToFrame(iframe[[1]])
stats <- remDr$findElements(using = "xpath", "//li[starts-with(@class, 'match-stats-header')]")
text <- lapply(stats, function(x) x$getElementText()) %>% unlist()
text <- gsub("\\\n", ":", text)

library(cfbfastR)
# "Fresno State" and "Southern Miss"
fresno_game_info <- cfbd_game_info(year = 2024, team = "Fresno State")
View(fresno_game_info)

# Days were incorrect when I cross referenced them on Google.
remDr$open()
remDr$navigate(go5_ref[[14]])
fresno_date_html <- remDr$findElements(using = "xpath", "//div[@data-test-id = 's-game-card-compact__header-date']")
fresno_date <- lapply(fresno_date_html, function(x) x$getElementText()) %>% unlist()
fresno_date <- fresno_date %>% str_sub(1, 6) %>% 
  trimws() %>%
  str_c(" 2024") %>%
  mdy() %>%
  as.character()
fresno_date <- fresno_date %>% ymd() %>% format("%m/%d/%Y")
fresno_dates <- str_c("Date", fresno_date, sep = ":")

# Southern Miss
southern_miss_gi <- cfbd_game_info(year = 2024, team = "Southern Miss")
southern_miss_gi %>% View()
southern_miss_start_date <- southern_miss_gi$start_date %>% str_sub(1, 10) 
southern_miss_start_date[[10]] <- "2024-11-16"
southern_miss_date <- gsub("-", "/", southern_miss_start_date)
sm_date <- southern_miss_date %>% ymd() %>% format("%m/%d/%Y")
sm_dates <- str_c("Date", sm_date, sep = ":")




