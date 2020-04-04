
# ツイート数をヒートマップ化 --------------------------------------------------

library(rtweet)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)


####ツイート収集####
# アカウントの指定
tw_screen_name <- "anemptyarchive"

# ツイートを収集
tw_dat <- get_timeline(tw_screen_name, n = 10000, include_rts = FALSE)
#tw_dat <- search_tweets(tw_screen_name, n = 30000, include_rts = FALSE)
tw_dat <- tw_data

# ツイート日時の抽出
tw_time_utc <- tw_dat$created_at

# タイムゾーンの変換
tw_time_jst <- as.POSIXlt(as.POSIXct(tw_time_utc, tz = "Etc/GMT"), tz = "Japan")


####年月日別####
# セルの単位(期間)の指定
term <- "day"
term <- "mon"

# 期間別ツイート数の集計
ntweet_df1 <- data.frame(terms = as.Date(floor_date(tw_time_jst, term)), 
                         tweets = 1) %>% 
  group_by(terms) %>% 
  summarise(tweets = sum(tweets))

# 初ツイートから現在までの期間
nterm <- seq(floor_date(tw_time_jst[length(tw_time_jst)], term), 
             floor_date(Sys.time(), term), 
             by = term)
nterm_df <- data.frame(terms = as.Date(1, nterm))

# 全期間のツイート数
ntweet_df2 <- left_join(nterm_df, ntweet_df1, by = "terms")
ntweet_df2$tweets[is.na(ntweet_df2$tweets)] <- 0

# データフレームの整形
if(term == "day") {
  # day
  ntweet_df2$terms1 <- ntweet_df2$terms %>% 
    str_remove_all("-\\d{2}$")
  ntweet_df2$terms2 <- ntweet_df2$terms %>% 
    str_remove_all("^\\d{4}-\\d{2}-")
} else if(term == "mon") {
  # mon
  ntweet_df2$terms1 <- ntweet_df2$terms %>% 
    str_remove_all("-\\d{2}-\\d{2}$")
  ntweet_df2$terms2 <- ntweet_df2$terms %>% 
    str_remove_all("^\\d{4}-|-\\d{2}$")
}

ntweet_df3 <- ntweet_df2 %>% 
  select(terms1, terms2, tweets)


# ヒートマップの作図
ggplot(ntweet_df3, mapping = aes(x = terms1, y = terms2, fill = tweets)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white" , high = "#00A968") + 
  labs(title = paste("「@", screen_name, "」のツイート数のヒートマップ", sep = ""), 
       x = "", y = "")


####時別####
# 期間別ツイート数の集計
ntweet_df1 <- data.frame(terms = as.POSIXct(floor_date(tw_time_jst, "hour")), 
                         tweets = 1) %>% 
  group_by(terms) %>% 
  summarise(tweets = sum(tweets))

# 初ツイートから現在までの期間
nterm <- seq(floor_date(tw_time_jst[length(tw_time_jst)], "hour"), 
             floor_date(Sys.time(), "hour"), 
             by = "hour")
nterm_df <- data.frame(terms = nterm)

# 全期間別のツイート数
ntweet_df2 <- left_join(nterm_df, ntweet_df1, by = "terms")
ntweet_df2$tweets[is.na(ntweet_df2$tweets)] <- 0


# データフレームの整形
ntweet_df2$terms1 <- ntweet_df2$terms %>% 
  str_remove_all(" \\d{2}:\\d{2}:\\d{2}$")

ntweet_df2$terms2 <- ntweet_df2$terms %>% 
  str_remove_all("^\\d{4}-\\d{2}-\\d{2} ") %>% 
  str_remove_all(":\\d{2}:\\d{2}$")

ntweet_df3 <- ntweet_df2 %>% 
  select(terms1, terms2, tweets)

# プロットする期間の指定
day_1 <- "2019-01-01" # 開始日を指定
day_1 <- floor_date(tw_time_jst[length(tw_time_jst)], term) # 一番古いツイート日を指定

day_100 <- seq(as.Date(day_1), by = "day", len = 100) %>% 
  as.character() %>% 
  paste(collapse = "|")
ntweet_df4 <- filter(ntweet_df3, grepl(day_100, terms1))


# ヒートマップの作図
ggplot(ntweet_df4, mapping = aes(x = terms1, y = terms2, fill = tweets)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white" , high = "#00A968") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = paste("「@", tw_screen_name, "」のツイート数のヒートマップ", sep = ""), 
       x = "", y = "")


scale_x_discrete(breaks = seq(1, nrow(ntweet_df3), by = 3), 
                 labels = as.character(seq(as.Date(ntweet_df3$terms1[1]), 
                                           as.Date(ntweet_df3$terms1[nrow(ntweet_df3)]), 
                                           by = "3 day")))



####try####
scale_x_discrete(breaks = seq(1, nrow(ntweet_df3), by = 3), 
                 labels = as.character(seq(as.Date(ntweet_df3$terms1[1]), as.Date(ntweet_df3$terms1[nrow(ntweet_df3)]), by = "3 day"))) + 
  
  as.Date("2019-01", "%Y-%m")
as.Date(floor_date(tw_time_jst, term), format = "%Y-%m-%d")

