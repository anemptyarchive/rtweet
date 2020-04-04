
# ツイート数をヒートマップ化 --------------------------------------------------


# 利用パッケージ -----------------------------------------------------------------

library(rtweet) # ツイート収集:get_timeline(), search_tweets()
library(dplyr) # データフレーム操作
library(lubridate) # 時間データ操作:floor_date(), as_date()
library(stringr) #文字列操作:str_remove_all()
library(ggplot2) # 作図


# ツイート収集 ------------------------------------------------------------------

# アカウントを指定
screen_name <- "anemptyarchive"
# キーワードを指定
#keyword <- "トピックモデル"

# アカウント指定でツイートを収集:
tw_dat <- get_timeline(screen_name, n = 10000, include_rts = FALSE)
# キーワード指定でツイートを収集:
#tw_dat <- search_tweets(keyword, n = 30000, include_rts = FALSE)

# ツイート日時を抽出
tw_time_utc <- tw_dat[["created_at"]]

# タイムゾーンを変換
tw_time_jst <- tw_time_utc %>% 
  as.POSIXct(tz = "Etc/GMT") %>% 
  as.POSIXlt(tz = "Japan")


# 年月日別に作図 --------------------------------------------------------------------

# セルの単位(期間)を指定
#term <- "year"
#term <- "mon"
term <- "day"

# 期間別ツイート数の集計
ntweet_df1 <- tw_time_jst %>% 
  floor_date(unit = term) %>% 
  as_date() %>% 
  tibble(terms = .) %>% 
  count(terms)

# ツイートがない時用の対策
nterm_df <- seq(
  floor_date(tw_time_jst[length(tw_time_jst)], term), # 一番古い時刻
  floor_date(Sys.time(), term), # 現在時刻
  by = term
) %>% 
  as_date() %>% 
  tibble(terms = .)

# 結合
ntweet_df2 <- left_join(nterm_df, ntweet_df1, by = "terms")

# ツイートがないと値がNAとなるので0に置換
ntweet_df2[["n"]][is.na(ntweet_df2[["n"]])] <- 0

# データフレームの整形
if(term == "day") {
  ntweet_df2 <- ntweet_df2 %>% 
    mutate(terms1 = str_remove_all(terms, "-\\d{2}$")) %>%  # 年月を抽出
    mutate(terms2 = str_remove_all(terms, "^\\d{4}-\\d{2}-")) # 日を抽出
} else if(term == "mon") {
  ntweet_df2 <- ntweet_df2 %>% 
    mutate(terms1 = str_remove_all(terms, "-\\d{2}-\\d{2}$")) %>%  # 年を抽出
    mutate(terms2 = str_remove_all(terms, "^\\d{4}-|-\\d{2}$")) # 月を抽出
} else if(term == "year") {
  ntweet_df2 <- ntweet_df2 %>% 
    mutate(year = str_remove_all(terms, "-\\d{2}-\\d{2}$")) # 年を抽出
}

# ヒートマップを作図
if(term == "day") {
  ggplot(ntweet_df2, aes(x = terms1, y = terms2, fill = n)) + 
    geom_tile() + # ヒートマップ
    scale_fill_gradient(low = "white" , high = "#00A968") + # 塗りつぶしの濃淡
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # 軸目盛の傾き
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year-mon", y = "day") # ラベル
} else if(term == "mon") {
  ggplot(ntweet_df2, aes(x = terms1, y = terms2, fill = n)) + 
    geom_tile() + # ヒートマップ
    scale_fill_gradient(low = "white" , high = "#00A968") + # 塗りつぶしの濃淡
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year", y = "mon") # ラベル
} else if(term == "year") {
  ggplot(ntweet_df2, aes(x = year, y = 0, fill = n)) + 
    geom_tile() + # ヒートマップ
    scale_fill_gradient(low = "white" , high = "#00A968") + # 塗りつぶしの濃淡
    ylim(c(-1, 1)) + # y軸の範囲
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year", y = "") # ラベル
}


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
  labs(title = paste("「@", screen_name, "」のツイート数のヒートマップ", sep = ""), 
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

