
# ツイート数を棒グラフで可視化---------------------------------------------------------------


# 利用パッケージ -----------------------------------------------------------------

library(rtweet) # ツイート収集:get_timeline(), search_tweets()
library(dplyr) # データフレーム操作
library(lubridate) # 時間データ操作:floor_date(), as_date(), now()
library(ggplot2) # 作図


# ツイート収集 ------------------------------------------------------------------

# アカウントを指定
screen_name <- "anemptyarchive"

# アカウント指定でツイートを収集
tw_data <- get_timeline(screen_name, n = 10000, include_rts = FALSE)

# 単語を指定
#search_word <- "トピックモデル"
# 単語指定でツイートを収集
#tw_data <- search_tweets(search_word, n = 30000, include_rts = FALSE)

# ツイート日時データを変換
tw_time <- tw_data[["created_at"]] %>% # ツイート日時を抽出
  as.POSIXct(tz = "Etc/GMT") %>% # POSIXct型の協定世界時を明示
  as.POSIXlt(tz = "Asia/Tokyo") # POSIXlt型の日本標準時に変換


# 期間(年・月・日)を指定してカウント ------------------------------------------------------------

# セルの単位(区切る期間)を指定
term <- "year"
term <- "mon"
term <- "day"

# 指定した期間ごとにツイート数を集計
tw_count <- tw_time %>% 
  floor_date(unit = term) %>% # 指定した単位に切り捨て
  as_date() %>% # Date型に変換
  tibble(terms = .) %>% # データフレームに変換
  count(terms) # ツイート数をカウント

# 棒グラフを作図
if(term == "day") {
  
  ggplot(tw_count, aes(x = terms, y = n)) + 
    geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
    scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d") + # x軸目盛(日付)
    theme(axis.text.x = element_text(angle = 90)) + # x軸目盛の傾き
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year-mon-day") # ラベル
  
} else if(term == "mon") {
  
  ggplot(tw_count, aes(x = terms, y = n)) + 
    geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
    scale_x_date(date_breaks  = "1 month", date_labels = "%Y-%m") + # x軸目盛(日付)
    theme(axis.text.x = element_text(angle = 90)) + # x軸目盛の傾き
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year-mon") # ラベル
  
} else if(term == "year") {
  
  ggplot(tw_count, aes(x = terms, y = n)) + 
    geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # x軸目盛(日付)
    theme(axis.text.x = element_text(angle = 90)) + # x軸目盛の傾き
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year") # ラベル
  
}


# 時間単位でカウント ------------------------------------------------------------

# セルの単位(区切る期間)を指定
term <- "hour"

# 指定した期間ごとにツイート数を集計
tw_count <- tw_time %>% 
  floor_date(unit = term) %>% # 指定した単位に切り捨て
  as.POSIXct() %>% # POSIXct型に変換
  tibble(terms = .) %>% # データフレームに変換
  count(terms) %>% # ツイート数をカウント
  filter(terms >= as.POSIXct("2020-01-01")) %>% # から
  filter(terms <= as.POSIXct("2020-01-31")) # まで

# 棒グラフを作図
ggplot(tw_count, aes(x = terms, y = n)) + 
  geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
  scale_x_datetime(date_breaks = "12 hours", 
                   date_labels = "%Y-%m-%d %H") + # x軸目盛(日時)
  theme(axis.text.x = element_text(angle = 90)) + # x軸目盛の傾き
  labs(title = paste0("@", screen_name, "のツイート数"), 
       x = "year-mon-day hour") # ラベル


