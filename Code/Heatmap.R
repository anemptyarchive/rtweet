
# ツイート数をヒートマップで可視化 --------------------------------------------------


# 利用パッケージ -----------------------------------------------------------------

library(rtweet) # ツイート収集:get_timeline(), search_tweets()
library(dplyr) # データフレーム操作
library(tidyr) # データフレーム操作:replace_na()
library(lubridate) # 時間データ操作:floor_date(), as_date(), now()
library(ggplot2) # 作図


# ツイート収集 ------------------------------------------------------------------

# アカウントを指定
screen_name <- "anemptyarchive"

# アカウント指定でツイートを収集
tw_data <- get_timeline(screen_name, n = 10000, include_rts = FALSE)

# キーワードを指定
#search_word <- "トピックモデル"
# 単語指定でツイートを収集
#tw_data <- search_tweets(search_word, n = 30000, include_rts = FALSE)

# ツイート日時データを変換
tw_time <- tw_data[["created_at"]] %>% # ツイート日時を抽出
  as.POSIXct(tz = "Etc/GMT") %>% # POSIXct型の協定世界時を明示
  as.POSIXlt(tz = "Asia/Tokyo") # POSIXlt型の日本標準時に変換


# 期間(年・月・日)を指定してカウント --------------------------------------------------------------------

# セルの単位(区切る期間)を指定
#term <- "year"
#term <- "mon"
term <- "day"

# 指定した期間ごとにツイート数を集計
tw_count1 <- tw_time %>% 
  floor_date(unit = term) %>% # 指定した期間に切り捨て
  as_date() %>% # Date型に変換
  tibble(terms = .) %>% # データフレームに変換
  count(terms) # ツイート数をカウント

# ツイートがない期間が欠落する対策
term_df <- seq(
  floor_date(tail(tw_time, 1), term), # 一番古い日時
  floor_date(now(), term), # 現在日時
  by = term
) %>% # 指定した期間刻みのベクトルを作成
  as_date() %>% # Date型に変換
  tibble(terms = .)# データフレームに変換

# 集計結果を結合
tw_count2 <- left_join(term_df, tw_count1, by = "terms") %>% 
  mutate(n = replace_na(n, 0)) # NAを0に置換

# 軸ラベル用にデータフレームを整形
if(term == "day") {
  
  tw_count2 <- tw_count2 %>% 
    mutate(year_mon = format(terms, "%Y-%m")) %>% # 年月を抽出
    mutate(day = format(terms, "%d")) # 日を抽出

} else if(term == "mon") {
  
  tw_count2 <- tw_count2 %>% 
    mutate(year = format(terms, "%Y")) %>% # 年を抽出
    mutate(mon = format(terms, "%m")) # 月を抽出
  
} else if(term == "year") {
  
  tw_count2 <- tw_count2 %>% 
    mutate(year = format(terms, "%Y")) # 年を抽出
  
}

# ヒートマップを作図
if(term == "day") {
  
  ggplot(tw_count2, aes(x = year_mon, y = day, fill = n)) + 
    geom_tile() + # ヒートマップ
    scale_fill_gradient(low = "white" , high = "#00A968") + # 塗りつぶしの濃淡
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # 軸目盛の傾き
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year-mon", y = "day") # ラベル
  
} else if(term == "mon") {
  
  ggplot(tw_count2, aes(x = year, y = mon, fill = n)) + 
    geom_tile() + # ヒートマップ
    scale_fill_gradient(low = "white" , high = "#00A968") + # 塗りつぶしの濃淡
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year", y = "mon") # ラベル
  
} else if(term == "year") {
  
  ggplot(tw_count2, aes(x = year, y = 0, fill = n)) + 
    geom_tile() + # ヒートマップ
    scale_fill_gradient(low = "white" , high = "#00A968") + # 塗りつぶしの濃淡
    ylim(c(-1, 1)) + # y軸の範囲
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year", y = "") # ラベル
  
}


# 時別にカウント -------------------------------------------------------------------

# 1時間ごとにツイート数を集計
tw_count1 <- tw_time %>% 
  floor_date(unit = "hour") %>% # 1時間単位で切り捨て
  as.POSIXct() %>% # POSIXct型に変換
  tibble(terms = .) %>% # データフレームに変換
  count(terms) # ツイート数をカウント

# ツイートがない期間が欠落する対策
term_df <- seq(
  floor_date(tw_time[length(tw_time)], "hour"), # 一番古い時刻
  floor_date(now(), "hour"), # 現在時刻
  by = "hour"
) %>% # 1時間刻みのベクトルを作成
  tibble(terms = .) # データフレームに変換

# 作図用にデータフレームを加工
tw_count2 <- left_join(term_df, tw_count1, by = "terms") %>% # 集計結果を結合
  mutate(n = replace_na(n, 0)) %>%  # NAを0に置換
  mutate(year_mon_day = as_date(terms)) %>% # 年月日を抽出
  mutate(hour = format(terms, "%H")) # 時間を抽出

# ヒートマップを作図
ggplot(tw_count2, aes(x = year_mon_day, y = hour, fill = n)) + 
  geom_tile() + # ヒートマップ
  scale_fill_gradient(low = "white" , high = "#00A968") + # 塗りつぶしの濃淡
  scale_x_date(breaks = seq(tw_count2[["year_mon_day"]][1], 
                            tw_count2[["year_mon_day"]][nrow(tw_count2)], 
                            by = "1 week")) + # x軸目盛(日付)
  theme(axis.text.x = element_text(angle = 90)) + # x軸目盛の傾き
  labs(title = paste0("@", screen_name, "のツイート数"), 
       x = "year-mon-day", y = "hour") # ラベル


