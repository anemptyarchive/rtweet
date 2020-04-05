
# ツイート数を棒グラフで可視化---------------------------------------------------------------


# 利用パッケージ -----------------------------------------------------------------

library(rtweet) # ツイート収集:get_timeline(), search_tweets()
library(dplyr) # データフレーム操作
library(lubridate) # 時間データ操作:floor_date(), as_date(), now()
library(stringr) # 文字列操作:str_remove_all()
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
#term <- "year"
#term <- "mon"
term <- "day"

# 指定した期間ごとにツイート数を集計
tw_count1 <- tw_time %>% 
  floor_date(unit = term) %>% # 指定した単位に切り捨て
  as_date() %>% # Date型に変換
  tibble(terms = .) %>% # データフレームに変換
  count(terms) # ツイート数をカウント

# ツイートがない期間が欠落する対策
term_list <- seq(
  floor_date(tail(tw_time, 1), term), # 一番古い時刻
  floor_date(now(), term), # 現在時刻
  by = term
) %>% # 指定した期間刻みのベクトルを作成
  as_date() %>% # Date型に変換
  tibble(terms = .)# データフレームに変換

# 集計結果を結合
tw_count2 <- left_join(term_list, tw_count1, by = "terms")

# ツイートがないと値がNAとなるので0に置換
tw_count2[["n"]][is.na(tw_count2[["n"]])] <- 0

# 棒グラフを作図
if(term == "day") {
  
  ggplot(tw_count2, aes(x = terms, y = n)) + 
    geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
    scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d") + # x軸目盛(日付)
    theme(axis.text.x = element_text(angle = 90)) + # x軸目盛の傾き
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year-mon-day") # ラベル
  
} else if(term == "mon") {
  
  ggplot(tw_count2, aes(x = terms, y = n)) + 
    geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
    scale_x_date(date_breaks  = "1 month", date_labels = "%Y-%m") + # x軸目盛(日付)
    theme(axis.text.x = element_text(angle = 90)) + # x軸目盛の傾き
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year-mon") # ラベル
  
} else if(term == "year") {
  
  ggplot(tw_count2, aes(x = terms, y = n)) + 
    geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # x軸目盛(日付)
    theme(axis.text.x = element_text(angle = 90)) + # x軸目盛の傾き
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year") # ラベル
  
}


# 時間単位で抽出 ------------------------------------------------------------

# 区切る期間を指定
term <- "hour" # 時間の場合

# 期間別ツイート数の集計
count_df <- data.frame(TERM = floor_date(tw_time, term)) %>% # データフレームを作成
            group_by(TERM) %>%          # 期間ごとにグループ化
            summarise(COUNT = n()) %>%  # グループごとにカウント
            filter(TERM >= as.POSIXct("2019-01-01")) %>%  # から
            filter(TERM <= as.POSIXct("2019-01-31"))      # まで



# 一番古い日から現在までの期間のベクトルを作成
term_vec <- seq(count_df[["TERM"]][1],               # 最古(最小値)
                count_df[["TERM"]][nrow(count_df)],  # 現在(最大値)
                by = term)                           # 間隔
term_vec[1:10]

# x軸ラベル用のベクトルを作成
x_label <- term_vec[seq(1, length(term_vec), by = 12)] # 間引く間隔を指定


# 描画
ggplot(data = count_df, mapping = aes(x = TERM, y = COUNT)) +  # データ
  geom_bar(stat = "identity", fill = "#00A968") +  # 棒グラフ
  scale_x_datetime(breaks = x_label, 
                   labels = str_remove(x_label, ":00$")) +  # x軸目盛(日付)
  theme(axis.text.x = element_text(angle = 90)) +           # x軸目盛の角度
  labs(title = paste0("@", screen_name, "のツイート数の推移"),  # タイトル
       x = term, y = "count")                                   # 軸ラベル



# try ---------------------------------------------------------------------

str_remove(as.character(x_label), ":00$")
str_remove(x_label, ":00$")

