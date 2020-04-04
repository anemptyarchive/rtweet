
# ツイート数の棒グラフ化---------------------------------------------------------------

## 利用パッケージ
# ツイート収集
library(rtweet)    # get_timeline(), search_tweets()
# データフレーム操作
library(dplyr)
#文字列操作
library(stringr)   # str_remove_all()
# 時間データ操作
library(lubridate) # floor_date()
# 作図
library(ggplot2)

# ツイート収集 ------------------------------------------------------------------

# アカウントを指定
screen_name <- "anemptyarchive"   # 任意のアカウントを指定する
# 検索ワードを指定
search_word <- "トピックモデル"   # 任意の単語を指定する

# ツイートを収集
#tw_data <- get_timeline(screen_name, n = 10000, include_rts = FALSE)  # アカウトを指定
#tw_data <- search_tweets(search_word, n = 30000, include_rts = FALSE) # 単語を指定
tw_data <- readRDS("tw_data/tw_data_anemptyarchive.obj")


# ツイート日時を抽出
tw_time <- tw_data$created_at %>% 
           as.POSIXlt(tz = "Japan") # タイムゾーンを変換
class(tw_time)
tw_time[1:10]


# 任意の期間(年・月・日)を指定して抽出 ------------------------------------------------------------

# 区切る期間を指定
term <- "year" # 年の場合
term <- "mon"  # 月の場合
term <- "day"  # 日の場合


# 期間別ツイート数の集計
count_df <- data.frame(TERM = as.Date(floor_date(tw_time, term))) %>% # データフレームを作成
            group_by(TERM) %>%     # 期間ごとにグループ化
            summarise(COUNT = n()) # グループごとにカウント

# 一番古い日から現在までの期間のベクトルを作成
term_vec <- seq(floor_date(tail(tw_time, 1), term),  # 最古(最小値)
                floor_date(Sys.time(), term),        # 現在(最大値)
                by = term)                           # 間隔
term_vec[1:10]

# x軸目盛用のベクトルを作成
x_label <- term_vec[seq(1, length(term_vec), by = 10)] # 間引く間隔を指定
x_label <- term_vec

# 描画
ggplot(data = count_df, mapping = aes(x = TERM, y = COUNT)) +  # データ
  geom_bar(stat = "identity", fill = "#00A968") +      # 棒グラフ
  scale_x_date(breaks = as.Date(x_label), 
               labels = as.Date.character(x_label)) +  # x軸目盛(日付)
  theme(axis.text.x = element_text(angle = 90)) +      # x軸目盛の角度
  labs(title = paste0("@", screen_name, "のツイート数の推移"),  # タイトル
       x = term, y = "count")                                   # 軸ラベル

# 描画
ggplot(data = count_df, mapping = aes(x = TERM, y = COUNT)) +  # データ
  geom_bar(stat = "identity", fill = "#00A968") +      # 棒グラフ
  scale_x_datetime(labels = date_format("%m-%d")) +  # x軸目盛(日付)
  theme(axis.text.x = element_text(angle = 90)) +      # x軸目盛の角度
  labs(title = paste0("@", screen_name, "のツイート数の推移"),  # タイトル
       x = term, y = "count")                                   # 軸ラベル


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

