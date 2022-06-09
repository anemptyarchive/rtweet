
# ツイート数を棒グラフで可視化---------------------------------------------------------------

# 利用パッケージ
library(rtweet)
library(tidyverse)
library(lubridate)

# チェック用
library(ggplot2)


# ツイートの収集 ------------------------------------------------------------------

# アカウントを指定
screen_name <- "anemptyarchive"

# アカウントでツイートを収集
tw_df <- rtweet::get_timeline(screen_name, n = 10000, include_rts = TRUE)


# 単語を指定
#search_word <- "トピックモデル"

# 単語でツイートを収集
#tw_df <- rtweet::search_tweets(search_word, n = 30000, include_rts = FALSE)


# 期間の指定 -------------------------------------------------------------------

# 集計開始日を指定
date_from <- "2021-01-01"
date_from <- min(tw_df[["created_at"]])

# 集計終了日を指定
date_to <- "2021-12-31"
date_to <- max(tw_df[["created_at"]])
date_to <- lubridate::today()


# 年別・月別・日別で集計 ------------------------------------------------------------

# 集計単位を指定
term <- "year"
term <- "mon"
term <- "day"

# Date型に変換
date_from <- date_from |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = term)
date_to <- date_to |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = term)

# 指定した単位ごとにツイート数を集計
freq_df <- tw_df |> 
  dplyr::select(date = created_at) |> # ツイート日時列を選択
  dplyr::mutate(
    date = date |> 
      #lubridate::with_tz(tzone = "Etc/GMT") |> # POSIXct型の協定世界時を明示
      lubridate::with_tz(tzone = "Asia/Tokyo") |> # POSIXct型の日本標準時に変換
      lubridate::floor_date(unit = term) |> # 指定した単位に切り捨て
      lubridate::as_date() # Date型に変換
  ) |> 
  dplyr::count(date, name = "n") |> # ツイート数をカウント
  dplyr::filter(date >= date_from, date <= date_to) # 期間内のデータを抽出

# 指定した単位に応じて作図
if(term == "day") {
  # 棒グラフを作図
  ggplot(freq_df, aes(x = date, y = n)) + 
    geom_bar(stat = "identity", fill = "#00A968", color = "#00A968") + # 棒グラフ
    scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") + # x軸目盛
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # x軸目盛ラベル
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year-mon-day", y = "frequency") # ラベル
  
} else if(term == "mon") {
  # 棒グラフを作図
  ggplot(freq_df, aes(x = date, y = n)) + 
    geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
    scale_x_date(date_breaks  = "1 month", date_labels = "%Y-%m") + # x軸目盛
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # x軸目盛ラベル
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year-mon", y = "frequency") # ラベル
  
} else if(term == "year") {
  # 棒グラフを作図
  ggplot(freq_df, aes(x = date, y = n)) + 
    geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # x軸目盛
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year", y = "frequency") # ラベル
  
}


# 時別で集計 ------------------------------------------------------------

# POSIXct型に変換
datetime_from <- date_from |> 
  lubridate::as_date() |> 
  lubridate::as_datetime(tz = "Asia/Tokyo")
datetime_to <- date_to |> 
  lubridate::as_date() |> 
  lubridate::as_datetime(tz = "Asia/Tokyo") + lubridate::days(1)

# 時刻ごとにツイート数を集計
freq_df <- tw_df |> 
  dplyr::select(datetime = created_at) |> # ツイート日時列を選択
  dplyr::mutate(
    datetime = datetime |> 
      #lubridate::with_tz(tzone = "Etc/GMT") |> # POSIXct型の協定世界時を明示
      lubridate::with_tz(tzone = "Asia/Tokyo") |> # POSIXct型の日本標準時に変換
      lubridate::floor_date(unit = "hour") # 1時間単位に切り捨て
  ) |> 
  dplyr::count(datetime, name = "n") |> # ツイート数をカウント
  dplyr::filter(datetime >= datetime_from, datetime < datetime_to) # 期間内のデータを抽出
dplyr::arrange(datetime) # 昇順に並べ替え


# 棒グラフを作図
ggplot(freq_df, aes(x = datetime, y = n)) + 
  geom_bar(stat = "identity", fill = "#00A968", color = "#00A968") + # 棒グラフ
  scale_x_datetime(date_breaks = "1 week", date_labels = "%Y-%m-%d %H") + # x軸目盛
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # x軸目盛ラベル
  labs(title = paste0("@", screen_name, "のツイート数"), 
       x = "year-mon-day hour", y = "frequency") # ラベル

