
# ツイート数をヒートマップで可視化 --------------------------------------------------

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
date_from <- "2022-01-01"
date_from <- min(tw_df[["created_at"]])

# 集計終了日を指定
date_to <- "2022-12-31"
date_to <- max(tw_df[["created_at"]])
date_to <- lubridate::today()


# 年別・月別・日別で集計 ----------------------------------------------------------------------

# 集計単位を指定
unit <- "year"
unit <- "month"
unit <- "day"

# Date型に変換
date_from <- date_from |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = unit)
date_to <- date_to |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = unit)

# 指定した単位ごとにツイート数を集計
freq_df <- tw_df |> 
  dplyr::select(date = created_at) |> # ツイート日時列を選択
  dplyr::mutate(
    date = date |> 
      #lubridate::with_tz(tzone = "Etc/GMT") |> # POSIXct型の協定世界時を明示
      lubridate::with_tz(tzone = "Asia/Tokyo") |> # POSIXct型の日本標準時に変換
      lubridate::floor_date(unit = unit) |> # 指定した単位に切り捨て
      lubridate::as_date() # Date型に変換
  ) |> 
  dplyr::count(date, name = "n") |> # ツイート数をカウント
  dplyr::right_join(
    tibble::tibble(date = seq(from = date_from, to = date_to, by = unit)), 
    by = "date"
  ) |> # 全ての日付に統合
  dplyr::mutate(n = tidyr::replace_na(n, replace = 0)) |> # ツイートの無い日付を0に置換
  dplyr::arrange(date) # 昇順に並べ替え

# 指定した単位に応じて作図
if(unit == "day") {
  # 軸用の文字列を作成
  freq_df <- freq_df |> 
    dplyr::mutate(
      year_mon = format(date, format = "%Y-%m"), # 年月を抽出
      day = format(date, format = "%d") # 日を抽出
    )
  
  # ヒートマップを作成
  ggplot(data = freq_df, mapping = aes(x = year_mon, y = day, fill = n)) + 
    geom_tile() + # ヒートマップ
    #geom_text(mapping = aes(label = n), color ="white") + # テキストラベル
    scale_fill_gradient(low = "white" , high = "#00A968") + # 塗りつぶし色
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # 軸目盛の傾き
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year-month", y = "day") # ラベル
  
} else if(unit == "month") {
  # 軸用の文字列を作成
  freq_df <- freq_df |> 
    dplyr::mutate(
      year = format(date, format = "%Y"), # 年を抽出
      mon = format(date, "%m") # 月を抽出
    )
  
  # ヒートマップを作成
  ggplot(data = freq_df, mapping = aes(x = year, y = mon, fill = n)) + 
    geom_tile() + # ヒートマップ
    #geom_text(mapping = aes(label = n), color ="white") + # テキストラベル
    scale_fill_gradient(low = "white" , high = "#00A968") + # 塗りつぶし色
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year", y = "month") # ラベル
  
} else if(unit == "year") {
  # 軸用のラベルを作成
  freq_df <- freq_df |> 
    dplyr::mutate(year = format(date, format = "%Y")) # 年を抽出
  
  # ヒートマップを作成
  ggplot(data = freq_df, mapping = aes(x = year, y = 0, fill = n)) + 
    geom_tile() + # ヒートマップ
    #geom_text(mapping = aes(label = n), color ="white") + # テキストラベル
    scale_fill_gradient(low = "white" , high = "#00A968") + # 塗りつぶし色
    scale_y_continuous(breaks = NULL, limits = c(-1, 1)) + # y軸目盛
    labs(title = paste0("@", screen_name, "のツイート数"), 
         x = "year", y = "") # ラベル
  
}


# 時別で集計 -------------------------------------------------------------------

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
  dplyr::right_join(
    tibble::tibble(
      datetime = seq(from = datetime_from, to = datetime_to, by = "hour") |> 
        head(n = -1)
    ), 
    by = "datetime"
  ) |> # 全ての日時に統合
  dplyr::mutate(
    n = tidyr::replace_na(n, replace = 0), 
    year_mon_day = lubridate::as_date(datetime), 
    hour = format(datetime, format = "%H")
  ) |> # ツイートの無い日時を0に置換
  dplyr::arrange(datetime) # 昇順に並べ替え

# ヒートマップを作成
ggplot(data = freq_df, mapping = aes(x = year_mon_day, y = hour, fill = n)) + 
  geom_tile() + # ヒートマップ
  scale_fill_gradient(low = "white" , high = "#00A968") + # 塗りつぶし色
  scale_x_date(breaks = seq(lubridate::as_date(datetime_from), lubridate::as_date(datetime_to), by = "1 week")) + # x軸目盛
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # x軸目盛の傾き
  labs(title = paste0("@", screen_name, "のツイート数"), 
       x = "year-mon-day", y = "hour") # ラベル


