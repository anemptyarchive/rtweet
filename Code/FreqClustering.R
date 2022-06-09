
# ツイート頻度でクラスタリング---------------------------------------------------------------

# 利用パッケージ
library(rtweet)
library(tidyverse)
library(lubridate)
library(ggdendro)

# チェック用
library(ggplot2)


# ツイート収集 ------------------------------------------------------------------

# アカウントを指定
screen_name_vec <- c(
  "MorningMusumeMg", "angerme_upfront", "JuiceJuice_uf", 
  "tsubakifac_uf", "BEYOOOOONDS_", "ocha_norma", "kenshusei_uf"
)

# ツイートを収集
tw_df <- tibble::tibble()
for(screen_name in screen_name_vec) {
  # ツイートを収集
  tmp_tw_df <- rtweet::get_timeline(screen_name, n = 10000, include_rts = FALSE)
  
  # データを結合
  tw_df <- dplyr::bind_rows(tw_df, tmp_tw_df)
  
  # おまじない
  print(paste0("(", which(screen_name_vec == screen_name), "/", length(screen_name_vec), ") ", screen_name))
  Sys.sleep(3)
}


# 期間の指定 -------------------------------------------------------------------

# 集計開始日を指定
date_from <- "2022-05-01"
date_from <- min(tw_df[["created_at"]])

# 集計終了日を指定
date_to <- "2022-05-07"
date_to <- max(tw_df[["created_at"]])
date_to <- lubridate::today()

# POSIXct型に変換
datetime_from <- date_from |> 
  lubridate::as_date() |> 
  lubridate::as_datetime(tz = "Asia/Tokyo")
datetime_to <- date_to |> 
  lubridate::as_date() |> 
  lubridate::as_datetime(tz = "Asia/Tokyo") + lubridate::days(1)


# ツイート数の集計 ----------------------------------------------------------------

# ツイート数を集計
freq_wide_df <- tw_df  |> 
  dplyr::select(datetime = created_at, screen_name) |> # ツイート日時列を選択
  dplyr::mutate(
    datetime = datetime |> 
      #lubridate::with_tz(tzone = "Etc/GMT") |> # POSIXct型の協定世界時を明示
      lubridate::with_tz(tzone = "Asia/Tokyo") |> # POSIXct型の日本標準時に変換
      lubridate::floor_date(unit = "hour") # 1時間単位に切り捨て
  ) |> 
  dplyr::group_by(screen_name) |> # カウント用にグループ化
  dplyr::count(datetime, name = "n") |> # ツイート数をカウント
  dplyr::ungroup() |> # グループ化を解除
  tidyr::pivot_wider(
    id_cols = datetime, 
    names_from = screen_name, 
    values_from = n, 
    values_fill = 0
  ) |> # アカウントごとの列に展開
  dplyr::right_join(
    tibble::tibble(
      datetime = seq(from = datetime_from, to = datetime_to, by = "hour") |> 
        head(n = -1)
    ), 
    by = "datetime"
  ) |> # 指定した期間の全ての日時を作成
  dplyr::mutate(
    dplyr::across(.cols = !datetime, .fns = ~tidyr::replace_na(., 0))
  ) |> # ツイートの無い日時を0に置換
  dplyr::arrange(datetime)
freq_wide_df


# ヒートマップによる可視化 ------------------------------------------------------------------

# 作図用のデータフレームを作成
freq_long_df <- freq_wide_df |> 
  tidyr::pivot_longer(
    cols = !datetime, 
    names_to = "screen_name", 
    names_ptypes = list(screen_name = factor(levels = screen_name_vec)), 
    values_to = "n"
  )

# ヒートマップを作成
ggplot(freq_long_df, aes(x = screen_name, y = datetime, fill = n)) + 
  geom_tile() + # ヒートマップ
  scale_fill_gradient(low = "white", high = "#00A968") + # 塗りつぶし色
  scale_y_datetime(date_breaks = "1 day", date_labels = "%Y-%m-%d %H") + # y軸目盛
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # x軸目盛ラベル
  labs(title = "ツイート数") # ラベル


# クラスタリング -----------------------------------------------------------------

# クラスタリング
res_dendro <- freq_wide_df |> 
  dplyr::select(!datetime) |> # 非数値列を除去
  t() |> # 転置
  dist() |> # 距離(類似度)を計算
  hclust("ward.D2") |>  # クラスタリング
  ggdendro::dendro_data() # 作図用にデータを変換

# デンドログラムを作成
ggdendro::ggdendrogram(res_dendro, theme_dendro = FALSE)


# クラスタリング
res_dendro <- freq_wide_df |> 
  dplyr::select(!datetime) |> # 非数値列を除去
  t() |> # 転置
  dist() |> # 距離(類似度)を計算
  hclust("ward.D2") |>  # クラスタリング
  as.dendrogram() # 作図用にデータを変換

# デンドログラムを作成
plot(res_dendro)


