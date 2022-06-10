
# ツイートテキストを棒グラフで可視化 -------------------------------------------------------

# 利用するパッケージ
library(RMeCab)
library(tidyverse)
library(lubridate)
library(gganimate)

# チェック用
library(ggplot2)


# ツイートの収集 ------------------------------------------------------------------

# アカウントを指定
screen_name <- "anemptyarchive"

# アカウントでツイートを収集
tw_df <- rtweet::get_timeline(screen_name, n = 10000, include_rts = FALSE)


# 単語を指定
#search_word <- "トピックモデル"

# 単語でツイートを収集
#tw_df <- rtweet::search_tweets(search_word, n = 30000, include_rts = FALSE)


# テキストの前処理 ----------------------------------------------------------------------

# 集計の単位を指定
unit <- "year"
unit <- "mon"
unit <- "day"

# 指定した単位でツイートテキストを結合
text_df <- tw_df |> 
  dplyr::select(date = created_at, text) |> # 利用する列を選択
  dplyr::mutate(
    date = date |> 
      lubridate::with_tz(tzone = "Asia/Tokyo") |> # 日本標準時に変換
      lubridate::as_date() |> # Date型に変換
      lubridate::floor_date(unit = unit), # 指定した単位に切り捨て
    text = text |> 
      stringr::str_remove_all(pattern = "https?://[\\w!\\?/\\+\\-_~=;\\.,\\*&@#\\$%\\(\\)'\\[\\]]+") |> # urlを除去
      stringr::str_remove_all(pattern = "@.*\\s") # リプライ先を除去
  ) |> 
  dplyr::group_by(date) |> # テキストの結合用にグループ化
  dplyr::summarise(
    text = paste(text, sep = " \n ", collapse = ""), 
    n = dplyr::n(), 
    .groups = "drop"
  ) # ツイートテキストを結合


# 棒グラフで可視化 ----------------------------------------------------------------

### ・形態素解析 -----

# 描画する日付を指定
date_val <- "2022-01-01"

# ツイートテキストを書き出し
text_df |> 
  dplyr::filter(date == lubridate::as_date(date_val)) |> # 指定した日付のデータを抽出
  dplyr::pull(text) |> # テキストを抽出
  write(file = "tmp_data/tmp.txt") # 書き出し

# 形態素解析
mecab_df <- RMeCab::docDF(target = "tmp_data/tmp.txt", type = 1)


### ・出現頻度の集計 -----

# 利用する品詞を指定
pos1_vec <- c("名詞", "動詞", "形容詞")
pos2_vec <- c("一般", "固有名詞", "サ変接続", "形容動詞語幹", "ナイ形容詞語幹", "自立")

# 削除する単語を指定
stopword_symbol_vec <- c("\\(", "\\)", "\\{", "\\}", "\\[", "]", "「", "」", "【", "】", ",", "_", "--", "-", "!", "#", "\\.", "\\$", "\\\\")
stopword_term_vec <- c("る", "ある", "する", "せる", "できる", "なる", "やる", "れる", "いい", "ない")

# 描画する単語数を指定
word_size <- 50

# 単語の出現頻度を集計
freq_df <- mecab_df |> 
  dplyr::filter(POS1 %in% pos1_vec) |> # 指定した品詞大分類を抽出
  dplyr::filter(POS2 %in% pos2_vec) |> # 指定した品詞小分類を抽出
  dplyr::filter(!stringr::str_detect(TERM, pattern = paste0(stopword_symbol_vec, collapse = "|"))) |> # 指定した記号を削除
  dplyr::filter(!stringr::str_detect(TERM, pattern = paste0(stopword_term_vec, collapse = "|"))) |> # 指定した単語を削除
  dplyr::select(term = TERM, frequency = "tmp.txt") |> # 単語と頻度の列を取り出し
  dplyr::group_by(term) |> # 頻度の合計用にグループ化
  dplyr::summarise(frequency = sum(frequency), .groups = "drop") |> # 頻度を合計
  dplyr::arrange(dplyr::desc(frequency)) |> # 降順に並べ替え
  head(n = word_size) # 頻度上位語を抽出


### ・棒グラフの作図 -----

# 棒グラフを作成
ggplot(freq_df, aes(x = reorder(term, frequency), y = frequency)) + 
  geom_bar(stat = "identity", fill = "hotpink", color = "white") + # 棒グラフ
  coord_flip(expand = FALSE) + # 軸の入れ替え
  labs(
    title = if(term == "year") {
      paste0(lubridate::year(date_val), "年のツイート")
    } else if(term == "mon") {
      paste0(lubridate::year(date_val), "年", lubridate::month(date_val), "月のツイート")
    }, 
    x = "", y = "頻度"
  )


# バーチャートレースによる可視化 ---------------------------------------------------------------

### ・形態素解析 -----

# 集計開始日を指定
date_from <- "2021-01-01"
date_from <- min(tw_df[["created_at"]])

# 集計終了日を指定
date_to <- "2021-12-31"
date_to <- max(tw_df[["created_at"]])
date_to <- lubridate::today()

# 描画する日付を抽出
date_vec <- text_df |> 
  dplyr::filter(date >= lubridate::as_date(date_from), date <= lubridate::as_date(date_to)) |> # 指定した期間のデータを抽出
  dplyr::pull(date) # ベクトルに変換

# ツイートテキストを書き出し
file.remove(paste0("tmp_data/", list.files("tmp_data"))) # フォルダ内のファイルを削除
for(i in seq_along(date_vec)) {
  # 日付を取得
  date_val <- date_vec[i]
  
  # ツイートテキストを書き出し
  text_df |> 
    dplyr::filter(date == lubridate::as_date(date_val)) |> # 指定した日付のデータを抽出
    dplyr::pull(text) |> # テキストを抽出
    write(file = paste0("tmp_data/", date_val, ".txt")) # 書き出し
}

# 形態素解析
mecab_df <- RMeCab::docDF(target = "tmp_data", type = 1) |>  
  tibble::as_tibble()


### ・出現頻度の集計 -----

# 単語数を指定
max_rank <- 30

# 頻出語を抽出
rank_df <- mecab_df |> 
  dplyr::filter(POS1 %in% pos1_vec) |> # 指定した品詞大分類を抽出
  dplyr::filter(POS2 %in% pos2_vec) |> # 指定した品詞小分類を抽出
  dplyr::filter(!stringr::str_detect(TERM, pattern = paste0(stopword_symbol_vec, collapse = "|"))) |> # 不要な記号を削除
  dplyr::filter(!stringr::str_detect(TERM, pattern = paste0(stopword_term_vec, collapse = "|"))) |> # 不要な単語を削除
  dplyr::select(term = TERM, !c("POS1", "POS2")) |> # 単語と頻度の列を取り出し
  tidyr::pivot_longer(cols = !term, names_to = "date", values_to = "frequency") |> # 頻度列をまとめる
  dplyr::mutate(
    date = date |> 
      stringr::str_remove(pattern = ".txt") |> 
      lubridate::as_date()
  ) |> # 日付情報に変換
  dplyr::group_by(term, date) |> # 頻度の合計用にグループ化
  dplyr::summarise(frequency = sum(frequency), .groups = "drop") |> # 頻度を合計
  dplyr::group_by(date) |> # 順位付け用にグループ化
  dplyr::mutate(ranking = dplyr::row_number(-frequency)) |> # 順位付け
  dplyr::ungroup() |> # グループ化を解除
  dplyr::filter(ranking <= max_rank) |> # 頻度上位語を抽出
  dplyr::arrange(date, ranking) # 昇順に並べ替え
rank_df


### ・バーチャートレースの作図 -----

# フレーム数を取得
n <- length(unique(rank_df[["date"]]))

# 遷移フレーム数を指定
t <- 8

# 停止フレーム数を指定
s <- 2

# バーチャートレースを作成:(y軸固定)
anim <- ggplot(rank_df, aes(x = ranking, y = frequency, fill = term, color = term)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 頻度バー
  geom_text(aes(y = 0, label = paste(" ", frequency)), hjust = 0, color = "white") + # 頻度ラベル
  geom_text(aes(y = 0, label = paste(term, " ")), hjust = 1) + # 単語ラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  coord_flip(clip = "off", expand = FALSE) + # 軸の入替
  scale_x_reverse(breaks = 1:max_rank) + # x軸の反転
  theme(
    axis.title.y = element_blank(), # 縦軸のラベル
    axis.text.y = element_blank(), # 縦軸の目盛ラベル
    panel.grid.major.y = element_blank(), # 縦軸の主目盛線
    panel.grid.minor.y = element_blank(), # 縦軸の補助目盛線
    #panel.border = element_blank(), # グラフ領域の枠線
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 50, b = 10, l = 100, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = paste0("@", screen_name, "のツイート"), 
    subtitle = paste0(
      "{lubridate::year(closest_state)}年", 
      "{stringr::str_pad(lubridate::month(closest_state), width = 2, pad = 0)}月"
    ), 
    y = "頻度"
  ) # ラベル

# gif画像を作成
g <- gganimate::animate(
  plot = anim, nframes = n*(t+s), fps = t+s, width = 1200, height = 900
)
g

# gif画像を保存
gganimate::anim_save(filename = "figure/WordBarChartRace.gif", animation = g)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, nframes = n*(t+s), fps = t+s, width = 1200, height = 900, 
  renderer = gganimate::av_renderer(file = "figure/WordBarChartRace.mp4")
)


