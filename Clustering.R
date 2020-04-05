
# ツイート頻度でクラスタリング---------------------------------------------------------------


# 利用パッケージ -----------------------------------------------------------------

library(rtweet) # ツイート収集:get_timeline()
library(dplyr) # データフレーム操作
library(lubridate) # 時間データ操作:floor_date()
library(tidyr) # データフレーム操作:pivot_longer()
library(ggplot2) # 作図
library(ggdendro) # 作図:ggdendrogram()


# ツイート収集 ------------------------------------------------------------------

# アカウントを指定
screen_names <- c(
  "consaofficial", "vega_official_", "atlrs_official", "REDSOFFICIAL", 
  "fctokyoofficial", "frontale_staff", "prompt_fmarinos", "bellmare_staff", 
  "yamagafc", "spulse_official", "Jubiloiwata_YFC", "nge_official", 
  "GAMBA_OFFICIAL", "crz_official", "visselkobe", "sanfrecce_SFC", 
  "saganofficial17", "TRINITAofficial"
)

# 期間を指定
tw_count <- seq(
  as.POSIXct("2020/01/01", tz = "Japan"), # から
  as.POSIXct("2020/02/01", tz = "Japan"), # まで
  by = "hour"
) %>% 
  tibble(terms = .)

# ツイート収集と集計
for(i in seq_along(screen_names)) {
  
  # ツイートを収集
  tw_data <- get_timeline(screen_names[i], n = 10000, include_rts = FALSE)

  # ツイート日時データを変換
  tw_time <- tw_data[["created_at"]] %>% # ツイート日時を抽出
    as.POSIXct(tz = "Asia/Tokyo") # 日本標準時に変換
  
  # 指定した期間ごとにツイート数を集計
  tmp_tw_count <- tw_time %>% 
    floor_date(unit = "hour") %>% # 指定した単位に切り捨て
    tibble(terms = .) %>% # データフレームに変換
    group_by(terms) %>% # グループ化
    summarise(!!screen_names[i] := n()) # ツイート数をカウント
  
  # 集計結果を結合
  tw_count <- left_join(tw_count, tmp_tw_count, by = "terms")
  
  # おまじない
  Sys.sleep(1)
}

# ツイートがないと値がNAとなるので0に置換
tw_count[is.na.data.frame(tw_count)] <- 0

# データフレームをlong型に変換
tw_count_long <- pivot_longer(
  tw_count, 
  cols = -terms, # 変換しない列
  names_to = "screen_name", # 現列名を格納する列の名前
  values_to = "n" # 現セルを格納する列の名前
)

# ヒートマップを作図
ggplot(tw_count_long, aes(x = screen_name, y = terms, fill = n)) +  # データ
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "#00A968") +  # 塗りつぶし色のグラデーション
  scale_y_datetime(date_breaks = "1 day", 
                   date_labels = "%Y-%m-%d %H") +  # y軸目盛(日時)
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # x軸目盛の角度
  labs(title = "アカウントごとのツイート数")


# クラスタリング -----------------------------------------------------------------

# クラスタリング
res_dendrogram <- tw_count[, -1] %>% # 非数値の列を落とす
  t() %>% # 転置
  dist() %>% # 距離(類似度)を測る
  hclust("ward.D2") %>% # クラスタリング
  dendro_data() # 作図用にデータを変換

# 描画
ggdendrogram(res_dendrogram, theme_dendro = FALSE)


# クラスタリング
res_dendrogram <- tw_count[, -1] %>% # 非数値の列を落とす
  t() %>% # 転置
  dist() %>% # 距離(類似度)を測る
  hclust("ward.D2") %>% # クラスタリング
  as.dendrogram() # 作図用にデータを変換

# 描画
plot(res_dendrogram)

