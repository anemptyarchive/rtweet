
# ツイート頻度でクラスタリング---------------------------------------------------------------

# 利用パッケージ
library(rtweet) # ツイート収集:get_timeline()
library(dplyr) # データフレーム操作
library(tidyr) # データフレーム操作:pivot_longer()
library(lubridate) # 時間データ操作:floor_date()
library(ggplot2) # 作図
library(ggdendro) # 樹形図:dendro_data(), ggdendrogram()


# ツイート収集 ------------------------------------------------------------------

# アカウントを指定
screen_names <- c(
  "MorningMusumeMg", "angerme_upfront", "JuiceJuice_uf", 
  "tsubakifac_uf", "BEYOOOOONDS_", "kenshusei_uf"
)

# ツイート収集と集計
tw_count <- tibble(terms = floor_date(Sys.time(), "hour")) ## (本当は列名だけを持つ空のdfを作りたい)
for(i in seq_along(screen_names)) {
  
  # ツイートを収集
  tw_data <- get_timeline(screen_names[i], n = 10000, include_rts = FALSE)
  
  # 指定した期間ごとにツイート数を集計
  tmp_tw_count <- tw_data[["created_at"]] %>% # ツイート日時を抽出
    as.POSIXct(tz = "Asia/Tokyo") %>% # 日本標準時に変換
    floor_date(unit = "hour") %>% # 1時間ごとに切り捨て
    tibble(terms = .) %>% # データフレームに変換
    group_by(terms) %>% # グループ化
    summarise(!!screen_names[i] := n()) # ツイート数をカウント
  
  # 集計結果を結合
  tw_count <- full_join(tw_count, tmp_tw_count, by = "terms")
  
  # おまじない
  Sys.sleep(1)
  print(paste0(screen_names[i], "...", round(i / length(screen_names) * 100, 1), "%"))
}
# とりあえず保存
#saveRDS(tw_count, file = "tw_data/freq_hello.rds")
#readRDS("tw_data/freq_hello.rds")
head(tw_count)

# 期間を指定してツイート数を抽出
tw_count2 <- seq(
  as.POSIXct("2020/04/01", tz = "Japan"), # から
  as.POSIXct("2020/05/01", tz = "Japan"), # まで
  by = "hour"
) %>% 
  tibble(terms = .) %>% # 指定した範囲のdfを作成
  left_join(tw_count, by = "terms") # 範囲内のツイート数を結合

# ツイートがないと値がNAとなるので0に置換
tw_count2[is.na.data.frame(tw_count2)] <- 0
head(tw_count2)


# ヒートマップ ------------------------------------------------------------------

# データフレームをlong型に変換
tw_count_long <- pivot_longer(
  tw_count2, 
  cols = -terms, # 変換しない列
  names_to = "screen_name", # 現列名を格納する列の名前
  values_to = "n" # 現セルを格納する列の名前
)

# ヒートマップを作図
ggplot(tw_count_long, aes(x = screen_name, y = terms, fill = n)) + 
  geom_tile() + # ヒートマップ
  scale_fill_gradient(low = "white", high = "#00A968") + # 塗りつぶし色のグラデーション
  scale_y_datetime(date_breaks = "1 day", 
                   date_labels = "%Y-%m-%d %H") + # y軸目盛(日時)
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # x軸目盛の角度
  labs(title = "アカウントごとのツイート数") # ラベル


# クラスタリング -----------------------------------------------------------------

# クラスタリング
res_dendrogram <- tw_count2[, -1] %>% # 非数値の列を落とす
  t() %>% # 転置
  dist() %>% # 距離(類似度)を測る
  hclust("ward.D2") %>% # クラスタリング
  dendro_data() # 作図用にデータを変換

# 描画
ggdendrogram(res_dendrogram, theme_dendro = FALSE)


# クラスタリング
res_dendrogram <- tw_count2[, -1] %>% # 非数値の列を落とす
  t() %>% # 転置
  dist() %>% # 距離(類似度)を測る
  hclust("ward.D2") %>% # クラスタリング
  as.dendrogram() # 作図用にデータを変換

# 描画
plot(res_dendrogram)


# try:ver ggplot() --------------------------------------------------------

# クラスタリング
res_dendrogram <- tw_count2[, -1] %>% # 非数値の列を落とす
  t() %>% # 転置
  dist() %>% # 距離(類似度)を測る
  hclust("ward.D2") %>% # クラスタリング
  dendro_data() # 作図用にデータを変換

# 作図：ggplot()
ggplot() + 
  geom_segment(data = segment(res_dendrogram), mapping = aes(x = x, y = y, xend = xend, yend = yend)) +          # 樹形図
  geom_text(data = label(res_dendrogram), 
            aes(x = x, y = y, label = label), hjust = 1, angle = 90) + # 各変数名
  ylim(c(-15, 50))


