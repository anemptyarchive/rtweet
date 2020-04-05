
# ツイート頻度でクラスタリング---------------------------------------------------------------

## 利用パッケージ
# ツイート収集
library(rtweet)    # get_timeline()
# データフレーム操作
library(dplyr)
library(tidyr)     # gather()
# 文字列操作
library(stringr)   # str_remove_all()
# 時間データ操作
library(lubridate) # floor_date()
# 作図
library(ggplot2)
library(ggdendro)  # ggdendrogram()


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
    count(terms) # ツイート数をカウント
  
  # 集計結果を結合
  tw_count <- left_join(tw_count, tmp_tw_count, by = "terms")
  
  # おまじない
  Sys.sleep(1)
}

## データフレームを整形
# 列名を付ける
colnames(tw_count) <- c("time", paste0("@", screen_names))

# NAを0に置換
tw_count[is.na.data.frame(tw_count)] <- 0

# データフレームをlong型に変換
tw_count_long <- pivot_longer(
  tw_count, 
  cols = -time, 
  names_to = "screen_name", 
  values_to = "n"
)

# ヒートマップを作図
ggplot(tw_count_long, aes(x = screen_name, y = time, fill = n)) +  # データ
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "#00A968") +  # 塗りつぶし色のグラデーション
  scale_y_datetime(date_breaks = "1 day", date_labels = "%Y-%m-%d %H") +  # y軸目盛
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # x軸目盛の角度
  labs(title = "アカウントごとのツイート数")


# クラスタリング -----------------------------------------------------------------


# データフレームを整形
cluster <- tw_count[, -1] # 非数値の列を落とす

# 類似度を測る
den <- cluster %>% 
  t() %>%     # 転置
  dist() %>%  # 
  hclust("ward.D2") %>% # 
  dendro_data()         # 作図用にデータを変換

# 作図
ggdendrogram(den, theme_dendro = FALSE)


# 類似度を測る
den <- cluster %>% 
  t() %>% 
  dist() %>% 
  hclust("ward.D2") %>% 
  as.dendrogram()

# 描画
plot(den)




# try ---------------------------------------------------------------------

# 期間を指定
term_from <- as.POSIXct("2019/08/21", tz = "Japan")
term_to   <- as.POSIXct("2019/08/27", tz = "Japan")
term_vec <- seq(term_from, term_to, by = "hour")


# 追加パッケージ
library(scales) # date_breaks(), date_format()

# 作図
ggplot(data = count_df_long, mapping = aes(x = account, y = time, fill = n)) +  # データ
  geom_tile() + 
  scale_fill_gradient(low = "#FFFFFF", high = "#00A968") +    # 塗りつぶし色のグラデーション
  scale_y_datetime(breaks = date_breaks("3 hour"),            # y軸目盛：表示位置
                   labels = date_format("%Y-%m-%d %H:%M", tz = "Japan")) +  # y軸目盛：表示テキスト
  theme(axis.text.x = element_text(angle = 90))               # x軸目盛の角度

apply(count_df[, -1], 2, sum)
class(count_df_long$time)


# 利用パッケージ
library(ggdendro)

# 類似度を測る
den <- cluster %>% 
       t() %>%     # 転置
       dist() %>%  # 
       hclust("ward.D2") %>% # 
       dendro_data()         # 作図用にデータを変換
den2 <- dendro_data(den)

# 作図
ggdendrogram(den, theme_dendro = FALSE)


# 作図：ggplot()
ggplot() + 
  geom_segment(data = segment(den), 
               aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_text(data = label(den), 
            aes(x = x, y = y, label = label), hjust = 1, angle = 90) + 
  ylim(c(-10, 50))



# ツイートを保存しておく -------------------------------------------------------------

for(i in seq_along(screen_name)) {
  
  # ツイートを収集
  tw_data <- get_timeline(screen_name[i], n = 10000, include_rts = FALSE)
  
  # 保存
  saveRDS(tw_data, paste0("tw_data_Jleague/", screen_name[i], ".obj"))
  
  # おまじない
  Sys.sleep(1)
}
