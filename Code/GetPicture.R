
# ツイート画像の取得 ----------------------------------------------------------------------

# 利用パッケージ
library(rtweet) # ツイートの取得:search_tweets()
library(magick) # 画像の取得:image_read(), image_write()
library(dplyr) # データ操作
library(tidyr) # unnest()


# ツイートの取得 ------------------------------------------------------------------

# 検索ワードを指定してツイートを取得
tw_data <- search_tweets("#鞘師里保誕生祭", n = 1000, include_rts = FALSE)
#saveRDS(tw_data, file = "tw_data/tw_data_rihoriho.rds")

# 画像のurlを抽出
tw_pic_url <- tw_data %>% 
  unnest(ext_media_url) %>% # 画像urlのネストを解除
  filter(!is.na(ext_media_url)) %>%  # 画像を含むもののみ抽出
  select(created_at, screen_name, text, ext_media_url)
nrow(tw_pic_url)


# 画像の取得 -------------------------------------------------------------------

# 画像を取得
for(i in 1:nrow(tw_pic_url)) {
  
  # urlを取り出す
  pic_url <- tw_pic_url[["ext_media_url"]][i]
  
  # 画像を取得
  pic_data <- image_read(pic_url)
  
  # 画像を保存
  image_write(pic_data, path = paste0("pic_data/rihoriho/rihoriho_", i, ".png"), format = "png")
  
  # おまじない
  Sys.sleep(1)
}


