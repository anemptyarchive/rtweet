

# 利用パッケージ
library(rtweet)
library(dplyr)
library(lubridate)
library(purrrlyr)

# ハッシュタグを指定
hashtag <- "FGO"


# ツイートを取得
tw_data_new <- search_tweets(paste0("#", hashtag), n = 18000, include_rts = FALSE)

# ツイートデータを保存
saveRDS(tw_data_new, file = paste0("tw_data/hashtag/", hashtag, "_", format(Sys.time(), "%Y%m%d"), ".rds"))
save(tw_data_new, file = paste0("tw_data/hashtag/", hashtag, "_", format(Sys.time(), "%Y%m%d"), ".Rdata"))

# 過去データと結合
tw_data <- rbind(tw_data, tw_data_new) %>% 
  dplyr::distinct(status_id, .keep_all = TRUE) %>% # 重複を削除
  dplyr::arrange(status_id)

# データを抽出
tw_data2 <- tw_data %>% 
  dplyr::select(status_id, created_at, text, hashtags, ext_media_url) %>% # 必要な列を抽出
  dplyr::mutate(
    created_at = created_at %>% 
      as.POSIXct(tz = "Etc/GMT") %>% 
      as.POSIXlt(tz = "Asia/Tokyo") %>% 
      as.POSIXct(tz = "Asia/Tokyo")
  ) %>% 
  dplyr::mutate(
    year = lubridate::year(created_at), 
    month = lubridate::month(created_at), 
    day = lubridate::day(created_at), 
    hour = lubridate::hour(created_at), 
    minute = lubridate::minute(created_at)
  ) %>% 
  dplyr::mutate(
    minute = case_when(
      minute >= 30 ~ 30, 
      minute < 30 ~ 0
    )
  )

# テキストを結合
tw_data3 <- tw_data2 %>% 
  dplyr::group_by(year, month, day, hour, minute) %>% 
  dplyr::summarise(text = paste(text, collapse = "\n"))

# 
df <- tw_data3 %>% 
  mutate(
    file_name = paste0("tmp_data/wordcloud_", year, month, day, "_", hour, minute, ".txt")
  ) %>% 
  mutate(
    res_mecab = purrr::map2(
      .x = text, .y = file_name, .f = write_and_docDF(.x, .y)
    )
  )
mutate_at(.vars = vars(text), .funs = funs(print))


write_and_docDF <- function(x, dir_name = "tmp_data", file_name = "tmp", pos = "名詞") {
  # ファイルパスを作成
  file_path <- paste0(dir_name, "/", file_name, ".txt")
  
  # 書き出し
  write(x = x, file = file_path)
  
  # 形態素解析
  df <- RMeCab::docDF(target = file_path, type = 1, pos = pos)
}
write_and_docDF <- function(x, y = "tmp_data/tmp.txt") {
  # 書き出し
  write(x = x, file = y)
  
  # 形態素解析
  df <- RMeCab::docDF(target = y, type = 1, pos = "名詞")
}
