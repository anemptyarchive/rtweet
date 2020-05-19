
# ネガポジ分析 -----------------------------------------------------

## 利用パッケージ
# ツイート収集:get_timeline()
library(rtweet)

# 形態素解析:docDF()
library(RMeCab)

# 時間データの処理:floor_date(), as_date()
library(lubridate)

# データフレーム操作
library(dplyr)
library(tidyr) # replace_na()

#文字列操作:str_remove_all
library(stringr)

# 作図
library(ggplot2)


# ツイート収集 ------------------------------------------------------------------

# アカウントの指定
screen_name <- "anemptyarchive"
# 検索ワードの指定
#search_word <- "トピックモデル"

# ツイートの収集
tw_data <- get_timeline(screen_name, n = 10000, include_rts = FALSE) # アカウント指定で収集
#tw_data <- search_tweets(search_word, n = 30000, include_rts = FALSE) # 単語指定で収集
#tw_data <- readRDS("tw_data/tw_data_anemptyarchive.rds") # 保存済みのデータを読み込む

# ツイート日時の抽出とPOSIXlt型・タイムゾーンの変換
tw_time <- tw_data[["created_at"]] %>% 
  as.POSIXct(tz = "Etc/GMT") %>% 
  as.POSIXlt(tz = "Japan")

# ツイートテキストの抽出と文字列処理
tw_text <- tw_data[["text"]] %>% 
  str_remove_all("^@.*?\\s") %>% # リプライ先の除去
  str_remove_all(("https?://[\\w/:%#\\$&\\?\\(\\)~\\.=\\+\\-]+")) %>% # urlの除去
  str_remove_all("[\U0001F004-\U000207BF]") # 絵文字の除去
tw_text[1:10]

# データハンドリング ------------------------------------------------------------

# 単位(期間)を指定
term <- "mon"
term <- "day"

# 期間ごとにツイートテキストをまとめる
text_df <- data.frame(
  terms = as_date(floor_date(tw_time, term)), # 指定した期間で丸める
  texts = tw_text
) %>% 
  group_by(terms) %>% # 期間ごとにグループ化
  summarise(texts = paste(texts, collapse = "\n")) # 同一期間のテキストを結合


# 感情分析 --------------------------------------------------------------------

# 単語感情極性対応表の取得
#np_dic_original <- read.table(
#  "http://www.lr.pi.titech.ac.jp/~takamura/pubs/pn_ja.dic", 
#  sep = ":", stringsAsFactors = FALSE
#)
np_dic_original <- read.csv("dic_data/pn_ja_dic.csv")
head(np_dic_original)

# ネガポジ辞書の作成
np_dic <- np_dic_original %>% 
  select(TERM = V1, POS1 = V3, allocation = V4) %>% # docDF仕様の列名に変更
  distinct(TERM, .keep_all = TRUE) # 重複の除去
head(np_dic)


## ネガポジ分析
# 一時テキストファイルの保存先を指定
folder_name <- "tmp_data"

# 分析結果の受け皿を初期化
score_df <- data.frame()
for(i in 1:nrow(text_df)) {
  
  # 一時ファイルパスを作成
  tmp_file_name <- paste(screen_name, "_", text_df[["terms"]][i], ".txt", sep = "")
  tmp_path <- paste(folder_name, tmp_file_name, sep = "/")
  
  # テキストファイルを書き出し
  write(text_df[["texts"]][i], tmp_path)
  
  # MeCabによる形態素解析
  mecab_df <- docDF(tmp_path, type = 1, pos = c("動詞", "形容詞", "副詞", "助動詞"))
  
  if(!is.null(mecab_df)) { ## (NULLでないときのみ)
    
    # ネガポジ配点を結合
    tmp_score_df <- mecab_df %>% 
      left_join(np_dic, by = c("TERM", "POS1")) %>% # 各単語に配点を付与
      mutate(allocation = replace_na(allocation, 0)) %>% # NAを0に置換
      select(TERM, FREQ = tmp_file_name, allocation) # docDF仕様の列名に変更
    
    # ネガポジスコアを計算
    tmp_score_df <- tmp_score_df %>% 
      mutate(terms = text_df[["terms"]][i]) %>% # 日付情報列の追加
      mutate(allocation = replace_na(allocation, 0)) %>% # 配点がNAの場合0に置換
      mutate(score = allocation * FREQ) %>% # (スコア) = (配点) * (語数)
      mutate(
        np_label = case_when(
          score < 0 ~ "negative", # (スコア) < 0ならネガ
          score > 0 ~ "positive", # (スコア) > 0ならポジ
          score == 0 ~ "neutral"  # (スコア) = 0ならニュート
        )
      ) # ネガポジラベルを付与
    
    # 全期間の結果を結合
    score_df <- rbind(score_df, tmp_score_df)
  }
}

# 期間ごとにネガスコア・ポジスコアの合計
result_df <- score_df %>% 
  select(terms, np_label, score, FREQ) %>% 
  group_by(terms, np_label) %>% 
  summarise(score = sum(score), FREQ = sum(FREQ)) # スコアと頻度を合算


# 可視化 ---------------------------------------------------------------------

# ネガポジ推移
ggplot(result_df, aes(x = terms, y = score)) + 
  geom_bar(mapping = aes(fill = np_label), stat = "identity") + # 棒グラフ
  scale_fill_manual(values = c("#00A968", "yellow", "orange")) + # 塗りつぶし色
  geom_line(stat = "summary", fun = "sum", color = "blue") + # 折れ線グラフ
  scale_x_date(date_breaks = "2 weeks") + # x軸目盛
  theme(axis.text.x = element_text(angle = 90)) + # x軸目盛の傾き
  labs(title = paste("@", screen_name, "のネガポジ推移", sep = ""), 
       x = term) # ラベル

# ネガポジ割合の推移
ggplot(result_df, aes(x = terms, y = FREQ, fill = np_label)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_manual(values = c("#00A968", "yellow", "orange")) + 
  scale_x_date(date_breaks = "2 weeks") + # x軸目盛
  theme(axis.text.x = element_text(angle = 90)) + # x軸目盛の傾き
  labs(title = paste("@", screen_name, "のネガポジ割合(語数)の推移", sep = ""), 
       x = term) # ラベル


