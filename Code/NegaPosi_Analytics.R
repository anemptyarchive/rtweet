
# ネガポジ分析 -----------------------------------------------------

## 利用パッケージ
# ツイート収集
library(rtweet)    # get_timeline()
# 時間データの処理
library(lubridate) # floor_date()
# 形態素解析
library(RMeCab)    # docDF()
# データフレーム操作
library(dplyr)
#文字列操作
library(stringr)
# 作図
library(ggplot2)



# ツイート収集 ------------------------------------------------------------------

# アカウントの指定
screen_name <- "anemptyarchive"
# 検索ワードの指定
search_word <- "トピックモデル"

# ツイートの収集
tw_data <- get_timeline(screen_name, n = 10000, include_rts = FALSE)  # アカウント指定で収集
#tw_data <- search_tweets(search_word, n = 30000, include_rts = FALSE) # 単語指定で収集


# ツイート日時の抽出
tw_time_utc <- tw_data$created_at

# タイムゾーンの変換
tw_time_jst <- as.POSIXlt(as.POSIXct(tw_time_utc, tz = "Etc/GMT"), tz = "Japan")

# ツイートテキストの抽出と文字列処理
tw_text <- tw_data$text %>% 
           str_remove_all("^@.*?\\s") %>%            # リプライ先の除去
           str_remove_all("[\U0001F004-\U000207BF]") # 絵文字の除去


# 任意の期間を指定して抽出 ------------------------------------------------------------

# セルの単位(期間)の指定
term <- "mon"
term <- "day"

# 期間ごとにツイートテキストをまとめる
text_df1 <- data.frame(terms = as.Date(floor_date(tw_time_jst, term)), 
                       texts = tw_text) %>% 
            group_by(terms) %>%                              # 期間ごとにグループ化
            summarise(texts = paste(texts, collapse = "\n")) # グループごとにテキストを結合

# 最古日時から現在までの期間の受け皿を用意する
term_vec <- seq(floor_date(tw_time_jst[length(tw_time_jst)], term),  # 最古(最小値)
                floor_date(Sys.time(), term),                        # 現在(最大値)
                by = term)                                           # 間隔

# 受け皿となるデータフレームの作成
term_df <- data.frame(terms = as.Date(1, term_vec))

# 全期間のツイートテキスト一覧が完成
text_df2 <- left_join(term_df, text_df1, by = "terms")
text_df2$texts[is.na(text_df2$texts)] <- ""   # NAを空欄に置換



# 感情分析 --------------------------------------------------------------------

# 単語感情極性対応表の取得
np_dic0 <- read.table("http://www.lr.pi.titech.ac.jp/~takamura/pubs/pn_ja.dic", 
                      sep = ":", stringsAsFactors = FALSE)
head(np_dic0)

# ネガポジ辞書の作成
np_dic <- np_dic0 %>% 
          select(TERM = V1, POS1 = V3, SCORE = V4) %>%  # 列の選択
          distinct(TERM, .keep_all = TRUE)              # 重複の除去
head(np_dic)



# ネガポジ分析
score_df <- data.frame() # 格納先の用意
for(i in 1:nrow(text_df1)) {
  # 一時ファイルパスの用意
  tmp_folder_name <- "tmp_data"
  tmp_file_name <- paste(screen_name, text_df1$terms[i], ".txt", sep = "")
  tmp_path <- paste(tmp_folder_name, tmp_file_name, sep = "/")
  
  # テキストファイルの書き出し
  write(text_df1$texts[i], tmp_path)
  
  # 形態素解析
  mecab_df <- docDF(tmp_path, type = 1, 
                    pos = c("動詞", "形容詞", "副詞", "助動詞"))
  
  if(is.null(mecab_df) != TRUE) {
    # ネガポジスコアの計測
    tmp_score_df <- left_join(mecab_df, np_dic, by = c("TERM", "POS1")) %>%  # 各単語にスコアを付与
                    select(TERM, FREQ = tmp_file_name, SCORE)
    tmp_score_df$SCORE[is.na(tmp_score_df$SCORE)] <- 0         # スコアがNAなら0にする
    tmp_score_df$FREQ_SCORE <- tmp_score_df$FREQ * tmp_score_df$SCORE # (語数) * (頻度)
    
    # 日付情報列の追加
    tmp_score_df$terms <- text_df1$terms[i]
    
    # ネガポジカテゴリ列の追加
    tmp_score_df$negaposi[tmp_score_df$FREQ_SCORE > 0]  <- "positive"  # スコア>0なら"posi"にする
    tmp_score_df$negaposi[tmp_score_df$FREQ_SCORE < 0]  <- "negative"  # スコア<0なら"nega"にする
    tmp_score_df$negaposi[tmp_score_df$FREQ_SCORE == 0] <- "neutral"  # スコア=0なら"neut"にする
    
    # 全期間の結果を結合
    score_df <- rbind(score_df, tmp_score_df)
  }
}


# 期間ごとにネガスコア・ポジスコアの合算
result_df1 <- score_df %>% 
  select(terms, negaposi, SCORE) %>% 
  group_by(terms, negaposi) %>% 
  summarise(SCORE = sum(SCORE))

# 全期間のスコア一覧の完成
result_df2 <- left_join(term_df, result_df1, by = "terms")
result_df2$negaposi[is.na(result_df2$negaposi)] <- "neutral"
result_df2$SCORE[is.na(result_df2$SCORE)] <- 0

# プロット
ggplot(result_df2, mapping = aes(x = terms, y = SCORE)) + 
  geom_bar(mapping = aes(fill = negaposi), stat = "identity") +  # 棒グラフの
  scale_fill_manual(values = c("#00A968", "yellow", "orange")) + # 塗りつぶし色
  geom_line(stat = "summary", fun.y = sum, color = "blue") +     # 折れ線グラフ
  scale_x_date(breaks = as.Date(term_vec), labels = as.Date(term_vec)) +  # x軸目盛
  theme(axis.text.x = element_text(angle = 90)) +                # x軸目盛の傾き
  labs(title = paste("「@", screen_name, "」のネガポジ推移", sep = ""), 
       x = term, y = "SCORE") # タイトル


# ネガ・ポジ語の割合の確認
df <- score_df %>% 
  select(terms, negaposi, FREQ) %>% 
  group_by(terms, negaposi) %>% 
  summarise(FREQ = sum(FREQ))



# プロット
ggplot(df, mapping = aes(x = terms, y = FREQ, fill = negaposi)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_manual(values = c("#00A968", "yellow", "orange")) + 
  scale_x_date(breaks = as.Date(term_vec), labels = as.Date(term_vec)) +  # x軸目盛
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "カテゴリ別語数の確認", x = "", y = "FREQ")

class(result_df2$terms)


# テキストデータの一時書き出し
tmp_path <- "tmp_data/tmp_text.txt"   # ファイルのパスの指定   # tempfile()
write(text_df2$texts[1], tmp_path)    # 書き出し

# 形態素解析
mecab_df <- docDF(tmp_path, type = 1, pos = c("動詞", "形容詞", "名詞", "副詞", "助動詞"))

np_df <- left_join(mecab_df, np_dic, by = c("TERM", "POS1")) %>% 
  select(TERM, FREQ = tmp_path, SCORE)

# データフレームの整形
if(term == "day") {
  # day
  text_df2$terms1 <- text_df2$terms %>% 
    str_remove_all("-\\d{2}$")
  text_df2$terms2 <- text_df2$terms %>% 
    str_remove_all("^\\d{4}-\\d{2}-")
} else if(term == "mon") {
  # mon
  text_df2$terms1 <- text_df2$terms %>% 
    str_remove_all("-\\d{2}-\\d{2}$")
  text_df2$terms2 <- text_df2$terms %>% 
    str_remove_all("^\\d{4}-|-\\d{2}$")
}

text_df3 <- text_df2 %>% 
  select(terms1, terms2, texts)



