
# プログレスバーの表示関数 --------------------------------------------------------------

### シャープver -----

# プログレスバーを表示する関数を作成
show_bar <- function(i, N) {
  # シンボルの数を指定
  max_bar <- 50
  
  # 進行率に応じたシンボル数を計算
  prg_n <- round(i / N * max_bar)
  
  # 進行率に応じてシンボルを作成
  prg_symbol <- paste0(rep("#", prg_n), collapse = "")
  
  # 残りのシンボルを作成
  add_symbol <- paste0(rep("-", max_bar - prg_n), collapse = "")
  
  # シンボルを結合してバーを作成
  prg_bar <- paste0("|", prg_symbol, add_symbol, "|")
  
  # 進行率のテキストを作成
  prg_text <- paste0(i, "/", N, " (", round(i / N * 100, 2), "%)")
  
  # プログレスバーを表示
  message("\r", rep(" ", max_bar * 2), appendLF = FALSE) # メッセージを初期化(前のテキストの方が長いと残ってしまうため)
  message("\r", prg_bar, " ", prg_text, appendLF = FALSE)
}


# お試し
N <- 231
for(i in 1:N) {
  #show_bar(i, N)
  Sys.sleep(0.1)
}


### 矢印ver -----

# プログレスバーを表示する関数を作成
show_bar <- function(i, N) {
  # シンボルの数を指定
  max_bar <- 50
  
  # 進行率に応じたシンボル数を計算
  prg_n <- round(i / N * max_bar)
  
  # 進行率に応じてシンボルを作成
  tmp_symbol <- rep("=", prg_n) # シンボルを作成
  if(i < N) tmp_symbol[prg_n] <- ">" # (0%と)100%未満の場合は先頭を矢印に置換
  prg_symbol <- paste0(tmp_symbol, collapse = "") # 結合
  
  # 残りのシンボルを作成
  add_symbol <- paste0(rep("-", max_bar - prg_n), collapse = "")
  
  # シンボルを結合してバーを作成
  prg_bar <- paste0("|", prg_symbol, add_symbol, "|")
  
  # 進行率のテキストを作成
  prg_text <- paste0(i, "/", N, " (", round(i / N * 100, 2), "%)")
  
  # プログレスバーを表示
  message("\r", rep(" ", max_bar * 2), appendLF = FALSE) # メッセージを初期化(前のテキストの方が長いと残ってしまうため)
  message("\r", prg_bar, " ", prg_text, appendLF = FALSE)
}


# お試し
N <- 1000
for(i in 980:N) {
  show_bar(i, N)
  Sys.sleep(0.5)
}


