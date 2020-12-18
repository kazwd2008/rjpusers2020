setwd(“C:/test”)      # ファイルを収録するディレクトリを指定する
#RCurlで読み込み、jsonliteパッケージを使ってjson形式のデータを処理する
require(RCurl)
require(jsonlite)
yourAPPID <- “*******************“  # e-StatのアプリケーションIDをセットする

#--------------------------------------------------------------------
#  最初のスクレイピング
#--------------------------------------------------------------------
# 変数urlにAPIのコマンドを作成する
url <- paste("http://api.e-stat.go.jp/rest/3.0/app/json/getStatsData?appId=",
       yourAPPID,                            # 上でセットした各人のアプリケーションID
       "&lang=J",
       "&statsDataId=0003125169",	      # 取得する表のID(半角)
       "&metaGetFlg=Y",			
       "&cntGetFlg=N",
       "&sectionHeaderFlg=1", sep="")
dt0 <- getURL(url)		              # ちょっと時間がかかる
dt1 <- fromJSON(dt0)

# 取得データの確認
str(dt1, max.level=3)                                  # 出力1
str(dt1$GET_STATS_DATA$STATISTICAL_DATA$RESULT_INF)    # 出力2
str(dt1$GET_STATS_DATA$STATISTICAL_DATA$DATA_INF$VALUE)       # 出力3
str(dt1$GET_STATS_DATA$STATISTICAL_DATA$CLASS_INF$CLASS_OBJ$CLASS) # 出力4

#--------------------------------------------------------------------
#  010表データの残りを全て取得する
#--------------------------------------------------------------------
# 全体のデータ数をTotal、続きの取得の開始ポイントをNKeyにセットして取得を繰り返す
(Total <- dt1$GET_STATS_DATA$STATISTICAL_DATA$RESULT_INF$TOTAL_NUMBER)
(NKey  <- dt1$GET_STATS_DATA$STATISTICAL_DATA$RESULT_INF$NEXT_KEY)
rp1 <- ceiling(Total / (NKey-1))	# 必要な読込回数を計算
nb1 <- 1:rp1
nb2 <- formatC(nb1, width=2, flag="0")     # 2桁でゼロ捕捉
(dname <- apply(cbind("dd", nb2), 1, paste, collapse=""))
# [1] "dd01" "dd02" "dd03" "dd04" "dd05" "dd06"  # データを収録する変数名をdnameに生成
# 一回目に読み込んだ統計数値部分を最初の領域に保存
assign(dname[1], dt1$GET_STATS_DATA$STATISTICAL_DATA$DATA_INF$VALUE)
for (i in 2:rp1){
    url <- paste("http://api.e-stat.go.jp/rest/3.0/app/json/getStatsData?appId=",
           yourAPPID,                     # アプリケーションID
           	"&lang=J",
			"&statsDataId=0003125169",	  # 取得する表のID(半角)
			"&startPosition=", NKey,      # データ取得の開始点
			"&metaGetFlg=Y",			  
			"&cntGetFlg=N",
			"&sectionHeaderFlg=1", sep="")
	dt0 <- getURL(url)              	  # 同じ領域を再利用
	dt1 <- fromJSON(dt0)
	# 利用する部分だけ確保した領域に保存する
	assign(dname[i], dt1$GET_STATS_DATA$STATISTICAL_DATA$DATA_INF$VALUE)
	# データ取得の開始ポイントを更新
	NKey  <- dt1$GET_STATS_DATA$STATISTICAL_DATA$RESULT_INF$NEXT_KEY
}    # この処理には数分かかる

#--------------------------------------------------------------------
#  取得データの確認と絞り込み
#--------------------------------------------------------------------
# 出力3参照。取得データの全ての属性を確認する。
dt1$GET_STATS_DATA$STATISTICAL_DATA$CLASS_INF$CLASS_OBJ$CLASS[1]
dt1$GET_STATS_DATA$STATISTICAL_DATA$CLASS_INF$CLASS_OBJ$CLASS[2] # 品目コード
dt1$GET_STATS_DATA$STATISTICAL_DATA$CLASS_INF$CLASS_OBJ$CLASS[3] # 世帯類型
dt1$GET_STATS_DATA$STATISTICAL_DATA$CLASS_INF$CLASS_OBJ$CLASS[4] # 地域コード
dt1$GET_STATS_DATA$STATISTICAL_DATA$CLASS_INF$CLASS_OBJ$CLASS[5] # 年次コード

# 変数名が長すぎるので使うものだけ名前を付け直す
itm <- as.data.frame(dt1$GET_STATS_DATA$STATISTICAL_DATA$CLASS_INF$CLASS_OBJ$CLASS[2])  
cty <- as.data.frame(dt1$GET_STATS_DATA$STATISTICAL_DATA$CLASS_INF$CLASS_OBJ$CLASS[4])
per <- as.data.frame(dt1$GET_STATS_DATA$STATISTICAL_DATA$CLASS_INF$CLASS_OBJ$CLASS[5])  

# 品目の選択: 内訳を持たない末端の品目（必ずしもレベル5ではない）を特定する
f.lv <- rep(0, length(itm$X.level))   # 末端フラグ
for (i in 1:(length(itm$X.level)-1)) if (itm$X.level[i] >= itm$X.level[i+1]) f.lv[i] <- 1   
# 次の品目が同じレベルなら末端品目と判定する
length(which(f.lv==1))			      # [1] 564
cbind(itm$X.name, itm$X.leve, f.lv)   # 確認表を表示

# 抽出条件セット  -------------------------------------------------------
s.setai <- "03"                       # 二人以上の世帯（2000年～）
s.area  <- cty[2:48,]                 # 都道府県庁所在市のみ
s.itm   <- itm[which(f.lv==1 & itm$X.code > "010000000" & itm$X.code < "020000000"),]
       dim(s.itm)                     # [1] 213   5
s.tm    <- per[31:35,]

# 表作成 ----------------------------------------------------------------
tb0 <- array(NA, c(length(s.area$X.code), length(s.itm$X.code), length(s.tm$X.code)))

# 市名の頭にある地域コードを除去
dimnames(tb0)[[1]] <- substr(s.area$X.name, 6, length(s.area$X.name)) 
# 品目名の頭にある品目番号を除去
require(stringi)    # 文字列操作
bk.pos <- stri_locate_first_regex(s.itm$X.name, " ")   # 探索文字列の開始位置と終了位置が戻る
dimnames(tb0)[[2]] <- substr(s.itm$X.name, bk.pos[,1], length(s.itm$X.name))  
dimnames(tb0)[[3]] <- s.tm$X.name
dim(tb0)		# [1]  47都道府県庁所在市   213 品目    5 時点（年）

# 抽出条件に合う統計数値のみtb0に収録する
f.err <- matrix(NA, nr=rp1, nc=3)  # 確認用エラーフラグ
for (i in 1:rp1){
    dat0 <- get(dname[i])
    colnames(dat0) <- c("tab", "cat01", "cat02", "area", "time", "unit", "val")
    # 元の変数名は＠がつくが、Rの変数名は記号から始まってはいけない
	dat1 <- subset(dat0, (cat01 %in% s.itm$X.code) & (cat02 == s.setai) 
	                      & (area %in% s.area$X.code) & (time %in% s.tm$X.code))

    c1 <- match(dat1$area,  s.area$X.code)
	c2 <- match(dat1$cat01, s.itm$X.code)
	c3 <- match(dat1$time,  s.tm$X.code)
    f.err[i,1] <- length(which(is.na(c1)))
    f.err[i,2] <- length(which(is.na(c2)))
    f.err[i,3] <- length(which(is.na(c3)))

    # tb0[c1,c2,c3] <- as.numeric(dat1$val) とすると時間がかかる
    for (j in 1:dim(dat1)[1]){
		tb0[c1[j],c2[j],c3[j]] <- as.numeric(dat1$val[j])
	}  
}
f.err		# 全て0であることを確認
apply(apply(tb0, c(1,2), is.na), 1, sum)  # 年次別の欠測数チェック
# 2015年 2016年 2017年 2018年 2019年 
#     0      0      0      0      0 

#--------------------------------------------------------------------
# 5年分の平均/中央値で都道府県庁所在市をクラスタリング 
#--------------------------------------------------------------------

# 5年分のデータを品目・市別に平均値と中央値を取る
Mn5 <- apply(tb0, c(1,2), mean)		# 平均値
Md5 <- apply(tb0, c(1,2), median)		# 中央値

# 品目により金額の差異があるので標準化する
    mmn5 <- apply(Mn5,2,mean)    # 品目別の平均
    ssd5 <- apply(Mn5,2,sd)
    mmed5 <- apply(Md5,2,median)
	mmad5 <- apply(Md5,2,mad)    # Rのmad関数は1.4826を掛けて基準化する

Mn5.s <- Md5.s <- matrix(NA, nr=dim(Mn5)[1], nc=dim(Mn5)[2])
colnames(Mn5.s) <- colnames(Md5.s) <- colnames(Mn5)
rownames(Mn5.s) <- rownames(Md5.s) <- rownames(Mn5)

for (j in 1:47){    # 標準化
    Mn5.s[j,] <- (Mn5[j,] - mmn5)  / ssd5
    Md5.s[j,] <- (Md5[j,] - mmed5) / mmad5
}

# 外れ値チェック  [213品目] 
par(mfrow=c(3,2), mar=c(12,3,3,2))	# defaultはpar(mar=c(5,4,4,2)) 
  boxplot(Mn5.s[,1:71],    las=2, cex=0.3, main="平均値(1)")
  boxplot(Md5.s[,1:71],    las=2, cex=0.3, main="中央値(1)")
  boxplot(Mn5.s[,72:142],  las=2, cex=0.3, main="平均値(2)")
  boxplot(Md5.s[,72:142],  las=2, cex=0.3, main="中央値(2)")
  boxplot(Mn5.s[,143:213], las=2, cex=0.3, main="平均値(3)")
  boxplot(Md5.s[,143:213], las=2, cex=0.3, main="中央値(3)")

# 距離行列作成
Mn5.d <- dist(Mn5.s)
Md5.d <- dist(Md5.s)

# Ward法でクラスタリング
Mn5.hc <- hclust(Mn5.d, method="ward.D")
Md5.hc <- hclust(Md5.d, method="ward.D")

# クラスター数を決める
(Mn5.hc.c12 <- cutree(Mn5.hc, k=12))
(Md5.hc.c12 <- cutree(Md5.hc, k=12))

# デンドログラム作成　12分割
dev.new(width=20, height=14)
plot(Mn5.hc)
rect.hclust(Mn5.hc, k=12)

dev.new(width=20, height=14)
plot(Md5.hc)
rect.hclust(Md5.hc, k=12)

# 地図作成
require("NipponMap")

set.seed(104)		# パレット固定
pl <- sample(rainbow(15, alpha=0.5), 15)  # 似た色が近くにならないように

par(mfrow=c(1,2), mar=c(0,0,2,0)) 
  mp0 <- JapanPrefMap(col=pl[Mn5.hc.c12], border=gray(.8), main="平均値")
   text(mp0, labels=rownames(tb0), cex=0.6, col=gray(.3))
  JapanPrefMap(col=pl[Md5.hc.c12], border=gray(.8), main="中央値")
   text(mp0, labels=rownames(tb0), cex=0.6, col=gray(.3))

# 比較
cbind(Mn5.hc.c12, Md5.hc.c12)

save.image("tb0.rdata")

#  分類に影響が大きい品目を探す  --------------------------------------------
require(rpart)
TMn12 <- rpart(Mn5.hc.c12~Mn5, method="class", control = rpart.control(cp = 0.0001))
TMd12 <- rpart(Md5.hc.c12~Md5, method="class", control = rpart.control(cp = 0.0001))

# デンドログラム
par(mfrow=c(1,2))
plot(TMn12); text(TMn12)
plot(TMd12); text(TMd12)

s.itm$X.name # 終端品目のコード表

# 平均値による分類に影響が大きい品目
#[35] " 塩さけ"
#[172] " しゅうまい" 
#[22] " たい" 

# 中央値による分類に影響が大きい品目
#[167] " うなぎのかば焼き"
#[135] " ソース"  
#[51] " 鶏肉"   

# 該当品目と閾値
dev.new(width=24, height=16)
par(mfrow=c(3,2), mar=c(8,4,4,0))	# par(mar=c(5,4,4,2))
barplot(Mn5[,35], las=2, col=pl[Mn5.hc.c12], main=colnames(Mn5)[35])
 abline(h=1251, col="gray")
barplot(Md5[,167], las=2, col=pl[Md5.hc.c12], main=colnames(Md5)[167])
 abline(h=2428, col="gray", lty=3)
barplot(Mn5[,172], las=2, col=pl[Mn5.hc.c12], main=colnames(Mn5)[172])
 abline(h=694, col="gray")
 abline(h=1076, col="gray")
barplot(Md5[,135], las=2, col=pl[Mn5.hc.c12], main=colnames(Md5)[46])
 abline(h=792, col="gray", lty=3)
barplot(Mn5[,22], las=2, col=pl[Mn5.hc.c12], main=colnames(Mn5)[22])
 abline(h=598, col="gray")
barplot(Md5[,51], las=2, col=pl[Mn5.hc.c12], main=colnames(Md5)[51])
 abline(h=16900, col="gray")
 
#--------------------------------------------------------------------
# 東西日本を分ける食料品目 
#--------------------------------------------------------------------
Y1 <- c("2015", "2016", "2017", "2018", "2019")

# 東日本(1)、西日本(2)、除外する中部地方(3)の地域区分を作る
rg1 <- c(rep(1,14), rep(3,9), rep(2,24))
# 47市の冒頭14市が東、次の9市が中部、残りの24市が西

dx0 <- tb0[,,5]    # 2019年
ex1 <- -(15:23)    # 分析から除外する中部地方の選択用
Tx1 <- rpart(rg1[ex1] ~ dx0[ex1,], method="class", control = rpart.control(cp = 0.0001))

dev.new(width=8, height=8)  # デンドログラム
plot(Tx1)
text(Tx1)

# 残りの四年分も
Tree1 <- list("list", 5)	# 要素数5の空のリスト
Tree1[[5]] <- Tx1
for (i in 1:4)  Tree1[[i]] <- rpart(rg1[ex1]~tb0[ex1,,i], method="class", control = rpart.control(cp = 0.0001))
Tree1

# dev.new(width=16, height=20)  
# par(mfrow=c(3,2), mar=c(5,4,8,2))
# for (i in 1:5){ 
#    plot(Tree1[[i]], main=paste("東西", Y1[i]), cex=1.5)
#    text(Tree1[[i]], cex=1.5)
# }    

dev.new(width=16, height=10)	# [45,] "213 魚介の漬物"
par(mar=c(8,4,4,0))	# par(mar=c(5,4,4,2))
barplot(tb0[,45,5], las=2, col=rg1, main=colnames(dx0)[45])
abline(h=2923, col="gray")

dev.new(width=16, height=10)	# [102] 納豆
par(mar=c(8,4,4,0))	# par(mar=c(5,4,4,2))
barplot(tb0[,102,5], las=2, col=rg1, main=colnames(dx0)[102])
abline(h=3886, col="gray")

dev.new(width=16, height=10)	# [52] 合いびき肉"
par(mar=c(8,4,4,0))	# par(mar=c(5,4,4,2))
barplot(tb0[,52,5], las=2, col=rg1, main=colnames(dx0)[52])
abline(h=2923, col="gray")







