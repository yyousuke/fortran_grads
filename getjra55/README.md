# getjra55

DATADIR_JRA55にJRA-55データを格納するディレクトリの絶対パスを記述

設定ファイルに以下の記述（bashの場合、$HOME/.bashrc）

export DATADIR_JRA55="JRA-55データへの絶対パス"

# データの取得

./wget_jra55.sh


# grib形式からgrads形式へ変換 

./grib2bin-jra_prs_day.sh 

${DATADIR_JRA55}/grib/Hist/Daily/以下に格納される

# 6時間毎データを日平均、月平均に変換

./exec_convert.sh

--var|-v 変数名（必須）

--nsyy/-ys 開始年 --nsmm|-ms 開始月

--neyy/-ye 終了年 --nemm|-me 終了月

-f : 既に作成したファイルを更新する場合

-d：日平均  -m：月平均（-dか-mのどちらかは必須）

表面データの場合には、-z 1 を指定


