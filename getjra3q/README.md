# getjra3q

DATADIR_JRA55にJRA-3Qデータを格納するディレクトリの絶対パスを記述

設定ファイルに以下の記述（bashの場合、$HOME/.bashrc）

export DATADIR_JRA3Q="JRA-3Qデータへの絶対パス"

# データの取得

./wget_jra3q.sh

# 取得したデータのチェック

./check.sh 2023 01 01 2023 01 31 slp

2023/01/01から2023/01/31までのslpのダウンロードをチェックする場合

# grib形式からgrads形式へ変換 

./grib2bin-jra_prs_day.sh 2023 01 01 2023 01 31

2023/01/01から2023/01/31まで変換する場合

${DATADIR_JRA3Q}/grib/Hist/Daily/以下に格納される

# 6時間毎データを日平均、月平均に変換

./exec_convert.sh

--var|-v 変数名（必須）

--nsyy/-ys 開始年 --nsmm|-ms 開始月

--neyy/-ye 終了年 --nemm|-me 終了月

-f : 既に作成したファイルを更新する場合

-d：日平均  -m：月平均（-dか-mのどちらかは必須）

表面データの場合には、-z 1 を指定


