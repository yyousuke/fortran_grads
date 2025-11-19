# getjra3qdias

DIASからJRA-3Qデータを取得する（[doi:10.20783/DIAS.645](https://doi.org/10.20783/DIAS.645)）

## 準備

- DIASの設定ファイル（$HOME/.netrc に次を記載、ファイルのパーミッションは600）

<pre>machine data.diasjp.net<br />login DIAS_account_name<br />password password_of_DIAS</pre>

- DATADIR_JRA55にJRA-3Qデータを格納するディレクトリの絶対パスを記述

設定ファイルに以下の記述（bashの場合、$HOME/.bashrc）

<pre>export DATADIR_JRA3Q="JRA-3Qデータへの絶対パス"<br />export WGRIB2="wgrib2への絶対パス"</pre>

- 実行環境の準備

<pre>mkdir $DATADIR_JRA3Q/grib<br />mkdir $DATADIR_JRA3Q/progs （一式をここに置く）<br />cd $DATADIR_JRA3Q/grib ; ln -s ../progs/JRA3Q/Hist<br />cd $DATADIR_JRA3Q/progs</pre>

## データの取得

./download_auto.py

- **--time_start/-s** <取得開始時刻をYYYYMMDDHH形式で指定>

- **--time_end/-s** <取得終了時刻をYYYYMMDDHH形式で指定>

- **--prefix/-p** <取得するデータのprefixを指定>：'anl_p125', 'fcst_p125', 'anl_surf125', 'fcst_phy2m125'に対応

- **--variable|-v** <変数名>：指定しない場合はautoとなる。prefixが'anl_p125', 'fcst_p125'の場合には、指定した変数のみ取得する

- **--missing|-m**：月毎に配置されているctl、idlを取得しない（未取得のデータファイルのみを再取得したい場合など）

- **--netrc|-n** <ファイル名>：指定しない場合は~/.netrc を探しに行く

- **--user|-u** <DIASアカウント名>：.netrcが読み込まれた場合は不要

## 取得したデータのチェック

./check.sh 2023 01 01 2023 01 31 slp

2023/01/01から2023/01/31までのslpのダウンロードをチェックする場合

## grib形式からgrads形式へ変換 

./grib2bin-jra3q.sh 2023 01 01 2023 01 31

2023/01/01から2023/01/31まで変換する場合

${DATADIR_JRA3Q}/grib/Hist/Daily/以下に格納される

## 6時間毎データを日平均、月平均に変換

./exec_convert.sh

- **--var|-v** <変数名>（必須）

- **--nsyy/-ys** <開始年> 

- **--nsmm|-ms** <開始月>

- **--neyy/-ye** <終了年>

- **--nemm|-me** <終了月>

- **-f **: 既に作成したファイルを更新する場合

- **-d**：日平均（-dか-mのどちらかは必須）

- **-m**：月平均

- **-z** <層数>：表面データの場合には、-z 1 を指定

## データ取得、変換作業の自動化

./main_auto.sh

download_auto.py、grib2bin-jra3q.sh、exec_convert.shを実行する

## 未取得のファイルの確認

./main_check.sh

実行前に開始、終了日を書き換えておく。内部ではcheck.shを実行し、結果をtinfoin.txtに書き出す。

 ## 不足しているファイルを再取得する

./download_missing.py

tinfoin.txtを読み込み、記載されているファイルをdownload_auto.pyで再取得する。
