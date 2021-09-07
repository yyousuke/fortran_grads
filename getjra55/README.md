# getjra55

JRA-55データを取得し単純バイナリ形式に変換、日平均・月平均データを作成するスクリプト集

- **wget_jra55.sh**：筑波大学　計算科学研究センターからJRA-55データを取得する

- **grib2bin-jra_prs_day.sh**：gribデータを単純バイナリ形式に変換

- **exec_convert.sh**：日平均・月平均データを作成

## 準備

DATADIR_JRA55にJRA-55データを格納するディレクトリの絶対パスを記述

設定ファイルに以下の記述（bashの場合、$HOME/.bashrc）

    export DATADIR_JRA55="JRA-55データへの絶対パス"

exec_convert.shでは、fortranコンパイラとbig endianを扱うオプションを設定する

    FC="gfortran"
    OPT="-O -fconvert=big-endian -frecord-marker=4"

intelコンパイラでは、

    FC="ifort"
    OPT="-O -assume byterecl -convert big_endian"


## データの取得

使用方法：

    % ./wget_jra55.sh

出力：
    gpvjma.ccs.hpcc.jp/data/jra55/Hist/Daily 以下に格納される

データのリンク：

    % ln -s `pwd`/gpvjma.ccs.hpcc.jp/data/jra55/Hist/Daily ${DATADIR_JRA55}/grib/Hist/Daily

## grib形式からgrads形式へ変換 

使用方法：

    % ./grib2bin-jra_prs_day.sh 

出力：
    ${DATADIR_JRA55}/grads/6hr 以下に格納される

## 6時間毎データを日平均、月平均に変換

使用方法：

    % ./exec_convert.sh オプション

出力：
    ${DATADIR_JRA55}/grads/daily 以下に格納される
    
オプション：    

    --var|-v 変数名（必須）

    --nsyy/-ys 開始年 --nsmm|-ms 開始月

    --neyy/-ye 終了年 --nemm|-me 終了月

    -f : 既に作成したファイルを更新する場合

    -d：日平均  -m：月平均（-dか-mのどちらかは必須）

    （表面データの場合には、-z 1 を指定）


