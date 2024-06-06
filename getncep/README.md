# NCEP/NCAR再解析データ取得用プログラム

作成者：山下陽介
2005/8/4 　v1.2
2024/06/06　最新版　v2.0　

NCEP/NCAR再解析データを取得するためのプログラム

## インストール

プログラム本体(getncep.sh)と付属ファイル(ncep.doc)をインストールしたい ディレクトリに置く

## コマンドの説明を見る
% ./getncep.sh --help

## 使用方法

% ./getncep.sh type time_step ファイル名

type: オプション1（データの種類）: surface|pressure|gauss|other|tropopause|spec

time_step: オプション2（時間間隔）: mon|day|6hr

### オプション1（データの種類）
surface ;  表面データ
pressure ; 気圧面データ
gauss ; surface flux データ
other ; other flux データ
tropopause ; 対流圏界面データ
spec ; T62 spectral coefficients

### オプション2（時間間隔）
mon ; monthly mean and other derived data
day ; daily mean data
6hr ; 4 time daily individual obs data

例：2009年の3次元東西風（uwnd）6時間毎データ
% ./getncep.sh pressure 6hr uwnd.2009.nc

例：月平均の3次元東西風データ
getncep.sh pressure mon uwnd.mon.mean.nc