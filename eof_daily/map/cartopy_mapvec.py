#!/usr/bin/env python3
import pandas as pd
import numpy as np
import os
import math
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import cartopy.crs as ccrs
from pandas.plotting import register_matplotlib_converters

register_matplotlib_converters()
plt.rcParams['xtick.direction'] = 'in'  # x軸目盛線を内側
plt.rcParams['xtick.major.width'] = 1.2  # x軸大目盛線の長さ
plt.rcParams['ytick.direction'] = 'in'  # y軸目盛線を内側
plt.rcParams['ytick.major.width'] = 1.2  # y軸大目盛線の長さ

# pc number
ipc = 1
#
#
# 軸のラベルとタイトルの設定
title = "EOF-" + str(ipc) + " vector"

# 出力ファイル
output_fname = "EOF-" + str(ipc) + ".png"

# 入力ファイル
input_dir = ".."
input_fname = "EOF-vector.dat"


def readdata(input_path):
    # ファイルから読み込み
    df = pd.read_fwf(input_path, header=None)
    df = df[df.iloc[:, 0] == ipc]
    # データの取り出し
    lon = np.array(df.iloc[:, 1]).reshape(33, 49)
    lat = np.array(df.iloc[:, 2]).reshape(33, 49)
    val = np.array(df.iloc[:, 4]).reshape(33, 49)
    return lon, lat, val


def plotmap(lon, lat, val, title=None, output_path=None):
    lon_step = 5
    lat_step = 5
    # 軽度・緯度範囲
    lon_min = lon.min()
    lon_max = lon.max()
    lat_min = lat.min()
    lat_max = lat.max()
    #
    # マップを作成
    fig = plt.figure(figsize=(10, 10))
    # cartopy呼び出し
    ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
    ax.set_extent([lon_min, lon_max, lat_min, lat_max])  # 領域の限定

    # 経度、緯度線を描く
    xticks = np.arange(-180, 180, lon_step)
    yticks = np.arange(-90, 90, lat_step)
    gl = ax.gridlines(crs=ccrs.PlateCarree(),
                      draw_labels=True,
                      linewidth=1,
                      linestyle=':',
                      color='k',
                      alpha=0.8)
    gl.xlocator = mticker.FixedLocator(xticks)  # 経度線
    gl.ylocator = mticker.FixedLocator(yticks)  # 緯度線
    gl.top_labels = False  # 上側の目盛り線ラベルを描かない
    gl.right_labels = False  # 下側の目盛り線ラベルを描かない
    #
    # 海岸線を描く
    ax.coastlines(color='k', linewidth=1.2)
    #
    # 等圧線をひく間隔をlevelsにリストとして入れる
    levels = range(math.floor(val.min() - math.fmod(val.min(), 2)),
                   math.ceil(val.max()) + 1, 2)
    # 等圧線をひく
    cr = ax.contour(lon, lat, val, levels=levels, colors='k', linewidths=1.2)
    # ラベルを付ける
    cr.clabel(cr.levels[::2], fontsize=12, fmt="%d")
    #
    #
    # タイトルを付ける
    if title is not None:
        ax.set_title(title, fontsize=20)
    #
    if output_path is not None:
        # ファイルへの書き出し
        plt.savefig(output_path, dpi=300, bbox_inches='tight')
    plt.show()
    plt.close()


if __name__ == '__main__':
    # 入力データの読み込み
    input_path = os.path.join(input_dir, input_fname)
    lon, lat, val = readdata(input_path)
    val = val * 100.
    #
    # 作図
    plotmap(lon, lat, val, title=title, output_path=output_fname)
