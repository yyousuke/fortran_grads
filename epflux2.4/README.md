# E-Pフラックス解析用プログラム（ver.2.4）

1. Makefileを編集

2. 準備

    % ln -s ncep/ydef.dat Y
    
    % ln -s ncep/zdef.dat Pのようにy軸z軸のグリッド情報ファイルをリンク
    
    % ln -s uwnd.4times.daily.grads U 
    
    % ln -s vwnd.4times.daily.grads V
    
    % ln -s air.4times.daily.grads T のように水平風、温度のデータをリンク

3. epflux.fを編集

    nx, ny, np, ntをデータに合わせた値に変える
    
    nx, ny, np ; 水平(x, y), 鉛直(p)方向のグリッド数
    
    nt ; 時間方向に平均するデータの個数  (時間平均された値のみが出力されます)

＊ 定数を変えれば他の惑星にも適用できる

4. コンパイル

    % make clean
    
    % make

5. 実行
    
    % ./epflux
    
    imethod(擾乱の種類)とista(計算を始めるレコードNo.)の値を入力
    
    imethod = 1 ;  短周期擾乱(時間平均からの偏差を擾乱とする)
    
    imethod = 2 ;  長周期擾乱(時間平均したデータに対して帯状平均とそれからの擾乱を計算する。)
    
    imethod = 3 ;  短周期+長周期(帯状平均からの偏差を擾乱とする)

6. 作図
    
    gradsを起動し、run epf.gsとすればE-Pフラックスのクイックルックができます。
    
    (E-Pフラックスの発散の値には惑星の半径をかけているので注意)

