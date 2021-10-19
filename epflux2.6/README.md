# E-Pフラックス解析用プログラム（ver.2.6）

1. ${EPF_HOME}にあるMkincludeを編集

    (システムごとのMkincludeファイルの例が${EPF_HOME}/sys以下にある)

2. データのリンクを作成する

    ${EPF_HOME}/dataに緯度方向のグリッド情報ファイルydef.datと鉛直方向のグリッド情報ファイルzdef.datを置く
    
    % ln -s uwnd.4times.daily.grads u.grads
    
    % ln -s vwnd.4times.daily.grads v.grads
    
    % ln -s air.4times.daily.grads t.grads のように水平風、温度のデータをリンク
    
${EPF_HOME}い移動し

    % make link
    
として実行ディレクトリ(run)からリンクを張る

3. グリッド数、データ数の変更

    ${EPF_HOME}/src/epflux.fを編集
    
    nx, ny, np, ntをデータに合わせた値に変える
    
    nx, ny, np ; 水平(x, y), 鉛直(p)方向のグリッド数
    
    nt ; 時間方向に平均するデータの個数 (時間平均された値のみが出力されます)

4. 各種定数の変更(地球の場合はそのまま使えます)

    ${EPF_HOME}/src/init.fを編集

   ここでは各種定数の値とキーボードからの入力の部分を設定

   ＊定数を変えれば他の惑星にも適用できる

5. 使用する式の変更（optional）

    ieq = 1 ; 元の式にCOS(lat)を掛けたもの(デフォルト)
    
    ieq = 2 ; 元の式
    
    ieq = 3 ; 元の式にAR * COS(lat)を掛けたもの
    
    ieq = 4 ; 元の式にARを掛けたもの

6,. コンパイル

    % make clean
    % make

7. 実行
    
    % cd run
    
    % ./a.out
    
    imethod(擾乱の種類)とista(計算を始めるレコードNo.)の値を入力します。 
    
    imethod = 1 ;  短周期擾乱(時間平均からの偏差を擾乱とする)
    
    imethod = 2 ;  長周期擾乱(時間平均したデータに対して帯状平均とそれからの擾乱を計算する。)
    
    imethod = 3 ;  短周期+長周期(帯状平均からの偏差を擾乱とする)

8. クイックルック

    ${EPF_HOME}/grdからEPy.ctl, EPz.ctl, diver.ctl. epf.gsをコピー
    
    gradsを起動し、run epf.gsとすればE-Pフラックスのクイックルックができます。
    
    (epf.gsの中でE-Pフラックスの発散の値に地球の半径をかけているので注意)
