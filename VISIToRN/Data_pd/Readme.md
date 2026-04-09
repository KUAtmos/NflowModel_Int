This directory stores the input dataset for VISIToRN function.

Here is the list of input data for runnning VISIToRN

* dat_dir = River direction (TRIP)　360*720: dat_dir.rdata
* dat_dis = Distance in river (km) in each grid 360*720: DistMat.rdata
* RivArea =  = River surface area 360*720: RivArea.rdat
* Land_mask = Land Mask (計算領域を指定する、グリッドの番号) : LandMask_Loop.rdat (全球)
* dat_tas = Monthly Air Temperature 360*720*12(or 360×720×12×年数): dat_tas.rdat (CRU-NCEPの2001年データをとりあえず置いておきます)
* Sriv = Initial value for River water storage　360×720: 1回モデルを一年分、走らせて初期値を得ます（最初は0でもOK）。
* SrivN = Initial value for River N storage　360×720:　1回モデルを一年分、走らせて初期値を得ます（最初は0でもOK）。
* VISITro = Runoff (m3/grid/day)　360×720×12(or 360×720×12×年数): VISITro_nat_ag.rdat
* VISITnl = N leaching (kg-N/grid/day) 360×720×12(or 360×720×12×年数): VISITnl_nat_ag.rdat or VISITnl_nat.rdat (←自然起源のみ)
* dat_humanN = N loading from Human Sewage (kg-N/grid/day) 360×720×12(or 360×720×12×年数): HumanN.rdat (複数年入っているので、単年を指定。季節変動しないと仮定)
* ParFit = 3 Parameters for N reactions in River (Vmax, Kno3, Q10): 

注1：VISIToRN自体はDailyの計算ですが、入力値は12ヶ月分です。月30日として、同じ値を繰り返し使っています。
注2：Cppの使用上、RでのNA値は受け付けません。データに含まれる場合はすべて0に置換してください。


## Usage ##



