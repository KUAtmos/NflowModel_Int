month="01 02 03 04 05 06 07 08 09 10 11 12" 

for i in $month
do
 cdo mergetime /data/SharedData/GeoschemData/ExtData/GEOS_4x5/MERRA2/2015/$i/MERRA2.2015*.A1.4x5.nc4 tmp$i.nc4 
 cdo select,name=PRECTOT tmp$i.nc4 tmp2$i.nc4
 cdo monmean tmp2$i.nc4 tmpavg$i.nc4
done

cdo mulc,2678400 tmpavg01.nc4 tmpexp01.nc4 #kg/m2/mon
cdo mulc,2419200 tmpavg02.nc4 tmpexp02.nc4
cdo mulc,2678400 tmpavg03.nc4 tmpexp03.nc4
cdo mulc,2592000 tmpavg04.nc4 tmpexp04.nc4
cdo mulc,2678400 tmpavg05.nc4 tmpexp05.nc4
cdo mulc,2592000 tmpavg06.nc4 tmpexp06.nc4
cdo mulc,2678400 tmpavg07.nc4 tmpexp07.nc4
cdo mulc,2678400 tmpavg08.nc4 tmpexp08.nc4
cdo mulc,2592000 tmpavg09.nc4 tmpexp09.nc4
cdo mulc,2678400 tmpavg10.nc4 tmpexp10.nc4
cdo mulc,2592000 tmpavg11.nc4 tmpexp11.nc4
cdo mulc,2678400 tmpavg12.nc4 tmpexp12.nc4
cdo mergetime tmpexp*.nc4 tmpexp2.nc4


cdo remapcon,geos.05x05.grid tmpexp2.nc4 0505PRECTOT.nc4

rm tmp*nc4



month="01 02 03 04 05 06 07 08 09 10 11 12"
# 各月の秒数（28日, 30日, 31日）に応じて定義
# ※閏年でない2015年を想定
seconds_in_month=(2678400 2419200 2678400 2592000 2678400 2592000 2678400 2678400 2592000 2678400 2592000 2678400)

i=0
for m in $month
do
  sec=${seconds_in_month[$i]}
  
  # 月データを結合・選択・平均
  cdo mergetime /data/SharedData/GeoschemData/ExtData/GEOS_4x5/MERRA2/2015/$m/MERRA2.2015*.A1.4x5.nc4 tmp$m.nc4
  cdo select,name=PRECTOT tmp$m.nc4 tmp2$m.nc4
  cdo monmean tmp2$m.nc4 tmpavg$m.nc4

  # 月平均降水強度 × 当月の秒数 → 月間降水量（kg/m²）
  cdo mulc,$sec tmpavg$m.nc4 monthprec$m.nc4

  ((i++))
done
