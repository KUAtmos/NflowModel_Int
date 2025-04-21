#Run this file to get a total of four files for regional aggregation.

#Wet deposition of NOy, selection of chemical species and unit conversion
cdo mergetime GEOSChem.WetLossLS.2016* MGT1.nc #kg/s*gridarea
cdo monmean MGT1.nc tmp1.nc
cdo mulc,31536000000000 tmp1.nc tmp2.nc #mg/year*gridarea
cdo select,name=WetLossLS_NITs tmp2.nc tmp4.nc
cdo select,name=WetLossLS_NIT tmp2.nc tmp5.nc
cdo select,name=WetLossLS_HNO3 tmp2.nc tmp6.nc
cdo select,name=WetLossLS_PAN tmp2.nc tmp7.nc
cdo chname,WetLossLS_NITs,NOx tmp4.nc tmp8.nc
cdo chname,WetLossLS_NIT,NOx tmp5.nc tmp9.nc
cdo chname,WetLossLS_HNO3,NOx tmp6.nc tmp10.nc
cdo chname,WetLossLS_PAN,NOx tmp7.nc tmp11.nc
cdo add tmp8.nc tmp9.nc tmp12.nc 
cdo add tmp10.nc tmp11.nc tmp13.nc
cdo add tmp12.nc tmp13.nc tmp14.nc
cdo mulc,0.222 tmp14.nc tmp15.nc #N換算 mg(N)/year*gridarea
cdo vertsum tmp15.nc NOYWETMON.nc 

#Dry deposition of NOy, selection of chemical species and unit conversion
cdo mergetime GEOSChem.DryDep.2016* MGT16.nc #分子数/cm2*s
cdo monmean MGT16.nc tmp17.nc
cdo mulc,0.00000000733135 tmp17.nc tmp18.nc #mg(N)/m2/year
cdo select,name=DryDep_NITs tmp18.nc tmp19.nc
cdo select,name=DryDep_NIT tmp18.nc tmp20.nc
cdo select,name=DryDep_HNO3 tmp18.nc tmp21.nc
cdo select,name=DryDep_NO2 tmp18.nc tmp22.nc
cdo select,name=DryDep_PAN tmp18.nc tmp23.nc
cdo chname,DryDep_NITs,NOx tmp19.nc tmp24.nc
cdo chname,DryDep_NIT,NOx tmp20.nc tmp25.nc
cdo chname,DryDep_HNO3,NOx tmp21.nc tmp26.nc
cdo chname,DryDep_NO2,NOx tmp22.nc tmp27.nc
cdo chname,DryDep_PAN,NOx tmp23.nc tmp28.nc
cdo add tmp24.nc tmp25.nc tmp29.nc
cdo add tmp26.nc tmp27.nc tmp30.nc
cdo add tmp30.nc tmp28.nc tmp31.nc
cdo add tmp29.nc tmp31.nc NOYDRYMON.nc

#Wet deposition of NHx, selection of chemical species and unit conversion
cdo select,name=WetLossLS_NH3 tmp2.nc tmp34.nc #mg/year*gridarea
cdo select,name=WetLossLS_NH4 tmp2.nc tmp35.nc
cdo chname,WetLossLS_NH3,NHx tmp34.nc tmp36.nc
cdo chname,WetLossLS_NH4,NHx tmp35.nc tmp37.nc
cdo add tmp36.nc tmp37.nc tmp38.nc
cdo mulc,0.8235 tmp38.nc tmp39.nc  #mg(N)/year*gridarea
cdo vertsum tmp39.nc NHXWETMON.nc

#Dry deposition of NHx and selection of chemical species
cdo select,name=DryDep_NH3 tmp18.nc tmp41.nc #mg(N)/m2/year
cdo select,name=DryDep_NH4 tmp18.nc tmp42.nc
cdo chname,DryDep_NH3,NHx tmp41.nc tmp43.nc
cdo chname,DryDep_NH4,NHx tmp42.nc tmp44.nc
cdo add tmp43.nc tmp44.nc NHXDRYMON.nc


cdo add NOYWETMON.nc NHXWETMON.nc NWETMON.nc
cdo add NOYDRYMON.nc NHXDRYMON.nc NDRYMON.nc
cdo remapcon,geos.05x05.grid NWETMON.nc 0505NWETMON.nc
cdo remapcon,geos.05x05.grid NDRYMON.nc 0505NDRYMON.nc
rm tmp*.nc