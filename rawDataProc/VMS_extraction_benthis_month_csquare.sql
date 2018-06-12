
select
  year, 
  month,
  c_square,
  Benthis_metiers,  
  sum(Data.totweight) as totweight, 
  sum(Data.totvalue) as totvalue, 
  sum(Data.kw_fishinghours) as kw_fishinghours, 
  sum(Data.fishing_hours) as fishing_hours
from
  (select * from tblAux_Lookup_Metiers_incl_log where not Benthis_metiers = 'NA') as metiers 
    right outer join
  _2018_ICES_VMS_Datacall_VMS as Data 
    on 
  metiers.LE_MET_level6 = LTRIM(RTRIM(REPLACE(Data.LE_MET_level6, '"', '')))              
group by year, month, c_square, Benthis_metiers
order by year, month, Benthis_metiers, c_square
