SELECT 
  year, 
  gear_code, 
  vessel_length_category, 
  vms_enabled, 
  Area_27 as area,
  sum(fishing_days) AS fishing_days, 
  sum(kw_fishing_days) AS kw_fishing_days, 
  sum(totweight) AS totweight, 
  sum(totvalue) AS totvalue 
FROM 
  _2018_ICES_VMS_Datacall_LE as LE
    INNER JOIN
  ICES_StatRec_mapto_Areas as A
    ON LE.ICES_rectangle = A.ICESNAME
GROUP BY 
  year, 
  Area_27, 
  gear_code, 
  vessel_length_category, 
  vms_enabled
ORDER BY 
  year, 
  gear_code, 
  vessel_length_category, 
  vms_enabled, 
  Area_27
