

select year,sum([No. Recs]) as [No. Recs],sum(totweight) totweight,sum(totvalue) as totvalue ,sum(kw_fishinghours) as kw_fishinghours, sum(fishing_hours) as fishing_hours, sum(Surface) as Surface ,dbo.getCSQuareArea(c_square) as area, dbo.getLatitude(c_square) as lat,  sum(Surface * (cast(subsurface_prop as float)/100)) as SubSurFaceSweptArea 
, sum( (Surface / dbo.getCSQuareArea(c_square)) ) as Surface_SweptAreaRatio,sum( ((Surface * (cast(subsurface_prop as float)/100)) / dbo.getCSQuareArea(c_square))) as SubSurface_SweptAreaRatio, c_square, Fishing_category, dbo.getLatitude(c_square) as Latitude,dbo.getLongitude(c_square) as Longitude
 from (

SELECT     Data.year, Data.c_square,  COUNT(*) AS [No. Recs], SUM(Data.totweight) AS totweight, 
                      SUM(Data.totvalue) AS totvalue, SUM(Data.kw_fishinghours) AS kw_fishinghours, SUM(Data.fishing_hours) AS fishing_hours, 
                      SUM(Data.fishing_hours * isnull(([FirstFactor]*Power(avg_kw,[SecondFactor]))/cast(1000 as float),B.gearWidth) * Data.ICES_avg_fishing_speed * 1.852) AS Surface,  JNCC.Fishing_category, subsurface_prop 
FROM         tblAux_benthisGearWidthsForWGSFD15 AS B RIGHT OUTER JOIN
                      tblAux_Lookup_Metiers_incl_log AS JNCC ON B.benthis_met = JNCC.Benthis_metiers RIGHT OUTER JOIN
                      _2016_ICES_VMS_Datacall_VMS AS Data ON JNCC.LE_MET_level6 = LTRIM(RTRIM(REPLACE(Data.LE_MET_level6, '"', '')))
WHERE     B.benthis_met in ('OT_CRU', 'OT_DMF', 'OT_MIX', 'OT_MIX_CRU', 'TBB_CRU', 'TBB_DMF')
GROUP BY Data.year, Data.c_square, JNCC.Fishing_category,		 subsurface_prop
union all
SELECT     Data.year, Data.c_square,  COUNT(*) AS [No. Recs], SUM(Data.totweight) AS totweight, 
                      SUM(Data.totvalue) AS totvalue, SUM(Data.kw_fishinghours) AS kw_fishinghours, SUM(Data.fishing_hours) AS fishing_hours, 
                      SUM(Data.fishing_hours * isnull(([FirstFactor]*avg_oal + [SecondFactor])/cast(1000 as float),B.gearWidth) * Data.ICES_avg_fishing_speed * 1.852) AS Surface,  JNCC.Fishing_category, subsurface_prop	 
FROM         tblAux_benthisGearWidthsForWGSFD15 AS B RIGHT OUTER JOIN
                      tblAux_Lookup_Metiers_incl_log AS JNCC ON B.benthis_met = JNCC.Benthis_metiers RIGHT OUTER JOIN
                      _2016_ICES_VMS_Datacall_VMS AS Data ON JNCC.LE_MET_level6 = LTRIM(RTRIM(REPLACE(Data.LE_MET_level6, '"', '')))
WHERE     B.benthis_met in ('OT_MIX_DMF_BEN', 'OT_MIX_CRU_DMF', 'OT_SPF')
GROUP BY Data.year, Data.c_square, JNCC.Fishing_category,		 subsurface_prop
union all
SELECT     Data.year, Data.c_square,  COUNT(*) AS [No. Recs], SUM(Data.totweight) AS totweight, 
                      SUM(Data.totvalue) AS totvalue, SUM(Data.kw_fishinghours) AS kw_fishinghours, SUM(Data.fishing_hours) AS fishing_hours, 
                      SUM(Data.fishing_hours * isnull(([FirstFactor]*Power(avg_oal,[SecondFactor]))/cast(1000 as float),B.gearWidth) * Data.ICES_avg_fishing_speed * 1.852) AS Surface,  JNCC.Fishing_category,  subsurface_prop	 
FROM         tblAux_benthisGearWidthsForWGSFD15 AS B RIGHT OUTER JOIN
                      tblAux_Lookup_Metiers_incl_log AS JNCC ON B.benthis_met = JNCC.Benthis_metiers RIGHT OUTER JOIN
                      _2016_ICES_VMS_Datacall_VMS AS Data ON JNCC.LE_MET_level6 = LTRIM(RTRIM(REPLACE(Data.LE_MET_level6, '"', '')))
WHERE     B.benthis_met in ('OT_MIX_DMF_PEL', 'TBB_MOL', 'DRB_MOL')
GROUP BY Data.year, Data.c_square, JNCC.Fishing_category,		 subsurface_prop
union all 
SELECT     Data.year, Data.c_square, COUNT(*) AS [No. Recs], 
                      SUM(Data.totweight) AS totweight, SUM(Data.totvalue) AS totvalue, SUM(Data.kw_fishinghours) AS kw_fishinghours, SUM(Data.fishing_hours) AS fishing_hours, 
                      SUM( ( pi()*square(isnull(([FirstFactor]*Power(avg_kw,[SecondFactor]))/cast(1000 as float),(B.gearWidth))/(2*pi())) * (Data.fishing_hours/2.591234)))  as Surface, 
					  JNCC.Fishing_category,  subsurface_prop
FROM         tblAux_benthisGearWidthsForWGSFD15 AS B RIGHT OUTER JOIN
                      tblAux_Lookup_Metiers_incl_log AS JNCC ON B.benthis_met = JNCC.Benthis_metiers RIGHT OUTER JOIN
                      _2016_ICES_VMS_Datacall_VMS AS Data ON JNCC.LE_MET_level6 = LTRIM(RTRIM(REPLACE(Data.LE_MET_level6, '"', '')))
where ISNULL(B.benthis_met, JNCC.JNCC_grouping) = 'SDN_DMF'
GROUP BY Data.year, Data.c_square, JNCC.Fishing_category,		 subsurface_prop
union all 
SELECT     Data.year, Data.c_square,COUNT(*) AS [No. Recs], 
                      SUM(Data.totweight) AS totweight, SUM(Data.totvalue) AS totvalue, SUM(Data.kw_fishinghours) AS kw_fishinghours, SUM(Data.fishing_hours) AS fishing_hours, 
                      SUM(    ( pi()*square(isnull(([FirstFactor]*Power(avg_oal,[SecondFactor]))/cast(1000 as float),(B.gearWidth))/(2*pi())) * (Data.fishing_hours/1.912500) )*1.5)  as Surface, 
                      JNCC.Fishing_category,  subsurface_prop
FROM         tblAux_benthisGearWidthsForWGSFD15 AS B RIGHT OUTER JOIN
                      tblAux_Lookup_Metiers_incl_log AS JNCC ON B.benthis_met = JNCC.Benthis_metiers RIGHT OUTER JOIN
                      _2016_ICES_VMS_Datacall_VMS AS Data ON JNCC.LE_MET_level6 = LTRIM(RTRIM(REPLACE(Data.LE_MET_level6, '"', '')))
--WHERE     (Data.c_square = '7401:390:382:1') AND (Data.year = 2010)
where ISNULL(B.benthis_met, JNCC.JNCC_grouping) = 'SSC_DMF' 
GROUP BY Data.year, Data.c_square, JNCC.Fishing_category,		 subsurface_prop

) as t
group by year, c_square, Fishing_category




