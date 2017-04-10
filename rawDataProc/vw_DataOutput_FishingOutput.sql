CREATE VIEW [dbo].[vw_DataOutput_FishingOutput]
AS

SELECT  Data.year, 
        Data.c_square, 
        SUM(Data.fishing_hours) AS fishing_hours, 
        SUM(Data.kw_fishinghours) AS kw_fishinghours, 
        SUM(Data.totweight) AS totweight, 
        SUM(Data.totvalue) AS totvalue, 
        JNCC.Fishing_category_FO, 
        dbo.getLatitude(Data.c_square) AS MidLat, 
        dbo.getLongitude(Data.c_square) AS MidLon, 
        geography::STPolyFromText('POLYGON((' + CAST(dbo.getLongitude(Data.c_square) + 0.025 AS varchar(10)) + ' ' 
                                              + CAST(dbo.getLatitude(Data.c_square) + 0.025 AS varchar(10)) + ', ' 
                                              + CAST(dbo.getLongitude(Data.c_square) - 0.025 AS varchar(10)) + ' ' 
                                              + CAST(dbo.getLatitude(Data.c_square) + 0.025 AS varchar(10)) + ', ' 
                                              + CAST(dbo.getLongitude(Data.c_square) - 0.025 AS varchar(10)) + ' ' 
                                              + CAST(dbo.getLatitude(Data.c_square) - 0.025 AS varchar(10)) + ', ' 
                                              + CAST(dbo.getLongitude(Data.c_square) + 0.025 AS varchar(10)) + ' ' 
                                              + CAST(dbo.getLatitude(Data.c_square) - 0.025 AS varchar(10)) + ', ' 
                                              + CAST(dbo.getLongitude(Data.c_square) + 0.025 AS varchar(10)) + ' ' 
                                              + CAST(dbo.getLatitude(Data.c_square) + 0.025 AS varchar(10)) + '))', 
                                  4326) AS CSquare2, 
        Data.LE_MET_level6, 
        Data.vessel_length_category
FROM  tblAux_benthisGearWidthsForWGSFD15 AS B RIGHT OUTER JOIN
        tblAux_Lookup_Metiers_incl_log AS JNCC ON B.benthis_met = JNCC.Benthis_metiers RIGHT OUTER JOIN
          _2016_ICES_VMS_Datacall_VMS AS Data ON JNCC.LE_MET_level6 = LTRIM(RTRIM(REPLACE(Data.LE_MET_level6, '"', '')))
WHERE (JNCC.Fishing_category_FO IS NOT NULL)
GROUP BY Data.year, Data.c_square, JNCC.Fishing_category_FO, Data.LE_MET_level6, Data.vessel_length_category

