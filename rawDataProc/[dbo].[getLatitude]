
-- =============================================
-- Author:		Carlos Pinto
-- Create date: 27 of May of 2014
-- Description:	This function extracts the latitude of a c-square of 0.05
-- =============================================
CREATE FUNCTION [dbo].[getLatitude](@csquare as nvarchar(20))

RETURNS float
AS
BEGIN

	-- Declare the return variable here
	DECLARE @Lat as float
	Declare @D38 as float
	Declare @D39 as float
	Declare @D40 as float
	Declare @G41 as float
	Declare @B40 as float
	Declare @C41 as float
	Declare @C38 as float

	set @Lat = null
	/*
	Declare @csquare as varchar(20)
	set @csquare = '7500:215:363:1'
	*/
		SET @C41 = SUBSTRING(@csquare,14,1)
		SET @C38 = SUBSTRING(@csquare,1,1)
		-- Add the T-SQL statements to compute the return value here
		SET @D38 = SUBSTRING(@csquare,2,1)
		SET @D39 = SUBSTRING(@csquare,7,1)
		SET @D40 = SUBSTRING(@csquare,11,1)

		SET @G41 = round(@C41*2,-1)/10
		SET @B40 = (round(ABS(@C38-4)*2,-1)/5)-1
		SET @Lat = ((@D38*10)+ @D39+(@D40*0.1)+(@G41*0.05)+0.025)*@B40
		RETURN @Lat
/*
	print 'B40:' + cast(@B40 as varchar)
	print 'G41:' + cast(@G41 as varchar)
	print 'D38:' + cast(@D38 as varchar)
	print 'D39:' + cast(@D39 as varchar)
	print 'D40:' + cast(@D40 as varchar)
	print 'C-square:' + @csquare
	print 'Latittude:' + cast( @lat  as varchar)
*/

	-- Return the result of the function

END
