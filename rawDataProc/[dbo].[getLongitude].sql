

-- =============================================
-- Author:		Carlos Pinto
-- Create date: 27 of May of 2014
-- Description:	This function extracts the latitude of a c-square of 0.05
-- =============================================
CREATE FUNCTION [dbo].[getLongitude](@csquare as nvarchar(20))

RETURNS float
AS
BEGIN

	-- Declare the return variable here
	DECLARE @Lon as float
	Declare @E38 as float
	Declare @E39 as float
	Declare @E40 as float
	Declare @H41 as float
	Declare @B41 as float

	Declare @C41 as float
	Declare @C38 as float

	/*
	Declare @csquare as varchar(20)
	set @csquare = '1502:499:100:1'
	*/

	SET @C41 = SUBSTRING(@csquare,14,1)
	SET @C38 = SUBSTRING(@csquare,1,1)
	-- Add the T-SQL statements to compute the return value here
	SET @E38 = SUBSTRING(@csquare,3,2)
	SET @E39 = SUBSTRING(@csquare,8,1)
	SET @E40 = SUBSTRING(@csquare,12,1)

	SET @H41 = (round((@C41-1)/2,1)-FLOOR((@C41-1)/2))*2

	SET @B41 = ((2*(round(@C38,-1)/10))-1)*-1

	SET @LON = ((@E38*10)+ @E39+(@E40*0.1)+(@H41*0.05)+0.025)*@B41


	/*
	print 'C41:' + cast(@C41 as varchar)
	print 'C38:' + cast(@C38 as varchar)
	print 'E38:' + cast(@E38 as varchar)
	print 'E39:' + cast(@E39 as varchar)
	print 'E40:' + cast(@E40 as varchar)
	print 'C-square:' + @csquare
	print 'Latittude:' + cast( @lon  as varchar)
	*/

	-- Return the result of the function
	RETURN @LON

END
