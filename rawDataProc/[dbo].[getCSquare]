
-- =============================================
-- Author:		Carlos Pinto
-- Create date: 27 of May of 2014
-- Description:	This function extracts the latitude of a c-square of 0.05
-- =============================================
CREATE FUNCTION [dbo].[getCSquare](@Latitude as decimal(16,10),@Longitude as decimal(16,10))

RETURNS nvarchar(20)
AS
BEGIN
/*
	--select dbo.getCSquare(-4.85, 47.28)
	declare @Latitude as decimal(6,3)
	declare @Longitude  as decimal(6,3)
	set @Latitude = -42.85

	set @Longitude = 147.28
*/
	
	declare @csquare as nvarchar(20)
	-- Declare the return variable here
	Declare @G16 as int
	Declare @G17 as int
	Declare @G18 as int
	Declare @B19 as int
	Declare @B16 as float
	Declare @B17 as float
	Declare @B18 as float
	Declare @C16 as float
	Declare @C13 as float
	Declare @D13 as float
	declare @D16 as float
	Declare @C12 as float
	Declare @D12 as float
	Declare @E16 as float
	Declare @F16 as float
	declare @C17 as float
	declare @D17 as float
	declare @C18 as float
	declare @D18 as float
	declare @E17 as float
	declare @F17 as float
	declare @E18 as float
	declare @F18 as float

	set @C12 = (FLOOR((ABS(@latitude)+10)/100))*0.000001
	set @D12 = (FLOOR((ABS(@Longitude)+20)/200))*0.000001

	set @C13 = abs(@Latitude)-@C12
	set @D13 = abs(@Longitude)-@D12

	set @D16 = FLOOR(@D13/10)
	set @C16 = floor(@C13/10)
	set @E16 = round(cast(@C13-(@C16*10) as float),7)
	set @F16 = round(@D13-(@D16*10),7)
	set @C17 = floor(@E16)
	set @D17 = floor(@F16)

	set @C18 = floor(@E17)


	set @F17 = round((@F16-@D17)*10,7)
	set @D18 = FLOOR(@F17)
	
	set @E17 = round((@E16-@C17)*10,7)
	set @C18 = FLOOR(@E17)

	set @D18 = floor(@F17)
	set @E18 = round((@E17-@C18)*10,7)
	set @F18 = round((@F17-@D18)*10,7)

	set @B16 = 4-(((2*FLOOR(1+(@Longitude/200)))-1)*((2*FLOOR(1+(@Latitude/200)))+1))
	set @B17 = (2*FLOOR(@E16*0.2))+FLOOR(@F16*0.2)+1


	set @B18 = (2*FLOOR(@E17*0.2))+FLOOR(@F17*0.2)+1
	set @G16 = (@B16*1000)+ (@C16*100) + @D16	
	set @G17 = (@B17*100)+(@C17*10)+@D17
	set @G18 = (@B18*100)+(@C18*10)+@D18
	set @B19 = (2*FLOOR(@E18*0.2))+FLOOR(@F18*0.2)+1

	SET @csquare = cast(@G16 as varchar(10))+ ':' + cast(@G17 as varchar(10))+ ':' + cast(@G18 as varchar(10)) + ':' + cast(@B19  as varchar(10))

/*
	print '@E16 - ' + cast(@E16 as varchar)
	print '@C17 - ' + cast(@C17 as varchar)
	print '@E17 - ' + cast(@E17 as varchar)
	print '@F17 - ' + cast(@F17 as varchar)
	print '@B18 - ' + cast(@B18 as varchar)

	print ''
	print ''
	print '@G16 - ' + cast(@G16 as varchar)
	print '@G17 - ' + cast(@G17 as varchar)
	print '@G18 - ' + cast(@G18 as varchar)
	print '@B19 - ' + cast(@B19 as varchar)
	print '@csquare:'  + @csquare
*/
	RETURN @csquare

END

