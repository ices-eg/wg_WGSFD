![](RackMultipart20220121-4-1kcp8ir_html_f20a92026ad6909b.jpg)

**Guidelines for the VMSdatacall\_proposedWorkflow.r February 2022**

**International Council for the Exploration of the Sea Conseil International pour l&#39;Exploration de la Mer**

H.C.AndersensBoulevard44–46 DK-1553 Copenhagen V Denmark

Telephone (+45) 33 38 67 00

Telefax (+45) 33 93 42 15 [www.ices.dk](http://www.ices.dk/)[info@ices.dk](mailto:info@ices.dk)

Recommended format for purposes of citation:

ICES. 2022. Guidelines for the VMSdatacall\_proposedWorkflow.r. (ver. 3, 14 February 2022). http://doi.org/10.17895/ices.pub.4705

The material in this report may be reused using the recommended citation. ICES may only grant usage rights of information, data, images, graphs, etc. of which it has ownership. For other third-party material cited in this report, you must contact the original copyright holder for permission. For citation of datasets or use of data to be included in other databases, please refer to the latest ICES data policy on the ICES website. All extracts must be acknowledged. For other reproduction requests please contact the General Secretary.

This document is the product of an Expert Group under the auspices of the International Council for the Exploration of the Sea and does not necessarily represent the view of the Council.

© 2022 International Council for the Exploration of the Sea

**Contents**

**[1](#_Toc63072938)****Part1 3**

[1.1Step 1: Installation ofR 3](#_Toc63072939)

[1.2Step 2: Installation ofRStudio 4](#_Toc63072940)

[1.3Step 3: Installingvmstools 5](#_Toc63072941)

**[2](#_Toc63072942)****Part2 7**

**[2.1](#_Toc63072943)****Load thedata 9**

[#- 1a) Load VMStools underlying data 9](#_Toc63072944)

[#-1b) Looping through the data 9](#_Toc63072945)

[#- 1c) load tacsat and eflalo data from file 9](#_Toc63072946)

**[2.2](#_Toc63072947)****Clean the tacsatdata 11**

**[2.3](#_Toc63072948)****Clean the eflalodata 12**

**[2.4](#_Toc63072949)****Mergethetacsatandeflalodatatogether 13**

**[2.5](#_Toc63072950)****Defineactivity 14**

**[2.6](#_Toc63072951)****Dispatch landings of merged eflalo at the pingscale 17**

**[2.7](#_Toc63072952)****Assign c-square, year, month, quarter, area and create table1 18**

**[2.8](#_Toc63072953)****Assign year, month, quarter, area and create table2 19**

**[3](#_Toc63072954)****Contacts 20**

**[4](#_Toc63072955)****Changelog 21**

**[Annex1 Format specificationforVMS data (VE) 22](#_Toc63072956)**

**[Annex 2 Format specificationforLogbook data (LE) 23](#_Toc63072957)**

1.
# Part1

This document is designed to aid analysts streamline the process of extracting VMS data in accordance with requirements of the ICES VMS data call.

Part 1 of the document is provides guidelines for installing all the software necessary for data manipulation and aggregation into the requested format. The software used, R and RStudio, are available asfreeware.

The document is designed to aid all users, regardless of their experience using R. The steps listed cover the installation of R and RStudio and detailed information will be providedtocoverallstagesoftheinstallationprocesstoensuresuccess.Dependingon yourskillsyoumightwanttojumpsomeofthesteps.Toensureconsistencyacrossall usersweadviseinstallingVMStoolsversion0.72.Followingthesestepsshouldenable quick and simple processing of alldata.

All the instructions and code below were tested in windows 7 and 8. However, if for any reason something is not working you can contact one of the members of our support team (emails at the end of the document).

  1.
## Step 1: Installation ofR

(Where R is already installed (any 3.x.x 32 bits version) move to step 2)

Completion of the first two steps of these guidelines is dependent on the user&#39;s computersecuritysetting.Ininstanceswhereadministratorprivilegeisrequiredthen pleaseaskamemberofyourIT/computersupportteamtorunthefirsttwostepsfor you.

So let&#39;s start by instaling R; click on the link below to download R version 3.1.3.

[https://cran.rstudio.com/bin/windows/base/old/3.1.3/R-3.1.3-win.exe](https://cran.rstudio.com/bin/windows/base/old/3.1.3/R-3.1.3-win.exe)

Once it is downloaded, double click on the file. Depending on your security settings you might get a pop up security warning asking if you want to Run or Cancel the installation, Click **Run**.

- Select a language (when you select the language, bear in mind that these instructions are inEnglish)

- AtthispointyoushouldbeontheRinstallationWizardmenu.Justclick **Next**

- HereyouarepresentedwiththeGNUgeneralpubliclicensewhichyouaremost welcome to read. Click **Next**

- The menu (picture below) will appear and you will be asked to select the destinationfolder

![](RackMultipart20220121-4-1kcp8ir_html_c7776596859df004.png)

Ratherthanacceptingthedefault(C:Files-3.1.3)youshouldclickonthebrowsebutton andcreateadirectoryC:-3.2.1thiswillallowyoutoinstallpackageswithouthaving administratorprivileges.Onceyouhavechangedthefolderjustclick **Next**

- Inthemenu&quot;Selectcomponents&quot;simplydeselect/untickthe64-bitfiles.Click

### Next.

- Click **Next** all the menus until the end of instalation, and that&#39;s it; R is now in- stalled on thecomputer.

  1.
## Step 2: Installation ofRStudio

(Where R studio is already installed move to step 3)

Ifyoudon&#39;thaveadministratorprivilegeonyourcomputeryouwillneedtocallyour IT/computersupporttoinstallRStudio.FirstdownloadRStudiobyclickingonthelink below

[https://download1.rstudio.org/RStudio-0.99.467.exe](https://download1.rstudio.org/RStudio-0.99.467.exe)

Once downloaded, double click on it. Depending on your security settings you might get a popup security warning asking if you want to Run or Cancel the installation, Click on **Run**. At this point you should be on the RStudio installation Wizard menu. RStudioisveryeasytoinstallsojustacceptalldefaultsandclick **Next** inallthemenus until theend.

That&#39;s it. Step 2 is complete, RStudiois now installed.

  1.
## Step 3: Installingvmstools

Next, download a compiled version of vmstools 0.72 by clicking the link below. Make sure you click save rather than open..

![Shape7](RackMultipart20220121-4-1kcp8ir_html_851005f3d5727d6a.gif)

install.packages(c(&quot;cluster&quot;,&quot;data.table&quot;,&quot;doBy&quot;,&quot;maps&quot;,&quot;mapdata&quot;,&quot;ma ptools&quot;,&quot;PBSmapping&quot;,&quot;sp&quot;,&quot;Matrix&quot;,&quot;ggplot2&quot;))

[https://github.com/nielshintzen/vmstools/releases/download/0.72/vmstools\_0.72.zip](https://github.com/nielshintzen/vmstools/releases/download/0.72/vmstools_0.72.zip)Now start Rstudio to install all the necessary R packages that vmstools depends on. Copy the following text

intotheconsoleandpressenter.Itshouldstartinstallingallthepackagesneeded.This might take two or three minutes and your console should look like this once it has finished.

![](RackMultipart20220121-4-1kcp8ir_html_87cc747119b9d5b6.jpg)

To install vmstools click on the Tools tab on the main menu in RStudio. Then select InstallPackages.Thiswilltriggerapopupmenuliketheonebelow.Clickonthedown arrowinthe&quot;Installfrom:&quot;dialogueboxandselectthesecondoption&quot;PackageArchive File(Zip;tar.gz)&quot; then browse to the vmstools zip file that you just downloaded and pressInstall.

![](RackMultipart20220121-4-1kcp8ir_html_4cba68b437cc424b.png)

That&#39;s it; you now have all you need to process your data.

1.
# Part2

Partoneoftheseguidelineshaveguidedyouthroughtheinstallationofallthesoftware needed to process your data into the formats specified in the data call. Now, part two willfocusonguidingyouthroughtheeightstepsthatcomprisetheworkflow.Theaim is to get your data converted into the tables requested by the data call in Annex 1 and 2..

The work flow was developed in R, the principal objective being to facilitate the submission of data in the specified format by providing all country institutions with a standardised tool for data extraction. This will make data outputs more comparable and easy to work with.

ManyofthespecificfunctionsthatthisworkflowusestoextractandprocessVMSVMS and landings data are part of the open-source vmstools package. This package was specificallydevelopedtoworkwithVMSandlandingsdata.Ifyouwanttolearnmore about this tool you will find plenty of information and tutorials in the linkbelow.

[https://github.com/nielshintzen/VMStools/wiki](https://github.com/nielshintzen/VMStools/wiki)

Beforeinitiatingtheworkflow,itisfirstnecessarythatyouhaveallVMSandlandings datainthetacsat2andeflalo2formatsrespectively.Ifyouneedmoreinformationabout either format you can download a document with the detailed specifications for both in the linkbelow.

[https://github.com/nielshintzen/VMStools/releases/download/0.0/Exchange\_EFLALO](https://github.com/nielshintzen/VMStools/releases/download/0.0/Exchange_EFLALO2_v2-1.doc)[2\_v2-1.doc](https://github.com/nielshintzen/VMStools/releases/download/0.0/Exchange_EFLALO2_v2-1.doc)

If you are experiencing difficulties and your data is not yet in the tacsat and/or eflalo format please get in touch with one of the contacts at the bottom of these guidelines. Someone will get back to you and help you to rearrange your data into the specified formats allowing you to move on with the analysis.

The proposed workflow is not a one-size-fits-all solution and there are parts of the script that may need to be adapted to allow for the specific nature of the fisheries data from each country. Throughout this document all parts of code that need adjustment will be highlighted and explained so you should have a good understanding of what is happening at alltimes.

The code is divided in eight sections and within these there are many blocks. We will explain briefly what each section and block does and its purpose within the code. The ideaoftheguidelinesisnottoexplainindetailwhateachlineofcodedoesbuttogive an overview of what is happening at each stage. The script itself is well annotated, so ifyouarefamiliarwithRandthevmstoolspackage,youprobablywon&#39;tneedtofollow these guidelines asclosely.

Let&#39;s begin:

House keeping

Open &quot;Rstudio&quot; and load the workflow. Before making any changes save the script with a different name. This will allow you to quickly refer back to the original code in case anything unexpected happens.

Justaquicknote,whichmaybeveryobviousforallofthoseusingR,butnotsomuch for someone just trying to follow the guidelines. Anything in the code after a hashtag (#)signisnotcodeanditwon&#39;tbereadbytheprogram.The#signisusedtoadd

sections and block headers or general annotations. As such, the first line of code starts on line 23.

To make it easier to follow the guidelines we will explain the code, referencing the sections, headers (numbered) and blocks by highlighting them in bold.

# - Clear workspace

This code will just clear your work space to allow you to start afresh. Also, the three packages that will be needed to run the code will be loaded into the session. If you followed the instructions in part one these should already be installed and loading them shouldn&#39;t be a problem.

#- Settings paths

Atthispointyouneedtoreplacethepathsshowninthecodewithyourownones.The approach used in the code is one of best practice, as everything will be in one main folder &quot;VMSdatacall&quot;. This will make it very easy to navigate between folders and to backup.So,foryourownconvenienceitisrecommendedthatyouusethedefaultpaths aslistedinthecode.However,itispossibletochangethesebysimplyspecifyingyour personal destination folders and defining the chosenpath.

#- Settings and specific thresholds

The thresholds here defined will be used later in different processes throughout the code. These will include, data cleaning or definition of vessel state (i.e. fishing/not fishing). The values set for the thresholds are considered to be reasonable and unless there are particularities in your data there shouldn&#39;t be a need to change these values.

  1.
# Load thedata

## #- 1a) Load VMStools underlying data

ThiswillloadintothesessionsupportdatasuchasamapofEurope,listofharbours andICESareasthatwillbeusedthroughoutthecode.

## #-1b) Looping through the data

Thenextlineofcodehasa&quot;for&quot;loopwhichmeansthattheallthecodewithintheloop willrunatthesametime.Thisparticularloopstretchesfromline51toline496leaving onlyacoupleoflinesattheendofthecode.However,beforerunningthe&quot;for&quot;loop thereareafewthingsthatneedtobechanged/adaptedtoyourcase.

In order to ensure that everything is working properly and to have a better understandingofwhatthecodeisdoinginsidetheloopwewillrunonesingleyearas atest.Ifweareabletorunoneyearofdatawithoutcomingacrosserrorsthenwecan runthecodeforalltheyearsatonce.

Soforthemomentwewillignorethisblock(#-1b) **Looping**** through ****the**** data**andcopy the line below into theconsole:

![Shape10](RackMultipart20220121-4-1kcp8ir_html_7654d64c61b2602a.gif)

year\&lt;-2009

## #- 1c) load tacsat and eflalo data from file

In your &quot;Data&quot; folder you should have all your tacsat and eflalo files in the .RDATA format.Inthecodeitisexpectedthatyourfileshavethefollowingnamingconvention &quot;tacsat\_ XXXX&quot; i.e. tacsat\_2009; tacsat\_2010, etc. The same naming convention is applied to the eflalo files. This will allow the code to load the files as they are needed duringthe&quot;for&quot;loop.Failingtocorrectlynamethefileswillresultinanerror.

Sincewehavejustcopied **year\&lt;-**** 2009**totheconsole,whenwerunthisblockonlythe 2009 year data will beloaded.

Now that you have just loaded both tacsat and eflalo for 2009 the next two lines will just change the name of your objects to &quot;tacsat&quot; and &quot;eflalo&quot;. Depending on what your objects are called you may need to change the code. (tip: If at any stage you don&#39;t remember what the names are just type &quot;ls()&quot; and a list of all objects already loaded will appear). If your object names are not &quot;tacsat&quot; and &quot;eflalo&quot; you will need to adapt the code. However, this is a simple process. Below are two examples for changing objects names in this case the names are &quot;2009Tacsat&quot; and another called &quot;Tac09&quot;

![Shape11](RackMultipart20220121-4-1kcp8ir_html_7a43faac36ba21ba.gif)

tacsat \&lt;- get(paste(year,&quot;Tacsat&quot;,sep=&quot;&quot;))

tacsat \&lt;- get(paste(&quot;Tac&quot;,substr(year,3,4),sep=&quot;&quot;))

If the objects are already called tacsat and eflalo then, you don&#39;t need to run those two lines and you can add an # at the beginning of each line. However, make sure that whatever you have called your objects the naming structure is consistent across all years otherwise the &quot;For&quot; loop won&#39;t run.

#- Make sure data is in right format

It just ensures that your files are formatted properly.

#- Take only VMS pings and eflalo records in the ICES areas

This block of code will identify all VMS pings (tacsat) and landings (eflalo) within the ICES areas.

  1.
# Clean the tacsatdata

Thissectionwillfocuson&quot;cleaning&quot;thedatainthetacsatfile.Theinformationinthe tacsat(vms)comesfromanelectronicsystemthatusesGPSinformationtocollectthe data on board the vessel and uses a satellite link to send the data to the database. Despitethereliabilityofthissystemconditionsatseaarenotalwaysthebest.Thereare twomainopportunitiesforerrorstooccur,whenreceivingorsendingdatafromthe GPSandtothedatabase.Thecodeinthissectionwilllooktothemostcommonerrors andtrytoidentifyallofthem.Thecodewillnotonlydeletetheerrorsbutalsokeepa recordofwhatwasdeletedallowingyoutokeeptrackofhowmuchdatayouhavelost due toerrors.

#- Keep track of removed points

Thissectionwillcheckforfivecommontypesoferrors.Ateachofthesecheckserrors will be removed from the tacsat object. However, the data removed will be kept and savedinthe&quot;Results&quot;foldersoyoucanverifytheerrors.Also,thevolumeforeachof errorsforeachofthefivechecksisrecordedthe&quot;remrecsTacsat&quot;object.Thisobjectwill tell you percentage wise how much you have lost in relation to the original tacsat object.

#- Remove duplicate records

#- Remove points that cannot be possible

#-Removepointswhicharepseudoduplicatesastheyhaveanintervalrate\&lt;xminutes #- Remove points inharbour

#- Remove points on land

All of the above are self-explanatory and each of the five blocks will check for a particular type of error, remove them where they occur and store the removed entries in the &quot;Results&quot; folder and will quantify the number of values removed.

#- Save the remrecsTacsat file

The file is now saved and by typing &quot;remrecsTacsat&quot; into the console you will get an overview of how much data was lost due to errors.

#- Save the cleaned tacsat file

Now you have your file cleaned and saved so no need to repeat the process in future analysis.

  1.
# Clean the eflalodata

This section, like the previous one also focuses on &quot;cleaning&quot; the data. This time the target is the eflalo file. The types of errors are of a different nature but once again the codetriestoaccountforthemostcommonserrors.Asintheprevioussection,thecode will keep track of what data has been removed and how much. All these files can be found in the &quot;Results&quot; folder. One should spend a bit of time looking at the data removed as it can be very useful to understand why and where problemsoccur.

#- Keep track of removed points

The &quot;remrecsEflalo&quot; object will keep you informed of how much data has been removed.

#- Warn for outlying catch records

Basicallythisblocklooksforoutliers.Foreachspecies,itgeneratesadata-drivenoutlier threshold.Ifanyoutliersarefound,thesewillbeconvertedinto&quot;NA&quot;values.Youcan checkinthe&quot;Results&quot;folderforthefilescontainingalltheoutliersandyoucandouble check if they are correct or not. If they were correct then you can run the code again but the code will need someadjustments.

#- Remove non-unique trip numbers

#- Remove impossible time stamp records #- Remove trip starting before 1st Jan

#- Remove trip with overlap with another trip

#- Remove records with arrival date before departure date

Theaboveblockheadersareself-explanatoryandthecodeineachoftheblocksisjust identifyingthosecommonerrorsandremovingthemfromtheeflaloobject.

#- Save the remrecsEflalo file #- Save the cleaned eflalo file

The&quot;remrecsEflalo&quot;fileissavedforfuturereference.Soisthecleanedeflalofilewhich, likethetacsat,willbereadytouseinthefuture.

  1.
# Mergethetacsatandeflalodatatogether

Insectionfourwebringthetacsatandeflalotogetherbymergingtocreateanewobject. Thiswillenableustorelatethelandingscomponent(eflalo)tothethevesselactivity

i.e. VMS (tacsat)

#- Merge eflalo and tacsat

Thefilestacsatandeflalowillbecombinedusingsomeverycleveralgorithmsthatuse the vessel identifier and date and time in both data sets to relate the landings to the correspondingVMSdataforthesametrip.

###-AssigngearandlengthtotacsatThenewobjecttacsatpisnowamergedversion ofthecleanedtacsatandeflaloobjects.However,thenewobjecthasn&#39;tinheritedallthe fieldsfromeflaloduetoreasonsofprocessingspeedandworkability.Atthispointwe willextractsomedatafromtheeflalodatasettopopulatethecorrespondingtacsatp fields.Thedataweareinterestedinaredatathatwillbeusedlateroninthecodeto populatethefinaldatatables.Thingslikegear;kw;metiers,etc.

#- Save not merged tacsat data

Not all vessel activity is associated with fishing events; quite often vessels may be testingequipmentorcharteredtodojobsotherthanfishing.So,themergeexecutedin the previous block only includesVMS data that can be linked to corresponding landings records. As such, it will not be possible to merge all tacsat data for allocation to the tacsatp object. This block will save both the merged and non-merged data into the &quot;Results&quot;folder.

  1.
# Defineactivity

Thisisacrucialsection,asvesselactivitywillbedefinedhere.Also,thisisthesection thatneedsthemostcustomizationforwhichsomeknowledgeoffisheriesactivitieswill beneeded.Inthissectionwewilltrytoexplainthestepsinmoredetailandincorporate some reproducible examples as well. The first couple of lines in this section will calculate time interval between points. The time values and the interval threshold will be paramount in identifying vessel activity lateron.

#- Remove points with NA&#39;s in them in critical places

This block gets rid of any rows in the tacsatp for which critical information (vessel reference, latitude, longitude, speed, date and time) is missing. If this data wasn&#39;t removed it would most likely lead to errors.

#- Define speed thresholds associated with fishing for gears

The code in this block creates a very useful plot of speed frequency by gear. This plot will be saved in the &quot;Results&quot; folder and before you run any further code you should look closely at the output plot.

The three last lines of this block will create a threshold object. However, your input and knowledge of the relevant fisheries will be needed at this stage. The threshold objectwillholdtheminimumandmaximumspeedoffishingforeachofthegears(i.e. the minimum and maximum speeds at which the specific fishing activity is thought to occur).Tohelpyouwiththistaskyoushouldlookatthepreviousspeedfrequencyplot to help distinguish steaming from fishingevents.

By running the third last line in this block you create an object &quot;speedarr&quot; with all the different gears in your data. The second and third lines will fill in column 2 and 3 of the &quot;speedarr&quot; object with the minimum and maximum fishing speeds. These values are set to 1kt and 6kt by default. At this stage you will need to set up the upper and lower limits for each of the gears. Although there are several ways of accomplishing this, we will demonstrate one of them here. In the example below we use a list of 5 gears(DRB;PTB;OTT;GN;FPO)althoughyouarelikelytohavemanymoregearsin yourdatasetsoyouwillneedtoextendthecodetoaccommodateallgearsaccordingly.

To create our example, copy the code below into the console:

![Shape16](RackMultipart20220121-4-1kcp8ir_html_12f52f25492d183d.gif)

speedarr$min[which(speedarr$LE\_GEAR==&quot;GN&quot;)]\&lt;-0.5 ; speedarr$ max[which(speedarr$LE\_GEAR==&quot;GN&quot;)]\&lt;- 3 speedarr$min[which(speedarr$LE\_GEAR==&quot;FPO&quot;)]\&lt;- 0.5 ; speedarr$ max[which(speedarr$LE\_GEAR==&quot;FPO&quot;)]\&lt;-3

speedarr

![Shape17](RackMultipart20220121-4-1kcp8ir_html_bbccfc5b04932d63.gif)

So, in the example above you can easily see how each line applies to one gear and on theleftyouhavetheminimumvaluesontherightthemaximum.Makesurewhenyou copy and paste the lines you change the gears and values on bothsides.

If your data varies from year to year you might want to check if you have different gears in different years. Before running the full code, you should make sure that all gears are included in the code above.

#-Analyseactivityautomatedforcommongearsonly.Usethespeedarrfortheother gears

This block allows you to select some gears for which the detection can be done automatically. This is another functionality of VMStools which applies for the most common gears. So, in this block you will need to choose which gears to want to apply auto detection. You can add or delete gears in the first line of code in this block.

The remainder of the code in the block will split the tacsatp object in two depending on whether gears will be detected automatically or whether the thresholds need to be userdefinedaccordingtothecodefromthepreviousblock.Theremaininglinesinthe block don&#39;t need to bechanged.

#- Fill the storeScheme values based on analyses of the pictures

In this block the speed histogram plot created previously will be used once more. You will have to identify the peaks in the plot for the gears for which you want the activity to be automatically detected (bear in mind that the algorithm was developed with trawling in mind).

So,firstofallmakesureyouhavealineforeachofthegears(seelinebelow)changing the gears and mean speedsaccordingly.

![Shape18](RackMultipart20220121-4-1kcp8ir_html_624a874ecde17481.gif)

storeScheme$means[which(storeScheme$analyse.by == &quot;SSC&quot;)] \&lt;- c(&quot;-9 0 9&quot;)

Nowusingtheplot,identifywherethepeaksareandusethistochangethecode.Make sure you follow the same nomenclature as the example provided. Also, for the algorithm to perform better, we need to create a mirror image of the peaks and with 0 (zero) in the middle .If the number of peaks for a particular gear is greater or less than 5 you will need to add a line (like the one below) with the true number of peaks observed. In the example above there were three peaks -9, 0, 9 so we would need to add the line below to thecode.

![Shape19](RackMultipart20220121-4-1kcp8ir_html_f9e04cd3e8c1bcf.gif)

storeScheme$peaks[which(storeScheme$analyse.by==&quot;SSC&quot;)] \&lt;-

3

![Shape20](RackMultipart20220121-4-1kcp8ir_html_f9e04cd3e8c1bcf.gif)

The second half of the block, checks the results of the auto detection; if they are not satisfactorytheanalysisisrunoncemore;thistimeusingfixedpeaks.However,inthis workflowwewillnotbeusingfixedpeakssononeedtoworryaboutthis.

#- Assign for visually inspected gears a simple speed rule classification

This block deals with all the other gears that are not automatically detected. The code simplyappliestheupperandlowerlimitsdefinedpreviouslytodefineifvesselactivity as either steaming orfishing.

#- Combine the two dataset together again

Now that all gears have had their activity defined, the code in this block is just putting it all back together in one object. As usual the object will be saved in the &quot;Results&quot; folder.

  1.
# Dispatch landings of merged eflalo at the pingscale

This section calculates the total daily landings (weight and value) and splits the values equally among the daily fishing pings.

  1.
# Assign c-square, year, month, quarter, area and create table1

Weareatfinalstageandthecodeinthisblockwillgenerateoneofthetablesrequested by the data call (see annex 1). The first part of the code pulls together all the fields needed to create the table. The second part deals with the aggregation byCSquare.

  1.
# Assign year, month, quarter, area and create table2

We have reached the last section. As in the previous one, the first part of the code will create all the fields needed for table 2 (see annex 2) as requested in the data call. The second part of the code deals with the aggregation into CSquares.

Runningthelasttwolinessavesthedataintothe&quot;Results&quot;folder.Don&#39;tforgettocheck the outputs to make sure that everything iscorrect.

If you are happy with all the results then you can proceed running the entire code. Make sure you double check the names of the input files, ensuring they follow the convention (as in the example) and run the entire code.

1.
# Contacts

Josefine Egekvist:[jsv@aqua.dtu.dk](mailto:jsv@aqua.dtu.dk)Niels Hintzen: [niels.hintzen@wur.nl](mailto:niels.hintzen@wur.nl) Rui Catarino: [rui.catarino@ices.dk](mailto:rui.catarino@ices.dk)

Lara Salvany: [lara.salvany@ices.dk](mailto:lara.salvany@ices.dk)

1.
# Changelog

| **Date** | **Change** | **Prepared by** |
| --- | --- | --- |
|
March 2016 |
Initial version created |
Rui Catarino, ICES |
| 1 February 2019 | Update | Lara Salvany, ICES |

1 February 2021 Update Lara Salvany, ICES

# Annex1 Format specificationforVMS data (VE)

The specification of the format for VMS can be found here: [datsu.ices.dk/web/selRep.aspx?Dataset=145](http://datsu.ices.dk/web/selRep.aspx?Dataset=145)

and in the table below

| Start | FieldCode | Datatype | Code List | Mandatory | Description |
| --- | --- | --- | --- | --- | --- |
| 1 | RecordType | char(2) |
 | M | RecordType field consists of a 2-character code,
 which defines the record type and thus the layout of the data fields included on that record. |
| 2 | CountryCode | char(3) | ISO\_3166 | M | ISO 3166-1 alpha-3 codes. The flag country
 of the vessel. |
| 3 | Year | char(4) |
 | M | Year |
| 4 | Month | int(2) |
 | M | Month |
| 5 | NoDistinctVessels | int(5) |
 | M | Number of disctinct vessels |
| 6 | AnonymizedVesselID | nvarchar(500) |
 | M | Anonimyzed vessel ID: Country code + 3 digits and semicolon separated.
 For example: ESP001; ESP003; |
| 7 | C-square | nvarchar(15) |
 | M | 0.05x0.05 degree,
 C-square reference XXXX:XXX:XXX:X |
| 8 | MetierL4 | char(25) | Metier-4 | M | Gear code |
| 9 | MetierL5 | char(50) | Metier-5 | M | Target assemblage |
| 10 | LowerMeshSize | int(50) |
 |
 | LowerMeshSize |
| 11 | UpperMeshSize | int(50) |
 |
 | UpperMeshSize |
| 12 | MetierL6 | char(40) |
 | M | Metier level 6 |
| 13 | VesselLengthRange | char(20) | [BYC\_VesselLRange](http://vocab.ices.dk/?ref=1502) | M | Vessel Length range |
| 14 | AverageFishingSpeed | float(15) |
 | M | Average fishing speed within the aggregation: year, month, c-square, vessel length category, gear code and DCF métier |
| 15 | FishingHour | float(15) |
 | M | Fishing hour calculated from VMS data (excluding nonfishing activity) |
| 16 | AverageVesselLength | float(15) |
 | M | Average vessel length within the aggregation: year,
 month, c-square, gear code and DCF metier |
| 17 | AveragekW | float(15) |
 | M | Average vessel power( kW) within the aggregation: year,
 month, c-square, gear code and DCF metier |
| 18 | kWFishingHour | float(15) |
 | M | kW\*Fishing hours |
| 19 | TotWeight | float(15) |
 | M | Total landings of all species caught in kg |
| 20 | TotValue | float(15) |
 |
 | Total Value of all species caught in Euro |
| 21 | AverageGearWidth | float(15) |
 | M | Averagfe Gear width |

M = mandatory

\*DCF level = Fishing activity – Metier: [http://datacollection.jrc.ec.europa.eu/wordef/fishing-activity-metier](http://datacollection.jrc.ec.europa.eu/wordef/fishing-activity-metier)

[http://ices.dk/marine-data/Documents/RDB/RDB%20Metiers%20by%20fishing%20grounds.csv](http://ices.dk/marine-data/Documents/RDB/RDB%20Metiers%20by%20fishing%20grounds.csv)

# Annex 2 Format specificationforLogbook data (LE)

The specification of the format for VMS can be found here: [datsu.ices.dk/web/selRep.aspx?Dataset=145](http://datsu.ices.dk/web/selRep.aspx?Dataset=145)

and in the table below:

| Start | FieldCode | Datatype | Code List | Mandatory | Description |
| --- | --- | --- | --- | --- | --- |
| 1 | RecordType | char(2) |
 | M | RecordType field consists of a 2-character code,
 which defines the record type and thus the layout of the data fields included on that record. |
| 2 | CountryCode | char(3) | ISO\_3166 | M | ISO 3166-1 alpha-3 codes. The flag country
 of the vessel. |
| 3 | Year | char(4) |
 | M | Year |
| 4 | Month | int(2) |
 | M | Month |
| 5 | NoDistinctVessels | int(5) |
 | M | Number of disctinct vessels |
| 6 | AnonymizedVesselID | nvarchar(500) |
 | M | Anonimyzed vessel ID: Country code + 3 digits and semicolon separated.
 For example: ESP001; ESP003; |
| 7 | ICESrectangle | char(4) | [StatRec](http://vocab.ices.dk/?ref=107) | M | ICES Statistical Rectangle |
| 8 | MetierL4 | char(25) | [Metier-4](http://vocab.ices.dk/?ref=1498) | M | Metier level 4 |
| 9 | MetierL5 | char(50) | [Metier-5](http://vocab.ices.dk/?ref=1499) | M | Gear code |
| 10 | LowerMeshSize | int(50) |
 |
 | LowerMeshSize |
| 11 | UpperMeshSize | int(50) |
 |
 | UpperMeshSize |
| 12 | MetierL6 | char(40) |
 | M | Metier level 6 |
| 13 | VesselLengthRange | char(20) | [BYC\_VesselLRange](http://vocab.ices.dk/?ref=1502) | M | Vessel Length range |
| 14 | VMSEnabled |
 | [YesNoFields](http://vocab.ices.dk/?ref=316) |
 | Yes/No |
| 15 | FishingDays | float(15) |
 | M | Number of fishing days by ICES rectangle. If a
 vessel fished in several ICEs squares one day, the day will be divided by the number of ICES rectangles |
| 16 | kWFishingDays |
 |
 |
 | kW\*FishingDays |
| 17 | TotWeight | float(15) |
 | M | Total landings of all species caught in kg |
| 18 | TotValue | float(15) |
 |
 | Total value of all species caught in Euro |

M = mandatory

\*DCF level = Fishing activity – Metier: [http://datacollection.jrc.ec.europa.eu/wordef/fishing-activity-metier](http://datacollection.jrc.ec.europa.eu/wordef/fishing-activity-metier)

[http://ices.dk/marine-data/Documents/RDB/RDB%20Metiers%20by%20fishing%20grounds.cs](http://ices.dk/marine-data/Documents/RDB/RDB%20Metiers%20by%20fishing%20grounds.cs)
