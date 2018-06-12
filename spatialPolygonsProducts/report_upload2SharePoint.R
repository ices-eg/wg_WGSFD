library(icesSharePoint)

# set up where files are going
directory <- "2017 Meeting Docs/06. Data/Standard data products"
site <- "/ExpertGroups/wgsfd/"
site_collection <- "https://community.ices.dk"

spdir(directory = directory, site = site, site_collection = site_collection)

# create maps folder (will not overwrite if already exists)
spdir.create("shapefiles",
             directory = directory,
             site = site,
             site_collection = site_collection)

# 'move' into effort_maps folder
directory <- "2018 Meeting Docs/06. Data/Standard data products/shapefiles"

# copy all files in plots folder over
copy2sp <- function(fname, comment = "2015 shapefiles", overwrite = FALSE)
{
  
  if (fname %in% spfiles(directory = directory, site = site, site_collection = site_collection)) {
    if (!overwrite) {
      message("skipping ", fname)
      return(NULL)
    }
  }
  
  file <- paste0("spatialPolygonsProducts/shapefiles/", fname)
  size <- file.info(file)$size
  message("copying ", fname, ". size: ", size)

  #con <- readBin(file, raw(), n = size+1)

  # save files
  #spfile.create(fname, text = con,
  #              directory = directory,
  #              site = site,
  #              site_collection = site_collection)

  if (size > 5e6) {
    # do in bits
    # geenerate guid for stream linking
    guid <- uuid::UUIDgenerate()
    # open connection to file
    con <- file(file, "rb")
    # read first 5000 kb
    out <- readBin(con, raw(), n = 5e6)
    
    
    
    # finally close connection
    close(con)
  }
  
  spfile.create(fname, con = file,
                directory = directory,
                site = site,
                site_collection = site_collection)

    
  # checkin
  spcheckin(comment, fname,
            directory = directory,
            site = site,
            site_collection = site_collection)

  invisible(NULL)
}

# get file to copy
fnames <- dir("spatialPolygonsProducts/shapefiles/")
fnames <- fnames[!fnames %in% spfiles(directory = directory, site = site, site_collection = site_collection)]
size <- file.info(paste0("spatialPolygonsProducts/shapefiles/", fnames))$size
# do it!
httr::set_config( httr::config( ssl_verifypeer = 0L ) )
lapply(fnames[order(size)], copy2sp)


guid <- uuid::UUIDgenerate()
icesSharePoint:::spfile.startupload("test.txt", "start of file\n", directory = directory, site = site, site_collection = site_collection, guid = guid)
icesSharePoint:::spfile.finishupload()


service <- sprintf("GetFileByServerRelativeUrl('%s/%s')", site, directory, "test.txt")
uri <- paste0(site_collection, site, "/_api/web/", utils::URLencode(service))
icesSharePoint:::spget(uri)

