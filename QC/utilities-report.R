
# ggplot settings
theme_icesqc <- function(legend.position = "none")
  theme(axis.text.y   = element_text(colour="black"),
        axis.text.x   = element_text(colour="black"),
        axis.title.y  = element_text(size=14,face="bold"),
        axis.title.x  = element_text(size=14,face="bold"),
        legend.position = legend.position,
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))


# utility functions
make_polVMS <- function(coordGrd, resolution = 0.05) {
  polVMS <- data.frame(long = rep(c(-1, -1, 1,  1, -1), nrow(coordGrd)) * resolution/2,
                       lat  = rep(c(-1,  1, 1, -1, -1), nrow(coordGrd)) * resolution/2) 
  polVMS$long <- polVMS$long + rep(coordGrd$SI_LONG, each = 5)
  polVMS$lat <- polVMS$lat + rep(coordGrd$SI_LATI, each = 5)
  polVMS$order <- rep(1:5, nrow(coordGrd))
  polVMS$hole <- FALSE
  polVMS$piece <- 1
  polVMS$id <- rep(1:nrow(coordGrd), each = 5)
  polVMS$group <- paste0(polVMS$id, ".", polVMS$piece)
  
  polVMS
}


# default-spatial-ggplot
spatialplot <- function(data, xyrange = spatCore) {
  ggplot(polLand, aes(long, lat)) +
    geom_polygon(aes(group=group), fill = "light grey") +
    coord_fixed(ratio = vmstools::lonLatRatio(min(xyrange$xrange), min(xyrange$yrange)),
                xlim = xyrange$xrange, 
                ylim = xyrange$yrange) +
    labs(x = "Longitude", y = "Latitude") +
    geom_polygon(data = data, 
                 aes(long, lat, group = group, fill = cols)) +
    geom_polygon(data = polLand, 
                 aes(long, lat, group = group), 
                 colour = "black", size = 0.25, fill = "transparent")
}

# area submitted plot
data_coverage <- function(coordGrd, spatBound, res) {

  # create a fortied polygon of csquares
  polVMS <- make_polVMS(coordGrd, resolution = res)
  polVMS$year <- rep(coordGrd$year, each = 5)
  polVMS$cols <- "red"
  
  spatialplot(polVMS, spatBound) +
    facet_wrap(~ year, ncol = 2) +
    theme_icesqc()
}


gear_splits <- function(response, data = ICES_VE, ylab_text, func = sum, year_groups = 1, gear_groups = 1) {
  dat2tab <- 
    with(data, 
         tapply(response, list(gear_code = gear_code, year = year), func, na.rm = TRUE))

  # split by year?
  out <- ""
  if (year_groups == 1) {
    out <- c(out, kable(dat2tab, booktabs = TRUE))
  } else {
    grp <- cut(as.numeric(colnames(dat2tab)), year_groups)
    for (igrp in levels(grp)) {
      out <- c(out, kable(dat2tab[, grp == igrp, drop = FALSE], booktabs = TRUE), "\n")
    }    
  }

  dat2plot <- as.data.frame.table(dat2tab, responseName = "response")
  dat2plot <- dat2plot[complete.cases(dat2plot),]
  max <- tapply(dat2plot$response, dat2plot$gear_code, max, na.rm = TRUE)
  if (gear_groups == 1 || length(unique(max)) == 1) {
    grp <- rep(1, length(max))
  } else {
    max[!is.finite(max)] <- min(max, na.rm = TRUE)
    grp <- as.numeric(cut(sqrt(max), gear_groups))    
  }

  p <- 
    lapply(sort(unique(grp), decreasing = TRUE), function(i) {
      dat <- dat2plot[dat2plot$gear_code %in% names(max)[grp == i],]

      ggplot(dat, aes(x = year, y = response)) +
      geom_line(aes(group = gear_code, colour = gear_code), lwd=1.5) +
      xlab("Year") + ylab(ylab_text) +
      theme_icesqc(legend.position = "right")
    })
  
  list(table = structure(paste(out, collapse = "\n"), format = "latex", class = "knitr_kable"), 
       plots = p)
}
