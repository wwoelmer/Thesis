# Load in the required packages that makes the 3D figure
pacman::p_load(scatterplot3d, rgdal, maptools, tidyverse, zoo, sf, GISTools, ggsn)


sites <- read_csv("./Materials for R mapping/nutrient_mapping/05_May/May_RCC_phosphate_mapping.csv")


scatter.grid <- function (x, y = NULL, z = NULL, color = par("col"), pch = NULL, 
                          main = NULL, sub = NULL, xlim = NULL, ylim = NULL, zlim = NULL, 
                          xlab = NULL, ylab = NULL, zlab = NULL, scale.y = 1, angle = 40, 
                          axis = TRUE, tick.marks = TRUE, label.tick.marks = TRUE, 
                          x.ticklabs = NULL, y.ticklabs = NULL, z.ticklabs = NULL, 
                          y.margin.add = 0, grid = TRUE, box = TRUE, lab = par("lab"), 
                          lab.z = mean(lab[1:2]), type = "p", highlight.3d = FALSE, 
                          mar = c(5, 3, 4, 3) + 0.1, bg = par("bg"), col.axis = par("col.axis"), 
                          col.grid = "grey", col.lab = par("col.lab"), cex.symbols = par("cex"), 
                          cex.axis = 0.8 * par("cex.axis"), cex.lab = par("cex.lab"), 
                          font.axis = par("font.axis"), font.lab = par("font.lab"), 
                          lty.axis = par("lty"), lty.grid = par("lty"), lty.hide = NULL, 
                          lty.hplot = par("lty"), log = "", ...) 
{
  mem.par <- par(mar = mar)
  x.scal <- y.scal <- z.scal <- 1
  xlabel <- if (!missing(x)) 
    deparse(substitute(x))
  ylabel <- if (!missing(y)) 
    deparse(substitute(y))
  zlabel <- if (!missing(z)) 
    deparse(substitute(z))
  if (highlight.3d && !missing(color)) 
    warning("color is ignored when highlight.3d = TRUE")
  if (!is.null(d <- dim(x)) && (length(d) == 2) && (d[2] >= 
                                                    4)) 
    color <- x[, 4]
  else if (is.list(x) && !is.null(x$color)) 
    color <- x$color
  xyz <- xyz.coords(x = x, y = y, z = z, xlab = xlabel, ylab = ylabel, 
                    zlab = zlabel, log = log)
  if (is.null(xlab)) {
    xlab <- xyz$xlab
    if (is.null(xlab)) 
      xlab <- ""
  }
  if (is.null(ylab)) {
    ylab <- xyz$ylab
    if (is.null(ylab)) 
      ylab <- ""
  }
  if (is.null(zlab)) {
    zlab <- xyz$zlab
    if (is.null(zlab)) 
      zlab <- ""
  }
  if (length(color) == 1) 
    color <- rep(color, length(xyz$x))
  else if (length(color) != length(xyz$x)) 
    stop("length(color) ", "must be equal length(x) or 1")
  angle <- (angle%%360)/90
  yz.f <- scale.y * abs(if (angle < 1) angle else if (angle > 
                                                      3) angle - 4 else 2 - angle)
  yx.f <- scale.y * (if (angle < 2) 
    1 - angle
    else angle - 3)
  if (angle > 2) {
    temp <- xyz$x
    xyz$x <- xyz$y
    xyz$y <- temp
    temp <- xlab
    xlab <- ylab
    ylab <- temp
    temp <- xlim
    xlim <- ylim
    ylim <- temp
  }
  angle.1 <- (1 < angle && angle < 2) || angle > 3
  angle.2 <- 1 <= angle && angle <= 3
  dat <- cbind(as.data.frame(xyz[c("x", "y", "z")]), col = color)
  if (!is.null(xlim)) {
    xlim <- range(xlim)
    dat <- dat[xlim[1] <= dat$x & dat$x <= xlim[2], , drop = FALSE]
  }
  if (!is.null(ylim)) {
    ylim <- range(ylim)
    dat <- dat[ylim[1] <= dat$y & dat$y <= ylim[2], , drop = FALSE]
  }
  if (!is.null(zlim)) {
    zlim <- range(zlim)
    dat <- dat[zlim[1] <= dat$z & dat$z <= zlim[2], , drop = FALSE]
  }
  n <- nrow(dat)
  if (n < 1) 
    stop("no data left within (x|y|z)lim")
  y.range <- range(dat$y[is.finite(dat$y)])
  if (type == "p" || type == "h") {
    y.ord <- rev(order(dat$y))
    dat <- dat[y.ord, ]
    if (length(pch) > 1) 
      if (length(pch) != length(y.ord)) 
        stop("length(pch) ", "must be equal length(x) or 1")
    else pch <- pch[y.ord]
    if (length(bg) > 1) 
      if (length(bg) != length(y.ord)) 
        stop("length(bg) ", "must be equal length(x) or 1")
    else bg <- bg[y.ord]
    if (length(cex.symbols) > 1) 
      if (length(cex.symbols) != length(y.ord)) 
        stop("length(cex.symbols) ", "must be equal length(x) or 1")
    else cex.symbols <- cex.symbols[y.ord]
    daty <- dat$y
    daty[!is.finite(daty)] <- mean(daty[is.finite(daty)])
    if (highlight.3d && !(all(diff(daty) == 0))) 
      dat$col <- rgb(red = seq(0, 1, length = n) * (y.range[2] - 
                                                      daty)/diff(y.range), green = 0, blue = 0)
  }
  p.lab <- par("lab")
  y.range <- range(dat$y[is.finite(dat$y)], ylim)
  y.prty <- pretty(y.range, n = lab[2], min.n = max(1, min(0.5 * 
                                                             lab[2], p.lab[2])))
  y.scal <- round(diff(y.prty[1:2]), digits = 12)
  y.add <- min(y.prty)
  dat$y <- (dat$y - y.add)/y.scal
  y.max <- (max(y.prty) - y.add)/y.scal
  if (!is.null(ylim)) 
    y.max <- max(y.max, ceiling((ylim[2] - y.add)/y.scal))
  x.range <- range(dat$x[is.finite(dat$x)], xlim)
  x.prty <- pretty(x.range, n = lab[1], min.n = max(1, min(0.5 * 
                                                             lab[1], p.lab[1])))
  x.scal <- round(diff(x.prty[1:2]), digits = 12)
  dat$x <- dat$x/x.scal
  x.range <- range(x.prty)/x.scal
  x.max <- ceiling(x.range[2])
  x.min <- floor(x.range[1])
  if (!is.null(xlim)) {
    x.max <- max(x.max, ceiling(xlim[2]/x.scal))
    x.min <- min(x.min, floor(xlim[1]/x.scal))
  }
  x.range <- range(x.min, x.max)
  z.range <- range(dat$z[is.finite(dat$z)], zlim)
  z.prty <- pretty(z.range, n = lab.z, min.n = max(1, min(0.5 * 
                                                            lab.z, p.lab[2])))
  z.scal <- round(diff(z.prty[1:2]), digits = 12)
  dat$z <- dat$z/z.scal
  z.range <- range(z.prty)/z.scal
  z.max <- ceiling(z.range[2])
  z.min <- floor(z.range[1])
  if (!is.null(zlim)) {
    z.max <- max(z.max, ceiling(zlim[2]/z.scal))
    z.min <- min(z.min, floor(zlim[1]/z.scal))
  }
  z.range <- range(z.min, z.max)
  plot.new()
  if (angle.2) {
    x1 <- x.min + yx.f * y.max
    x2 <- x.max
  }
  else {
    x1 <- x.min
    x2 <- x.max + yx.f * y.max
  }
  plot.window(c(x1, x2), c(z.min, z.max + yz.f * y.max))
  temp <- strwidth(format(rev(y.prty))[1], cex = cex.axis/par("cex"))
  if (angle.2) 
    x1 <- x1 - temp - y.margin.add
  else x2 <- x2 + temp + y.margin.add
  plot.window(c(x1, x2), c(z.min, z.max + yz.f * y.max))
  if (angle > 2) 
    par(usr = par("usr")[c(2, 1, 3:4)])
  usr <- par("usr")
  title(main, sub, ...)
  if ("xy" %in% grid || grid) {
    i <- x.min:x.max
    segments(i, z.min, i + (yx.f * y.max), yz.f * y.max + 
               z.min, col = col.grid, lty = lty.grid)
    i <- 0:y.max
    segments(x.min + (i * yx.f), i * yz.f + z.min, x.max + 
               (i * yx.f), i * yz.f + z.min, col = col.grid, lty = lty.grid)
  }
  if ("xz" %in% grid) {
    i <- x.min:x.max
    segments(i + (yx.f * y.max), yz.f * y.max + z.min, 
             i + (yx.f * y.max), yz.f * y.max + z.max, 
             col = col.grid, lty = lty.grid)
    temp <- yx.f * y.max
    temp1 <- yz.f * y.max
    i <- z.min:z.max
    segments(x.min + temp,temp1 + i, 
             x.max + temp,temp1 + i , col = col.grid, lty = lty.grid)
    
  }
  
  if ("yz" %in% grid) {
    i <- 0:y.max
    segments(x.min + (i * yx.f), i * yz.f + z.min,  
             x.min + (i * yx.f) ,i * yz.f + z.max,  
             col = col.grid, lty = lty.grid)
    temp <- yx.f * y.max
    temp1 <- yz.f * y.max
    i <- z.min:z.max
    segments(x.min + temp,temp1 + i, 
             x.min, i , col = col.grid, lty = lty.grid)
    
    
    
  }
  
  if (axis) {
    xx <- if (angle.2) 
      c(x.min, x.max)
    else c(x.max, x.min)
    if (tick.marks) {
      xtl <- (z.max - z.min) * (tcl <- -par("tcl"))/50
      ztl <- (x.max - x.min) * tcl/50
      mysegs <- function(x0, y0, x1, y1) segments(x0, 
                                                  y0, x1, y1, col = col.axis, lty = lty.axis)
      i.y <- 0:y.max
      mysegs(yx.f * i.y - ztl + xx[1], yz.f * i.y + z.min, 
             yx.f * i.y + ztl + xx[1], yz.f * i.y + z.min)
      i.x <- x.min:x.max
      mysegs(i.x, -xtl + z.min, i.x, xtl + z.min)
      i.z <- z.min:z.max
      mysegs(-ztl + xx[2], i.z, ztl + xx[2], i.z)
      if (label.tick.marks) {
        las <- par("las")
        mytext <- function(labels, side, at, ...) mtext(text = labels, 
                                                        side = side, at = at, line = -0.5, col = col.lab, 
                                                        cex = cex.axis, font = font.lab, ...)
        if (is.null(x.ticklabs)) 
          x.ticklabs <- format(i.x * x.scal)
        mytext(x.ticklabs, side = 1, at = i.x)
        if (is.null(z.ticklabs)) 
          z.ticklabs <- format(i.z * z.scal)
        mytext(z.ticklabs, side = if (angle.1) 
          4
          else 2, at = i.z, adj = if (0 < las && las < 
                                      3) 
            1
          else NA)
        temp <- if (angle > 2) 
          rev(i.y)
        else i.y
        if (is.null(y.ticklabs)) 
          y.ticklabs <- format(y.prty)
        else if (angle > 2) 
          y.ticklabs <- rev(y.ticklabs)
        text(i.y * yx.f + xx[1], i.y * yz.f + z.min, 
             y.ticklabs, pos = if (angle.1) 
               2
             else 4, offset = 1, col = col.lab, cex = cex.axis/par("cex"), 
             font = font.lab)
      }
    }
    mytext2 <- function(lab, side, line, at) mtext(lab, 
                                                   side = side, line = line, at = at, col = col.lab, 
                                                   cex = cex.lab, font = font.axis, las = 0)
    lines(c(x.min, x.max), c(z.min, z.min), col = col.axis, 
          lty = lty.axis)
    mytext2(xlab, 1, line = 1.5, at = mean(x.range))
    lines(xx[1] + c(0, y.max * yx.f), c(z.min, y.max * yz.f + 
                                          z.min), col = col.axis, lty = lty.axis)
    mytext2(ylab, if (angle.1) 
      2
      else 4, line = 0.5, at = z.min + y.max * yz.f)
    lines(xx[c(2, 2)], c(z.min, z.max), col = col.axis, 
          lty = lty.axis)
    mytext2(zlab, if (angle.1) 
      4
      else 2, line = 1.5, at = mean(z.range))
    if (box) {
      if (is.null(lty.hide)) 
        lty.hide <- lty.axis
      temp <- yx.f * y.max
      temp1 <- yz.f * y.max
      lines(c(x.min + temp, x.max + temp), c(z.min + temp1, 
                                             z.min + temp1), col = col.axis, lty = lty.hide)
      lines(c(x.min + temp, x.max + temp), c(temp1 + z.max, 
                                             temp1 + z.max), col = col.axis, lty = lty.axis)
      temp <- c(0, y.max * yx.f)
      temp1 <- c(0, y.max * yz.f)
      lines(temp + xx[2], temp1 + z.min, col = col.axis, 
            lty = lty.hide)
      lines(temp + x.min, temp1 + z.max, col = col.axis, 
            lty = lty.axis)
      temp <- yx.f * y.max
      temp1 <- yz.f * y.max
      lines(c(temp + x.min, temp + x.min), c(z.min + temp1, 
                                             z.max + temp1), col = col.axis, lty = if (!angle.2) 
                                               lty.hide
            else lty.axis)
      lines(c(x.max + temp, x.max + temp), c(z.min + temp1, 
                                             z.max + temp1), col = col.axis, lty = if (angle.2) 
                                               lty.hide
            else lty.axis)
    }
  }
  x <- dat$x + (dat$y * yx.f)
  z <- dat$z + (dat$y * yz.f)
  col <- as.character(dat$col)
  if (type == "h") {
    z2 <- dat$y * yz.f + z.min
    segments(x, z, x, z2, col = col, cex = cex.symbols, 
             lty = lty.hplot, ...)
    points(x, z, type = "p", col = col, pch = pch, bg = bg, 
           cex = cex.symbols, ...)
  }
  else points(x, z, type = type, col = col, pch = pch, bg = bg, 
              cex = cex.symbols, ...)
  if (axis && box) {
    lines(c(x.min, x.max), c(z.max, z.max), col = col.axis, 
          lty = lty.axis)
    lines(c(0, y.max * yx.f) + x.max, c(0, y.max * yz.f) + 
            z.max, col = col.axis, lty = lty.axis)
    lines(xx[c(1, 1)], c(z.min, z.max), col = col.axis, 
          lty = lty.axis)
  }
  ob <- ls()
  rm(list = ob[!ob %in% c("angle", "mar", "usr", "x.scal", 
                          "y.scal", "z.scal", "yx.f", "yz.f", "y.add", "z.min", 
                          "z.max", "x.min", "x.max", "y.max", "x.prty", "y.prty", 
                          "z.prty")])
  rm(ob)
  invisible(list(xyz.convert = function(x, y = NULL, z = NULL) {
    xyz <- xyz.coords(x, y, z)
    if (angle > 2) {
      temp <- xyz$x
      xyz$x <- xyz$y
      xyz$y <- temp
    }
    y <- (xyz$y - y.add)/y.scal
    return(list(x = xyz$x/x.scal + yx.f * y, y = xyz$z/z.scal + 
                  yz.f * y))
  }, points3d = function(x, y = NULL, z = NULL, type = "p", 
                         ...) {
    xyz <- xyz.coords(x, y, z)
    if (angle > 2) {
      temp <- xyz$x
      xyz$x <- xyz$y
      xyz$y <- temp
    }
    y2 <- (xyz$y - y.add)/y.scal
    x <- xyz$x/x.scal + yx.f * y2
    y <- xyz$z/z.scal + yz.f * y2
    mem.par <- par(mar = mar, usr = usr)
    on.exit(par(mem.par))
    if (type == "h") {
      y2 <- z.min + yz.f * y2
      segments(x, y, x, y2, ...)
      points(x, y, type = "p", ...)
    } else points(x, y, type = type, ...)
  }, plane3d = function(Intercept, x.coef = NULL, y.coef = NULL, 
                        lty = "dashed", lty.box = NULL, ...) {
    if (!is.atomic(Intercept) && !is.null(coef(Intercept))) Intercept <- coef(Intercept)
    if (is.null(lty.box)) lty.box <- lty
    if (is.null(x.coef) && length(Intercept) == 3) {
      x.coef <- Intercept[if (angle > 2) 3 else 2]
      y.coef <- Intercept[if (angle > 2) 2 else 3]
      Intercept <- Intercept[1]
    }
    mem.par <- par(mar = mar, usr = usr)
    on.exit(par(mem.par))
    x <- x.min:x.max
    ltya <- c(lty.box, rep(lty, length(x) - 2), lty.box)
    x.coef <- x.coef * x.scal
    z1 <- (Intercept + x * x.coef + y.add * y.coef)/z.scal
    z2 <- (Intercept + x * x.coef + (y.max * y.scal + y.add) * 
             y.coef)/z.scal
    segments(x, z1, x + y.max * yx.f, z2 + yz.f * y.max, 
             lty = ltya, ...)
    y <- 0:y.max
    ltya <- c(lty.box, rep(lty, length(y) - 2), lty.box)
    y.coef <- (y * y.scal + y.add) * y.coef
    z1 <- (Intercept + x.min * x.coef + y.coef)/z.scal
    z2 <- (Intercept + x.max * x.coef + y.coef)/z.scal
    segments(x.min + y * yx.f, z1 + y * yz.f, x.max + y * 
               yx.f, z2 + y * yz.f, lty = ltya, ...)
  }, box3d = function(...) {
    mem.par <- par(mar = mar, usr = usr)
    on.exit(par(mem.par))
    lines(c(x.min, x.max), c(z.max, z.max), ...)
    lines(c(0, y.max * yx.f) + x.max, c(0, y.max * yz.f) + 
            z.max, ...)
    lines(c(0, y.max * yx.f) + x.min, c(0, y.max * yz.f) + 
            z.max, ...)
    lines(c(x.max, x.max), c(z.min, z.max), ...)
    lines(c(x.min, x.min), c(z.min, z.max), ...)
    lines(c(x.min, x.max), c(z.min, z.min), ...)
  }))
}

scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6){
  the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, distance_lat = distance_lat, distance_legend = distance_legend, dist_unit = dist_unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour)
  
  # Legend
  scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=""), x = the_scale_bar$legend[,"long"], y = the_scale_bar$legend[,"lat"], size = legend_size, colour = legend_colour)
  
  res <- list(rectangle1, rectangle2, scale_bar_legend)
  
  if(orientation){# Add an arrow pointing North
    coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length, distance = arrow_distance, dist_unit = dist_unit)
    arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"], y = coords_arrow$coords_n[1,"y"], size = arrow_north_size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}
create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
  # First rectangle
  bottom_right <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon, dist.units = dist_units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_lat, dist.units = dist_units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottom_right[1,"long"], bottom_right[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottom_right2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon*2, dist.units = dist_units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottom_right[1,"long"], bottom_right[1,"long"], bottom_right2[1,"long"], bottom_right2[1,"long"], bottom_right[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_legend, dist.units = dist_units, model = "WGS84")
  on_top2 <- on_top3 <- on_top
  on_top2[1,"long"] <- bottom_right[1,"long"]
  on_top3[1,"long"] <- bottom_right2[1,"long"]
  
  legend <- rbind(on_top, on_top2, on_top3)
  legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}
create_orientation_arrow <- function(scale_bar, length, distance = 1, dist_units = "km"){
  lon <- scale_bar$rectangle2[1,1]
  lat <- scale_bar$rectangle2[1,2]
  
  # Bottom point of the arrow
  beg_point <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist_units, model = "WGS84")
  lon <- beg_point[1,"long"]
  lat <- beg_point[1,"lat"]
  
  # Let us create the endpoint
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist_units, model = "WGS84")
  
  left_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 225, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  right_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 135, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  res <- rbind(
    cbind(x = lon, y = lat, xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = left_arrow[1,"long"], y = left_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = right_arrow[1,"long"], y = right_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]))
  
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  
  # Coordinates from which "N" will be plotted
  coords_n <- cbind(x = lon, y = (lat + on_top[1,"lat"])/2)
  
  return(list(res = res, coords_n = coords_n))
}


# Extract the lake shape file that was originally developed from Zach Munger. 
lakeShape = readOGR(dsn=path.expand("./Materials for R mapping/FCR"), layer="506_9")
inflow = readOGR(dsn=path.expand("./Materials for R mapping/FCR"), layer="inf2")
em = readOGR(dsn=path.expand("./Materials for R mapping/FCR"), layer="EM_line")
hox = readOGR(dsn=path.expand("./Materials for R mapping/FCR"), layer="sss_system")

# Extract the lake shape file that was originally developed from Zach Munger. 
lakeShapebvr = readOGR(dsn=path.expand("./Materials for R mapping/BVR"), layer="0_2mpoly12apr18")
inflowpipe = readOGR(dsn=path.expand("./Materials for R mapping/BVR"), layer="inflow pipe")

# The data are in Standard Easting-Northing Units, this short line of code transforms our units into Lat-Long
# If you are unsure about this, follow the link below for a detailed workflow about spatial transformations.
# http://gis.stackexchange.com/questions/142156/r-how-to-get-latitudes-and-longitudes-from-a-rasterlayer

#FCR
fcr1 <- spTransform(lakeShape, CRS("+init=epsg:4326"))
bvr1 <- spTransform(lakeShapebvr, CRS("+init=epsg:4326"))
inf <- spTransform(inflow, CRS("+init=epsg:4326"))
emer <- spTransform(em, CRS("+init=epsg:4326"))
sss <- spTransform(hox, CRS("+init=epsg:4326"))
pipe <- spTransform(inflowpipe, CRS("+init=epsg:4326"))
#BVR






# At this point, fcr is a Formal Class Spatial Polygon dataframe. In order to plot it in 3D space we need to 
# convert it into a 2D dataframe with a corresponding depth. This is done using the "fortify" function.
fcr1 <- fortify(fcr1)
bvr1 <- fortify(bvr1)
inf <- fortify(inf)
emer <- fortify(emer)
sss <- fortify(sss)
pipe <- fortify(pipe)
# Unfortunately, this is a necessary subsetting in order to get a nice clear perimeter figure of FCR. 
# If the whole dataset is plotted, there are weird lines that emerge fromt he shape file. It starts 
# after row 1022, and I have absolutely no idea why it does that. 
# This subsets out the first 1022 rows from the fortified dataframe.

fcr1 <- fcr1[1:1024,]
inf <- inf[2:45,]



fcr1$depth <- 0
fcr1$Depths <- "Falling Creek"
bvr1$depth <- 0
bvr1$Depths <- "Beaverdam"
inf$depth <- 0
inf$Depths <- "Inflow"
emer$depth <- 3
emer$Depths <- "EM"
sss$depth <- 3
sss$Depths <- "HOx"
pipe$depth <- 0
pipe$Depths <- "Inflow Pipe"


bvr1 <- bvr1[,-4]
fcr1 <- fcr1[,-4]
fcr_map <- rbind(fcr1, bvr1, deparse.level = 1)
fcr_add <- rbind(inf, emer, sss, pipe, deparse.level = 1)

a = ggplot(fcr_map, aes(long, lat, fill=Depths)) +
  geom_polygon()+
  geom_path(color="black", lwd = 1.7) +
  coord_equal() +
  #geom_point(data = traps, aes(x = lon, y = lat), pch = 21, bg = "red", col = "black", cex = 5)+
  labs(x = "Longitude", y = "Latitude")+
  scale_fill_brewer()+
  theme(axis.title = element_text(size = 25, color = "black"))+
  theme(axis.text = element_text(size = 25, color = "black"))+
  theme_bw()

b = a +
  geom_line(data = inf, aes(x = long, y = lat), lwd = 1, color = "dodgerblue1")+
  geom_line(data = pipe, aes(x = long, y = lat), lwd = 1, color = "dodgerblue4", lty = "dashed")+
  geom_point(data = sites, aes(x = lon, y = lat, size = PO4_ppb ), pch = 21, bg = "purple", col = "black")+
  scale_size_continuous(limits=c(0,25))

c = b +
 # north(fcr_map, symbol = 3, scale = 0.15, location = "topleft") +
  scalebar(fcr_map, dist = 0.25, dist_unit = "km",
           transform = TRUE, model = "WGS84")

c = c + ggtitle('May') + theme(plot.title = element_text(size = 40))


pdf("./Materials for R mapping/nutrient_mapping/30May19_RCC_PO4.pdf", width = 8, height = 6)
c
dev.off()

png("./Materials for R mapping/nutrient_mapping/30May19_RCC_PO4.png", width = 1100, height = 800)
c
dev.off()



# ggplot(fcr1, aes(long, lat, fill=Depths)) +
#   geom_polygon()+
#   geom_path(color="black", lwd = 1.7) +
#   coord_equal() +
#   xlim(-79.8391, -79.836)+
#   ylim(37.3025,37.305)+
#   #geom_point(data = traps, aes(x = lon, y = lat), pch = 21, bg = "red", col = "black", cex = 5)+
#   labs(x = "Longitude", y = "Latitude")+
#   scale_fill_brewer()+
#   theme(axis.title = element_text(size = 12, color = "black"))+
#   theme(axis.text = element_text(size = 12, color = "black"))+
#   theme_bw()+
#   theme(legend.position = "none")+
#   geom_line(data = sss, aes(x = long, y = lat), lwd = 1, color = "dodgerblue4")+
#   geom_line(data = emer, aes(x = long, y = lat), lwd = 1, color = "dodgerblue4", lty = "dashed")
