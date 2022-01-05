geomorph.def <- data.frame(
  num_lf = 1:10,
  id_lf = c("PK", "RI", "SH", "SP", "SL", "FS", "FL", "HL", "VL", "PT"),
  name_en = c("Peak", "Ridge", "Shoulder", "Spur", "Slope", "Footslope", "Flat", "Hollow", "Valley", "Pit"),
  colour = c("magenta", "red", "orange", "yellow", "grey40",  "grey70", "grey90", "skyblue1", "dodgerblue", "royalblue3"),
  stringsAsFactors = F
)

geomorph.lut <- data.frame(
  V0 = c("FL", "FL", "FL", "SH", "SH", "RI", "RI", "RI", "PK"),
  V1 = c("FL", "FL", "SH", "SH", "SH", "RI", "RI", "RI", NA),
  V2 = c("FL", "FS", "SL", "SL", "SP", "SP", "RI", NA, NA),
  V3 = c("FS", "FS", "SL", "SL", "SL", "SP", NA, NA, NA),
  V4 = c("FS", "FS", "HL", "SL", "SL", NA, NA, NA, NA),
  V5 = c("VL", "VL", "HL", "HL", NA, NA, NA, NA, NA),
  V6 = c("VL", "VL", "VL", NA, NA, NA, NA, NA, NA),
  V7 = c("VL", "VL", NA, NA, NA, NA, NA, NA, NA),
  V8 = c("PT", NA, NA, NA, NA, NA, NA, NA, NA)
)
geomorph.lut <- as.matrix(geomorph.lut)
m.lut.num <- matrix(match(geomorph.lut, geomorph.def$id_lf), nrow = nrow(geomorph.lut)) 

# Definitions
thresh <- 1 # Flatness threshold [degrees] used in geomporph()
w <- 13 # width/height of focal window #0.001° = 111m
# 29 : 700m de fenêtre

# TODO : à essayer avec w = 13 (12 * 25 = 300m)

# Produce raster
r.volcano <- MNT25m

# Focal function
focal.lf <- function(x) geomorph(x, flatness.thresh = thresh, ncell = ncell(r.volcano), res = res(r.volcano)[1])

# Focal window
focal.matrix <- matrix(1, nrow = w, ncol = w)

# Run landform classification
r.volcano.lf <- suppressWarnings(focal(r.volcano, fun = focal.lf, w = focal.matrix, pad = T, padValue = NA))

r.volcano.lf

writeRaster(r.volcano.lf, paste0(dossier_sauv,"/landform_10classes_25m_13neigh.tif"))
plot(r.volcano.lf)

# Melt raster to data frame
df.lf <- r.volcano.lf %>% raster::flip(direction = 2) %>% as.data.frame(xy = T)

pl1 <- ggplot(df.lf, aes(x, y, fill = factor(layer, geomorph.def$num_lf))) + 
  geom_raster() +
  scale_fill_manual("Landform", values = geomorph.def$colour, labels = geomorph.def$name, drop = F) +
  coord_cartesian(expand = F) + 
  theme(plot.background = element_rect(colour = "black", size = .3)) +
  labs(x = NULL, y = NULL, title = "Volcano Landform Classes")

pl2 <- ggplot(volcano %>% reshape2::melt(), aes(Var1, Var2, fill = value)) + 
  geom_raster() +
  scale_fill_distiller("Elevation", palette = "Spectral") + 
  coord_cartesian(expand = F) + 
  theme(plot.background = element_rect(colour = "black", size = .3)) +
  labs(x = NULL, y = NULL, title = "Volcano Elevation")

geomorph <- function(r, flatness.thresh = NA, res = NA, ncell = NA, ...){
  #' @description Note, that no performance optimisation has been done to this function, yet.
  #' @author M. Sänger 2018
  #' @source Jasiewicz, Stepinkski 2013
  #' @param r output (vector) from raster::focal function
  #' @param m.lut.num (global variable) look-up table to derive landform class from ternary pattern
  #' @param res resolution, same unit as values in r
  
  # Breaks for flatness threshold
  brks <- c(-Inf, -flatness.thresh, flatness.thresh, Inf)
  brks.ind <- c(-1, 0, 1)
  
  # Create matrix from incoming vector r
  size = sqrt(length(r))
  m <- matrix(r, nrow = size)
  
  # Distance from central point to edget (number of cells)
  mid <- ceiling(size/2)
  
  # Matrix of all vectors from the central point to the octants
  oct <- rbind(
    ne = cbind(mid:size, mid:size),
    e = cbind(mid:size, mid),
    se = cbind(mid:size, mid:1),
    s = cbind(mid, mid:1),
    sw = cbind(mid:1, mid:1),
    w = cbind(mid:1, mid),
    nw = cbind(mid:1, mid:size),
    n = cbind(mid, mid:size)
  )
  
  # Coordinates and cell distance (sqrt(2) for diagonals)
  oct.vector <- m[oct]
  cell.scaling <- rep(c(sqrt(2), 1), 4) # Horizontal cell distance in all 8 directions
  cell.size <- res * cell.scaling
  
  # Matrix octants vs. cell values
  m1 <- matrix(oct.vector, nrow = 8, byrow = T)
  
  # z diff from central point
  m.diff <-  m1[, -1] - m1[, 1]
  
  # Calculate slope angle and transform to degrees
  m.slope <- atan(m.diff/(cell.size * 1:ncol(m.diff)))
  m.angle <- m.slope * 180/pi
  
  # Calculate zenith and nadir angles for each octant
  nadir <- 90 + apply(m.angle, 1, min, na.rm = T)
  zenith <- 90 - apply(m.angle, 1, max, na.rm = T)
  
  # Derive ternary pattern
  ternary.pattern <- brks.ind[findInterval(nadir - zenith, brks)]
  
  plus.ind <- length(which(ternary.pattern == 1))
  neg.ind <- length(which(ternary.pattern == -1))
  
  # Look up ternarity pattern and assign landform class
  m.lut.num[neg.ind + 1, plus.ind + 1]  
}
