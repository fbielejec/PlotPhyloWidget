#########################
#---GGPLOT2 FUNCTIONS---#
#########################

#####################
#---GEOM_SEGMENT2---#
#####################
GeomSegment2 <- proto(ggplot2:::GeomSegment, {
  objname <- "geom_segment2"
  
  draw <- function(., data, scales, coordinates, arrow = NULL, ...) {
    if (is.linear(coordinates)) {
      return(with(coord_transform(coordinates, data, scales),
                  
                  segmentsGrob(x, y, xend, yend, default.units = "native",
                               gp = gpar(col = alpha(colour, alpha), lwd = size * .pt,
                                         lty = linetype, lineend = "round"),
                               arrow = arrow)
      ))
    }
  }})

geom_segment2 <- function(mapping = NULL, data = NULL, stat =
  "identity", position = "identity", arrow = NULL, ...)  {
  GeomSegment2$new(mapping = mapping, data = data, stat = stat,
                   position = position, arrow = arrow, ...)
}

#####################
#---FORTIFY.PHYLO---#
#####################
fortify.phylo <- function(phylo) {

Ntip <- length(phylo$tip.label)
Nnode <- phylo$Nnode
Nedge <- dim(phylo$edge)[1]
z <- reorder(phylo, order = "pruningwise")
yy <- numeric(Ntip + Nnode)
TIPS <- phylo$edge[phylo$edge[, 2] <= Ntip, 2]
yy[TIPS] <- 1:Ntip


ans1 <- .C("node_height_clado", as.integer(Ntip), as.integer(Nnode), as.integer(z$edge[, 1]), 
	as.integer(z$edge[,2]), as.integer(Nedge), double(Ntip + Nnode), as.double(yy), 
	DUP = FALSE, PACKAGE = "ape")

	yy <- ans1[[7]]

ans2 <- .C("node_depth_edgelength", as.integer(Ntip), as.integer(Nnode), as.integer(z$edge[, 1]), 
	as.integer(z$edge[, 2]), as.integer(Nedge), as.double(z$edge.length), double(Ntip + Nnode), 
	DUP = FALSE, PACKAGE = "ape")

	xx <- ans2[[7]]

    edge <- phylo$edge
    nodes <- (Ntip + 1):(Ntip + Nnode)
    x0v <- xx[nodes]
    y0v <- y1v <- numeric(Nnode)
    NodeInEdge1 <- vector("list", Nnode)

    for (i in nodes) {
        ii <- i - Ntip
        j <- NodeInEdge1[[ii]] <- which(edge[, 1] == i)
        tmp <- range(yy[edge[j, 2]])
        y0v[ii] <- tmp[1]
        y1v[ii] <- tmp[2] }

    x0h <- xx[edge[, 1]]
    x1h <- xx[edge[, 2]]
    y0h <- yy[edge[, 2]]

lineh <- data.frame(x=x1h, y=y0h, xend=x0h, yend=y0h)
linev <- data.frame(x=x0v, y=y1v, xend=x0v, yend=y0v)
x <- rbind(linev, lineh) 

return(x)
}


###################
#---LABEL.PHYLO---#
###################
label.phylo <- function(phylo) {

Ntip <- length(phylo$tip.label)
Nnode <- phylo$Nnode
Nedge <- dim(phylo$edge)[1]
z <- reorder(phylo, order = "pruningwise")
yy <- numeric(Ntip + Nnode)
TIPS <- phylo$edge[phylo$edge[, 2] <= Ntip, 2]
yy[TIPS] <- 1:Ntip

ans1 <- .C("node_height_clado", as.integer(Ntip), as.integer(Nnode), as.integer(z$edge[, 1]), 
	as.integer(z$edge[,2]), as.integer(Nedge), double(Ntip + Nnode), as.double(yy), 
	DUP = FALSE, PACKAGE = "ape")

	yy <- ans1[[7]]

ans2 <- .C("node_depth_edgelength", as.integer(Ntip), as.integer(Nnode), as.integer(z$edge[, 1]), 
	as.integer(z$edge[, 2]), as.integer(Nedge), as.double(z$edge.length), double(Ntip + Nnode), 
	DUP = FALSE, PACKAGE = "ape")

	xx <- ans2[[7]]

data.frame(x = xx[1 : Ntip], y = yy[1 : Ntip], label = phylo$tip.label)
}

##################
#---Maxx.Phylo---#
##################
# Maxx.Phylo <- function(phylo) {
# 
# Ntip <- length(phylo$tip.label)
# Nnode <- phylo$Nnode
# Nedge <- dim(phylo$edge)[1]
# z <- reorder(phylo, order = "pruningwise")
# 
# ans2 <- .C("node_depth_edgelength", as.integer(Ntip), as.integer(Nnode), as.integer(z$edge[, 1]), 
# 	as.integer(z$edge[, 2]), as.integer(Nedge), as.double(z$edge.length), double(Ntip + Nnode), 
# 	DUP = FALSE, PACKAGE = "ape")
# 
# 	xx <- ans2[[7]]
# 	MAXX <- round(max(xx))
# 	return(MAXX)
# }
