###########
#---GUI---#
###########

GGplotHandler <- function(h,...) {
  
  x    <- read.nexus(h$file)
  phylo = fortify.phylo(x)
  phyloLabels = label.phylo(x)
  
#   reverseAxis = ifelse(
#     svalue(select_axis_alignment) == "Reverse axis",
#     TRUE,
#     FALSE
#     )
  
  reverseAxis = svalue(reverseCheckbox)
  
  if(reverseAxis) {
    
    phylo$x = max(phylo$x) - phylo$x
    phylo$xend = max(phylo$x) - phylo$xend
    phyloLabels$x = max(phyloLabels$x) - phyloLabels$x
    
  } 
  
  
  MAXX <- max(phylo$x)#Maxx.Phylo(x)
  plot(1, col = "white", xlab = "", ylab = "", main = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0, MAXX), axes = F)
  MAXSTRING <- max(strwidth(x$tip.label))
  xlim <- MAXX + MAXSTRING
  
  p <- ggplot(data = phylo)
  p <- p + geom_segment2(aes(x = x, y = y, xend = xend, yend = yend), colour = "blue", alpha = 1)
  p <- p + geom_text(data = phyloLabels, aes(x = x, y = y, label = label), hjust = 0, family = 3, vjust = 0.5, size = 3) + xlim(0, xlim)
  theme <- theme_update(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_rect(fill = NA, size = .5, linetype = NULL, colour = NA),
    legend.key = element_rect(colour = "grey80"),
    legend.position = "top",
    legend.justification = "center",
    panel.background = element_rect(size = 1, fill = "white", colour = NA),
    panel.border = element_rect(fill = NA, colour = "grey50"),
    panel.grid.major = element_line(colour = "grey90", size = 0.2),
    panel.grid.minor = element_line(colour = "grey98", size = 0.5),
    strip.background = element_rect(fill = "grey80", colour = "grey50"),
    plot.background = element_rect(colour = NA),
    legend.position = "none"
  )
  
  p <- p + theme_set(theme)
  
  if(reverseAxis) {
    p <- p + scale_x_reverse()
  }
  
  print(p)
}

ReadNex <- function(h, ...) {
  gfile(
    text = "Upload nexus file...",
    type = "open",      
    filter = list(
      "Newick files" = list(patterns = c("*.nex", "*.tree", "*.tre")),
      "All files" = list(patterns = c("*"))
    ),
    handler = GGplotHandler
  )
}

get.eps <- function(h, ...) {
  local({
    dev.set (2)
    dev.print(device = postscript, file = h$file, onefile = FALSE, paper = "special", horizontal = FALSE);
  })
}


SavePlot <- function(h, ...) {
  gfile(text = "Save as...", type = "save", initialfilename = "Rplot.eps", handler = get.eps, filter = 
    list(
      "All files" = list(patterns = c("*")), 
      "eps files" = list(patterns = c("*.eps")) )
  )
}

# select_axis_alignment <- gdroplist(c("Normal axis", "Reverse axis"))#, container = tmp)

reverseCheckbox = gcheckbox("Reverse axis")

menulist = list(
  new = gaction("New", icon = "new", handler = function(h, ...) add(nb, ggraphics(), label = "plot" ) ),
  open = list(icon = "open", handler = ReadNex ),
  close = gaction("Close", icon ="delete", handler = function(h,...)  dispose(nb)  ),
  save = gaction("Save", icon ="save", handler =  SavePlot ),   
  separator = gseparator(),
#   reverse = select_axis_alignment,
  reverse = reverseCheckbox,
  separator = gseparator(),
  quit = list(icon="quit", handler = function(h,...)  dispose(win) )
)


win <- gwindow("PlotPhylo Widget")
group <- ggroup(horizontal = FALSE, container=win)
nb <- gnotebook(container = group, expand=TRUE)
firstplot <- ggraphics(container = nb, label="plot")
gtoolbar(menulist, cont=TRUE, container=win, style="both")





