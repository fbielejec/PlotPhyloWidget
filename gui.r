###########
#---GUI---#
###########

GGplotHandler <- function(h,...) {
  
  x    <- read.nexus(h$file)
  phylo = fortify.phylo(x)
  phyloLabels = label.phylo(x)
  
  reverseAxis = ifelse(
    svalue(select_axis_alignment) == "Reverse axis",
    TRUE,
    FALSE
    )
  
  if(reverseAxis) {
    
    phylo$x = max(phylo$x) - phylo$x
    phylo$xend = max(phylo$x) - phylo$xend
    phyloLabels$x = max(phyloLabels$x) - phyloLabels$x
    
  } 
  
  
  MAXX <- Maxx.Phylo(x)
  plot(1, col = "white", xlab = "", ylab = "", main = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0, MAXX), axes = F)
  MAXSTRING <- max(strwidth(x$tip.label))
  xlim <- MAXX + MAXSTRING
  
  p <- ggplot(data = phylo)
  p <- p + geom_segment2(aes(x = x, y = y, xend = xend, yend = yend), colour = "blue", alpha = 1)
  p <- p + geom_text(data = phyloLabels, aes(x = x, y = y, label = label), hjust = 0, family = 3, vjust = 0.5, size = 3) + xlim(0, xlim)
  theme <- theme_update(
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(),
    axis.title.x = theme_blank(),
    axis.title.y = theme_blank(),
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

select_axis_alignment <- gdroplist(c("Normal axis", "Reverse axis"))#, container = tmp)

menulist = list(
  new = gaction("New", icon = "new", handler = function(h, ...) add(nb, ggraphics(), label = "plot" ) ),
  open = list(icon = "open", handler = ReadNex ),
  close = gaction("Close", icon ="delete", handler = function(h,...)  dispose(nb)  ),
  save = gaction("Save", icon ="save", handler =  SavePlot ),   
  separator = gseparator(),
  reverse = select_axis_alignment,
  separator = gseparator(),
  quit = list(icon="quit", handler = function(h,...)  dispose(win) )
)


win <- gwindow("PlotPhylo Widget")
group <- ggroup(horizontal = FALSE, container=win)
nb <- gnotebook(container = group, expand=TRUE)
firstplot <- ggraphics(container = nb, label="plot")
gtoolbar(menulist, cont=TRUE, container=win, style="both")





