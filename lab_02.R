# plot mds of data using various distance methods
jf.plot.mds <- function(data,
                        method = c("euc", "man", "can", "max")) {
  
  method <- match.arg(method, c("euclidean",
                                "maximum",
                                "manhattan",
                                "canberra",
                                "binary",
                                "minkowski"),
                      several.ok = TRUE)
  
  if (any("binary" %in% method) && all(apply(data, 2, is.numeric))) {
    stop("Method 'binary' does not work with all numerical data.")
  }
  
  scale_factor <- 1.1
  
  d.scale <- list()
  lims <- list()
  
  layout_vec <- c(round(sqrt(length(method))), ceiling(sqrt(length(method))))
  
  par(mfrow = layout_vec)
  
  for (m in method) {
    d.scale[[m]] <- cmdscale(d = dist(data, method = m))
    
    # TODO: Scaling that works regardless of sign
    lims[[m]] <- range(d.scale[[m]]) * scale_factor
    
    plot(d.scale[[m]], xlim = lims[[m]], ylim = lims[[m]],
         type = "n", ann = FALSE)
    points(d.scale[[m]], pch = 18)
    text(d.scale[[m]], labels = row.names(data), adj = c(-0.3, -0.3))
    title(m)
  }
  
}

# main part
if (sys.nframe() == 0) {
  cancer <- read.table("data/ML-Data/fraumeni1960-cancer.tab",
                       header = TRUE,
                       row.names = 1)
  
  jf.plot.mds(cancer, method = c("euc", "min", "man", "can"))
}
