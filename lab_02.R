# plot mds of data using various distance methods
jf.plot.mds <- function(data,
                        method = c("euc", "man", "can", "max"),
                        scale.factor = 1.1,
                        row.labels = TRUE) {
  
  method <- match.arg(method, c("euclidean",
                                "maximum",
                                "manhattan",
                                "canberra",
                                "binary",
                                "minkowski",
                                "correlation"),
                      several.ok = TRUE)
  
  # check if methods are possible with data
  if (any("binary" %in% method) && all(apply(data, 2, is.numeric))) {
    stop("Method 'binary' does not work with all numerical data.")
  } 
  
  if(any("correlation" %in% method) && any(!apply(data, 2, is.numeric))) {
    stop("Method 'correlation' does not work as some columns are not numeric.")
  } 
  
  d.scale <- list()
  lims <- list()
  
  layout_vec <- c(round(sqrt(length(method))), ceiling(sqrt(length(method))))
  
  op <- par(mfrow = layout_vec)
  
  for (m in method) {
    if (m == "correlation") {
      # matrix transposed as internal correlation works colwise
      d.scale[[m]] <- cmdscale(d = jf.cor.dist(t(as.matrix(data))))
    } else {
      d.scale[[m]] <- cmdscale(d = dist(data, method = m))
    }
    
    lims[[m]] <- range(d.scale[[m]])
    
    if (all(jf.is.positive(lims[[m]]))) {
      lims[[m]][1] <- lims[[m]][1] - lims[[m]][1] * (scale.factor - 1)
      lims[[m]][2] <- lims[[m]][2] * scale.factor
    } else if (all(!jf.is.positive(lims[[m]]))) {
      lims[[m]][1] <- lims[[m]][1] * scale.factor
      lims[[m]][2] <- lims[[m]][2] + lims[[m]][2] * (scale.factor - 1)
    } else {
      lims[[m]] <- lims[[m]] * scale.factor
    }
    
    plot(d.scale[[m]], xlim = lims[[m]], ylim = lims[[m]],
         type = "n", ann = FALSE)
    points(d.scale[[m]], pch = 18)
    title(m)
    
    if (row.labels){
      text(d.scale[[m]], labels = row.names(data), adj = c(-0.3, -0.3))
    }
  }
  
  # reset par
  par(op)
}

jf.cor.dist <- function(x) {
  
  if (! is.matrix(x)) {
    warning("Coercing data to matrix...")
  }
  
  x <- as.matrix(x)
  
  return(1 - abs(cor(x)))
}

jf.is.positive <- function(x) {
  return(x / abs(x) == 1)
}


# main part
if (sys.nframe() == 0) {
  cancer <- read.table("data/ML-Data/fraumeni1960-cancer.tab",
                       header = TRUE,
                       row.names = 1)
  
  # normal plot
  jf.plot.mds(cancer, method = c("euc", "man", "can", "cor"))
  
  # plot with transposed matrix
  # will show the variable distances
  jf.plot.mds(t(as.matrix(cancer)), c("euc", "man", "can", "cor"))
}
