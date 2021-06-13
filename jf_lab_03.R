

# task 1-2 ----------------------------------------------------------------


jf.plot.mds.clust <- function(x,
                              clust.method = "complete",
                              dist.method = c("euc", "cor")) {
  if (!require("cluster", quietly = TRUE)) {
    stop("Package 'cluster' required for this function.")
  }
  
  if (!file.exists("jf_lab_02.R")){
    "Script 'jf_lab_02' required for this function."
    }
  
  source("jf_lab_02.R")
  
  clust.method <- match.arg(clust.method, c("complete",
                                            "single",
                                            "average",
                                            "diana"))
  
  dist.method <- match.arg(dist.method, c("euclidean",
                                "maximum",
                                "manhattan",
                                "canberra",
                                "binary",
                                "minkowski",
                                "correlation"),
                      several.ok = TRUE)
  
  
  # save original graphics parameters and set new ones
  opar <- par(mfrow = c(length(dist.method), 2), omi = c(0,0.5,0.5,0))
  
  for (m in clust.method) {
    
    jf.plot.mds(x, method = m)
    
  }
  
  
  mtext("Euclidean", side = 3, line = 2)
  
  mtext("MDS", side = 2, line = 2)
  
  jf.plot.mds(x, method = "euc", grid = FALSE)
  
  jf.plot.mds(x, method = "cor", grid = FALSE)
  
  mtext("Correlation", side = 3, line = 2)
  
  plot(hclust(dist(x, "euc")), ylab = "", main = "")
  
  mtext("CL", side = 2, line = 2)
  
  plot(hclust(as.dist(jf.cor.dist(t(as.matrix(x))))),
       ylab = "", main = "")
  
  par(opar)
}



# task 3 ------------------------------------------------------------------

jf.randCols <- function(x) {
  x <- apply(x, 2, sample, size = nrow(x))
  return(x)
}


# Driver code -------------------------------------------------------------

if (sys.nframe() == 0) {
  
  cancer <- read.table("data/ML-Data/fraumeni1960-cancer.tab",
                       header = TRUE,
                       row.names = 1)
  
  jf.plot.mds.clust(cancer)
  
  jf.plot.mds.clust(t(as.matrix(cancer)))
  
  jf.plot.mds.clust(scale(cancer))
  
    jf.plot.mds.clust(scale(t(as.matrix(cancer))))
  
}
