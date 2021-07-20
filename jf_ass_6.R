## ----Setup, include=FALSE----------------------------------------------------------------------------------------

knitr::opts_chunk$set(fig.height = 4)



## ----Fucntions---------------------------------------------------------------------------------------------------
checkVecs <- function(x, y) {
  if (length(x) != length(y)) {
    stop(paste0("Input vectors are of different lengths. (", length(x), ", ",
                length(y), ")"))
  }
}

makeData <- function(delta = 1, n_p = 200, n_n = 200) {
  delta <-  delta # shift of score distribution of positive vs. negative samples
  score <-  rnorm(n_n) # random score values
  score <-  c(score, rnorm(n_p) + delta)
  class <-  c(rep(0, n_n), rep(1, n_p)) # class assignment
  
  return(list(score = score, class = class))
}

confusion <- function(score, class, x) {
  checkVecs(score, class)
  
  score_dich <- as.numeric(score > x)
  
  pos_ind <- which(score_dich == 1)
  neg_ind <- which(score_dich == 0)
  
  tp <- sum(score_dich[pos_ind] == class[pos_ind])
  tn <- sum(score_dich[neg_ind] == class[neg_ind])
  
  fp <- sum(score_dich[pos_ind] != class[pos_ind])
  fn <- sum(score_dich[neg_ind] != class[neg_ind])
  
  return(list(tp = tp, tn = tn, fp = fp, fn = fn))
}


youdensJ <- function(score = NULL, class = NULL, x = NULL,
                     conf = NULL) {
  
  if (is.null(conf)) {
    if (any(is.null(c(score, class, x)))) {
      stop("If conf is not supplied, score, class and x must be provided.")
    }
    
    conf <- confusion(score, class, x)
  }
  
  # browser()
  
  j <- (conf$tp / (conf$tp + conf$fn)) + (conf$tn / (conf$fp + conf$tn)) - 1
  
  return(j)
}


# this function is adapted from the function presented in
#   https://www.r-bloggers.com/2016/11/calculating-auc-the-area-under-a-roc-curve/
simple_auc <- function(TPR, FPR){
  # inputs already sorted, best scores first
  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  
  # browser()
  
  AUC <- sum(TPR * dFPR) + sum(dTPR * dFPR) / 2
  
  return(AUC)
}


ROC <- function(score, class) {
  
  checkVecs(score, class)
  
  tpr_vec <- rep(0, length(score))
  fpr_vec <- tpr_vec
  ysJ_vec <- tpr_vec
  
  for (i in 1:length(score)) {
    conf <- confusion(score, class, score[i])
    
    tpr_vec[i] <- conf$tp / (conf$tp + conf$fn)
    fpr_vec[i] <- conf$fp / (conf$tn + conf$fp)
    ysJ_vec[i] <- youdensJ(conf = conf)
    
  }
  
  AUC <- simple_auc(sort(tpr_vec), sort(fpr_vec))
  
  opar <- par(mfrow = c(1, 2))
  
  plot(pch = 18, fpr_vec[order(tpr_vec)], tpr_vec[order(tpr_vec)],
       main = paste("AUC =", round(AUC, 3)),
       xlab = "FPR", ylab = "TPR")
  
  plot(type = "l", score[order(score)], ysJ_vec[order(score)],
       main = paste("Best J =", round(max(ysJ_vec), 3)),
       xlab = "cutoff", ylab = "Youden's J")
  
  par(opar)
  
  return(AUC)
}



## ----Multiple delta----------------------------------------------------------------------------------------------

delta_vec <- seq(0, 2, 0.5)

n_rep <- 1000

res <- data.frame(delta = delta_vec, p = rep(0, length(delta_vec)),
                  AUC = rep(0, length(delta_vec)))

for (i in 1:length(delta_vec)) {
  
  delta <- delta_vec[i]
  
  data <- makeData(delta = delta)
  
  score <- data$score
  class <- data$class
  
  n_neg_greater <- 0
  
  for (j in 1:n_rep) {
    rnd_pos <- sample(score[class == 1], 1)
    rnd_neg <- sample(score[class != 1], 1)
    
    n_neg_greater <- n_neg_greater + (rnd_pos > rnd_neg)
  }
  
  
  p <- n_neg_greater / n_rep
  
  cat("Delta = ", delta)
  
  AUC <- ROC(score, class)
  
  res$p  [i] <- p
  res$AUC[i] <- AUC
  
}


