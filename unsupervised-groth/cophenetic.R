data(iris)

d_m <- as.matrix(iris[, 1:4], dimnames = list(names(iris[, 1:4]),
                                              as.character(iris[5])))

dst <- dist(scale(d_m))

tree <- hclust(dst)

cph <- cophenetic(tree)

dst_m <- as.matrix(dst)
cph_m <- as.matrix(cph)

cor(as.vector(dst_m), as.vector(cph_m))
cor(dst_m[upper.tri(dst_m)], cph_m[upper.tri(cph_m)])

n <- nrow(dst_m)

N_d <- 0
N_s <- 0

dst_rnk <- rank(dst_m)
cph_rnk <- rank(cph_m)

# for (i in 1:length(dst_rnk)) {
#   for (j in 1:lenth(dst_rnk)) {
#     if ()
#   }
}

# for every pairing of objects
for (i in 1:n) {
  for (j in 1:n) {
    
    # compare it to every other pairing of objects
    for(k in 1:n) {
      for (l in 1:n) {
        
        if (dst_m[i,j] > dst_m[k,l]) {
          if (cph_m[i,j] > cph_m[k,l]) {
            N_s <- N_s + 1
          } else if (cph_m[i,j] < cph_m[k,l]) {
            N_d <- N_d + 1
          }
        } else if (dst_m[i,j] < dst_m[k,l]) {
          if (cph_m[i,j] < cph_m[k,l]) {
            N_s <- N_s + 1
          } else if (cph_m[i,j] > cph_m[k,l]) {
            N_d <- N_d + 1
          }
        }
        
      }
    }
    
  }
}

G <- (N_s - N_d) / (N_s + N_d)
G