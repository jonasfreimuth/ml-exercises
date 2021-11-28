
# setup -------------------------------------------------------------------

source("jf_lab_02.R")

library(cluster)

cancer <- read.table("data/ML-Data/fraumeni1960-cancer.tab",
                     header = TRUE,
                     row.names = 1)

cancer_scale <- scale(cancer)

k_means <- list()

k_means[["cent_2"]] <- kmeans(cancer_scale, centers = 2)
k_means[["cent_3"]] <- kmeans(cancer_scale, centers = 3)


k_means[["cent_3_nstart_1"]] <- kmeans(cancer_scale, centers = 3, nstart = 1)
k_means[["cent_3_nstart_10"]] <- kmeans(cancer_scale, centers = 3, nstart = 10)
k_means[["cent_5_nstart_10"]] <- kmeans(cancer_scale, centers = 5, nstart = 10)

clusters <- data.frame(row.names = row.names(cancer))

for (km in names(k_means)) {
  clusters <- cbind(clusters, k_means[[km]]$cluster)
}

names(clusters) <- names(k_means)


plot(cmdscale(dist(cancer_scale)),
     col = clusters$cent_3, pch = 19, xlim = c(-4, 4), ylim = c(-3, 3))
text(cmdscale(dist(cancer_scale)), labels = rownames(cancer),
     col = clusters$cent_3, pch = 19, adj = c(-0.3, -0.3))


# task 2 ------------------------------------------------------------------

op <- par(mfrow = c(2, 2))

pam_res <- list()

pam_res[["k_2"]] <- pam(cancer_scale, 2)
pam_res[["k_3"]] <- pam(cancer_scale, 3)
pam_res[["k_4"]] <- pam(cancer_scale, 4)


pam_clust <- data.frame(row.names = row.names(cancer))

for (pam_nm in names(pam_res)) {
  pam_clust <- cbind(pam_clust, pam_res[[pam_nm]]$clustering)
}

names(pam_clust) <- names(pam_res)

jf.plot.mds(cancer_scale, grid = FALSE, method = "euclidean")

plot(cmdscale(dist(cancer_scale)),
     col = pam_clust$k_2, pch = 19,
     xlim = c(-4, 4), ylim = c(-3, 3)
     )
text(cmdscale(dist(cancer_scale)), labels = rownames(cancer),
     col = pam_clust$k_2, pch = 19, adj = c(-0.3, -0.3))

plot(cmdscale(dist(cancer_scale)),
     col = pam_clust$k_3, pch = 19, xlim = c(-4, 4), ylim = c(-3, 3))
text(cmdscale(dist(cancer_scale)), labels = rownames(cancer),
     col = pam_clust$k_3, pch = 19, adj = c(-0.3, -0.3))

plot(cmdscale(dist(cancer_scale)),
     col = pam_clust$k_4, pch = 19, xlim = c(-4, 4), ylim = c(-3, 3))
text(cmdscale(dist(cancer_scale)), labels = rownames(cancer),
     col = pam_clust$k_4, pch = 19, adj = c(-0.3, -0.3))





