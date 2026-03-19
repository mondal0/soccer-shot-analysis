library(ggplot2)
library(reshape2)
library(patchwork)
library(plotly)
library(ggdendro)
library(Matrix)
library(expm)
library(pROC)
detach(package:pROC,unload=TRUE)

Beta = readRDS("output/Beta1.rds")

mean_quantile_ci <- function(x) {
  qs <- quantile(x, probs = c(0.025, 0.975))
  data.frame(y = mean(x), ymin = qs[1], ymax = qs[2])
}

m_p = 4
m_p1 = 523
m_p2 = 111
m_p3 = 378
m_p4 = 244
m_p5 = 341

m_a = 34
m_k = 6

m_b = 1
m_g = 1
m_t = 5
m_v = 6
m_s = 10

m_h = 1
m_psi = 57*74

m_1 =57
m_2 =74

m = c(m_p,m_p1,m_p2,m_p3,m_p4,m_p5,
      m_a,m_a,m_k,
      m_b,m_g,m_t,m_v,m_s,
      m_s,m_a,m_a,
      m_h,m_psi)

m = cumsum(m)

beta.A1 = cbind(-rowSums(Beta[,(m[6]+1):m[7]]), Beta[,(m[6]+1):m[7]])
beta.A2 = -cbind(-rowSums(Beta[,(m[7]+1):m[8]]), Beta[,(m[7]+1):m[8]])
colnames(beta.A1) = colnames(beta.A2) = c("ARS", "AVL", "BOU", "BRE", "BHA", "BRN",
                                          "CAR", "CHE", "CRY", "EVE", "FUL", "HUD",
                                          "HUL", "IPS", "LEE", "LEI", "LIV", "LUT", "MCI",
                                          "MUN", "MID", "NEW", "NOR", "NFO", "QPR",
                                          "SHU", "SOU", "STO", "SUN", "SWA", "TOT",
                                          "WAT", "WBA", "WHU", "WOL")


# 1. Atk
est.A1 = colMeans(beta.A1)
beta.A1.new = beta.A1[,order(est.A1, decreasing = T)][,1:8]
X_df <- as.data.frame(beta.A1.new)
X_melt <- melt(X_df)

P1 = ggplot(X_melt, aes(x = variable, y = value)) +
  geom_violin(fill = "lightgray", color = "#999999", linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +
  stat_summary(fun.data = mean_quantile_ci,
               geom = "errorbar", width = 0.25, color = "#999999", size = 0.25) +
  stat_summary(fun = mean, geom = "point", color = "#999999", size = 0.25) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 8)) +
  ylim(c(-0.3, 0.45)) +
  labs(title = "(A)",
       x = " ",
       y = "Estimates")


# 2. Def
est.A2 = colMeans(beta.A2)
beta.A2.new = beta.A2[,order(est.A2, decreasing = T)][,1:8]
X_df <- as.data.frame(beta.A2.new)
X_melt <- melt(X_df)

P2 = ggplot(X_melt, aes(x = variable, y = value)) +
  geom_violin(fill = "lightgray", color = "#999999", linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +
  stat_summary(fun.data = mean_quantile_ci,
               geom = "errorbar", width = 0.25, color = "#999999", size = 0.25) +
  stat_summary(fun = mean, geom = "point", color = "#999999", size = 0.25) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 8)) +
  ylim(c(-0.3, 0.45)) +
  labs(title = "(B)",
       x = " ",
       y = "Estimates")

# 3. Dendogram
sampx = beta.A1[seq(9, 10000, 10), ]
sampy = beta.A2[seq(9, 10000, 10), ]

mu = list()
Sig = list()
Sig_det = numeric()
for(i in 1:ncol(beta.A1)){
  temp = cbind(sampx[,i], sampy[,i])
  mu[[i]] = colMeans(temp)
  Sig[[i]] = cov(temp)
  Sig_det[i] = det(cov(temp))
}

wasserstein_distance <- function(mu1, mu2, Sigma1, Sigma2) {
  mean_diff <- mu1 - mu2
  cov_sqrt <- sqrtm(Sigma1) %*% Sigma2 %*% sqrtm(Sigma1)
  distance <- sum((mean_diff)^2) + sum(diag(Sigma1 + Sigma2 - 2 * sqrtm(cov_sqrt)))
  return(sqrt(distance))
}

means_list <- mu 
covariances_list <- Sig 
n <- length(means_list)

ot_distance_matrix <- matrix(0, n, n)
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    distance <- wasserstein_distance(means_list[[i]], means_list[[j]], covariances_list[[i]], covariances_list[[j]])
    ot_distance_matrix[i, j] <- distance
    ot_distance_matrix[j, i] <- distance
  }
}

ot_dist <- as.dist(ot_distance_matrix)
ot_ranking <- hclust(ot_dist, method = "ward.D2")
cl = cutree(ot_ranking,k=6)
ot_ranking$labels <- colnames(beta.A1)

P4 <- ggdendrogram(ot_ranking, rotate = FALSE, size = 1) +
  labs(title = "(C)",
       x = " ",
       y = "OT Distance") +
  theme(axis.text.x = element_text(size = 6))+
  geom_hline(yintercept = 0.119, linetype = "dashed", color = "gray", linewidth = 0.25)

# 4. Atk - Def Plot
nam = c("ARS", "AVL", " ", " ", "BHA", "BRN",
        " ", "CHE", " ", "EVE", " ", "HUD",
        " ", " ", "LEE", "LEI", "LIV", " ", "MCI",
        "MUN", " ", " ", " ", " ", " ",
        " ", " ", " ", " ", " ", "TOT",
        " ", " ", " ", "WOL")


alp = as.numeric(cl) + 14 
alp2 = 1 - (as.numeric(cl)-1)/6

library(ggplot2)
library(dplyr)

data <- data.frame(
  x = as.vector(as.matrix(beta.A1)),
  y = as.vector(as.matrix(beta.A2)),
  group = rep(1:ncol(beta.A1), each = nrow(beta.A1))
)

centers <- data %>%
  group_by(group) %>%
  summarise(x = mean(x), y = mean(y))

cluster_labels <- data.frame(
  name = nam,
  x = centers$x + 0.01, 
  y = centers$y + 0.005 
)


P3 = ggplot(data, aes(x = x, y = y)) + 
  geom_point(data = centers, aes(x = x, y = y), color = "black", alpha = alp2, size = 1, shape = alp) +
  geom_text(data = cluster_labels, aes(x = x, y = y, label = name), 
            color = "black", size = 2) +  
  xlim(c(-0.13, 0.13)) + ylim(c(-0.13, 0.13)) +
  theme_classic()  + geom_hline(yintercept=0, linetype="dashed", size=0.3, alpha=0.3) +
  geom_vline(xintercept=0, linetype="dashed", size=0.3, alpha=0.3) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", size=0.3, alpha=0.3) +
  labs(title = "(D)",
       x = "Offensive Ability",
       y = "Defensive Ability") 


layout <- "
AAAAABBBB
DDDDDDEEE
"

p = P1 + P2 + P4 + P3 + plot_layout(design = layout)

p
# ggsave("P1_5774.png",
#        
#        plot = p,
#        
#        width = 10,
#        
#        height = 6,
#        
#        units = "in",
#        
#        dpi = 300)