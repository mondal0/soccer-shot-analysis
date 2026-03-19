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

beta.SH = cbind(-rowSums(Beta[,(m[14]+1):m[15]]), Beta[,(m[14]+1):m[15]])

# 7. Season Effet
mat <- as.matrix(beta.SH)
colnames(mat) <- c("S2014","S2015","S2016","S2017","S2018","S2019","S2020",
                   "S2021","S2022","S2023","S2024")

df <- melt(mat)
colnames(df) <- c("Observation", "Variable", "Value")

p = ggplot(df, aes(x = Variable, y = Value)) +
  geom_violin(fill = "lightgray", color = "#999999", linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +
  stat_summary(fun.data = mean_quantile_ci,
               geom = "errorbar", width = 0.25, color = "#999999", size = 0.25) +
  stat_summary(fun = mean, geom = "point", color = "#999999", size = 0.25) +
  theme_minimal() +
  labs(title = "",
       x = "Seasons",
       y = "Estimate") +
  theme_classic() +
  scale_x_discrete(labels = c("S2014" = 14, "S2015" = 15,
                              "S2016" = 16, "S2017" = 17,
                              "S2018" = 18, "S2019" = 19,
                              "S2020" = 20, "S2021" = 21,
                              "S2022" = 22, "S2023" = 23,
                              "S2024" = 24))

p

# ggsave("P4_5774.png",
#        
#        plot = p,
#        
#        width = 6,
#        
#        height = 2,
#        
#        units = "in",
#        
#        dpi = 300)
