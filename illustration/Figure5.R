{
library(ggplot2)
library(reshape2)
library(patchwork)

Beta = readRDS("output/Beta1.rds")
Beta.Fs = readRDS("output/Beta2.rds")

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

beta.P = cbind(-rowSums(Beta[,1:m[1]]), Beta[,1:m[1]])
beta.P1 = cbind(-rowSums(Beta[,(m[1]+1):m[2]]), -Beta[,(m[1]+1):m[2]])
beta.P2 = cbind(-rowSums(Beta[,(m[2]+1):m[3]]), -Beta[,(m[2]+1):m[3]])
beta.P3 = cbind(-rowSums(Beta[,(m[3]+1):m[4]]), -Beta[,(m[3]+1):m[4]])
beta.P4 = cbind(-rowSums(Beta[,(m[4]+1):m[5]]), -Beta[,(m[4]+1):m[5]])
beta.P5 = cbind(-rowSums(Beta[,(m[5]+1):m[6]]), -Beta[,(m[5]+1):m[6]])

beta.A1 = cbind(-rowSums(Beta[,(m[6]+1):m[7]]), Beta[,(m[6]+1):m[7]])
beta.A2 = -cbind(-rowSums(Beta[,(m[7]+1):m[8]]), Beta[,(m[7]+1):m[8]])
beta.K = -cbind(-rowSums(Beta[,(m[8]+1):m[9]]), Beta[,(m[8]+1):m[9]])

beta.B = cbind(Beta[,(m[9]+1):m[10]], -Beta[,(m[9]+1):m[10]])
beta.G = cbind(Beta[,(m[10]+1):m[11]], -Beta[,(m[10]+1):m[11]])
beta.Ti = cbind(-rowSums(Beta[,(m[11]+1):m[12]]), Beta[,(m[11]+1):m[12]])
beta.V = cbind(-rowSums(Beta[,(m[12]+1):m[13]]), Beta[,(m[12]+1):m[13]])
beta.S = cbind(-rowSums(Beta[,(m[13]+1):m[14]]), Beta[,(m[13]+1):m[14]])

beta.SH = cbind(-rowSums(Beta[,(m[14]+1):m[15]]), -Beta[,(m[14]+1):m[15]])
beta.AH1 = cbind(-rowSums(Beta[,(m[15]+1):m[16]]), -Beta[,(m[15]+1):m[16]])
beta.AH2 = -cbind(-rowSums(Beta[,(m[16]+1):m[17]]), -Beta[,(m[16]+1):m[17]])

beta.H = cbind(Beta[,(m[17]+1):m[18]], -Beta[,(m[17]+1):m[18]])

beta.Fs = Beta.Fs

est.Fs = as.numeric(colMeans(beta.Fs))
}
{
  
  # 1. Spatial Effect
  mu = mean(as.matrix(beta.Fs))
  mat <- matrix(as.numeric(apply(beta.Fs, 2, mean)-mu), 57, 74)
  
  df <- melt(mat)
  
  colnames(df) <- c("Row", "Column", "est")
  
  df$Column <- as.numeric(as.character(df$Column))
  df$Row <- as.numeric(as.character(df$Row))
  
  
  p <- ggplot(df, aes(x = Column, y = Row, fill = est)) +
    geom_tile() +
    scale_fill_gradientn(colors = rev(gray.colors(2, 0.2,1)),
                         guide = guide_colorbar(
                           barwidth = 0.75,  
                           barheight = 10    
                         )) +
    labs(title = "(A)",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.position = "left",
          #legend.title=element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  ux = 74/74
  uy = 57/57
  p <- p +
    geom_rect(aes(xmin = 0.5, xmax = 74.5, ymin = 0.5, ymax = 57.5),
              fill = NA, color = "gray25", inherit.aes = FALSE)
  
  p <- p +
    geom_rect(aes(xmin = 37.5-10*ux, xmax = 37.5+10*ux, ymin = 57.5-6*uy, ymax = 57.5),
              fill = NA, color = "gray25", inherit.aes = FALSE)
  
  p <- p +
    geom_rect(aes(xmin = 37.5-22*ux, xmax = 37.5+22*ux, ymin = 57.5-18*uy, ymax = 57.5),
              fill = NA, color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(29*pi/24, 43*pi/24, length.out = 100)
  arc <- data.frame(x = 37.5 + 10 * ux * cos(theta),
                    y = 57.5 - 12*uy + 10 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(0, pi, length.out = 100)
  arc <- data.frame(x = 37.5 + 10 * ux * cos(theta),
                    y = 0.5 + 10 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(-pi, -pi/2, length.out = 100)
  arc <- data.frame(x = 74.5 + 1 * ux * cos(theta),
                    y = 57.5 + 1 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(-pi/2, 0, length.out = 100)
  arc <- data.frame(x = 0.5 + 1 * ux * cos(theta),
                    y = 57.5 + 1 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  p <- p +
    geom_point(aes(x = 37.5, y = 57.5-12*uy), color = "gray25", size = 1) +
    geom_point(aes(x = 37.5, y = 0.5), color = "gray25", size = 1) +
    geom_segment(aes(x = 37.5-4*ux, y = 57.5, xend = 37.5+4*ux, yend = 57.5), color = "gray25", size = 1)

  p <- p + scale_x_reverse()
  P1 <- p + coord_fixed(ratio = 1)
  
  P1 <- P1 +
     geom_text(aes(x = 44, y = 39, label = "I"),    color = "black", size = 4) +
     geom_text(aes(x = 31, y = 39, label = "I\u2032"), color = "black", size = 4)
}
mat.mu = mat
{
  
  # 2. Var
  mat <- matrix(as.numeric(apply(beta.Fs, 2, var)), 57, 74)
  
  df <- melt(mat)
  
  colnames(df) <- c("Row", "Column", "var")
  
  df$Column <- as.numeric(as.character(df$Column))
  df$Row <- as.numeric(as.character(df$Row))
  
  
  
  p <- ggplot(df, aes(x = Column, y = Row, fill = var)) +
    geom_tile() +
    scale_fill_gradientn(colors = rev(gray.colors(2, 0.2,1)),
                         guide = guide_colorbar(
                           barwidth = 0.75,   
                           barheight = 10    
                         )) +
    labs(title = "(B)",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.position = "right",
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  ux = 74/74
  uy = 57/57
  p <- p +
    geom_rect(aes(xmin = 0.5, xmax = 74.5, ymin = 0.5, ymax = 57.5),
              fill = NA, color = "gray25", inherit.aes = FALSE)
  
  p <- p +
    geom_rect(aes(xmin = 37.5-10*ux, xmax = 37.5+10*ux, ymin = 57.5-6*uy, ymax = 57.5),
              fill = NA, color = "gray25", inherit.aes = FALSE)
  
  p <- p +
    geom_rect(aes(xmin = 37.5-22*ux, xmax = 37.5+22*ux, ymin = 57.5-18*uy, ymax = 57.5),
              fill = NA, color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(29*pi/24, 43*pi/24, length.out = 100)
  arc <- data.frame(x = 37.5 + 10 * ux * cos(theta),
                    y = 57.5 - 12*uy + 10 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(0, pi, length.out = 100)
  arc <- data.frame(x = 37.5 + 10 * ux * cos(theta),
                    y = 0.5 + 10 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(-pi, -pi/2, length.out = 100)
  arc <- data.frame(x = 74.5 + 1 * ux * cos(theta),
                    y = 57.5 + 1 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(-pi/2, 0, length.out = 100)
  arc <- data.frame(x = 0.5 + 1 * ux * cos(theta),
                    y = 57.5 + 1 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  p <- p +
    geom_point(aes(x = 37.5, y = 57.5-12*uy), color = "gray25", size = 1) +
    geom_point(aes(x = 37.5, y = 0.5), color = "gray25", size = 1) +
    geom_segment(aes(x = 37.5-4*ux, y = 57.5, xend = 37.5+4*ux, yend = 57.5), color = "gray25", size = 1)
  
  p <- p + scale_x_reverse()
  P2 <- p + coord_fixed(ratio = 1)
}
mat.vr = sqrt(mat)

rho.0uv = beta.Fs + beta.H[,1]
rho.1uv = beta.Fs + beta.H[,2]

mu = mean(as.matrix(beta.Fs))
mat_r <- matrix(as.numeric(apply(rho.0uv, 2, mean)-mu), 57, 74)

mat_l <- matrix(as.numeric(apply(rho.1uv, 2, mean)-mu), 57, 74)
mat = mat_l
for(i in 1:ncol(mat)){
  mat_l[,i] = mat[,ncol(mat)-i+1]
}

K = 10

{
  mat = mat_r
  
  cutoff = as.numeric(quantile(as.numeric(rbind(mat_r, mat_l)), seq(0,1,1/K) ))
  
  mat.arr = array(0, c(57,74,K))
  for(k in 1:K){
    mat.arr[,,k] = as.matrix(mat <= cutoff[k+1] & mat > cutoff[k], 57, 74) * cutoff[k]
  }
  
  mat = rowSums(mat.arr, dims = 2)
  
  df <- melt(mat)
  
  colnames(df) <- c("Row", "Column", "est")
  
  df$Column <- as.numeric(as.character(df$Column))
  df$Row <- as.numeric(as.character(df$Row))
  
  
  
  p <- ggplot(df, aes(x = Column, y = Row, fill = est)) +
    geom_tile() +
    scale_fill_gradientn(colors = rev(gray.colors(2, 0.2,1)),
                         guide = guide_colorbar(
                           barwidth = 0.75,   # make thinner (default ~1)
                           barheight = 10    # controls length
                         )) +
    labs(title = "(D)",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.position = "right",
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  ux = 74/74
  uy = 57/57
  p <- p +
    geom_rect(aes(xmin = 0.5, xmax = 74.5, ymin = 0.5, ymax = 57.5),
              fill = NA, color = "gray25", inherit.aes = FALSE)
  
  p <- p +
    geom_rect(aes(xmin = 37.5-10*ux, xmax = 37.5+10*ux, ymin = 57.5-6*uy, ymax = 57.5),
              fill = NA, color = "gray25", inherit.aes = FALSE)
  
  p <- p +
    geom_rect(aes(xmin = 37.5-22*ux, xmax = 37.5+22*ux, ymin = 57.5-18*uy, ymax = 57.5),
              fill = NA, color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(29*pi/24, 43*pi/24, length.out = 100)
  arc <- data.frame(x = 37.5 + 10 * ux * cos(theta),
                    y = 57.5 - 12*uy + 10 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(0, pi, length.out = 100)
  arc <- data.frame(x = 37.5 + 10 * ux * cos(theta),
                    y = 0.5 + 10 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(-pi, -pi/2, length.out = 100)
  arc <- data.frame(x = 74.5 + 1 * ux * cos(theta),
                    y = 57.5 + 1 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(-pi/2, 0, length.out = 100)
  arc <- data.frame(x = 0.5 + 1 * ux * cos(theta),
                    y = 57.5 + 1 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  p <- p +
    geom_point(aes(x = 37.5, y = 57.5-12*uy), color = "gray25", size = 1) +
    geom_point(aes(x = 37.5, y = 0.5), color = "gray25", size = 1) +
    geom_segment(aes(x = 37.5-4*ux, y = 57.5, xend = 37.5+4*ux, yend = 57.5), color = "gray25", size = 1)
  
  p <- p + scale_x_reverse()
  Pn <- p + coord_fixed(ratio = 1)
}


{
  mat = mat_l
  
  cutoff = as.numeric(quantile(as.numeric(rbind(mat_r, mat_l)), seq(0,1,1/K) ))
  
  mat.arr = array(0, c(57,74,K))
  for(k in 1:K){
    mat.arr[,,k] = as.matrix(mat <= cutoff[k+1] & mat > cutoff[k], 57, 74) * cutoff[k]
  }
  
  mat = rowSums(mat.arr, dims = 2)
  
  df <- melt(mat)
  
  colnames(df) <- c("Row", "Column", "est")
  
  df$Column <- as.numeric(as.character(df$Column))
  df$Row <- as.numeric(as.character(df$Row))
  
  
  
  p <- ggplot(df, aes(x = Column, y = Row, fill = est)) +
    geom_tile() +
    scale_fill_gradientn(colors = rev(gray.colors(2, 0.2,1)),
                         guide = guide_colorbar(
                           barwidth = 0.75,   
                           barheight = 10    
                         )) +
    labs(title = "(C)",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.position = "left",
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  ux = 74/74
  uy = 57/57
  p <- p +
    geom_rect(aes(xmin = 0.5, xmax = 74.5, ymin = 0.5, ymax = 57.5),
              fill = NA, color = "gray25", inherit.aes = FALSE)
  
  p <- p +
    geom_rect(aes(xmin = 37.5-10*ux, xmax = 37.5+10*ux, ymin = 57.5-6*uy, ymax = 57.5),
              fill = NA, color = "gray25", inherit.aes = FALSE)
  
  p <- p +
    geom_rect(aes(xmin = 37.5-22*ux, xmax = 37.5+22*ux, ymin = 57.5-18*uy, ymax = 57.5),
              fill = NA, color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(29*pi/24, 43*pi/24, length.out = 100)
  arc <- data.frame(x = 37.5 + 10 * ux * cos(theta),
                    y = 57.5 - 12*uy + 10 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(0, pi, length.out = 100)
  arc <- data.frame(x = 37.5 + 10 * ux * cos(theta),
                    y = 0.5 + 10 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(-pi, -pi/2, length.out = 100)
  arc <- data.frame(x = 74.5 + 1 * ux * cos(theta),
                    y = 57.5 + 1 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  theta <- seq(-pi/2, 0, length.out = 100)
  arc <- data.frame(x = 0.5 + 1 * ux * cos(theta),
                    y = 57.5 + 1 * uy * sin(theta))
  p <- p +
    geom_path(data = arc, aes(x = x, y = y),
              color = "gray25", inherit.aes = FALSE)
  
  p <- p +
    geom_point(aes(x = 37.5, y = 57.5-12*uy), color = "gray25", size = 1) +
    geom_point(aes(x = 37.5, y = 0.5), color = "gray25", size = 1) +
    geom_segment(aes(x = 37.5-4*ux, y = 57.5, xend = 37.5+4*ux, yend = 57.5), color = "gray25", size = 1)
  
  p <- p + scale_x_reverse()
  Pn2 <- p + coord_fixed(ratio = 1)
}

p = (P1 + P2) / (Pn2 + Pn)

p

# ggsave("P3_5774.png",
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
