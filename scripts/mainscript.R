#Treecover and urban islands

#Installing and activating packages
library(performance)
library(terra)
library(mgcViz)
install.packages("mgcViz")
library(mgcv)
library(terra)
library(raster)
library(tidyverse)

lstmax <- rast("data/rasters/lst_max_sant.tif") #Temperatura máxima de superfície
dist  <- rast("data/rasters/dist_river_sant.tif") #Santarem com distancia (0,5 m)
cob <- rast("data/rasters/tree_cover_sant.tif") #Cobertura vegetal de santarem (0,5 m)
ai <- rast("data/rasters/ai_120m_sant.tif") #Entropia escala de 120 m
dist_tree <- rast("data/rasters/dist_florests_sant.tif") #Distancia floresta (120 m)

dist_tree[is.na(dist_tree)] <- 0

summary(dist_tree)
plot(dist_tree)

#Agrengando 120m
dis120 <- aggregate(dist, fact = 240, fun = mean)

plot(dis120)

cob120 <- aggregate(cob, fact = 240, fun = mean) 

plot(cob120)

lstmax120 <- aggregate(lstmax, fact = 4, fun = mean) 

plot(lstmax120)

dis120 <- resample(dis120, lstmax, method = "near")
plot(dis120)

cob120 <- resample(cob120, lstmax, method = "near")
plot(cob120)

lstmax120<- resample(lstmax120, lstmax, method = "near")

ai<- resample(ai, lstmax, method = "near")

dist_tree<- resample(dist_tree, lstmax, method = "near")


raster::origin(cob120)
raster::origin(dis120)
raster::origin(lstmax120)
raster::origin(ai)
raster::origin(dist_tree)

# empilhar
stack <- c(dis120, cob120, lstmax, ai, dist_tree)
names(stack) <- c("dist","tc", "lstmax", "ai", "dist_tree")

# converter para dataframe
df <- as.data.frame(stack, xy = TRUE, na.rm = TRUE)

write.csv(df, "data/csv/dataframesant.csv")

View(df)

#GAM model:
mod <- gam(lstmax ~ tc * ai + dist + dist_tree + s(x, y, k = 50),
           data = df,
           method = "REML")

# DHARMa
library(DHARMa)
sim <- simulateResiduals(mod, plot = FALSE)

ks_p   <- testUniformity(sim)$p.value
disp_p <- testDispersion(sim)$p.value
out_p  <- testOutliers(sim)$p.value



#Gráfico tree cover e temperatura
ggplot(df, aes(tc, lstmax)) +
  geom_point(aes(color =dist,
                 size = dist_tree),
             alpha = 0.6) +
  
  geom_smooth(method = "loess",
              color = "red",
              se = TRUE,
              linewidth = 1.2) +
  
  scale_color_viridis_c(option = "plasma") +
  
  scale_size_continuous(range = c(0.5, 4)) +
  
  labs(
    x = "Tree cover",
    y = "Max Land Surface Temperature (°C)",
    color = "Distance from rivers",
    size = "Distance from trees"
  ) +
  
  theme_classic(base_size = 14)

ggsave("output/graphics/tree_cover_max_temp_sant.png",
       width=30, height=20, units="cm", dpi=600)


#Gráfico aggregation index e temperatura
ggplot(df, aes(ai, lstmax)) +
  geom_point(aes(color =dist,
                 size = dist_tree),
             alpha = 0.6) +
  
  geom_smooth(method = "loess",
              color = "red",
              se = TRUE,
              linewidth = 1.2) +
  
  scale_color_viridis_c(option = "plasma") +
  
  scale_size_continuous(range = c(0.5, 4)) +
  
  labs(
    x = "Aggragation index",
    y = "Max Land Surface Temperature (°C)",
    color = "Distance from rivers",
    size = "Distance from trees"
  ) +
  
  theme_classic(base_size = 14)

ggsave("output/graphics/ai_max_temp_sant.png",
       width=30, height=20, units="cm", dpi=600)
