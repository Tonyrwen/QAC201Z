# package deposit
library(descr)
library(jpeg)
library(png)
library(ggplot2)
library(patchwork)

# load in data set
load(url("https://github.com/nazzstat/AppliedData/raw/master/marscrater_pds.RData"))



test = data.frame(x = runif(2000, -180, 180), y = runif(2000, -85, 85))


ggplot(test, aes(x, y))+
  geom_bin2d(bins = 30)+
  scale_fill_continuous(type = "viridis")+
  scale_x_continuous(breaks = seq(-180, 180, by = 10))+
  scale_y_continuous(breaks = seq(-100, 100, by = 10))+
  geom_vline(xintercept = 0, color = "red")+
  geom_hline(yintercept = 0, color = "red")


colnames(marscrater_pds)


############################################################
#
# Exploratory Visualization of crater density and depth 
#
############################################################

library(scales)
library(ggpubr)

# density within 20-by-20 boxes
ggplot(marscrater_pds, aes(y = latitude_circle_image, x = longitude_circle_image))+
  geom_bin2d(bins = 20)+
  #geom_point(color = "red")+
  scale_fill_continuous(type = "viridis")+
  scale_x_continuous(breaks = seq(-180, 180, by = 10))+
  scale_y_continuous(breaks = seq(-100, 100, by = 10))+
  geom_vline(xintercept = -162, color = "red")+
  geom_hline(yintercept = 86.201, color = "red")+
  geom_hline(yintercept = -86.201, color = "red")+
  theme_bw()


# depth within 20-by-20 boxes
ggplot(marscrater_pds, aes(y = latitude_circle_image, x = longitude_circle_image, z = depth_rimfloor_topog))+
  stat_summary_2d(bins = 20)+
  #geom_point(color = "red")+
  scale_fill_continuous(type = "viridis")+
  scale_x_continuous(breaks = seq(-180, 180, by = 10))+
  scale_y_continuous(breaks = seq(-100, 100, by = 10))+
  theme_bw()


# experiment with adding background image
marsImage = readPNG("C:\\Users\\tonyr\\Downloads\\Mars_topography_crop.png", native = T)                 

ggplot(test, aes(v1, v2)) + 
  background_image(marsImage)+
  geom_point()


##########################################################
#
# create new variable to identify crater region
#
##########################################################

# bin height
bheight = (max(marscrater_pds$latitude_circle_image)-min(marscrater_pds$latitude_circle_image))/20

# bin width
bwidth = (max(marscrater_pds$longitude_circle_image)-min(marscrater_pds$longitude_circle_image))/20

0+10*(max(marscrater_pds$latitude_circle_image)-min(marscrater_pds$latitude_circle_image))/20


# creating list of bin boundaries
#longitude_bins = c()
#for (i in 0:20){
#  longitude_bins = append(longitude_bins, -180 + i*bwidth)
#}
#longitude_bins[21] = 180

seq(-180, 180, length = 21)


#latitude_bins = c()
#for (i in 10:-10){
#  latitude_bins = append(latitude_bins, i*bheight)
#}

seq(-86.2010, 86.2010, length = 21)

# using bin_boundary and cut() to create new variable long_bin, lat_bin
marscrater_pds = marscrater_pds %>% mutate(long_bin = cut(marscrater_pds$longitude_circle_image, seq(-180, 180, length = 21), include.lowest = T),
                                           lat_bin = cut(marscrater_pds$latitude_circle_image, seq(-86.2010, 86.2010, length = 21), include.lowest = T))

# summarize mean(depth) and n() based on bin groups
bin_depth_density = marscrater_pds %>% group_by(long_bin, lat_bin) %>% 
                                       summarise(mean_depth = mean(depth_rimfloor_topog),
                                                 density = n())

# join density into marscrater_pds
marscrater_pds = marscrater_pds %>% left_join(bin_depth_density %>% select(lat_bin, long_bin, density), by = c("lat_bin", "long_bin"))


ggplot(marscrater_pds, aes(x= longitude_circle_image, y = latitude_circle_image))+
  geom_bin2d(bins = 1000)+
  scale_fill_continuous(type = "viridis")+
  geom_density_2d_filled(alpha = 0.25)

ggplot(marscrater_pds, aes(x= longitude_circle_image, y = latitude_circle_image))+
  geom_point(size = 2e-16, color = "red")+
  geom_density_2d_filled(alpha = 0.9, contour_var = "count")

# run a pearson correlation test
cor.test(bin_depth_density$mean_depth, bin_depth_density$density)
cor.test(marscrater_pds$depth_rimfloor_topog, marscrater_pds$density)

ggplot(marscrater_pds, aes(x = density, y = depth_rimfloor_topog))+
  geom_point()+
  xlab("crater density")+
  ylab("mean depth of crater clusters")+
  geom_smooth(method = "lm")+
  stat_summary(fun = "median", col= "red")

# check if lat or long moderate the relationship
ggplot(bin_depth_density, aes(x = bin_depth_density$density, y = bin_depth_density$mean_depth))+
  geom_point()+
  xlab("crater density")+
  ylab("mean depth of crater clusters")+
  geom_smooth(method = "lm")+
  facet_wrap(~lat_bin)

ggplot(bin_depth_density, aes(x = bin_depth_density$density, y = bin_depth_density$mean_depth))+
  geom_point()+
  xlab("crater density")+
  ylab("mean depth of crater clusters")+
  geom_smooth(method = "lm")+
  facet_wrap(~long_bin)

# run regression
summary(lm(mean_depth ~ density+lat_bin, data = bin_depth_density))


summary(lm(mean_depth ~ ., data = bin_depth_density))



####################################################
#
# Since lat and long, especially lat moderates the relationship, check their correlation with depth
#
####################################################
# cor.test
cor.test(marscrater_pds$depth_rimfloor_topog, abs(marscrater_pds$latitude_circle_image))
cor.test(marscrater_pds$depth_rimfloor_topog, abs(marscrater_pds$longitude_circle_image))

# regression
summary(lm(depth_rimfloor_topog~density+latitude_circle_image+longitude_circle_image+latitude_circle_image*longitude_circle_image, data = marscrater_pds))

#

plot(lm(depth_rimfloor_topog~density+latitude_circle_image+longitude_circle_image+latitude_circle_image*longitude_circle_image, data = marscrater_pds))





test$v1 <- relevel(test$v1, ref = "y")



#####################################################
# logistic regression graph
#####################################################

test_mod = glm(spam ~ num_char, data = email, family = "binomial")
summary(test_mod)

summary(email$num_char)
graphdata = expand.grid(num_char = seq(1, 3921, 1))

graphdata = cbind(graphdata, predict(test_mod, newdata = graphdata, type = "link", se = T))

graphdata = cbind(graphdata, predictedProb = plogis(graphdata$fit))

LL = plogis(predict(test_mod, newdata = graphdata) - 1.96*predict(test_mod, newdata = graphdata, se = T)$se.fit)
UL = plogis(predict(test_mod, newdata = graphdata) + 1.96*predict(test_mod, newdata = graphdata, se = T)$se.fit)

ggplot(graphdata)+
  geom_line(aes(x = num_char, y = predictedProb))+
  geom_point(aes(x = num_char, y = predictedProb))


cont_test = tibble(prob = test_mod$fitted.values,
       spam = email$spam) %>% arrange(prob) %>%
  mutate(rank = 1:n())

ggplot(cont_test, aes(x = rank, y = prob))+
  geom_point(aes(color = spam), shape = 4)+
  geom_errorbar(aes(ymin = LL, ymax = UL))+
  scale_color_gradient(low = "blue",
                       high = "red")

############################################
#
############################################

marscrater_pds[grepl("Hu[a-zA-Z]{0,2}$", marscrater_pds$morphology_ejecta_2),]
marscrater_pds[grepl("Sm[a-zA-z]{0,2}$", marscrater_pds$morphology_ejecta_2),]



############################################
#
############################################

library(dplyr)
ggplot(data = graphdata1 %>% rename(action = fit, `Age Group` = age_categories))+
  geom_point(aes(x = , y = action))+
  facet_wrap(~.`Age Group`)















#############################################################
#
#############################################################
