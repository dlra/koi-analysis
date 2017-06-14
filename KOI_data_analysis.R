##########################################
#
# KOI analysis
# DLRA
# 01/06/2017
#
#
#
##########################################



library(ggplot2)
library(dplyr)
library(Hmisc)

# load koi data; ignore metadata
kepler<-read.csv("./cumulative.csv",
                 stringsAsFactors = FALSE,skip=53)

# additional data
planets <- data.frame(name = "Earth", temp.eq.K = 255,
                      period = 365.25, radius = 1)
stars <- data.frame(name = "Sun", temp.K = 5800, radius = 1)

################
# Looking at exoplanet period
################

# form a histogram for exoplanet period
hist_T <- ggplot(kepler,
                     aes(x=koi_period)) +
  geom_histogram(
                 colour = "darkgray", fill = "lightgray",
                 stat = "bin", binwidth = 10) +
  theme_bw()

# Histogram of exoplanet period
# Interesting region (0 d < T < 750 d), showing Earth vline

hist_T +
  scale_x_continuous(limits = c(0,750),
                     name = "Exoplanet orbital period (days)") +
  scale_y_continuous(limits = c(0,2300),
                     name = "Count") +
  ggtitle("Distribution of exoplanets by orbital period") +
  geom_vline(data=planets, aes(xintercept = period, colour = name), size = 1,
             linetype = "dashed", show.legend = TRUE) +
  scale_color_manual(name=NULL, values = c(Earth = "orange")) +
  theme(
    legend.justification = c(0,0),
    legend.position = c(0.7,0.7),
    legend.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Histogram of exoplanet period
# Around 400 d region (300 d < T < 500 d), showing Earth vline

hist_T.400 <- ggplot(kepler,
                 aes(x=koi_period)) +
  geom_histogram(
    colour = "darkgray", fill = "lightgray",
    stat = "bin", binwidth = 2) +
  theme_bw()
hist_T.400 +
  scale_x_continuous(limits = c(300,500),
                     name = "Exoplanet orbital period (days)") +
  scale_y_continuous(limits = c(0,30),
                     name = "Count") +
  ggtitle("Distribution of exoplanets by orbital period") +
  geom_vline(data=planets, aes(xintercept = period, colour = name), size = 1,
             linetype = "dashed", show.legend = TRUE) +
  scale_color_manual(name=NULL, values = c(Earth = "orange")) +
  theme(
    legend.justification = c(0,0),
    legend.position = c(0.7,0.7),
    legend.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# form a new histogram for exoplanet period, ln(count)
hist_log.T <- ggplot(kepler,
  aes(x=koi_period)) +
  geom_histogram(aes(y=log(..count..)),
                 colour = "darkgray", fill = "lightgray",
                 stat = "bin", binwidth = 10) +
  theme_bw()

# limit data to interesting region

hist_log.T +
  scale_x_continuous(limits = c(0,750)) +
  scale_y_continuous(limits = c(0,8)) +
  ggtitle("Distribution of exoplanets by orbital period") +
  geom_vline(data=planets, aes(xintercept = period, colour = name), size = 1,
             linetype = "dashed", show.legend = TRUE) +
  scale_color_manual(name=NULL, values = c(Earth = "orange")) +
  theme(
    legend.justification = c(0,0),
    legend.position = c(0.7,0.7),
    legend.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


################
# Looking at exoplanet period
################

# form a histogram plot for exoplanet radius
hist_R <- ggplot(kepler,
  aes(x=koi_prad)) +
  geom_histogram()

# limit data to interesting region
hist_R +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,1300))


# change bin width...
hist_R.2 <- ggplot(kepler,
  aes(x=koi_prad)) +
  geom_histogram(aes(y=..density..), # use density when plotting normal curve
                 colour = "darkgray", fill = "lightgray",
                 stat = "bin", binwidth = 0.1) +
  theme_bw()


# ... and check again
hist_R.2 +
  scale_x_continuous(limits = c(0,10), name =
                       "Exoplanet radius (Earth radii)",
                     breaks = seq(0,10,1)) +
  scale_y_continuous(limits = c(0,0.6), name = "Density") +
  ggtitle("Distribution of exoplanets by radius") +
# add a normal density curve, positive skew
  stat_function(fun = dlnorm, colour = "red", size = 1,
              args = list(meanlog = mean(log(filter(kepler,koi_prad<=7)$koi_prad),
                                         na.rm = TRUE),
                         sdlog = sd(log(filter(kepler,koi_prad<=7)$koi_prad),
                                    na.rm = TRUE))) +
# add earth vline
  geom_vline(data=planets, aes(xintercept = radius, colour = name), size = 1,
             linetype = "dashed", show.legend = TRUE) +
  scale_color_manual(name=NULL, values = c(Earth = "orange")) +
  theme(
    legend.justification = c(0,0),
    legend.position = c(0.7,0.7),
    legend.background = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

################
# Looking at stellar temperature
################


hist_S_Temp <- ggplot(kepler,
                 aes(x=koi_s)) +
  geom_histogram(
                 colour = "darkgray", fill = "lightgray",
                 stat = "bin", bins = 100) +
  theme_bw()

# Histogram of stellar effective temperature
# add Sun vline

hist_S_Temp +
  scale_x_continuous(limits = c(2e3,1e4),
                     name = "Solar effective temperature (K)",
                     breaks = seq(2e3,1e4,1e3)) +
  scale_y_continuous(limits = c(0,7e2),
                     name = "Count",
                     breaks = seq(0,7e2,1e2)) +
  ggtitle("Distribution of effective stellar temperature") +
  geom_vline(data=stars, aes(xintercept = temp.K, colour = name), size = 1,
             linetype = "dashed", show.legend = TRUE) +
  scale_color_manual(name=NULL, values = c(Sun = "orange")) +
  theme(
    legend.justification = c(0,0),
    legend.position = c(0.7,0.7),
    legend.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

################
# Looking at stellar radius
################

hist_S_rad <- ggplot(kepler,
                      aes(x=koi_srad)) +
  geom_histogram(
    colour = "darkgray", fill = "lightgray",
    stat = "bin", bins = 100) +
  theme_bw()

hist_S_rad +
  scale_x_continuous(limits = c(0,8),
                     name = "Stellar radius (Solar radii)",
                     breaks = seq(0,10,2)) +
  scale_y_continuous(limits = c(0,1200),
                     name = "Count",
                     breaks = seq(0,11e2,2e2)) +
  ggtitle("Distribution of stellar radii") +
  geom_vline(data=stars, aes(xintercept = radius, colour = name), size = 1,
             linetype = "dashed", show.legend = TRUE) +
  scale_color_manual(name=NULL, values = c(Sun = "orange")) +
  theme(
    legend.justification = c(0,0),
    legend.position = c(0.7,0.7),
    legend.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

################
# Looking at equilibrium temperature
################

hist_p_Temp <- ggplot(kepler,
                     aes(x=koi_teq)) +
  geom_histogram(
    colour = "darkgray", fill = "lightgray",
    stat = "bin", bins = 100) +
  theme_bw()

hist_p_Temp +
  scale_x_continuous(limits = c(0,5.5e3),
                     name = "Equilibrium temperature (K)",
                     breaks = seq(0,5e3,1e3)) +
  scale_y_continuous(limits = c(0,420),
                     name = "Count",
                     breaks = seq(0,400,100)) +
  ggtitle("Distribution of exoplanet equilibrium temperature") +
  geom_vline(data=planets, aes(xintercept = temp.eq.K, colour = name), size = 1,
             linetype = "dashed", show.legend = TRUE) +
  scale_color_manual(name=NULL, values = c(Earth = "orange")) +
  theme(
    legend.justification = c(0,0),
    legend.position = c(0.7,0.7),
    legend.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

################
# Looking at scatter plots of
# exoplanet period vs exoplanet radius
################

scatter_R_T <- ggplot(data = kepler,
                      aes(x = koi_prad,
                          y = koi_period)) +
  theme_bw()

# scatter plot, R-T, colour scaled by
# stellar effective temperature
# filtered by T_eq

scatter_R_T +
  geom_point(data=kepler %>%
               filter(koi_teq < 280 &
                        koi_teq >230),aes(colour=koi_steff)) +
  scale_colour_gradientn(colours =
                           c("brown","red",
                             "orange","yellow",
                             "white"),
                         limits = c(3e3,9e3)) +
  scale_x_continuous(limits = c(0,10),
                     name = "Exoplanet radius (Earth radii)",
                     breaks = seq(0,10,2)) +
  scale_y_continuous(limits = c(0,750),
                     name = "Exoplanet period (days)",
                     breaks = seq(0,800,100)) +
  ggtitle("Exoplanet period versus radius") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(colour = "black"),
    panel.grid.minor = element_line(colour = "black")
  ) +
  labs(colour = "Stellar\nTemp (K)\n")


# scatter plot, R-T, colour scaled by
# stellar radius



scatter_R_T +
  geom_point(data=kepler,aes(colour=koi_srad),size=1) +
  scale_colour_gradientn(colours =
                           c("blue","white",
                             "red"), limits = c(0,4)) +
  scale_x_continuous(limits = c(0,10),
                     name = "Exoplanet radius (Earth radii)",
                     breaks = seq(0,10,2)) +
  scale_y_continuous(limits = c(0,750),
                     name = "Exoplanet period (days)",
                     breaks = seq(0,800,100)) +
  ggtitle("Exoplanet period versus radius") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(colour = "black"),
    panel.grid.minor = element_line(colour = "black")
  ) +
  labs(colour = "Stellar\nradius")

# scatter plot, R-T, colour scaled by
# stellar radius
# focus on region of interest

scatter_R_T_Srad.zoom <- scatter_R_T +
  geom_point(data=kepler,aes(colour=koi_srad),size=2) +
  scale_colour_gradientn(colours =
                           c("blue","lightblue","white",
                             "pink","red"), limits = c(0,4)) +
  scale_x_continuous(limits = c(0.5,5),
                     name = "Exoplanet radius (Earth radii)",
                     breaks = seq(0,10,1)) +
  scale_y_continuous(limits = c(300,450),
                     name = "Exoplanet period (days)",
                     breaks = seq(0,800,50)) +
  ggtitle("Exoplanet period versus radius") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(colour = "black"),
    panel.grid.minor = element_line(colour = "black")
  ) +
  labs(colour = "Stellar\nradius")

# plot and add Earth point
scatter_R_T_Srad.zoom +
  geom_point(data = planets, aes(x=radius,y=period,
                                 shape=name),
              colour = "green", size = 2) +
  scale_shape_manual(values = 2, name=NULL)

# scatter plot, R-T, colour scaled by
# equilibrium temperature

scatter_R_T +
  geom_point(data=kepler,aes(colour=koi_teq),size=1.5) +
  scale_colour_gradientn(colours =
                           c("purple","darkblue",
                             "blue","lightblue","brown",
                             "red","yellow","white"),
                         
                         #values = c(0,0.1,0.2,0.5,1.0),
                         limits = c(0,4e3)) +
  scale_x_continuous(limits = c(0,10),
                     name = "Exoplanet radius (Earth radii)",
                     breaks = seq(0,10,2)) +
  scale_y_continuous(limits = c(0,750),
                     name = "Exoplanet period (days)",
                     breaks = seq(0,800,100)) +
  ggtitle("Exoplanet period versus radius") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(colour = "black"),
    panel.grid.minor = element_line(colour = "black")
  ) +
  labs(colour = "Planet\nTemp (K)")

# scatter plot, R-T, colour scaled by
# equilibrium temperature
# zone 1a region of interest

scatter_R_T +
  geom_point(data=kepler,aes(colour=koi_teq),size=2) +
  scale_colour_gradientn(colours =
                           c("purple","darkblue",
                             "blue","lightblue","brown",
                             "red","yellow","white"),
                         
                         #values = c(0,0.1,0.2,0.5,1.0),
                         limits = c(0,1e3)) +
  scale_x_continuous(limits = c(0,5),
                     name = "Exoplanet radius",
                     breaks = seq(0,10,1)) +
  scale_y_continuous(limits = c(50,650),
                     name = "Exoplanet period",
                     breaks = seq(0,800,100)) +
  ggtitle("Exoplanet period versus radius") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(colour = "black"),
    panel.grid.minor = element_line(colour = "black")
  )

# scatter plot, R-T, colour scaled by
# equilibrium temperature
# zone 1b region of interest

scatter_R_T +
  geom_point(data=kepler,aes(colour=koi_teq),size=2) +
  scale_colour_gradientn(colours =
                           c("purple","darkblue",
                             "blue","lightblue","brown",
                             "red","yellow","white"),
                         
                         #values = c(0,0.1,0.2,0.5,1.0),
                         limits = c(0,750)) +
  scale_x_continuous(limits = c(0.5,5),
                     name = "Exoplanet radius",
                     breaks = seq(0,10,1)) +
  scale_y_continuous(limits = c(300,450),
                     name = "Exoplanet period",
                     breaks = seq(0,800,50)) +
  ggtitle("Exoplanet period versus radius") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(colour = "black"),
    panel.grid.minor = element_line(colour = "black")
  )
