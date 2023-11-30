rm( list = ls() )

library(graphics)
library(plot3D)
library(rgl)
library(car)
library(scatterplot3d)

#####################
### DATA CLEANING ###
#####################


###### NUTRIENT ONLY (NSA1 and NSA2)
###

data = read.csv( "Lab_new.csv" )
#data = subset( data, group %in% c( "NSA1", "NSA2" ) )
data$id = paste0( data$treatment, "_", data$samples, "_", data$number )
head( data, 3 )
n = dim(data)[1]

a = data[ , c( "a_1", "a_6", "a_9", "a_11", "a_14" ) ]; a = c( t(a) )
b = data[ , c( "b_1", "b_6", "b_9", "b_11", "b_14" ) ]; b = c( t(b) )
L = data[ , c( "L_1", "L_6", "L_9", "L_11", "L_14" ) ]; L = c( t(L) )
day = rep( c( 1, 6, 9, 11, 14), times = n )
group = rep( data$group, each = 5 )
concentration = rep( as.numeric( substr( data$treatment.method, 6, 8 ) ), each = 5 )
cult = rep( data$cultivar, each = 5 )
no = rep( data$number, each = 5 )
samp = rep( data$samples, each = 5 )
id = rep( data$id, each = 5 )
data.long = data.frame( id, group, cult, day, concentration, no, samp, a, b, L )
data.long$a = ifelse( data.long$a > 50, NA, data.long$a )
data.long$a = ifelse( data.long$a < -50, NA, data.long$a )
data2 = data.long[ apply( is.na(data.long), 1, sum ) == 0, ]

###
###### MERGE SOIL AND NUTRIENT
###
data3 = data2
#data3 = rbind( data1, data2 )

# #data3$group = factor( data3$group, 
#                       levels = c( "PSA1", "PSA2", "NSA1", "NSA2" ),
#                       labels = c( "SSV", "SSR", "HSV", "HSR" ) )

data3$cult = factor( data3$cult, 
                     levels = c( "AG", "BP", "CP", "KW"))

data3$day = ifelse( data3$day < 10, paste0( "0", data3$day ), data3$day )
data3$id = paste0( data3$cult, "-", data3$day )

###############
### GRAPHIC ###
###############

#data3 = subset( data3, group %in% c( "HSR" ) ) ### choose treatments here

u = unique(data3$id)

for (i in 1:length(u)) {
  
  data.temp = subset( data3, id == u[i] )
  cult.temp = data.temp$cult[1]
  day.temp = as.numeric( data.temp$day[1] )
  
  a = data.temp$a
  b = data.temp$b
  L = data.temp$L
  g = as.factor(data.temp$cult)
  
  Lab = cbind( L, a, b )
  sRGB = convertColor( Lab, from = "Lab", to = "sRGB", clip = TRUE )
  RGB = rgb( red = sRGB[,1], green = sRGB[,2], blue = sRGB[,3], maxColorValue = 1 )

  Sigma = cov( Lab[,c(3,1,2)] )
  Mean = apply( Lab[,c(3,1,2)], 2, mean )
  
  Lab2 = apply( cbind( L, a, b ), 2, mean, na.rm = TRUE )
  sRGB2 = convertColor( Lab2, from = "Lab", to = "sRGB", clip = TRUE )
  RGB_mean = rgb( red = sRGB2[1], green = sRGB2[2], blue = sRGB2[3], maxColorValue = 1 )
  
  open3d( windowRect = c(50, 50, 562, 562) )

  plot3d( x = Lab[,3], y = Lab[,1], z = Lab[,2], col = RGB, xlab = "b", ylab = "L", zlab = "a",
          size = 10, aspect = TRUE, xlim = c(-40,40), ylim = c(40,120), zlim = c(-40,40), add = FALSE )
  ### choose the data point size using the "size" option above (size = ...)
  
  plot3d( ellipse3d( Sigma, centre = Mean, alpha = 0.95 ), 
          col = "grey", xlab = "b", ylab = "L", zlab = "a", type = "wire", aspect = TRUE,
          xlim = c(-40,40), ylim = c(40,120), zlim = c(-40,40), alpha = 0.25, add = FALSE )
  
  bgplot3d( {
    
    plot.new()
    par( cex = 1.5, font = 2 )
    title( main = paste0( cult.temp, " (Day ", day.temp, ")" ), line = 0 )
    #mtext(side = 1, 'This is a subtitle', line = 4)
    #use here any other way you fancy to write your title
    
  })
  
  #scatter3d(
  # x = b,
  # y = a,
  # z = L,
  # ellipsoid = TRUE,
  # axis.col = c("black", "black", "black"),
  # axis.scales = FALSE,
  # axis.ticks = TRUE,
  # surface = FALSE,
  # xlim = c(-40, 40),
  # ylim = c(-20, 50),
  # zlim = c(20, 100),
  # fogtype = "exp",
  # ellipsoid.alpha = 0.1,
  # surface.alpha = 0.1,
  # surface.col = RGB_mean,
  # point.col = RGB,
  #)
  
  view3d(theta = -220, phi = 20, fov = 45, zoom = 1) ### choose angle and zoom parameters here
  
  rgl.snapshot( paste0( "Figures 3D/", u[i], ".png" ), fmt = "png" )

}
