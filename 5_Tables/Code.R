rm( list = ls() )

################
### PACKAGES ###
################

library(lme4)
library(lmerTest)

############
### DATA ###
############

data = read.csv( "Data/Lisianthius Data.csv" )
data = subset( data, group %in% c( "NSA1", "NSA2", "PSA1", "PSA2" ) )
data$group = factor( data$group, levels = c( "PSA1", "PSA2", "NSA1", "NSA2" ), labels = c( "SSV", "SSR", "HSV", "HSR" ) )
data$cultivar = factor( data$cultivar, levels = c( "AG", "BP", "KP", "KW" ), labels = c( "AG", "BP", "CP", "KW" ) )
#head(data)

###
###### P-VALUE FORMATTING FUNCTION
###

f = function(p) {
  temp1 = ifelse( p < 0.0001, formatC( p, format = "e", digits = 1 ), formatC( p, format = "f", digits = 3 ) )
  temp2 = rep( "", length(p) )
  temp2 = ifelse( p < 0.05, "*", temp2 )
  temp2 = ifelse( p < 0.01, "**", temp2 )
  temp2 = ifelse( p < 0.001, "***", temp2 )
  out = paste0( temp1, temp2 )
  out
}

###############
### TABLE 1 ###
###############

table1 = data.frame()

###
###### AG
###

fit = lm( vase_life_days ~ concentration * group, data = subset( data, cultivar == "AG" ) )
B = matrix( as.numeric( summary(fit)$coef[,1] ), ncol = 1 )
V = matrix( vcov(fit), ncol = length(B), nrow = length(B) )
c = matrix( c( 0, 1, 0, 0, 0, 0, 0, 0 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )
c = matrix( c( 0, 1, 0, 0, 0, 1, 0, 0 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )
c = matrix( c( 0, 1, 0, 0, 0, 0, 1, 0 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )
c = matrix( c( 0, 1, 0, 0, 0, 0, 0, 1 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )

###
###### BP
###

fit = lm( vase_life_days ~ concentration * group, data = subset( data, cultivar == "BP" ) )
B = matrix( as.numeric( summary(fit)$coef[,1] ), ncol = 1 )
V = matrix( vcov(fit), ncol = length(B), nrow = length(B) )
c = matrix( c( 0, 1, 0, 0, 0, 0, 0, 0 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )
c = matrix( c( 0, 1, 0, 0, 0, 1, 0, 0 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )
c = matrix( c( 0, 1, 0, 0, 0, 0, 1, 0 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )
c = matrix( c( 0, 1, 0, 0, 0, 0, 0, 1 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )

###
###### CP
###

fit = lm( vase_life_days ~ concentration * group, data = subset( data, cultivar == "CP" ) )
B = matrix( as.numeric( summary(fit)$coef[,1] ), ncol = 1 )
V = matrix( vcov(fit), ncol = length(B), nrow = length(B) )
c = matrix( c( 0, 1, 0, 0, 0, 0, 0, 0 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )
c = matrix( c( 0, 1, 0, 0, 0, 1, 0, 0 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )
c = matrix( c( 0, 1, 0, 0, 0, 0, 1, 0 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )
c = matrix( c( 0, 1, 0, 0, 0, 0, 0, 1 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )

###
###### KW
###

fit = lm( vase_life_days ~ concentration * group, data = subset( data, cultivar == "KW" ) )
B = matrix( as.numeric( summary(fit)$coef[,1] ), ncol = 1 )
V = matrix( vcov(fit), ncol = length(B), nrow = length(B) )
c = matrix( c( 0, 1, 0, 0, 0, 0, 0, 0 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )
c = matrix( c( 0, 1, 0, 0, 0, 1, 0, 0 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )
c = matrix( c( 0, 1, 0, 0, 0, 0, 1, 0 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )
c = matrix( c( 0, 1, 0, 0, 0, 0, 0, 1 ) ); hat = t(c) %*% B; se = sqrt( as.numeric( t(c) %*% V %*% c ) ); z = hat / se; p = 1 - pnorm(z); table1 = rbind( table1, data.frame( estimate = hat, se = se, z = z, p = p ) )

### TABLE FORMATTING

table1[,1] = formatC( table1[,1], format = "f", digits = 3 )
table1[,2] = formatC( table1[,2], format = "f", digits = 3 )
table1[,3] = formatC( table1[,3], format = "f", digits = 3 )
table1[,4] = f( table1[,4] )
table1

###############
### TABLE 2 ###
###############

fit = aov( vase_life_days ~ factor(concentration) + group + cultivar + factor(concentration):group + factor(concentration):cultivar + group:cultivar, data = data )
#summary(fit)
p.only = f( p = unlist( summary(fit) )[29:34] )
names(p.only) = c( "A", "B", "C", "A x B", "A x C", "B x C" )
table2 = p.only
table2
