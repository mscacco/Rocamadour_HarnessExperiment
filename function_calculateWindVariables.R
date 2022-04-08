
### Functions for calculating wind support, cross wind and airspeed:
#input: u and v are the wind components downloaded from movebank, dg is the track direction and has to range from 0 to 360
wind.support <- function(u, v, dg){ 
  if(any(dg[!is.na(dg)] < 0)){stop("The track direction (Dg) has negative values, it has to range from 0 to 360!")}
  dg_rad <- dg/180*pi                        #transform Dg in radians from 0 to 2*pi
  wd <- atan2(u, v)                          #wd (wind direction) from -pi to +pi
  wd_2pi <- ifelse(wd < 0, 2*pi + wd, wd)    #wd from 0 to 2*pi
  beta <- wd_2pi - dg_rad                    #beta is the abs of wd-dg (they are both from 0 to 2*pi)
  return(cos(beta) * sqrt(u * u + v * v))    #ws = cos(beta)*Vw
}
cross.wind <- function(u, v, dg){
  if(any(dg[!is.na(dg)] < 0)){stop("The track direction (Dg) has negative values, it has to range from 0 to 360!")}
  dg_rad <- dg/180*pi                           #transform Dg in radians from 0 to 2*pi
  wd <- atan2(u, v)                             #wd (wind direction) from -pi to +pi
  wd_2pi <- ifelse(wd < 0, 2*pi + wd, wd)       #wd from 0 to 2*pi
  beta <- wd_2pi - dg_rad                       #beta is the abs of wd-dg (they are both from 0 to 2*pi)
  return(abs(sin(beta) * sqrt(u * u + v * v)))  #wc = |sin(beta)*Vw|
}
#calculate airspeed, you need cross wind, wind support and track ground speed (Vg)
#Va = sqrt((Vg-Ws)^2 + Wc^2)
airspeed <- function(Vg, Ws, Cw) {
  return(sqrt((Vg - Ws)^2 + (Cw)^2))
}
# Transfrom direction from +- 180 to 0-360
direction360 <- function(x){ 
  return(ifelse(x < 0, 360 + x, x))} 