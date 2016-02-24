##FEB 10, 2016
##CODE FOR DISTS

#graphically get single trait distributions at sites w/in locales
dists=cbind(start,complete(test, action="broad"))
dists[,1][dists[,1] %in% "Arizona"]="Lemmon"
dists[,1][dists[,1] %in% "Colorado"]="RMBL"
dists[,1][dists[,1] %in% c("Puntarenas", "San Jose")]="Savegre"
dists[,1][dists[,1] %in% "California"]="Sequoia"

#other noodling to get stuff right, not scripted.




#bwplot(log(dists[,8])~dists[,2]|dists[,1], as.table=TRUE,index.cond=list(c(3,5,2,6,1,4)),horizontal=FALSE) #orders them temp on top, trop on bottom
#bwplot(log(dists[,9])~dists[,2]|dists[,1],as.table=TRUE,index.cond=list(c(3,5,2,6,1,4)),horizontal=FALSE)
#bwplot(log(dists[,10])~dists[,2]|dists[,1],as.table=TRUE,index.cond=list(c(3,5,2,6,1,4)),horizontal=FALSE)



#some sort of integral measure??
#OVERLAPPING DENSITY CURVES for whole locales

par(mfrow=c(1,3))

plot(density(dists[,8][dists[,1] %in% "Savegre"]), xlim=c(0,45), ylim=c(0,.15), main="Densities for SLA", col="red")
points(density(dists[,8][dists[,1] %in% "Guanacaste"]), col="orange", type="l")
points(density(dists[,8][dists[,1] %in% "Tamaulipas"]), col="yellow", type="l")
points(density(dists[,8][dists[,1] %in% "Lemmon"]), col="green", type="l")
points(density(dists[,8][dists[,1] %in% "Sequoia"]), col="blue", type="l")
points(density(dists[,8][dists[,1] %in% "RMBL"]), col="cyan", type="l")
plot(density(dists[,9][dists[,1] %in% "Savegre"]), xlim=c(0,60), ylim=c(0,.1), main="Densities for height", col="red")
points(density(dists[,9][dists[,1] %in% "Guanacaste"]), col="orange", type="l")
points(density(dists[,9][dists[,1] %in% "Tamaulipas"]), col="yellow", type="l")
points(density(dists[,9][dists[,1] %in% "Lemmon"]), col="green", type="l")
points(density(dists[,9][dists[,1] %in% "Sequoia"]), col="blue", type="l")
points(density(dists[,9][dists[,1] %in% "RMBL"]), col="cyan", type="l")
plot(density(log(dists[,10][dists[,1] %in% "Savegre"])), ylim=c(0,.5),main="Densities for seed mass", col="red")
points(density(log(dists[,10][dists[,1] %in% "Guanacaste"])), col="orange", type="l")
points(density(log(dists[,10][dists[,1] %in% "Tamaulipas"])), col="yellow", type="l")
points(density(log(dists[,10][dists[,1] %in% "Lemmon"])), col="green", type="l")
points(density(log(dists[,10][dists[,1] %in% "Sequoia"])), col="blue", type="l")
points(density(log(dists[,10][dists[,1] %in% "RMBL"])), col="cyan", type="l")
#for w/in locales

par(mfrow=c(2,3))
par(mfrow=c(1,1))

elev=sort(unique(dists$plotElev[dists[,1] %in% "Savegre"]))
plot(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[1]),])))), ylim=c(0,1.7),xlim=c(-.7,4.2), main="Savegre", xlab="log(SLA)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[6]),])))), col="red4", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Guanacaste"]))
plot(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[1]),])))), ylim=c(0,1.7), xlim=c(-.7,4.2),main="Guanacaste", xlab="log(SLA)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[2]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[3]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[4]),])))), col="red", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Tamaulipas"]))
plot(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[1]),])))), ylim=c(0,1.7), xlim=c(-.7,4.2),, main="Tamaulipas", xlab="log(SLA)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
#points(density(log(dists[,8][dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[7]])), col="midnightblue", type="l") only 1 sp!

elev=sort(unique(dists$plotElev[dists[,1] %in% "Lemmon"]))
plot(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[1]),])))), ylim=c(0,1.7), xlim=c(-.7,4.2),, main="Lemmon", xlab="log(SLA)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[4]),])))), col="orange", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Sequoia"]))
plot(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[1]),])))), ylim=c(0,1.7), xlim=c(-.7,4.2),, main="Sequoia", xlab="log(SLA)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
points(density(log(as.vector(as.matrix(dists[,c(8,17,17+9, 17+18, 17+27)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[7]),])))), col="midnightblue", type="l")
# points(density(log(dists[,8][dists[,1] %in% "Sequoia" & dists[,2] %in% elev[8]])), col="black", type="l") only 1 sp!

#not really enough to do-- use 6th panel as a key??
# elev=sort(unique(dists$plotElev[dists[,1] %in% "RMBL"]))
# plot(density(log(dists[,8][dists[,1] %in% "RMBL" & dists[,2] %in% elev[1]])),xlim=c(0,4.5), ylim=c(0,10), main="Densities for SLA, RMBL", col="springgreen4")
# points(density(log(dists[,8][dists[,1] %in% "RMBL" & dists[,2] %in% elev[2]])), col="yellowgreen", type="l")
# points(density(log(dists[,8][dists[,1] %in% "RMBL" & dists[,2] %in% elev[3]])), col="yellow", type="l") # only 2 sp here so v sketch don't do it
# points(density(log(dists[,8][dists[,1] %in% "RMBL" & dists[,2] %in% elev[4]])), col="orange", type="l") #same 2 sp also don't do it.


par(mfrow=c(1,1))
par(mfrow=c(2,3))

##for dN15
elev=sort(unique(dists$plotElev[dists[,1] %in% "Savegre"]))
plot(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8),xlim=c(0,2.5), main="Savegre", xlab="log(dN15)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[6]),])))), col="red4", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Guanacaste"]))
plot(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(0,2.5),main="Guanacaste", xlab="log(dN15)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[2]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[3]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[4]),])))), col="red", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Tamaulipas"]))
plot(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(0,2.5),, main="Tamaulipas", xlab="log(dN15)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
#points(density(log(dists[,8][dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[7]])), col="midnightblue", type="l") only 1 sp!

elev=sort(unique(dists$plotElev[dists[,1] %in% "Lemmon"]))
plot(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(0,2.5),, main="Lemmon", xlab="log(dN15)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[4]),])))), col="orange", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Sequoia"]))
plot(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(0,2.5),, main="Sequoia", xlab="log(dN15)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
points(density(log(as.vector(as.matrix(dists[,c(11,11+9,11+18,11+27,11+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[7]),])))), col="midnightblue", type="l")

##for np
par(mfrow=c(1,1))
par(mfrow=c(2,3))

elev=sort(unique(dists$plotElev[dists[,1] %in% "Savegre"]))
plot(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[1]),])))), ylim=c(0,3),xlim=c(1.5,4.5), main="Savegre", xlab="log(leaf N:P)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[6]),])))), col="red4", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Guanacaste"]))
plot(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(1.5,4.5),main="Guanacaste", xlab="log(leaf N:P)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[2]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[3]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[4]),])))), col="red", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Tamaulipas"]))
plot(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(1.5,4.5), main="Tamaulipas", xlab="log(leaf N:P)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
#points(density(log(dists[,8][dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[7]])), col="midnightblue", type="l") only 1 sp!

elev=sort(unique(dists$plotElev[dists[,1] %in% "Lemmon"]))
plot(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(1.5,4.5), main="Lemmon", xlab="log(leaf N:P)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[4]),])))), col="orange", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Sequoia"]))
plot(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(1.5,4.5), main="Sequoia", xlab="log(leaf N:P)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
points(density(log(as.vector(as.matrix(dists[,c(12,12+9,12+18,12+27,12+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[7]),])))), col="midnightblue", type="l")


##for SSD
par(mfrow=c(1,1))
par(mfrow=c(2,3))

elev=sort(unique(dists$plotElev[dists[,1] %in% "Savegre"]))
plot(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8),xlim=c(-2,.2), main="Savegre", xlab="log(SSD)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[6]),])))), col="red4", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Guanacaste"]))
plot(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(-2,.2),main="Guanacaste", xlab="log(SSD)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[2]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[3]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[4]),])))), col="red", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Tamaulipas"]))
plot(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(-2,.2), main="Tamaulipas", xlab="log(SSD)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
#points(density(log(dists[,8][dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[7]])), col="midnightblue", type="l") only 1 sp!

elev=sort(unique(dists$plotElev[dists[,1] %in% "Lemmon"]))
plot(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(-2,.2), main="Lemmon", xlab="log(SSD)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[4]),])))), col="orange", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Sequoia"]))
plot(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(-2,.2), main="Sequoia", xlab="log(SSD)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
points(density(log(as.vector(as.matrix(dists[,c(13,13+9,13+18,13+27,13+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[7]),])))), col="midnightblue", type="l")



##for CN
par(mfrow=c(1,1))
par(mfrow=c(2,3))

elev=sort(unique(dists$plotElev[dists[,1] %in% "Savegre"]))
plot(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[1]),])))), ylim=c(0,3),xlim=c(.2,4.6), main="Savegre", xlab="log(leaf C:N)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[6]),])))), col="red4", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Guanacaste"]))
plot(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(.2,4.6),main="Guanacaste", xlab="log(leaf C:N)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[2]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[3]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[4]),])))), col="red", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Tamaulipas"]))
plot(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(.2,4.6), main="Tamaulipas", xlab="log(leaf C:N)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
#points(density(log(dists[,8][dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[7]])), col="midnightblue", type="l") only 1 sp!

elev=sort(unique(dists$plotElev[dists[,1] %in% "Lemmon"]))
plot(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(.2,4.6), main="Lemmon", xlab="log(leaf C:N)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[4]),])))), col="orange", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Sequoia"]))
plot(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[1]),])))), ylim=c(0,2.8), xlim=c(.2,4.6), main="Sequoia", xlab="log(leaf C:N)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
points(density(log(as.vector(as.matrix(dists[,c(14,14+9,14+18,14+27,14+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[7]),])))), col="midnightblue", type="l")

##for height
par(mfrow=c(1,1))
par(mfrow=c(2,3))

elev=sort(unique(dists$plotElev[dists[,1] %in% "Savegre"]))
plot(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[1]),])))), ylim=c(0,1),xlim=c(0,4.5), main="Savegre", xlab="log(height)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[6]),])))), col="red4", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Guanacaste"]))
plot(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[1]),])))), ylim=c(0,1), xlim=c(0,4.5),main="Guanacaste", xlab="log(height)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[2]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[3]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[4]),])))), col="red", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Tamaulipas"]))
plot(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[1]),])))), ylim=c(0,1), xlim=c(0,4.5), main="Tamaulipas", xlab="log(height)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
#points(density(log(dists[,8][dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[7]])), col="midnightblue", type="l") only 1 sp!

elev=sort(unique(dists$plotElev[dists[,1] %in% "Lemmon"]))
plot(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[1]),])))), ylim=c(0,1), xlim=c(0,4.5), main="Lemmon", xlab="log(height)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[4]),])))), col="orange", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Sequoia"]))
plot(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[1]),])))), ylim=c(0,1), xlim=c(0,4.5), main="Sequoia", xlab="log(height)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
points(density(log(as.vector(as.matrix(dists[,c(9,9+9,9+18,9+27,9+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[7]),])))), col="midnightblue", type="l")

##for Seed Mass
par(mfrow=c(1,1))
par(mfrow=c(2,3))

elev=sort(unique(dists$plotElev[dists[,1] %in% "Savegre"]))
plot(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[1]),])))), ylim=c(0,.6),xlim=c(-3,10.6), main="Savegre", xlab="log(seed mass)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Savegre" & dists[,2] %in% elev[6]),])))), col="red4", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Guanacaste"]))
plot(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[1]),])))), ylim=c(0,.6), xlim=c(-3,10.6),main="Guanacaste", xlab="log(seed mass)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[2]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[3]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Guanacaste" & dists[,2] %in% elev[4]),])))), col="red", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Tamaulipas"]))
plot(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[1]),])))), ylim=c(0,.6), xlim=c(-3,10.6), main="Tamaulipas", xlab="log(seed mass)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
#points(density(log(dists[,8][dists[,1] %in% "Tamaulipas" & dists[,2] %in% elev[7]])), col="midnightblue", type="l") only 1 sp!

elev=sort(unique(dists$plotElev[dists[,1] %in% "Lemmon"]))
plot(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[1]),])))), ylim=c(0,.6), xlim=c(-3,10.6), main="Lemmon", xlab="log(seed mass)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Lemmon" & dists[,2] %in% elev[4]),])))), col="orange", type="l")

elev=sort(unique(dists$plotElev[dists[,1] %in% "Sequoia"]))
plot(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[1]),])))), ylim=c(0,.6), xlim=c(-3,10.6), main="Sequoia", xlab="log(seed mass)", col="springgreen4")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[2]),])))), col="yellowgreen", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[3]),])))), col="yellow", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[4]),])))), col="orange", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[5]),])))), col="orangered", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[6]),])))), col="red4", type="l")
points(density(log(as.vector(as.matrix(dists[,c(10,10+9,10+18,10+27,10+36)][(dists[,1] %in% "Sequoia" & dists[,2] %in% elev[7]),])))), col="midnightblue", type="l")

