#WORKING VERSIONS OF ALL FUNCTIONS AS OF 7-6-15!!! 

#REQUIRED PACKAGES:  hypervolume, mice, lattice, picante (?)
#index:
#SETTING UP:  gm_mean @ 8, locs @ ~15, imps @ ~ 70.
#ANALYSIS:  mats @ 135, mats.nest @ 205, beta @ 255, alpha @ 300, beta.p @ 365, alpha.p @ 425.

gm_mean = function(x, na.rm=TRUE){    # this function taken from http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}




#LOCS FUNCTION
#INPUTS VARIABLE VECTOR, DF, OUTPUTS SUBSETS OF DATA BY LOCALES. 
#STATUS: FUNCTIONAL. COULD USE TWEAKING FOR ELEGANCE, WIDER APPLICABILITY.

locs=function(vars, df){   #vars is a vector of column numbers or names for desired trait axes, df is data frame to call from
  #returns a list of subsetted dataframes in ALPHABETICAL order (Guan first, Tamaul last!)
  #Guanacoste Locale: HACHAL, Cacao-1, Cacao-2, Boshum, PH (Playa Hachal)
  grows=c("Cacao-1","Cacao-2", "Boshum","BOSHUM","HACHAL", "PH", "Maritza","Maritza-1")
  cols=c(vars, 10)
  guan.loc=df[(df$Plot_Name %in% grows), cols]
  guan.loc=na.exclude(guan.loc)
  guan.loc=abs(guan.loc)
  guan.loc[,1:length(vars)]=log(guan.loc[,1:length(vars)])
  
  #Lemmon Locale. LSV, SkiValley, RoseCanyon (not yet), ChihuahuaP (not yet)
  lrows=c("LSV","Ski_Valley","SkiValley", "RoseCanyon", "ChihuahuaP")
  lemm.loc=df[df$Plot_Name %in% lrows, cols]
  lemm.loc=na.exclude(lemm.loc) 
  lemm.loc=abs(lemm.loc)
  lemm.loc[,1:length(vars)]=log(lemm.loc[,1:length(vars)])   
  
  #RMBL Locale:  Plot_Name WG-1, wg4, Gothic_Asp, AlmontCO
  rrows=c("WG-1", "W.Gulch 1", "wg4","WG-4", "Gothic_Asp","Gothic Asp", "AlmontCO")
  rock.loc=df[df$Plot_Name %in% rrows, cols]  
  rock.loc=na.exclude(rock.loc)
  rock.loc=abs(rock.loc)
  rock.loc[,1:length(vars)]=log(rock.loc[,1:length(vars)])
  
  #Sequoia Locale. "SNP1","SNP2","SNP3","SNP4","SNP5","SNP6","SNP7","SNP8". 
  srows=c("SNP1","SNP2","SNP3","SNP4","SNP5","SNP6","SNP7","SNP8")
  seq.loc=df[df$Plot_Name %in% srows, cols]
  seq.loc=na.exclude(seq.loc) 
  seq.loc=abs(seq.loc)
  seq.loc[,1:length(vars)]=log(seq.loc[,1:length(vars)])
  
  
  #Savegre Locale. Mantonio, Nara, Tinamu, SV-2, SV-1, Cerro
  sarows=c("SV-1","SV-2","Mantonio","Cerro","Nara","Tinamu")
  sav.loc=df[df$Plot_Name %in% sarows, cols]
  sav.loc=na.exclude(sav.loc) 
  sav.loc=abs(sav.loc)
  sav.loc[,1:length(vars)]=log(sav.loc[,1:length(vars)])                                                   
  #only two elevations
  
  #Taumalipas Locale: LAMARCELA, MIQUIHUA LAPENA ElCujete LaGloria SanJose Cielo
  trows=c("LAMARCELA", "MIQUIHUA", "MIQUIHUANA", "LAPENA", "ElCujete", "LaGloria", "SanJose", "Cielo", "Cielo750")
  taum.loc=df[df$Plot_Name %in% trows, cols]
  taum.loc=na.exclude(taum.loc)
  taum.loc=abs(taum.loc)
  taum.loc[,1:length(vars)]=log(taum.loc[,1:length(vars)])
  
  return(list(guan.loc,lemm.loc, rock.loc,sav.loc,seq.loc,taum.loc))
  
}




##IMPS  script and FUNCTION-- to impute m datasets 



# vars=tr.std[c(###)]     #put the desired tr variables in here!   
# df.fill.orig=df[,c(4,10,8,13,15)]   #gets the 5 df cols state/prov, elev, lat, genus, and species.
# df.fill=df.fill.orig
# df.fill[,6:(5+length(vars))]=NA   #sets up the desired number of columns, empty
# colnames(df.fill)[6:length(df.fill)]=vars
# df.fill[,c(###)]=df[,c(###)]   #import columns from df for variables that we have data for!! v important that u do this!!


  #IMP FUNCTION-- SEARCHES TRY AND DF FOR SPECIES MATCHES, LOCAL GENUS MATCHES, AND OVERALL GENUS MATCHES, THEN RUNS MICE IMPUTATION ON ELEV, LAT, AND TRS.
    #INPUT df.fill as above, with vars as cols 6:n, elev and lat as 2:3, vars as in trait names, and m desired imputations.
  #OUTPUT: a mice.object (mids). also warnings if there are <50% filled obs when it begins mice. if desired, you can switch the hasing on the 'return' statements and get only pulled try data.

imp=function(df.fill, vars, m){  #a data frame with vars in columns to fill set up as in df.fill, and a list of var names as in try.data
  #df.fill=df
  tr.ex=vars
  for(i in 1:nrow(df.fill)){
    for(j in 6:length(df.fill)){   #j in length vars-- we need to set up df.fill and tr to have the same vars in same order
      if(is.na(df.fill[i,j])==TRUE){
        
        #Species matches from df
        if(df.fill$Taxon[i]!="" && length(na.exclude(df.fill[,j][df.fill$Taxon %in% df.fill[i,5]]))!=0){
          df.fill[i,j]=gm_mean(na.exclude(df.fill[,j][df.fill$Taxon %in% df.fill[i,5]]))
          next
        }
        #Species matches from TRY
        if(df.fill$Taxon[i]!="" && length(na.exclude(try.data$StdValue[try.data$TraitName %in% tr.ex[j-5] & try.data$AccSpeciesName %in% df.fill[i,5]]))!=0){
          df.fill[i,j]=gm_mean(na.exclude(try.data$StdValue[try.data$TraitName %in% tr.ex[j-5] & try.data$AccSpeciesName %in% df.fill[i,5]]))
          next
        }
        #Genus matches from the same state/province
        if((df.fill$Genus[i]!="" || is.na(df.fill$Genus[i]==FALSE))
           && length(na.exclude(df.fill[,j][df.fill$Genus %in% df.fill[i,4] & df.fill$State.province %in% df.fill[i,1]]))!=0){
          df.fill[i,j]=gm_mean(na.exclude(df.fill[,j][df.fill$Genus %in% df.fill[i,4] & df.fill$State.province %in% df.fill[i,1]]))
          next
        }
        #Genus matches from overall (including TRY??)
        if((df.fill$Genus[i]!="" || is.na(df.fill$Genus[i]==FALSE))
           && (length(na.exclude(c(df.fill[,j][df.fill$Genus %in% df.fill[i,4]], try.data[grep(df.fill$Genus[i], try.data$AccSpeciesName),]$StdValue[try.data[grep(df.fill$Genus[i], try.data$AccSpeciesName),]$TraitName %in% tr.ex[j-5]])))!=0)){
          df.fill[i,j]=gm_mean(na.exclude(c(df.fill[,j][df.fill$Genus %in% df.fill[i,4]], try.data[grep(df.fill$Genus[i], try.data$AccSpeciesName),]$StdValue[try.data[grep(df.fill$Genus[i], try.data$AccSpeciesName),]$TraitName %in% tr.ex[j-5]])))
          next
        }
        
        
      }}}
  
  for(i in 6:length(df.fill)){
    print(c(colnames(df.fill)[i], (length(na.exclude(df.fill[,i]))/length(df.fill[,i]))*100))
  }
  
  #return(df.fill)  
  
  mice.obj=mice(df.fill[,c(2,3,6:length(df.fill))], m=m)   #i think this will impute holes in the lat and elevation but like those dont go into the final so it's fine???
  return(mice.obj)
}


#MATS FUNCTION== WORKING, TAKES INPUT AS IN TAUM.LOC. DOES NOT LABEL GRAPHS OR STORE PS BUT WE NEVER USE THIS ANYWAY SO IDGAF

mats=function(loc, mode=c("kernel","overlap")){    #loc is locale grid with column names as in traits.all and elevation as terminal column. (as generated by locs)
  elevs=sort(unique(loc$Elevation.m.))
  names=c("Guanacaste","Lemmon","RMBL","Rio Savegre","Seq. Natl. Park", "Tamaulipas")
  locs=list()
  length(locs)=length(elevs)
  for(i in 1:(length(locs))){
    locs[[i]]=loc[loc$Elevation.m. %in% elevs[i], 1:(length(loc)-1)]
  }
 ts=numeric(length(elevs))
 ts[1:length(ts)]=NA
  for(i in 1:length(locs)){
    if (nrow(locs[[i]])<5){
      ts[i]=i
    }
}
ts=na.exclude(ts)
if(length(ts)!=0){
  locs=locs[-ts]
  elevs=elevs[-ts]  }

if(length(elevs)>1){
  e.comb=combn(elevs, 2)
  diffs=numeric(length(e.comb)/2)
  for (i in 1:length(diffs)){
    diffs[i]=abs(e.comb[[1,i]]-e.comb[[2,i]])
  }
  
  rows=numeric(length(elevs))
  for(i in 1:length(elevs)){
    rows=c("Low", 2:(i-1), "High")
  }
  
  vols=list()
  length(vols)=length(locs) 
  for(i in 1:(length(vols))){
    vols[[i]]=hypervolume(locs[[i]], repsperpoint=NULL, 1.2, .1, name=rows[i], warnings=F)
  }
  
  vol.com=combn(vols, 2)
  
  if(mode=="kernel"){
    dists=numeric(ncol(vol.com))
    for (i in 1:length(dists)){
      dists[i]=hypervolume_distance(vol.com[[1,i]], vol.com[[2,i]])
      
    }
    if(length(dists)>=3){
      dists=as.numeric(dists)
      r=cor(diffs,dists)
      plot(diffs, dists, sub=r^2, xlab="Elevational Distance (m)", ylab="Hypervolume Distance")
      eq=line(diffs,dists)
      abline(coef(eq))
    }}
  if(mode=="overlap"){
    dists=list()
    length(dists)=ncol(vol.com)
    for(i in 1:length(dists)){
      dists[[i]]=hypervolume_sorensen_overlap(hypervolume_set(vol.com[[1,i]],vol.com[[2,i]], check_memory=FALSE))
    }
    
    if(length(dists)>=3){
      dists=as.numeric(dists)
      r=cor(diffs,dists)
      plot(diffs, dists, sub=r^2, xlab="Elevational Distance (m)", ylab="Hypervolume Overlap", ylim=c(0,1))
      eq=line(diffs,dists)
      abline(coef(eq))
    }
  }
  return(list(diffs,dists))
}
}



#NESTED MATS FUNCTION-- GENERATES LISTS OF ELEVATIONAL AND FUNCTIONAL DISTANCES as with mats, but does not plot. used in the beta function.
  #REQUIRES: A LOC AS IN TAUM.LOC, WITH NUMERIC TRAITS IN COLUMNS AND ELEVATION AS TERMINAL COLUMN. 
  #RETURNS: a list of 2== outs[[1]] is elevational distances, outs[[2]] is hypervolume distances or overlap.


mats.nest=function(loc, mode=c("kernel","overlap")){    #loc is locale grid with column names as in traits.all and elevation as terminal column. (as generated by locs)
  elevs=sort(unique(loc$Elevation.m.))
  locs=list()
  length(locs)=length(elevs)
  for(i in 1:(length(locs))){
    locs[[i]]=loc[loc$Elevation.m. %in% elevs[i], 1:(length(loc)-1)]   #sets up dfs for traits at each elevation within a list
  }
  
  ts=numeric(length(elevs))
  ts[1:length(ts)]=NA
  for(i in 1:length(locs)){
    if (nrow(locs[[i]])<5){
      ts[i]=i
    }
  }
  ts=na.exclude(ts)
  if(length(ts)!=0){
  locs=locs[-ts]
  elevs=elevs[-ts]  }
  
  if(length(elevs)>1){
  e.comb=combn(elevs, 2)    #sets up elevational distance vector diffs with pairwise elevation changes stored
  diffs=numeric(ncol(e.comb))
  for (i in 1:length(diffs)){
    diffs[i]=abs(e.comb[[1,i]]-e.comb[[2,i]])
  }
  
  rows=numeric(length(elevs))
  for(i in 1:length(elevs)){
    rows=c("Low", 2:(i-1), "High")
  }
  
  vols=list()
  length(vols)=length(locs) 
  for(i in 1:(length(vols))){
    vols[[i]]=hypervolume(locs[[i]], repsperpoint=NULL, 1.2, .1, name=rows[i], warnings=F)
  }
  
  vol.com=combn(vols, 2)
  
  if(mode=="kernel"){
    dists=numeric(ncol(vol.com))
    for (i in 1:length(dists)){
      dists[i]=hypervolume_distance(vol.com[[1,i]], vol.com[[2,i]])
    }}
  if(mode=="overlap"){
    dists=list()
    length(dists)=ncol(vol.com)
    for(i in 1:length(dists)){
      dists[[i]]=hypervolume_sorensen_overlap(hypervolume_set(vol.com[[1,i]],vol.com[[2,i]], check_memory=FALSE))
    }}
  dists=as.numeric(dists)
  return(list(diffs,dists))
  }
}



#BETA FUNCTION
#INPUTS VARIABLE VECTOR (numeric, columns as in df), DATAFRAME. 
#OUTPUTS: list of 2== [[1]] is a list, 6 by 2-- elev and hypervol dists for each of 6 locales. [[2]] is a 3 by 6 matrix of P, intercept, and slope of linreg for each loc.
#STATUS: FUNCTIONAL. 
beta=function(vars, df, mode=c("kernel","overlap")){
  locs=locs(vars, df)
  outs=list()
  names=c("Guanacaste","Lemmon","RMBL","Rio Savegre","Seq. Natl. Park", "Tamaulipas")
  p=numeric(6)
  p[1:6]=NA
  sl=p
  int=p
  
  for(i in 1:length(locs)){
    if(nrow(locs[[i]])==0){
      next
    }else
      
      outs[[i]]=mats.nest(locs[[i]], mode=mode)
    
    if(length(outs[[i]][[1]])<3){
      next
    }else
      #outs=as.vector(outs)
    r=cor(outs[[i]][[1]],outs[[i]][[2]])
    p[i]=cor.test(outs[[i]][[1]],outs[[i]][[2]])$p.value
    if(mode=="kernel"){
      plot(outs[[i]][[1]],outs[[i]][[2]], main=names[i], sub=bquote(~P==.(p[i])), xlab="Elevational Distance (m)", ylab="Functional Distance")
      eq=line(outs[[i]][[1]],outs[[i]][[2]])
      abline(coef(eq))
      }else
        plot(outs[[i]][[1]],outs[[i]][[2]], main=names[i], sub=bquote(~P==.(p[i])), xlab="Elevational Distance (m)", ylab="Functional Overlap", ylim=c(0,1))
    eq=line(outs[[i]][[1]],outs[[i]][[2]])
    abline(coef(eq))
    int[i]=coef(eq)[1]
    sl[i]=coef(eq)[2]
  }
  out=list(outs, cbind(p,int,sl))
  return(out)
}


#ALPHA FUNCTION-- input vector of desired variable col numbers and dataframe to draw from
#output: [[1]] a list of hypervolume volumes and elevations for each locale by elevation, and [[2]] a 3by6 matrix of P,int,slp for each locale.
#STATUS: FUNCTIONAL.  COULD USE TWEAKING FOR STORAGE OF HYPERS.

alpha=function(vars, df){
  
  subs=locs(vars, df)  #this uses the locs function I've written to subset the data by locales. This function is not directly applicable to other datasets, but can easily be adapted.
  
  elevs=list()  #these are all lists that will be filled and used later in the function.
  length(elevs)=length(subs)
  #hypers=list()
  comms=list()
  length(comms)=length(subs)
  vols=list()
  length(vols)=length(subs)
  names=c("Guanacaste","Lemmon","RMBL","Rio Savegre","Seq. Natl. Park", "Tamaulipas")
  p=numeric(6)
  p[1:6]=NA
  sl=p
  int=p
  
  for(i in 1:length(subs)){  #generates a single list of elevations for locales-- G to T, low to high.
    elevs[[i]]=sort(unique(subs[[i]]$Elevation.m.))
  } 
  
  for(i in 1:length(comms)){
    if(nrow(subs[[i]])!=0){
      
      for(j in 1:length(elevs[[i]])){
        comms[[i]][[j]]=subs[[i]][(subs[[i]]$Elevation.m. %in% elevs[[i]][j]),]     #subsets subs into elevation sets.
        
        if (nrow(comms[[i]][[j]])<5){  #discards communities with very few observations
          vols[[i]][[j]]=NA
          elevs[[i]][[j]]=NA
          next 
        } 
        
        a=hypervolume(comms[[i]][[j]][1:length(vars)],repsperpoint=NULL, 1.2, .1, name=c(names[i],elevs[[i]][[j]]), warnings=F)
        #hypers[[i]][[j]]=a
        vols[[i]][[j]]=a@Volume   #generates (but does not store) hypervolumes for each elevational community, extracts and stores the volumes of these in a list.
        
      }
      
        if(length(na.exclude(elevs[[i]]))<3){
          next
          }else
          #vols[[i]]=as.numeric(vols[[i]])
          p[i]=cor.test(elevs[[i]],vols[[i]])$p.value
          int[i]=lm(vols[[i]]~elevs[[i]])$coef[1]
          sl[i]=lm(vols[[i]]~elevs[[i]])$coef[2]
          plot(elevs[[i]],vols[[i]], main=names[i], sub=bquote(~P==.(p[i])), xlab="Elevation", ylab="Alpha Div")   #generates 6 plots, with the alpha diversity (hypervolume vol) of elevational communities plotted against elevation for each locale.
          abline(int[i],sl[i])
          
        
        #plot(elevs[[i]],vols[[i]], main=names[i], xlab="Elevation", ylab="Alpha Div")
      
      
    }}
  outs=list()
  length(outs)=6
  for(i in 1:6){
    outs[[i]]=list(elevs[[i]],vols[[i]])
  }
  return(list(outs, cbind(p,int,sl)))  #returns values for community volumes- ideally, this will transition to returning the actual hypervolumes in a hypervolume list!
}


#BETA ANALYSES FOR IMPUTED DATASETS!!
  #input a mice object with imputation for elevation and latitude as cols 1:2 of complete(), traits as other cols. optional vars w/in as 16:etc, optional mode.
  #outputs 1) a dataframe that has the m different Bdiv vals for each individual point at each locale, 2) an m by 18 matrix with ps, intercepts, and slps for the m regressions at 6 locales.
beta.p=function(imps, m=imps$m, vars=c(16:(15+length(complete(imps))-2)), mode=c("kernel","overlap")){
 
  bs.full=list()
  length(bs.full)=m
  bs=bs.full
  p=bs.full
  iter=bs.full
  df.temp=df[,1:15]
  for( i in 1:m){
    df.temp[,16:(15+length(vars))]=complete(imps, action=i)[,-c(1:2)]     #for imps w elev and lat!!!
    colnames(df.temp)[16:length(df.temp)]=vars
    bs.full[[i]]=beta(vars,df.temp, mode=mode)
    
  }
  for(i in 1:m){
    bs[[i]]=bs.full[[i]][[1]]
    p[[i]]=bs.full[[i]][[2]]
    
  }
  
  x=list()
  length(x)=6
  y=list()
  length(y)=6
  
  for (j in 1:6){  #for all locales, pulling single spot
    for(k in 1:length(bs[[i]][[j]][[2]])){   #points at each locale
      x[[j]][[k]]=numeric(length=m)
      y[[j]][[k]]=numeric(length=m)
    }}
  for (j in 1:6){  #for all locales, pulling single spot
    for(k in 1:length(bs[[i]][[j]][[2]])){
      for(i in 1:length(bs)){
        x[[j]][[k]][i]=bs[[i]][[j]][[2]][k]
        y[[j]][[k]][i]=bs[[i]][[j]][[1]][k]
      }}}
  
  sites=c(rep("Guanacaste",m*3),rep("Lemmon", m*1),rep("RMBL",m*6),rep("Rio Sav",m*1),rep("Seq Natl Park",m*21),rep("Tamaulipas",m*10))
  df.pool=data.frame(unlist(x),unlist(y), sites, rep(1:m))
  if(mode=="kernel"){
    colnames(df.pool)=c("Functional distance", "Elevation change","Locale", "imputation")
  }
  if(mode=="overlap"){
  colnames(df.pool)=c("Functional overlap", "Elevation change","Locale", "imputation")
  }
  ps=matrix(nrow=m, ncol=6)
  slps=ps
  ints=ps
  for(i in 1:m){  
    for(j in 1:6){
      ps[i,j]=p[[i]][j,1]
      slps[i,j]=p[[i]][j,3]
      ints[i,j]=p[[i]][j,2]
    }}
  #ps=ps[,-1]
  colnames(ps)=c("Guanacaste P","Lemmon P", "RMBL P", "Rio Sav P", "Seq Natl P", "Tamaul. P")
  colnames(slps)=c("Guanacaste Slp","Lemmon Slp","RMBL Slp","Rio Sav Slp","Seq Natl Slp","Tamaul. Slp")
  colnames(ints)=c("Guanacaste int","Lemmon int","RMBL int","Rio Sav int","Seq Natl int","Tamaul. int")
  bwplot(df.pool[,1] ~ df.pool[,2] | df.pool[,3])
  
  return(list(df.pool, cbind(ps,ints,slps))) 
}


#ALPHA ANALYSES FOR IMPUTED DATASETS.
  #input a mice obj with elev and lat at 1:2 of complete(), optional vars as 16:n (but u really cant do anything abt them rn oops)
  #output as in beta.p, a df of vals for imputed points (which u can plot, etc), and a matrix of p, int, and slope for imputations of regressions.


alpha.p=function(imps, m=imps$m, vars=c(16:(15+length(complete(imps))-2))){
  
  as.full=list()
  length(as.full)=m
  as=as.full
  p=as.full
  df.temp=df[,1:15]
  for( i in 1:m){
    df.temp[,16:(15+length(vars))]=complete(imps, action=i)[,-c(1:2)]     #for imps w elev and lat!!!
    #colnames(df.temp)[16:length(df.temp)]=vars
    as.full[[i]]=alpha(vars,df.temp)
    
  }
  for(i in 1:m){
    as[[i]]=as.full[[i]][[1]]
    p[[i]]=as.full[[i]][[2]]
  }
  
  x=list()
  length(x)=6
  y=list()
  length(y)=6
  for(i in 1:m){   #the for loop in question???
    for (j in 1:6){  #for all locales, pulling single spot
      for(k in 1:length(as[[i]][[j]][[2]])){   #points at each locale
        x[[j]][[k]]=numeric(length=m)
        y[[j]][[k]]=numeric(length=m)
      }}}
  for (j in 1:6){  #for all locales, pulling single spot
    for(k in 1:length(as[[i]][[j]][[2]])){
      for(i in 1:length(as)){
        x[[j]][[k]][i]=as[[i]][[j]][[2]][k]
        y[[j]][[k]][i]=as[[i]][[j]][[1]][k]
      }}}
  
  sites=c(rep("Guanacaste",m*3),rep("Lemmon", m*2),rep("RMBL",m*4),rep("Rio Sav",m*2),rep("Seq Natl Park",m*7),rep("Tamaulipas",m*6))
  df.pool=data.frame(unlist(x),unlist(y), sites, rep(1:m))  
  colnames(df.pool)=c("Alpha Div","Elevation","Locale", "imputation")
  
  ps=matrix(nrow=m, ncol=6)
  slps=ps
  ints=ps
  for(i in 1:m){  
    for(j in 1:6){
      ps[i,j]=p[[i]][j,1]
      slps[i,j]=p[[i]][j,3]
      ints[i,j]=p[[i]][j,2]
    }}
  #ps=ps[,-1]
  colnames(ps)=c("Guanacaste P","Lemmon P", "RMBL P", "Rio Sav P", "Seq Natl P", "Tamaul. P")
  colnames(slps)=c("Guanacaste Slp","Lemmon Slp","RMBL Slp","Rio Sav Slp","Seq Natl Slp","Tamaul. Slp")
  colnames(ints)=c("Guanacaste int","Lemmon int","RMBL int","Rio Sav int","Seq Natl int","Tamaul. int")
  bwplot(df.pool[,1] ~ df.pool[,2] | df.pool[,3])
  
  return(list(df.pool, ps, cbind(ints,slps))) 
}

##STANDARDIZING BETA SCORES

#input a mice object with imputation for elevation and latitude as cols 1:2 of complete(), traits as other cols. optional vars w/in as 16:etc, optional mode.
#outputs a dataframe w/ real func diffs, randomized func diff, elevs and locales, and std scores.
b.rand=function(imps, m=imps$m, vars=c(16:(15+length(complete(imps))-2)), mode=c("kernel","overlap")){
  
  bs.full=list()
  length(bs.full)=m
  bs=bs.full
  p=bs.full
  df.temp=df[,1:15]
  for( i in 1:m){
    df.temp[,16:(15+length(vars))]=complete(imps, action=i)[,-c(1:2)]     #for imps w elev and lat!!!
    df.temp[,16:length(df.temp)]=randomizeMatrix(df.temp[,16:length(df.temp)], null.model="frequency")
    bs.full[[i]]=beta(vars,df.temp, mode=mode)
    
  }
  for(i in 1:m){
    bs[[i]]=bs.full[[i]][[1]]
    p[[i]]=bs.full[[i]][[2]]
  }
  
  x=list()
  length(x)=6
  y=list()
  length(y)=6
  
  for (j in 1:6){  #for all locales, pulling single spot
    for(k in 1:length(bs[[i]][[j]][[2]])){   #points at each locale
      x[[j]][[k]]=numeric(length=m)
      y[[j]][[k]]=numeric(length=m)
    }}
  for (j in 1:6){  #for all locales, pulling single spot
    for(k in 1:length(bs[[i]][[j]][[2]])){
      for(i in 1:length(bs)){
        x[[j]][[k]][i]=bs[[i]][[j]][[2]][k]
        y[[j]][[k]][i]=bs[[i]][[j]][[1]][k]
      }}}
  
  sites=c(rep("Guanacaste",m*3),rep("Lemmon", m*1),rep("RMBL",m*6),rep("Rio Sav",m*1),rep("Seq Natl Park",m*21),rep("Tamaulipas",m*10))
  df.pool=data.frame(unlist(x),unlist(y), sites)  
  if(mode=="kernel"){
    colnames(df.pool)=c("Functional distance", "Elevation change","Locale")
  }
  if(mode=="overlap"){
    colnames(df.pool)=c("Functional Overlap", "Elevation change","Locale")
  }  
  
  #   #ps=matrix(nrow=m, ncol=6)
  #   #slps=ps
  #   #ints=ps
  #   #for(i in 1:m){  
  #     for(j in 1:6){
  #       ps[i,j]=p[[i]][j,1]
  #       slps[i,j]=p[[i]][j,3]
  #       ints[i,j]=p[[i]][j,2]
  #     }}
  #   #ps=ps[,-1]
  #   colnames(ps)=c("Guanacaste P","Lemmon P", "RMBL P", "Rio Sav P", "Seq Natl P", "Tamaul. P")
  #   colnames(slps)=c("Guanacaste Slp","Lemmon Slp","RMBL Slp","Rio Sav Slp","Seq Natl Slp","Tamaul. Slp")
  #   colnames(ints)=c("Guanacaste int","Lemmon int","RMBL int","Rio Sav int","Seq Natl int","Tamaul. int")
  #   bwplot(df.pool[,1] ~ df.pool[,2] | df.pool[,3])
  #   
  return(df.pool) 
}

b.std=function(imps,m=imps$m, vars=c(16:(15+length(complete(imps))-2)), mode=c("kernel","overlap")){
  b.norm=beta.p(imps,vars,mode=mode)
  b.scr=b.rand(imps,vars,mode=mode)
  
  bs=cbind(b.norm[[1]][,1],b.scr)
  bs[,5]=abs((bs[,1]-bs[,2])/sd(bs[,2]))    #z scores it by whole random deviance
  
  #for(i in 1:nrow(bs)){
  #bs[,5]=abs((bs[i,1]-bs[i,2])/sd(bs[,2][bs[,3] %in% bs[i,3] & bs[,4] %in% bs[i,4]])
  #}
  colnames(bs)=c("Functional change","randomized change","Elevation change","Locale","Std Score")
  
  return(bs)
}



##SPECIES POOLED IMPUTATION, BETA AND ALPHA



# #simp for species imp    start needs: 5 cols--state/prov, elev, lat, genus, taxon
simp=function(start, df, m){  #start, df.fill with cols from df so that we can call from it w/out majorly reformatting everything, var names as in TRY, m
  tr.ex=colnames(df)[6:length(df)]
  start[,6:length(df)]=NA
  colnames(start)[6:length(df)]=colnames(df)[6:length(df)]
  for(i in 1:nrow(start)){
    for(j in 6:length(start)){   #j in length vars-- we need to set up df.fill and tr to have the same vars in same order
      if(is.na(start[i,j])==TRUE){   #it will always be true?? no it's for looping don't even worry abt it
        
        #Species matches from df
        if(start$Taxon[i]!="NULL" && length(na.exclude(df[,j][df$Taxon %in% start[i,5]]))!=0){
          start[i,j]=gm_mean(na.exclude(df[df$Taxon %in% start[i,5],j]))
          next
        }
        
        
        #Species matches from TRY
        if(start$Taxon[i]!="NULL" && length(na.exclude(try.data$StdValue[try.data$TraitName %in% tr.ex[j-5] & try.data$AccSpeciesName %in% start[i,5]]))!=0){
          start[i,j]=gm_mean(na.exclude(try.data$StdValue[try.data$TraitName %in% tr.ex[j-5] & try.data$AccSpeciesName %in% start[i,5]]))
          next
        }
        
        
        #Genus matches from the same state/province
        if(length(na.exclude(df[df$Genus %in% start[i,4] & df$State.province %in% start[i,1],j]))>0){
          start[i,j]=gm_mean(na.exclude(df[df$Genus %in% start[i,4] & df$State.province %in% start[i,1],j]))
          #start[i,j]="yes i am here"
          next
        }
        
        #         #Genus matches from overall (including TRY??)
        if(length(na.exclude(c(df[,j][df$Genus %in% start[i,4]], try.data[grep(start$Genus[i], try.data$AccSpeciesName),]$StdValue[try.data[grep(start$Genus[i], try.data$AccSpeciesName),]$TraitName %in% tr.ex[j-5]])))!=0){
          start[i,j]=gm_mean(na.exclude(c(df[,j][df$Genus %in% start[i,4]], try.data[grep(start$Genus[i], try.data$AccSpeciesName),]$StdValue[try.data[grep(start$Genus[i], try.data$AccSpeciesName),]$TraitName %in% tr.ex[j-5]])))
          next
        }
        
      }
    }}
  
  for(i in 6:length(start)){
    print(c(colnames(start)[i], (length(na.exclude(start[,i]))/length(start[,i]))*100))
  }
  
  #return(start)  
  
  mice.obj=mice(start[,c(2,3,6:length(start))], m=m)   #i think this will impute holes in the lat and elevation but like those dont go into the final so it's fine???
  return(mice.obj)
}




#z-transform for shits and giggles??
# zstart=test
# for(i in 6:length(zstart)){
#   zstart[,i]=(zstart[,i]-mean(na.exclude(zstart[,i])))/sd(na.exclude(zstart[,i]))
# }
# 
# smice=mice(zstart[c(2,3,6:8)],m=3)


#BETA ANALYSES FOR IMPUTED DATASETS with species lists!!
#requires the object plotNameforSbeta with plotNames, as generated by above script.
#input a mice object with imputation for elevation and latitude as cols 1:2 of complete(), traits as other cols. optional vars w/in as 16:etc, optional mode.
#outputs 1) a dataframe that has the m different Bdiv vals for each individual point at each locale, 2) an m by 18 matrix with ps, intercepts, and slps for the m regressions at 6 locales.
sbeta=function(imps, m=imps$m, vars=c(16:(15+length(complete(imps))-2)), mode=c("kernel","overlap")){
  bs.full=list()
  length(bs.full)=m
  bs=bs.full
  p=bs.full
  df.temp=as.data.frame(matrix(nrow=nrow(complete(imps)), ncol=15))
  df.temp[,10]=complete(imps)[,1]
  df.temp[,3]=plotNameforSbeta
  df.temp[,4]=start[,1]
  df.temp[,13:14]=start[,4:5]
  colnames(df.temp)=colnames(df)[1:15]
  df.temp[,16:(15+length(vars))]=NA
  for( i in 1:m){
    df.temp[,16:length(df.temp)]=complete(imps, action=i)[,-c(1:2)]     #for imps w elev and lat!!!
    colnames(df.temp)[16:length(df.temp)]=colnames(imps$data)[-c(1:2)]
    
    bs.full[[i]]=beta(vars,df.temp, mode=mode)
    
  }
  for(i in 1:m){
    bs[[i]]=bs.full[[i]][[1]]
    p[[i]]=bs.full[[i]][[2]]
  }
  
  x=list()
  length(x)=6
  y=list()
  length(y)=6
  for(i in 1:m){
    for (j in 1:6){  #for all locales, pulling single spot
      for(k in 1:length(bs[[i]][[j]][[2]])){   #points at each locale   
        x[[j]][[k]]=numeric(length=m)
        y[[j]][[k]]=numeric(length=m)
      }}}
  for (j in 1:6){  #for all locales, pulling single spot
    for(k in 1:length(bs[[i]][[j]][[2]])){
      for(i in 1:length(bs)){
        x[[j]][[k]][i]=bs[[i]][[j]][[2]][k]
        y[[j]][[k]][i]=bs[[i]][[j]][[1]][k]
      }}}
  
  sites=c(rep("Guanacaste",m*length(bs[[1]][[1]][[1]])),rep("Lemmon", m*length(bs[[1]][[2]][[1]])),rep("RMBL",m*length(bs[[1]][[3]][[1]])),rep("Rio Sav",m*length(bs[[1]][[4]][[1]])),rep("Seq Natl Park",m*length(bs[[1]][[5]][[1]])),rep("Tamaulipas",m*length(bs[[1]][[6]][[1]])))
  df.pool=data.frame(unlist(x),unlist(y), sites, rep(1:m))
  if(mode=="kernel"){
    colnames(df.pool)=c("Functional distance", "Elevation change","Locale", "imputation")
  }
  if(mode=="overlap"){
    colnames(df.pool)=c("Functional Overlap", "Elevation change","Locale", "imputation")
  }
  ps=matrix(nrow=m, ncol=6)
  slps=ps
  ints=ps
  for(i in 1:m){  
    for(j in 1:6){
      ps[i,j]=p[[i]][j,1]
      slps[i,j]=p[[i]][j,3]
      ints[i,j]=p[[i]][j,2]
    }}
  #ps=ps[,-1]
  colnames(ps)=c("Guanacaste P","Lemmon P", "RMBL P", "Rio Sav P", "Seq Natl P", "Tamaul. P")
  colnames(slps)=c("Guanacaste Slp","Lemmon Slp","RMBL Slp","Rio Sav Slp","Seq Natl Slp","Tamaul. Slp")
  colnames(ints)=c("Guanacaste int","Lemmon int","RMBL int","Rio Sav int","Seq Natl int","Tamaul. int")
  bwplot(df.pool[,1] ~ df.pool[,2] | df.pool[,3])
  
  return(list(df.pool, cbind(ps,ints,slps))) 
}


##salpha--

salpha=function(imps, m=imps$m, vars=c(16:(15+length(complete(imps))-2))){
  
  as.full=list()
  length(as.full)=m
  as=as.full
  p=as.full
  
  df.temp=as.data.frame(matrix(nrow=nrow(complete(imps)), ncol=15))
  df.temp[,10]=complete(imps)[,1]
  df.temp[,3]=plotNameforSbeta
  df.temp[,4]=start[,1]
  df.temp[,13:14]=start[,4:5]
  colnames(df.temp)=colnames(df)[1:15]
  df.temp[,16:(15+length(vars))]=NA
  
  for( i in 1:m){
    df.temp[,16:(15+length(vars))]=complete(imps, action=i)[,-c(1:2)]     #for imps w elev and lat!!!
    #colnames(df.temp)[16:length(df.temp)]=vars
    as.full[[i]]=alpha(vars,df.temp)
    
  }
  for(i in 1:m){
    as[[i]]=as.full[[i]][[1]]
    p[[i]]=as.full[[i]][[2]]
  }
  
  x=list()
  length(x)=6
  y=list()
  length(y)=6
  for(i in 1:m){   #the for loop in question???
    for (j in 1:6){  #for all locales, pulling single spot
      for(k in 1:length(as[[i]][[j]][[2]])){   #points at each locale
        x[[j]][[k]]=numeric(length=m)
        y[[j]][[k]]=numeric(length=m)
      }}}
  for (j in 1:6){  #for all locales, pulling single spot
    for(k in 1:length(as[[i]][[j]][[2]])){
      for(i in 1:length(as)){
        x[[j]][[k]][i]=as[[i]][[j]][[2]][k]
        y[[j]][[k]][i]=as[[i]][[j]][[1]][k]
      }}}
  
  sites=c(rep("Guanacaste",m*length(as[[1]][[1]][[1]])),rep("Lemmon", m*length(as[[1]][[2]][[1]])),rep("RMBL",m*length(as[[1]][[3]][[1]])),rep("Rio Sav",m*length(as[[1]][[4]][[1]])),rep("Seq Natl Park",m*length(as[[1]][[5]][[1]])),rep("Tamaulipas",m*length(as[[1]][[6]][[1]])))
  
  df.pool=data.frame(unlist(x),unlist(y), sites, rep(1:m))  
  colnames(df.pool)=c("Alpha Div","Elevation","Locale", "imputation")
  
  ps=matrix(nrow=m, ncol=6)
  slps=ps
  ints=ps
  for(i in 1:m){  
    for(j in 1:6){
      ps[i,j]=p[[i]][j,1]
      slps[i,j]=p[[i]][j,3]
      ints[i,j]=p[[i]][j,2]
    }}
  #ps=ps[,-1]
  colnames(ps)=c("Guanacaste P","Lemmon P", "RMBL P", "Rio Sav P", "Seq Natl P", "Tamaul. P")
  colnames(slps)=c("Guanacaste Slp","Lemmon Slp","RMBL Slp","Rio Sav Slp","Seq Natl Slp","Tamaul. Slp")
  colnames(ints)=c("Guanacaste int","Lemmon int","RMBL int","Rio Sav int","Seq Natl int","Tamaul. int")
  bwplot(df.pool[,1] ~ df.pool[,2] | df.pool[,3])
  
  return(list(df.pool, cbind(ps,ints,slps))) 
}

#MAKE SURE THAT ALL PARENS IN try.data$Trait.Name are gsub'd out!!
#try.data$TraitName=gsub("\\(","",try.data$TraitName)
#try.data$TraitName=gsub("\\)","",try.data$TraitName)


sb.rand=function(imps, its=999, m=imps$m, vars=c(16:(15+length(complete(imps))-2)), mode=c("kernel","overlap")){
  bs.full=list()
  length(bs.full)=its
  bs=bs.full
  p=bs.full
  
  pool=matrix(nrow=nrow(imps$data), ncol=(ncol(imps$data)-2))
  colnames(pool)=colnames(imps$data)[3:ncol(imps$data)]
  for(i in 1:ncol(pool)){
    pool[,i]=apply(complete(imps, action="broad")[,grep(colnames(pool)[i],colnames(complete(imps, action="broad")))], 1, mean)
  }
  
  df.temp=as.data.frame(matrix(nrow=nrow(complete(imps)), ncol=15))
  df.temp[,10]=complete(imps)[,1]
  df.temp[,3]=plotNameforSbeta
  df.temp[,4]=start[,1]
  df.temp[,13:14]=start[,4:5]
  colnames(df.temp)=colnames(df)[1:15]
  df.temp[,16:(15+length(vars))]=NA
    df.temp[,16:length(df.temp)]=pool    #for imps w elev and lat!!! also weights--get rid of this when done w 3 var!!
for( i in 1:its){  
  df.temp[,16:length(df.temp)][df.temp$State.province %in% "Guanacaste",]=df.temp[,16:length(df.temp)][df.temp$State.province %in% "Guanacaste",][sample(nrow(df.temp[df.temp$State.province %in% "Guanacaste",])),]
    df.temp[,16:length(df.temp)][df.temp$State.province %in% "Arizona",]=df.temp[,16:length(df.temp)][df.temp$State.province %in% "Arizona",][sample(nrow(df.temp[df.temp$State.province %in% "Arizona",])),]
    df.temp[,16:length(df.temp)][df.temp$State.province %in% "Colorado",]=df.temp[,16:length(df.temp)][df.temp$State.province %in% "Colorado",][sample(nrow(df.temp[df.temp$State.province %in% "Colorado",])),]
    df.temp[,16:length(df.temp)][df.temp$State.province %in% "California",]=df.temp[,16:length(df.temp)][df.temp$State.province %in% "California",][sample(nrow(df.temp[df.temp$State.province %in% "California",])),]
  df.temp[,16:length(df.temp)][df.temp$State.province %in% "Tamaulipas",]=df.temp[,16:length(df.temp)][df.temp$State.province %in% "Tamaulipas",][sample(nrow(df.temp[df.temp$State.province %in% "Tamaulipas",])),]
  df.temp[,16:length(df.temp)][df.temp$State.province %in% c("Puntarenas","San Jose"),]=df.temp[,16:length(df.temp)][df.temp$State.province %in% c("Puntarenas","San Jose"),][sample(nrow(df.temp[df.temp$State.province %in% c("Puntarenas","San Jose"),])),]
    
    #colnames(df.temp)[16:length(df.temp)]=colnames(imps$data)[-c(1:2)]
    bs.full[[i]]=beta(vars,df.temp, mode=mode)
}
    
  for(i in 1:its){
    bs[[i]]=bs.full[[i]][[1]]
    p[[i]]=bs.full[[i]][[2]]
  }
  
  x=list()
  length(x)=6
  y=list()
  length(y)=6
  for(i in 1:its){
    for (j in 1:6){  #for all locales, pulling single spot
      for(k in 1:length(bs[[i]][[j]][[2]])){   #points at each locale   
        x[[j]][[k]]=numeric(length=its)
        y[[j]][[k]]=numeric(length=its)
      }}}
  for (j in 1:6){  #for all locales, pulling single spot
    for(k in 1:length(bs[[i]][[j]][[2]])){
      for(i in 1:length(bs)){
        x[[j]][[k]][i]=bs[[i]][[j]][[2]][k]
        y[[j]][[k]][i]=bs[[i]][[j]][[1]][k]
      }}}
  
  sites=c(rep("Guanacaste",its*length(bs[[1]][[1]][[1]])),rep("Lemmon", its*length(bs[[1]][[2]][[1]])),rep("RMBL",its*length(bs[[1]][[3]][[1]])),rep("Rio Sav",its*length(bs[[1]][[4]][[1]])),rep("Seq Natl Park",its*length(bs[[1]][[5]][[1]])),rep("Tamaulipas",its*length(bs[[1]][[6]][[1]])))
  df.pool=data.frame(unlist(x),unlist(y), sites, rep(1:its))
  if(mode=="kernel"){
    colnames(df.pool)=c("Null model functional distance", "Elevation change","Locale", "iteration")
  }
  if(mode=="overlap"){
    colnames(df.pool)=c("Randomized functional overlap", "Elevation change","Locale", "iteration")
  }
  
  return(df.pool) 
}




sb.std=function(imps, m=imps$m, nulls, vars=c(16:(15+length(complete(imps))-2)), mode=c("kernel","overlap")){
  b.norm=sbeta(imps,m, vars,mode=mode)
  b.scr=nulls
  
  bs=b.norm[[1]]
  score=numeric(length=nrow(bs))
  for(i in 1:nrow(bs)){
  score[i]=(bs[i,1]-mean(na.exclude(nulls[,1][nulls[,2] %in% bs[i,2] & nulls[,3] %in% bs[i,3]])))/sd(na.exclude(nulls[,1][nulls[,2] %in% bs[i,2] & nulls[,3] %in% bs[i,3]]))  
  }
  
 bs[,5]=score 
  colnames(bs)=c("Functional change","Elevation change","Locale","imputation","Std Score")
  
  return(bs)
}


sa.rand=function(imps, its, m=imps$m, vars=c(16:(15+length(complete(imps))-2))){  #THIS WORKS
  
  as.full=list()
  length(as.full)=its
  as=as.full
  #p=as.full
  
  pool=matrix(nrow=nrow(imps$data), ncol=(ncol(imps$data)-2))
  colnames(pool)=colnames(imps$data)[3:ncol(imps$data)]
  for(i in 1:ncol(pool)){
    pool[,i]=apply(complete(imps, action="broad")[,grep(colnames(pool)[i],colnames(complete(imps, action="broad")))], 1, mean)
  }
  
  df.temp=as.data.frame(matrix(nrow=nrow(complete(imps)), ncol=15))
  df.temp[,10]=complete(imps)[,1]
  df.temp[,3]=plotNameforSbeta
  df.temp[,4]=start[,1]
  df.temp[,13:14]=start[,4:5]
  colnames(df.temp)=colnames(df)[1:15]
  df.temp[,16:(15+length(vars))]=pool
  
  for( i in 1:its){
    df.temp[,16:length(df.temp)][df.temp$State.province %in% "Guanacaste",]=df.temp[,16:length(df.temp)][df.temp$State.province %in% "Guanacaste",][sample(nrow(df.temp[df.temp$State.province %in% "Guanacaste",])),]
    df.temp[,16:length(df.temp)][df.temp$State.province %in% "Arizona",]=df.temp[,16:length(df.temp)][df.temp$State.province %in% "Arizona",][sample(nrow(df.temp[df.temp$State.province %in% "Arizona",])),]
    df.temp[,16:length(df.temp)][df.temp$State.province %in% "Colorado",]=df.temp[,16:length(df.temp)][df.temp$State.province %in% "Colorado",][sample(nrow(df.temp[df.temp$State.province %in% "Colorado",])),]
    df.temp[,16:length(df.temp)][df.temp$State.province %in% "California",]=df.temp[,16:length(df.temp)][df.temp$State.province %in% "California",][sample(nrow(df.temp[df.temp$State.province %in% "California",])),]
    df.temp[,16:length(df.temp)][df.temp$State.province %in% "Tamaulipas",]=df.temp[,16:length(df.temp)][df.temp$State.province %in% "Tamaulipas",][sample(nrow(df.temp[df.temp$State.province %in% "Tamaulipas",])),]
    df.temp[,16:length(df.temp)][df.temp$State.province %in% c("Puntarenas","San Jose"),]=df.temp[,16:length(df.temp)][df.temp$State.province %in% c("Puntarenas","San Jose"),][sample(nrow(df.temp[df.temp$State.province %in% c("Puntarenas","San Jose"),])),]
    
        as.full[[i]]=alpha(vars,df.temp)
}
  for(i in 1:its){
    as[[i]]=as.full[[i]][[1]]
    #p[[i]]=as.full[[i]][[2]]
  }
  
  x=list()
  length(x)=6
  y=list()
  length(y)=6
  for(i in 1:its){   #the for loop in question???
    for (j in 1:6){  #for all locales, pulling single spot
      for(k in 1:length(as[[i]][[j]][[2]])){   #points at each locale
        x[[j]][[k]]=numeric(length=its)
        y[[j]][[k]]=numeric(length=its)
      }}}
  for (j in 1:6){  #for all locales, pulling single spot
    for(k in 1:length(as[[i]][[j]][[2]])){
      for(i in 1:length(as)){
        x[[j]][[k]][i]=as[[i]][[j]][[2]][k]
        y[[j]][[k]][i]=as[[i]][[j]][[1]][k]
      }}}
  
  sites=c(rep("Guanacaste",its*length(as[[1]][[1]][[1]])),rep("Lemmon", its*length(as[[1]][[2]][[1]])),rep("RMBL",its*length(as[[1]][[3]][[1]])),rep("Rio Sav",its*length(as[[1]][[4]][[1]])),rep("Seq Natl Park",its*length(as[[1]][[5]][[1]])),rep("Tamaulipas",its*length(as[[1]][[6]][[1]])))
  
  df.pool=data.frame(unlist(x),unlist(y), sites, rep(1:its))  
  colnames(df.pool)=c("Null model alpha div","Elevation","Locale", "iteration")  
  
  return(df.pool) 
}

sa.std=function(imps, m=imps$m, nulls, vars=c(16:(15+length(complete(imps))-2))){
  a.norm=salpha(imps, m, vars)
  a.rand=nulls
  
  as=a.norm[[1]]
  score=numeric(length=nrow(as))
  for(i in 1:nrow(as)){
    score[i]=(as[i,1]-mean(na.exclude(nulls[,1][nulls[,2] %in% as[i,2] & nulls[,3] %in% as[i,3]])))/sd(na.exclude(nulls[,1][nulls[,2] %in% as[i,2] & nulls[,3] %in% as[i,3]]))  
  }
  
  as[,5]=score 
  colnames(as)=c("Functional volume","Elevation","Locale","imputation","Std Score")
return(as)  
}



#DETERMINING WHETHER THEY"RE SYSTEMATICALLY DIFFERENT??
#output of beta.p or sbeta= out
# ms=out[[2]][,13:18]   #gets slopes -- you may have to drop RMBL??
# ms=as.data.frame(unlist(ms))   #puts in single column
# ms[,2]=c(rep("G",3),rep("L",3),rep("R",3),rep("Sa",3),rep("se",3),rep("T",3))
# colnames(ms)=c("slope","loc")
# mod=aov(slope~loc, data=ms)
# 
# anova(mod)
# TukeyHSD(mod)
# i=HSD.test(mod, "loc", group=TRUE)   #requires agricolae
# i
# 

