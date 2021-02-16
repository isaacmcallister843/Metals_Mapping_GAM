#---------------------------------------------- Functions for spatial analysis 
# - functions labeled with (sub) are not called by the user and used in other functions


#----------------------------------------------- Grid function (sub)
# Takes a dataset sand returns adjusted and centered position values
# a = dataframe with e and n values
# Output:  x and y positions adjusted and scaled

grid1 <- function(a){
  xy <- cbind (a$e, a$n)
  xy_coords <- as.matrix (xy)
  
  xy_centre <- scale(xy_coords, scale = FALSE)
  
  #We are also creating the coordinate for the bubble plot
  X <- xy_centre [, 1]
  Y <- xy_centre [, 2]
  XY <- cbind (X, Y)
  xy_coords <- as.matrix (XY)
  plot(X,Y)
  return(xy_coords)
}
#----------------------------------------------- Rotations Function (sub)
 #Rotates x and y pairs by an angle and plots the result
 #coords = a matrix containg x and y values 
 #theta  = angle in degrees
rot <- function(coords, theta){
  theta <- theta  * 3.14/180

  coords[,1] <- coords[,1]*cos(theta) - coords[,2]*sin(theta)
  
  coords[,2] <- coords[,1]*sin(theta) + coords[,2]*cos(theta)

  plot(coords[,1],coords[,2])
  return(coords)
}

#----------------------------------------------- Grid2 Function
# a = Dataframe contianing e and n values 
# rownum = The number of rows we want
# sens = How sensitive the program should be at finding vertical columns. 
#       Keep this high for well defined columns and low for illdefined ones. 
#       A value around 8 works for most grids
# theta = angle we want to rotate in degrees
# Output: a matrix with binned e and n values adjusted and scaled 

grid2 <- function (a,rownum,sens,theta){
  
  xy <- as.matrix(cbind(a$e,a$n))
  xy_center <- scale(xy, scale= FALSE)
  
  X <- xy_center[,1]
  Y <- xy_center[,2]
  
  xy_coords <- data.frame(cbind(X,Y))
  xy_coords <- rot(xy_coords,theta)
  xy_coords <- xy_coords[order(xy_coords$X),]

  rownames(xy_coords) <- 1:nrow(xy_coords)

  bin_h <- (max(Y) - min(Y))/rownum

  xy_coords$Y_ad[xy_coords$Y <= (min(Y)+.5*bin_h)] = min(Y)
  
  xy_coords$Y_ad[xy_coords$Y > (max(Y)-.5*bin_h)] = max(Y)
  
  for(i in 1:(rownum - 1)){
    xy_coords$Y_ad[(min(Y)+i*bin_h - .5*bin_h) < xy_coords$Y & xy_coords$Y <= (min(Y)+i*bin_h+.5*bin_h)] = min(Y)+ i*bin_h
  }
  
  xy_coords <- xy_coords[complete.cases(xy_coords),]
  rowval <- unique(xy_coords$Y_ad)
  longest <- 10^-10
  rows <- vector(mode="numeric", length = 60)
  count = 1
  for(i in 1:length(rowval)){
    for(j in 1:(nrow( xy_coords[xy_coords$Y_ad == rowval[i],]) -1 ) ){

      if( (xy_coords[xy_coords$Y_ad == rowval[i],]$X[j+1] - xy_coords[xy_coords$Y_ad == rowval[i],]$X[j]) < sens){
        xy_saveX <- xy_coords[as.numeric(rownames(xy_coords[xy_coords$Y_ad == rowval[i],][j,])),]$X
        xy_saveY <- xy_coords[as.numeric(rownames(xy_coords[xy_coords$Y_ad == rowval[i],][j,])),]$Y
        rows[count] <- as.numeric(rownames(xy_coords[xy_coords$Y_ad == rowval[i],][j,]))
        count = count + 1
      }
      
    }
    
    rows <- rows[rows != 0]
    if(nrow(xy_coords[xy_coords$Y_ad == rowval[i],]) > longest){
      
      longest = nrow(xy_coords[xy_coords$Y_ad == rowval[i],])
    }
  }
  
  adju <- xy_coords[rows,]
  xy_coords <- xy_coords[-rows,]
  bin_w <- (max(X) - min(X))/longest
  xy_coords$X_ad[xy_coords$X <= (min(X)+.5*bin_w)] = min(X)
  
  xy_coords$X_ad[xy_coords$X > (max(X)-.5*bin_w)] = max(X)
  
  for(i in 1:(longest - 1)){
    xy_coords$X_ad[(min(X)+i*bin_w - .5*bin_w) < xy_coords$X & xy_coords$X <= (min(X)+i*bin_w+.5*bin_w)] = min(X)+ i*bin_w
  }
  
  adju$X_ad[adju$X <= (min(X)+.5*bin_w)] = min(X)
  
  adju$X_ad[adju$X > (max(X)-.5*bin_w)] = max(X)
  
  for(i in 1:nrow(adju)){
    adju$X_ad[(min(X)+i*bin_w - .5*bin_w) < adju$X & adju$X <= (min(X)+i*bin_w+.5*bin_w)] = min(X)+ i*bin_w
  }
  
  adju$Y_ad <- adju$Y
  xy_coords <- rbind(xy_coords,adju)
  
  xy_coords$X <- xy_coords$X_ad
  
  xy_coords$Y <- xy_coords$Y_ad
  
  xy_coords <- xy_coords[,c(1,2)]
  
  plot(xy_coords$X, xy_coords$Y)
  
  return(xy_coords)
}

#----------------------------------------------- Gridtools Function
# Takes a dataset and displays it, rotates and centered
# This is purely for data exploration and to find the theta and rownum values for grid2 
# data = unaltered dataframe with e and n values
# theta = angle in degrees
gridtools <- function(data,theta=0){
  coords <- grid1(data)
  theta <- theta  * 3.14/180
  coords[,1] <- coords[,1]*cos(theta) - coords[,2]*sin(theta)
  coords[,2] <- coords[,1]*sin(theta) + coords[,2]*cos(theta)
  plot(coords[,1],coords[,2])
  coords <- data.frame(coords)
  return(coords)
}
#----------------------------------------------- vs.gam.advanced (sub)
# Creates heat maps with a provided model
# Indicator = Retrieved from model function, indicates which dataframe to use
# model = Model from model function
# dataset = dataframe with metal values, slope, and e and n values
# modified = modified dataframe with adjusted e and n values
# output = list with the plot and x and y ranges

vis.gam.advanced <- function(indicator,model,dataset,modifiedframe){
  
  if(indicator == "gam"){
    usedata <- dataset
    per <- c("e","n")
    grid <- cbind(e = usedata$e, n = usedata$n)
    
  }else{
    usedata <- modifiedframe
    per <- c("X","Y")
    grid <- cbind(X = usedata$X, Y = usedata$Y)
    
  }
  
  xrange <- c(min(grid[,1])-2,max(grid[,1])+2)
  yrange <- c(min(grid[,2])-2,max(grid[,2])+2)
  
  vis.gam(model, view =per, plot.type = "contour", contour.col = "blue", nlevels = 15,
          xlim = xrange, ylim = yrange)
  
  points(usedata[,1],usedata[,2])
  
  plot <- recordPlot()
  
  report <- list(plot,xrange,yrange)
  return(report)
}
#----------------------------------------------- Error check family (sub)
#Used to make sure we are running a valid model
#Takes the formula, the metal, and the dataset we are using, runs the formula and 
#if it cant build the model builds the simpliest model instead. Error check 2 
#does the same but for modified frame 
#f: formula for model
#met: metal name as a string
#dataset: the data we are using 

errorcheck <- function(f,met,dataset){
  d <- as.formula(paste(met,"s(e,n)",sep="~"))
  
  a <- tryCatch( gam(f,data=dataset,method="REML"), error=function(e){
    print("Unable to construct model, moving on")
    gam(d,data=dataset,method="REML")
  })
 
  vari <- as.character(a$formula)[3]

  out <- list(a,vari)
  return(out)
}

errorcheck2 <- function(f,met,dataset){
  d <- as.formula(paste(met,"s(X,Y)",sep="~"))
  
  a <- tryCatch( gam(f,data=dataset,method="REML"), error=function(e){
    gam(d,data=dataset,method="REML")
  } )
  
  vari <- as.character(a$formula)[3]
  
  out <- list(a,vari)
  return(out)
}
#-------------------------------------------- Model Function (sub)
#Takes a dataset and a modified dataframe and finds the best model for a specific metal
# dataset = unaltered data contaiing e and n values as well as the metal concentrations
# modified frame = dataframe with the altered e and n values gotten from grid2 and metal concentations
# metal = metal we want to analyze
# Output = a list containing the best model with the lowest AIC value and an "indicator" string
#          which tells the user which dataframe was used to make the model

model <- function(dataset,modifiedframe,metal){
  #Variables
  met <- as.character(metal)
  ind <- 0
  
  print(paste("Modeling",met, sep = " "))

  #all models needed to be strucutred this way so a custom metal can be entered
  var1    <- c("s(e,n) + s(aspect) + te(e,n,slope, k = 3)")
  f       <- as.formula(paste(met,var1,sep="~"))
  gamlist<- errorcheck(f,met,dataset)
  gam1    <- gamlist[[1]]
  var1    <- gamlist[[2]]
  
  var2    <- c("s(e,n)+ te(e,n,aspect, k = 3) ")
  f       <- as.formula(paste(met,var2,sep="~"))
  gamlist<- errorcheck(f,met,dataset)
  gam2    <- gamlist[[1]]
  var2    <- gamlist[[2]]

  var3    <- c("s(e,n)+  te(e,n,slope, k = 3)")
  f       <- as.formula(paste(met,var3,sep="~"))
  gamlist<- errorcheck(f,met,dataset)
  gam3    <- gamlist[[1]]
  var3    <- gamlist[[2]]
  
  var4    <- c("s(e,n) + s(aspect)")
  f       <- as.formula(paste(met,var4,sep="~"))
  gamlist<- errorcheck(f,met,dataset)
  gam4    <- gamlist[[1]]
  var4    <- gamlist[[2]]
  
  var5    <- c("s(e,n)+s(slope)")
  f       <- as.formula(paste(met,var5,sep="~"))
  gam1list<- gamlist <- errorcheck(f,met,dataset)
  gam5    <- gamlist[[1]]
  var5    <- gamlist[[2]]
  
  var6    <- c("s(e,n)")
  f       <- as.formula(paste(met,var6,sep="~"))
  gamlist <- gamlist <- errorcheck(f,met,dataset)
  gam6    <- gamlist[[1]]
  var6    <- gamlist[[2]]
    
  Sys.sleep(1)
  
  #Adjusted models with new modified frame
  print("Modeling Adjusted Functions")
  
  #colnames(modifiedframe) <- c("X","Y","slope","aspect","Au","Cu","Zn","Pb")
  var7    <- c("s(X,Y)+ s(aspect) + te(X,Y,slope, k = 3)")
  f       <- as.formula(paste(met,var7,sep="~"))
  gamlist <- errorcheck2(f,met,modifiedframe)
  gam1mod <- gamlist[[1]]
  var7    <- gamlist[[2]]

  var8    <- c("s(X,Y) + te(X,Y,aspect, k = 3)")
  f       <- as.formula(paste(met,var8,sep="~"))
  gamlist <- errorcheck2(f,met,modifiedframe)
  gam2mod <- gamlist[[1]]
  var8    <- gamlist[[2]]
  
  var9    <- c("s(X,Y) +te(X,Y,slope,k=3)")
  f       <- as.formula(paste(met,var9,sep="~"))
  gamlist <- errorcheck2(f,met,modifiedframe)
  gam3mod <- gamlist[[1]]
  var9    <- gamlist[[2]]
  
  var10    <- c("s(X,Y) + s(aspect)")
  f       <- as.formula(paste(met,var10,sep="~"))
  gamlist <- errorcheck2(f,met,modifiedframe)
  gam4mod <- gamlist[[1]]
  var10    <- gamlist[[2]]
  
  var11   <- c("s(X,Y) + s(slope)")
  f       <- as.formula(paste(met,var11,sep="~"))
  gamlist <- errorcheck2(f,met,modifiedframe)
  gam5mod <- gamlist[[1]]
  var11   <- gamlist[[2]]
  
  var12   <- c("s(X,Y)")
  f       <- as.formula(paste(met,var12,sep="~"))
  gamlist <- errorcheck2(f,met,modifiedframe)
  gam6mod <- gamlist[[1]]
  var12   <- gamlist[[2]]
  
  #Get the AIC values and put them in a table
  AIC <- AIC(gam1, gam2, gam3, gam4,gam5,gam6,
             gam1mod,gam2mod,gam3mod,gam4mod, gam5mod,gam6mod)

  #Creating a new dataframe with the AIC numbers and the models
  Collection <- cbind(AIC,model = c(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12),
                      index = c(0,0,0,0,0,0,1,1,1,1,1,1))
  
  #Find the lowest AIC value
  ret <- Collection[Collection$AIC == min(Collection$AIC),]
  
  
  #Finding alternatives
  #This is done when we just have a gradient and there might be a better option
  
  altern <- ret
  
  if (ret$model == "s(X, Y)" | ret$model == "s(e, n)"){
    #Checks if we have a gradient
    for (i in 1:nrow(Collection)){
      #Checks if the AIC value is close (within 4) of another model and the index is the same
      if(abs(ret$AIC - Collection$AIC[i]) < 4 & Collection$index[i] == ret$index){
        #Puts all the alternatives into a new df
        altern <- rbind(altern,Collection[i,])
      }
    }
    #Grabs the most complex model
    ret <- altern[2,]
  } 
  
  #Reconstruct the formula
  model_char <- ret$model[1] 
  
  f <- as.formula(paste(met,model_char,sep="~"))

  #Perform a check which dataframe we are using

  if(ret$index[1] == 1){
      bestfit <- gam(f,data=modifiedframe,method="REML")
      indicator <- "gammode"
  }
  if(ret$index[1] == 0){
      bestfit <- gam(f,data=dataset,method="REML")
      indicator <- "gam"
  }

  list <- list(bestfit,indicator,Collection )
  print("Finished Modeling")
  
  #Return the bestfit model
  return(list)
}

#----------------------------------------------- Raster Function
#This function needs a very particular setup, check the format of any other the west_ grids, the order of columns does matter
#Takes a dataframe, a GAM model, an indicator (tells if we are using the normal or adjusted values), and the name of the metal (ex "Pb")
makeraster <- function(dataframe, bestmodel, indicator, metal,name){
  
  raster_grd <- dataframe[,c(1,6:7)]
  coordinates(raster_grd) <- ~ e + n
  # coerce to SpatialPixelsDataFrame
  gridded(raster_grd) <- TRUE
  
  # coerce to raster
  raster_grd <- raster(raster_grd)
  crs(raster_grd) <- "+init=epsg:26909" 

  #Create the gradients
  print("Grid Created")
  
  Polygon1 <- Polygon(rbind(c(min(dataframe$e), min(dataframe$n)), 
                            c(min(dataframe$e), max(dataframe$n)),
                            
                            c(max(dataframe$e), min(dataframe$n)), 
                            c(max(dataframe$e), max(dataframe$n))
  ))
  
  Polygon1 <- Polygons(list(Polygon1), 1)
  Polygon1 <- SpatialPolygons(list(Polygon1))
  
  print("Polygons created")
  
  ## make raster
  r <- raster(extent(Polygon1), ncol = 127, nrow = 224)
  polyraster <- rasterize(Polygon1, r)
  values(polyraster) <- 1

  crs(polyraster) <- "+init=epsg:26909"
  
  ## Make Raster gradient for direction 0
  polymatx <- as.matrix(polyraster)
  polymatx <- apply(polymatx, 1, function(x) seq(1, ncol(polymatx), 1)) 
  polymatx <- raster(polymatx,
                     xmn = extent(polyraster)[1],
                     xmx = extent(polyraster)[2],
                     ymn = extent(polyraster)[3],
                     ymx = extent(polyraster)[4])
  
  polymaty <- as.matrix(polyraster)
  polymaty <- apply(polymaty, 1, function(x) rev(seq(1, ncol(polymatx), 1))) 
  polymaty <- raster(t(polymaty),
                     xmn = extent(polyraster)[1],
                     xmx = extent(polyraster)[2],
                     ymn = extent(polyraster)[3],
                     ymx = extent(polyraster)[4])
  print("poly mats made")
  
  ##############
  # Cropping the DEM
  # The extent was calculated after cropping to west_S_grd - the resolution changed,
  # so I had to change the extent slightly. The ex_area is used to crop the other coerced rasters.
  raster_DEM <- crop(DEM, extent(raster_grd), snap="out")
  raster_DEM_aspect <- terrain(raster_DEM, opt='aspect', unit='degrees')
  raster_DEM_slope <- terrain(raster_DEM, opt='slope', unit='degrees')
  
  print("DEM imported and cropped")
  
  if(indicator == "gam"){
    names(polymatx) = "e" ## has to be named the same as in the gam
    names(polymaty) = "n"
  }
  if(indicator == "gammode"){
    names(polymatx) = "X" ## has to be named the same as in the gam
    names(polymaty) = "Y"
  }
  ##########
  # Final stack for prediction
  X <- polymatx
  X <- resample(X,raster_DEM,method='ngb')
  Y <- polymaty
  Y <- resample(Y,raster_DEM,method='ngb')
  aspect <- raster_DEM_aspect
  slope <- raster_DEM_slope
  
  metstack_XY <- stack(X, Y)
  metstack_XYA <- stack(X, Y, aspect)
  metstack_XYAS <- stack(X, Y, aspect, slope)
  metstack_XYS <- stack(X, Y, slope)
  
  print("Stacks created")
  
  
  ##
  # type = reponse: see https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/predict.gam.html
  # "When type="response" predictions on the scale of the response are returned (possibly with approximate standard errors)."
  print("Trying to build raster grid")
  
  try(raster_metal_grd <- predict(metstack_XY,bestmodel,type="response"),silent=TRUE)
  print("1st Grid Complete")
  
  try(raster_metal_grd <- predict(metstack_XYA,bestmodel,type="response"),silent=TRUE)
  print("2nd Grid Complete")
  
  try(raster_metal_grd <- predict(metstack_XYAS,bestmodel,type="response"),silent=TRUE)
  print("3rd Grid Complete")
  
  try(raster_metal_grd <- predict(metstack_XYS,bestmodel,type="response"),silent=TRUE)
  
  print("Finished building grid")
  
  # Plot predictions
  par(mfrow=c(1,1))
  
  print("Plotting")
  
  plot(raster_metal_grd, main=metal, col=colors)
  plot  <- recordPlot()
  
  #writing the raster
  #this has more code then it probably needs. The issue is R cant retrieve the name
  #of the dataset when it is passed to a function, so the name needs to be grabbed outside
  #this is a pain.
  print("Writing Raster")
  
  new <- deparse(name)

  char <- paste(new,metal,sep="")

  file <- paste(char,".tif",sep="")
  
  send <- as.character(file)
  
  print(send)
  writeRaster(raster_metal_grd, filename=send, format="GTiff", overwrite=TRUE)
  
  #output
  print("Complete")
  
  out <- list(raster_metal_grd,plot)

  return(out)
}

#----------------------------------------------- ana Function
# Performs the analysis 
# dataset = an unaltered dataset containing e and n values, slope, substrate, and metal concentrations
# gridad  = xy coordinates adjusted, binned, and scaled. Gotten from the grid2 function
# Output  = a list contains: the bubble map, best models for all metals, patch sizes and spatial models
#                            for each metal, heatmaps constructed from the best model, the raster images and outputs the raster objects 

ana <- function(dataset,gridad, modeling = TRUE, raster = TRUE,
                gold = TRUE){
  #Get the name of the dataset, this needs to be done early and not in any functions
  name <- substitute(dataset)
  
  #Create the modified dataset that contains the adjusted e and n values
  grid <- grid1(dataset)
  dataset <- dataset[order(dataset$e),]
  gridad <- gridad[order(gridad[,1]),]
  
  #-------------------------- Creating the modified dataframe
  modifiedframe <- cbind(gridad,dataset$slope,dataset$aspect,dataset$Au,
                         dataset$Cu,dataset$Zn,dataset$Pb)
  
  colnames(modifiedframe) <- c("X","Y","slope","aspect","Au","Cu","Zn","Pb")
  
  #Dealing with NA via median replacement
  try(modifiedframe[!complete.cases(modifiedframe),]$Y <- median(modifiedframe[complete.cases(modifiedframe),]$Y),silent=TRUE) 
  try(modifiedframe[!complete.cases(modifiedframe),]$slope <- median(modifiedframe[complete.cases(modifiedframe),]$slope),silent=TRUE) 
  try(modifiedframe[!complete.cases(modifiedframe),]$aspect <- median(modifiedframe[complete.cases(modifiedframe),]$aspect),silent=TRUE)
  try(modifiedframe[!complete.cases(modifiedframe),]$Cu <- median(modifiedframe[complete.cases(modifiedframe),]$Cu),silent=TRUE)
  try(modifiedframe[!complete.cases(modifiedframe),]$Zn <- median(modifiedframe[complete.cases(modifiedframe),]$Zn),silent=TRUE) 
  try(modifiedframe[!complete.cases(modifiedframe),]$Pb <- median(modifiedframe[complete.cases(modifiedframe),]$Pb),silent=TRUE) 
  try(modifiedframe[!complete.cases(modifiedframe),]$X <- median(modifiedframe[complete.cases(modifiedframe),]$X),silent=TRUE)
  
  rownames(modifiedframe) <- 1:nrow(modifiedframe)
  
  #Reseting the adjusting grid with the new values
  gridad <- cbind(X=modifiedframe$X,Y=modifiedframe$Y)
  
  #----------------------- Bubble Maps

  #Zinc
  par (mfrow = c(2, 4))
  
  Zn_add <- scale(dataset$Zn,scale=FALSE)
  
  try(s.value (grid, dataset$Zn))
  title ("Zinc Plot")
  try(s.value (gridad, Zn_add))
  title ("Zinc Adjusted and Scaled")
  
  #Copper
  Cu_add <- scale(dataset$Cu,scale=FALSE)
  
  try(s.value (grid, dataset$Cu))
  title ("Copper Plot")
  try(s.value (gridad, Cu_add))
  title ("Copper Adjusted and Scaled")
  
  #Lead
  Pb_add <- scale(dataset$Pb,scale=FALSE)
  
  try(s.value (grid, dataset$Pb))
  title ("Lead Plot")
  try(s.value (gridad, Pb_add))
  title ("Lead Adjusted and Scaled")
  
  #Gold 
  
  Au_add <- scale(dataset$Au,scale=FALSE)
  
  try(s.value (grid, dataset$Au))
  title ("Gold Plot")
  try(s.value (gridad, Au_add))
  title ("Gold Adjusted and Scaled")
  
  par(mar=c(1,1,1,1))
  
  bubble_map  <- recordPlot()
  
  #----------------------------------- Models
  if(modeling == TRUE){
    
    lead_list   <- model(dataset,modifiedframe,"Pb")
    copper_list <- model(dataset,modifiedframe,"Cu")
    zinc_list   <- model(dataset,modifiedframe,"Zn")
    gold_list   <- model(dataset,modifiedframe,"Au")
    
    
    #Getting all the models
    leadmodel   <- lead_list[[1]]
    goldmodel   <- gold_list[[1]]
    coppermodel <- copper_list[[1]]
    zincmodel   <- zinc_list[[1]]
    
    #Getting the indicator values
    lead_in     <- lead_list[[2]]
    gold_in     <- gold_list[[2]]
    copper_in   <- copper_list[[2]]
    zinc_in     <- zinc_list[[2]]
    
    #The collection
    lead_col   <- lead_list[[3]]
    gold_col   <- gold_list[[3]]
    zinc_col   <- zinc_list[[3]]
    copper_col <- copper_list[[3]]
                            
    
    #------------------------------------------------- Heatmaps 
    par (mfrow = c(1, 1)) 
    
    #Plots
    lead_plot_l   <- vis.gam.advanced(lead_in,leadmodel,dataset,modifiedframe)
    gold_plot_l   <- vis.gam.advanced(gold_in,goldmodel,dataset,modifiedframe)
    copper_plot_l <- vis.gam.advanced(copper_in,coppermodel,dataset,modifiedframe)
    zinc_plot_l   <- vis.gam.advanced(zinc_in,zincmodel,dataset,modifiedframe)
    
    lead_plot     <- lead_plot_l[[1]]
    leadrange     <- cbind(xrange =lead_plot_l[[2]],yrange = lead_plot_l[[3]])
    
    gold_plot     <- gold_plot_l[[1]]
    goldrange     <- cbind(xrange =gold_plot_l[[2]], yrange = gold_plot_l[[3]])
    
    zinc_plot     <- zinc_plot_l[[1]]
    zincrange     <- cbind(xrange = zinc_plot_l[[2]], yrange = zinc_plot_l[[3]])
    
    copper_plot   <- copper_plot_l[[1]]
    copperrange   <- cbind(xrange = copper_plot_l[[2]], yrange = copper_plot_l[[3]])
    
    modelreport <- list(zinc=zincmodel, lead = leadmodel, gold = goldmodel, copper = coppermodel,
                        leadheat = lead_plot,goldheat= gold_plot,copperheat = copper_plot,
                        zincheat = zinc_plot, lead_col = lead_col, gold_col = gold_col,
                        zinc_col=zinc_col,copper_col=copper_col)
    
  }else{
    modelreport <- list(c("Modeling not complete"))
  }
  
  #----------------------------------- Spatial Componets 

  geo_pb      <- as.geodata (data.frame (dataset$e, dataset$n, dataset$Pb))
  geo_pbmod   <- as.geodata (data.frame (modifiedframe$X, modifiedframe$Y, modifiedframe$Pb))
  
  geo_au      <- as.geodata (data.frame (dataset$e, dataset$n, dataset$Au))
  geo_aumod   <- as.geodata (data.frame (modifiedframe$X, modifiedframe$Y, modifiedframe$Au))
  
  geo_zn      <- as.geodata (data.frame (dataset$e, dataset$n, dataset$Zn))
  geo_znmod   <- as.geodata (data.frame (modifiedframe$X, modifiedframe$Y, modifiedframe$Zn))
  
  geo_cu      <- as.geodata (data.frame (dataset$e, dataset$n, dataset$Cu))
  geo_cumod   <- as.geodata (data.frame (modifiedframe$X, modifiedframe$Y, modifiedframe$Cu))

  #----------------------------------- Variogram
  #This creates a omnidirectional variagrram in every directions
  par (mfrow = c(1, 1)) 
  
  #Normal Variograms
  variopb   <- variog (geo_pb)
  #varPb <- plot(variopb)
  
  varioau   <- variog (geo_au)
  #varAu <- plot(varioau)
  
  variozn   <- variog (geo_zn)
  #varZn <- plot(variozn)
  
  variocu   <- variog (geo_cu)
  #varCu <- plot(variocu)
  
  #Adjusted Variograms
  variopb_ad <- variog (geo_pbmod)
  #varPb_ad <- plot(variopb_ad)
  
  varioau_ad <- variog (geo_aumod)
  #varAu_ad <- plot(varioau_ad)
  
  variozn_ad <- variog (geo_znmod)
  #varZn_ad <- plot(variozn_ad)
  
  variocu_ad <- variog (geo_cumod)
  #varCu_ad <- plot(variocu_ad)
  
  #----------------------------------- Patches
  sphp <- variofit (variopb, cov.model = "spherical",  weights = "npairs")
  sphppb <- sphp$practicalRange
  
  sphp <- variofit (varioau, cov.model = "spherical",  weights = "npairs")
  sphpau <- sphp$practicalRange
  
  sphp <- variofit (variozn, cov.model = "spherical",  weights = "npairs")
  sphpzn <-sphp$practicalRange
  
  sphp <- variofit (variocu, cov.model = "spherical",  weights = "npairs")
  sphp$practicalRange
  sphpcu <-sphp$practicalRange
  
  #gau
  gau <- variofit (variopb, cov.model = "gau",  weights = "npairs")
  gaupb <- gau$practicalRange
  
  gau <- variofit (varioau, cov.model = "gau",  weights = "npairs")
  gauau <- gau$practicalRange
  
  gau <- variofit (variozn, cov.model = "gau",  weights = "npairs")
  gauzn <- gau$practicalRange
  
  gau <- variofit (variocu, cov.model = "gau",  weights = "npairs")
  gaucu <- gau$practicalRange
  
  #Adjusted
  
  sphp <- variofit (variopb_ad, cov.model = "spherical",  weights = "npairs")
  sphppbad <- sphp$practicalRange
  
  sphp <- variofit (varioau_ad, cov.model = "spherical",  weights = "npairs")
  sphpauad <- sphp$practicalRange
  
  sphp <- variofit (variozn_ad, cov.model = "spherical",  weights = "npairs")
  sphpznad <- sphp$practicalRange
  
  sphp <- variofit (variocu_ad, cov.model = "spherical",  weights = "npairs")
  sphpcuad <- sphp$practicalRange 
  #gau
  gau <- variofit (variopb_ad, cov.model = "gau",  weights = "npairs")
  gaupbad <- gau$practicalRange
  
  gau <- variofit (varioau_ad, cov.model = "gau",  weights = "npairs")
  gauauad <- gau$practicalRange
  
  gau <- variofit (variozn_ad, cov.model = "gau",  weights = "npairs")
  gauznad <- gau$practicalRange
  
  gau <- variofit (variocu_ad, cov.model = "gau",  weights = "npairs")
  gaucuad <- gau$practicalRange
  
  #Making a dataframe
  modelnorm <- c(sphppb,sphpau,sphpzn,sphpcu,gaupb,gauau,gauzn,gaucu)
  modernadj <- c(sphppbad,sphpauad,sphpznad,sphpcuad,gaupbad,gauauad,gauznad,gaucuad)
  names <- c("sph pb","sph au", "sph zn", "sph cu","gau pb","gau au", "gau zn", "gau cu")
  
  patches <- data.frame(cbind(names,modelnorm,modernadj))
  
  #------------------------------------------------- Rasters

  if(raster == TRUE){
    par (mfrow = c(1, 1))
    leadraster_list <- makeraster(dataset,leadmodel,lead_in,"Pb",name)
    zincraster_list <- makeraster(dataset,zincmodel,zinc_in,"Zn",name)
    goldraster_list <- makeraster(dataset,goldmodel,gold_in,"Au",name)
    copperraster_list <- makeraster(dataset,coppermodel,copper_in,"Cu",name)
    
    leadraster <- leadraster_list[[1]]
    zincraster <- zincraster_list[[1]]
    goldraster <- goldraster_list[[1]]
    copperraster <- copperraster_list[[1]]
    
    leadplotav <- leadraster_list[[2]]
    zincplotav <- zincraster_list[[2]]
    goldplotav <- goldraster_list[[2]]
    copperplotav <- copperraster_list[[2]]
    
    rasterreport <- list(leadraster = leadraster,
                         goldraster = goldraster,
                         copperraster = copperraster,
                         zincraster = zincraster,
                         leadploav = leadplotav,
                         goldploav = goldplotav,
                         copperploav = copperplotav,
                         zincplotav = zincplotav )
  }else{
    rasterreport <- list(c("no rasters built"))
  }
  #------------------------------------------------- Final Report
  dev.off()

  report <- c(list(bubble = bubble_map, patch = patches),modelreport,rasterreport)
  return(report)
}

#WITHOUT GOLD-----------------------------------------------------------------------
#There where a few dataframes that had all 0 for gold, this cant be modeled (there is nothing to model)
#This function is identical but doesnt model gold, was easier then adding functionality to metal each individaul metal 


ana2 <- function(dataset,gridad, modeling = TRUE, raster = TRUE,
                gold = TRUE){
  name <- substitute(dataset)
  
  #Create the modified dataset that contains the adjusted e and n values
  grid <- grid1(dataset)
  dataset <- dataset[order(dataset$e),]
  gridad <- gridad[order(gridad[,1]),]
  
  #-------------------------- Creating the modified dataframe
  modifiedframe <- cbind(gridad,dataset$slope,dataset$aspect,dataset$Au,
                         dataset$Cu,dataset$Zn,dataset$Pb)
  
  colnames(modifiedframe) <- c("X","Y","slope","aspect","Au","Cu","Zn","Pb")
  
  #Dealing with NA via median replacement
  try(modifiedframe[!complete.cases(modifiedframe),]$Y <- median(modifiedframe[complete.cases(modifiedframe),]$Y),silent=TRUE) 
  try(modifiedframe[!complete.cases(modifiedframe),]$slope <- median(modifiedframe[complete.cases(modifiedframe),]$slope),silent=TRUE) 
  try(modifiedframe[!complete.cases(modifiedframe),]$aspect <- median(modifiedframe[complete.cases(modifiedframe),]$aspect),silent=TRUE)
  try(modifiedframe[!complete.cases(modifiedframe),]$Cu <- median(modifiedframe[complete.cases(modifiedframe),]$Cu),silent=TRUE)
  try(modifiedframe[!complete.cases(modifiedframe),]$Zn <- median(modifiedframe[complete.cases(modifiedframe),]$Zn),silent=TRUE) 
  try(modifiedframe[!complete.cases(modifiedframe),]$Pb <- median(modifiedframe[complete.cases(modifiedframe),]$Pb),silent=TRUE) 
  try(modifiedframe[!complete.cases(modifiedframe),]$X <- median(modifiedframe[complete.cases(modifiedframe),]$X),silent=TRUE)
  
  rownames(modifiedframe) <- 1:nrow(modifiedframe)
  
  #Reseting the adjusting grid with the new values
  gridad <- cbind(X=modifiedframe$X,Y=modifiedframe$Y)
  
  #----------------------- Bubble Maps
  
  #Zinc
  par (mfrow = c(2, 4))
  
  Zn_add <- scale(dataset$Zn,scale=FALSE)
  
  try(s.value (grid, dataset$Zn))
  title ("Zinc Plot")
  try(s.value (gridad, Zn_add))
  title ("Zinc Adjusted and Scaled")
  
  #Copper
  Cu_add <- scale(dataset$Cu,scale=FALSE)
  
  try(s.value (grid, dataset$Cu))
  title ("Copper Plot")
  try(s.value (gridad, Cu_add))
  title ("Copper Adjusted and Scaled")
  
  #Lead
  Pb_add <- scale(dataset$Pb,scale=FALSE)
  
  try(s.value (grid, dataset$Pb))
  title ("Lead Plot")
  try(s.value (gridad, Pb_add))
  title ("Lead Adjusted and Scaled")
  
  #Gold 
  
  par(mar=c(1,1,1,1))
  
  bubble_map  <- recordPlot()
  
  #----------------------------------- Models
  if(modeling == TRUE){
    
    lead_list   <- model(dataset,modifiedframe,"Pb")
    copper_list <- model(dataset,modifiedframe,"Cu")
    zinc_list   <- model(dataset,modifiedframe,"Zn")
   
    #Getting all the models
    leadmodel   <- lead_list[[1]]
    coppermodel <- copper_list[[1]]
    zincmodel   <- zinc_list[[1]]
    
    #Getting the indicator values
    lead_in     <- lead_list[[2]]
    copper_in   <- copper_list[[2]]
    zinc_in     <- zinc_list[[2]]
    
    #------------------------------------------------- Heatmaps 
    par (mfrow = c(1, 1)) 
    
    #Plots
    lead_plot_l   <- vis.gam.advanced(lead_in,leadmodel,dataset,modifiedframe)
    copper_plot_l <- vis.gam.advanced(copper_in,coppermodel,dataset,modifiedframe)
    zinc_plot_l   <- vis.gam.advanced(zinc_in,zincmodel,dataset,modifiedframe)
    
    lead_plot     <- lead_plot_l[[1]]
    leadrange     <- cbind(xrange =lead_plot_l[[2]],yrange = lead_plot_l[[3]])
  
    zinc_plot     <- zinc_plot_l[[1]]
    zincrange     <- cbind(xrange = zinc_plot_l[[2]], yrange = zinc_plot_l[[3]])
    
    copper_plot   <- copper_plot_l[[1]]
    copperrange   <- cbind(xrange = copper_plot_l[[2]], yrange = copper_plot_l[[3]])
    
    modelreport <- list(zinc=zincmodel, lead = leadmodel, copper = coppermodel,
                        leadheat = lead_plot,copperheat = copper_plot,
                        zincheat = zinc_plot)
    
  }else{
    modelreport <- list(c("Modeling not complete"))
  }
  
  #----------------------------------- Spatial Componets 
  
  geo_pb      <- as.geodata (data.frame (dataset$e, dataset$n, dataset$Pb))
  geo_pbmod   <- as.geodata (data.frame (modifiedframe$X, modifiedframe$Y, modifiedframe$Pb))
  
  geo_zn      <- as.geodata (data.frame (dataset$e, dataset$n, dataset$Zn))
  geo_znmod   <- as.geodata (data.frame (modifiedframe$X, modifiedframe$Y, modifiedframe$Zn))
  
  geo_cu      <- as.geodata (data.frame (dataset$e, dataset$n, dataset$Cu))
  geo_cumod   <- as.geodata (data.frame (modifiedframe$X, modifiedframe$Y, modifiedframe$Cu))
  
  #----------------------------------- Variogram
  #This creates a omnidirectional variagrram in every directions
  par (mfrow = c(1, 1)) 
  
  #Normal Variograms
  variopb   <- variog (geo_pb)
  #varPb <- plot(variopb)
  
  variozn   <- variog (geo_zn)
  #varZn <- plot(variozn)
  
  variocu   <- variog (geo_cu)
  #varCu <- plot(variocu)
  
  #Adjusted Variograms
  variopb_ad <- variog (geo_pbmod)
  #varPb_ad <- plot(variopb_ad)
  
  variozn_ad <- variog (geo_znmod)
  #varZn_ad <- plot(variozn_ad)
  
  variocu_ad <- variog (geo_cumod)
  #varCu_ad <- plot(variocu_ad)
  
  #----------------------------------- Patches
  sphp <- variofit (variopb, cov.model = "spherical",  weights = "npairs")
  sphppb <- sphp$practicalRange
  
  sphp <- variofit (variozn, cov.model = "spherical",  weights = "npairs")
  sphpzn <-sphp$practicalRange
  
  sphp <- variofit (variocu, cov.model = "spherical",  weights = "npairs")
  sphp$practicalRange
  sphpcu <-sphp$practicalRange
  
  #gau
  gau <- variofit (variopb, cov.model = "gau",  weights = "npairs")
  gaupb <- gau$practicalRange
  
  gau <- variofit (variozn, cov.model = "gau",  weights = "npairs")
  gauzn <- gau$practicalRange
  
  gau <- variofit (variocu, cov.model = "gau",  weights = "npairs")
  gaucu <- gau$practicalRange
  
  #Adjusted
  
  sphp <- variofit (variopb_ad, cov.model = "spherical",  weights = "npairs")
  sphppbad <- sphp$practicalRange

  sphp <- variofit (variozn_ad, cov.model = "spherical",  weights = "npairs")
  sphpznad <- sphp$practicalRange
  
  sphp <- variofit (variocu_ad, cov.model = "spherical",  weights = "npairs")
  sphpcuad <- sphp$practicalRange 
  #gau
  gau <- variofit (variopb_ad, cov.model = "gau",  weights = "npairs")
  gaupbad <- gau$practicalRange
  
  gau <- variofit (variozn_ad, cov.model = "gau",  weights = "npairs")
  gauznad <- gau$practicalRange
  
  gau <- variofit (variocu_ad, cov.model = "gau",  weights = "npairs")
  gaucuad <- gau$practicalRange
  
  #Making a dataframe
  modelnorm <- c(sphppb,sphpzn,sphpcu,gaupb,gauzn,gaucu)
  modernadj <- c(sphppbad,sphpznad,sphpcuad,gaupbad,gauznad,gaucuad)
  names <- c("sph pb", "sph zn", "sph cu","gau pb", "gau zn", "gau cu")
  
  patches <- data.frame(cbind(names,modelnorm,modernadj))
  
  #------------------------------------------------- Rasters
  if(raster == TRUE){
    par (mfrow = c(1, 1))
    
    leadraster_list <- makeraster(dataset,leadmodel,lead_in,"Pb",name)
    zincraster_list <- makeraster(dataset,zincmodel,zinc_in,"Zn",name)
    copperraster_list <- makeraster(dataset,coppermodel,copper_in,"Cu",name)
    
    leadraster <- leadraster_list[[1]]
    zincraster <- zincraster_list[[1]]
    copperraster <- copperraster_list[[1]]
    
    leadplotav <- leadraster_list[[2]]
    zincplotav <- zincraster_list[[2]]
    copperplotav <- copperraster_list[[2]]
    
    rasterreport <- list(leadraster = leadraster,
                         copperraster = copperraster,
                         zincraster = zincraster,
                         leadploav = leadplotav,
                         copperploav = copperplotav,
                         zincplotav = zincplotav )
  }else{
    rasterreport <- list(c("no rasters built"))
  }
  #------------------------------------------------- Final Report
  dev.off()
  
  report <- c(list(bubble = bubble_map, patch = patches),modelreport,rasterreport)
  return(report)
}

