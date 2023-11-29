# Data handling functions

# Functions for non-dimensionalizing values
rmdim.speed <- function(speeds, dia) speeds/dia
rmdim.pressure <- function(pressure, dia, density) pressure/(density*dia^2)
rmdim.volflowrate <- function(Q, dia) Q/(dia^2)

# Loading flow data
loadflowdata <- function(track, date, dia, density){
  data <- read.csv(paste("../results/r-csv-files/", track, 
                         "_results/combined_data_",track,"_165_", date, ".csv", sep = ""), 
                   header = TRUE)
  U.names <- subset(colnames(data), grepl({"U"}, colnames(data)))
  for(y in 1:length(U.names)) data[[U.names[y]]] <- rmdim.speed(data[[U.names[y]]], dia)
  P.names<- subset(colnames(data), grepl({"P"}, colnames(data)))
  for(y in 1:length(P.names)) data[[P.names[y]]] <- rmdim.pressure(data[[P.names[y]]], dia, density)
  data$delta_P <- data$vena_P_avg - data$aorta_P_avg
  data.Q <- read.csv(paste("../results/r-csv-files/", track, 
                           "_results/Qall_165_", date, ".csv", sep = ""), 
                     header = TRUE)
  data$Q <- rmdim.volflowrate(data.Q$Qall, dia)
  return(data)
}

# Loading cost of transport and work data
loadcostdata <- function(track, date){
  data <- read.csv(paste("../results/matlab-csv-files/", track, 
                         "_results/cost_of_transport_165_", date, ".csv", sep = ""),
                   header = FALSE)
  colnames(data) <- c("COT","Work")
  return(data)
}

# Combining flow and COT data
combinedata <- function(flowdata,costdata) {
  if(is.na(costdata)){ 
    costdata <- data.frame("COT" =rep(NA,dim(flowdata)[1]), 
                           "Work" =rep(NA,dim(flowdata)[1]))}
  data <- cbind(flowdata,costdata)
}

# Combining data from multiple tracks after running above functions
combinetracks <- function(data, tracklist){
  numtracks <- length(tracklist)
  alldata <- data[[1]]
  nsims <- as.numeric(dim(alldata)[1])
  alldata$track <- rep(tracklist[1],nsims)
  for(i in 2:numtracks){
    temp.data <- data[[i]]
    temp.data$track <- rep(tracklist[i],nsims)
    alldata <- rbind(alldata,temp.data)
  }
  alldata$track <- factor(alldata$track, levels = tracklist)
  return(alldata)
}

# Loading and shaping data for plotting tracks
plot.track <- function(track){
  race.data <- read.table(paste("../data/ibamr-files/", track,
                                "_files/heart_race_512.vertex", sep = ""),
                          skip = 1, header = FALSE)
  colnames(race.data) <- c("x", "y")
  tube.data <- read.table(paste("../data/ibamr-files/", track,
                                "_files/heart_tube_512.vertex", sep = ""),
                          skip = 1, header = FALSE)
  colnames(tube.data) <- c("x", "y")
  alldata <- list("race" = race.data, "tube" = tube.data)
  return(alldata)
}

# Loading Sobol indices 

load.sobols <- function(track){
  data <- read.table(paste0("../results/gpc-surrogates/",track,"/heart_sob_fulldataset_",track,"_2021-06-15_np2.dat"))
  colnames(data) <- c("aorta_Um_avg","aorta_Um_max","aorta_Ux_avg","aorta_Ux_max","connect_Um_avg",
              "connect_Um_max","connect_Uy_avg","connect_Uy_max","vena_Um_avg","vena_Um_max",
              "vena_Ux_avg","vena_Ux_max","aorta_P_avg","aorta_P_max","vena_P_avg","vena_P_max",
              "delta_P","Q","COT","Work")
  rownames(data) <- c("Wo","CR","Freq","Wo&CR","Wo&Freq","CR&Freq","All")
  return(data)
}
