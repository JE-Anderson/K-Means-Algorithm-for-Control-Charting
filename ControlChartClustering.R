#Joshua Anderson
#Data Mining
#HW4 - Control Chart Cluster Analysis

#Cluster given control charts into 6 groups representing 6 types of control chart
# 1. Normal  2. Cyclic  3. Increasing Trend  4. Decreasing Trend  5. Upward Shift  6. Downward Shift


#Euclidean distance function for distance between 2 rows.  Any row can be used as centroid.
dis <- function(row1, row2)
{
  distance <- 0
  
  for(i in 1:60) 
  {
    distance <- distance + (row1[i] - row2[i])^2
  }
  
  distance <- sqrt(distance)
  return(distance)
}


#Primary Clustering Method - takes in whole dataframe and outputs 6 dataframes representing the 6 time series clusters.  Also outputs the seed chosen by the random numbers as last value in output list
cluster <- function(df)
{
  #Select 6 random, distinct rows to be starting centroids
  #Add a column onto the dataframe to store which cluster a row is currently assigned to
  df$cluster <- NA
  firstChoices <- sample(1:600, 6, replace=FALSE) #Starting centroids row indices
  centList <- vector("list", 6) #List of centroids
  distList <- vector("numeric", 6) #Vector to store distances to centroids
  for(i in 1:6)
  {
    centList[[i]] <- df[firstChoices[i], ]
    df[firstChoices[i], 61] <- i
  }
  
  
  #Determine initial cluster assignments based on randomly chosen rows as centroids
  #   After this first determination, will repeat process in a while loop until
  #   difference in sum squared error between runs is below a set threshhold
  for(i in 1:600)
  {
    if(i %in% firstChoices == FALSE)
    {
      #Calculate distance for each row to each centroid
      for(j in 1:6)
      {
        distList[j] <- dis(centList[[j]], df[i,])
      }
      
      #Assign cluster value for row based on min distance
      df[i,61] <- which.min(distList)
    }
  }
  
  #Find initial sum square error for initial cluster assignments
  sumSqCur <- sumSqErr(df, centList)
  
  
  #Update each centroid in centList based on row assignments/averages
  for(i in 1:6)
  {
    subFrame <- df[ which(df$cluster == i), ] #subframe containing only rows assigned to cluster i
    for(j in 1:60)
    {
      centList[[i]][j] <- mean(subFrame[,j])
    }
  }
  
  #Main while loop - repeatedly re-assigns rows to clusters, re-determines centroids, then repeats until threshold met
  sumSqPrev <- sumSqCur + 100000000 #Just to initialize this value for use in while loop with a value guranteeing loop runs at least once
  while(sumSqCur / sumSqPrev != 1)
  {
    #Assign cluster value to every row
    for(i in 1:600)
    {
      #Calculate distance for each row to each centroid
      for(j in 1:6)
      {
        distList[j] <- dis(centList[[j]], df[i,])
      }
      
      #Assign cluster value for row based on min distance
      df[i,61] <- which.min(distList)
    }
    
    #Update sumSqErrors
    sumSqPrev <- sumSqCur
    sumSqCur <- sumSqErr(df, centList)
    
    #Update each centroid in centList based on row assignments/averages
    for(i in 1:6)
    {
      subFrame <- df[ which(df$cluster == i), ] #subframe containing only rows assigned to cluster i
      for(j in 1:60)
      {
        centList[[i]][j] <- mean(subFrame[,j])
      }
    }
  }
  
  outList <- vector("list", 7) #List of dataframes with each frame containing rows assigned to only one specific cluster
  for(i in 1:6)
  {
    outList[[i]] <- df[ which(df$cluster == i), ]
    rownames(outList[[i]]) <- 1:nrow(outList[[i]])
  }
  outList[[7]] <- firstChoices
  
  return(outList)
}


#Sum Squared Error Criterion Function
sumSqErr <- function(df, centList)
{
  res <- 0
  for(i in 1:600)
  {
    res <- res + (dis(df[i,], centList[[ df[i,61] ]]))^2
  }
  
  return(res)
}


main <- function()
{
  #Get the working directory from the user.
  path <- readline(cat("Please enter the filepath for the location where the program and data files have been extracted to.\n"))
  path <- gsub("\\\\", "/",path)
  setwd(path)
  
  #Import data set.  Assumes data files are in the working directory.
  ControlChartData <- read.table("synthetic_control_data.txt", quote="\"", comment.char="")
  
  #Run cluster analysis
  outList <- cluster(ControlChartData)
  
  #Plot the results
  plot(1:60, outList[[1]][1,1:60], type = "l", xlab = "Time", ylab = "Results", main = "Plot 1")
  for(i in 2:dim(outList[[1]])[1])
  {
    lines(1:60, outList[[1]][i,1:60], type = "l")
  }
  plot(1:60, outList[[2]][1,1:60], type = "l", xlab = "Time", ylab = "Results", main = "Plot 2")
  for(i in 2:dim(outList[[2]])[1])
  {
    lines(1:60, outList[[2]][i,1:60], type = "l")
  }
  plot(1:60, outList[[3]][1,1:60], type = "l", xlab = "Time", ylab = "Results", main = "Plot 3")
  for(i in 2:dim(outList[[3]])[1])
  {
    lines(1:60, outList[[3]][i,1:60], type = "l")
  }
  plot(1:60, outList[[4]][1,1:60], type = "l", xlab = "Time", ylab = "Results", main = "Plot 4")
  for(i in 2:dim(outList[[4]])[1])
  {
    lines(1:60, outList[[4]][i,1:60], type = "l")
  }
  plot(1:60, outList[[5]][1,1:60], type = "l", xlab = "Time", ylab = "Results", main = "Plot 5")
  for(i in 2:dim(outList[[5]])[1])
  {
    lines(1:60, outList[[5]][i,1:60], type = "l")
  }
  plot(1:60, outList[[6]][1,1:60], type = "l", xlab = "Time", ylab = "Results", main = "Plot 6")
  for(i in 2:dim(outList[[6]])[1])
  {
    lines(1:60, outList[[6]][i,1:60], type = "l")
  }
  
  #return outputs for inspection
  return(outList)
  
}

Results <- main()