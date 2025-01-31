# Useful libraries ----
library(shiny)
library(shinyjs)
library(shinyBS)
library(tidyverse)
library(rsconnect)

# Constants ----
source('./modals.R')

clusters.colors = c('black', 'cyan', 'orange', 'purple', 'green', 'red', 'blue', 'brown', 'darkgrey', 'magenta', 'pink')

coeffs = list(
  x = c(1/6, 5/6, 5/6, 1/6, 3/6, 5/6, 3/6, 1/6, 3/6),
  y = c(5/6, 1/6, 5/6, 1/6, 3/6, 3/6, 5/6, 3/6, 1/6)
)

mink_deg = list(Euclidean = 2, Manhattan = 1)

# Functions ----
# Computes the Minkowski distance
distance_Mink = function(p1_x, p1_y, p2_x, p2_y, deg){
  return((((abs(p1_x - p2_x)) ** deg) + ((abs(p1_y - p2_y)) ** deg)) ** (1/deg)) 
}

# Initializes the labels
# Called when the reset button is pressed
getInitialLabels = function(nbNodes, nbClusters){
  labels = rep(0, nbNodes)

  initial_indexes = sample(nbNodes, nbClusters)
  for (i in 1:length(initial_indexes)){
    labels[initial_indexes[i]] = i
  }

  return(labels)
}

# Computes the centroids of the clusters
updateCenters = function(x, y, labels, nbClusters){
  centres = list(x = rep(0, nbClusters), y = rep(0, nbClusters))
  for (cluster in 1:max(labels)){
    centres$x[cluster] = mean(x[labels == cluster])
    centres$y[cluster] = mean(y[labels == cluster])
  }
  return(centres)
}

# Computes the next step of the K-means algorithm
computeNextStep = function(x, y, centres, deg){
  labels = rep(0, length(x))
  for (i in 1:length(x)){
    dist_min = Inf
    jmin = 0

    for (j in 1:length(centres[['x']])){
      d = distance_Mink(x[i], y[i], centres[['x']][j], centres[['y']][j], deg)
      if (d < dist_min){
        dist_min = d
        jmin = j
      }
    }
    labels[i] = jmin
  }

  return(labels)
}

# Dynamically changes the number of clusters that can be set manually
updateClusterVisibility = function(nbClusters) {
    shinyjs::runjs(sprintf("
      document.querySelectorAll('#cluster-container .cluster').forEach((el, index) => {
        if (index < %d) {
          el.style.display = 'grid';
        } else {
          el.style.display = 'none';
        }
      });
    ", nbClusters))
  }

defaultManualClusterInputs = function(nbClusters, minValx, maxValx, minValy, maxValy){
  for (i in 1:9){
    updateNumericInput(
      inputId = paste0('manualCluster', i, 'x'),
      value = (minValx + (maxValx - minValx)) * coeffs[['x']][i],
      min = minValx,
      max = maxValx
    )
    updateNumericInput(
      inputId = paste0('manualCluster', i, 'y'),
      value = (minValy + (maxValy - minValy)) * coeffs[['y']][i],
      min = minValy,
      max = maxValy
    )
  }
  
}

# Imports the dataframe to use based on user inputs
updateDataset = function(file, dfName, csvOptions){
  dataframe = NULL
  error = NULL
  additional_info = NULL
  if (!is.null(file[['name']])){ 
    file_extension = str_extract(file[['name']], regex('\\.[^.]+$'))
    if (!(file_extension %in% c('.rdata', '.csv'))){
      error = 'Please select a supported file type (.csv or .rdata)'
    } else {
      if (file_extension == '.rdata'){
        load(file[['datapath']])
        if (dfName == ''){
          error = 'Please enter the name of your dataframe'
        } else {
          result = mget(dfName, ifnotfound = list(dfName = NULL))
          found = FALSE
          for (item in result){
            if (class(item) == 'data.frame'){
              col_quanti = names(item)[lapply(names(item), FUN = function(x){return(class(item[[x]]))}) %in% c('integer', 'numeric')]
              if (length(col_quanti) >= 2){
                dataframe = item
                additional_info = list(datafileName = file[['name']], dataframeName = dfName)
                found = TRUE
              } else {
                error = paste0('There must be at least to quantitative columns (', length(col_quanti), 'found)')
              }
            }
          }
          if (!found){
            error = paste0('Please enter a valid dataframe name (', dfName, ' is not in the file)')
          }
        }
        
      } else if (file_extension == '.csv'){
        tryCatch({
          dataframe = read.csv(
            file[['datapath']],
            header = csvOptions$csvHeader,
            sep = if (csvOptions$csvSep == 'TAB') {"\t"} else {csvOptions$csvSep},
            dec = csvOptions$decimalSep
          )
          additional_info = list(datafileName = file[['name']], dataframeName = NULL)
        }, error = function(e){
          error = paste0('Caught error while loading the csv file: ',as.character(e))
        }
        )
      }
    }
  }



  return(list(df = dataframe, error = error, additional_info = additional_info))
}




