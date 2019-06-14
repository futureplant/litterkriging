# function to create a sampling scheme and turn it into a SpatialPointsDataFrame with an ID column

create_samples_to_spatialDF <- function(input, numberpoints = 100, scheme = 'random'){
  samples <- spsample(input, n = numberpoints, type = scheme)
  
  # Create DataFrame with ID numbers
  id_df = as.data.frame((1:length(samples)))
  id_df[1:length(samples),] = as.numeric(id_df[1:length(samples),])
  names(id_df) = 'ID'
  
  samples <- SpatialPointsDataFrame(samples, id_df)
  
  return(samples)
}