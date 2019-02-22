######################################################################################################################################
# Authors: Rami Masoud, Jeet Roal, Mudit Kumar, Jonathan Munoz                                                                       #
# Class  : UIC CS 424, Spring 2019                                                                                                   #
# File   : (AggregateAndOutput.r) PreProcessing commands for project #2 Every Breath You Take                                        #
# About  : The CSV files taken off of the EPA website are very large and take sometime to process into a dataframe.                  #
#          By aggregating the files into 2 dataframes before hand then outputting into a feather format using the feather library,   #
#          we are able to decrease loading times by a considerable amount.                                                           #
#          Below you could follow the steps and instructions to use in your own R application.                                       #
######################################################################################################################################

# Libraries used in our preProcessing file include
library(readr)
library(fst)

# Import the csv file names that hold the data for "daily" recorded information
dailyFiles = list.files(pattern = "^daily.*")

# Combine all of the "daily" files into one data frame.
dailyData = do.call(rbind, lapply(dailyFiles, function(x) read_csv(x, col_names = TRUE)))

# Save the dataframe into a fst file format
# A file is outputed 'dailyData.fst' into your current directory
write_fst(dailyData, 'dailyData.fst')

# Import the csv file names that hold the data for "hourly gasses" recorded information
hourlyFiles = list.files(pattern = "^hourly.*")

# Combine all the files into one data frame.
hourlyData = do.call(rbind, lapply(hourlyFiles, function(x) read_csv(x, col_names = TRUE)))

# Save the dataframe into a fst file format
# A file is outputed 'hourlyGasData.fst' into your current directory
write_fst(hourlyData, 'hourlyData.fst')

