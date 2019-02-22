NOTICE: The files in this directory have already had thier unnecessary columns removed, then aggregated, finally outputted into 2 files of fst format.
		These 2 files are ready to use and can be found within the 'step 2' folder with the file name dailyData.fst & hourlyData.fst.
		
STEP 0: If you'd like start fresh and pre-process on your own, 
		Delete all files in both folders ending in .CSV & .fst
		Fill the "dailyData" and "hourlyData" folders, with the un-edited files off of the EPA website, the rest of the steps are below.

STEP 1: Install python3, by entering the following commands into a debian based linux terminal

		sudo apt update && upgrade
		sudo apt install python3 python3-pip ipython3

STEP 2: Install the pandas python package, by entering the following command into a debian based linux terminal

		sudo apt-get install python3-pandas

STEP 3: Finally you can run the script to remove the extra columns from your CSV files,
		Go into each of folders "dailyData" and "hourlyData" and run the script seperatly for each.
		You can do this by opening another terminal window seperatly in each folders and entering in the following script.
		
		python3 remove_column_script.py -f remove_columns.txt

STEP 4: Wait, the script will automatically work on each of the CSV files located within each of the folders.

		RESULTS: We were able to decrease the data files considerably.
		hourlyData: Went from 6.48 GB to 2.97 GB
		dailyData : Went from 649 MB to 370 MB
		 
STEP 5: Now you can go ahead and start aggregating the CSV files.
		Start out by moving the edited CSV files inside of the "dailyData" and "hourlyData" folders 
		into the folder labled "Step 2 - Aggregate The CSVs And Output To fst"
		
STEP 6: Then, With RStudio open the file called "AggregateAndOutput.r" located within the same folder.
		Set the current working directory to the current folder using the command
		
		setwd("ENTER THE CURRENT DIRECTORY HERE")
		
STEP 7: Next enter in each of the commands included in the file one by one into the console window in RStudio.
		Make sure you import the used libraries first!
		
		library(readr)
		library(fst)
		
		reading in the files can take some time so, please be patient.

STEP 8: Once done you should have 2 seperate files that have been outputted into your current working directory.

		dailyData.fst
		hourlyData.fst
		
STEP 9: Almost done! To use these files inside of your own implementation of project 2, place the 2 "fst" files into your working directory.
		Then enter in the following commands into your app
		
		dailyData  <- read_fst('dailyData.fst')
		hourlyData <- read_fst('hourlyData.fst')
		
		You now should have two decluttered and quick opening data frames that you can use!
		
		
