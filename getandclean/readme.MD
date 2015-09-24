Script loads required libraries (dplyr, reshape2)
Gathers working directory, and loads in 6 files, 2 each (of train/test data) for the three data sets (subject, x/data, y/activity)
Header data for the activities is stored in a separate file, that is loaded from features.txt, cleaned of potentially bad characters, and then used to rename the X files
Training and Test data sets are bound together, assumption(validated) is that both files will have the same number/type of columns.
Subject and Activity data columns are renmaed to meaningful names.
A list of columns with the mean and standard deviation data are extracted via grep from the meaninful naming conventions, combined, and then used to extract from the prior combined data set.
A tidy set is then produced by merging the three data sets

To get the final mean of each column by subject and activity, the data is melted, averaged, and then recast, and finally written to a file.

