# **Thioflavin T (ThT) Analysis Shiny App**

This Shiny app, ThT\_analysis.R, is designed to analyze Thioflavin T (ThT) fluorescence data, typically used in protein aggregation studies. The app allows users to upload a CSV file, visualize the raw and smoothened data, and automatically calculate key parameters of the aggregation kinetics, such as lag time, growth rate, and plateau values.

## **How to Use**

1. **Launch the app:** Run the ThT\_analysis.R script in RStudio. This will launch the Shiny application in a new window or your web browser.  
2. **Upload your data:** Click the "Choose CSV File" button and select your CSV file. The file should be formatted with the first column as 'time' (in minutes) and subsequent columns representing different samples. A sample file, test.csv, is provided for demonstration.  
3. **View and interact:** The app will display a plot of your data. You can select a sample, observe the raw data and manually select the baseline, growth phase and plateau regions, which will be highlighted on the plot on the right. You can zoom in and out as needed by double clicking on a selected region and outside, respectively.  
4. **Calculate:** Once the baseline, growth and plateau regions are selected, click on “Calculate” to calculate the parameters and display a normalized plot showing the raw data, smoothened data and fit for the maximum slope.  
5. **View results:** Above the plot, the app will display a table of the calculated parameters for each sample in your dataset, including:  
   * Lag Time (lagtime): The time before the aggregation process begins to accelerate.  
   * Time at Max Slope (tnode): The time at which the maximum slope occurs.  
   * Time at 5%, 50%, 95% aggregation (t5, t50, t95): The times at these points are reached.  
   * Max Slope (dymax\_raw and dymax\_normalized): The maximum rate of aggregation, provided as raw intensity values and normalized intensity values.
6. **Repeat:** Steps 1-5 for all the samples. Once done, the table can be downloaded as a .csv file.

## **Input Data Format (test.csv)**

The input CSV file must have a specific format for the app to function correctly:

* **Column 1:** Must be named time and contain numerical values representing time points (e.g., in minutes).  
* **Subsequent Columns:** Each subsequent column represents a different sample and should contain the fluorescence intensity values at each time point. The header names for these columns will be used as the sample identifiers in the app's output.

Example (test.csv):

time,Sample X10,Sample X11,Sample X12  
0,78,135,123  
1.5,80,127,119  
3,80,120,114  
...

## **Dependencies**

This app requires the following R packages to be installed:

* shiny  
* readr  
* dplyr  
* tidyr  
* ggplot2  
* zoo

You can install them by running this command in your R console:

install.packages(c("shiny", "readr", "dplyr", "tidyr", "ggplot2", "zoo"))

