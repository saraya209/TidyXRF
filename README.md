# TidyXRF
## About
This app imports `.csv` files exported from **Olympus Vanta C-Series** Handheld X-Ray Fluorescence Spectrometer and creates tidied-up tables in multiple formats.


## How To Use App
- Upload a .csv file exported from the XRF instrument and click *Run Tidy Function*.

- Use the *Raw Table* tab to view the uploaded file, the *Tidy Long Table* and *Tidy Wide Table* tabs display the tidied table in long and wide formats.

- The *Tidying Parameters* allow additional controls: how to deal with `<LOD` values, whether or not `NA` values should be droped from the wide format tables, and whether or not only a subset of elements should appear in the wide format tables. To use the Subset elements function, **you must supply a list of element symbols separated by a comma** (e.g., 'Pb,Zn,Hg,As').

- To create additoinal Excel sheets that compare sample concentration values versus maximum concentration guidelines, select a guideline from the drop-down list.

- Click *Download Data to Excel* button to save tidied tables on your computer.


## Description of Excel File
The exported Excel file will contain either 5 or 7 sheets.

- *Concentrations*: List of samples with element concentrations.

- *Concentrations-and-Error*: List of samples with element concentrations and 1-standard deviation error (Error1s).

- *Concentrations-v-Guidelines*: Wide-format table showing concentrations. Values above selected guideline are appended with the percent-above the guideline values in parenthesis. Cells with concentrations that exceed the selected guideline are highlighted red. For reference, all available guideline values are printed at the bottom of this table with the selected guideline highlighted in red.

- *Guidelines-Description*: Description of the guidelines and additional notes where avaialble.

- *Tidy-Long-Format*: The full data pivoted to long format. This is the master tidied data.

- *Original-Raw*: The original uploaded data.

- *Tidying-Parameters*: List of parameters used to tidy the data.



Author: Samuel.Araya@usda.gov
