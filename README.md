## dataMeta - An R package to create and append a data dictionary for an R dataset  

Datasets generally require a data dictionary that will incorporate information related to the dataset's variables and their descriptions, including the description of any variable options. This information is crucial, particularly when a dataset will be shared for further analyses. However, constructing it may be time-consuming. The `dataMeta` package has a collection of functions that are designed to construct a data dictionary that appended to the original dataset as an attribute, along with other information generally provided in other software as metadata. This information will include: the time and date when it was edited last, the user name, a main description of the dataset. Finally, the dataset is saved as an R dataset (.Rd).

```diff
Suggestions are most welcome!
```

<b>Install dataMeta in R from GitHub using devtools:</b>

```
install.packages("devtools")
library(devtools)
install_github("dmrodz/dataMeta")
```

### Feel free to view the [vignette](http://htmlpreview.github.io/?https://github.com/dmrodz/dataMeta/blob/master/vignettes/dataMeta_Vignette.html)!  
  
<b> dataMeta workflow </b>
  
There are three basic steps to building a data dictionary with this package and which are outlined in the figure below. First, a "linker" data frame is created in which the user will add each variable description and also provide a variable "type" or key, which will be explained in the next section. THe vaiable type will depend on whether the user wants to list all available variable options or if a range of variable values would suffice. Secondly, the main data dictionary is created using the original dataset and the linker data frame. Here, the user will be able to construct any additional variable option descriptions as needed or just build a dictionary with variable names, their descriptions and options. Finally, the user can append the dictionary to the original dataset as an R attribute, along with the date in which the dictionary is created, the author name, and also general R attributes included for data frames.  The new dataset with its attributes is then saved as an R dataset (.rds).  
  
<img src="https://cloud.githubusercontent.com/assets/7705604/25308808/656831e4-2793-11e7-89b7-d5ee2e112dbd.jpeg" alt="workflow" width="800" align="auto"></p>
