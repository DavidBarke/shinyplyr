
# shinydplyr

The Shiny app created by the code in this repository is my contribution to the Shiny Contest 2020. If you are seeing this page, you are probably interested in the code behind the app. If not, here are two links to the

* [Shiny app](https://davidbarke.shinyapps.io/shinyplyr/) 
* [RStudio Community blog post](https://community.rstudio.com/t/shinyplyr-data-transformation-and-visualisation-2020-shiny-contest-submission/59776)
  
### Folder structure

* `app.R`: Entry point of the app
* `www/css/styles.css`: css style definitions
* `/modules`: R scripts, mostly modules
  * `/About`: About Tab
  * `/Data`: Data Tab
  * `/Help`: Help pages
  * `/Home`: Home Tab
  * `/Import`: Import tabs (csv and rds)
  * `/Init`: Initialisation of the .values environment (see below).
  * `/Operations`: One folder for every operation of the Transformation Table
  * `/Other`: R scripts, that don't belong to any other category
  * `/Output`: Viewer, modules for data and plot output
  * `/R6`: Custom R6 classes: DatasetObject and TabBox
  * `/Transformation`: Transformation table (logic and general layout, details are in /Operations)

### Sourcing files

The `R.utils` package exports the nice function `sourceDirectory`, which allows to recursively source R scripts in a directory. I did some small changes and stored the modified function in `/modules/Other/source_directory.R`. This function is sourced manually and afterwards used to source all other scripts. 

### .values environment

The .values environment is created and filled in `App.R`. It is passed as an argument to
every module server function. That makes it, for example, easy to talk to the viewer (an R6 object), which is stored in `.values$viewer`. For appending a tab you just need to call `.values$viewer$append_tab(shiny::tabPanel(...))`. Another example are the help pages, which can be appended to the viewer by calling `.values$help$open("page_name")`.

### Dataset Explorer

The dataset explorer is created with the `shinyExplorer` package, whose Github repo can be found [here](https://github.com/DavidBarke/shinyExplorer). The explorer is initialised in `/modules/Init/init_tree.R`. Explorer classes are initialised in `./modules/Init/init_explorer_classes.R`. The README of the Github repo is a stub, but there are documentation and two vignettes available [here](https://davidbarke.github.io/shinyExplorer/).

### Transformation Table

`/modules/Transformation/m_table.R` contains the transformation table module. The module server function contains the logic related to adding, dis-, enabling and removing rows. Each row is a `/modules/Transformation/m_row.R`. If you add a row with a specific index for the first time, all operation modules are called. If you later add the row with that index again (for example, after you changed the dataset), the modules are already called and the row is added noticeably faster.

Some operations like `Rename`, `Type` or `Plot` add subrows, whose display can be toggled by an `actionButton`. To achieve that, the `shinyjs` package is used. These subrows start in a closed state. Shiny inputs don't get their value until the UI element is displayed to the user. Therefore the fallback function `function(x, y) if (is.null(x)) y else x` is introduced to start with a default value `y` until the input `x` is displayed.


<!-- badges: start -->
<!-- badges: end -->



