tab_about_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "help-page",
    htmltools::p(
      "This application was created using the",
      htmltools::a(href = "https://shiny.rstudio.com", "Shiny"),
      "framework developed by",
      htmltools::a(href = "https://rstudio.com", "RStudio"),
      "for the R programming language. It is my contribution to the Shiny Contest
      2020. I'm a 21 years old student of Physical Engineering and Computer
      Science at the Technische Universität Berlin. I started programming with
      R and Shiny, when I started working as a student assistant at the
      Institute for Quality Science at my university in late 2017. Since then I have
      developed and supervised two lectures: Introduction to Data Analtytics with R 
      and Applied Data Science for Quality Engineering. This application compresses 
      many things together that I have learned in the last two and a half years and shows some
      unique features:"
    ),
    htmltools::tags$ul(
      htmltools::tags$li(
        "The Transformation Table shows how to dynamically call Shiny modules. The
        robust layout is created with the CSS Grid Layout."
      ),
      htmltools::tags$li(
        "The Dataset Explorer is created with the",
        htmltools::a(href = "https://github.com/DavidBarke/shinyExplorer", "shinyExplorer"),
        "package, which I started developing last August. It is currently
        only available on GitHub."
      ),
      htmltools::tags$li(
        "The Viewer is a shinydashboard::tabBox embedded in an R6 class. It
        tracks all open tabs and adds a close button to each tab."
      )
    ),
    htmltools::p(
      "If you are interested in the code, you should visit my",
      htmltools::a(
        href = "https://github.com/DavidBarke/shinyplyr", "Github repo."
      ),
      "If you find errors, feel free to file an issue."
    )
  )
}

tab_about <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  
}