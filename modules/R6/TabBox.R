#' TabBox
#'
#' \code{\link[R6:R6Class]{R6Class}} expanding the functionalities of
#' \code{\link[shinydashboard]{tabBox}}. A TabBox object contains methods for
#' creating the tabBox, inserting and removing \code{\link[shiny]{tabPanel}}
#' elements and the ability to expand the usual tabBox with an additional action
#' button which closes a tab.
#'
#' @section Usage:
#' \preformatted{viewer <- TabBox$new("viewer", title = "Viewer")
#'
#' # In UI:
#' ui <- viewer$tabBox()
#'
#' # In server function:
#' server <- function(input, output, session) {
#'   viewer$set_session(session)
#' }
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(id, title = "Viewer", width = 6, height = NULL,
#'   side = c("left", "right"))}}{Initialize the TabBox object.
#'     \tabular{ll}{
#'       \code{id} \tab You can use \code{input$id} in your server logic to
#'       determine which of the current tabs is active. The value will correspond
#'       to the \code{value} argument that is passed to \code{\link[shiny]{tabPanel}}.
#'       Don't forget to namespace \code{id}, if inside a module. \cr
#'       \code{title} \tab Title for the tabBox. \cr
#'       \code{width} \tab The width of the box, using the Bootstrap grid system.
#'       This is used for row-based layouts. The overall width of a region is 12,
#'       so the default valueBox width of 4 occupies 1/3 of that width. For
#'       column-based layouts, use \code{\link[base]{NULL}} for the width; the
#'       width is set by the column that contains the box. \cr
#'       \code{height} \tab The height of a box, in pixels or other CSS unit. By
#'       default the height scales automatically with the content. \cr
#'       \code{side} \tab Which side of the box the tabs should be on ("left" or
#'       "right"). When \code{side="right"}, the order of tabs will be reversed.
#'     }
#'   }
#'   \item{\code{append_tab(tab, select = FALSE, closeable = TRUE)}}{Append a
#'     \code{\link[shiny:tabPanel]{tabPanel}} to the tabBox.
#'     \tabular{ll}{
#'       \code{tab} \tab A \code{\link[shiny]{tabPanel}}. \cr
#'       \code{select} \tab If \code{\link[base:logical]{TRUE}}, select \code{tab}
#'       upon being inserted. \cr
#'       \code{closeable} \tab If \code{\link[base:logical]{TRUE}}, \code{tab}
#'       is closeable via an \code{\link[shiny]{actionButton}}.
#'     }
#'   }
#'   \item{\code{insert_tab(tab, target, position = c("before", "after"),
#'   select = FALSE, closeable = TRUE)}}{Insert a
#'     \code{\link[shiny:tabPanel]{tabPanel}} to the tabBox next to \code{target}.
#'     \tabular{ll}{
#'       \code{tab} \tab A \code{\link[shiny]{tabPanel}}. \cr
#'       \code{target} \tab The \code{value} of an existing \code{\link[shiny]{tabPanel}},
#'       next to which \code{tab} will be added. \cr
#'       \code{position} \tab Should \code{tab} be added before or after the
#'       \code{target} tab? \cr
#'       \code{select} \tab If \code{\link[base:logical]{TRUE}}, select \code{tab}
#'       upon being inserted. \cr
#'       \code{closeable} \tab If \code{\link[base:logical]{TRUE}}, \code{tab}
#'       is closeable via an \code{\link[shiny]{actionButton}}.
#'     }
#'   }
#'   \item{\code{is_open(value)}}{Returns \code{\link[base:logical]{TRUE}}, if
#'   a tab with \code{value = value} is open.
#'   }
#'   \item{\code{is_value(value)}}{Returns \code{\link[base:logical]{TRUE}}, if
#'   a tab with \code{value = value} ever existed (currently open or not).
#'   }
#'   \item{\code{prepend_tab(tab, select = FALSE, closeable = TRUE)}}{Prepend a
#'     \code{\link[shiny:tabPanel]{tabPanel}} to the tabBox.
#'     \tabular{ll}{
#'       \code{tab} \tab A \code{\link[shiny]{tabPanel}}. \cr
#'       \code{select} \tab If \code{\link[base:logical]{TRUE}}, select \code{tab}
#'       upon being inserted. \cr
#'       \code{closeable} \tab If \code{\link[base:logical]{TRUE}}, \code{tab}
#'       is closeable via an \code{\link[shiny]{actionButton}}.
#'     }
#'   }
#'   \item{\code{remove_tab(target)}}{Remove the tab with \code{value=target}.
#'   }
#'   \item{\code{set_session(session)}}{Call this function in the server function
#'   to connect the TabBox with a session.
#'     \tabular{ll}{
#'       \code{session} \tab A shiny \code{\link[shiny:session]{session}} object.
#'     }
#'   }
#'   \item{\code{tabBox()}}{Return the HTML representing the
#'   \code{\link[shinydashboard]{tabBox}}.
#'   }
#' }
#'
#' @name TabBox
NULL

#' @export
TabBox <- R6::R6Class(
  "TabBox",
  public = list(
    initialize = function(id, title = "Viewer",
                          width = 6, height = NULL, side = c("left", "right")) {
      private$id <- id
      private$title <- title
      private$width <- width
      private$height <- height
      private$side <- match.arg(side)
    },
    
    append_tab = function(tab, select = TRUE, closeable = TRUE) {
      private$tabCounter <- private$tabCounter + 1
      data_value <- tab$attribs[["data-value"]]
      if (data_value %in% private$open_tab_values) {
        updateTabsetPanel(
          session = private$session,
          inputId = private$id,
          selected = data_value
        )
      } else {
        private$open_tab_values <- c(private$open_tab_values, data_value)
        shiny::appendTab(
          inputId = private$id,
          tab = tab,
          select = select,
          session = private$session
        )
        if (closeable) private$createActionButton(data_value)
      }
      
      if (data_value %in% private$tab_values) {
        return(FALSE)
      } else {
        private$tab_values <- c(private$tab_values, data_value)
        return(TRUE)
      }
    },
    
    get = function(what) {
      if (missing(what)) return(names(private))
      private[[what]]
    },
    
    insert_tab = function(tab, target, position = c("before", "after"),
                          select = FALSE, closeable = TRUE) {
      private$tabCounter <- private$tabCounter + 1
      data_value <- tab$attribs[["data-value"]]
      if (data_value %in% private$open_tab_values) {
        updateTabsetPanel(
          session = private$session,
          inputId = private$id,
          selected = data_value
        )
      } else {
        private$open_tab_values <- c(private$open_tab_values, data_value)
        private$tab_values <- c(private$tab_values, data_value)
        shiny::insertTab(
          inputId = private$id,
          tab = tab,
          target = target,
          position = match.arg(position),
          select = select,
          session = private$session
        )
        if (closeable) private$createActionButton(data_value)
      }
      invisible(self)
    },
    
    is_open = function(value) {
      value %in% private$open_tab_values
    },
    
    is_value = function(value) {
      value %in% private$tab_values
    },
    
    prepend_tab = function(tab, select = FALSE, closeable = TRUE) {
      private$tabCounter <- private$tabCounter + 1
      data_value <- tab$attribs[["data-value"]]
      if (data_value %in% private$open_tab_values) {
        updateTabsetPanel(
          session = private$session,
          inputId = private$id,
          selected = data_value
        )
      } else {
        private$open_tab_values <- c(private$open_tab_values, data_value)
        private$tab_values <- c(private$tab_values, data_value)
        shiny::prependTab(
          inputId = private$id,
          tab = tab,
          target = target,
          select = select,
          session = private$session
        )
        if (closeable) private$createActionButton(data_value)
      }
      invisible(self)
    },
    
    remove_tab = function(target) {
      index <- which(private$open_tab_values == target)
      private$open_tab_values <- private$open_tab_values[-index]
      shiny::removeTab(
        inputId = private$id,
        target = target,
        session = private$session
      )
      invisible(self)
    },
    
    set_session = function(session) {
      private$session <- session
    },
    
    tabBox = function(collapsible = FALSE) {
      if (!private$once) {
        if (!collapsible) {
          ui <- shinydashboard::tabBox(
            id = private$session$ns(private$id),
            title = private$title,
            width = private$width,
            height = private$height,
            side = private$side
          )
        } else {
          ui <- shinydashboard::box(
            title = private$title,
            collapsible = TRUE,
            width = private$width,
            height = private$height,
            shinydashboard::tabBox(
              id = private$session$ns(private$id),
              width = 12,
              side = private$side
            )
          )
          ui$children[[1]]$children[[2]]$children[[1]]$attribs$class <- paste(
            ui$children[[1]]$children[[2]]$children[[1]]$attribs$class,
            "collapsible-tab-box"
          )
        }
        private$once <- TRUE
        return(ui)
      }
      else print("tabBox has been already created.")
    }
  ),
  private = list(
    id = NULL,
    title = "Viewer",
    width = 6,
    height = NULL,
    side = "left",
    once = FALSE,
    session = NULL,
    tabCounter = 0,
    open_tab_values = character(),
    tab_values = character(),
    
    createActionButton = function(data_value) {
      closeId <- private$id %_% private$tabCounter
      div_button <- div(
        class = "div-btn-close",
        actionButton(
          inputId = private$session$ns(closeId),
          label = NULL,
          icon = icon("window-close")
        )
      )
      selector <- paste0("#", private$session$ns(private$id), " li a[data-value=\"", data_value, "\"]")
      insertUI(
        selector = selector,
        where = "beforeEnd",
        ui = div_button
      )
      observeEvent(private$session$input[[closeId]], {
        self$remove_tab(target = data_value)
      }, domain = private$session)
    }
  )
)
