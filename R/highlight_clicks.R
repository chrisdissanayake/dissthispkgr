#' Highlight Plotly Points
#'
#' This JavaScript code defines a function that is used in a Plotly chart to capture changes in
#' the visibility of the traces on the chart.
#' The [layer_brush] is used to extract a DOM element from a D3.js selection to access
#' its properties and modify its attributes.
#'
#'
#' @param layer_brush Provide a numeric value. If none supplied, layer_brush = 0 will be used.
#' In shiny, if this value is larger than 3, consider using modules.
#' @param plotly_obj plotly object with points.
#' @return
#' @export
#'
#' @examples
#' ## Not run:
#' library(dplyr)
#' library(ggplot2)
#'
#' p_1 <- mtcars %>%
#'   ggplot(aes(x = mpg, y = disp)) +
#'   geom_point()
#'
#'library(plotly)
#'library(htmlwidgets)
#'
#'p_1 %>%
#'  ggplotly() %>%
#'  onRender(jsCode = highlight_clicks(.))
#'
#'## End(Not run)
highlight_clicks <- function(plotly_obj, layer_brush = 0) {
  stopifnot("You did not pass me a 'plotly' object" = inherits(plotly_obj, c("plotly", "htmlwidget")))

  htmlwidgets::JS("function(el, x, layer_brush) {
    el.on('plotly_click', function (data) {",
    stringr::str_glue(
      "var point = document.getElementsByClassName('scatterlayer')[{layer_brush}].getElementsByClassName('scatter')[data.points[0].curveNumber].getElementsByClassName('point')[data.points[0].pointNumber];
      var plotly_div = document.getElementsByClassName('plotly')[{layer_brush}];"
    ),
    "if (plotly_div.backup !== undefined) {",
    stringr::str_glue(
      "var old_point = document.getElementsByClassName('scatterlayer')[{layer_brush}].getElementsByClassName('scatter')[plotly_div.backup.curveNumber].getElementsByClassName('point')[plotly_div.backup.pointNumber]"
    ),
    "if (old_point !== undefined) {
          old_point.setAttribute('d', plotly_div.backup.d);
        }
      }
      plotly_div.backup = {
        curveNumber: data.points[0].curveNumber,
        pointNumber: data.points[0].pointNumber,
        d: point.attributes['d'].value,
        style: point.attributes['style'].value
      }

      point.setAttribute('d', 'M10,0A10,10 0 1,1 0,-10A10,10 0 0,1 10,0Z');
      Shiny.setInputValue(inputName, plotly_div);
    });
  }")
}
