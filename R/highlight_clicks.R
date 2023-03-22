#' Highlight Plotly Points
#'
#' @param layer_brush Provide a numeric value. If none supplied, layer_brush = 0 will be used.
#'
#' @return
#' @export
#'
#' @examples
highlight_clicks <- function(layer_brush = 0) {
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
