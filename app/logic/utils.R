box::use(
  checkmate[assert_string],
  shiny[div, img, reactiveVal, tagList],
  waiter[spin_3, waiter_preloader],
)

#' @export
waiter_on_load <- function(html) {
  shiny::tagList(html)
}

#' @export
loading_screen <- function(text = "Loading...", bkg_color = "white") {
  assert_string(text, min.chars = 1)
  assert_string(bkg_color, min.chars = 1)
  waiter_preloader(
    html = tagList(
      img(
        src = "static/Callodine_Capital.png", 
        style = "display:block; margin:auto; width:100%; height:auto;"
      ),
      div(style = "text-align:center; margin-top:20px;", spin_3())
    ),
    color = bkg_color,
    fadeout = TRUE
  )
}