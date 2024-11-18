#' Create a div pointing to site content
#' 
#' @description
#' The function ingests a character vector and returns a div.
#'
#' If `x` is `NA_character`, a div of this form:
#'
#' <div
#'   role = "img",
#'   style = "color #XXXX"
#' >
#'   <!-- fontawesome icon -->
#'   <svg></svg>
#' </div>
#'
#' If `x` is not NA, a div of this form:
#'
#' <a href = "path">
#'   <div
#'     role = "img",
#'     style = "color #XXXX"
#'   >
#'     <!-- fontawesome icon -->
#'     <svg></svg>
#'   </div>
#' </a>
#' 
#' @param x Character. Relative path to site content.
#' @param icon_name Character.
#' Name of a fontawesome icon that is passed to `fontawesome::fa()`
#' @param color_enabled Character.
#' Color of "enabled" icons for cells with URI.
#' CSS color attribute. Color name or hex code.
#' @param color_disabled Character.
#' Color of "disabled" icons for cells with URI.
#' CSS color attribute. Color name or hex code.
#'
#' @importFrom glue glue
#' @importFrom htmltools div a
create_div <- function(
  x,
  icon_name,
  color_enabled,
  color_disabled
) {

  # determine icon style
  if (!is.na(x)) {
    icon_style <- glue::glue("color: {color_enabled}")
  } else {
    icon_style <- glue::glue("color: {color_disabled}")
  }

  # create div containing fontawesome icon
  # applying proper styling
  fa_icon_div <- htmltools::div(
    role = "img",
    style = icon_style,
    fontawesome::fa(name = icon_name)
  )

  # create outer div with link
  if (!is.na(x)) {
    div <- htmltools::a(
      href = x,
      fa_icon_div
    )
  } else {
    div <- fa_icon_div
  }

  return(div)

}

#' Transform `gt` column from txt to a div containing an icon and (maybe) link.
#' 
#' @description
#' Transform a column that may contain links into  a column of fontawesome
#' icons. 
#' Cells with a link will have a primary color and wrapped in an <a> tag.
#' Cells without a link will be a div and contain an icon of a secondary color.
#'
#' @param gt_object `gt` table object
#' @param column Bare name for a single table column
#' @inheritParams create_div
#'
#' @importFrom gt text_transform cells_body
#' @importFrom purrr map_chr
create_icon_w_link <- function(
  gt_object,
  column,
  icon_name,
  color_enabled,
  color_disabled
) {

  gt::text_transform(
    data = gt_object,
    locations = gt::cells_body(columns = {{column}}),
    fn = function(x) {

      purrr::map_chr(
        .x = x,
        .f = ~ as.character(
          create_div(
            x = .x,
            icon_name = icon_name,
            color_enabled = color_enabled,
            color_disabled = color_disabled
          )

        )
      )

    }

  )

}