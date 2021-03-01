#' Make Calendar Background
#'
#' @param img character. Location of a background image.
#' @param filename character. File name of the output.
#' @param month either a date or name of a month in the current locale.
#' @param resolution "auto" to use the resolution of the input image or vector
#'   of length two with width and height of the output image.
#' @param position position of calendar in the image. Possible values are:
#'   "center", "bottom", "top", "right", "left", "bottomright", "bottomleft",
#'   "topright", "topleft" and a numeric vector of length 4 .
#' @param scale if resolution is provided, should the image be scaled down/up
#'   before cropping to the provided resolution?
#' @param colour colour of the font.
#' @param fill a colour to fill the background.
#' @param family font.
#' @param text_size size of the text in the image.
#' @param headline_factor the factor by which the name of the month is larger
#'   then the remaining text (2 means twice the size).
#' @param start_monday If TRUE the week starts on Monday, otherwise starts on
#'   Sunday.
#' @param locale provide locale if you want to use non English names for months
#'   and days.
#'
#' @return saves image in file location
#' @export
#'
#' @examples
#' \dontrun{
#' make_calendar_background(
#'   system.file("extdata", "mull.jpg", package = "wallpapr")
#' )
#'
#' # put dates in upper right corner
#' make_calendar_background(
#'   system.file("extdata", "zima.png", package = "wallpapr"),
#'   position = c(2, 0, 2, 0)
#' )
#' }
#' @import ggplot2
#' @importFrom magick image_read image_scale geometry_area image_info image_crop
#' @importFrom graphics text
#' @importFrom grid rasterGrob
#' @importFrom stringi stri_datetime_symbols stri_datetime_parse
make_calendar_background <- function(img,
                                     filename,
                                     month = Sys.Date(),
                                     resolution = "auto",
                                     position = "center",
                                     scale = c("width", "height"),
                                     colour = "white",
                                     fill = "grey",
                                     family = "",
                                     text_size = 9,
                                     headline_factor = 2,
                                     start_monday = TRUE,
                                     locale = NULL) {

  plot_data <- calender_data(
    month = month,
    headline_factor = headline_factor,
    locale = locale,
    start_monday = start_monday
  )

  imgage <- image_read(img)

  dpi <- text_size * 100

  if (isTRUE(resolution == "auto")) {
    width <- image_info(imgage)[[2]]
    height <- image_info(imgage)[[3]]
  } else if (length(resolution) >= 2) {
    width <- resolution[1]
    height <- resolution[2]
    if (!is.null(scale)) {
      if (isTRUE(scale)) {
        scale  <-  "width"
      }
      scale = match.arg(scale)
      if (isTRUE(scale == "width")) {
        scl <- image_scale(imgage, geometry_area(width = width))
        if (image_info(scl)[[3]] < height) {
          scl <- image_scale(imgage,
                             geometry_area(width = width,
                                           height = height))
        }
        imgage <- scl
      } else if (isTRUE(scale == "height")) {
        scl <- image_scale(imgage, geometry_area(height = height))
        if (image_info(scl)[[3]] < width) {
          scl <- image_scale(imgage,
                             geometry_area(width = width,
                                           height = height))
        }
        imgage <- scl
      }
    }
    imgage <- image_crop(imgage, geometry_area(width = width, height = height))
  }

  # position
  if (is.character(position)) {
    pos <- switch (position,
                   center      = c(1, 1, 1, 1),
                   bottom      = c(1, 1, 0.1, 1.9),
                   top         = c(1, 1, 1.9, 0.1),
                   right       = c(1.9, 0.1, 1, 1),
                   left        = c(0.1, 1.9, 1, 1),
                   bottomright = c(1.9, 0.1, 0.1, 1.9),
                   bottomleft  = c(0.1, 1.9, 0.1, 1.9),
                   topright    = c(1.9, 0.1, 1.9, 0.1),
                   topleft     = c(0.1, 1.9, 1.9, 0.1)
    )
  } else if (is.numeric(position)) {
    pos <- position
  }

  plot <- ggplot(
    plot_data,
    aes_string(
      x = "day",
      y = "week",
      label = "text",
      size = "size"
    )
  ) +
    annotation_custom(grid::rasterGrob(imgage)) +
    geom_text(colour = colour, family = family, show.legend = FALSE) +
    scale_size_identity() +
    scale_x_discrete(expand = expansion(mult = c(pos[1], pos[2]))) +
    scale_y_reverse(expand = expansion(mult = c(pos[3], pos[4]))) +
    theme_void() +
    theme(panel.background = element_rect(fill = fill))

  if (missing(filename)) {
    filename <- paste0(format(
      plot_data$date[1],
      format = "%B-%Y"
    ), ".", tools::file_ext(img))
  }

  ggsave(
    filename = filename,
    dpi = dpi,
    width = width / dpi,
    height = height / dpi,
    units = "in",
    plot = plot
  )
  message("wallpaper saved as ", filename)
  return(invisible(filename))
}
