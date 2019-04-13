#' Make Calendar Background
#'
#' @param img character. Location of a background image.
#' @param filename character. File name of the output.
#' @param month either a date or name of a month in the current locale.
#' @param resolution "auto" to use the resolution of the input image or vector
#'   of length two with width and height of the output image.
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
#' make_calendar_background(
#'   system.file("extdata", "mull.jpg", package = "wallpapr")
#' )
#' @import ggplot2
#' @import magick
#' @importFrom graphics text
#' @importFrom grid rasterGrob
#' @importFrom tibble tibble
#' @importFrom stringi stri_datetime_symbols stri_datetime_parse
make_calendar_background <- function(img,
                                     filename = paste0(format(
                                       Sys.Date(),
                                       format = "%B"
                                     ), img_format(img)),
                                     month = Sys.Date(),
                                     resolution = "auto",
                                     colour = "white",
                                     fill = "grey",
                                     family = "",
                                     text_size = 9,
                                     headline_factor = 2,
                                     start_monday = TRUE,
                                     locale = NULL) {
  imgage <- image_read(img)

  if (is.character(month)) {
    month <- as.Date(
      stri_datetime_parse(
        str = month,
        format = "MMMM",
        locale = locale
      )
    )
  }
  if (is.na(month)) {
    stop("\"month\" was not given as a proper date or name of a month")
  }

  first <- as.Date(
    paste0(
      format(
        month,
        format = "%Y-%m"
      ), "-01"
    )
  )

  last <- as.Date(
    paste(
      format(first, "%Y"),
      as.numeric(format(first, "%m")) + 1,
      format(first, "%d"),
      sep = "-"
    )
  ) - 1

  if (is.null(locale)) {
    if (start_monday) {
      wdays <- c(
        "Monday", "Tuesday", "Wednesday", "Thursday",
        "Friday", "Saturday", "Sunday"
      )
    } else {
      wdays <- c(
        "Sunday", "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday"
      )
    }
  } else {
    if (start_monday) {
      wdays <- stri_datetime_symbols(locale = locale)$Weekday[c(2:7, 1)]
    } else {
      wdays <- stri_datetime_symbols(locale = locale)$Weekday
    }
  }

  dat <- data.frame(
    date = c(first, seq(first, last, "days")),
    type = "date",
    stringsAsFactors = FALSE
  )

  dat$type[1] <- "title"
  dat$text <- ifelse(dat$type == "date",
                     format(dat$date, "%d"),
                     format(dat$date, "%B"))
  dat$text <- gsub("^0", "", dat$text)
  dat$day <- ifelse(dat$type == "date", weekdays(dat$date), "Thursday")
  dat$week <- ifelse(dat$type == "date",
    as.numeric(strftime(dat$date, format = "%V")),
    min(as.numeric(strftime(dat$date, format = "%V")) - 2 * headline_factor)
  )
  dat$day <- factor(dat$day, levels = wdays)

  dat2 <- data.frame(
    date = NA,
    type = NA,
    text = substr(levels(dat$day), 1, 1),
    day = factor(levels(dat$day), levels(dat$day)),
    week = min(as.numeric(strftime(dat$date, format = "%V")) - 1),
    stringsAsFactors = FALSE
  )

  plot_data <- rbind(dat, dat2,
    stringsAsFactors = FALSE
  )

  plot_data$size <- 1
  plot_data$size[1] <- headline_factor

  dpi <- text_size * 100

  if (isTRUE(resolution == "auto")) {
    width <- image_info(imgage)[[2]]
    height <- image_info(imgage)[[3]]
  } else if (length(resolution) >= 2) {
    width <- resolution[1]
    height <- resolution[2]
    imgage <- image_crop(imgage, geometry_area(width = width, height = height))
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
    scale_y_reverse(expand = c(1, -1)) +
    scale_x_discrete(expand = c(1, 1)) +
    theme_void() +
    theme(panel.background = element_rect(fill = fill))

  ggsave(
    filename = filename,
    dpi = dpi,
    width = width / dpi,
    height = height / dpi,
    units = "in",
    plot = plot
  )
  message("wallpaper saved as ", filename)
}


#' Get image format
#'
#' @param img name of image.
#' @noRd
img_format <- function(img) {
  stringi::stri_extract_last_regex(img, ".{4}$")
}
