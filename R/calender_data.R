#' Make calendar data
#'
#' Make the calendar data for plotting. Can be used alone if you want a more
#' specialised plotting experience.
#'
#' @inheritParams make_calendar_background
#'
#' @return data.frame. Calendar data that works well with ggplot2.
#' @export
calender_data <- function(month = Sys.Date(),
                          headline_factor = 2,
                          start_monday = TRUE,
                          locale = NULL) {

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

  last <- seq(first, length = 2, by = "months")[2] - 1

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
  dat$day <- ifelse(dat$type == "date", weekdays(dat$date), ifelse(start_monday,
                                                                   "Thursday",
                                                                   "Wednesday"))
  if (format(first, "%m") == "12") {
    dat$week <- as.numeric(strftime(dat$date, format = "%V"))
    dat$week[dat$week == 1] <- dat$week[dat$week == 1] + 52
    dat$week[dat$type == "title"] <- dat$week[dat$type == "title"] - 2 * headline_factor
  } else {
    dat$week <- ifelse(dat$type == "date",
                       as.numeric(strftime(dat$date, format = "%V")),
                       min(as.numeric(strftime(dat$date, format = "%V")) - 2 * headline_factor)
    )
  }

  if (!start_monday) {
    dat$week[dat$day == "Sunday"] <- dat$week[dat$day == "Sunday"]
  }
  dat$day <- factor(dat$day, levels = wdays)

  dat2 <- data.frame(
    date = NA,
    type = NA,
    text = substr(levels(dat$day), 1, 1),
    day = factor(levels(dat$day), levels(dat$day)),
    week = dat$week[dat$type == "title"] + headline_factor,
    stringsAsFactors = FALSE
  )

  plot_data <- rbind(dat, dat2,
                     stringsAsFactors = FALSE
  )

  plot_data$size <- 1
  plot_data$size[1] <- headline_factor

  return(plot_data)
}
