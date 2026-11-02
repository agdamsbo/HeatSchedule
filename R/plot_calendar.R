# --------------------------------------------------------------
#  Minimal calendar‑heat‑map (drop‑in replacement for the old plot_calendar)
# --------------------------------------------------------------
#' Calendar heat‑map of occupied hours (slim version with palette handling)
#'
#' @param df            Data frame containing
#'                        * `date` – a Date (or POSIXct) for the day, and\n
#'                        * `duration`   – total occupied hours for that day (0‑24).\n
#'                      Missing days are filled with 0.
#' @param start         "S" (Sunday, default) or "M" (Monday). Determines the first
#'                      day of the week on the x‑axis.
#' @param day.size      Size of the day‑number text (default = 5, same as the
#'                      original call).
#' @param title         Plot title (character). If missing a title is built from
#'                      the date range.
#' @param subtitle      Plot subtitle (character). Passed straight to ggplot2.
#' @param legend.pos    Legend position – e.g. "bottom", "right".
#' @param palette       **Character vector of colours**.
#'                      * Length = 1 → gradient `white → palette[1]`.
#'                      * Length ≥ 2 → gradient `palette[1] → palette[length(palette)]`.
#'                      If `NULL` a default blue (`#4575b4`) is used.
#' @param source_caption Optional caption that appears at the bottom of the plot.
#' @return A `ggplot2` object (returned invisibly, like the original).
#' @export
#' @examples
#' ## -----------------------------------------------------------------
#' ##  Fake data – one row per day with random occupied hours
#' ## -----------------------------------------------------------------
#' set.seed(1)
#' df <- data.frame(
#'   date = seq(as.Date("2023-01-01"), as.Date("2023-03-31"), by = "day"),
#'   duration   = sample(0:24, 90, replace = TRUE)
#' )
#' p <- plot_calendar(df, start = "M", title = "Occupied hours – Q1 2023",
#'               palette = c("#edf8fb", "#2c7fb8"))   # two‑colour gradient
#' p
plot_calendar <- function(df,
                          start         = c("S", "M"),
                          day.size      = 5,
                          title,
                          subtitle      = "",
                          legend.pos    = "bottom",
                          palette       = NULL,
                          source_caption = NULL,
                          ...) {

  ## --------------------------------------------------------------
  ## 1. Argument checks
  ## --------------------------------------------------------------
  start <- match.arg(start)

  # browser()

  ## --------------------------------------------------------------
  ## 2. Validate required columns
  ## --------------------------------------------------------------
  req_cols <- c("date", "duration")
  missing_cols <- setdiff(req_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("`df` must contain columns: ", paste(missing_cols, collapse = ", "))
  }

  df$duration <-as.numeric(df$duration)

  ## Coerce to Date (works for POSIXct as well)
  df$date <- as.Date(df$date, tz = "UTC")

  ## --------------------------------------------------------------
  ## 3. Build a complete day sequence (fill missing days with 0)
  ## --------------------------------------------------------------
  full_seq <- seq(min(df$date), max(df$date), by = "day")
  day_tbl  <- data.frame(.date = full_seq, stringsAsFactors = FALSE)

  day_tbl <- merge(day_tbl, df,
                   by.x = ".date", by.y = "date",
                   all.x = TRUE, sort = FALSE)
  # day_tbl$duration[is.na(day_tbl$duration)] <- 0L

  ## --------------------------------------------------------------
  ## 4. Calendar helpers
  ## --------------------------------------------------------------
  ## 4.1 Weekday (1‑7)
  wday <- as.POSIXlt(day_tbl$.date)$wday               # 0 = Sun … 6 = Sat
  if (start == "M") {                                 # Monday = 1, Sunday = 7
    wday <- ((wday + 6) %% 7) + 1
  } else {                                            # Sunday = 1 … Saturday = 7
    wday <- wday + 1
  }
  day_tbl$.dow <- wday

  ## 4.2 Absolute ISO week number (depends on start of week)
  week_fmt          <- if (start == "M") "%W" else "%U"
  day_tbl$.week_abs <- as.integer(format(day_tbl$.date, week_fmt))

  ## 4.3 Day‑of‑month label (text inside each tile)
  day_tbl$day_label <- format(day_tbl$.date, "%d")

  ## --------------------------------------------------------------
  ## 5. Month facet (chronological) + local week numbers
  ## --------------------------------------------------------------
  n_months <- length(unique(format(day_tbl$.date, "%Y-%m")))

  if (n_months > 1L) {
    ## ---- month facet -------------------------------------------------
    month_seq    <- seq(from = min(day_tbl$.date),
                        to   = max(day_tbl$.date),
                        by   = "month")
    month_levels <- format(month_seq, "%Y-%b")   # "2023-Jan", "2023-Feb", …

    day_tbl$month_facet <- factor(
      format(day_tbl$.date, "%Y-%b"),
      levels = month_levels
    )

    ## ---- local week (restart at 1 for each month) --------------------
    first_week_by_month <- tapply(day_tbl$.week_abs,
                                  day_tbl$month_facet,
                                  min)

    day_tbl$.week <- first_week_by_month[day_tbl$month_facet] +
      day_tbl$.week_abs -
      first_week_by_month[day_tbl$month_facet] + 1L
  } else {
    ## Single‑month view – just renumber weeks starting at 1
    first_week   <- min(day_tbl$.week_abs)
    day_tbl$.week <- day_tbl$.week_abs - first_week + 1L
  }

  ## --------------------------------------------------------------
  ## 6. Build the fill scale (gradient handling for palette)
  ## --------------------------------------------------------------
  high.col <- "#9b2948"                # default blue
  low.col  <- "#FFEF99"

  fill_scale <- ggplot2::scale_fill_gradientn(
    colours = c(low.col, high.col),
    name    = "Occupied\nhours",
    na.value = "white",
    limits  = c(0, max(day_tbl$duration))
  )

  # browser()

  ## --------------------------------------------------------------
  ## 7. Build the plot (no axis text, day numbers inside tiles)
  ## --------------------------------------------------------------
  p <- ggplot2::ggplot(
    day_tbl,
    ggplot2::aes(x = .dow, y = .week, fill = duration)
  ) +
    ggplot2::geom_tile(colour = "grey80", size = 0.2) +
    fill_scale +

    ## Remove all axis annotations
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid      = ggplot2::element_blank(),
      axis.title.x    = ggplot2::element_blank(),
      axis.title.y    = ggplot2::element_blank(),
      axis.text       = ggplot2::element_blank(),
      axis.ticks      = ggplot2::element_blank(),
      plot.title      = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = legend.pos,
      legend.title    = ggplot2::element_text(size = 10)
    ) +

    ## Day‑of‑month number inside each tile
    ggplot2::geom_text(
      ggplot2::aes(label = day_label),
      colour = "gray30",
      size   = day.size / 2,
      fontface = "bold"
    ) +

    ## Reverse y‑axis so week 1 appears at the top
    ggplot2::scale_y_reverse(
      breaks = function(x) sort(unique(day_tbl$.week)),
      expand = c(0, 0)
    ) +

    ## X‑axis (weekday) – hide its labels as well
    ggplot2::scale_x_continuous(
      breaks = 1:7,
      labels = if (start == "M")
        c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
      else
        c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),
      expand = c(0, 0)
    ) +

    ggplot2::labs(
      title    = if (missing(title)) {
        rng <- range(day_tbl$.date)
        paste0("Occupied hours – ",
               format(rng[1], "%b %Y"),
               " to ",
               format(rng[2], "%b %Y"))
      } else {
        title
      },
      subtitle = subtitle,
      caption  = source_caption
    )

  ## --------------------------------------------------------------
  ## 8. Facet by month when needed (free y‑scale → only weeks belonging
  ##    to the month are shown)
  ## --------------------------------------------------------------
  if ("month_facet" %in% names(day_tbl)) {
    p <- p + ggplot2::facet_wrap(~ month_facet, ncol = 3, scales = "free_y")
  }

  ## --------------------------------------------------------------
  ## 9. Return the plot (invisible, like the original)
  ## --------------------------------------------------------------
  invisible(p)
}
