### These functions allow one to input an excel file and output a nice bulletchart

library(ggplot2)
library(dplyr)
library(lubridate)
library(xlsx)
library(testthat)

extra_field_calculator <- function(fileName, for_year=year(Sys.Date()), FY=TRUE, project_start_date=NULL) {

  test_that("Does the specified file exist in the directory?", {

    expect_equal(TRUE, file.exists(fileName)
    )

  })

  ## Read in Excel file:
  df <- read.xlsx(fileName, sheetName = "Sheet1", stringsAsFactors = FALSE)


  # If df is empty, break function and output empty chart
  if(nrow(df) == 0){
    output_plot <- "No data available"
    return()
  }


  # Create percentage variables
  df <- df %>% mutate(Perc = Actual/(Target + 0.0000000000001) * 100,
                      PercWeek = Actual_lastWeek/(Target + 0.0000000000001) * 100,
                      PercYear = Actual_lastYear/(Target + 0.0000000000001) * 100)

  # Protect against values greater than 100
  df <- df %>% mutate(
    Perc = case_when(
      Perc > 100 ~ 100,
      TRUE ~ Perc
    ),
    PercWeek = case_when(
      PercWeek > 100 ~ 100,
      TRUE ~ PercWeek
    ),
    PercYear = case_when(
      PercYear > 100 ~ 100,
      TRUE ~ PercYear
    )
  )

  # Calculate "Today" within specified Fiscal Year
  if(FY == TRUE){
    ## ||TODO If FY is true, then we know the project starts in some october, so we don't actually give a crap about the
    ## Startdate... this section of the if should be simplified so that it doesn't require `project_start_date` at all
    ## because as you can see from the inputs with defaults, right now the user only needs to provide a filename. Cool, no?
    ## I think we should be able to make this work just with the `for_year` indicator. What do you think?

    if(month(project_start_date) != 10) {project_start_date <- as.Date(paste("01 10", year(project_start_date)), format = "%d %m %Y")}

    df <- df %>% mutate(PercentTime = as.numeric(Sys.Date() - as.Date(paste(format(as.Date(project_start_date), "%d %b"),
                                                                            for_year - 1), format = "%d %b %Y")) / 365.25 * 100)

  } else {

    df <- df %>% mutate(PercentTime = as.numeric(Sys.Date() - as.Date(project_start_date)) / 365.25 * 100)

  }

  # Percent
  df <- df %>% mutate(PercentTime = case_when(
    PercentTime > 100 ~ 100,
    TRUE ~ PercentTime
  ))

  # Value for Indicator lateness or on time
  df <- df %>% mutate(text = PercentTime/100 * Target - Actual)

  # Calculate how far behind TODAY the percent for the indicator is
  df <- df %>% mutate(BehindBy = Perc - PercentTime)

  df <- df %>% mutate(text = case_when(

    BehindBy > 0 ~ "OK!",
    BehindBy <= 0 & !is.na(BehindBy) ~ paste("Need ", round(as.numeric(text)), " more", sep = "")

  ))

  # Behind By to lower limit = 0
  df <- df %>% mutate(Low_Level = -0.2 * df$PercentTime)

  df <- df %>% mutate(BehindBy = case_when(
    BehindBy > 0 ~ 0,
    BehindBy < Low_Level ~ Low_Level
  ))

  ## output
  return(df)

}

# BULLET CHART FUNCTIONs --------------------------------------------------

# 2. text above or below indicator bar?

# bullet plot Version 1 ----------------------------------------------------------

#' @title bullet_chart
#' @description creates bullet chart with symbols
#' @param df data frame of indicator data passed through extra_field_calculator function
#' @return bullet chart with symbols for "last week" and "last year"
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[scales]{pretty_breaks}}
#' @rdname bullet_chart
#' @export
#' @importFrom scales pretty_breaks

bullet_chart <- function(df) {

  Low_Level <- df$Low_Level[1]

  g <- ggplot(df, aes(x = IndicatorName)) +
    geom_col(aes(y = Perc, width = 0.1, fill = BehindBy), color = "black") +
    scale_fill_gradient("Indicator\nBehind By:", limits = c(Low_Level, 0), low = "red", high = "green",
                        guide = FALSE) +
    geom_point(aes(y = PercWeek, shape = "Last Week"), size = 6, stroke = 1) +
    geom_point(aes(y = PercYear, shape = "Last Year"), size = 6, stroke = 1) +
    scale_shape_manual(" ", values = c(23, 21)) +
    geom_col(aes(y = 100, width = 0.5), alpha = 0.25) +
    geom_text(y = 1, aes(label = text), vjust = -1.5, hjust = 0) +
    geom_hline(yintercept = df$PercentTime, alpha = 0.33) +
    annotate("text", x = 0, y = df$PercentTime + 1.5, hjust = 0, label = "Today",
             angle = 90, alpha = 0.5, size = 5) +
    coord_flip() +
    labs(y = "Percent of Yearly Target\n&\n Percent of Year",
         x = " ") +
    ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(face = "bold", size = 10),
          axis.text.x = element_text(face = "bold", size = 14),
          title = element_text(face = "bold", size = 14),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 8),
          legend.position = c(0.8, -0.12),
          legend.key.size = unit(1.5, "lines")) +
    expand_limits(x = 6.75, y = 102)

  print(g)

}

# bullet plot: multiple bars -----------------------------------------------

bullet_chart2 <- function(df) {

  Low_Level <- -0.2 * df$PercentTime %>% unique()

  g <- ggplot(df, aes(x = IndicatorName)) +
    geom_col(aes(y = PercWeek, width = 0.5), alpha = 0.6) +
    geom_col(aes(y = PercYear, width = 0.75), alpha = 0.3) +
    geom_col(aes(y = Perc, width = 0.15, fill = BehindBy), color = "black") +
    scale_fill_gradient("Indicator\nBehind By:", limits = c(Low_Level, 0), low = "red", high = "green") +
    geom_text(y = 1, aes(label = text), vjust = -2, hjust = 0, size = 4) +
    geom_hline(yintercept = df$PercentTime, alpha = 0.33) +
    annotate("text", x = 0, y = df$PercentTime + 1.5, hjust = 0, label = "Today", angle = 90, alpha = 0.5, size = 5) +
    coord_flip() +
    labs(y = "Percent of Yearly Target\n&\n Percent of Year",
         x = " ") +
    ggtitle(paste("Ongoing Indicator Accomplishment (", for_year, ")", sep = "")) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(face = "bold", size = 10),
          axis.text.x = element_text(face = "bold", size = 12),
          title = element_text(face = "bold"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 8),
          legend.position = "none")

  print(g)


}



################################################# TESTING



## Inputs
fileName="Indicators_Targets.com"   # NOT excel file
fileName="data/Indicators_Targets.xlsx"  # excel file
for_year = 2018 ## Specify Year the analysis represents
FY = TRUE       ## Is this a fiscal year? (as opposed to calendar year)
project_start_date <- "2016/03/01"   # as string! %Y/%D/%M

##
dataframe <- extra_field_calculator(fileName, for_year = 2018, FY = TRUE, project_start_date = project_start_date)
df <- extra_field_calculator(fileName, for_year = 2018, FY = TRUE, project_start_date = project_start_date)

bullet_chart(df = df)
bullet_chart(df = dataframe)

bullet_chart2(df = df)
bullet_chart2(df = dataframe)
