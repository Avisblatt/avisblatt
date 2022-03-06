#' @import quanteda.textplots 
#' @export
show_wordcloud <- function(ids = NULL, coll = c_all, remove = "", max_words = 200){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to search in texts")
  } else{
    corp <- corpus_subset(c_all$corpus, names(c_all$corpus) %in% ids)
    removal <- c(avis_stop(), remove)
    corp <- corp %>%
      tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
      tokens_remove(removal, min_nchar = 3)
    textplot_wordcloud(dfm(corp), max_words = max_words)
  }
}


#' @importFrom zoo "na.approx"
#' @export
calculate_frequency_data <- function(results, coll = c_all, universe = "all", aggregation_level = "year"){
  # This will take a list of search results (i.e, a list of lists of ids)
  # as, for example, produced by the group_by-functions,
  # and generate a data frame with different frequency measures
  # to be used as input for plotting:
  #      N             = number of records in each search result in time intervals (default: year-by-year)
  #      N_index       = N as index numbers (100 = average N over all time frames)
  #      N_per1k       = N per 1000 inhabitants
  #      N_per1k_index = N1k as index numbers (100 = average N1k over all time frames)
  #      share         = share of N among all records in the universe, in time interval
  #      share_index   = share as index numbers (100 = average N1k over all time frames)
  #      length        = average number of tokens per record
  #      length_index  = average number of tokens as index numbers (100 = average length over all time frames)
  # Getting statistical population/universe ("Grundgesamtheit")
  d_all <- count_records_by_date(universe, coll, level = aggregation_level)

  # change time frame to actual date
  if (aggregation_level == "year"){
    d_all$year <- as.Date(paste(d_all$year, 1, 1, sep = "-"))
  } else if (aggregation_level == "quarter"){
    d_all$quarter <- (d_all$quarter-1)*3+1
    d_all$year <- as.Date(paste(d_all$year, d_all$quarter, 1, sep = "-"))
    d_all$quarter <- NULL
  } else if (aggregation_level == "month"){
    d_all$year <- as.Date(paste(d_all$year, d_all$month, 1, sep = "-"))
    d_all$month <- NULL
  } else {
    # To make things (much) easier,
    # start first week of year always on Jan 1
    d_all$year <- as.Date(paste(d_all$year, 1, 1, sep = "-")) + 7*(d_all$week-1)
    d_all$week <- NULL
  }
  d_time_frames <- d_all[,1]

  # Get population for all the years in current collection
  d_pop <- fread("../avis-analysis/data/population.csv", encoding="UTF-8")
  # Change year to date
  d_pop$year <- as.Date(paste(d_pop$year, 1, 1, sep = "-"))
  # add all dates from d_all to d_pop
  d_pop <- merge(d_time_frames, d_pop, all = TRUE)
  # interpolate missing data (with constant growth rate):
  # logarithmise the data, make linear interpolation, reverse logarithm
  d_pop[,2] <- log(d_pop[,2])
  d_pop[,2] <- na.approx(d_pop[,2])
  d_pop[,2] <- exp(d_pop[,2])
  # throw out all dates not in the universe
  d_pop <- merge(d_time_frames, d_pop, by = "year", all = FALSE)

  colnames(d_all)[1] <- "date"
  data <- data.frame(group=character(), date=integer(),
                     length=numeric(), N=integer(), length_index=numeric(), N_index=numeric(), N_per1k=numeric(), N_per1k_index=numeric(), share=numeric(), share_index=numeric())
  class(data$date) <- "Date"
  for (i in 1:length(results)){
    if (length(results[[i]]) > 0){
      d_i <- average_length_by_date(
        ids = results[[i]], coll, level = aggregation_level)      # calculate mean_length
      d_i <- merge(d_i, count_records_by_date(
        ids = results[[i]], coll, level = aggregation_level))     # calculate N
      # change time frame to actual date
      if (aggregation_level == "year"){
        d_i$year <- as.Date(paste(d_i$year, 1, 1, sep = "-"))
      } else if (aggregation_level == "quarter"){
        d_i$quarter <- (d_i$quarter-1)*3+1
        d_i$year <- as.Date(paste(d_i$year, d_i$quarter, 1, sep = "-"))
        d_i$quarter <- NULL
      } else if (aggregation_level == "month"){
        d_i$year <- as.Date(paste(d_i$year, d_i$month, 1, sep = "-"))
        d_i$month <- NULL
      } else {
        d_i$year <- as.Date(paste(d_i$year, 1, 1, sep = "-")) + 7*(d_i$week-1)
        d_i$week <- NULL
      }
      # add time_frames not having records to the data frame so that rbinding works
      d_i <- merge(d_time_frames, d_i, all = TRUE)
      d_i[is.na(d_i)] = 0
      # Calculate the other measures
      d_i <- cbind(d_i, 100  * d_i[,2] / mean(unlist(d_i[,2])))   # calculate length_index
      d_i <- cbind(d_i, 100  * d_i[,3] / mean(unlist(d_i[,3])))   # calculate N_index
      d_i <- cbind(d_i, 1000 * d_i[,3] / d_pop[,2])               # calculate N1k
      d_i <- cbind(d_i, 100  * d_i[,6] / mean(unlist(d_i[,6])))   # calculate N1k_index
      d_i <- cbind(d_i, d_i[,3] / d_all[,2])                      # calculate share
      d_i <- cbind(d_i, 100  * d_i[,8] / mean(unlist(d_i[,8])))   # calculate share_index
      # add group name and attach to data frame
      d_i <- cbind(names(results[i]), d_i)
      colnames(d_i) <- c("group", "date", "length", "N", "length_index", "N_index", "N_per1k", "N_per1k_index", "share", "share_index")
      data <- rbind (data, d_i)
    }
  }
  data
}

#' @import ggplot2
#' @import viridis
#' @import hrbrthemes
#' @export
plot_frequency_data <- function(data,
                                data_type = c("N", "N_index", "Nper_1k", "N_per1k_index", "share", "share_index", "length", "length_index"),
                                plot_type = c("line", "stack", "heatmap"),
                                diagram_title = "Use the diagram_title argument of the plot function to change this text",
                                legend_title = "Type of record",
                                colour = "D"){
  switch(data_type,
         length        = colnames(data)[3] <- "Y",
         N             = colnames(data)[4] <- "Y",
         length_index  = colnames(data)[5] <- "Y",
         N_index       = colnames(data)[6] <- "Y",
         N_per1k       = colnames(data)[7] <- "Y",
         N_per1k_index = colnames(data)[8] <- "Y",
         share         = colnames(data)[9] <- "Y",
         share_index   = colnames(data)[10] <- "Y")

  if (nrow(data)==0){stop("No data, nothing to print")}

  # fix the order of the groups, otherwise will be sorted alphabetically
  data$group <- factor(data$group, levels = unique(data$group))

  # determine x axis labels
  date_diff <- max(data$date)-min(data$date)
  interval <- case_when(
    date_diff > 365*100 ~ "10 years",
    date_diff > 365*50 ~ "5 years",
    date_diff > 365*20 ~ "2 years",
    date_diff > 365*10 ~ "1 year",
    date_diff > 365*5 ~ "6 months",
    date_diff > 365*1 ~ "3 months",
    TRUE ~ "1 month")
  if (date_diff > 365*10){
    x_labelform <- "%Y"
  } else if (date_diff > 365*1){
    x_labelform <- "%Y\n%b" # later: find a way to use quarters instead of month
  } else {
    x_labelform <- "%Y\n%b"
  }
  label_min <- case_when(
    date_diff > 365*100 ~ as.Date(paste(10*round(year(min(data$date))/10), 1, 1, sep = "-")),
    date_diff > 365*50 ~ as.Date(paste(5*round(year(min(data$date))/5), 1, 1, sep = "-")),
    TRUE ~ as.Date(paste(year(min(data$date)), 1, 1, sep = "-")))
  label_max <- case_when(
    date_diff > 365*100 ~ as.Date(paste(10*round(year(max(data$date))/10), 1, 1, sep = "-")),
    date_diff > 365*50 ~ as.Date(paste(5*round(year(max(data$date))/5), 1, 1, sep = "-")),
    TRUE ~ as.Date(paste(year(max(data$date)), 1, 1, sep = "-")))
  x_labelseq <- seq.Date(from = label_min, to = label_max, by = interval)
  my_ggprotos <- list(theme_ipsum(base_size = 14, caption_size = 14,  axis_title_size = 14),
                      scale_x_date(breaks = x_labelseq,
                                   date_labels = x_labelform),
                      labs(x = NULL, y = NULL),
                      ggtitle(diagram_title))
  P_L = ggplot(data, aes(x=date, y=Y)) +
    geom_line(aes(color = group)) + geom_point(aes(color = group) +
    scale_colour_viridis_d(legend_title, option=colour, end=0.8))
  P_S = ggplot(data, aes(x=date, y=Y)) +
    geom_col(aes(fill = group), width = resolution(as.double(data$date), FALSE)) +
    scale_fill_viridis_d(legend_title, option=colour)
  P_H = ggplot(data, aes(x=date, y=group, fill=Y, height=0.7)) +
    geom_tile() +
    scale_fill_viridis_c(legend_title, option=colour)
  switch(plot_type,
         line    = P_L + my_ggprotos,
         stack   = P_S + my_ggprotos,
         heatmap = P_H + my_ggprotos)
}
