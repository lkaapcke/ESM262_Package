#' Fisheries Summary
#'
#' This function summarizes the most caught fish and the revenue for each fishery location as well as the total revenue and a plot of revenue by location.
#' @param prices A table with types of fish in rows and prices for each fish the column.
#' @param catch_table A table with the number of fish caught in each fishery location.
#' @param plot An indicator whether the user would like to see a graph of revenue by location as an output. Default is FALSE.
#' @return A list of the top fish caught in each location, the revenue for each location, the total revenue of all locations and a graph of revenue by location.
#' @author Lauren Kaapcke

fisheries_summary = function (prices, catch_table, plot = F) {
  # Turn into df and add fish names column
  catch_table = as.data.frame(catch_table)
  nloc = ncol(catch_table)
  catch_table$Fish = rownames(catch_table) # Add a column with the fish names
  catch_table

  # Pull out the top fish for each location
  top_fish = catch_table$Fish[apply(catch_table[,1:nloc],2, which.max)]
  top_fish = as.data.frame(top_fish)
  top_fish$Location = colnames(catch_table[,1:nloc])
  #top_fish

  # Output total revenue for each location
  # Re-order data to add prices
  catch_table_new = gather(catch_table, key="Location", value="Catch", -Fish)

  # Join with price table
  catch_table_new = left_join(catch_table_new, fish_prices, by = 'Fish')
  #catch_table_new

  # Add a revenue column
  catch_table_new$Prices = as.numeric(catch_table_new$Prices)
  catch_table_new = catch_table_new %>%
    mutate(Revenue = Catch*Prices)
  #catch_table_new

  # Summarize the revenue column
  rev_location <- catch_table_new %>%
    group_by(Location) %>%
    summarize(sum(Revenue))
  colnames(rev_location) = c("Location", "Revenue")
  #rev_location

  # Summarize the revenue column for the entire fishery
  total_rev <- catch_table_new %>%
    summarize(sum(Revenue))
  colnames(total_rev) = "Total Revenue"

  if(plot) {
    # Graph of revenue by location
    total = sprintf("Total fishery revenue is $%0.f.", total_rev)
    rev_graph <- ggplot(rev_location, aes(x = Location, y = Revenue, fill = Location)) +
      geom_col() +
      labs(y = "Revenue ($)", x = "Location") +
      annotate("text", x = 3, y = 5000, label = total)
  }
  else rev_graph = NULL

  return(list(top_fish_location = top_fish, rev_location = rev_location, total_rev = total_rev, plot = rev_graph))
}
