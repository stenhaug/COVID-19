# filter to states and add total population
states_to_df <- function(the_states){
	data %>%
		filter(state %in% the_states) %>%
		select(date, state, new_positive_per, new_test_per) %>%
		mutate(state = as.character(state)) %>%
		bind_rows(
			data %>%
				group_by(date) %>%
				summarize(
					state = "All",
					new_positive_per = sum(new_positive) / 316000000 * 100000,
					new_test_per = sum(new_test) / 316000000 * 100000
				)
		) %>%
		mutate(
			state =
				state %>%
				fct_reorder2(date, new_positive_per) %>%
				fct_relevel("All")
		)
}

# assign colors based on level at last date
states_df_to_colors <- function(the_states){

	#fd9e88

	colors <- c("black", "#E12A3C", "#FFBF03", "#5380E4", "#00b723", "darkgray")

	states_df <- states_to_df(the_states)

	states_df %>%
		group_by(state) %>%
		filter(date == max(date)) %>%
		ungroup() %>%
		arrange(state != "All", new_positive_per) %>%
		mutate(
			state = as_factor(state),
			color = colors[1:nrow(.)]
		)
}

# tie it all together to be able to make a graph
graph_covid <- function(the_states, the_title){
	the_states %>%
		states_to_df() %>%
		gather(measure, val, -date, -state) %>%
		mutate(
			measure =
				case_when(
					measure == "new_positive_per" ~ "New positive cases",
					measure == "new_test_per" ~ "New tests"
				)
		) %>%
		filter(date >= mdy("3/13/2020")) %>%
		ggplot(
			aes(
				x = date,
				y = val,
				color = state
			)
		) +
		geom_point(size = 2.5, alpha = 0.8) +
		geom_path(size = 0.35, linetype = "longdash", alpha = 0.8) +
		facet_wrap(~ measure, ncol = 1, scales = "free_y")  +
		labs(
			x = "",
			y = "Per 100,000 residents",
			color = "State",
			title = the_title
		) +
		scale_color_manual(values = states_df_to_colors(the_states)$color) +
		theme(plot.title = element_text(color = "#dd4b39"))
}


