library(rvest)
library(tidyverse)
library(lubridate)
clean_dat<-readRDS("./data/clean_dat.rds")
current_senators<-readRDS("./data/current_senators.rds")
senator_not_running_2024<-readRDS("./data/senator_not_running_2024.rds")
senator_running_2024<-readRDS("./data/senator_running_2024.rds")
filter_data<- function(data=clean_dat,from,state_input,cycle_input=2024,population_input,grade_input){
  data<-data|>
    mutate(numeric_grade=ifelse(is.na(numeric_grade),0,numeric_grade))|>
    mutate(general_score=numeric_grade*1e6+sample_size)|>
    mutate(quartile_category = ntile(general_score, 4),  
           quartile_category = case_when(
             quartile_category == 4 ~ "A",
             quartile_category == 3 ~ "B",
             quartile_category == 2 ~ "C",
             quartile_category == 1 ~ "D"),
           quartile_category = factor(quartile_category,
                                      levels = c("A", "B", "C", "D"),
                                      ordered = TRUE)
           )|>
    filter(start_date>from,
           state==state_input,
           cycle%in%cycle_input,
           population%in% population_input,
           quartile_category<=grade_input)|>
    select(-c(general_score))
  return(data)
}
filter_data(data=clean_dat,
            from = make_date(2024,1,1),
            state_input = "California",
            cycle_input = seq(2018,2024),
            population_input = c("a","lv","rv"),
            grade_input = "B")|>View()
sd_calculate<-function(data=clean_dat,prior_from,state_input,population_input,grade_input,polls_from,cycle_input=2024){
  overall_sd <- sd(data$spread, na.rm = TRUE)
  data<-filter_data(data=data,from=prior_from,state_input,cycle_input=seq(2016,2024),population_input,grade_input)
  sd_data<-data|>group_by(pollster)|>
    mutate(n=n())|>
    ungroup()|>
    mutate(pollster_type=case_when(n<3~"Other",
                              .default = pollster))|>
    group_by(pollster_type)|>
    mutate(sd=sd(spread))|>
    ungroup()|>
    mutate(sd=ifelse(pollster_type=="Other",overall_sd,sd))|>
    group_by(pollster)|>
    slice(1)|>
    ungroup()|>
    select(c(pollster,sd))
  
  cycle_interest<-filter_data(data=data,from=polls_from,state_input,cycle_input,population_input,grade_input)
  final_data<-cycle_interest|>group_by(pollster)|>
    mutate(n=n())|>
    ungroup()|>
    left_join(sd_data,by=c("pollster"))|>
    mutate(higher=spread+1.96*sd,
           lower=spread-1.96*sd)
  return(final_data)
}

# sd_calculate(data=clean_dat,
#             state_input = "California",
#             population_input = c("a","lv","rv"),
#             grade_input = "B",
#             prior_from = make_date(2016,1,1),
#             cycle_input = 2024,
#             polls_from = make_date(2016,1,1))|>View()

prior_calculate<-function(data=clean_dat,prior_from,state_input,population_input,grade_input,cycle_input=2024,factor=5){
  prior_data<-filter_data(data=data,from=prior_from,state_input,cycle_input=seq(2016,2022),population_input,grade_input)
  summary<-prior_data|>group_by(cycle,type)|>
    mutate(weight=sample_size/sum(sample_size))|>
    mutate(weighted_spread=weight*spread)|>
    summarise(prior=sum(weighted_spread),.groups = "drop")|>
    mutate(win_party = ifelse(prior > 0, substr(type, 1, 3), substr(type, nchar(type) - 2, nchar(type))))|>
    count(win_party)|>mutate(n=n*factor)|>
    complete(win_party = c("IND", "DEM", "REP"), fill = list(n = 0))|>
    deframe()
  cycle_interest<-data|>filter(state==state_input,cycle==cycle_input)|>
    pull(type)|>
    unique()
    parties <- strsplit(cycle_interest, " - ")[[1]]
  
  # Calculate the difference
  difference <- summary[parties[1]] - summary[parties[2]]
  return(difference)
}

# prior_calculate(data=clean_dat,
#                  state_input = "Nebraska",
#                  population_input = c("lv","rv"),
#                  grade_input = "B",
#                  prior_from = make_date(2016,1,1),cycle_input = 2024)

bayesian_estimate<-function(data=clean_dat,prior_from,state_input,population_input,grade_input,cycle_input,polls_from,tau_input=0.035,factor=5){
  data_with_sd<-sd_calculate(data=data,
                             state_input = state_input,
                             population_input = population_input,
                             grade_input = grade_input,
                             prior_from = prior_from,
                             cycle_input = cycle_input,
                             polls_from = polls_from)
  theta_all<-prior_calculate(data=data,
                                     state_input = state_input,
                                     population_input = population_input,
                                     grade_input = grade_input,
                                     prior_from = prior_from,
                                     cycle_input = cycle_input,
                             factor=factor)
  prediction_mean=sum(data_with_sd$spread*(data_with_sd$sample_size/sum(data_with_sd$sample_size)))
  prediction_sd=sqrt(sum((data_with_sd$sd)^2*(data_with_sd$sample_size/sum(data_with_sd$sample_size))^2))
  B=(prediction_sd/100)^2 / ((prediction_sd/100)^2 + tau_input^2)
  posterior_mean = B*theta_all + (1 - B)*prediction_mean
  posterior_sd = 100*sqrt(1/(1/(prediction_sd/100)^2 + 1/tau_input^2))
  lower_predict=posterior_mean-1.96*posterior_sd
  higher_predict=posterior_mean+1.96*posterior_sd
  prediction_row <- data.frame(
    poll_id = NA,                     
    state = state_input,              
    pollster = "Prediction",          
    numeric_grade = NA,               
    sample_size = sum(data_with_sd$sample_size),             
    answer = data_with_sd$answer[1],   
    population = NA,     
    cycle = cycle_input,               
    start_date = polls_from,           
    n = NA,                            
    spread = posterior_mean,                       
    type = NA,                         
    quartile_category = NA,            
    sd = posterior_sd,                           
    lower = lower_predict,             
    higher = higher_predict         
  )
  data_with_sd <- rbind(data_with_sd, prediction_row)
  
  return(data_with_sd)
}

# bayesian_estimate(data=clean_dat,
#              state_input = "Hawaii",
#              population_input = c("a","lv","rv"),
#              grade_input = "B",
#              prior_from = make_date(2016,1,1),
#              cycle_input = 2024,tau_input = 0.085,
#              polls_from = make_date(2018,1,1),factor=2)
 
 

 
plot_forest <- function(data) {
  # Extract the unique candidate pair and party information
  candidate_pair <- unique(data$answer)  # Should be something like "O'Rourke vs Cruz"
  party_pair <- unique(data$type)        # Should be something like "DEM – REP"
  state<-unique(data$state)
  # Construct the plot title
  plot_title <- paste(state, ":", candidate_pair, "(", party_pair, ")")
  
  # Sort data to place "Prediction" first
  data <- data %>%
    mutate(
      poll_id_label = ifelse(
        pollster == "Prediction",
        paste0("Prediction (n=", sample_size[which(pollster == "Prediction")], ", ", start_date[which(pollster == "Prediction")], ")"),
        paste0(pollster, " (n=", sample_size, ", ", start_date, ", ", population, ")")
      ),
      color_type= ifelse(pollster == "Prediction","Prediction","Poll")
    ) %>%
    arrange(desc(pollster == "Prediction")) # Place Prediction first
  
  # Create the forest plot
  plot <- ggplot(data, aes(x = reorder(poll_id_label, pollster == "Prediction"), y = (lower + higher) / 2)) +
    geom_pointrange(aes(ymin = lower, ymax = higher, color = color_type), size = 0.8) +
    coord_flip() +  # Flip coordinates to make it horizontal
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add a vertical dashed line at x = 0
    theme_minimal() +
    labs(
      title = plot_title, # Use the dynamically generated title
      x = "Pollster (Sample Size, Start Date)",
      y = "95% Confidence Interval"
    ) +
    theme(
      axis.text.y = element_text(size = 10), # Improve readability of y-axis labels
      axis.text.x = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "none"
    )
  return(plot)
}

 
# 
# plot_forest(bayesian_estimate(data=clean_dat,
#                               state_input = "Texas",
#                               population_input = c("a","lv","rv"),
#                               grade_input = "B",
#                               prior_from = make_date(2016,1,1),
#                               cycle_input = 2024,tau_input = 0.035,
#                               polls_from = make_date(2024,10,1)))

chair_prediction<-function(prior_from,population_input,grade_input,polls_from,tau_input=0.035,factor=5){
  current_party_count<-senator_not_running_2024|>
    group_by(Party)|>
    summarise(n=n(),.groups="drop")
  current_demo<-current_party_count$n[current_party_count$Party=="DEM"]
  states<-c()
  prediction_means<-c()
  prediction_sds<-c()
  types<-c()
  for (state in senator_running_2024$State) {
    tryCatch({
      result <- bayesian_estimate(
        data = clean_dat,
        state_input = state,
        population_input = population_input,
        grade_input = grade_input,
        prior_from = prior_from,
        cycle_input = 2024,
        tau_input = tau_input,
        polls_from = polls_from,
        factor = factor
      )
      states <- c(states, state)
      prediction_means <- c(prediction_means, result$spread[length(result$spread)])
      prediction_sds <- c(prediction_sds, result$sd[length(result$sd)])
      types <- c(types, result$type[1])
    }, error = function(e) {
      state_without_poll <<- c(state_without_poll, state)
      message(sprintf("Error processing state '%s': %s", state, e$message))
    })
  }
  
  state_without_poll<-setdiff(senator_running_2024$State, states)
  party_count_without_poll<-senator_not_running_2024|>filter(State%in%state_without_poll)|>group_by(Party)|>
    summarise(n=n(),.groups="drop")
  demo_without_poll<-party_count_without_poll$n[party_count_without_poll$Party=="DEM"]
  demo_count_before_simulation<-current_demo+demo_without_poll
  simulation<-function(means,sds,types){
    random_values<-mapply(rnorm, n = 1, mean = means, sd = sds)
    results <- sapply(seq_along(random_values), function(i) {
      if (random_values[i] > 0) {
        substr(types[i], 1, 3) # First three characters
      } else {
        substr(types[i], nchar(types[i]) - 2, nchar(types[i])) # Last three characters
      }})
    demo_elect<-sum(results=="DEM")
  }
  simu_results<-replicate(10000,simulation(prediction_means,prediction_sds,types))
  final_result<-simu_results+demo_count_before_simulation
  return(final_result)
}


# a<-chair_prediction(population_input = c("a","lv","rv"),
#                   grade_input = "D",
#                   prior_from = make_date(2016,1,1),tau_input = 0.085,
#                   polls_from = make_date(2018,1,1),factor=2)
chair_plot<-function(simu_result){
  ggplot(data.frame(simu_result), aes(x = factor(simu_result))) +
    geom_bar(aes(y = (..count..) / sum(..count..)), bins = 10, fill = "blue", alpha = 0.7) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Histogram of Simulated Democrat Chair Outcomes",
      x = "Chair Outcomes",
      y = "Percentage"
    ) +
    theme_minimal()
}

