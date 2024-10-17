
##### Forecasting tool demo using local Llama 3.1 with elmer and ollama

# Two goals:
# 1. use local LLM
# 2. use tool calling


library(tidyverse)
library(dotenv)
library(elmer)


### Notes:
# Overly-ambitious in original scope - Wanted to injest actual data and output forecast models and deep analysis tools in Shiny
# Scaled way back to only describe outputted sample forecast data
# Could scale up to achieve original scope with enough time


# three tools:
# 1. give forecast monthly for date range
# 2. plot forecasted wins by team
# 3. give pipeline win probability details



##### Set up functions for tool calling

get_monthly_forecast <- function(start_date=floor_date(today(),"month"), end_date=floor_date(today(),"month")+months(12)) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  month_dates <- seq.Date(from=floor_date(start_date,"month"), to=floor_date(end_date,"month"), by="month")
  sample_forecast <- rnorm(length(month_dates), mean=1e6, sd=3e5)
  return(tibble(month = month_dates, forecast = sample_forecast))
}



plot_monthly_forecast <- function(start_date=floor_date(today(),"month"), 
                                  end_date=floor_date(today(),"month")+months(12),
                                  teams=c("Enterprise","Commercial","Public Sector")) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  month_dates <- seq.Date(from=floor_date(start_date,"month"), to=floor_date(end_date,"month"), by="month")
  sample_forecast <- rnorm(length(month_dates), mean=1e6, sd=3e5)
  comm_fcst <- sample_forecast * 0.25
  ent_fcst <- sample_forecast * 0.55
  ps_fcst <- sample_forecast * 0.20
  fcst <- bind_rows(
    tibble(month=month_dates, team="Total", forecast=sample_forecast),
    tibble(month=month_dates, team="Commercial", forecast=comm_fcst),
    tibble(month=month_dates, team="Enterprise", forecast=ent_fcst),
    tibble(month=month_dates, team="Public Sector", forecast=ps_fcst),
  ) %>%
    filter(team %in% c(teams, "Total"))
  ggplot(fcst, aes(month, forecast, col=team, group=team)) +
    geom_line() +
    theme_minimal() +
    labs(x="Month", y="Foreacst", title="Monthly forecast by team", col="Team") +
    scale_color_manual(values=c("indianred","dodgerblue","springgreen4","goldenrod")) +
    scale_y_continuous(labels=scales::dollar)
}


get_pipeline_win_prob <- function(start_date=floor_date(today(),"month"), 
                                  end_date=floor_date(today(),"month")+months(12),
                                  teams=c("Enterprise","Commercial","Public Sector")) {
  
  generate_sample_pipeline <- function(start_date_to_use, end_date_to_use) {
    tibble(
      opp_id = 1:25,
      team = sample(c("Enterprise","Commercial","Public Sector"), size=25, replace=TRUE),
      close_date = sample(seq.Date(from=start_date_to_use, to=end_date_to_use, by="day"), size=25, replace=TRUE),
      value = runif(25, min=1e4, max=2.5e5),
      win_prob = runif(25, min=0.01, max=0.99)
    ) %>%
      mutate(weighted_value = win_prob * value)
  }

  return(generate_sample_pipeline(start_date, end_date) %>% filter(team %in% teams))
  
}


##### initialize chat in Llama 3.1

chat <- chat_openai(
  model = "gpt-4o",
  system_prompt = paste0(collapse = "\n", readLines("initial_call.md")),
  echo = TRUE
)


##### Use helper functions to set up register_tool details

# create_tool_def(get_monthly_forecast)
# create_tool_def(plot_monthly_forecast)
# create_tool_def(get_pipeline_win_prob)



##### Register each tool

chat$register_tool(ToolDef(
  fun = get_monthly_forecast,
  name = "get_monthly_forecast",
  description = "Provides a monthly forecast from the start date to the end 
date.",
  arguments = list(
    start_date = ToolArg(
      type = "string",
      description = "The start date for the forecast. Defaults to the start 
of the current month.",
      required = FALSE
    ),
    end_date = ToolArg(
      type = "string",
      description = "The end date for the forecast. Defaults to 12 months 
from the start of the current month.",
      required = FALSE
    )
  )
))


chat$register_tool(ToolDef(
  fun = plot_monthly_forecast,
  name = "plot_monthly_forecast",
  description = "Plots a monthly forecast for the specified time range and teams.",
  arguments = list(
    start_date = ToolArg(
      type = "string",
      description = "The start date for the forecast. Defaults to the beginning of the current month."
    ),
    end_date = ToolArg(
      type = "string",
      description = "The end date for the forecast. Defaults to the beginning of the month 12 months from now."
    ),
    teams = ToolArg(
      type = "array",
      description = "A vector of team names for which the forecast is to be plotted. Defaults to c('Enterprise', 'Commercial', 'Public Sector')."
    )
  )
))



chat$register_tool(ToolDef(
  fun = get_pipeline_win_prob,
  name = "get_pipeline_win_prob",
  description = "Calculates the pipeline win probability for specified teams 
within the given date range.",
  arguments = list(
    start_date = ToolArg(
      type = "string",
      description = "The start date for the forecast. Defaults to the beginning of the current month."
    ),
    end_date = ToolArg(
      type = "string",
      description = "The end date for the forecast. Defaults to the beginning of the month 12 months from now."
    ),
    teams = ToolArg(
      type = "array",
      description = "A vector of team names for which the forecast is to be plotted. Defaults to c('Enterprise', 'Commercial', 'Public Sector')."
    )
  )
))




##### Execute prompts that should trigger tool calling

chat$chat("Hello.", echo=TRUE)
chat$chat("Show me the monthly forecast for the next six months")

# previous response (note the Jan 2023 start date):
# 😊 To get that info, I'll call the `get_monthly_forecast` function with these arguments:
# {"name": "get_monthly_forecast", "parameters": {"start_date": "2023-01-01", "end_date": "2024-07-01"}}

chat$chat("Plot the monthly forecast for the next 24 months for the Commercial and Public Sector teams", echo=TRUE)

# {"name": "plot_monthly_forecast", "parameters": {"start_date": "2023-01-01", "teams": "[\"Commercial\", \"Public 
# Sector\"]", "end_date": "2025-12-31"}}

chat$chat("Show me the pipeline win probabilities for all teams on deals expected to close in the next three months", echo=TRUE)

# {"name": "get_pipeline_win_prob", "parameters": {"start_date": "2023-03-01", "end_date": "2023-06-01", "teams": 
# "[\"Enterprise\", \"Commercial\", \"Public Sector\"]"}}





##### Try in OpenAI GPT-4o


chat <- chat_openai(
  model = "gpt-4o",
  system_prompt = paste0(collapse = "\n", readLines("initial_call.md")),
  echo = TRUE
)

chat$chat("Hello.", echo=TRUE)
chat$chat("Show me the monthly forecast for the next six months")