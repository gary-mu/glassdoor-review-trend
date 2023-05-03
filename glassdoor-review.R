#Shiny apps for company reviews 
# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(rvest)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(lexicon)



# Load data
# companies <- c('Allbirds',
# 'Amazon',
# 'Apple',
# 'Chan Zuckerberg Initiative',
# 'Google',
# 'Meta',
# 'Netflix',
# 'Twitter')

file_paths <- c(
  'https://raw.githubusercontent.com/gary-mu/glassdoor-review-trend/main/data/allbirds_2023-04-28_review_data.csv',
  'https://raw.githubusercontent.com/gary-mu/glassdoor-review-trend/main/data/apple_2023-04-28_review_data.csv',
  'https://raw.githubusercontent.com/gary-mu/glassdoor-review-trend/main/data/bill_and_melinda_gates_foundation_2023-04-28_review_data.csv',
  'https://raw.githubusercontent.com/gary-mu/glassdoor-review-trend/main/data/chan_zuckerberg_initiative_2023-04-28_review_data.csv',
  'https://raw.githubusercontent.com/gary-mu/glassdoor-review-trend/main/data/google_2023-05-01_review_data.csv',
  'https://raw.githubusercontent.com/gary-mu/glassdoor-review-trend/main/data/google_2023-05-01_review_data.csv',
  'https://raw.githubusercontent.com/gary-mu/glassdoor-review-trend/main/data/netflix_2023-05-01_review_data.csv',
  'https://raw.githubusercontent.com/gary-mu/glassdoor-review-trend/main/data/reforge_2023-04-28_review_data.csv',
  'https://raw.githubusercontent.com/gary-mu/glassdoor-review-trend/main/data/twitter_2023-05-01_review_data.csv'
)


##Function to get data 
get_company_review_data <- function(file_paths){
  main_df <- tibble()
  company_names <- c()
  for(file in file_paths){
    print(paste0('reading: ', file))
    name <- str_extract(file, ".*data/(.*)_\\d{4}-\\d{2}-\\d{2}_review_data\\.csv$", group = 1)
    df <- read_csv(file) %>% mutate(company_name = name)
    main_df <- main_df %>% bind_rows(df)
    company_names <- c(company_names, name)
  }
  data <- list(main_df = main_df, company_names = company_names)
  return(data)
}

capitalize_first_letter <- function(name) {
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", name, perl = TRUE)
}

data <- get_company_review_data(file_paths)
companies <- data$company_names %>% str_replace_all('_', ' ') %>% capitalize_first_letter()

#Get filtered company data
filtered_company_data <- function(company, data){
  cleaned_company_name <- company %>% tolower() %>% str_replace_all(' ', '_')
  df <- data %>% pluck('main_df') %>% filter(company_name == cleaned_company_name)
  return(df)
}



#plot ratings 
plot_ratings <- function(dataframe){
  dataframe %>% 
    mutate(review_time = floor_date(review_date, unit = 'year')) %>% 
    group_by(review_time) %>% 
    summarise(
      num_ratings = n(), 
      mean_ratings = mean(ratings)
    ) %>% 
    pivot_longer(cols = num_ratings:mean_ratings, names_to = 'metrics') %>% 
    ggplot(aes(x = review_time, y = value, color = metrics)) +
    geom_line() + 
    facet_wrap(~metrics, scales = 'free') + 
    scale_x_date(name="", date_breaks="1 year", minor_breaks=NULL, date_labels="%Y") + 
    labs(x = 'Review time (year)', 
         y = 'Metric (ratings range 1-5)', 
         title = 'CZI Glassdoor review trends overtime') + 
    theme(text = element_text(size = 20),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
}

#create corpus, tokens and DFM 
#create unigram to tri-gram tokens to get more context
create_dfm <- function(dataframe){
  df <- dataframe %>% 
    select(review_text_pro, review_text_con) %>% 
    pivot_longer(cols = everything(), names_to = 'sentiment', values_to = 'review_text') %>% 
    mutate(sentiment = if_else(str_detect(sentiment, 'pro'), 'pros', 'cons'))
  
  review_corpus <- corpus(df, text_field= 'review_text') 
  review_tokens <- review_corpus %>% 
    tokens(
      remove_punct = T,
      remove_symbols = T,
      remove_numbers = T,
      remove_url = T,
      remove_separators = T) %>% 
    tokens_remove(pattern = stopwords("en")) %>%
    tokens_group(groups = sentiment) %>% 
    tokens_ngrams(1:3) %>% 
    tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)
  
  review_dfm_clean <- review_tokens %>% dfm()
  return(review_dfm_clean)
}

plot_wordcloud <- function(dfm){
  plot <- dfm %>% 
    textplot_wordcloud(comparison=T, 
                       min_count = 3, 
                       max_words = 100, 
                       color = c("red", "darkgreen"), 
                       labelsize = 1,
                       labelcolor = "gray10",
                       labeloffset = 0) 
  
  title("Top words used in Pros vs Cons review")
  return(plot)
}

plot_keyness <- function(dfm){
  tstat_key <- textstat_keyness(dfm, 
                                target = dfm$sentiment == 'pros')
  top_btm_20 <- head(tstat_key, n = 15) %>% bind_rows(tail(tstat_key, n = 15)) %>% mutate(group = if_else(chi2 > 0, 'pros', 'cons'))
  
  plot <- top_btm_20 %>%
    mutate(feature_sorted = fct_reorder(feature, chi2, .desc = F)) %>%
    ggplot(aes(x = chi2, y = feature_sorted, fill = group)) + 
    geom_col() + 
    labs(x = 'likelihood to be used in pros/cons review (chi-sq stats)', 
         y = 'Words used', 
         title = 'Most likely used words in Pros and Cons reviews') + 
    theme(text = element_text(size = 20), aspect.ratio = 1/2)
  
  return(plot)
}


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Company Review Info (from Glassdoor)"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "company", label = strong("Company Name"),
                                choices = companies,
                                selected = 'Chan Zuckerberg Initiative')
                    
                    # Select date range to be plotted
                    # dateRangeInput("date", strong("Date range"), start = "2007-01-01", end = "2017-07-31",
                    #                min = "2007-01-01", max = "2017-07-31"),
                    
                  ),
                  
                  # Output: Description, lineplot, and reference
                  
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Ratings Trend", plotOutput("rating_plot")),
                                tabPanel("Wordcloud", plotOutput("wordcloud_plot")),
                                tabPanel("Top words Pro vs Con reviews", plotOutput("keyness_plot", height = 800))
                    )
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  #Subset data
  selected_company_data <- reactive({
    req(input$company)
    
    filtered_company_data(input$company, data)
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$rating_plot <- renderPlot({
    plot_ratings(selected_company_data())
  })
  
  
  output$wordcloud_plot <- renderPlot({
    create_dfm(selected_company_data()) %>% plot_wordcloud()
  })

  output$keyness_plot <- renderPlot({
    create_dfm(selected_company_data()) %>% plot_keyness()
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)