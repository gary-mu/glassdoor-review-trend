library(tidyverse)
library(rvest)

# companies <- tibble(
#   company_name = c(
#     'Allbirds',
#     'Chan Zuckerberg Initiative',
#     'Amazon',
#     'Apple',
#     'Google',
#     'Meta',
#     'Netflix',
#     'Twitter'
#   ),
#   base_url = c(
#     'https://www.glassdoor.com/Reviews/Allbirds-Reviews-E1989436_P',
#     'https://www.glassdoor.com/Reviews/Chan-Zuckerberg-Initiative-Reviews-E1735421_P',
#     'https://www.glassdoor.com/Reviews/Amazon-Reviews-E6036_P',
#     'https://www.glassdoor.com/Reviews/Apple-Reviews-E1138_P',
#     'https://www.glassdoor.com/Reviews/Google-Reviews-E9079_P',
#     'https://www.glassdoor.com/Reviews/Meta-Reviews-E40772_P',
#     'https://www.glassdoor.com/Reviews/Netflix-Reviews-E11891_P',
#     'https://www.glassdoor.com/Reviews/Twitter-Reviews-E100569_P'
#   )
# )


base_urls <-  c(
  # 'https://www.glassdoor.com/Reviews/Allbirds-Reviews-E1989436.htm',
  # 'https://www.glassdoor.com/Reviews/Chan-Zuckerberg-Initiative-Reviews-E1735421.htm',
  # 'https://www.glassdoor.com/Reviews/Reforge-Reviews-E3058795.htm',
  # 'https://www.glassdoor.com/Reviews/Bill-and-Melinda-Gates-Foundation-Reviews-E9097.htm',
  'https://www.glassdoor.com/Reviews/Amazon-Reviews-E6036.htm'
  #'https://www.glassdoor.com/Reviews/Apple-Reviews-E1138.htm',
  # 'https://www.glassdoor.com/Reviews/Google-Reviews-E9079.htm',
  # 'https://www.glassdoor.com/Reviews/Meta-Reviews-E40772.htm',
  # 'https://www.glassdoor.com/Reviews/Netflix-Reviews-E11891.htm',
  # 'https://www.glassdoor.com/Reviews/Twitter-Reviews-E100569.htm',
  #'https://www.glassdoor.com/Reviews/BetterHelp-Reviews-E1304061.htm'
)

#for(company in companies$company_name){
for(url in base_urls){
  print(paste0('Getting data from: ', url))
  company_name <- str_extract(url, 'Reviews/([A-Za-z-]*)-Reviews', group = 1) 
  company_url <- str_replace_all(url,'.htm', '_P')
  # name <- company
  # company_url <- companies %>% filter(company_name == name) %>% pull(base_url)
  
  num_pages <- read_html(paste0(company_url, 1, '.htm')) %>% 
    html_elements('.v2__EIReviewsRatingsStylesV2__count') %>% 
    html_text2() %>% 
    str_extract('Found (\\d*,?\\d*)', group = 1) %>% 
    str_remove_all(',') %>% 
    as.numeric()%/%10
  
  num_pages <- num_pages +1
  sample_rate <- if_else(num_pages < 100, 1, round(num_pages^(1/3)))
  
  reviews_data <- list()
  
  for(i in seq(from = 1, to = num_pages, by = sample_rate)){
    page_url <- paste0(company_url, i,'.htm?filter.iso3Language=eng')
    reviews <- read_html(page_url) %>% html_elements('div .gdReview')
    reviews_data <- append(reviews_data, reviews)
    Sys.sleep(sample(seq(1,15, by = 2), 1))
  }
  
  review_title <- map_chr(reviews_data, ~.x %>% 
                            pluck() %>% 
                            html_elements('h2') %>% 
                            html_text2() %>% 
                            pluck())
  review_date <- map_dbl(reviews_data, ~.x %>% 
                           pluck() %>% 
                           html_elements('.common__EiReviewDetailsStyle__newUiJobLine') %>%
                           html_text2() %>% 
                           substr(1,12) %>%
                           mdy() %>% 
                           pluck()
  ) %>% as_date()
  
  
  review_person <- map_chr(reviews_data, ~.x %>% 
                             pluck() %>% 
                             html_elements('.common__EiReviewDetailsStyle__newUiJobLine') %>% 
                             html_text2() %>% 
                             substr(15,100) %>% 
                             str_trim() %>% 
                             pluck())
  
  ratings <- map_dbl(reviews_data, ~.x %>% 
                             pluck() %>% 
                             html_elements('.ratingNumber') %>% 
                             html_text2() %>% 
                             as.numeric() %>% 
                             pluck())
  
  review_text <- map(reviews_data, ~.x %>% 
                       pluck() %>% 
                       html_elements('.v2__EIReviewDetailsV2__fullWidth') %>% 
                       html_text2() %>% 
                       str_remove_all('\n|\r') %>% 
                       str_trim() %>% 
                       pluck())
  
  review_text_pro <- map_chr(review_text, ~.x %>% 
                               str_extract('Pros(.*)', group = 1) %>% 
                               str_trim() %>% 
                               na.omit(.) %>% 
                               head(n=1))
  
  review_text_con <- map_chr(review_text, ~.x %>% 
                               str_extract('Cons(.*)', group = 1) %>% 
                               str_trim() %>% 
                               na.omit(.) %>% 
                               head(n=1))
  
  review_df <- tibble(
    review_title = review_title, 
    review_date = review_date,
    review_person = review_person, 
    ratings = ratings,
    review_text = review_text, 
    review_text_pro = review_text_pro, 
    review_text_con = review_text_con
  )
  
  file_name <- paste0('/Users/gmu/Dropbox/R_code/glassdoor_review/data/', str_replace_all(tolower(company_name), ' |-', '_'),'_',Sys.Date(), '_review_data.csv')
  write_csv(review_df, file_name)
  
  Sys.sleep(sample(c(5:10),1))
}



