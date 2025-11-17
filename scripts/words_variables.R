setwd("C:/Users/lveras/OneDrive - FLAR/Personal/Repositorios de Github/PS3_Equipo3/stores/")

require(pacman)

p_load("tidyverse","textir","wordcloud","tidytext","stopwords","glmnet")



data_train <- read.csv("train.csv")


spanish_stop <- tibble(word = stopwords("es"))

# ---- 3. Tokenize TITLE ----
tokens_title <- data_train %>%
  select(property_id, title) %>%
  unnest_tokens(word, title, token = "words") %>%
  anti_join(spanish_stop, by = "word") %>%
  mutate(source = "title")


wordcloud(words = tokens_title$word,
          min.freq = 0,
          scale = c(1.5, 0.1), 
          max.words=200, 
          random.order=FALSE, 
          colors=brewer.pal(8, "YlOrRd"))


tokens_description <- data_train %>%
  select(property_id, description) %>%
  unnest_tokens(word, description, token = "words") %>%
  anti_join(spanish_stop, by = "word") %>%
  mutate(source = "description") %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  filter(str_length(word) > 3) %>% filter(word != "apartamento") %>%
  filter(word != "sala") %>%
  filter(word != "alcobas")%>%
  filter(word != "bano") %>%
  filter(word != "banos") %>%
  filter(word != "cocina") %>%
  filter(word != "comedor")%>%
  filter(word != "habitaciones")%>%
  filter(word != "casa")%>%
  filter(word != "zona")%>%
  filter(word != "estudio") %>%
  filter(word != "price") %>%
  filter(word != "carolina") %>%
  filter(word != "cali") %>%
  filter(word != "cedritos") 

tokens_description <- tokens_description %>%
  add_count(word, name = "freq_word") %>%
  filter(freq_word > 500) %>%
  select(-freq_word)

tokens_description <- tokens_description %>% count(property_id, word) %>%
  tidyr::pivot_wider(
    names_from = word,
    values_from = n,
    values_fill = 0
  )


#-------------------------------------------------------------------------------
# Making the traind database sacandos precios negativos y no-numericos

train_price <- data_train %>% select(property_id, price) %>%
  mutate(price = suppressWarnings(as.numeric(price))) %>%
  filter(!is.na(price), price >= 0) %>% mutate(price = log(price))

data_lasso <- tokens_description %>%
  inner_join(train_price, by = "property_id")

predictor_data <- data_lasso %>% select(-property_id, -price)

# X matrix: predictors (words)
X <- as.matrix(predictor_data)

# y vector: price
y <- data_lasso$price

lasso_cv <- cv.glmnet(
  x = X,
  y = y,
  alpha = 1,       # LASSO
  nfolds = 10
)

best_lambda <- lasso_cv$lambda.min
best_lambda

coef_lasso <- coef(lasso_cv, s = "lambda.min")
coef_lasso

# Extract coefficients as a data frame
coef_df <- coef(lasso_cv, s = "lambda.min") %>%
  as.matrix() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("word") %>%
  rename(coef = 2)

# Remove intercept
coef_df <- coef_df %>% filter(word != "(Intercept)")

# Select top 10 words with largest absolute coefficients
top10 <- coef_df %>%
  mutate(abs_coef = abs(coef)) %>%
  arrange(desc(abs_coef)) %>%
  slice(1:10)

word <- top10$word

# Selecting Tokens descriptions

tokens_description <- tokens_description %>% select(property_id, word)


# Guardando

write.csv(tokens_description,"data_words.csv")