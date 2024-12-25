library(shiny)
library(shinythemes)
library(tidyverse)
library(stringr)
library(tidytext)
library(ggplot2)
library(quanteda.textplots)
library(caret)
library(randomForest)
library(e1071)
library(caTools)
library(MASS)


thematic::thematic_shiny(font = "auto")
# ui <- fluidPage(
# numericInput("num", "Number one", value = 0, min = 0, max = 100),
# sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
# sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100)
#)
#
ui <- fluidPage(
  theme = shinytheme("flatly"),
  sidebarLayout(
   
      textOutput("panel")
    ,
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Информация о проекте", 
                 HTML("Выполнил: Монастырев Кирилл, гр. 132383.<br>
               Преподаватель: Андрей Александрович.<br>
               Это мой проект на языке R.<br>
               Проводились работы с датасетом. Датасет был предобработан, была проанализирована статистика.")),
        tabPanel("Облако слов", fileInput("file", "Выберите файл в формате .txt"),
                 checkboxInput("remove_stopwords", "Убрать стоп-слова", value = TRUE),
                 checkboxInput("remove_punctuation", "Убрать пунктуацию", value = TRUE),
                 sliderInput("min_freq", "Минимальная частота", min = 1, max = 100, value = 2),
                 sliderInput("max_words", "Максимальное количество слов", min = 10, max = 500, value = 200)),
        tabPanel("Статистика", verbatimTextOutput("stat_results")),
        tabPanel("Графики",
                 plotOutput("histogram"),
                 plotOutput("bar_plot")
      )
    )
  )
)
)

server <- function(input, output){
  #Облако слов
  text <- reactive({
    file <- input$file
    if (is.null(file)) return(NULL)
    text <- readLines(file$datapath)
    text
  })
  
  corpus <- reactive({
    if (is.null(text())) return(NULL)
    corpus <- Corpus(VectorSource(text()))
    if (input$remove_stopwords) {
      corpus <- tm_map(corpus, removeWords, stopwords("russian"))
    }
    if (input$remove_punctuation) {
      corpus <- tm_map(corpus, content_transformer(removePunctuation))
    }
    corpus
  })
  output$wordcloud <- renderPlot({
    if (is.null(corpus())) return()
    wordcloud(corpus(), scale=c(5,0.5), min.freq = input$min_freq, max.words = input$max_words,
              random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
  })
}

server <- function(input, output) {

data <- data.frame(
  Шутка = c("Да", "Нет", "Да", "Нет"),
  text_word_count = c(100, 150, 200, 250),
  title_word_count = c(10, 15, 20, 25)
)

# Выполнение статистических тестов и вывод результатов
output$stat_results <- renderPrint({
  # t-тест для кол-ва слов в текстах
  t_test_text <- t.test(text_word_count ~ Шутка, data = data)

  # t-тест для кол-ва слов в заголовках
  t_test_title <- t.test(title_word_count ~ Шутка, data = data)

  # Корреляционный анализ между кол-вом слов в заголовках и текстах
  cor_test <- cor.test(data$title_word_count, data$text_word_count)

  # Объединение результатов в один вывод
  cat("Результаты t-теста для количества слов в текстах:\n")
  print(t_test_text)
  cat("\nРезультаты t-теста для количества слов в заголовках:\n")
  print(t_test_title)
  cat("\nРезультаты корреляционного анализа:\n")
  print(cor_test)
})
}

shinyApp(ui, server)