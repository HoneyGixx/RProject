library(quanteda)
library(quanteda.textstats)
library(shiny)
library(wordcloud)
library(RColorBrewer)
library(readtext)
#Нужно чтобы было меню
ui <- fluidPage(
  titlePanel("Text Mining"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Выберите файл в формате .txt"),
      checkboxInput("remove_stopwords", "Убрать стоп-слова", value = TRUE),
      checkboxInput("remove_punctuation", "Убрать пунктуацию", value = TRUE),
      sliderInput("min_freq", "Минимальная частота", min = 1, max = 100, value = 2),
      sliderInput("max_words", "Максимальное количество слов", min = 10, max = 500, value = 200)
    ),
    mainPanel(
      plotOutput("wordcloud")
    )
  )
)

server <- function(input, output) {
  text <- reactive({
    file <- input$file
    if (is.null(file)) return(NULL)
    # Указываем кодировку при чтении текста
    text <- read.csv(file$datapath, encoding = "UTF-8", warn = FALSE)
    print(text)
    return(text)
  })
  
  output$wordcloud <- renderPlot({
    text_data <- text$Шутка
    if (is.null(text_data)) return()
    
    # Токенизация
    tokens_data <- tokens(text_data)
    
    # Удаление стоп-слов и пунктуации, если выбрано
    if (input$remove_stopwords) {
      tokens_data <- tokens_remove(tokens_data, stopwords("ru"))
    }
    if (input$remove_punctuation) {
      tokens_data <- tokens_remove(tokens_data, "[[:punct:]]")
    }
    
    # Создание таблицы частот
    dfm_data <- dfm(tokens_data)
    top_terms <- topfeatures(dfm_data, n = input$max_words)
    top_terms <- top_terms[top_terms >= input$min_freq]
    
    # Построение облака слов
    wordcloud(names(top_terms), top_terms, min.freq = input$min_freq, max.words = input$max_words,
              random.order = FALSE, colors = brewer.pal(8, "Dark2"))
  })
}

shinyApp(ui = ui, server = server)