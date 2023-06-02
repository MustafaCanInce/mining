## Mustafa Can İNCE - 200709081
## Adem VAROL - 200709078

required_packages <- c("shiny", "ggplot2", "dplyr", "naivebayes", "caret", "e1071",
                       "rpart", "randomForest", "rpart.plot", "DT", "rstudioapi",
                       "png")

for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
  if (require(package, character.only = TRUE)) {
    library(package, character.only = TRUE)
  }
}

ui <- fluidPage(
  
  tabsetPanel(type = "tabs",
              tabPanel("Data", dataTableOutput("veri_tablosu")),
              tabPanel("Plot", plotOutput("piechart"), plotOutput("piechart2"), 
                       plotOutput("piechart3"), plotOutput("piechart4"), plotOutput("piechart5"),
                       imageOutput("dt_plot_image")),
              tabPanel("Summary", 
                       fluidRow(
                         column(6, tableOutput("overall_table")),
                       ),
                       fluidRow(
                         fluidRow(
                           column(4, h5("Estimates and Original G3 Values for NB"), tableOutput("nb_compare")),
                           column(4, h5("Estimates and Original G3 Values for RF"), tableOutput("rf_compare")),
                           column(4, h5("Estimates and Original G3 Values for RF (G1 + G2 + studytime + age + Fedu + Medu + freetime)"), tableOutput("rf_compare_more")),
                           column(4, h5("Estimates and Original G3 Values for DT"), tableOutput("dt_compare"))
                         )
                       )
              )
  )
)

server <- function(input, output, session) {
  
  
  setwd(dirname(getActiveDocumentContext()$path))
  getwd()
  
  # read_csv <- read.csv('student-poRR.csv')
  # colnames(read_csv)
  
  ##############################################################################
  ############################## Naive Bayes ###################################
  ##############################################################################
  nb_veri_seti  <- read.csv('student-poRR.csv')
  
  set.seed(123)  # Rastgelelik ayarını sabitler
  nb_veri_seti$G3 <- as.factor(nb_veri_seti$G3)  # Hedef değişkeni faktör yapar
  nb_indisler <- sample(1:nrow(nb_veri_seti), round(0.7*nrow(nb_veri_seti)))  # Rastgele indisler seçer
  egitim_veri <- nb_veri_seti[nb_indisler, ]
  test_veri <- nb_veri_seti[-nb_indisler, ]
  
  # Naive Bayes modelini eğitir
  nb_modelg1g2 <- naiveBayes(G3 ~ G1 + G2, data = egitim_veri)
  
  # Test verilerini kullanarak tahmin yapar
  nb_tahminler <- predict(nb_modelg1g2, test_veri)
  
  nb_confusion_mat_g1g2 <- caret::confusionMatrix(nb_tahminler, test_veri$G3)
  
  ##############################################################################
  # Naive Bayes modelini eğitir
  nb_model_g1g2stage <- naiveBayes(G3 ~ G1 + G2 + studytime + age, data = egitim_veri)
  
  # Test verilerini kullanarak tahmin yapar
  nb_tahminler_g1g2stage <- predict(nb_model_g1g2stage, test_veri)
  
  nb_confusion_mat_g1g2stage <- caret::confusionMatrix(nb_tahminler_g1g2stage, test_veri$G3)
  
  ##############################################################################
  # Naive Bayes modelini eğitir
  nb_model_g1g2stagefedu <- naiveBayes(G3 ~ G1 + G2 + studytime + age + Fedu, data = egitim_veri)
  
  # Test verilerini kullanarak tahmin yapar
  nb_tahminler_g1g2stagefedu <- predict(nb_model_g1g2stagefedu, test_veri)
  
  nb_confusion_mat_g1g2stagefedu <- caret::confusionMatrix(nb_tahminler_g1g2stagefedu, test_veri$G3)
  
  ##############################################################################
  # Naive Bayes modelini eğitir
  nb_model_g1g2stagefedumedu <- naiveBayes(G3 ~ G1 + G2 + studytime + age + Fedu + Medu, data = egitim_veri)
  
  # Test verilerini kullanarak tahmin yapar
  nb_tahminler_g1g2stagefedumedu <- predict(nb_model_g1g2stagefedumedu, test_veri)
  
  nb_confusion_mat_g1g2stagefedumedu <- caret::confusionMatrix(nb_tahminler_g1g2stagefedumedu, test_veri$G3)
  ##############################################################################
  # Naive Bayes modelini eğitir
  nb_model_g1g2stagefedumeduft <- naiveBayes(G3 ~ G1 + G2 + studytime + age + Fedu + Medu + freetime, data = egitim_veri)
  
  # Test verilerini kullanarak tahmin yapar
  nb_tahminler_g1g2stagefedumeduft <- predict(nb_model_g1g2stagefedumeduft, test_veri)
  
  nb_confusion_mat_g1g2stagefedumeduft <- caret::confusionMatrix(nb_tahminler_g1g2stagefedumeduft, test_veri$G3)
  
  ##############################################################################
  ############################# Random Forest ##################################
  ##############################################################################
  
  rf_veri_seti <- read.csv('student-poRR.csv')
  
  # G3 sütununu faktör olarak dönüştürür
  rf_veri_seti$G3 <- as.factor(rf_veri_seti$G3)
  
  # Random Forest modelini eğitir
  rf_modelg1g2 <- randomForest(G3 ~ G2 + G1, data = rf_veri_seti)
  
  # Tahminleri yapar
  rf_tahminler <- predict(rf_modelg1g2, newdata = rf_veri_seti)
  
  # Tahminler ve asıl değerler arasında aynı seviyelere sahip faktörler oluşturur
  rf_tahminler_factor <- factor(rf_tahminler, levels = levels(rf_veri_seti$G3))
  rf_asil_degerler_factor <- factor(rf_veri_seti$G3, levels = levels(rf_veri_seti$G3))
  
  # Confusion matrix'i oluşturur
  rf_confusion_mat_g1g2 <- confusionMatrix(rf_tahminler_factor, rf_asil_degerler_factor)
  
  # Random Forest modelini eğitir
  ##############################################################################
  
  rf_model_g1g2stage <- randomForest(G3 ~ G2 + G1 + studytime + age , data = rf_veri_seti)
  
  # Tahminleri yapar
  rf_tahminler_g1g2stage <- predict(rf_model_g1g2stage, newdata = rf_veri_seti)
  
  # Tahminler ve asıl değerler arasında aynı seviyelere sahip faktörler oluşturur
  rf_tahminler_factor_more <- factor(rf_tahminler_g1g2stage, levels = levels(rf_veri_seti$G3))
  rf_asil_degerler_factor_more <- factor(rf_veri_seti$G3, levels = levels(rf_veri_seti$G3))
  
  # Confusion matrix'i oluşturur
  rf_confusion_mat_g1g2stage <- confusionMatrix(rf_tahminler_factor_more, rf_asil_degerler_factor_more)
  
  # Random Forest modelini eğitir
  ##############################################################################
  
  rf_model_g1g2stagefedu <- randomForest(G3 ~ G2 + G1 + studytime + age + Fedu , data = rf_veri_seti)
  
  # Tahminleri yapar
  rf_tahminler_g1g2stagefedu <- predict(rf_model_g1g2stagefedu, newdata = rf_veri_seti)
  
  # Tahminler ve asıl değerler arasında aynı seviyelere sahip faktörler oluşturur
  rf_tahminler_factor_more <- factor(rf_tahminler_g1g2stagefedu, levels = levels(rf_veri_seti$G3))
  rf_asil_degerler_factor_more <- factor(rf_veri_seti$G3, levels = levels(rf_veri_seti$G3))
  
  # Confusion matrix'i oluşturur
  rf_confusion_mat_g1g2stagefedu <- confusionMatrix(rf_tahminler_factor_more, rf_asil_degerler_factor_more)
  
  # Random Forest modelini eğitir
  ##############################################################################
  
  rf_model_g1g2stagefedumedu <- randomForest(G3 ~ G2 + G1 + studytime + age + Fedu +Medu , data = rf_veri_seti)
  
  # Tahminleri yapar
  rf_tahminler_g1g2stagefedumedu <- predict(rf_model_g1g2stagefedumedu, newdata = rf_veri_seti)
  
  # Tahminler ve asıl değerler arasında aynı seviyelere sahip faktörler oluşturur
  rf_tahminler_factor_more <- factor(rf_tahminler_g1g2stagefedumedu, levels = levels(rf_veri_seti$G3))
  rf_asil_degerler_factor_more <- factor(rf_veri_seti$G3, levels = levels(rf_veri_seti$G3))
  
  # Confusion matrix'i oluşturur
  rf_confusion_mat_g1g2stagefedumedu <- confusionMatrix(rf_tahminler_factor_more, rf_asil_degerler_factor_more)
  
  # Random Forest modelini eğitir
  ##############################################################################
  
  rf_model_g1g2stagefedumeduft <- randomForest(G3 ~ G2 + G1 + studytime + age + Fedu + Medu +freetime , data = rf_veri_seti)
  
  # Tahminleri yapar
  rf_tahminler_g1g2stagefedumeduft <- predict(rf_model_g1g2stagefedumeduft, newdata = rf_veri_seti)
  
  # Tahminler ve asıl değerler arasında aynı seviyelere sahip faktörler oluşturur
  rf_tahminler_factor_more <- factor(rf_tahminler_g1g2stagefedumeduft, levels = levels(rf_veri_seti$G3))
  rf_asil_degerler_factor_more <- factor(rf_veri_seti$G3, levels = levels(rf_veri_seti$G3))
  
  # Confusion matrix'i oluşturur
  rf_confusion_mat_g1g2stagefedumeduft <- confusionMatrix(rf_tahminler_factor_more, rf_asil_degerler_factor_more)
  
   ##############################################################################
  ############################## Decision tree #################################
  ##############################################################################
  
  dt_veri_seti <- read.csv('student-poRR.csv')
  
  # G3 sütununu faktör olarak dönüştürür
  dt_veri_seti$G3 <- as.factor(dt_veri_seti$G3)
  
  # Karar ağacı modelini eğitir
  dt_model_g1g2 <- rpart(G3 ~ G2 + G1, data = dt_veri_seti)
  
  # Karar ağacını görselleştirir
  #rpart.plot(dt_model)
  
  # Tahminleri yapar
  dt_tahminler_g1g2 <- predict(dt_model_g1g2, newdata = dt_veri_seti, type = "class")
  
  # Tahminler ve asıl değerler arasında aynı seviyelere sahip faktörler oluşturur
  tahminler_factor <- factor(dt_tahminler_g1g2, levels = levels(dt_veri_seti$G3))
  asil_degerler_factor <- factor(dt_veri_seti$G3, levels = levels(dt_veri_seti$G3))
  
  # Confusion matrix'i oluşturur
  dt_confusion_mat_g1g2 <- confusionMatrix(tahminler_factor, asil_degerler_factor)
  
  ##############################################################################
  
  # Karar ağacı modelini eğitir
  dt_model_g1g2stage <- rpart(G3 ~ G2 + G1 + studytime + age, data = dt_veri_seti)
  
  # Tahminleri yapar
  dt_tahminler_g1g2stage <- predict(dt_model_g1g2stage, newdata = dt_veri_seti, type = "class")
  
  # Tahminler ve asıl değerler arasında aynı seviyelere sahip faktörler oluşturur
  tahminler_factor_more <- factor(dt_tahminler_g1g2stage, levels = levels(dt_veri_seti$G3))
  asil_degerler_factor_more <- factor(dt_veri_seti$G3, levels = levels(dt_veri_seti$G3))
  
  # Confusion matrix'i oluşturur
  dt_confusion_mat_g1g2stage <- confusionMatrix(tahminler_factor_more, asil_degerler_factor_more)
  
  ##############################################################################
  
  # Karar ağacı modelini eğitir
  dt_model_g1g2stagefedu <- rpart(G3 ~ G2 + G1 + studytime + age + Fedu, data = dt_veri_seti)
  
  # Tahminleri yapar
  dt_tahminler_g1g2stagefedu <- predict(dt_model_g1g2stagefedu, newdata = dt_veri_seti, type = "class")
  
  # Tahminler ve asıl değerler arasında aynı seviyelere sahip faktörler oluşturur
  tahminler_factor_more <- factor(dt_tahminler_g1g2stagefedu, levels = levels(dt_veri_seti$G3))
  asil_degerler_factor_more <- factor(dt_veri_seti$G3, levels = levels(dt_veri_seti$G3))
  
  # Confusion matrix'i oluşturur
  dt_confusion_mat_g1g2stagefedu <- confusionMatrix(tahminler_factor_more, asil_degerler_factor_more)
  
  ##############################################################################
  
  # Karar ağacı modelini eğitir
  dt_model_g1g2stagefedumedu <- rpart(G3 ~ G2 + G1 + studytime + age + Fedu + Medu, data = dt_veri_seti)
  
  # Tahminleri yapar
  dt_tahminler_g1g2stagefedumedu <- predict(dt_model_g1g2stagefedumedu, newdata = dt_veri_seti, type = "class")
  
  # Tahminler ve asıl değerler arasında aynı seviyelere sahip faktörler oluşturur
  tahminler_factor_more <- factor(dt_tahminler_g1g2stagefedumedu, levels = levels(dt_veri_seti$G3))
  asil_degerler_factor_more <- factor(dt_veri_seti$G3, levels = levels(dt_veri_seti$G3))
  
  # Confusion matrix'i oluşturur
  dt_confusion_mat_g1g2stagefedumedu <- confusionMatrix(tahminler_factor_more, asil_degerler_factor_more)
  
  ##############################################################################
  
  # Karar ağacı modelini eğitir
  dt_model_g1g2stagefedumeduft <- rpart(G3 ~ G2 + G1 + studytime + age + Fedu + Medu + freetime, data = dt_veri_seti)
  
  # Tahminleri yapar
  dt_tahminler_g1g2stagefedumeduft <- predict(dt_model_g1g2stagefedumeduft, newdata = dt_veri_seti, type = "class")
  
  # Tahminler ve asıl değerler arasında aynı seviyelere sahip faktörler oluşturur
  tahminler_factor_more <- factor(dt_tahminler_g1g2stagefedumeduft, levels = levels(dt_veri_seti$G3))
  asil_degerler_factor_more <- factor(dt_veri_seti$G3, levels = levels(dt_veri_seti$G3))
  
  # Confusion matrix'i oluşturur
  dt_confusion_mat_g1g2stagefedumeduft <- confusionMatrix(tahminler_factor_more, asil_degerler_factor_more)
  
  ##############################################################################
  
   output$veri_tablosu <- renderDataTable({
    data <- read.csv('student-poRR.csv')
    colnames(data)
    all_data <- head(data, nrow(data))
    # Veri tablosunu döndürür
    datatable(all_data)
  })
  
  output$dt_plot_image <- renderImage({
    # Çıktıyı bir grafik cihazına yönlendirir
    png(filename = "dt_plot.png", width = 800, height = 600)
    rpart.plot(dt_model_g1g2, type = 4, extra = 1, under = TRUE, tweak = 1.2, box.palette = 0)
    dev.off()
    
    # Grafik dosyasını Shiny uygulamasında görüntüler
    list(src = "dt_plot.png",
         contentType = "image/png",
         width = "800px",
         height = "600px",
         alt = "DT Plot")
  }, deleteFile = FALSE)
  
  
  output$nb_compare <- renderTable({
    # Tahmin edilen sınıfların ve orijinal "G3" değerlerinin olduğu veri çerçevesi oluşturur
    nb_veri_tablosu <- data.frame(
      Original = nb_veri_seti$G3[1:25], # İlk 25
      Predictions = nb_tahminler[1:25] # İlk 25
    )
    
    nb_veri_tablosu
  })
  
  output$rf_compare <- renderTable({
    # Tahmin edilen sınıfların ve orijinal "G3" değerlerinin olduğu veri çerçevesi oluşturur
    rf_veri_tablosu <- data.frame(
      Original = rf_veri_seti$G3[1:25], # İlk 25
      Predictions = rf_tahminler[1:25] # İlk 25
    )
    
    rf_veri_tablosu
  })
  
  output$rf_compare_more <- renderTable({
    # Tahmin edilen sınıfların ve orijinal "G3" değerlerinin olduğu veri çerçevesi oluşturur
    rf_veri_tablosu <- data.frame(
      Original = rf_veri_seti$G3[1:25], # İlk 25
      Predictions = rf_tahminler_g1g2stagefedumeduft[1:25] # İlk 25
    )
    
    rf_veri_tablosu
  })
  
  output$dt_compare <- renderTable({
    # Tahmin edilen sınıfların ve orijinal "G3" değerlerinin olduğu veri çerçevesi oluşturur
    dt_veri_tablosu <- data.frame(
      Original = dt_veri_seti$G3[1:25], # İlk 25
      Predictions = dt_tahminler_g1g2[1:25] # İlk 25
    )
    
    dt_veri_tablosu
  })
  
  
  output$overall_table <- renderTable({
    
    # 3 sütun ve 3 satırdan oluşan bir veri çerçevesi (data frame) oluşturur
    veri <- data.frame(
      'Accuracy_G1_G2' = c(nb_confusion_mat_g1g2$overall['Accuracy'], 
                            rf_confusion_mat_g1g2$overall['Accuracy'], 
                            dt_confusion_mat_g1g2$overall['Accuracy']),
      
      'Accuracy_G1_G2_st_age' = c(nb_confusion_mat_g1g2stage$overall['Accuracy'], 
                                          rf_confusion_mat_g1g2stage$overall['Accuracy'], 
                                          dt_confusion_mat_g1g2stage$overall['Accuracy']),
      
      'Accuracy_G1_G2_st_age_Fedu' = c(nb_confusion_mat_g1g2stagefedu$overall['Accuracy'], 
                                       rf_confusion_mat_g1g2stagefedu$overall['Accuracy'], 
                                       dt_confusion_mat_g1g2stagefedu$overall['Accuracy']),
      
      'Accuracy_G1_G2_st_age_Fedu_Medu' = c(nb_confusion_mat_g1g2stagefedumedu$overall['Accuracy'], 
                                       rf_confusion_mat_g1g2stagefedumedu$overall['Accuracy'], 
                                       dt_confusion_mat_g1g2stagefedumedu$overall['Accuracy']),
      
      'Accuracy_G1_G2_st_age_Fedu_Medu_freetime' = c(nb_confusion_mat_g1g2stagefedumeduft$overall['Accuracy'], 
                                            rf_confusion_mat_g1g2stagefedumeduft$overall['Accuracy'], 
                                            dt_confusion_mat_g1g2stagefedumeduft$overall['Accuracy'])
  
    )
    
    # Satır isimlerini belirler
    satir_isimleri <- c("Naive Bayes", "Random Forest", "Decision Tree")
    rownames(veri) <- satir_isimleri
    
    veri
  }, rownames = TRUE)
  
  
  output$piechart <- renderPlot({
    dataset <- read.csv('student-poRR.csv')
    renkler <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                 "#D55E00", "#CC79A7", "#000000", "#999999", "#123020",
                 "#666666", "#999999", "#BBBBBB", "#CCCCCC", "#DDDDDD",
                 "#EEEEEE", "#F0F880")
    
    plot <- ggplot(data = dataset) +
      geom_bar(aes(x = "", fill = factor(G3))) +
      coord_polar("y", start = 0) +
      labs(fill = "G3") +
      scale_fill_manual(values = renkler) +
      theme_void()
    
    plot <- plot + ggtitle("G3 Distribution (original data)")
    
    plot
    
  })
  
  output$piechart2 <- renderPlot({
    
    tahmin_veri <- data.frame(Tahmin = nb_tahminler)
    
    # Tahminlerin frekansını hesaplar
    frekanslar <- tahmin_veri %>%
      group_by(Tahmin) %>%
      summarise(Frekans = n())
    
    # Pie chart'ı oluşturur
    renkler <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                 "#D55E00", "#CC79A7", "#000000", "#999999", "#000000",
                 "#666666", "#999999", "#BBBBBB", "#CCCCCC", "#DDDDDD",
                 "#EEEEEE", "#F0F0F0")
    
    pie_chart <- ggplot(data = frekanslar, aes(x = "", y = Frekans, fill = Tahmin)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(fill = "Tahmin") +
      scale_fill_manual(values = renkler) +
      theme_void() +
      theme(legend.position = "right") +
      ggtitle("G3 Distribution for NB (model using G1 and G2)")
    
    # Pie chart'ı görüntüler
    print(pie_chart)
    
    
  })
  
  output$piechart3 <- renderPlot({
    
    tahmin_veri <- data.frame(Tahmin = rf_tahminler)
    
    # Tahminlerin frekansını hesaplar
    frekanslar <- tahmin_veri %>%
      group_by(Tahmin) %>%
      summarise(Frekans = n())
    
    # Pie chart'ı oluşturur
    renkler <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                 "#D55E00", "#CC79A7", "#000000", "#999999", "#000000",
                 "#666666", "#999999", "#BBBBBB", "#CCCCCC", "#DDDDDD",
                 "#EEEEEE", "#F0F0F0")
    
    pie_chart <- ggplot(data = frekanslar, aes(x = "", y = Frekans, fill = Tahmin)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(fill = "Tahmin") +
      scale_fill_manual(values = renkler) +
      theme_void() +
      theme(legend.position = "right") +
      ggtitle("G3 Distribution for RF (model using G1 and G2)")
    
    # Pie chart'ı görüntüler
    print(pie_chart)
    
  })
  
  output$piechart4 <- renderPlot({
    
    tahmin_veri <- data.frame(Tahmin = dt_tahminler_g1g2)
    
    # Tahminlerin frekansını hesaplar
    frekanslar <- tahmin_veri %>%
      group_by(Tahmin) %>%
      summarise(Frekans = n())
    
    # Pie chart'ı oluştur
    renkler <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                 "#D55E00", "#CC79A7", "#000000", "#999999", "#000000",
                 "#666666", "#999999", "#BBBBBB", "#CCCCCC", "#DDDDDD",
                 "#EEEEEE", "#F0F0F0")
    
    pie_chart <- ggplot(data = frekanslar, aes(x = "", y = Frekans, fill = Tahmin)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(fill = "Tahmin") +
      scale_fill_manual(values = renkler) +
      theme_void() +
      theme(legend.position = "right") +
      ggtitle("G3 Distribution for DT (model using G1 and G2)")
    
    # Pie chart'ı görüntüler
    print(pie_chart)
    
  })
  
  output$piechart5 <- renderPlot({
    
    tahmin_veri <- data.frame(Tahmin = rf_tahminler_g1g2stagefedumeduft)
    
    # Tahminlerin frekansını hesaplar
    frekanslar <- tahmin_veri %>%
      group_by(Tahmin) %>%
      summarise(Frekans = n())
    
    # Pie chart'ı oluştur
    renkler <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                 "#D55E00", "#CC79A7", "#000000", "#999999", "#000000",
                 "#666666", "#999999", "#BBBBBB", "#CCCCCC", "#DDDDDD",
                 "#EEEEEE", "#F0F0F0")
    
    pie_chart <- ggplot(data = frekanslar, aes(x = "", y = Frekans, fill = Tahmin)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(fill = "Tahmin") +
      scale_fill_manual(values = renkler) +
      theme_void() +
      theme(legend.position = "right") +
      ggtitle("G3 Distribution for RF (G1 + G2 + studytime + age + Fedu + Medu + freetime)")
    
    # Pie chart'ı görüntüler
    print(pie_chart)
    
  })
  
  read_csv <- read.csv('student-poRR.csv')
  
  combined_data <- read_csv %>%
    filter(paid %in% c("yes", "no")) %>%
    group_by(sex, paid) %>%
    summarise(count = n()) %>%
    mutate(label = paste0(round(count / sum(count) * 100), "%"))
  
}

shinyApp(ui, server)

