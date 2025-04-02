#LOAD AND INSPECT DATA
install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
setwd("C:/Users/HP/Downloads")
graduate_df <- read.csv("Graduate.csv")

#PREPROCESSING AND CLEANING THE DATA
str(graduate_df)
View(graduate_df)
graduate_df$Admit <- as.factor(graduate_df$Admit)
graduate_df<-graduate_df[-1]
graduate_df<- na.omit(graduate_df)
summary(graduate_df)

#PARTITION OF DATA
library(caret)
index <- createDataPartition(graduate_df$Admit, p =0.8,list = FALSE)
index
training_set <- graduate_df[index,]
testing_set <- graduate_df[-index,]
training_labels <-graduate_df[index,"Admit"]
testing_labels <-graduate_df[-index,"Admit"]
target <- Admit ~ GRE.Score + TOEFL.Score + University.Rating + SOP + LOR + CGPA + Research
target
#EVALUATION OF DATA USING DESICION TREE ALGORITHM

tree <- rpart(target, data = training_set, method = "class")
rpart.plot(tree)
prediction1 <- predict(tree, testing_set, type = "class")
prediction1
confus_mat <- confusionMatrix(prediction1, testing_labels)
print(confus_mat)

#EVALUATION OF DATA USING SUPPORT VECTOR MACHINE
library(e1071)
svm_model <- svm(target, data = training_set, kernel = "linear")
prediction2 <- predict(svm_model, testing_set)
confus_mat1 <- confusionMatrix(prediction2, testing_labels)
print(confus_mat1)

#COMPARISON OF ALGORITHMS
dt_accuracy <- confus_mat$overall["Accuracy"]
svm_accuracy <- confus_mat1$overall["Accuracy"]


cat("DECISION TREE ACCURACY : ",round(dt_accuracy *100,2),"%\n")

cat("SUPPORT VECTOR MACHINE ACCURACY : ",round(svm_accuracy *100,2),"%\n")

cat("------DECISION TREE PREDICTIONS------")
print(table(prediction1,testing_labels))

cat("------SUPPORT VECTOR MACHINE PREDICTIONS------")
print(table(prediction2,testing_labels))

#VISUALIZING BOTH PREDICITIONS AND ACCURACY OF BOTH ALGORITHMS
#BAR PLOT OF COMPARING ACCURACY OF ALL THE ALGORITHMS

library("ggplot2")
accuracy_df <- data.frame(
  Model = c("Decision Tree", "SVM"),
  Accuracy = c( dt_accuracy * 100, svm_accuracy * 100)
)
accuracy_comparison_plot <- ggplot(accuracy_df, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  ylim(0, 100) +
  labs(title = "Model Accuracy Comparison", y = "Accuracy (%)", x = "Model") +
  theme_minimal() +
  scale_fill_manual(values = c("Decision Tree" = "pink", "SVM" = "green"))+
  geom_text(aes(label = round(Accuracy, 2)),vjust = -0.5)
print(accuracy_comparison_plot)

#COMPARISON TABLE OF PREDICTED AND ACTUAL PRECIPITATION TYPE
dt_table <- as.data.frame(table(Predicted = prediction1,Actual = testing_labels))
svm_table <- as.data.frame(table(Predicted = prediction2,Actual = testing_labels))
print(dt_table)
print(svm_table)

#DECISION TREE CONFUSION MATRIX PLOT
dt_confMatrix <- ggplot(dt_table,aes(x=Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "pink") +
  labs(title = "Decision Tree Confusion Matrix", x = "Actual", y = "Predicted")
print(dt_confMatrix)

#SUPPORT VECTOR MACHINE CONFUSION MATRIX PLOT
svm_confMatrix <- ggplot(svm_table, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "green") +
  labs(title = "Support Vector Machine Confusion Matrix", x = "Actual", y = "Predicted")
print(svm_confMatrix)


#DEFINE UI FOR THE SHINY DASHBOARD
library(shiny)
ui <- fluidPage(
  titlePanel("Graduation predicition"),
  sidebarLayout(
    sidebarPanel(
      h3("Model Comparison and Metrics"),
      p("This dashboard displays the accuracy comparison and confusion matrices for predicting the university admission by the factors GRE score,TOEFl score, university.rating ,SOP,LOR,CGPA,research by using these two predictive models."),
      p("Decision Tree Algorithm"),
      p("Support Vector Machine Algorithm")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Accuracy Comparison", plotOutput("accuracyPlot")),
        tabPanel("Decision Tree Confusion Matrix", plotOutput("dtConfMatrix")),
        tabPanel("SVM Confusion Matrix", plotOutput("svmConfMatrix"))
      )
    )
  )
)
server <- function(input, output) {
  output$accuracyPlot <- renderPlot({
    print(accuracy_comparison_plot)  
  })
  
  output$dtConfMatrix <- renderPlot({
    print(dt_confMatrix)  
  })
  
  output$svmConfMatrix <- renderPlot({
    print(svm_confMatrix)  
  })
}
shinyApp(ui = ui, server=server)