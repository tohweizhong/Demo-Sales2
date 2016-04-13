
# app.R

source("load.R")

ui <- dashboardPage(
    dashboardHeader(title = "Students' performance", titleWidth = 250),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data", tabName = "data", icon = icon("table")),
            menuItem("Descriptive", tabName = "descriptive", icon = icon("eye")),
            menuItem("Predictive", tabName = "predictive", icon = icon("line-chart")),
            menuItem("Prescriptive", tabName = "prescriptive", icon = icon("thumbs-o-up"))
        )
    ),
    dashboardBody(
        tabItems(
            # data
            tabItem(tabName = "data",
                    dataTableOutput("view_data")
            ),
            
            # descriptive
            tabItem(tabName = "descriptive",
                    box(selectInput("var1",
                                    label = "Which variable?",
                                    choices = c("G1", "G2", "G3"))),
                    box(selectInput("var2",
                                    label = "And which variable?",
                                    choices = colnames(subset(Xtt, select = -c(G1, G2, G3))))),
                    box(plotOutput("plot_scores"), width = 4),
                    box(plotOutput("plot_othervars"), width = 4),
                    box(plotOutput("plot_2var"), width = 4)
            ),
            
            # predictive
            tabItem(tabName = "predictive",
                    tabsetPanel(
                        tabPanel("G1",
                                 tags$h4("Predicting G1 (pass or fail)"),
                                 box(plotOutput("plot_tr0_G1")),
                                 box(dataTableOutput("plot_tr0_G1_pred"))),
                        tabPanel("G2",
                                 tags$h4("Predicting G2 (actual scores)"),
                                 box(plotOutput("plot_tr0_G2")),
                                 box(plotOutput("plot_tr0_G2_pred"))
                        ),
                        tabPanel("G3",
                                 tags$h4("Predicting improvements in G3 (students who failed either G1 or G2 or both)"),
                                 box(plotOutput("plot_lm0_G3_beta"), width = 4),
                                 box(plotOutput("plot_lm0_G3_anova"), width = 4),
                                 box(plotOutput("plot_lm0_G3_pred"), width = 4)
                        )
                    )
            ),
            
            # prescriptive
            tabItem(tabName = "prescriptive",
                    tabsetPanel(
                        tabPanel("FACTORS OF INTEREST",
                                 tags$br(),
                                 fluidRow(
                                     valueBoxOutput("prescriptive_text", width = 4)
                                 ),
                                 tags$h4("Static variables"),
                                 fluidRow(
                                     box(selectInput("age",
                                                     label = "Age of sudent",
                                                     choices = sort(unique(Xtt$age)),
                                                     selected = "15"), width = 2, height = 110),
                                     box(selectInput("failures",
                                                     label = "Past failures",
                                                     choices = sort(unique(Xtt$failures)),
                                                     selected = "0"), width = 2, height = 110),
                                     box(selectInput("G1_cate",
                                                     label = "Passed G1?",
                                                     choices = c("Pass", "Fail"),
                                                     selected = "Pass"), width = 2, height = 110),
                                     box(selectInput("G2",
                                                     label = "G2 score",
                                                     choices = seq(10, 100, by = 10),
                                                     selected = "60"), width = 2, height = 110),
                                     box(selectInput("famrel",
                                                     label = "Quality of family relationships",
                                                     choices = sort(unique(Xtt$famrel)),
                                                     selected = "5"), width = 2, height = 110),
                                     box(selectInput("famsup",
                                                     label = "Govt financial aid?",
                                                     choices = c("yes", "no"),
                                                     selected = "yes"), width = 2, height = 110)
                                 ),
                                 tags$h4("Actionable variables"),
                                 fluidRow(
                                     box(selectInput("romantic",
                                                     label = "Is the student in a romantic relationship?",
                                                     choices = c("yes", "no"),
                                                     selected = "yes"), width = 3, height = 110),
                                     box(selectInput("activities",
                                                     label = "Is the student participating in Co-curriculum activities?",
                                                     choices = c("yes", "no"),
                                                     selected = "yes"), width = 3, height = 110),
                                     box(selectInput("paid",
                                                     label = "Is the student going for private tuition classes?",
                                                     choices = c("yes", "no"),
                                                     selected = "no"), width = 3, height = 110),
                                     box(selectInput("schoolsup",
                                                     label = "Is the school subsidizes the student's education?",
                                                     choices = c("yes", "no"),
                                                     selected = "no"), width = 3, height = 110))
                        ),
                        tabPanel("ACTING ON THE ACTIONABLE",
                                 tags$br(),
                                 fluidRow(
                                     box(selectInput("romantic_improv",
                                                     label = "What if the student's romantic relationship status is changed to:",
                                                     choices = c("yes", "no"),
                                                     selected = "yes"), width = 3),
                                     box(selectInput("activities_improv",
                                                     label = "What if the student CCA participation is changed to:",
                                                     choices = c("yes", "no"),
                                                     selected = "yes"), width = 3),
                                     box(selectInput("paid_improv",
                                                     label = "What if the school provides/don't provide supplementary classes:",
                                                     choices = c("yes", "no"),
                                                     selected = "no"), width = 3),
                                     box(selectInput("schoolsup_improv",
                                                     label = "What if the school changes the student's education subsidy to:",
                                                     choices = c("yes", "no"),
                                                     selected = "no"), width = 3)),
                                 fluidRow(
                                     box(plotOutput("prescriptive_plot", width = "100%", height = "500px"), width = 12)
                                 )
                        )
                    )
            )
        )
    )
)
server <- function(input, output){
    
    # data
    output$view_data <- renderDataTable({
        print(getwd())
        datatable(Xtt[1:20,], options = list(pageLength = 20, searching = FALSE, paging = FALSE, scrollX = TRUE))
    })
    
    # ====
    
    # descriptive
    output$plot_scores <- renderPlot({
        qplot(Xtt[,input$var1], geom = "histogram", xlab = input$var1, main = input$var1) + theme_bw()
    })
    output$plot_othervars <- renderPlot({
        if(input$var2 %in% num_vars)
            qplot(Xtt[,input$var2], geom = "histogram", xlab = input$var2, main = input$var2) + theme_bw()
        
        else if(input$var2 %in% cate_vars)
            qplot(Xtt[,input$var2], geom = "bar", xlab = input$var2, main = input$var2) + theme_bw()
    })
    output$plot_2var <- renderPlot({
        qplot(y = Xtt[,input$var1], x = factor(Xtt[,input$var2]),
              main = paste0("Relationship between ", input$var1, " and ", input$var2),
              geom = "boxplot", xlab = input$var2, ylab = input$var1) + theme_bw()
    })
    
    # ====
    
    # predictive
    
    output$plot_tr0_G1 <- renderPlot({
        rpart.plot(tr0_G1, main = "Predicting G1 (pass or fail)")
    })
    output$plot_tr0_G1_pred <- renderDataTable({
        tr0_G1_pred_class <- predict(tr0_G1, newdata = Xtest_G1, type = "class")
        tr0_G1_tb <- table(tr0_G1_pred_class, ytest_G1)
        tr0_G1_acc <- sum(diag(tr0_G1_tb)) / sum(tr0_G1_tb)
        
        tr0_G1_pred_prob <- predict(tr0_G1, newdata = Xtest_G1, type = "prob")
        tr0_G1_auc <- pROC::auc(response = ytest_G1, predictor = tr0_G1_pred_prob[,2])
        
        class(tr0_G1_tb) <- "matrix"
        colnames(tr0_G1_tb) <- c("Fail", "Pass")
        rownames(tr0_G1_tb) <- c("Predicted as fail", "Predicted as pass")
        datatable(tr0_G1_tb, options = list(paging = FALSE, searching = FALSE))
        
    })
    output$plot_tr0_G2 <- renderPlot({
        rpart.plot(tr0_G2, main = "Predicting G2")
    })
    output$plot_tr0_G2_pred <- renderPlot({
        tr0_G2_pred <- predict(tr0_G2, newdata = Xtest_G2)
        tr0_G2_pred_rmse <- rmse(actual = ytest_G2, predicted = tr0_G2_pred)
        
        plot_df <- data.frame(cbind(ytest_G2, tr0_G2_pred))
        colnames(plot_df) <- c("Actual", "Predicted")
        ggplot(data = plot_df, aes(x = Predicted, y = Actual)) +
            geom_point() +
            ggtitle(paste0("Actual vs. Predicted G2")) +
            geom_abline(slope = 1, intercept = 0) + theme_bw()
        
        #str(tr0_G2_pred)
        #print(tr0_G2_pred_rmse)
        #print(str(plot_df))
        #ggplot2::qplot(plot_df, x = plot_df$Predicted, y = plot_df$Actual)
#               main = paste0("Actual G2 vs. Predicted G2, error = ", round(tr0_G2_pred_rmse, 2)),
#               xlab = "Predicted G2", ylab = "Actual G2")
#             #geom_abline(slope = 1, intercept = 0) + theme_bw()
#         
    })
    output$plot_lm0_G3_beta <- renderPlot({
        coef <- summary(lm0_G3)$coefficients[-1,1]
        #n <- names(coef)
        #coef <- data.frame(coef)
        #colnames(coef) <- "n"
        #tr(coef)
        
#         str(coef)
#         str(names(coef))
#         str(seq_along(coef))
#         str(data.frame(coef))
#         
        n <- names(coef)
        
        ggplot(data = data.frame(coef), aes(n, coef)) +
            geom_bar(stat = "identity") +
            xlab("Variables") +
            ylab("Model coefficients") +
            ggtitle("Model coefficients") +
            theme_bw() +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        
#         qplot(coef, x = factor(names(coef)), 
#               main = "Predicting G3 improvements: model coefficients",
#               geom = "bar",
#               #stat = "identity",
#               xlab = "Variables",
#               ylab = "Model coefficients") +
#             theme_bw()
    })
    output$plot_lm0_G3_anova <- renderPlot({
        an <- anova(lm0_G3)
        impt <- an$"Sum Sq"[-nrow(an)]
        names(impt) <- rownames(an)[-nrow(an)]
        
        n <- names(impt)
        #str(impt);print(rownames(data.frame(impt)))
        ggplot(data = data.frame(impt), aes(n, impt)) +
            geom_bar(stat = "identity") +
            xlab("Variables") +
            ylab("Variable importance") +
            ggtitle("Variable importance") +
            theme_bw() +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
            
        
#         ggplot(data = data.frame(impt), aes(factor(names(impt)))) +
#             geom_bar(stat = "identity") +
#             ggtitle("Variable importances for predicting G3 improvements") +
#             theme_bw()
        
#         qplot(impt, x = factor(names(impt)), main = "Variable importances for predicting G3 improvements",
#               geom = "bar", stat = "identity", xlab = "Variables", ylab = "Variable importance") + theme_bw()
    })
    output$plot_lm0_G3_pred <- renderPlot({
        lm0_G3_pred <- predict(lm0_G3, newdata = Xtest_G3)
        lm0_G3_pred_rmse <- rmse(actual = ytest_G3, predicted = lm0_G3_pred)
        
        plot_df <- data.frame(cbind(ytest_G3, lm0_G3_pred))
        colnames(plot_df) <- c("Actual", "Predicted")
        
        
        ggplot(data = plot_df, aes(x = Predicted, y = Actual)) +
            geom_point() +
            ggtitle(paste0("Actual vs. predicted improvements in G3")) +
            geom_abline(slope = 1, intercept = 0) +
            xlab("Predicted") +
            ylab("Actual") +
            geom_abline(slope = 1, intercept = 0) + theme_bw() 
        
#         qplot(plot_df, x = plot_df$Predicted, y = plot_df$Actual,
#               main = paste0("Actual improvements in G3 vs. predicted improvements, error = ", round(lm0_G3_pred_rmse, 2)),
#               xlab = "Predicted improvements", ylab = "Actual improvements") + geom_abline(slope = 1, intercept = 0) + theme_bw()
    })
    
    # ====
    
    # prescriptive
    output$prescriptive_text <- renderValueBox({
        newdata <- data.frame(as.numeric(input$age),
                              as.numeric(input$failures),
                              as.factor(input$activities),
                              as.numeric(input$famrel),
                              as.factor(input$G1_cate),
                              as.numeric(input$G2),
                              as.factor(input$schoolsup),
                              as.factor(input$paid),
                              as.factor(input$famsup),
                              as.factor(input$romantic)
                              #as.numeric(input$Dalc)
        )
        colnames(newdata) <- lm0_G3_vars[-1]
        #         colnames(newdata) <- c("absences", "failures", "G1", "G2",
        #                                "studytime", "Dalc", "famsup", "activities")
        
        lm0_G3_pred <- predict(lm0_G3, newdata = newdata)
        
        valueBox(value = round(as.numeric(lm0_G3_pred), 2),
                 "Predicted improvement", icon = icon("arrows-v"))
        
#         return(paste0("Student is predicted to make an improvement of ",
#                       round(as.numeric(lm0_G3_pred), 2),
#                       " in G3 relative to G2"))
    })
    
    output$prescriptive_plot <- renderPlot({
        
        newdata <- data.frame(as.numeric(input$age),
                              as.numeric(input$failures),
                              as.factor(input$activities),
                              as.numeric(input$famrel),
                              as.factor(input$G1_cate),
                              as.numeric(input$G2),
                              as.factor(input$schoolsup),
                              as.factor(input$paid),
                              as.factor(input$famsup),
                              as.factor(input$romantic)
                              #as.numeric(input$Dalc)
        )
        colnames(newdata) <- lm0_G3_vars[-1]
        
        #str(newdata)
        
        lm0_G3_pred <- as.numeric(predict(lm0_G3, newdata = newdata))
        betas <- summary(lm0_G3)$coefficients[c("romanticyes","activitiesyes", "paidyes" ,"schoolsupyes"),1]
        
        improv <- c(input$romantic_improv, input$activities_improv, input$paid_improv, input$schoolsup_improv)
        #diff <- as.numeric(as.numeric(improv[c(1,2)]) - newdata[,c(5,11)])
        
        diff <- NULL
        if(input$romantic == input$romantic_improv)
            diff <- c(diff, 0)
        else if(input$romantic != input$romantic_improv && input$romantic == "yes")
            diff <- c(diff, -1)
        else if(input$romantic != input$romantic_improv && input$romantic == "no")
            diff <- c(diff, 1)
        
        if(input$activities == input$activities_improv)
            diff <- c(diff, 0)
        else if(input$activities != input$activities_improv && input$activities == "yes")
            diff <- c(diff, -1)
        else if(input$activities != input$activities_improv && input$activities == "no")
            diff <- c(diff, 1)
        
        if(input$paid == input$paid_improv)
            diff <- c(diff, 0)
        else if(input$paid != input$paid_improv && input$paid == "yes")
            diff <- c(diff, -1)
        else if(input$paid != input$paid_improv && input$paid == "no")
            diff <- c(diff, 1)
        
        if(input$schoolsup == input$schoolsup_improv)
            diff <- c(diff, 0)
        else if(input$schoolsup != input$schoolsup_improv && input$schoolsup == "yes")
            diff <- c(diff, -1)
        else if(input$schoolsup != input$schoolsup_improv && input$schoolsup == "no")
            diff <- c(diff, 1)
        
        #str(diff); print(diff)
        
        print("betas")
        print(betas)
        print("diff*betas")
        print(diff*betas)
        print("lm0_G3_pred")
        print(lm0_G3_pred)
        
        
        after_one_action <- lm0_G3_pred + diff*betas
        after_all_actions <- lm0_G3_pred + sum(diff*betas)
        
        print(after_one_action)
        print(after_all_actions)
        
        all_tgt <- c(lm0_G3_pred, after_one_action, after_all_actions)
        names(all_tgt) <- c("Predicted", "Romantic", "CCA", "Supplementary", "Subsidies", "All")
        print(all_tgt)
        #qplot(all_tgt, x = factor(c("Predicted", "Absences", "Alcohol", "CCA", "Supplementary", "Subsidies", "All"),
        #                          levels = c("Predicted", "Absences", "Alcohol", "CCA", "Supplementary", "Subsidies", "All")),
        #      main = "Predicted G3 improvements; acting on actionable variables", geom = "bar", stat = "identity", xlab = "Acting on ...", ylab = "Predicted improvements") + theme_bw()
        
        n <- factor(names(all_tgt), levels = c("Predicted","Romantic", "CCA", "Supplementary", "Subsidies", "All"))
        ggplot(data = data.frame(all_tgt), aes(n, all_tgt)) +
            geom_bar(stat = "identity") +
            xlab("Acting on...") +
            ylab("Predicted improvements") +
            ggtitle("Predicted G3 improvements; acting on actionable variables") +
            theme_bw()
        
        
        #qplot(y = all_tgt, x = factor(names(all_tgt), levels = c("Predicted","Romantic", "CCA", "Supplementary", "Subsidies", "All")), main = "Predicted G3 improvements; acting on actionable variables",
        #      geom = "bar", stat = "identity", xlab = "Acting on...", ylab = "Predicted improvements") + theme_bw()
    })
}

shinyApp(ui, server)