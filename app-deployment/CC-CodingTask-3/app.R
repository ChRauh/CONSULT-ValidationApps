library(shiny)

# Shiny App
ui <- fluidPage(
  titlePanel(HTML("<em>Coding task:</em> <b style='color: darkred;'>Information quality of stakeholder comments</b><br><br>")),
  
  sidebarLayout(
    sidebarPanel(
      style = "text-align: justify;",
      h3("Instructions"),
      p(HTML("<strong><u>Your role</strong></u></b>: Imagine you are a <b><i>bureaucratic official</b></i> who must draft a <b><i>new policy</b></i>. You want to make sure that this policy reflects <b><i>the best and most accurate information available</b></i>. Thus, you have shared your draft policy with potentially affected societal actors and asked for their <b><i>feedback</b></i> in a public consultation.")),
      p(HTML("<strong><b><u>Your task</strong></u></b>: We show you excerpts from the stakeholder responses to your consultation. <em><b style='color: darkred;'>Rate how useful each comment excerpt is for helping you to take the best evidence-based decision possible</b></em>. Ignore whether you agree with the stance or values expressed. Focus only on the <b><i>quality of information</b></i> the comment provides.")),
      p(HTML("<strong><b><u>Guidance</strong></u></b>: High-quality information comes in many forms and we know that you have little context. There are no clear right or wrong answers - <b><i>only your interpretation in this scenario matters!</b></i>")),
      p(HTML("Just judging by the text examples that you see on the right-hand side, <b><i>do you think that the feedback is valuable for an official who is trying to take evidence-based decisions in designing an efficient and effective law?</b></i>")),
      p(HTML("To make your assessment easier, we split the task into more specific questions before asking for your overall assessment on whether this information could be useful for a bureaucratic policymaker.")),
      
      # tags$ul(style = "padding-left: 0; list-style-position: outside; text-align: match-parent;list-style-type: none;",
      #         tags$li(HTML("<em><b>Relevance:</b></em> Does the comment directly address a regulatory issue or does it rather appear as vague or off-topic?")),
      #         tags$li(HTML("<em><b>Richness:</b></em> Is the comment rather simplistic and limited or does it provide much information?")),
      #         tags$li(HTML("<em><b>Analytic content:</b></em> Is the comment mostly opinion, advocacy, or anecdote rather than providing arguments or , factual and checkable claims (e.g., data, studies, mechanisms, legal/technical details)?")),
      #         tags$li(HTML("<em><b>Specificity</b></em> Is the comment rather general and unspecific or does it provide information concrete and detailed enough to be directly useful for legal policymaking?")),
      # ),
      # p(HTML("There are no clear right or wrong answers - <b>your interpretation matters!</b>")),
      verbatimTextOutput("status"),
      p(HTML("Your assessments are saved on every text example. <b>You can close and later reload the app whenever you need a break.</b> Many thanks for your help!")),
      p(HTML("We are happy to provide more information about the research context once the coding is complete. If you have any further questions, please do not hesitate to contact us via <a href=\"mailto:rauh@wzb.eu\">Email</a>."))
    ),
    
    mainPanel(
      HTML("<br>"),
      h3("The excerpt from the stakeholder comment"),
      div(
        id = "text-frame",
        uiOutput("current_text")
      ),
      HTML("<br>"),
      
      h3("Your assessments"),
      HTML("<br>"),
      
      # Classification radio buttons and next button placed together
      div(
        # First set of radio buttons
        radioButtons("classification1", 
                     HTML("<b>Low or high <u>relevance</u>?</b>&nbsp;&nbsp;&nbsp;<span style=\"color: #bbb;\">Comments may appear as vague or off-topic or they can directly address a regulatory issue</span>"),
                     choices = c("Very low", "Rather low", "Moderate", "Rather high", "Very high"), 
                     inline = TRUE,
                     selected = character(0)),
        
        # Second set of radio buttons
        radioButtons("classification2", 
                     HTML("<b>Low or high <u>information richness</u>?</b>&nbsp;&nbsp;&nbsp;<span style=\"color: #bbb;\">Comments may be rather simplistic and limited or they provide a lot of information</span>"),
                     choices = c("Very low", "Rather low", "Moderate", "Rather high", "Very high"), 
                     inline = TRUE,
                     selected = character(0)),
        
        # Third set of radio buttons
        radioButtons("classification3", 
                     HTML("<b>Low or high <u>analytic quality</u>?</b>&nbsp;&nbsp;&nbsp;<span style=\"color: #bbb;\">Comments may be mostly opinion and anecdote or they can provide clear arguments and checkable claims</span>"),
                     choices = c("Very low", "Rather low", "Moderate", "Rather high", "Very high"), 
                     inline = TRUE,
                     selected = character(0)),
        
        # Fourth set of radio buttons
        radioButtons("classification4", 
                     HTML("<b>Low or high <u>specificity</u>?</b>&nbsp;&nbsp;&nbsp;<span style=\"color: #bbb;\">Comments may be rather general and unspecific or provide concrete and detailed information for policymaking</span>"),
                     choices = c("Very low", "Rather low", "Moderate", "Rather high", "Very high"), 
                     inline = TRUE,
                     selected = character(0)),
        
        # Fifth set of radio buttons
        radioButtons("classification5", 
                     HTML("<b>Low or high <u>information quality overall</u>?</b>&nbsp;&nbsp;&nbsp;<span style=\"color: #bbb;\">Comments may be hardly or very useful an official trying to make evidence-based decisions</span>"),
                     choices = c("Very low", "Rather low", "Moderate", "Rather high", "Very high"), 
                     inline = TRUE,
                     selected = character(0)),
        
        # Continue button
        actionButton("next_btn", "Continue", style = "margin-top: 10px; background-color: #4CAF50; color: white;"),
        HTML("<br>"),
        HTML("<br>"),
        HTML("<br>")
      ),
      
      # Add custom CSS for the text frame
      tags$style(HTML("  
        #text-frame {
          border: 2px solid darkred;       /* Border around the text */
          border-radius: 5px;              /* Rounded corners */
          padding: 15px;                   /* Spacing inside the frame */
          background-color: white;         /* Light background color */
          margin-top: 10px;                /* Space above the frame */
          margin-bottom: 10px;             /* Space below the frame */
          font-size: 18px;                 /* Adjust text size */
          color: #333;                     /* Text color */
        }

        /* Style the question label */
        .shiny-input-container > label {
          font-size: 16px;        /* Larger font size for the question label */
          font-weight: bold;      /* Bold text */
          margin-bottom: 10px;    /* Space below the question label */
        }

        /* Style the radio button texts */
        .shiny-options-group {
          font-size: 16px;        /* Slightly smaller font size */
          font-style: italic;     /* Make radio button texts italic */
        }
      "))
    )
  )
)

server <- function(input, output, session) {
  # Load the text data from a local file only once at the start
  coder <- 3
  textfile <- paste0("coder", coder, "texts.csv")
  text_data <- read.csv(textfile, stringsAsFactors = FALSE)
  
  # Add columns for each classification question if not already present
  if (!"classification1" %in% colnames(text_data)) text_data$classification1 <- NA
  if (!"classification2" %in% colnames(text_data)) text_data$classification2 <- NA
  if (!"classification3" %in% colnames(text_data)) text_data$classification3 <- NA
  if (!"classification4" %in% colnames(text_data)) text_data$classification4 <- NA
  if (!"classification5" %in% colnames(text_data)) text_data$classification5 <- NA
  
  # Reactive value to keep track of the current text index
  rv <- reactiveValues(index = 1, data = text_data)
  
  # Function to skip already classified rows
  skip_to_next_unclassified <- function() {
    while (rv$index <= nrow(rv$data) &&
           !is.na(rv$data$classification1[rv$index]) &&
           !is.na(rv$data$classification2[rv$index]) &&
           !is.na(rv$data$classification3[rv$index]) &&
           !is.na(rv$data$classification4[rv$index]) &&
           !is.na(rv$data$classification5[rv$index])) {
      rv$index <- rv$index + 1
    }
  }
  
  # Skip already classified rows on app initialization
  observeEvent(rv$index, {
    skip_to_next_unclassified()
  }, ignoreInit = FALSE)
  
  # Display the current text bit
  output$current_text <- renderUI({
    if (rv$index <= nrow(rv$data)) {
      HTML(rv$data$text[rv$index])
    } else {
      HTML("<b>All classifications completed. Thank you very much!</b> <br>You can close the app now.")
    }
  })
  
  # Display progress dynamically
  output$status <- renderText({
    paste0("Your task progress: ", sum(!is.na(rv$data$classification1) &
                                         !is.na(rv$data$classification2) &
                                         !is.na(rv$data$classification3) &
                                         !is.na(rv$data$classification4) &
                                         !is.na(rv$data$classification5)
                                       ), 
           "/", nrow(rv$data), " (", 
           round((sum(!is.na(rv$data$classification1) &
                        !is.na(rv$data$classification2) &
                        !is.na(rv$data$classification3) &
                        !is.na(rv$data$classification4) &
                        !is.na(rv$data$classification5)
                      ) / nrow(rv$data)) * 100, 0), "%)")
  })
  
  # Save classification and move to the next text when 'Next' is clicked
  observeEvent(input$next_btn, {
    if (rv$index <= nrow(rv$data)) {
      if (!is.null(input$classification1) && !is.null(input$classification2) && !is.null(input$classification3)) {
        # Save the classification directly in the in-memory data object
        rv$data$classification1[rv$index] <- input$classification1
        rv$data$classification2[rv$index] <- input$classification2
        rv$data$classification3[rv$index] <- input$classification3
        rv$data$classification4[rv$index] <- input$classification4
        rv$data$classification5[rv$index] <- input$classification5
        
        # Write the updated data to the file immediately
        write.csv(rv$data, file = textfile, row.names = FALSE)
        
        # Reset the selection
        updateRadioButtons(session, "classification1", selected = character(0))
        updateRadioButtons(session, "classification2", selected = character(0))
        updateRadioButtons(session, "classification3", selected = character(0))
        updateRadioButtons(session, "classification4", selected = character(0))
        updateRadioButtons(session, "classification5", selected = character(0))
        
        # Move to the next text
        rv$index <- rv$index + 1
        
        # Skip already classified rows
        skip_to_next_unclassified()
      } else {
        showNotification("Please answer all questions before moving to the next text.", type = "error")
      }
    } else {
      showNotification("No more texts to classify. You're done! Thanks!", type = "warning")
    }
  })
}

shinyApp(ui, server)
