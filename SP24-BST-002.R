# developer : Ahmad Fayyaz
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(wordcloud)
library(RColorBrewer)
library(scales)
library(rpart)       
library(rpart.plot)  


# Helpers

parse_mdy <- function(x) as.Date(x, format = "%m/%d/%Y")
safe_numeric <- function(x) suppressWarnings(as.numeric(x))
coalesce_chr <- function(x, replacement = "Unknown") {
  x <- as.character(x)
  x[is.na(x) | trimws(x) == ""] <- replacement
  x
}

lump_top_n <- function(x, n = 12, other = "Other") {
  x <- coalesce_chr(x, replacement = other)
  tab <- sort(table(x), decreasing = TRUE)
  keep <- names(tab)[seq_len(min(n, length(tab)))]
  ifelse(x %in% keep, x, other)
}

required_cols <- c(
  "job_id","job_title","salary_usd","salary_currency","salary_local",
  "experience_level","employment_type","company_location","company_size",
  "employee_residence","remote_ratio","required_skills","education_required",
  "years_experience","industry","posting_date","application_deadline",
  "job_description_length","benefits_score","company_name"
)

make_sankey <- function(df, from_col, to_col, value_col = "n") {
  df <- df %>% filter(!is.na(.data[[from_col]]), !is.na(.data[[to_col]]))
  nodes <- unique(c(df[[from_col]], df[[to_col]]))
  nodes_df <- data.frame(name = nodes, stringsAsFactors = FALSE)
  links <- df %>% transmute(
    source = match(.data[[from_col]], nodes_df$name) - 1,
    target = match(.data[[to_col]], nodes_df$name) - 1,
    value = .data[[value_col]]
  )
  list(nodes = nodes_df, links = links)
}


# UI

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "AI Job Analytics Dashboard"),
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Salary Explorer", tabName = "salary", icon = icon("dollar-sign")),
      menuItem("Remote & Location", tabName = "remote", icon = icon("globe")),
      menuItem("Skills & Demand", tabName = "skills", icon = icon("brain")),
      menuItem("Experience & Education", tabName = "experience", icon = icon("graduation-cap")),
      menuItem("Timeline", tabName = "timeline", icon = icon("calendar-alt")),
      menuItem("Salary Forecast", tabName = "forecast", icon = icon("chart-line")), # NEW 
      menuItem("Job ML Classifier", tabName = "ml", icon = icon("robot")),          # NEW 
      menuItem("Company & Industry", tabName = "company", icon = icon("building")),
      menuItem("Job Browser", tabName = "browser", icon = icon("table")),
      menuItem("My Profile", tabName = "profile", icon = icon("user")),
      menuItem("Video Tutorial", tabName = "video", icon = icon("video"))
    ),
    hr(),
    h4("Data"),
    fileInput("csv_file", "Upload ai_job_dataset1.csv", accept = c(".csv")),
    helpText("Or keep ai_job_dataset1.csv next to app.R (auto-load)."),
    hr(),
    h4("Global filters"),
    uiOutput("filters_ui")
  ),
  dashboardBody(
    tags$head(tags$style(HTML(" 
      .content-wrapper, .right-side { background-color: #f4f6f9; } 
      .box { border-radius: 10px; } 
      .small-box { border-radius: 10px; } 
    "))),
    tabItems(
      #---------------- Overview ----------------
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_jobs", width = 3),
          valueBoxOutput("vb_salary", width = 3),
          valueBoxOutput("vb_remote", width = 3),
          valueBoxOutput("vb_benefits", width = 3)
        ),
        fluidRow(
          box(
            title = "Salary distribution (density)", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("p_salary_density", height = "320px")
          ),
          box(
            title = "Salary vs Experience ", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("p_salary_vs_years", height = "320px")
          )
        ),
        fluidRow(
          box(
            title = "Industry → Experience (Sankey)", width = 12, status = "info", solidHeader = TRUE,
            plotlyOutput("p_industry_exp_sankey", height = "420px")
          )
        )
      ),
      #---------------- Salary Explorer ----------------
      tabItem(
        tabName = "salary",
        fluidRow(
          box(
            title = "Salary by education (boxplot)", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("p_salary_edu_box", height = "340px")
          ),
          box(
            title = "Salary by experience level (violin + box)", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("p_salary_exp_violin", height = "340px")
          )
        ),
        fluidRow(
          box(
            title = "Benefits vs salary (scatter)", width = 6, status = "info", solidHeader = TRUE,
            plotlyOutput("p_benefits_vs_salary", height = "340px")
          ),
          box(
            title = "Description length vs salary (scatter)", width = 6, status = "info", solidHeader = TRUE,
            plotlyOutput("p_desc_vs_salary", height = "340px")
          )
        )
      ),
      #---------------- Remote & Location ----------------
      tabItem(
        tabName = "remote",
        fluidRow(
          box(
            title = "World map: jobs by company location", width = 7, status = "primary", solidHeader = TRUE,
            selectInput("map_metric", "Map metric", choices = c("Job count", "Average salary (USD)")),
            plotlyOutput("p_world_map", height = "420px"),
            uiOutput("map_note")
          ),
          box(
            title = "Remote ratio vs salary (jitter)", width = 5, status = "primary", solidHeader = TRUE,
            plotlyOutput("p_remote_vs_salary", height = "420px")
          )
        ),
        fluidRow(
          box(
            title = "Experience → Remote type (Sankey)", width = 12, status = "info", solidHeader = TRUE,
            plotlyOutput("p_exp_remote_sankey", height = "420px")
          )
        )
      ),
      #---------------- Skills & Demand ----------------
      tabItem(
        tabName = "skills",
        fluidRow(
          box(
            title = "Skills word cloud (wordcloud package)", width = 6, status = "primary", solidHeader = TRUE,
            plotOutput("p_skills_cloud", height = "380px")
          ),
          box(
            title = "Skills bubble chart (freq vs avg salary)", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("p_skills_bubbles", height = "380px")
          )
        )
      ),
      #---------------- Experience & Education ----------------
      tabItem(
        tabName = "experience",
        fluidRow(
          box(
            title = "Years of experience distribution (density)", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("p_years_density", height = "340px")
          ),
          box(
            title = "Years experience by education (violin)", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("p_years_edu_violin", height = "340px")
          )
        ),
        fluidRow(
          box(
            title = "Education → Experience (Sankey)", width = 12, status = "info", solidHeader = TRUE,
            plotlyOutput("p_edu_exp_sankey", height = "420px")
          )
        )
      ),
      #---------------- Timeline ----------------
      tabItem(
        tabName = "timeline",
        fluidRow(
          box(
            title = "Postings over time (line)", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("p_postings_time", height = "340px")
          ),
          box(
            title = "Average salary over time (line)", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("p_salary_time", height = "340px")
          )
        ),
        fluidRow(
          box(
            title = "Posting date vs deadline (scatter)", width = 12, status = "info", solidHeader = TRUE,
            plotlyOutput("p_post_vs_deadline", height = "340px")
          )
        )
      ),
      #---------------- Forecast Tab ---------------- 
      tabItem(
        tabName = "forecast",
        fluidRow(
          box(
            title = "6-Month Salary Trend Forecast", width = 12, status = "warning", solidHeader = TRUE,
            helpText("Forecasting future salary trends based on historical monthly averages."),
            plotlyOutput("p_salary_forecast", height = "450px")
          )
        )
      ),
      #----------------  ML Classification Tab ---------------- 
      tabItem(
        tabName = "ml",
        fluidRow(
          box(
            title = "Predict Job Level (ML Classifier)", width = 4, status = "danger", solidHeader = TRUE,
            numericInput("ml_salary", "Annual Salary (USD):", value = 75000, min = 10000),
            numericInput("ml_years", "Years of Experience:", value = 2, min = 0),
            actionButton("run_ml", "Classify Job Level", icon = icon("play")),
            hr(),
            h3(textOutput("ml_result"), align = "center")
          ),
          box(
            title = "Model Decision Logic", width = 8, status = "danger",
            plotOutput("p_ml_tree", height = "400px"),
            helpText("Decision Tree showing how the AI classifies experience levels based on Salary and Experience.")
          )
        )
      ),
      #---------------- Company & Industry ----------------
      tabItem(
        tabName = "company",
        fluidRow(
          box(
            title = "Industry / Company Size / Experience (Parallel Categories)", width = 7, status = "primary", solidHeader = TRUE,
            plotlyOutput("p_parcats", height = "420px")
          ),
          box(
            title = "Company size vs salary (boxplot)", width = 5, status = "primary", solidHeader = TRUE,
            plotlyOutput("p_company_size_box", height = "420px")
          )
        ),
        fluidRow(
          box(
            title = "Company salary vs benefits (bubble, companies with >= 3 jobs)", width = 12, status = "info", solidHeader = TRUE,
            plotlyOutput("p_company_salary_benefits", height = "360px")
          )
        )
      ),
      #---------------- Job Browser ----------------
      tabItem(
        tabName = "browser",
        fluidRow(
          box(
            title = "Filtered jobs", width = 12, status = "primary", solidHeader = TRUE,
            DTOutput("jobs_table"), br(), downloadButton("download_filtered", "Download filtered CSV")
          )
        )
      ),
      #---------------- My Profile ----------------
      tabItem(
        tabName = "profile",
        fluidRow(
          box(
            title = "My Profile", width = 6, status = "primary", solidHeader = TRUE,
            tags$img(
              src = "profile.jpg", height = "180px", width = "180px",
              style = "display:block;margin-left:auto;margin-right:auto;border-radius:50%;"
            ),
            br(), h3("Ahmad Fayyaz", align = "center"), h4("BS Statistics (Data Science)", align = "center"), hr(),
            tags$p(tags$b("Age: "), "21"), tags$p(tags$b("Date of Birth: "), "1st October"),
            tags$p(tags$b("Religious Education: "), "Hafiz-e-Quran"), hr(),
            h4("Education"), tags$ul(
              tags$li("Primary Education: The Educators"),
              tags$li("Secondary Education: Government College Township"),
              tags$li("BS Statistics – Specialization in Data Science")
            )
          ),
          box(
            title = "Experience & Skills", width = 6, status = "info", solidHeader = TRUE,
            h4("Experience"), tags$ul(
              tags$li("Property Management (Clients, Vendors & Owners Coordination)"),
              tags$li("Sales Representative – Geek Tech")
            ),
            h4("Skills"), tags$ul(
              tags$li("Data Analysis & Visualization"),
              tags$li("Event Management & Society Participation"),
              tags$li("Communication & Client Handling"),
              tags$li("Table Tennis")
            )
          )
        )
      ),
      #---------------- Video Tutorial ----------------
      tabItem(
        tabName = "video",
        fluidRow(
          box(
            title = "Dashboard Video Tutorial", width = 12, status = "primary", solidHeader = TRUE,
            tags$video(
              src = "tutorial.mp4", type = "video/mp4", controls = TRUE, width = "100%",
              height = "500px", style = "border-radius:10px;"
            ),
            br(), tags$p(strong("Note: "), "This tutorial video is stored locally inside the www folder.")
          )
        )
      )
    )
  )
)


# Server

server <- function(input, output, session) {
  # Load raw data
  raw_data <- reactive({
    local_path <- "ai_job_dataset1.csv"
    if (!is.null(input$csv_file) && nzchar(input$csv_file$datapath)) {
      return(read.csv(input$csv_file$datapath, stringsAsFactors = FALSE))
    }
    if (file.exists(local_path)) {
      return(read.csv(local_path, stringsAsFactors = FALSE))
    }
    NULL
  })
  
  # Validate + clean
  jobs <- reactive({
    df <- raw_data()
    validate(need(!is.null(df), "Upload the CSV or place ai_job_dataset1.csv in the app folder."))
    missing <- setdiff(required_cols, names(df))
    validate(need(length(missing) == 0, paste("Missing required columns:", paste(missing, collapse = ", "))))
    
    df$posting_date <- parse_mdy(df$posting_date)
    df$application_deadline <- parse_mdy(df$application_deadline)
    df$salary_usd <- safe_numeric(df$salary_usd)
    df$remote_ratio <- safe_numeric(df$remote_ratio)
    df$benefits_score <- safe_numeric(df$benefits_score)
    df$years_experience <- safe_numeric(df$years_experience)
    df$job_description_length <- safe_numeric(df$job_description_length)
    
    df %>% mutate(
      job_title = coalesce_chr(job_title),
      experience_level = coalesce_chr(experience_level),
      employment_type = coalesce_chr(employment_type),
      company_location = coalesce_chr(company_location),
      company_size = coalesce_chr(company_size),
      industry = coalesce_chr(industry),
      education_required = coalesce_chr(education_required),
      company_name = coalesce_chr(company_name),
      required_skills = coalesce_chr(required_skills, replacement = ""),
      remote_category = case_when(
        remote_ratio == 0 ~ "On-site",
        remote_ratio == 50 ~ "Hybrid",
        remote_ratio == 100 ~ "Fully Remote",
        TRUE ~ "Mixed"
      ),
      posting_month = ifelse(is.na(posting_date), NA_character_, format(posting_date, "%Y-%m"))
    )
  })
  
  # Dynamic filter UI
  output$filters_ui <- renderUI({
    df <- jobs()
    salary_min <- floor(min(df$salary_usd, na.rm = TRUE) / 10000) * 10000
    salary_max <- ceiling(max(df$salary_usd, na.rm = TRUE) / 10000) * 10000
    if (!is.finite(salary_min)) salary_min <- 0
    if (!is.finite(salary_max)) salary_max <- 500000
    
    tagList(
      selectizeInput("f_job_title", "Job title", choices = sort(unique(df$job_title)), multiple = TRUE, options = list(placeholder = "All job titles")),
      selectizeInput("f_exp", "Experience level", choices = sort(unique(df$experience_level)), multiple = TRUE, options = list(placeholder = "All levels")),
      selectizeInput("f_country", "Company location", choices = sort(unique(df$company_location)), multiple = TRUE, options = list(placeholder = "All locations")),
      selectizeInput("f_industry", "Industry", choices = sort(unique(df$industry)), multiple = TRUE, options = list(placeholder = "All industries")),
      selectizeInput("f_remote_cat", "Remote type", choices = sort(unique(df$remote_category)), multiple = TRUE, options = list(placeholder = "All remote types")),
      sliderInput("f_salary", "Salary (USD)", min = salary_min, max = salary_max, value = c(salary_min, salary_max), step = 5000, pre = "$"),
      sliderInput("f_remote", "Remote ratio (%)", min = 0, max = 100, value = c(0, 100), step = 25),
      dateRangeInput("f_posting", "Posting date range", start = min(df$posting_date, na.rm = TRUE), end = max(df$posting_date, na.rm = TRUE))
    )
  })
  
  # Apply filters
  filtered <- reactive({
    df <- jobs()
    req(input$f_salary, input$f_remote, input$f_posting)
    
    if (!is.null(input$f_job_title) && length(input$f_job_title) > 0) df <- df %>% filter(job_title %in% input$f_job_title)
    if (!is.null(input$f_exp) && length(input$f_exp) > 0) df <- df %>% filter(experience_level %in% input$f_exp)
    if (!is.null(input$f_country) && length(input$f_country) > 0) df <- df %>% filter(company_location %in% input$f_country)
    if (!is.null(input$f_industry) && length(input$f_industry) > 0) df <- df %>% filter(industry %in% input$f_industry)
    if (!is.null(input$f_remote_cat) && length(input$f_remote_cat) > 0) df <- df %>% filter(remote_category %in% input$f_remote_cat)
    
    df <- df %>% filter(
      !is.na(salary_usd), salary_usd >= input$f_salary[1], salary_usd <= input$f_salary[2],
      !is.na(remote_ratio), remote_ratio >= input$f_remote[1], remote_ratio <= input$f_remote[2],
      !is.na(posting_date), posting_date >= input$f_posting[1], posting_date <= input$f_posting[2]
    )
    validate(need(nrow(df) > 0, "No rows after filtering. Widen filters and try again."))
    df
  })
  
  #---------------- Value boxes ----------------
  output$vb_jobs <- renderValueBox({
    df <- filtered()
    valueBox(formatC(nrow(df), format = "d", big.mark = ","), "Jobs (filtered)", icon = icon("briefcase"), color = "purple")
  })
  output$vb_salary <- renderValueBox({
    df <- filtered()
    valueBox(paste0("$", format(round(mean(df$salary_usd, na.rm = TRUE)), big.mark = ",")), "Average salary (USD)", icon = icon("money-bill-wave"), color = "green")
  })
  output$vb_remote <- renderValueBox({
    df <- filtered()
    valueBox(paste0(round(mean(df$remote_ratio, na.rm = TRUE), 1), "%"), "Average remote ratio", icon = icon("home"), color = "yellow")
  })
  output$vb_benefits <- renderValueBox({
    df <- filtered()
    valueBox(round(median(df$benefits_score, na.rm = TRUE), 1), "Median benefits score", icon = icon("gift"), color = "blue")
  })
  
  #---------------- Overview plots ----------------
  output$p_salary_density <- renderPlotly({
    df <- filtered()
    p <- ggplot(df, aes(x = salary_usd, fill = experience_level)) +
      geom_density(alpha = 0.55) + scale_x_continuous(labels = comma) +
      labs(x = "Salary (USD)", y = "Density", fill = "Experience") + theme_minimal()
    ggplotly(p)
  })
  
  output$p_salary_vs_years <- renderPlotly({
    df <- filtered() %>% filter(!is.na(years_experience), !is.na(salary_usd))
    cap <- quantile(df$salary_usd, 0.99, na.rm = TRUE)
    df <- df %>% filter(salary_usd <= cap)
    if (nrow(df) > 5000) {
      set.seed(123)
      df <- df %>% sample_n(5000)
    }
    p <- ggplot(df, aes(x = years_experience, y = salary_usd)) +
      geom_point(color = "#4DA3FF", alpha = 0.25, size = 1.5) +
      geom_smooth(method = "loess", se = TRUE, color = "#0B5ED7", fill = "#9EC5FE", linewidth = 1.2) +
      scale_y_continuous(labels = comma) +
      labs(title = "Salary vs Experience", x = "Years of Experience", y = "Salary (USD)") +
      theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$p_industry_exp_sankey <- renderPlotly({
    df <- filtered() %>%
      mutate(industry_top = lump_top_n(industry, n = 12, other = "Other"), exp = coalesce_chr(experience_level, "Unknown")) %>%
      count(industry_top, exp, name = "n") %>% filter(n > 0)
    sk <- make_sankey(df, "industry_top", "exp", "n")
    plot_ly(type = "sankey", arrangement = "snap", node = list(label = sk$nodes$name, pad = 15, thickness = 12),
            link = list(source = sk$links$source, target = sk$links$target, value = sk$links$value))
  })
  
  #---------------- Salary Explorer ----------------
  output$p_salary_edu_box <- renderPlotly({
    df <- filtered()
    p <- ggplot(df, aes(x = education_required, y = salary_usd, fill = education_required)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.25) + scale_y_continuous(labels = comma) +
      labs(x = "Education required", y = "Salary (USD)", fill = "Education") + theme_minimal()
    ggplotly(p)
  })
  
  output$p_salary_exp_violin <- renderPlotly({
    df <- filtered()
    p <- ggplot(df, aes(x = experience_level, y = salary_usd, fill = experience_level)) +
      geom_violin(alpha = 0.7, trim = FALSE) + geom_boxplot(width = 0.12, outlier.alpha = 0.2) +
      scale_y_continuous(labels = comma) + labs(x = "Experience level", y = "Salary (USD)", fill = "Experience") + theme_minimal()
    ggplotly(p)
  })
  
  output$p_benefits_vs_salary <- renderPlotly({
    df <- filtered()
    p <- ggplot(df, aes(x = benefits_score, y = salary_usd, color = industry, text = paste(company_name, job_title, sep = " • "))) +
      geom_point(alpha = 0.65) + scale_y_continuous(labels = comma) +
      labs(x = "Benefits score", y = "Salary (USD)", color = "Industry") + theme_minimal()
    ggplotly(p, tooltip = c("text", "x", "y", "color"))
  })
  
  output$p_desc_vs_salary <- renderPlotly({
    df <- filtered()
    p <- ggplot(df, aes(x = job_description_length, y = salary_usd, color = experience_level)) +
      geom_point(alpha = 0.6) + geom_smooth(method = "loess", se = FALSE, color = "black") +
      scale_y_continuous(labels = comma) + labs(x = "Job description length", y = "Salary (USD)", color = "Experience") + theme_minimal()
    ggplotly(p)
  })
  
  #---------------- Remote & Location (Map + Sankey) ----------------
  map_data <- reactive({
    df <- filtered()
    if (identical(input$map_metric, "Average salary (USD)")) {
      df %>% group_by(company_location) %>% summarise(z = mean(salary_usd, na.rm = TRUE), n = n(), .groups = "drop")
    } else {
      df %>% group_by(company_location) %>% summarise(z = n(), n = n(), .groups = "drop")
    }
  })
  
  output$p_world_map <- renderPlotly({
    md <- map_data()
    validate(need(nrow(md) > 0, "No map data for current filters."))
    plot_ly(data = md, type = "choropleth", locations = ~company_location, locationmode = "country names", z = ~z,
            text = ~paste(company_location, "<br>Value:", round(z, 2), "<br>Jobs:", n), colorscale = "Viridis", colorbar = list(title = "")) %>%
      layout(geo = list(projection = list(type = "natural earth"), showframe = FALSE, showcountries = TRUE), margin = list(l = 0, r = 0, t = 0, b = 0))
  })
  
  output$map_note <- renderUI({
    tags$small("If some countries don’t color on the map, they may not match Plotly’s country-name list. Try filtering or renaming those values in the CSV.")
  })
  
  output$p_remote_vs_salary <- renderPlotly({
    df <- filtered()
    p <- ggplot(df, aes(x = remote_ratio, y = salary_usd, color = remote_category)) +
      geom_jitter(alpha = 0.6, width = 3) + scale_y_continuous(labels = comma) +
      labs(x = "Remote ratio (%)", y = "Salary (USD)", color = "Remote type") + theme_minimal()
    ggplotly(p)
  })
  
  output$p_exp_remote_sankey <- renderPlotly({
    df <- filtered() %>% count(experience_level, remote_category, name = "n") %>% filter(n > 0)
    sk <- make_sankey(df, "experience_level", "remote_category", "n")
    plot_ly(type = "sankey", arrangement = "snap", node = list(label = sk$nodes$name, pad = 15, thickness = 12),
            link = list(source = sk$links$source, target = sk$links$target, value = sk$links$value))
  })
  
  #---------------- Skills & Demand ----------------
  skills_long <- reactive({
    df <- filtered() %>% select(job_id, salary_usd, required_skills) %>%
      mutate(required_skills = coalesce_chr(required_skills, replacement = "")) %>%
      separate_rows(required_skills, sep = ",") %>% mutate(required_skills = str_squish(required_skills)) %>%
      filter(required_skills != "")
    validate(need(nrow(df) > 0, "No skills available for the current filters."))
    df
  })
  
  skills_summary <- reactive({
    sl <- skills_long()
    sl %>% group_by(required_skills) %>% summarise(freq = n(), avg_salary = mean(salary_usd, na.rm = TRUE), .groups = "drop") %>% arrange(desc(freq))
  })
  
  output$p_skills_cloud <- renderPlot({
    ss <- skills_summary() %>% slice_head(n = 120)
    pal <- brewer.pal(8, "Dark2")
    par(mar = c(0, 0, 0, 0), bg = "white")
    wordcloud(words = ss$required_skills, freq = ss$freq, scale = c(4, 0.8), random.order = FALSE, colors = pal)
  })
  
  output$p_skills_bubbles <- renderPlotly({
    ss <- skills_summary() %>% slice_head(n = 60)
    p <- ggplot(ss, aes(x = avg_salary, y = freq, size = freq, color = avg_salary, label = required_skills)) +
      geom_point(alpha = 0.85) + scale_x_continuous(labels = comma) + scale_size_continuous(range = c(6, 20)) +
      scale_color_viridis_c(option = "C") + labs(x = "Average salary (USD)", y = "Frequency", size = "Frequency") + theme_minimal()
    ggplotly(p, tooltip = c("label", "x", "y"))
  })
  
  #---------------- Experience & Education ----------------
  output$p_years_density <- renderPlotly({
    df <- filtered()
    p <- ggplot(df, aes(x = years_experience, fill = experience_level)) +
      geom_density(alpha = 0.55) + labs(x = "Years of experience", y = "Density", fill = "Experience") + theme_minimal()
    ggplotly(p)
  })
  
  output$p_years_edu_violin <- renderPlotly({
    df <- filtered()
    p <- ggplot(df, aes(x = education_required, y = years_experience, fill = education_required)) +
      geom_violin(alpha = 0.7, trim = FALSE) + geom_boxplot(width = 0.12, outlier.alpha = 0.2) +
      labs(x = "Education required", y = "Years of experience", fill = "Education") + theme_minimal()
    ggplotly(p)
  })
  
  output$p_edu_exp_sankey <- renderPlotly({
    df <- filtered() %>% count(education_required, experience_level, name = "n") %>% filter(n > 0)
    sk <- make_sankey(df, "education_required", "experience_level", "n")
    plot_ly(type = "sankey", arrangement = "snap", node = list(label = sk$nodes$name, pad = 15, thickness = 12),
            link = list(source = sk$links$source, target = sk$links$target, value = sk$links$value))
  })
  
  #---------------- Timeline ----------------
  time_summary <- reactive({
    df <- filtered() %>% filter(!is.na(posting_month)) %>%
      group_by(posting_month) %>% summarise(postings = n(), avg_salary = mean(salary_usd, na.rm = TRUE), .groups = "drop") %>%
      arrange(posting_month)
    validate(need(nrow(df) > 0, "No timeline data for the current filters."))
    df
  })
  
  output$p_postings_time <- renderPlotly({
    ts <- time_summary()
    p <- ggplot(ts, aes(x = posting_month, y = postings, group = 1)) +
      geom_line(color = "#4e79a7") + geom_point(color = "#4e79a7") +
      labs(x = "Posting month", y = "Number of postings") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$p_salary_time <- renderPlotly({
    ts <- time_summary()
    p <- ggplot(ts, aes(x = posting_month, y = avg_salary, group = 1)) +
      geom_line(color = "#f28e2b") + geom_point(color = "#f28e2b") +
      scale_y_continuous(labels = comma) + labs(x = "Posting month", y = "Average salary (USD)") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$p_post_vs_deadline <- renderPlotly({
    df <- filtered() %>% filter(!is.na(application_deadline))
    validate(need(nrow(df) > 0, "No deadline data available for the current filters."))
    p <- ggplot(df, aes(x = posting_date, y = application_deadline, color = experience_level, text = paste(job_title, company_name, sep = " • "))) +
      geom_point(alpha = 0.65) + labs(x = "Posting date", y = "Application deadline", color = "Experience") + theme_minimal()
    ggplotly(p, tooltip = c("text", "x", "y", "color"))
  })
  
  #---------------- Forecast Server Logic ---------------- 
  output$p_salary_forecast <- renderPlotly({
    ts <- time_summary()
    ts$month_index <- 1:nrow(ts)
    # Fit Linear Model for Forecasting
    model <- lm(avg_salary ~ month_index, data = ts)
    # Predict next 6 months
    future_index <- (max(ts$month_index) + 1):(max(ts$month_index) + 6)
    preds <- predict(model, newdata = data.frame(month_index = future_index))
    # Generate future dates for plotting
    last_date <- as.Date(paste0(max(ts$posting_month), "-01"))
    future_dates <- format(seq(last_date, by = "month", length.out = 7)[-1], "%Y-%m")
    
    forecast_df <- data.frame(posting_month = future_dates, avg_salary = preds, type = "Forecast")
    plot_df <- rbind(ts %>% mutate(type = "Actual") %>% select(posting_month, avg_salary, type), forecast_df)
    
    p <- ggplot(plot_df, aes(x = posting_month, y = avg_salary, group = 1, color = type)) +
      geom_line(aes(linetype = type), size = 1) + geom_point() +
      scale_color_manual(values = c("Actual" = "#5b2c83", "Forecast" = "#f28e2b")) +
      scale_y_continuous(labels = comma) + theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  #---------------- ML Classification Server Logic ---------------- 
  ml_model <- reactive({
    df <- jobs() %>% select(experience_level, salary_usd, years_experience) %>% na.omit()
    # Train Decision Tree model
    rpart(experience_level ~ salary_usd + years_experience, data = df, method = "class")
  })
  
  output$p_ml_tree <- renderPlot({
    rpart.plot(ml_model(), box.palette = "RdYlGn", shadow.col = "gray", nn = TRUE)
  })
  
  observeEvent(input$run_ml, {
    new_data <- data.frame(salary_usd = input$ml_salary, years_experience = input$ml_years)
    pred <- predict(ml_model(), new_data, type = "class")
    output$ml_result <- renderText({ paste("AI Prediction:", as.character(pred)) })
  })
  
  #---------------- Company & Industry ----------------
  output$p_parcats <- renderPlotly({
    df <- filtered() %>%
      mutate(industry_top = lump_top_n(industry, n = 10, other = "Other"), company_size = coalesce_chr(company_size, "Unknown"), experience_level = coalesce_chr(experience_level, "Unknown"))
    plot_ly(type = "parcats", dimensions = list(list(label = "Industry", values = df$industry_top), list(label = "Company size", values = df$company_size), list(label = "Experience", values = df$experience_level)), labelfont = list(size = 12), tickfont = list(size = 10))
  })
  
  output$p_company_size_box <- renderPlotly({
    df <- filtered()
    p <- ggplot(df, aes(x = company_size, y = salary_usd, fill = company_size)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.25) + scale_y_continuous(labels = comma) +
      labs(x = "Company size", y = "Salary (USD)", fill = "Company size") + theme_minimal()
    ggplotly(p)
  })
  
  output$p_company_salary_benefits <- renderPlotly({
    df <- filtered() %>% group_by(company_name) %>%
      summarise(n_jobs = n(), avg_salary = mean(salary_usd, na.rm = TRUE), avg_benefits = mean(benefits_score, na.rm = TRUE), .groups = "drop") %>%
      filter(n_jobs >= 3)
    validate(need(nrow(df) > 0, "Not enough repeated companies under current filters (need >= 3 jobs per company)."))
    p <- ggplot(df, aes(x = avg_benefits, y = avg_salary, size = n_jobs, label = company_name)) +
      geom_point(alpha = 0.8, color = "#5b2c83") + scale_y_continuous(labels = comma) +
      labs(x = "Avg benefits score", y = "Avg salary (USD)", size = "Jobs") + theme_minimal()
    ggplotly(p, tooltip = c("label", "x", "y", "size"))
  })
  
  #---------------- Job Browser ----------------
  output$jobs_table <- renderDT({
    df <- filtered() %>% select(job_id, job_title, company_name, company_location, industry, experience_level, employment_type, salary_usd, remote_ratio, education_required, years_experience, posting_date, application_deadline, benefits_score, required_skills)
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  output$download_filtered <- downloadHandler(
    filename = function() paste0("filtered_ai_jobs_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
  
  #---------------- My Profile outputs ----------------
  output$profile_preview <- renderUI({
    tags$div(
      tags$h3(coalesce_chr(input$prof_name, "Your name")),
      tags$h4(style = "color:#5b2c83;", coalesce_chr(input$prof_role, "Role")),
      tags$p(tags$b("Email: "), coalesce_chr(input$prof_email, "you@example.com")),
      tags$hr(),
      tags$p(coalesce_chr(input$prof_about, ""))
    )
  })
}

shinyApp(ui, server)