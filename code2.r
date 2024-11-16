# daily_shiny_app.R

# 设置代理
Sys.setenv(http_proxy = "http://127.0.0.1:7890")
Sys.setenv(https_proxy = "http://127.0.0.1:7890")

# 加载必要的包
library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(shinycssloaders)

# 设置结果文件路径
results_path <- ""

# 函数：读取结果文件并提取日期
get_result_dates <- function(path) {
  files <- list.files(path, pattern = "^apikey_channelid_response_\\d{4}-\\d{2}-\\d{2}\\.txt$", full.names = FALSE)
  dates <- str_match(files, "^apikey_channelid_response_(\\d{4}-\\d{2}-\\d{2})\\.txt$")[,2]
  dates <- dates[!is.na(dates)]
  dates <- sort(unique(dates), decreasing = TRUE)
  return(dates)
}

# 函数：读取单个结果文件
read_single_result_file <- function(file_full_path) {
  tryCatch({
    read.table(file_full_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("读取文件失败：", file_full_path, "，错误：", e$message)
    return(NULL)
  })
}

# Shiny UI
ui <- fluidPage(
  tags$head(
    # 引入Bootstrap CSS
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css")
  ),
  tags$style(HTML("
    .container {
      margin-top: 30px;
    }
    .filter-section {
      margin-bottom: 20px;
    }
  ")),
  div(class = "container",
      h2(class = "mb-4", "渠道模型测试结果管理系统"),
      div(class = "filter-section",
          div(class = "row",
              div(class = "col-md-4",
                  tags$label("按日期筛选：", `for` = "dateFilter", class = "form-label"),
                  selectInput("dateFilter", label = NULL, choices = NULL, selected = NULL)
              )
          )
      ),
      DTOutput("resultTable") %>% withSpinner()
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  # 获取所有日期
  dates <- reactive({
    get_result_dates(results_path)
  })
  
  # 初始化日期筛选下拉菜单
  observe({
    available_dates <- dates()
    choices <- c("所有时间", available_dates)
    
    # 默认选择最新日期
    if (length(available_dates) > 0) {
      selected_date <- "所有时间"
    } else {
      selected_date <- NULL
    }
    
    updateSelectInput(session, "dateFilter",
                      choices = choices,
                      selected = selected_date)
  })
  
  # 读取并合并结果数据
  all_results <- reactive({
    selected_date <- input$dateFilter
    if (is.null(selected_date) || selected_date == "") {
      return(NULL)
    }
    
    data <- NULL
    
    if (selected_date == "所有时间") {
      dates_selected <- dates()
      if (length(dates_selected) == 0) {
        return(NULL)
      }
      
      for (date in dates_selected) {
        file_name <- paste0("apikey_channelid_response_", date, ".txt")
        file_full_path <- file.path(results_path, file_name)
        
        if (file.exists(file_full_path)) {
          temp_data <- read_single_result_file(file_full_path)
          if (!is.null(temp_data)) {
            temp_data$Date <- date  # 添加日期列
            data <- bind_rows(data, temp_data)
          }
        }
      }
    } else {
      # 构建文件名
      file_name <- paste0("apikey_channelid_response_", selected_date, ".txt")
      file_full_path <- file.path(results_path, file_name)
      
      if (file.exists(file_full_path)) {
        data <- read_single_result_file(file_full_path)
        if (!is.null(data)) {
          data$Date <- selected_date  # 添加日期列
        }
      }
    }
    
    if (is.null(data)) {
      return(NULL)
    }
    
    # 根据需求进行去重或其他处理
    # 例如，去重基于id和model的组合
    data <- data %>%
      distinct(id, successful_models, .keep_all = TRUE)
    
    return(data)
  })
  
  # 渲染数据表
  output$resultTable <- renderDT({
    data <- all_results()
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame("消息" = "没有找到相关数据"),
                       options = list(dom = 't')))
    }
    
    display_data <- data %>%
      mutate(
        `测试日期` = Date,
        `渠道ID` = id,
        `类型` = type,
        `API Key` = key,
        `基础URL` = base_url,
        `模型` = models,
        `成功模型` = successful_models,
        `来源URL` = source_url,
        `状态` = ifelse(successful_models != "", 
                      '<span class="badge bg-success">成功</span>', 
                      '<span class="badge bg-danger">失败</span>')
      ) %>%
      select(`测试日期`, `渠道ID`, `类型`, `API Key`, `基础URL`, `模型`, `成功模型`, `来源URL`, `状态`)
    
    datatable(display_data, 
              escape = FALSE, 
              options = list(
                pageLength = 10,
                autoWidth = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                language = list(
                  url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Chinese.json'
                )
              ),
              rownames = FALSE
    )
  }, server = FALSE)
}

# 运行 Shiny App 并自动在默认浏览器中打开
runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
