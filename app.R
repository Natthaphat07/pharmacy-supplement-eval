library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(DT)
library(shinyjs)
library(rsconnect)

# ==========================================
# 1. อ่านฐานข้อมูลอาหารเสริม
# ==========================================
data <- read_excel("pharmacy_database.xlsx")

# ==========================================
# 2. หน้าเว็บ (UI)
# ==========================================
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("ระบบเภสัชกรคัดเลือก ประเมินความปลอดภัย และเปรียบเทียบอาหารเสริม"),
  
  sidebarLayout(
    
    sidebarPanel(
      id = "side-panel",
      width = 4,
      
      h4("1. ความต้องการทั่วไป", style = "color: #2c3e50; font-weight: bold;"),
      
      # 1.1 หมวดหมู่หลัก (จัดให้ 'ผิว-ความงาม' ขึ้นก่อน)
      uiOutput("category_ui"),
      
      # 1.2 หมวดหมู่ย่อย ( Dynamic Render ตามหมวดหมู่หลัก )
      uiOutput("sub_category_ui"),
      
      numericInput("budget", "งบประมาณสูงสุด (บาท):", value = 1000, min = 0, step = 100),
      selectInput("target", "กลุ่มช่วงวัยผู้ใช้:", 
                  choices = c("ไม่ระบุ" = "all", 
                              "ผู้ใหญ่/วัยทำงาน" = "ผู้ใหญ่|วัยทำงาน|ทำงาน|ผิวแห้ง", 
                              "ผู้สูงอายุ" = "ผู้สูงอายุ|สูงวัย|ชะลอวัย", 
                              "นักศึกษา/วัยเรียน" = "นักศึกษา|วัยเรียน|เรียน")),
      
      hr(),
      
      h4("2. ซักประวัติความปลอดภัย (เภสัชกร)", style = "color: #c0392b; font-weight: bold;"),
      
      # --- 2.1 แพ้อาหาร / สารสกัด ---
      checkboxGroupInput("allergies_check", "ประวัติการแพ้อาหาร / สารสกัด:", 
                         choices = c("แพ้อาหารทะเล / ปลา" = "ปลา|อาหารทะเล|ซีฟู้ด|กุ้ง|ปู",
                                     "แพ้ถั่ว / ถั่วเหลือง" = "ถั่ว|Lecithin|Soy",
                                     "แพ้นม / แลคโตส" = "นม|Lactose|Whey",
                                     "แพ้เกสรดอกไม้ / พืช" = "เกสร|ดอกไม้|พืช")),
      textInput("allergies_text", "ระบุอาหาร/สารสกัดที่แพ้เพิ่มเติม (ใช้จุลภาค , คั่น):", 
                placeholder = "เช่น กลูเตน, ชาเขียว, ยีสต์, Gluten"),
      
      br(),
      
      # --- 2.2 แพ้ยา ---
      checkboxGroupInput("drug_allergies_check", "ประวัติการแพ้ยา:", 
                         choices = c("กลุ่มแอสไพริน / NSAIDs" = "Aspirin|NSAID|Ibuprofen",
                                     "กลุ่มยาซัลฟา (Sulfa)" = "Sulfa|Sulfonamide")),
      textInput("drug_allergies_text", "ระบุชื่อยาที่แพ้เพิ่มเติม (ใช้จุลภาค , คั่น):", 
                placeholder = "เช่น Penicillin, Amoxicillin, พารา"),
      
      br(),
      
      # --- 2.3 สถานะตั้งครรภ์ ---
      radioButtons("pregnant", "สถานะการตั้งครรภ์ / ให้นมบุตร:", 
                   choices = c("ไม่ได้ตั้งครรภ์ / ให้นมบุตร" = "no", 
                               "ตั้งครรภ์ หรือ ให้นมบุตร" = "yes"), selected = "no"),
      
      br(),
      
      # --- 2.4 โรคประจำตัว (เพิ่มช่องพิมพ์ระบุโรคเพิ่ม) ---
      checkboxGroupInput("conditions", "โรคประจำตัวสำคัญ:", 
                         choices = c("โรคลมชัก" = "ลมชัก", 
                                     "โรคตับ / โรคไต" = "ตับ|ไต", 
                                     "โรคนิ่วในไต" = "นิ่ว",
                                     "โรคไทรอยด์" = "ไทรอยด์")),
      textInput("conditions_text", "ระบุโรคประจำตัวเพิ่มเติม (ใช้จุลภาค , คั่น):", 
                placeholder = "เช่น ความดัน, ความดันโลหิตสูง, G6PD"),
      
      br(),
      
      # --- 2.5 ยาเดิมที่ใช้อยู่ ---
      checkboxGroupInput("medications_check", "ยาเดิมที่รับประทานอยู่เป็นประจำ:", 
                         choices = c("ยาละลายลิ่มเลือด (Warfarin, Aspirin)" = "ลิ่มเลือด|Aspirin|Warfarin",
                                     "ยาลดน้ำตาล / ยาเบาหวาน" = "เบาหวาน|น้ำตาล")),
      textInput("medications_text", "ระบุชื่อยาเดิมที่ใช้เพิ่มเติม (ใช้จุลภาค , คั่น):", 
                placeholder = "เช่น ยาลดความดัน, Metformin, ยาความดัน"),
      
      br(),
      
      fluidRow(
        column(8, actionButton("search", "ประเมินและคัดเลือก", class = "btn-primary btn-block", style = "font-weight:bold;")),
        column(4, actionButton("reset_btn", "ล้างค่า", class = "btn-default btn-block"))
      )
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "main_tabs",
        
        # --- TAB 1: ผลการประเมินหลัก ---
        tabPanel(
          "ผลการประเมินและการคัดเลือก",
          br(),
          h4("1. ผลการประเมินผลิตภัณฑ์ (จัดอันดับตาม Safety Status & คะแนนรวม)"),
          p("คลิกเลือกรายการในตารางอย่างน้อย 1 รายการ แล้วกดปุ่มเพื่อดูรายละเอียดหรือเปรียบเทียบ"),
          DTOutput("result_table"),
          br(),
          actionButton("go_compare", "ดูรายละเอียด / เปรียบเทียบรายการที่เลือก (Side-by-Side)", class = "btn-success"),
          
          br(), hr(),
          
          h4("2. สรุปเหตุผลข้อห้ามใช้ / คำเตือนความปลอดภัย (Excluded & Warning Reasons)"),
          DTOutput("warning_summary_table"),
          
          br(), hr(),
          
          h4("3. รายละเอียดสารสำคัญ ข้อมูลการใช้ และขนาดบรรจุ"),
          DTOutput("detail_table")
        ),
        
        # --- TAB 2: Compare Mode ---
        tabPanel(
          "Compare Mode (เปรียบเทียบสินค้า)",
          br(),
          h3("แสดงรายละเอียด / เปรียบเทียบผลิตภัณฑ์ (Side-by-Side)"),
          uiOutput("compare_ui")
        )
      )
    )
  )
)

# ==========================================
# 3. ระบบประมวลผล (Server Logic)
# ==========================================
server <- function(input, output, session) {
  
  # ฟังก์ชันช่วยหาชื่อคอลัมน์แบบยืดหยุ่น ป้องกัน Error
  get_col <- function(df, keywords, default = "") {
    for (kw in keywords) {
      match_col <- names(df)[grepl(kw, names(df), ignore.case = TRUE)]
      if (length(match_col) > 0) {
        return(as.character(df[[match_col[1]]]))
      }
    }
    return(rep(default, nrow(df)))
  }
  
  # --------------------------------------
  # Dynamic UI: สร้าง Dropdown หมวดหมู่หลัก (จัดให้ 'ผิว-ความงาม' ขึ้นก่อน)
  # --------------------------------------
  output$category_ui <- renderUI({
    cat_col <- names(data)[grepl("หมวดหมู่หลัก", names(data))][1]
    if (!is.na(cat_col)) {
      all_cats <- sort(unique(na.omit(data[[cat_col]])))
    } else {
      all_cats <- c()
    }
    
    target_cat <- "ผิว-ความงาม"
    if (target_cat %in% all_cats) {
      other_cats <- setdiff(all_cats, target_cat)
      sorted_cats <- c(target_cat, other_cats)
    } else {
      sorted_cats <- all_cats
    }
    
    selectInput("category", "เป้าหมายสุขภาพที่ต้องการดูแล (หมวดหมู่หลัก):", 
                choices = c("ทั้งหมด", sorted_cats), 
                selected = "ทั้งหมด")
  })
  
  # --------------------------------------
  # Dynamic UI: สร้าง Dropdown หมวดหมู่ย่อย
  # --------------------------------------
  output$sub_category_ui <- renderUI({
    req(input$category)
    
    cat_col <- names(data)[grepl("หมวดหมู่หลัก", names(data))][1]
    sub_col <- names(data)[grepl("หมวดหมู่ย่อย", names(data))][1]
    
    if (is.na(sub_col)) return(NULL)
    
    if (input$category == "ทั้งหมด" || is.na(cat_col)) {
      raw_subs <- data[[sub_col]]
    } else {
      raw_subs <- data[[sub_col]][data[[cat_col]] == input$category]
    }
    
    parsed_subs <- raw_subs %>%
      na.omit() %>%
      str_split(";") %>%
      unlist() %>%
      str_trim() %>%
      unique() %>%
      sort()
    
    selectInput(
      inputId = "sub_category",
      label = "ระบุเน้นเจาะจง/หมวดหมู่ย่อย (เช่น ส่วนของผิวหนัง/อาการ):",
      choices = parsed_subs,
      selected = NULL,
      multiple = TRUE,
      selectize = TRUE
    )
  })
  
  processed_data <- reactiveVal(NULL)
  
  observeEvent(input$search, {
    
    products <- data
    
    # แมปข้อมูลคอลัมน์แบบปลอดภัย
    products$`วิธีทานจริง` <- get_col(products, c("วิธีรับประทาน", "Serving"), "-")
    products$`ข้อควรระวังจริง` <- get_col(products, c("ข้อควรระวัง"), "-")
    products$`สารสำคัญจริง` <- get_col(products, c("สารสำคัญ", "ส่วนประกอบ"), "-")
    products$`ความแรงจริง` <- get_col(products, c("ความแรง", "ปริมาณ"), "")
    products$`ขนาดบรรจุจริง` <- get_col(products, c("ขนาดบรรจุ"), "-")
    products$`กลุ่มเป้าหมายจริง` <- get_col(products, c("กลุ่มเป้าหมาย"), "")
    products$`หมวดหมู่ย่อยจริง` <- get_col(products, c("หมวดหมู่ย่อย"), "")
    products$`ข้อบ่งใช้จริง` <- get_col(products, c("ประโยชน์", "สรรพคุณ", "ข้อบ่งใช้"), "-")
    products$`หมวดหมู่หลักจริง` <- get_col(products, c("หมวดหมู่หลัก"), "")
    
    price_col <- names(products)[grepl("ราคา", names(products))][1]
    if (!is.na(price_col)) {
      products$`ราคาจริง` <- as.numeric(products[[price_col]])
    } else {
      products$`ราคาจริง` <- NA
    }
    
    if (input$category != "ทั้งหมด") {
      products <- products %>% filter(`หมวดหมู่หลักจริง` == input$category)
    }
    
    if (!is.null(input$sub_category) && length(input$sub_category) > 0) {
      sub_pattern <- paste(input$sub_category, collapse = "|")
      products <- products %>% filter(grepl(sub_pattern, `หมวดหมู่ย่อยจริง`, ignore.case = TRUE))
    }
    
    products <- products %>%
      filter(is.na(`ราคาจริง`) | `ราคาจริง` <= input$budget)
    
    typed_food_allergies <- unlist(strsplit(input$allergies_text, "[,;]"))
    typed_food_allergies <- trimws(typed_food_allergies[typed_food_allergies != ""])
    all_food_patterns <- c(input$allergies_check, typed_food_allergies)
    
    typed_drug_allergies <- unlist(strsplit(input$drug_allergies_text, "[,;]"))
    typed_drug_allergies <- trimws(typed_drug_allergies[typed_drug_allergies != ""])
    all_drug_patterns <- c(input$drug_allergies_check, typed_drug_allergies)
    
    typed_conditions <- unlist(strsplit(input$conditions_text, "[,;]"))
    typed_conditions <- trimws(typed_conditions[typed_conditions != ""])
    all_condition_patterns <- c(input$conditions, typed_conditions)
    
    typed_meds <- unlist(strsplit(input$medications_text, "[,;]"))
    typed_meds <- trimws(typed_meds[typed_meds != ""])
    all_med_patterns <- c(input$medications_check, typed_meds)
    
    safety_status <- c()
    warning_reasons <- c()
    safety_penalty <- c()
    
    if (nrow(products) > 0) {
      for (i in 1:nrow(products)) {
        status <- "Pass"
        reasons <- c()
        penalty <- 0
        
        caution_text <- products$`ข้อควรระวังจริง`[i]
        ingredient_text <- products$`สารสำคัญจริง`[i]
        
        if (input$pregnant == "yes") {
          if (grepl("ครรภ์|ให้นม|เด็ก", caution_text)) {
            status <- "Danger"
            reasons <- c(reasons, "⚠️ ข้อห้ามใช้: ไม่เหมาะสำหรับสตรีมีครรภ์/ให้นมบุตร")
            penalty <- penalty + 50
          }
        }
        
        if (length(all_food_patterns) > 0) {
          food_pattern <- paste(all_food_patterns, collapse = "|")
          if (grepl(food_pattern, ingredient_text, ignore.case = TRUE) || grepl(food_pattern, caution_text, ignore.case = TRUE)) {
            status <- "Danger"
            reasons <- c(reasons, "⚠️ ข้อห้ามใช้: มีส่วนประกอบของสารหรืออาหารที่ผู้ป่วยแพ้")
            penalty <- penalty + 50
          }
        }
        
        if (length(all_drug_patterns) > 0) {
          drug_pattern <- paste(all_drug_patterns, collapse = "|")
          if (grepl(drug_pattern, caution_text, ignore.case = TRUE) || grepl(drug_pattern, ingredient_text, ignore.case = TRUE)) {
            status <- "Danger"
            reasons <- c(reasons, "⚠️ ข้อห้ามใช้: ตัวยา/สารสกัดอาจเกิดปฏิกิริยากับประวัติแพ้ยาของผู้ป่วย")
            penalty <- penalty + 50
          }
        }
        
        if (length(all_condition_patterns) > 0) {
          condition_pattern <- paste(all_condition_patterns, collapse = "|")
          if (grepl(condition_pattern, caution_text, ignore.case = TRUE)) {
            if (status != "Danger") status <- "Warning"
            reasons <- c(reasons, "⚡ ควรระวัง: มีข้อควรระวังสำหรับผู้ป่วยโรคประจำตัว")
            penalty <- penalty + 25
          }
        }
        
        if (length(all_med_patterns) > 0) {
          med_pattern <- paste(all_med_patterns, collapse = "|")
          if (grepl(med_pattern, caution_text, ignore.case = TRUE)) {
            if (status != "Danger") status <- "Warning"
            reasons <- c(reasons, "⚡ ควรระวัง: เสี่ยงเกิดปฏิกิริยาระหว่างยากับอาหารเสริม (Drug-Supplement Interaction)")
            penalty <- penalty + 25
          }
        }
        
        if (length(reasons) == 0) {
          reasons_str <- "ปลอดภัย ไม่พบข้อห้ามใช้เฉพาะบุคคล"
        } else {
          reasons_str <- paste(reasons, collapse = "<br>")
        }
        
        safety_status <- c(safety_status, status)
        warning_reasons <- c(warning_reasons, reasons_str)
        safety_penalty <- c(safety_penalty, penalty)
      }
      
      res <- products %>%
        mutate(
          Status = safety_status,
          Warning_Reason = warning_reasons,
          
          status_rank = case_when(
            Status == "Pass" ~ 1,
            Status == "Warning" ~ 2,
            Status == "Danger" ~ 3
          ),
          
          `ระดับความปลอดภัย` = case_when(
            Status == "Pass" ~ "<span class='label label-success' style='font-size:12px;'>✓ ปลอดภัย</span>",
            Status == "Warning" ~ "<span class='label label-warning' style='font-size:12px;'>⚡ ควรระวัง</span>",
            Status == "Danger" ~ "<span class='label label-danger' style='font-size:12px;'>⚠️ ข้อห้ามใช้</span>"
          ),
          
          unit_num = as.numeric(str_extract(`ขนาดบรรจุจริง`, "\\d+")),
          cost_per_unit = ifelse(!is.na(`ราคาจริง`) & !is.na(unit_num) & unit_num > 0, round(`ราคาจริง` / unit_num, 2), NA),
          
          `ขนาดบรรจุและราคาต่อหน่วย` = ifelse(
            is.na(cost_per_unit), 
            `ขนาดบรรจุจริง`,
            paste0(`ขนาดบรรจุจริง`, " <small style='color:gray;'>(~", cost_per_unit, " บาท/หน่วย)</small>")
          ),
          
          `สารสำคัญและปริมาณ` = paste0(`สารสำคัญจริง`, ifelse(`ความแรงจริง` != "", paste0(" (", `ความแรงจริง`, ")"), "")),
          
          price_score = ifelse(is.na(`ราคาจริง`), 0, (1 - (`ราคาจริง` / input$budget)) * 20),
          need_score = 30,
          target_score = ifelse(input$target != "all" & grepl(input$target, `กลุ่มเป้าหมายจริง`), 10, 0),
          active_score = ifelse(`สารสำคัญจริง` != "-", 20, 0),
          
          base_score = price_score + need_score + target_score + active_score,
          total_score = as.integer(pmax(0, round(base_score - safety_penalty, 0))),
          
          `ราคาแสดงผล` = ifelse(is.na(`ราคาจริง`), "-", as.character(`ราคาจริง`))
        ) %>%
        arrange(status_rank, desc(total_score))
      
      processed_data(res)
    } else {
      processed_data(NULL)
    }
  })
  
  observeEvent(input$reset_btn, {
    reset("side-panel")
    updateSelectInput(session, "category", selected = "ทั้งหมด")
    updateSelectInput(session, "sub_category", selected = character(0))
    processed_data(NULL)
  })
  
  output$result_table <- renderDT({
    df <- processed_data()
    req(!is.null(df))
    
    df %>%
      select(
        `ชื่อผลิตภัณฑ์`,
        `หมวดหมู่ย่อย` = `หมวดหมู่ย่อยจริง`,
        `สถานะความปลอดภัย` = `ระดับความปลอดภัย`,
        `ราคา (บาท)` = `ราคาแสดงผล`,
        `คะแนนรวม` = total_score
      ) %>%
      datatable(
        escape = FALSE,
        options = list(dom = 't', pageLength = 10, ordering = FALSE),
        selection = 'multiple',
        rownames = FALSE
      )
  })
  
  output$warning_summary_table <- renderDT({
    df <- processed_data()
    req(!is.null(df))
    
    df %>%
      filter(Status != "Pass") %>%
      select(
        `ชื่อผลิตภัณฑ์`,
        `สถานะความปลอดภัย` = `ระดับความปลอดภัย`,
        `เหตุผลการติดเตือน / ข้อห้ามใช้` = Warning_Reason
      ) %>%
      datatable(
        escape = FALSE,
        options = list(dom = 't', autoWidth = TRUE, ordering = FALSE),
        rownames = FALSE
      )
  })
  
  output$detail_table <- renderDT({
    df <- processed_data()
    req(!is.null(df))
    
    df %>%
      select(
        `ชื่อผลิตภัณฑ์`,
        `สารสำคัญและปริมาณ`,
        `วิธีรับประทาน` = `วิธีทานจริง`,
        `ข้อควรระวัง` = `ข้อควรระวังจริง`,
        `ขนาดบรรจุ` = `ขนาดบรรจุและราคาต่อหน่วย`
      ) %>%
      datatable(
        escape = FALSE,
        options = list(pageLength = 5, dom = 't', autoWidth = TRUE),
        rownames = FALSE
      )
  })
  
  # --------------------------------------
  # Compare Mode
  # --------------------------------------
  observeEvent(input$go_compare, {
    updateTabsetPanel(session, "main_tabs", selected = "Compare Mode (เปรียบเทียบสินค้า)")
  })
  
  output$compare_ui <- renderUI({
    df <- processed_data()
    if (is.null(df)) {
      return(div(class = "alert alert-info", "ยังไม่มีข้อมูล กรุณาทำการประเมินและคัดเลือกก่อน"))
    }
    
    selected_rows <- input$result_table_rows_selected
    
    if (length(selected_rows) == 0) {
      return(div(class = "alert alert-warning", "กรุณาย้อนกลับไปที่ตารางผลการประเมิน แล้วคลิกเลือกผลิตภัณฑ์ในตารางอย่างน้อย 1 รายการ"))
    }
    
    selected_df <- df[selected_rows, ]
    
    col_width <- case_when(
      nrow(selected_df) == 1 ~ 8,
      nrow(selected_df) == 2 ~ 6,
      TRUE ~ 4
    )
    
    cols <- lapply(1:nrow(selected_df), function(i) {
      item <- selected_df[i, ]
      
      img_url <- if ("รูปภาพ" %in% names(item) && !is.na(item$`รูปภาพ`) && item$`รูปภาพ` != "") {
        item$`รูปภาพ`
      } else {
        "https://via.placeholder.com/150?text=No+Image"
      }
      
      column(
        width = col_width,
        offset = if(nrow(selected_df) == 1) 2 else 0,
        wellPanel(
          style = "background-color: #ffffff; border-radius: 8px; box-shadow: 0px 2px 5px rgba(0,0,0,0.1); margin-bottom: 20px;",
          
          div(
            style = "text-align: center; margin-bottom: 15px;",
            tags$img(
              src = img_url, 
              style = "max-height: 180px; max-width: 100%; object-fit: contain; border-radius: 5px;"
            )
          ),
          
          h4(item$`ชื่อผลิตภัณฑ์`, style = "color: #2c3e50; font-weight: bold; min-height: 40px;"),
          HTML(item$`ระดับความปลอดภัย`),
          hr(),
          p(strong("หมวดหมู่ย่อย: "), item$`หมวดหมู่ย่อยจริง`),
          p(strong("ราคา: "), item$`ราคาแสดงผล`, " บาท"),
          p(strong("ขนาดบรรจุ: "), item$`ขนาดบรรจุจริง`),
          p(strong("ราคาต่อหน่วย: "), ifelse(is.na(item$cost_per_unit), "-", paste0(item$cost_per_unit, " บาท/หน่วย"))),
          hr(),
          p(strong("สารสำคัญและปริมาณ:")),
          p(item$`สารสำคัญและปริมาณ`),
          hr(),
          p(strong("ข้อบ่งใช้ / ประโยชน์:")),
          p(item$`ข้อบ่งใช้จริง`, style = "color: #16a085;"),
          hr(),
          p(strong("วิธีรับประทาน:")),
          p(item$`วิธีทานจริง`),
          hr(),
          p(strong("ข้อควรระวัง / ข้อห้ามใช้:")),
          p(item$`ข้อควรระวังจริง`, style = "color: #c0392b;")
        )
      )
    })
    
    do.call(fluidRow, cols)
  })
}

# ==========================================
# 4. เปิด Web Application
# ==========================================
shinyApp(ui = ui, server = server)