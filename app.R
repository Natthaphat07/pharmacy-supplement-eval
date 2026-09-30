library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(DT)
library(shinyjs)
library(digest)
library(rsconnect)

# ==========================================
# 1. อ่านฐานข้อมูล
# ==========================================

data <- read_excel("pharmacy_database.xlsx")
esc <- htmltools::htmlEscape

FOOD_CHOICES <- c(
  "อาหารทะเล", "ปลา", "กุ้ง", "ปู", "หอย", "หมึก", "ถั่ว", "ถั่วลิสง",
  "ถั่วเหลือง", "นม", "แลคโตส", "Whey", "ไข่", "กลูเตน", "Gluten",
  "ชาเขียว", "ยีสต์", "เกสรดอกไม้", "งา", "ข้าวสาลี"
)

DRUG_CHOICES <- c(
  "Penicillin", "Amoxicillin", "Ampicillin", "Cephalexin", "Ceftriaxone",
  "Sulfa", "Sulfonamide", "Aspirin", "Ibuprofen", "Diclofenac",
  "Paracetamol", "พาราเซตามอล", "Warfarin", "Metformin", "ยาลดความดัน"
)

COND_CHOICES <- c(
  "เบาหวาน", "เบาหวานชนิดที่ 2", "ความดัน", "ความดันโลหิตสูง",
  "โรคหัวใจ", "โรคไต", "โรคไตเรื้อรัง", "ไตวาย", "โรคตับ",
  "โรคตับเรื้อรัง", "นิ่วในไต", "โรคไทรอยด์", "ไทรอยด์", "โรคลมชัก",
  "โรคกระเพาะ", "กรดไหลย้อน", "หอบหืด", "โรคหืด", "ไขมันในเลือดสูง",
  "เกาต์", "โลหิตจาง", "G6PD"
)

MED_CHOICES <- c(
  "Warfarin", "Aspirin", "Clopidogrel", "Metformin", "Glipizide",
  "Insulin", "ยาลดน้ำตาล", "ยาลดความดัน", "Amlodipine", "Losartan",
  "Enalapril", "ยาขับปัสสาวะ", "Atorvastatin", "Simvastatin",
  "Levothyroxine", "ยาละลายลิ่มเลือด", "ยาลดไขมัน"
)

# ==========================================
# 1.1 ระบบบัญชี
# ==========================================

USERS_FILE <- "users.rds"

empty_users <- function() {
  data.frame(
    username = character(0), salt = character(0), hash = character(0),
    store_name = character(0), store_address = character(0),
    pharm_full = character(0), created = character(0),
    stringsAsFactors = FALSE
  )
}

load_users <- function() {
  if (file.exists(USERS_FILE)) {
    u <- tryCatch(readRDS(USERS_FILE), error = function(e) NULL)
    if (!is.null(u)) {
      if ("license_no" %in% names(u)) u$license_no <- NULL
      return(u)
    }
  }
  empty_users()
}

save_users <- function(u) saveRDS(u, USERS_FILE)

make_salt <- function() paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
hash_pw <- function(salt, pw) digest(paste0(salt, pw), algo = "sha256", serialize = FALSE)

# ==========================================
# 1.2 CSS
# ==========================================

# ==========================================
# 2. UI
# ==========================================

ui <- fluidPage(
  useShinyjs(),
  div(
    id = "login_page",
    style = "max-width:480px;margin:50px auto;padding:25px 30px;background:#fff;border-radius:10px;box-shadow:0 4px 15px rgba(0,0,0,.15);",
    h3("ระบบช่วยคัดเลือกอาหารเสริม", style = "text-align:center;color:#2c3e50;font-weight:bold;margin-bottom:15px;"),
    tabsetPanel(
      id = "auth_tabs",
      tabPanel(
        "เข้าสู่ระบบ", value = "tab_login",
        br(), uiOutput("login_msg"),
        textInput("login_user", "ชื่อผู้ใช้งาน (Username):"),
        passwordInput("login_pass", "รหัสผ่าน (Password):"),
        actionButton("login_btn", "เข้าสู่ระบบ", class = "btn-primary btn-block", style = "font-weight:bold;padding:10px;"),
        br(),
        p("ยังไม่มีบัญชี? ไปที่แท็บ \"สร้างบัญชีใหม่\" ด้านบน", style = "text-align:center;color:#777;")
      ),
      tabPanel(
        "สร้างบัญชีใหม่", value = "tab_register",
        br(), uiOutput("register_msg"),
        textInput("reg_store", "ชื่อร้านยา / คลินิก *"),
        textInput("reg_addr", "ที่อยู่ / เบอร์โทร (แสดงบนฉลาก ไม่บังคับ)"),
        fluidRow(
          column(4, selectInput("reg_prefix", "คำนำหน้า *", choices = c("ภก.", "ภญ."))),
          column(8, textInput("reg_pharm", "ชื่อ-นามสกุล เภสัชกร *"))
        ),
        hr(),
        textInput("reg_user", "ตั้งชื่อผู้ใช้งาน * (ภาษาอังกฤษ/ตัวเลข 4-20 ตัว)"),
        passwordInput("reg_pass", "ตั้งรหัสผ่าน * (อย่างน้อย 6 ตัวอักษร)"),
        passwordInput("reg_pass2", "ยืนยันรหัสผ่าน *"),
        actionButton("reg_btn", "สร้างบัญชี", class = "btn-success btn-block", style = "font-weight:bold;padding:10px;")
      )
    )
  ),
  hidden(
    div(
      id = "main_app",
      fluidRow(
        style = "background-color:#2c3e50;color:white;padding:12px 20px;margin-bottom:20px;border-radius:0 0 8px 8px;",
        column(8, h4(uiOutput("header_info"), style = "margin:5px 0;font-weight:bold;")),
        column(4, class = "text-right", actionButton("logout_btn", "ออกจากระบบ", class = "btn-danger btn-sm", style = "margin-top:3px;"))
      ),
      titlePanel("ระบบเภสัชกรคัดเลือก ประเมินความปลอดภัย และเปรียบเทียบอาหารเสริม"),
      sidebarLayout(
        sidebarPanel(
          id = "side-panel",
          width = 4,
          h4("1. ความต้องการทั่วไป", style = "color:#2c3e50;font-weight:bold;"),
          uiOutput("category_ui"),
          uiOutput("sub_category_ui"),
          numericInput("budget", "งบประมาณสูงสุด (บาท):", value = 1000, min = 0, step = 100),
          selectInput(
            "target",
            "กลุ่มช่วงวัยผู้ใช้:",
            choices = c(
              "ไม่ระบุ" = "all",
              "ผู้ใหญ่/วัยทำงาน" = "ผู้ใหญ่|วัยทำงาน|ทำงาน|ผิวแห้ง",
              "ผู้สูงอายุ" = "ผู้สูงอายุ|สูงวัย|ชะลอวัย",
              "นักศึกษา/วัยเรียน" = "นักศึกษา|วัยเรียน|เรียน"
            )
          ),
          hr(),
          h4("2. ซักประวัติความปลอดภัย (เภสัชกร)", style = "color:#c0392b;font-weight:bold;"),
          checkboxGroupInput(
            "allergies_check",
            "ประวัติการแพ้อาหาร / สารสกัด:",
            choices = c(
              "แพ้อาหารทะเล / ปลา" = "ปลา|อาหารทะเล|ซีฟู้ด|กุ้ง|ปู",
              "แพ้ถั่ว / ถั่วเหลือง" = "ถั่ว|Lecithin|Soy",
              "แพ้นม / แลคโตส" = "นม|Lactose|Whey",
              "แพ้เกสรดอกไม้ / พืช" = "เกสร|ดอกไม้|พืช"
            )
          ),
          selectizeInput(
            "allergies_text",
            "Smart Search อาหาร / สารสกัดที่แพ้:",
            choices = FOOD_CHOICES,
            multiple = TRUE,
            options = list(
              placeholder = "พิมพ์ 1–2 ตัวอักษรเพื่อค้นหา หรือพิมพ์ชื่อเอง...",
              minChars = 1, create = TRUE, persist = TRUE, delimiter = ",",
              plugins = list("remove_button")
            )
          ),
          br(),
          checkboxGroupInput(
            "drug_allergies_check",
            "ประวัติการแพ้ยา:",
            choices = c(
              "กลุ่มแอสไพริน / NSAIDs" = "Aspirin|NSAID|Ibuprofen",
              "กลุ่มยาซัลฟา (Sulfa)" = "Sulfa|Sulfonamide"
            )
          ),
          selectizeInput(
            "drug_allergies_text",
            "Smart Search ยาที่แพ้:",
            choices = DRUG_CHOICES,
            multiple = TRUE,
            options = list(
              placeholder = "พิมพ์ 1–2 ตัวอักษรเพื่อค้นหา หรือพิมพ์ชื่อยาเอง...",
              minChars = 1, create = TRUE, persist = TRUE, delimiter = ",",
              plugins = list("remove_button")
            )
          ),
          br(),
          radioButtons(
            "pregnant",
            "สถานะการตั้งครรภ์ / ให้นมบุตร:",
            choices = c(
              "ไม่ได้ตั้งครรภ์ / ให้นมบุตร" = "no",
              "ตั้งครรภ์ หรือ ให้นมบุตร" = "yes"
            ),
            selected = "no"
          ),
          br(),
          checkboxGroupInput(
            "conditions",
            "โรคประจำตัวสำคัญ:",
            choices = c(
              "โรคลมชัก" = "ลมชัก",
              "โรคตับ / โรคไต" = "ตับ|ไต",
              "โรคนิ่วในไต" = "นิ่ว",
              "โรคไทรอยด์" = "ไทรอยด์"
            )
          ),
          selectizeInput(
            "conditions_text",
            "Smart Search โรคประจำตัวเพิ่มเติม:",
            choices = COND_CHOICES,
            multiple = TRUE,
            options = list(
              placeholder = "พิมพ์ 1–2 ตัวอักษรเพื่อค้นหา หรือพิมพ์โรคเอง...",
              minChars = 1, create = TRUE, persist = TRUE, delimiter = ",",
              plugins = list("remove_button")
            )
          ),
          br(),
          checkboxGroupInput(
            "medications_check",
            "ยาเดิมที่รับประทานอยู่เป็นประจำ:",
            choices = c(
              "ยาละลายลิ่มเลือด (Warfarin, Aspirin)" = "ลิ่มเลือด|Aspirin|Warfarin",
              "ยาลดน้ำตาล / ยาเบาหวาน" = "เบาหวาน|น้ำตาล"
            )
          ),
          selectizeInput(
            "medications_text",
            "Smart Search ยาที่ใช้อยู่เพิ่มเติม:",
            choices = MED_CHOICES,
            multiple = TRUE,
            options = list(
              placeholder = "พิมพ์ 1–2 ตัวอักษรเพื่อค้นหา หรือพิมพ์ชื่อยาเอง...",
              minChars = 1, create = TRUE, persist = TRUE, delimiter = ",",
              plugins = list("remove_button")
            )
          ),
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
  )
)

# ==========================================
# 3. SERVER
# ==========================================

server <- function(input, output, session) {
  
  user_auth <- reactiveValues(
    logged_in = FALSE,
    username = "",
    store_name = "",
    store_address = "",
    pharm_full = ""
  )
  
  login_msg_val <- reactiveVal(NULL)
  reg_msg_val <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  
  output$login_msg <- renderUI(login_msg_val())
  output$register_msg <- renderUI(reg_msg_val())
  
  # ==========================================
  # สร้างบัญชี
  # ==========================================
  
  observeEvent(input$reg_btn, {
    store <- trimws(input$reg_store)
    addr <- trimws(input$reg_addr)
    pharm <- trimws(input$reg_pharm)
    u <- tolower(trimws(input$reg_user))
    pw <- input$reg_pass
    err <- NULL
    
    if (store == "" || pharm == "" || u == "" || pw == "") {
      err <- "กรุณากรอกข้อมูลที่มีเครื่องหมาย * ให้ครบ"
    } else if (!grepl("^[a-z0-9_.-]{4,20}$", u)) {
      err <- "ชื่อผู้ใช้งานต้องเป็นภาษาอังกฤษ/ตัวเลข/._- ความยาว 4-20 ตัว"
    } else if (nchar(pw) < 6) {
      err <- "รหัสผ่านต้องมีอย่างน้อย 6 ตัวอักษร"
    } else if (pw != input$reg_pass2) {
      err <- "รหัสผ่านและการยืนยันรหัสผ่านไม่ตรงกัน"
    } else {
      users <- load_users()
      if (u %in% users$username) err <- "ชื่อผู้ใช้งานนี้ถูกใช้แล้ว กรุณาตั้งชื่ออื่น"
    }
    
    if (!is.null(err)) {
      reg_msg_val(div(class = "alert alert-danger", err))
      return()
    }
    
    users <- load_users()
    salt <- make_salt()
    
    new_user <- data.frame(
      username = u,
      salt = salt,
      hash = hash_pw(salt, pw),
      store_name = store,
      store_address = addr,
      pharm_full = paste(input$reg_prefix, pharm),
      created = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    
    saved <- tryCatch({
      save_users(rbind(users, new_user))
      TRUE
    }, error = function(e) FALSE)
    
    if (!saved) {
      reg_msg_val(div(class = "alert alert-danger", "บันทึกบัญชีไม่สำเร็จ (ไม่สามารถเขียนไฟล์ได้)"))
      return()
    }
    
    reg_msg_val(NULL)
    login_msg_val(div(class = "alert alert-success", "สร้างบัญชีสำเร็จ กรุณาเข้าสู่ระบบด้วยชื่อผู้ใช้และรหัสผ่านที่ตั้งไว้"))
    
    updateTextInput(session, "login_user", value = u)
    updateTextInput(session, "login_pass", value = "")
    
    for (id in c("reg_store", "reg_addr", "reg_pharm", "reg_user", "reg_pass", "reg_pass2")) {
      updateTextInput(session, id, value = "")
    }
    
    updateTabsetPanel(session, "auth_tabs", selected = "tab_login")
  })
  
  # ==========================================
  # Login
  # ==========================================
  
  observeEvent(input$login_btn, {
    u <- tolower(trimws(input$login_user))
    users <- load_users()
    idx <- which(users$username == u)
    
    ok <- length(idx) == 1 &&
      identical(hash_pw(users$salt[idx], input$login_pass), users$hash[idx])
    
    if (!ok) {
      login_msg_val(div(class = "alert alert-danger", "ชื่อผู้ใช้หรือรหัสผ่านไม่ถูกต้อง (หากยังไม่มีบัญชี ให้สร้างบัญชีก่อน)"))
      return()
    }
    
    user_auth$logged_in <- TRUE
    user_auth$username <- u
    user_auth$store_name <- users$store_name[idx]
    user_auth$store_address <- users$store_address[idx]
    user_auth$pharm_full <- users$pharm_full[idx]
    
    login_msg_val(NULL)
    updateTextInput(session, "login_pass", value = "")
    shinyjs::hide("login_page")
    shinyjs::show("main_app")
  })
  
  # ==========================================
  # Logout
  # ==========================================
  
  observeEvent(input$logout_btn, {
    user_auth$logged_in <- FALSE
    user_auth$username <- ""
    user_auth$store_name <- ""
    user_auth$store_address <- ""
    user_auth$pharm_full <- ""
    processed_data(NULL)
    updateTextInput(session, "patient_name", value = "")
    updateTextInput(session, "login_user", value = "")
    updateTextInput(session, "login_pass", value = "")
    shinyjs::show("login_page")
    shinyjs::hide("main_app")
  })
  
  output$header_info <- renderUI({
    req(user_auth$logged_in)
    HTML(paste0("🏥 ", esc(user_auth$store_name), " | 👤 ", esc(user_auth$pharm_full)))
  })
  
  # ==========================================
  # ฟังก์ชันช่วย
  # ==========================================
  
  get_col <- function(df, keywords, default = "") {
    for (kw in keywords) {
      match_col <- names(df)[grepl(kw, names(df), ignore.case = TRUE)]
      if (length(match_col) > 0) {
        x <- as.character(df[[match_col[1]]])
        x[is.na(x)] <- default
        return(x)
      }
    }
    rep(default, nrow(df))
  }
  
  # ==========================================
  # Dynamic UI
  # ==========================================
  
  output$category_ui <- renderUI({
    cat_col <- names(data)[grepl("หมวดหมู่หลัก", names(data))][1]
    
    if (!is.na(cat_col)) {
      all_cats <- sort(unique(na.omit(data[[cat_col]])))
    } else {
      all_cats <- c()
    }
    
    target_cat <- "ผิว-ความงาม"
    sorted_cats <- if (target_cat %in% all_cats) c(target_cat, setdiff(all_cats, target_cat)) else all_cats
    
    selectizeInput(
      "category",
      "เป้าหมายสุขภาพที่ต้องการดูแล (เลือกได้หลายหมวดหมู่):",
      choices = c("ทั้งหมด", sorted_cats),
      selected = "ทั้งหมด",
      multiple = TRUE,
      options = list(
        placeholder = "เลือกได้หลายหมวดหมู่...",
        plugins = list("remove_button")
      )
    )
  })
  
  observeEvent(input$category, {
    sel <- input$category
    
    if (is.null(sel) || length(sel) == 0) {
      updateSelectizeInput(session, "category", selected = "ทั้งหมด")
      return()
    }
    
    if ("ทั้งหมด" %in% sel && length(sel) > 1) sel <- setdiff(sel, "ทั้งหมด")
    if (length(sel) == 0) sel <- "ทั้งหมด"
    
    updateSelectizeInput(session, "category", selected = sel)
  }, ignoreInit = TRUE)
  
  output$sub_category_ui <- renderUI({
    req(input$category)
    
    preselect <- isolate(input$sub_category)
    if (is.null(preselect)) preselect <- character(0)
    
    cat_col <- names(data)[grepl("หมวดหมู่หลัก", names(data))][1]
    sub_col <- names(data)[grepl("หมวดหมู่ย่อย", names(data))][1]
    
    if (is.na(sub_col)) return(NULL)
    
    selected_categories <- input$category
    
    if (length(selected_categories) == 0 || "ทั้งหมด" %in% selected_categories || is.na(cat_col)) {
      raw_subs <- data[[sub_col]]
    } else {
      raw_subs <- data[[sub_col]][data[[cat_col]] %in% selected_categories]
    }
    
    parsed_subs <- raw_subs %>%
      na.omit() %>%
      str_split(";") %>%
      unlist() %>%
      str_trim() %>%
      unique() %>%
      sort()
    
    selectizeInput(
      "sub_category",
      "ระบุเน้นเจาะจง/หมวดหมู่ย่อย (เลือกได้หลายรายการ):",
      choices = parsed_subs,
      selected = intersect(unique(preselect), parsed_subs),
      multiple = TRUE,
      options = list(
        placeholder = "เลือกได้หลายรายการ...",
        plugins = list("remove_button")
      )
    )
  })
  
  # ==========================================
  # ประมวลผลหลัก
  # ==========================================
  
  run_search <- function() {
    products <- data
    
    products$`วิธีทานจริง` <- get_col(products, c("วิธีรับประทาน", "Serving"), "-")
    products$`ข้อควรระวังจริง` <- get_col(products, c("ข้อควรระวัง"), "-")
    products$`สารสำคัญจริง` <- get_col(products, c("สารสำคัญ", "ส่วนประกอบ"), "-")
    products$`ความแรงจริง` <- get_col(products, c("ความแรง", "ปริมาณ"), "")
    products$`ขนาดบรรจุจริง` <- get_col(products, c("ขนาดบรรจุ"), "-")
    products$`กลุ่มเป้าหมายจริง` <- get_col(products, c("กลุ่มเป้าหมาย"), "")
    products$`หมวดหมู่ย่อยจริง` <- get_col(products, c("หมวดหมู่ย่อย"), "")
    products$`ข้อบ่งใช้จริง` <- get_col(products, c("ประโยชน์", "สรรพคุณ", "ข้อบ่งใช้"), "-")
    products$`หมวดหมู่หลักจริง` <- get_col(products, c("หมวดหมู่หลัก"), "")
    
    price_col <- names(data)[grepl("ราคา", names(data)) & !grepl("ทุน|ซื้อ|cost", names(data), ignore.case = TRUE)][1]
    
    if (!is.na(price_col)) {
      products$`ราคาจริง` <- suppressWarnings(as.numeric(data[[price_col]]))
    } else {
      products$`ราคาจริง` <- NA_real_
    }
    
    cost_col <- names(data)[grepl("ต้นทุน|ราคาทุน|ราคาซื้อ|ทุน|cost", names(data), ignore.case = TRUE)][1]
    
    if (!is.na(cost_col)) {
      products$`ต้นทุนจริง` <- suppressWarnings(as.numeric(data[[cost_col]]))
    } else {
      products$`ต้นทุนจริง` <- NA_real_
    }
    
    cat_sel <- input$category
    
    if (!is.null(cat_sel) && length(cat_sel) > 0 && !"ทั้งหมด" %in% cat_sel) {
      products <- products %>% filter(`หมวดหมู่หลักจริง` %in% cat_sel)
    }
    
    subs_in <- input$sub_category
    
    if (!is.null(subs_in) && length(subs_in) > 0) {
      sub_pattern <- paste(subs_in, collapse = "|")
      products <- products %>% filter(grepl(sub_pattern, `หมวดหมู่ย่อยจริง`, ignore.case = TRUE))
    }
    
    products <- products %>% filter(is.na(`ราคาจริง`) | `ราคาจริง` <= input$budget)
    
    parse_terms <- function(x) {
      if (is.null(x) || length(x) == 0) return(character(0))
      x <- unlist(x, use.names = FALSE)
      x <- unlist(strsplit(as.character(x), "[,;]"))
      x <- trimws(x)
      unique(x[!is.na(x) & x != ""])
    }
    
    all_food_patterns <- unique(c(input$allergies_check, parse_terms(input$allergies_text)))
    all_drug_patterns <- unique(c(input$drug_allergies_check, parse_terms(input$drug_allergies_text)))
    all_condition_patterns <- unique(c(input$conditions, parse_terms(input$conditions_text)))
    all_med_patterns <- unique(c(input$medications_check, parse_terms(input$medications_text)))
    
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
        
        if (input$pregnant == "yes" && grepl("ครรภ์|ให้นม|เด็ก", caution_text)) {
          status <- "Danger"
          reasons <- c(reasons, "⚠️ ข้อห้ามใช้: ไม่เหมาะสำหรับสตรีมีครรภ์/ให้นมบุตร")
          penalty <- penalty + 50
        }
        
        if (length(all_food_patterns) > 0) {
          food_pattern <- paste(all_food_patterns, collapse = "|")
          
          if (grepl(food_pattern, ingredient_text, ignore.case = TRUE) ||
              grepl(food_pattern, caution_text, ignore.case = TRUE)) {
            status <- "Danger"
            reasons <- c(reasons, "⚠️ ข้อห้ามใช้: มีส่วนประกอบของสารหรืออาหารที่ผู้ป่วยแพ้")
            penalty <- penalty + 50
          }
        }
        
        if (length(all_drug_patterns) > 0) {
          drug_pattern <- paste(all_drug_patterns, collapse = "|")
          
          if (grepl(drug_pattern, caution_text, ignore.case = TRUE) ||
              grepl(drug_pattern, ingredient_text, ignore.case = TRUE)) {
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
        
        reasons_str <- if (length(reasons) == 0) {
          "ปลอดภัย ไม่พบข้อห้ามใช้เฉพาะบุคคล"
        } else {
          paste(reasons, collapse = "<br>")
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
          cost_per_unit = ifelse(
            !is.na(`ราคาจริง`) & !is.na(unit_num) & unit_num > 0,
            round(`ราคาจริง` / unit_num, 2),
            NA
          ),
          `ขนาดบรรจุและราคาต่อหน่วย` = ifelse(
            is.na(cost_per_unit),
            `ขนาดบรรจุจริง`,
            paste0(`ขนาดบรรจุจริง`, " <small style='color:gray;'>(~", cost_per_unit, " บาท/หน่วย)</small>")
          ),
          `สารสำคัญและปริมาณ` = paste0(
            `สารสำคัญจริง`,
            ifelse(`ความแรงจริง` != "", paste0(" (", `ความแรงจริง`, ")"), "")
          ),
          price_score = ifelse(
            is.na(`ราคาจริง`),
            0,
            (1 - (`ราคาจริง` / input$budget)) * 20
          ),
          need_score = 30,
          target_score = ifelse(
            input$target != "all" & grepl(input$target, `กลุ่มเป้าหมายจริง`),
            10,
            0
          ),
          active_score = ifelse(`สารสำคัญจริง` != "-", 20, 0),
          base_score = price_score + need_score + target_score + active_score,
          total_score = as.integer(pmax(0, round(base_score - safety_penalty, 0))),
          `ราคาแสดงผล` = ifelse(is.na(`ราคาจริง`), "-", as.character(`ราคาจริง`)),
          `กำไรต่อหน่วย` = ifelse(
            !is.na(`ราคาจริง`) & !is.na(`ต้นทุนจริง`),
            round(`ราคาจริง` - `ต้นทุนจริง`, 2),
            NA_real_
          ),
          `อัตรากำไร (%)` = ifelse(
            !is.na(`ราคาจริง`) & !is.na(`ต้นทุนจริง`) & `ราคาจริง` > 0,
            round((`ราคาจริง` - `ต้นทุนจริง`) / `ราคาจริง` * 100, 2),
            NA_real_
          )
        ) %>%
        arrange(status_rank, desc(total_score))
      
      processed_data(res)
    } else {
      processed_data(NULL)
    }
  }
  
  # ==========================================
  # Search
  # ==========================================
  
  observeEvent(input$search, {
    run_search()
  })
  
  # ==========================================
  # DataTable Proxy
  # ==========================================
  
  result_proxy <- dataTableProxy("result_table")
  
  # ==========================================
  # Reset
  # ==========================================
  
  observeEvent(input$reset_btn, {
    tryCatch(selectRows(result_proxy, NULL), error = function(e) NULL)
    
    updateSelectizeInput(session, "category", selected = "ทั้งหมด")
    updateSelectizeInput(session, "sub_category", selected = character(0))
    updateNumericInput(session, "budget", value = 1000)
    updateSelectInput(session, "target", selected = "all")
    updateCheckboxGroupInput(session, "allergies_check", selected = character(0))
    updateSelectizeInput(session, "allergies_text", selected = character(0))
    updateCheckboxGroupInput(session, "drug_allergies_check", selected = character(0))
    updateSelectizeInput(session, "drug_allergies_text", selected = character(0))
    updateRadioButtons(session, "pregnant", selected = "no")
    updateCheckboxGroupInput(session, "conditions", selected = character(0))
    updateSelectizeInput(session, "conditions_text", selected = character(0))
    updateCheckboxGroupInput(session, "medications_check", selected = character(0))
    updateSelectizeInput(session, "medications_text", selected = character(0))
    processed_data(NULL)
    
    shinyjs::runjs("
      setTimeout(function(){
        $('#result_table').find('tbody tr').removeClass('selected');
      }, 100);
    ")
  })
  
  # ==========================================
  # Result Table
  # ==========================================
  
  output$result_table <- renderDT({
    df <- processed_data()
    if (is.null(df)) return(NULL)
    
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
        options = list(
          dom = "t",
          pageLength = 10,
          ordering = FALSE,
          autoWidth = TRUE
        ),
        selection = "multiple",
        rownames = FALSE
      )
  })
  
  output$warning_summary_table <- renderDT({
    df <- processed_data()
    if (is.null(df)) return(NULL)
    
    df %>%
      filter(Status != "Pass") %>%
      select(
        `ชื่อผลิตภัณฑ์`,
        `สถานะความปลอดภัย` = `ระดับความปลอดภัย`,
        `เหตุผลการติดเตือน / ข้อห้ามใช้` = Warning_Reason
      ) %>%
      datatable(
        escape = FALSE,
        options = list(
          dom = "t",
          autoWidth = TRUE,
          ordering = FALSE
        ),
        rownames = FALSE
      )
  })
  
  output$detail_table <- renderDT({
    df <- processed_data()
    if (is.null(df)) return(NULL)
    
    df %>%
      select(
        `ชื่อผลิตภัณฑ์`,
        `สารสำคัญและปริมาณ`,
        `วิธีรับประทาน` = `วิธีทานจริง`,
        `ข้อควรระวัง` = `ข้อควรระวังจริง`,
        `ขนาดบรรจุ` = `ขนาดบรรจุและราคาต่อหน่วย`,
        `ราคาทุน (บาท)` = `ต้นทุนจริง`,
        `กำไร/หน่วย (บาท)` = `กำไรต่อหน่วย`,
        `อัตรากำไร (%)`
      ) %>%
      datatable(
        escape = FALSE,
        options = list(
          pageLength = 5,
          dom = "t",
          autoWidth = TRUE
        ),
        rownames = FALSE
      )
  })
  
  # ==========================================
  # Compare Mode
  # ==========================================
  
  observeEvent(input$go_compare, {
    updateTabsetPanel(
      session,
      "main_tabs",
      selected = "Compare Mode (เปรียบเทียบสินค้า)"
    )
  })
  
  output$compare_ui <- renderUI({
    df <- processed_data()
    
    if (is.null(df)) {
      return(div(
        class = "alert alert-info",
        "ยังไม่มีข้อมูล กรุณาทำการประเมินและคัดเลือกก่อน"
      ))
    }
    
    selected_rows <- input$result_table_rows_selected
    
    if (length(selected_rows) == 0) {
      return(div(
        class = "alert alert-warning",
        "กรุณาย้อนกลับไปที่ตารางผลการประเมิน แล้วคลิกเลือกผลิตภัณฑ์ในตารางอย่างน้อย 1 รายการ"
      ))
    }
    
    selected_df <- df[selected_rows, , drop = FALSE]
    
    col_width <- case_when(
      nrow(selected_df) == 1 ~ 8,
      nrow(selected_df) == 2 ~ 6,
      TRUE ~ 4
    )
    
    cols <- lapply(seq_len(nrow(selected_df)), function(i) {
      item <- selected_df[i, ]
      
      img_url <- if (
        "รูปภาพ" %in% names(item) &&
        !is.na(item$`รูปภาพ`) &&
        item$`รูปภาพ` != ""
      ) item$`รูปภาพ` else "https://via.placeholder.com/150?text=No+Image"
      
      column(
        width = col_width,
        offset = if (nrow(selected_df) == 1) 2 else 0,
        wellPanel(
          style = "background-color:#fff;border-radius:8px;box-shadow:0 2px 5px rgba(0,0,0,.1);margin-bottom:20px;",
          div(
            style = "text-align:center;margin-bottom:15px;",
            tags$img(
              src = img_url,
              style = "max-height:180px;max-width:100%;object-fit:contain;border-radius:5px;"
            )
          ),
          h4(item$`ชื่อผลิตภัณฑ์`, style = "color:#2c3e50;font-weight:bold;min-height:40px;"),
          HTML(item$`ระดับความปลอดภัย`),
          hr(),
          p(strong("หมวดหมู่ย่อย: "), item$`หมวดหมู่ย่อยจริง`),
          p(strong("ราคา: "), item$`ราคาแสดงผล`, " บาท"),
          p(strong("ขนาดบรรจุ: "), item$`ขนาดบรรจุจริง`),
          p(strong("ราคาต่อหน่วย: "), ifelse(is.na(item$cost_per_unit), "-", paste0(item$cost_per_unit, " บาท/หน่วย"))),
          p(strong("ราคาทุน: "), ifelse(is.na(item$`ต้นทุนจริง`), "-", paste0(item$`ต้นทุนจริง`, " บาท"))),
          p(strong("กำไรต่อหน่วย: "), ifelse(is.na(item$`กำไรต่อหน่วย`), "-", paste0(item$`กำไรต่อหน่วย`, " บาท"))),
          p(strong("อัตรากำไร: "), ifelse(is.na(item$`อัตรากำไร (%)`), "-", paste0(item$`อัตรากำไร (%)`, "%"))),
          hr(),
          p(strong("สารสำคัญและปริมาณ:")),
          p(item$`สารสำคัญและปริมาณ`),
          hr(),
          p(strong("ข้อบ่งใช้ / ประโยชน์:")),
          p(item$`ข้อบ่งใช้จริง`, style = "color:#16a085;"),
          hr(),
          p(strong("วิธีรับประทาน:")),
          p(item$`วิธีทานจริง`),
          hr(),
          p(strong("ข้อควรระวัง / ข้อห้ามใช้:")),
          p(item$`ข้อควรระวังจริง`, style = "color:#c0392b;")
        )
      )
    })
    
    do.call(fluidRow, cols)
  })
}

# ==========================================
# 4. เปิด Web Application
# ==========================================

shinyApp(
  ui = ui,
  server = server
)