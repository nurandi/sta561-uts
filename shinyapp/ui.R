ui <- tagList(
    
    tags$head(
        tags$title("Tugas Praktikum Sesi UTS .:. STA561 .:. P1-K5"),
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ), 
    
    # theme selector
    # shinythemes::themeSelector(),  
    
    navbarPage(
        theme = shinytheme("sandstone"), 
        title = "P1-K5",
        
        # uncomment to use shiny dashboard module
        # header = tagList(useShinydashboard()), 

        tabPanel("About",
                 sidebarPanel(includeMarkdown("md/about-team.md")),
                 mainPanel(includeHTML("md/desc.html"))),
        
        tabPanel("Generating Data",
                 mainPanel(includeHTML("md/01-uts.html"), width = "100%")),
        tabPanel("PCR Function",
                 mainPanel(includeHTML("md/02-rku.html"), width = "100%")),
        tabPanel("PCR For Multicolinearity",
                 mainPanel("on progress", width = "100%")),
        tabPanel("Data Munging",
                 mainPanel(includeHTML("md/04-daman.html"), width = "100%")),
        
        tabPanel("Data Visualization",
                 sidebarPanel(
                     
                     selectInput(inputId = "selectProv", 
                                 label = h4("Pilih Provinsi"), 
                                 choices = provList,
                                 selected = provList[1]),
                     
                     includeMarkdown("md/about.md")
                 ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Peta Wilayah",
                                  h2("Peta Wilayah"),
                                  plotOutput("ipmMap", width = "100%", height = 400)),
                         tabPanel("IPM", 
                                  h2("Indeks Pembangunan Manusia"),
                                  plotOutput("ipmChartLatest", width = "100%", height = 500),
                                  plotOutput("ipmChartDiff", width = "100%", height = 500),
                                  plotOutput("ipmChartHistoris", width = "100%", height = 500)),
                         
                         tabPanel("AHH", 
                                  h2("Angka Harapan Hidup"),
                                  plotOutput("ahhChartLatest", width = "100%", height = 500),
                                  plotOutput("ahhChartDiff", width = "100%", height = 500),
                                  plotOutput("ahhChartHistoris", width = "100%", height = 500)),
                         
                         tabPanel("RLS", 
                                  h2("Rata-rata Lama Sekolah"),
                                  plotOutput("mysChartLatest", width = "100%", height = 500),
                                  plotOutput("mysChartDiff", width = "100%", height = 500),
                                  plotOutput("mysChartHistoris", width = "100%", height = 500)),
                         
                         tabPanel("HLS", 
                                  h2("Harapan Lama Sekolah"),
                                  plotOutput("eysChartLatest", width = "100%", height = 500),
                                  plotOutput("eysChartDiff", width = "100%", height = 500),
                                  plotOutput("eysChartHistoris", width = "100%", height = 500)),
                         
                         tabPanel("Pengeluaran", 
                                  h2("Pengeluaran per Kapita"),
                                  plotOutput("pppChartLatest", width = "100%", height = 500),
                                  plotOutput("pppChartDiff", width = "100%", height = 500),
                                  plotOutput("pppChartHistoris", width = "100%", height = 500))
                         
                         )
                     )
                ),
            tabPanel("Shinyapp",
                    mainPanel(includeHTML("md/09-shiny.html"), width = "100%"))
    )
)
