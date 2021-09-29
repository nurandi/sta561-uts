server <- function(input, output) {
    
    # Apakah memilih Nasional atau Provinsi?
    regType  <- reactive({ifelse(input$selectProv == "NASIONAL", "Nasional", "Provinsi")})
    regType2 <- reactive({ifelse(input$selectProv == "NASIONAL", "Provinsi", "Kabupaten/Kota")})
    
    # Data periode terakhir
    ipmLatest <- reactive({
        ipm %>% filter(Reg2 == input$selectProv & Tahun == latestYear) })

    ipmReg2 <- reactive({
        ipmLatest() %>% head(1) })
    
    # Data historis
    ipmHistoris <- reactive({
        ipm %>% filter(Reg == "NASIONAL" | Reg == input$selectProv) })

    # Bar chart untuk menampilkan IPM periode terakhir
    # IPM    
    output$ipmChartLatest <- renderPlot({
        generateChart(ipmLatest(), xvar = Reg, yvar = IPM, refline = ipmReg2()$IPM2, fillvar = IPMs,
                 llegend = paste0(c("< IPM ", "> IPM "), regType()),
                 xtitle = regType2(),
                 ytitle = "Indeks Pembangunan Manusia (IPM)")
    })
    
    output$ipmChartDiff <- renderPlot({
        generateChart(ipmLatest(), xvar = Reg, yvar = IPMd, fillvar = IPMs,
                      llegend = paste0(c("< IPM ", "> IPM "), regType()),
                      xtitle = regType2(),
                      ytitle = paste0("Selisih terhadap IPM ", regType()),
                      gtitle = paste0("Perbandingan dengan IPM ", regType()))
    })
    
    # AHH
    output$ahhChartLatest <- renderPlot({
        generateChart(ipmLatest(), xvar = Reg, yvar = AHH, refline = ipmReg2()$AHH2, fillvar = AHHs,
                 llegend = paste0(c("< AHH ", "> AHH "), regType()),
                 xtitle = regType2(),
                 ytitle = "Angka Harapan Hidup (Tahun)")
    })
    
    output$ahhChartDiff <- renderPlot({
        generateChart(ipmLatest(), xvar = Reg, yvar = AHHd, fillvar = AHHs,
                      llegend = paste0(c("< IPM ", "> IPM "), regType()),
                      xtitle = regType2(),
                      ytitle = paste0("Selisih terhadap AHH ", regType()),
                      gtitle = paste0("Perbandingan dengan AHH ", regType()))
    })
    
    # HLS
    output$eysChartLatest <- renderPlot({
        generateChart(ipmLatest(), xvar = Reg, yvar = EYS, refline = ipmReg2()$EYS2, fillvar = EYSs,
                 llegend = paste0(c("< HLS ", "> HLS "), regType()),
                 xtitle = regType2(),
                 ytitle = "Harapan Lama Sekolah (Tahun)")
    })
    
    output$eysChartDiff <- renderPlot({
        generateChart(ipmLatest(), xvar = Reg, yvar = EYSd, fillvar = EYSs,
                      llegend = paste0(c("< IPM ", "> IPM "), regType()),
                      xtitle = regType2(),
                      ytitle = paste0("Selisih terhadap HLS ", regType()),
                      gtitle = paste0("Perbandingan dengan HLS ", regType()))
    })
    
    
    # RLS
    output$mysChartLatest <- renderPlot({
        generateChart(ipmLatest(), xvar = Reg, yvar = MYS, refline = ipmReg2()$MYS2, fillvar = MYSs,
                 llegend = paste0(c("< RLS ", "> RLS "), regType()),
                 xtitle = regType2(),
                 ytitle = "Rata-rata Lama Sekolah (Tahun)")
    })
    
    output$mysChartDiff <- renderPlot({
        generateChart(ipmLatest(), xvar = Reg, yvar = MYSd, fillvar = MYSs,
                      llegend = paste0(c("< IPM ", "> IPM "), regType()),
                      xtitle = regType2(),
                      ytitle = paste0("Selisih terhadap RLS ", regType()),
                      gtitle = paste0("Perbandingan dengan RLS ", regType()))
    })
    
    # PPP
    output$pppChartLatest <- renderPlot({
        generateChart(ipmLatest(), xvar = Reg, yvar = PPP, refline = ipmReg2()$PPP2, fillvar = PPPs,
                 llegend = paste0(c("< PPP ", "> PPP "), regType()),
                 xtitle = regType2(),
                 ytitle = "Pengeluaran Per Kapita (Juta Rupiah/Tahun)")
    })
    
    output$pppChartDiff <- renderPlot({
        generateChart(ipmLatest(), xvar = Reg, yvar = PPPd, fillvar = PPPs,
                      llegend = paste0(c("< IPM ", "> IPM "), regType()),
                      xtitle = regType2(),
                      ytitle = paste0("Selisih terhadap PPP ", regType()),
                      gtitle = paste0("Perbandingan dengan PPP ", regType()))
    })
    
    
    # Line chart untuk menampilkan IPM historis
    # IPM    
    output$ipmChartHistoris <- renderPlot({
        generateChart(ipmHistoris(), chart = "line",
                      xvar = Tahun, yvar = IPM, fillvar = Reg,
                      llegend = unique(ipmHistoris()$Reg),
                      xtitle = "Tahun",
                      ytitle = "Indeks Pembangunan Manusia (IPM)",
                      gtitle = "Trend Indeks Pembangunan Manusia")
    })
    
    # AHH    
    output$ahhChartHistoris <- renderPlot({
        generateChart(ipmHistoris(), chart = "line",
                      xvar = Tahun, yvar = AHH, fillvar = Reg,
                      llegend = unique(ipmHistoris()$Reg),
                      xtitle = "Tahun",
                      ytitle = "Angka Harapan Hidup (Tahun)",
                      gtitle = "Trend Angka Harapan Hidup")
    })
    
    # RLS/MYS    
    output$mysChartHistoris <- renderPlot({
        generateChart(ipmHistoris(), chart = "line",
                      xvar = Tahun, yvar = MYS, fillvar = Reg,
                      llegend = unique(ipmHistoris()$Reg),
                      xtitle = "Tahun",
                      ytitle = "Rata-rata Lama Sekolah (Tahun)",
                      gtitle = "Trend Rata-rata Lama Sekolah")
    })
    
    # HLS/EYS   
    output$eysChartHistoris <- renderPlot({
        generateChart(ipmHistoris(), chart = "line",
                      xvar = Tahun, yvar = EYS, fillvar = Reg,
                      llegend = unique(ipmHistoris()$Reg),
                      xtitle = "Tahun",
                      ytitle = "Harapan Lama Sekolah (Tahun)",
                      gtitle = "Trend Harapan Lama Sekolah")
    })
    
    # PPP  
    output$pppChartHistoris <- renderPlot({
        generateChart(ipmHistoris(), chart = "line",
                      xvar = Tahun, yvar = PPP, fillvar = Reg,
                      llegend = unique(ipmHistoris()$Reg),
                      xtitle = "Tahun",
                      ytitle = "Pendapatan Per Kapita (Jutaan Rupiah/Tahun)",
                      gtitle = "Trend Pendapatan Per Kapita")
    })
    
    
    ## Peta
    
 
    mapData <- reactive({
        if (input$selectProv == "NASIONAL") {
            d <- ipm %>% 
                filter(Reg2 == "NASIONAL" & Tahun == latestYear) %>%
                inner_join(mapProv) 
        } 
        else {
            d <- ipm %>% 
                filter(Reg2 == input$selectProv & Tahun == latestYear) %>%
                inner_join(mapKab) 
        }
        return(d)
    }) %>%
        bindCache(input$selectProv)
    
    # ggplot
    output$ipmMap <- renderPlot({
        mapData() %>%
        ggplot() +  
        geom_sf(aes(geometry = geometry, fill=IPM)) +
            scale_fill_gradientn(colors = c('#CC7351','#E08F62','#DED7B1','#9DAB86','#3A6351')) +
            #scale_fill_gradientn(colors = colorRampPalette(rev(brewer.pal(10, "BrBG")))(6)) +
            theme_ipsum() +
            theme(axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank())
    })
    
}
