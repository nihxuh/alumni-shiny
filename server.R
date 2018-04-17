## server.R ## 
function(input, output, session) {
  # set ggplot2 default font size to 12
  ggplot2::theme_set(theme_classic(base_size = 14))

  # wrap string
  wrap_strings <- function(vector_of_strings,width) {
    new_stringvector <- gsub('/', ' / ', vector_of_strings)
    sapply(new_stringvector,FUN = function(x) {paste(strwrap(x,width=width), collapse="\n")})
  }
  
  # (3/26/18) change data table header
  changeTableHeader <- function(inTable, newHeader) {
    outTable = inTable
    colnames(outTable) <- newHeader
    return(outTable)
  }
  
  # (1/22/18 hack) convert time period
  timeConvert <- function(disp_time) {
    dfTime <- data.frame("key" = c('Left NIEHS in 2000-2004',
                                   'Left NIEHS in 2005-2009',
                                   'Left NIEHS in 2010-2014',
                                   'Left NIEHS in 2000-2014'), 
                         "value" = c('2000-2004',
                                     '2005-2009',
                                     '2010-2014',
                                     '2000-2014'))
    result <- as.character(dfTime$value[dfTime$key == disp_time])
    return(result)
  }
  
  # count of alumni
  alnCount = nrow(dataGroup)
  
  # coverage
  cUKnwn = table(dataGroup[,"job_sector"])
  covRate = paste0(round(1 - cUKnwn["Unknown or Undecided"] / alnCount, 3) * 100, "%")
  
  # count job categories
  cntSect = length(unique(dataGroup[,"job_sector"]))
  cntType = length(unique(dataGroup[,"job_type"]))
  cntSpec = 20
  # cntSpec = length(unique(dataGroup[,"specifics"]))
  
  output$years <- renderValueBox({
    valueBox(
      value = totalYrs,
      subtitle = "Postdocs became alumni in this time span",
      icon = icon("calendar")
    )
  })
  
  output$total <- renderValueBox({
    # Total number of postdoc alumni.
    valueBox(
      value = alnCount,
      subtitle = "Total alumni",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$coverage <- renderValueBox({
    valueBox(
      value = covRate,
      subtitle = "Alumni with known outcome snapshots",
      icon = icon("pie-chart"),
      color = "yellow"
    )
  })
  
  output$jobSect <- renderValueBox({
    # number of job sector
    valueBox(
      subtitle = "Job sector definitions",
      value = cntSect,
      icon = icon("th-large"),
      color = "maroon"
    )
  })

  output$jobType <- renderValueBox({
    # number of job type
    valueBox(
      subtitle = "Job type definitions",
      value = cntType,
      icon = icon("th-list"),
      color = "green"
    )
  })

  output$jobSpec <- renderValueBox({
    # number of job specifics
    valueBox(
      subtitle = "Job specifics definitions",
      value = cntSpec,
      icon = icon("th"),
      color = "blue"
    )
  })
  
  output$jobExample <- renderValueBox({
    # number of job specifics
    valueBox(
      subtitle = "Job classification examples",
      value = 23,
      icon = icon("table"),
      color = "yellow"
    )
  })

  ##### https://stackoverflow.com/questions/32971921/navigate-to-particular-sidebar-menu-item-in-shinydashboard
  ##### https://stackoverflow.com/questions/37169039/direct-link-to-tabitem-with-r-shiny-dashboard
  
  # # dynamic input in sidebar
  # output$selectInput <- renderUI({
  #   if(input$tabs == "demograph") {
  #     menuItem("Select setting", startExpanded = TRUE,
  #       menuSubItem(# select time period
  #         selectInput("selectDmTp", label = "Time period",
  #                     choices = c(timePeriods,"Total","Trend"))),
  #       menuSubItem(# select plot style
  #         selectInput("selectDmPc", label = "Plot category",
  #                     choices = list("By job sector"=1, "By job type"=2, 
  #                                    "By job specifics"=3), selected = 1))
  #     )
  #   }
  # })
  
  #############################################################################
  # Demographics >>>
  #############################################################################

  # demographics data
  dmHeight <- function() {
    input$sldHeightDm
  }
  dmWidth <- function() {
    input$sldWidthDm
  }
  
  # likert plot help function
  likertHelper <- function(dataAll, dataCat, inType, titleAll, titleCat, inColors) {
    # convert data format for likert data
    pctAll <- round(dataAll/rowSums(dataAll)*100,2)
    pctCat <- round(dataCat[-1]/rowSums(dataCat[-1])*100,2)
    ldAll <- data.frame(Item = c(' '), pctAll)
    ldCat <- data.frame(dataCat[,1], pctCat)
    colnames(ldCat)[1] <- "Item"
    dcAll = data.frame(Item = ' ', Count = rowSums(dataAll))
    dcCat = data.frame(ldCat[,1], data.frame(rowSums(dataCat[-1])))
    colnames(dcCat)[1] <- "Item"
    colnames(dcCat)[2] <- "Count"
    
    # likert plot
    ltAll <- likert(summary=ldAll)
    # p1 <- plot(ltAll, colors=inColors, text.size=4) + ggtitle(titleAll) + theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12,face="bold"))
    p1 <- plot(ltAll, colors=inColors) + ggtitle(titleAll) + theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12,face="bold"))
    p1 <- p1 + guides(fill=guide_legend(title=inType)) + geom_label(data=dcAll, aes(x=Item, y = 1, label=Count))
          # annotate("text", x=dcAll$Item, y = 1, label=dcAll$Count, colour = "#e6e600", fontface =2)
    ltCat <- likert(summary=ldCat)
    # p2 <- plot(ltCat, colors=inColors, ordered = FALSE, text.size=4) + ggtitle(titleCat) + theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12,face="bold"))
    p2 <- plot(ltCat, colors=inColors, ordered = FALSE) + ggtitle(titleCat) + theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12,face="bold"))
    p2 <- p2 + guides(fill=guide_legend(title=inType)) + geom_label(data=dcCat, aes(x=Item, y = 1, label=Count))
          # annotate("text", x=dcCat$Item, y = 1, label=dcCat$Count, colour = "#e6e600", fontface =2)
    
    grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
  }
  
  # gender colors
  genderColors = c("#D81B60", "#4F94CD")
  
  # plot gender data
  output$genderLtPlot <- renderPlot({
    # choose data based on input$selectDmTp from ui.R
    if (input$selectDmTp == 'Left NIEHS in 2000-2014') {
      # change for Likert plot (3/15/18)
      if (input$selectDmPc == 1) {
        titleAll = paste0("Gender distribution in all data \n", totalYrs)
        titleCat = paste0("Gender distribution in each job sector \n", totalYrs)
        likertHelper(gend4All, gend4Sect, 'Gender', titleAll, titleCat, genderColors)
      }
      else if (input$selectDmPc == 2) {
        titleAll = paste0("Gender distribution in all data \n", totalYrs)
        titleCat = paste0("Gender distribution in each job type \n", totalYrs)
        likertHelper(gend4All, gend4Type, 'Gender', titleAll, titleCat, genderColors)
      }
      else {
        titleAll = paste0("Gender distribution in all data \n", totalYrs)
        titleCat = paste0("Gender distribution in each job specifics \n", totalYrs)
        likertHelper(gend4All, gend4Spec, 'Gender', titleAll, titleCat, genderColors)
      }
    }
    else if (input$selectDmTp == 'Trend') {
      # if (input$selectDmPc == 1) {
      #   p1 <- likert(years ~ ., gendYrAll, as.percent=TRUE, main=paste0("Gender distribution in data \n", trendYrs), col=genderColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   p2 <- likert(years ~ . | job_sector, gendYrSect, as.percent=TRUE, main=paste0("Gender distribution in each job sector \n", trendYrs), col=genderColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
      # }
      # else if (input$selectDmPc == 2) {
      #   p1 <- likert(years ~ ., gendYrAll, as.percent=TRUE, main=paste0("Gender distribution in data \n", trendYrs), col=genderColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   p2 <- likert(years ~ . | job_type, gendYrType, as.percent=TRUE, main=paste0("Gender distribution in each job type \n", trendYrs), col=genderColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
      # }
      # else {
      #   p1 <- likert(years ~ ., gendYrAll, as.percent=TRUE, main=paste0("Gender distribution in data \n", trendYrs), col=genderColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   p2 <- likert(years ~ . | specifics, gendYrSpec, as.percent=TRUE, main=paste0("Gender distribution in each job specifics \n", trendYrs), col=genderColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
      # }
    }
    else {
      selectYears = timeConvert(input$selectDmTp)
      pltGndAll <- subset(gendYrAll, years == selectYears)
      pltGndAll$years <- NULL
      pltGndSec <- subset(gendYrSect, years == selectYears)
      pltGndSec$years <- NULL
      pltGndTyp <- subset(gendYrType, years == selectYears)
      pltGndTyp$years <- NULL
      pltGndSpe <- subset(gendYrSpec, years == selectYears)
      pltGndSpe$years <- NULL
      
      # change for Likert plot (3/15/18)
      if (input$selectDmPc == 1) {
        titleAll = paste0("Gender distribution in all data \n", selectYears)
        titleCat = paste0("Gender distribution in each job sector \n", selectYears)
        likertHelper(pltGndAll, pltGndSec, 'Gender', titleAll, titleCat, genderColors)
      }
      else if (input$selectDmPc == 2) {
        titleAll = paste0("Gender distribution in all data \n", selectYears)
        titleCat = paste0("Gender distribution in each job type \n", selectYears)
        likertHelper(pltGndAll, pltGndTyp, 'Gender', titleAll, titleCat, genderColors)
      }
      else {
        titleAll = paste0("Gender distribution in all data \n", selectYears)
        titleCat = paste0("Gender distribution in each job specifics \n", selectYears)
        likertHelper(pltGndAll, pltGndSpe, 'Gender', titleAll, titleCat, genderColors)
      }
    }
  },height=dmHeight,width=dmWidth)
  output$genderLtTxt  <- renderText({HTML("<p><strong>Likert Plot of Gender Distribution.</strong> <em>Upper panel</em>: Relative percentages of male and female NIEHS alumni in the selected time period(s).
                                          <em>Lower panel</em>: Relative percentages of male and female NIEHS alumni in each job category during selected time period(s). 
                                          The sum of male and female percentages in each row equals 100%. The number in the middle of each row indicates total number of alumni in a job category.</p><p>&nbsp;</p>")})

  output$genderPbPlot <- renderPlot({
    # choose data based on input$selectDmTp from ui.R
    if (input$selectDmTp == 'Left NIEHS in 2000-2014') {
      if (input$selectDmPc == 1) {
        gend4Sect.m <- melt(gend4Sect, id.vars = "job_sector")
        colnames(gend4Sect.m)[colnames(gend4Sect.m)=="variable"] <- "gender"
        p <- ggplot(gend4Sect.m, aes(job_sector, value)) + labs(x="Job Sector", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=genderColors)
      }
      else if (input$selectDmPc == 2) {
        gend4Type.m <- melt(gend4Type, id.vars = "job_type")
        colnames(gend4Type.m)[colnames(gend4Type.m)=="variable"] <- "gender"
        p <- ggplot(gend4Type.m, aes(job_type, value)) + labs(x="Job Type", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=genderColors)
      }
      else {
        # gdr4Spec <- gend4Spec[grepl('research', gend4Spec$specifics),]
        gdr4Spec <- gend4Spec
        gend4Spec.m <- melt(gdr4Spec, id.vars = "specifics")
        colnames(gend4Spec.m)[colnames(gend4Spec.m)=="variable"] <- "gender"
        p <- ggplot(gend4Spec.m, aes(specifics, value)) + labs(x="Job Specifics", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=genderColors)
      }
    }
    else if (input$selectDmTp == 'Trend') {
      if (input$selectDmPc == 1) {
        gdrYrSect.m <- melt(gendYrSect, id.vars = c("job_sector","years"))
        colnames(gdrYrSect.m)[colnames(gdrYrSect.m)=="variable"] <- "gender"
        p <- ggplot(gdrYrSect.m, aes(job_sector, value)) + labs(x="Job Sector", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( ~ years) + scale_fill_manual(values=genderColors)
      }
      else if (input$selectDmPc == 2) {
        gdrYrType.m <- melt(gendYrType, id.vars = c("job_type","years"))
        colnames(gdrYrType.m)[colnames(gdrYrType.m)=="variable"] <- "gender"
        p <- ggplot(gdrYrType.m, aes(job_type, value)) + labs(x="Job Type", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( ~ years) + scale_fill_manual(values=genderColors)
      }
      else {
        gdrYrSpec.m <- melt(gendYrSpec, id.vars = c("specifics","years"))
        colnames(gdrYrSpec.m)[colnames(gdrYrSpec.m)=="variable"] <- "gender"
        p <- ggplot(gdrYrSpec.m, aes(specifics, value)) + labs(x="Job Specifics", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( ~ years) + scale_fill_manual(values=genderColors)
      }
    }
    else {
      selectYears = timeConvert(input$selectDmTp)
      pltGndAll <- subset(gendYrAll, years == selectYears)
      pltGndSec <- subset(gendYrSect, years == selectYears)
      pltGndTyp <- subset(gendYrType, years == selectYears)
      pltGndSpe <- subset(gendYrSpec, years == selectYears)
      
      # plot
      if (input$selectDmPc == 1) {
        gdrPlSect.m <- melt(pltGndSec, id.vars = c("job_sector","years"))
        colnames(gdrPlSect.m)[colnames(gdrPlSect.m)=="variable"] <- "gender"
        p <- ggplot(gdrPlSect.m, aes(job_sector, value)) + labs(x="Job Sector", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=genderColors)
      }
      else if (input$selectDmPc == 2) {
        gdrPlType.m <- melt(pltGndTyp, id.vars = c("job_type","years"))
        colnames(gdrPlType.m)[colnames(gdrPlType.m)=="variable"] <- "gender"
        p <- ggplot(gdrPlType.m, aes(job_type, value)) + labs(x="Job Type", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=genderColors)
      }
      else {
        gdrPlSpec.m <- melt(pltGndSpe, id.vars = c("specifics","years"))
        colnames(gdrPlSpec.m)[colnames(gdrPlSpec.m)=="variable"] <- "gender"
        p <- ggplot(gdrPlSpec.m, aes(specifics, value)) + labs(x="Job Specifics", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=genderColors)
      }
    }
    p
  },height=dmHeight,width=dmWidth)
  output$genderPbTxt  <- renderText({HTML("<p><strong>Bar Chart of Gender Distribution.</strong> Absolute number of male and female NIEHS alumni in each job category during selected time period(s).</p><p>&nbsp;</p>")})
  
  # output gender table
  output$genderTable <- renderTable({
    # choose data based on input$selectGt from ui.R
    if (input$selectDmTp == 'Left NIEHS in 2000-2014') {
      if (input$selectDmPc == 1) {
        changeTableHeader(gend4Sect, c('Job_Sector','Female','Male'))
      }
      else if (input$selectDmPc == 2) {
        changeTableHeader(gend4Type, c('Job_Type','Female','Male'))
      }
      else {
        changeTableHeader(gend4Spec, c('Job_Specifics','Female','Male'))
      }
    }
    else if (input$selectDmTp == 'Trend') {
      if (input$selectDmPc == 1) {
        changeTableHeader(gendYrSect, c('Years','Job_Sector','Female','Male'))
      }
      else if (input$selectDmPc == 2) {
        changeTableHeader(gendYrType, c('Years','Job_Type','Female','Male'))
      }
      else {
        changeTableHeader(gendYrSpec, c('Years','Job_Specifics','Female','Male'))
      }
    }
    else {
      selectYears = timeConvert(input$selectDmTp)
      pltGndSec <- subset(gendYrSect, years == selectYears)
      pltGndTyp <- subset(gendYrType, years == selectYears)
      pltGndSpe <- subset(gendYrSpec, years == selectYears)
      if (input$selectDmPc == 1) {
        changeTableHeader(pltGndSec, c('Years','Job_Sector','Female','Male'))
      }
      else if (input$selectDmPc == 2) {
        changeTableHeader(pltGndTyp, c('Years','Job_Type','Female','Male'))
      }
      else {
        changeTableHeader(pltGndSpe, c('Years','Job_Specifics','Female','Male'))
      }
    }
  }, digits = 0)
  
  
  # plot citizenship data
  # visiting colors
  visitColors = c("#47427e","#6cc06d")
  
  output$visitLtPlot <- renderPlot({
    # choose data based on input$selectDmTp from ui.R
    if (input$selectDmTp == 'Left NIEHS in 2000-2014') {
      # change for Likert plot (3/15/18)
      if (input$selectDmPc == 1) {
        titleAll = paste0("Country origin distribution in all data \n", totalYrs)
        titleCat = paste0("Country origin distribution in each job sector \n", totalYrs)
        likertHelper(citi4All, citi4Sect, 'Country', titleAll, titleCat, visitColors)
      }
      else if (input$selectDmPc == 2) {
        titleAll = paste0("Country origin distribution in all data \n", totalYrs)
        titleCat = paste0("Country origin distribution in each job type \n", totalYrs)
        likertHelper(citi4All, citi4Type, 'Country', titleAll, titleCat, visitColors)
      }
      else {
        titleAll = paste0("Country origin distribution in all data \n", totalYrs)
        titleCat = paste0("Country origin distribution in each job specifics \n", totalYrs)
        likertHelper(citi4All, citi4Spec, 'Country', titleAll, titleCat, visitColors)
      }
    }
    else if (input$selectDmTp == 'Trend') {
      # if (input$selectDmPc == 1) {
      #   p1 <- likert(years ~ ., citiYrAll, as.percent=TRUE, main=paste0("Country origin distribution in data \n", trendYrs), col=visitColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   p2 <- likert(years ~ . | job_sector, citiYrSect, as.percent=TRUE, main=paste0("Country origin distribution in each job sector \n", trendYrs), col=visitColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
      # }
      # else if (input$selectDmPc == 2) {
      #   p1 <- likert(years ~ ., citiYrAll, as.percent=TRUE, main=paste0("Country origin distribution in data \n", trendYrs), col=visitColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   p2 <- likert(years ~ . | job_type, citiYrType, as.percent=TRUE, main=paste0("Country origin distribution in each job type \n", trendYrs), col=visitColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
      # }
      # else {
      #   p1 <- likert(years ~ ., citiYrAll, as.percent=TRUE, main=paste0("Country origin distribution in data \n", trendYrs), col=visitColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   p2 <- likert(years ~ . | specifics, citiYrSpec, as.percent=TRUE, main=paste0("Country origin distribution in each job specific \n", trendYrs), col=visitColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
      # }
    }
    else {
      selectYears = timeConvert(input$selectDmTp)
      pltCtzAll <- subset(citiYrAll, years == selectYears)
      pltCtzAll$years <- NULL
      pltCtzSec <- subset(citiYrSect, years == selectYears)
      pltCtzSec$years <- NULL
      pltCtzTyp <- subset(citiYrType, years == selectYears)
      pltCtzTyp$years <- NULL
      pltCtzSpe <- subset(citiYrSpec, years == selectYears)
      pltCtzSpe$years <- NULL
      
      # change for Likert plot (3/15/18)
      if (input$selectDmPc == 1) {
        titleAll = paste0("Country origin distribution in all data \n", selectYears)
        titleCat = paste0("Country origin distribution in each job sector \n", selectYears)
        likertHelper(pltCtzAll, pltCtzSec, 'Country', titleAll, titleCat, visitColors)
      }
      else if (input$selectDmPc == 2) {
        titleAll = paste0("Country origin distribution in all data \n", selectYears)
        titleCat = paste0("Country origin distribution in each job type \n", selectYears)
        likertHelper(pltCtzAll, pltCtzTyp, 'Country', titleAll, titleCat, visitColors)
      }
      else {
        titleAll = paste0("Country origin distribution in all data \n", selectYears)
        titleCat = paste0("Country origin distribution in each job specifics \n", selectYears)
        likertHelper(pltCtzAll, pltCtzSpe, 'Country', titleAll, titleCat, visitColors)
      }
    }
  },height=dmHeight,width=dmWidth)
  output$visitLtTxt  <- renderText({HTML("<p><strong>Likert Plot of Country Origin Distribution.</strong> <em>Upper panel</em>: Relative percentages of US and international alumni in the selected time period(s).
                                          <em>Lower panel</em>: Relative percentages of US and international alumni in each job category during selected time period(s). 
                                          The sum of US and international percentages in each row equals 100%. The number in the middle of each row indicates total number of alumni in a job category.</p><p>&nbsp;</p>")})
  
  output$visitPbPlot <- renderPlot({
    # choose data based on input$selectDmTp from ui.R
    if (input$selectDmTp == 'Left NIEHS in 2000-2014') {
      if (input$selectDmPc == 1) {
          citi4Sect.m <- melt(citi4Sect, id.vars = "job_sector")
          colnames(citi4Sect.m)[colnames(citi4Sect.m)=="variable"] <- "visiting"
          p <- ggplot(citi4Sect.m, aes(job_sector, value)) + labs(x="Job Sector", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=visitColors)
      }
      else if (input$selectDmPc == 2) {
          citi4Type.m <- melt(citi4Type, id.vars = "job_type")
          colnames(citi4Type.m)[colnames(citi4Type.m)=="variable"] <- "visiting"
          p <- ggplot(citi4Type.m, aes(job_type, value)) + labs(x="Job Type", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=visitColors)
      }
      else {
          citi4Spec.m <- melt(citi4Spec, id.vars = "specifics")
          colnames(citi4Spec.m)[colnames(citi4Spec.m)=="variable"] <- "visiting"
          p <- ggplot(citi4Spec.m, aes(specifics, value)) + labs(x="Job Specifics", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=visitColors)
      }
    }
    else if (input$selectDmTp == 'Trend') {
      if (input$selectDmPc == 1) {
          vnvYrSect.m <- melt(citiYrSect, id.vars = c("job_sector","years"))
          colnames(vnvYrSect.m)[colnames(vnvYrSect.m)=="variable"] <- "visiting"
          p <- ggplot(vnvYrSect.m, aes(job_sector, value)) + labs(x="Job Sector", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( ~ years) + scale_fill_manual(values=visitColors)
      }
      else if (input$selectDmPc == 2) {
          vnvYrType.m <- melt(citiYrType, id.vars = c("job_type","years"))
          colnames(vnvYrType.m)[colnames(vnvYrType.m)=="variable"] <- "visiting"
          p <- ggplot(vnvYrType.m, aes(job_type, value)) + labs(x="Job Type", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( ~ years) + scale_fill_manual(values=visitColors)
      }
      else {
          vnvYrSpec.m <- melt(citiYrSpec, id.vars = c("specifics","years"))
          colnames(vnvYrSpec.m)[colnames(vnvYrSpec.m)=="variable"] <- "visiting"
          p <- ggplot(vnvYrSpec.m, aes(specifics, value)) + labs(x="Job Specifics", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( ~ years) + scale_fill_manual(values=visitColors)
      }
    }
    else {
      selectYears = timeConvert(input$selectDmTp)
      pltCtzAll <- subset(citiYrAll, years == selectYears)
      pltCtzSec <- subset(citiYrSect, years == selectYears)
      pltCtzTyp <- subset(citiYrType, years == selectYears)
      pltCtzSpe <- subset(citiYrSpec, years == selectYears)
      
      # plot
      if (input$selectDmPc == 1) {
          vnvPlSect.m <- melt(pltCtzSec, id.vars = c("job_sector","years"))
          colnames(vnvPlSect.m)[colnames(vnvPlSect.m)=="variable"] <- "visiting"
          p <- ggplot(vnvPlSect.m, aes(job_sector, value)) + labs(x="Job Sector", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=visitColors)
      }
      else if (input$selectDmPc == 2) {
          vnvPlType.m <- melt(pltCtzTyp, id.vars = c("job_type","years"))
          colnames(vnvPlType.m)[colnames(vnvPlType.m)=="variable"] <- "visiting"
          p <- ggplot(vnvPlType.m, aes(job_type, value)) + labs(x="Job Type", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=visitColors)
      }
      else {
          vnvPlSpec.m <- melt(pltCtzSpe, id.vars = c("specifics","years"))
          colnames(vnvPlSpec.m)[colnames(vnvPlSpec.m)=="variable"] <- "visiting"
          p <- ggplot(vnvPlSpec.m, aes(specifics, value)) + labs(x="Job Specifics", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=visitColors)
      }
    }
    p
  },height=dmHeight,width=dmWidth)
  output$visitPbTxt  <- renderText({HTML("<p><strong>Bar Chart of Country Origin Distribution.</strong> Absolute number of US and international alumni in each job category during selected time period(s).</p><p>&nbsp;</p>")})
  
  # output citizen table
  output$visitTable <- renderTable({
    # choose data based on input$selectGt from ui.R
    if (input$selectDmTp == 'Left NIEHS in 2000-2014') {
      if (input$selectDmPc == 1) {
        changeTableHeader(citi4Sect, c('Job_Sector','International','US'))
      }
      else if (input$selectDmPc == 2) {
        changeTableHeader(citi4Type, c('Job_Type','International','US'))
      }
      else {
        changeTableHeader(citi4Spec, c('Job_Specifics','International','US'))
      }
    }
    else if (input$selectDmTp == 'Trend') {
      if (input$selectDmPc == 1) {
        changeTableHeader(citiYrSect, c('Years','Job_Sector','International','US'))
      }
      else if (input$selectDmPc == 2) {
        changeTableHeader(citiYrType, c('Years','Job_Type','International','US'))
      }
      else {
        changeTableHeader(citiYrSpec, c('Years','Job_Specifics','International','US'))
      }
    }
    else {
      pltCtzSec <- subset(citiYrSect, years == timeConvert(input$selectDmTp))
      pltCtzTyp <- subset(citiYrType, years == timeConvert(input$selectDmTp))
      pltCtzSpe <- subset(citiYrSpec, years == timeConvert(input$selectDmTp))
      if (input$selectDmPc == 1) {
        changeTableHeader(pltCtzSec, c('Years','Job_Sector','International','US'))
      }
      else if (input$selectDmPc == 2) {
        changeTableHeader(pltCtzTyp, c('Years','Job_Type','International','US'))
      }
      else {
        changeTableHeader(pltCtzSpe, c('Years','Job_Specifics','International','US'))
      }
    }
  }, digits = 0)
  
  #>>>
  output$demoDynamicUI<-renderUI({
    # zoom gender plot
    if (input$selectShowDm == 1) {
      fluidRow(
        tabBox(
          title = "Gender", width = 12,
          # The id lets us use input$tabset3 on the server to find the current tab
          id = "tab_gender",
          tabPanel("Gender Likert plot", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("genderLtTxt"), plotOutput("genderLtPlot")),
          tabPanel("Gender bar chart", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("genderPbTxt"), plotOutput("genderPbPlot")),
          tabPanel("Gender data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 800px", tableOutput("genderTable"))
        )
      )
    }
    # zoom visiting plot
    else if (input$selectShowDm == 3) {
      fluidRow(
        tabBox(
          title = "Country origin", width = 12,
          # The id lets us use input$tabset4 on the server to find the current tab
          id = "tab_visit",
          tabPanel("Origin Likert plot", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("visitLtTxt"), plotOutput("visitLtPlot")),
          tabPanel("Origin bar chart", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("visitPbTxt"), plotOutput("visitPbPlot")),
          tabPanel("Origin data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 800px", tableOutput("visitTable"))
        )
      )
    }
    # default to show both
    else {
      fluidRow(
        tabBox(
          title = "Gender",
          # The id lets us use input$tab_genderSml on the server to find the current tab
          id = "tab_genderSml",
          tabPanel("Gender Likert plot", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("genderLtTxt"), plotOutput("genderLtPlot")),
          tabPanel("Gender bar chart", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("genderPbTxt"), plotOutput("genderPbPlot")),
          tabPanel("Gender data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 800px", tableOutput("genderTable"))
        ),
        tabBox(
          title = "Country origin",
          # The id lets us use input$tab_visitSml on the server to find the current tab
          id = "tab_visitSml",
          tabPanel("Origin Likert plot", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("visitLtTxt"), plotOutput("visitLtPlot")),
          tabPanel("Origin bar chart", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("visitPbTxt"), plotOutput("visitPbPlot")),
          tabPanel("Origin data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 800px", tableOutput("visitTable"))
        )
      )
    }
  })
  #<<<

  # demographics information boxes
  output$demoInfoBox<-renderUI({
    if (input$selectDmTp == 'Trend') {
      gendData = gendYrAll
      citiData = citiYrAll
      
      # https://stackoverflow.com/questions/15059076/call-apply-like-function-on-each-row-of-dataframe-with-multiple-arguments-from-e
      gendData.m <- melt(gendData, id.vars = "years")
      gendData.nm <- gendData.m %>% group_by(years) %>% mutate(Percent=round(value/sum(value),3)*100)
      femalePct = paste0(gendData.nm[gendData.nm$variable == "Female",]$Percent,"%")
      malePct = paste0(gendData.nm[gendData.nm$variable == "Male",]$Percent,"%")

      citiData.m <- melt(citiData, id.vars = "years")
      citiData.nm <- citiData.m %>% group_by(years) %>% mutate(Percent=round(value/sum(value),3)*100)
      visitPct = paste0(citiData.nm[citiData.nm$variable == "International",]$Percent,"%")
      usaPct = paste0(citiData.nm[citiData.nm$variable == "US",]$Percent,"%")
      
      box(
        width = 12,
        fluidRow(
          infoBox("Female change", paste(femalePct, collapse = " -> "), icon = icon("female"), color = "maroon", fill=TRUE, width=3),
          infoBox("Male change", paste(malePct, collapse = " -> "), icon = icon("male"), color = "blue", fill=TRUE, width=3),
          infoBox("International fellow change", paste(visitPct, collapse = " -> "), icon = icon("globe"), color = "purple", fill=TRUE, width=3),
          infoBox("US fellow change", paste(usaPct, collapse = " -> "), icon = icon("home"), color = "green", fill=TRUE, width=3)
        )
      )
    }
    else {
      if (input$selectDmTp == 'Left NIEHS in 2000-2014') {
        gendData = gend4All
        citiData = citi4All
      }
      else {
        gendData = subset(gendYrAll, years == timeConvert(input$selectDmTp))
        citiData = subset(citiYrAll, years == timeConvert(input$selectDmTp))
      }
      malePct = as.numeric(gendData[,"Male"] / (gendData[,"Male"] + gendData[,"Female"]))
      femalePct = 1 - malePct
      visitPct = as.numeric(citiData[,"International"] / (citiData[,"International"] + citiData[,"US"]))
      usaPct = 1 - visitPct
      
      box(
        width = 12,
        fluidRow(
          infoBox("Female", paste0(round(femalePct, 3)*100, "%"), icon = icon("female"), color = "maroon", fill=TRUE, width=3),
          infoBox("Male", paste0(round(malePct, 3)*100, "%"), icon = icon("male"), color = "blue", fill=TRUE, width=3),
          infoBox("Int'l fellow", paste0(round(visitPct, 3)*100, "%"), icon = icon("globe"), color = "purple", fill=TRUE, width=3),
          infoBox("US fellow", paste0(round(usaPct, 3)*100, "%"), icon = icon("home"), color = "green", fill=TRUE, width=3)
        )
      )
    }
  })
  
  
  #############################################################################
  # Location >>>
  #############################################################################
  
  
  # location data
  lcHeight <- function() {
    input$sldHeightLc
  }
  lcWidth <- function() {
    input$sldWidthLc
  }
  
  # plot migration data
  output$migratePlot <- renderPlot({
    # colors
    myColors <- c(brewer.pal(8,"Dark2"),brewer.pal(12,"Paired"),brewer.pal(12,"Set3"))
    
    # generate df1 for plotting
    createDF1 <- function(iM) {
      #### log2_count <- log2(rowSums(iM)+colSums(iM))
      mTRX <- iM
      
      countD10 <- (rowSums(mTRX)+colSums(mTRX)) / 10.00
      countD10 <- countD10[order(countD10, decreasing = TRUE)]
      srtMTRX <- data.frame(countD10)
      colnames(srtMTRX)[colnames(srtMTRX)=="countD10"] <- "max"
      srtMTRX$country <- rownames(srtMTRX)
      
      nMTRX <- nrow(srtMTRX)
      srtMTRX$col <- myColors[1:nMTRX]
      return(srtMTRX)
    }
    
    # circular plot
    createCirc <- function(iM, iDF) {
      # plot
      circos.clear()
      par(mar = rep(0, 4), cex=0.95)
      circos.par(start.degree = 90, gap.degree = 4)
      p <- chordDiagram(x = iM, directional = TRUE, order = iDF$country, grid.col = iDF$col, annotationTrack = "grid", transparency = 0.25,  preAllocateTracks=list(track.height = 0.33), direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE )
      # add in labels and axis
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim") #
        sector.index = get.cell.meta.data("sector.index")
        # circos.axis("top", labels.away.percentage = 0.1, labels.niceFacing = TRUE)
        circos.text(mean(xlim), ylim[1], sector.index, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
      }, bg.border = NA)
      circos.clear()
    }
    
    if (input$selectLcTp == 'Left NIEHS in 2000-2014') {
      migrate <- makeCircMD(mgrCountryAll)
    }
    else {
      dct <- mgrCountryGroup[mgrCountryGroup$years == timeConvert(input$selectLcTp), ]
      migrate <- makeCircMD(dct)
    }
    
    dfDF1 <- createDF1(migrate)
    createCirc(migrate, dfDF1)
    # })
  },height=lcHeight,width=lcWidth)
  
  output$migrateTxt  <- renderText({
    slctYear <- timeConvert(input$selectLcTp)
    if (input$selectLcTp == 'Left NIEHS in 2000-2014') {
      migrData = dataGroup
    }
    else {
      migrData = subset(dataGroup, years == slctYear)
    }
    
    talMigr = nrow(migrData)
    cntUS = round(sum(migrData$job_country == 'United States') / talMigr, 3) * 100
    
    HTML(paste0("<p><strong>Visualizing Alumni Migration by Pairing Country/Continent of Origin with Country/Continent of Job Location</strong> 
         This circular plot displays the countries/continents of origin alongside the countries/continents of job location (arrows point to job locations). In ",
         slctYear, ", ", cntUS, "% of all alumni remain in the U.S. after training, with fellows from Japan, South Korea, the UK and Germany returning to their home countries at greater proportions that the other countries shown. 
         *=North American countries excluding U.S. & Canada; **=European countries excluding UK & Germany; ***=Asian countries excluding China, Japan, India, & South Korea. 
         Individual countries with enough alumni to visualize are shown in titlecase. Alumni from remaining countries are grouped and depicted by continent in all caps within enclosed brackets.</p><p>&nbsp;</p>"))
  })
  
  # output migration table
  output$migrateTable <- renderTable({
    if (input$selectLcTp == 'Left NIEHS in 2000-2014') {
      migrate <- makeCircMD(mgrCountryAll)
    }
    else {
      dct <- mgrCountryGroup[mgrCountryGroup$years == timeConvert(input$selectLcTp), ]
      migrate <- makeCircMD(dct)
    }
    migrate_df <- as.data.frame(migrate)
    setDT(migrate_df, keep.rownames = TRUE)[]
  })
  
  # http://www.arilamstein.com/blog/2015/07/02/exploring-the-demographics-of-ferguson-missouri/
  highlight_state = function(states) {
    data(state.map, package="choroplethrMaps", envir=environment())
    df = state.map[state.map$region %in% states, ]
    geom_polygon(data=df, aes(long, lat, group = group), color = "red", fill = NA, size = 2)
  }
  
  # plot working state data
  output$statePlot <- renderPlot({
    # choose data based on input$selectLcTp from ui.R
    slctYear <- timeConvert(input$selectLcTp)
    tlYear <- slctYear
    if (input$selectLcTp == 'Left NIEHS in 2000-2014') {
      stData <- dfStateAll
      tlYear <- totalYrs
    }
    else {
      stData <- dfStateYrs[dfStateYrs$years == slctYear, ]
    }
    
    # Setting breakpoints for data
    maxValue <- max(stData$value) # max state value - usually for NC
    minValue <- min(stData$value[stData$value > 0]) # find min positive value
    nV <- length(stData$value)
    s2Value <- sort(stData$value,partial=nV-1)[nV-1]
    
    # http://stackoverflow.com/questions/5746544/r-cut-by-defined-interval
    # https://github.com/trulia/choroplethr/wiki/Choosing-a-Scale-Type
    sclData <- levels(cut(c(minValue, s2Value-1), 3))
    nBrks <- c(0, ceiling( as.numeric(sub("\\((.+),.*", "\\1", sclData)) ), s2Value, maxValue-1, maxValue+1)
    stData$value = Hmisc::cut2(stData$value, cuts=nBrks)
    
    sTitle <- paste0('NIEHS Alumni Job Location in the United States (', tlYear, ')')
    p <- state_choropleth(stData, title = sTitle, legend = "Number of\nalumni") + theme(axis.line=element_blank())
    p <- p + highlight_state("north carolina")
    p
    # })
  },height=lcHeight,width=lcWidth)
  
  output$stateTxt  <- renderText({
    slctYear <- timeConvert(input$selectLcTp)
    if (input$selectLcTp == 'Left NIEHS in 2000-2014') {
      migrData = dataGroup
    }
    else {
      migrData = subset(dataGroup, years == slctYear)
    }
    
    talMigr = nrow(migrData)
    cntNC = sum(migrData$job_state == 'North Carolina')
    talUS = sum(migrData$job_country == 'United States')
    pctNC = round(cntNC / talMigr, 3) * 100
    pctNCUS = round(cntNC / talUS, 3) * 100
    
    
    HTML(paste0("<p><strong>NIEHS Alumni Job Location in the United States</strong> In ", slctYear, ", ", pctNC,  
         "% of ALL alumni work in North Carolina, and when only examining those working within the United States, 
         the proportion in North Carolina is ", pctNCUS, "%. The next highest concentration of alumni includes those in the Maryland/DC metro area. 
         The remaining alumni are distributed across the United States in a manner approximately proportional to state populations.</p><p>&nbsp;</p>"))
  })
  
  # output working state table
  output$stateTable <- renderTable({
    slctYear <- timeConvert(input$selectLcTp)
    if (input$selectLcTp == 'Left NIEHS in 2000-2014') {
      stData <- changeTableHeader(dfStateAll,c('State','Alumni_Count'))
    }
    else {
      stData <- changeTableHeader(dfStateYrs[dfStateYrs$years == slctYear, ], c('Years','State','Alumni_Count'))
    }
    
    setDT(stData)[order(-Alumni_Count)]
  })
  
  #>>> location dynamic UI
  output$locaDynamicUI<-renderUI({
    # zoom migration plot
    if (input$selectShowLc == 1) {
      fluidRow(
        tabBox(
          title = "Alumni migration", width = 12,
          # The id lets us use input$tab_migr on the server to find the current tab
          id = "tab_migr",
          tabPanel("Migration plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("migrateTxt"), plotOutput("migratePlot")),
          tabPanel("Migration data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("migrateTable"))
        )
      )
    }
    # zoom state plot
    else if (input$selectShowLc == 3) {
      fluidRow(
        tabBox(
          title = "Alumni within U.S.", width = 12,
          # The id lets us use input$tab_state on the server to find the current tab
          id = "tab_state",
          tabPanel("State plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("stateTxt"), plotOutput("statePlot")),
          tabPanel("State data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("stateTable"))
        )
      )
    }
    # default to show both
    else {
      fluidRow(
        tabBox(
          title = "Alumni migration",
          # The id lets us use input$tab_migrSml on the server to find the current tab
          id = "tab_migrSml",
          tabPanel("Migration plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("migrateTxt"), plotOutput("migratePlot")),
          tabPanel("Migration data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("migrateTable"))
        ),
        tabBox(
          title = "Alumni within U.S.",
          # The id lets us use input$tab_stateSml on the server to find the current tab
          id = "tab_stateSml",
          tabPanel("State plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("stateTxt"), plotOutput("statePlot")),
          tabPanel("State data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("stateTable"))
        )
      )
    }
  })
  #<<< location dynamic UI
  
  # location information boxes
  output$locaInfoBox<-renderUI({
    if (input$selectLcTp == 'Left NIEHS in 2000-2014') {
      locData = dataGroup
    }
    else {
      locData = dataGroup[dataGroup$years == timeConvert(input$selectLcTp), ]
    }
    
    visTT = nrow(locData[locData$citizenship == 'International',])
    nvsTT = nrow(locData[locData$citizenship == 'US',])
    visUS = nrow(locData[locData$citizenship == 'International' & locData$job_country == 'United States',])
    nvsUS = nrow(locData[locData$citizenship == 'US' & locData$job_country == 'United States',])
    visNC = nrow(locData[locData$citizenship == 'International' & locData$job_state == 'North Carolina',])
    nvsNC = nrow(locData[locData$citizenship == 'US' & locData$job_state == 'North Carolina',])
    
    box(
      width = 12,
      fluidRow(
        infoBox("", value = tags$p(paste(visUS, "out of total", visTT, "international fellows remain in US"), style = "font-size: 75%;"), icon = icon("globe"), color = "maroon", fill=TRUE, width=3),
        infoBox("", value = tags$p(paste(nvsUS, "out of total", nvsTT, "US fellows remain in US"), style = "font-size: 75%;"), icon = icon("home"), color = "blue", fill=TRUE, width=3),
        infoBox("", value = tags$p(paste(visNC, "international fellows remain in NC"), style = "font-size: 75%;"), icon = icon("globe"), color = "purple", fill=TRUE, width=3),
        infoBox("", value = tags$p(paste(nvsNC, "US fellows remain in NC"), style = "font-size: 75%;"), icon = icon("home"), color = "green", fill=TRUE, width=3)
      )
    )
  }) 
  
  
  #############################################################################
  # General career >>>
  #############################################################################
  
  
  # general career information boxes
  output$genrInfoBox<-renderUI({
    if (input$selectGnrTp %in% c('Left NIEHS in 2000-2014', 'Trend')) {
      itmdData = dataGroup
    }
    else {
      itmdData = subset(dataGroup, years == timeConvert(input$selectGnrTp))
    }
    gnrCount = nrow(itmdData)
    cGnrSector = table(itmdData[,"job_sector"])
    cGnrSectorPct = round(cGnrSector / gnrCount, 3) * 100
    cGnrType = table(itmdData[,"job_type"])
    cGnrTypePct = round(cGnrType / gnrCount, 3) * 100
    cGnrSpecifics = table(itmdData[,"specifics"])
    cGnrSpecPct = round(cGnrSpecifics / gnrCount, 3) * 100

    box(
      width = 12,
      fluidRow(
        valueBox(subtitle = paste0("in most common Job Sector: ", names(which(cGnrSectorPct==max(cGnrSectorPct)))), value = tags$p(paste0(max(cGnrSectorPct), "%"), style = "font-size: 75%;"), icon = icon("plus"), color = "maroon", width=4),
        valueBox(subtitle = paste0("in most common Job Type: ", names(which(cGnrTypePct==max(cGnrTypePct)))), value = tags$p(paste0(max(cGnrTypePct), "%"), style = "font-size: 75%;"), icon = icon("plus"), color = "yellow", width=4),
        valueBox(subtitle = paste0("in most common Job Specifics: ", names(which(cGnrSpecPct==max(cGnrSpecPct)))), value = tags$p(paste0(max(cGnrSpecPct), "%"), style = "font-size: 75%;"), icon = icon("plus"), color = "blue", width=4)
      ),
      fluidRow(
        valueBox(subtitle = paste0("in least common Job Sector: ", names(which(cGnrSectorPct==min(cGnrSectorPct)))), value = tags$p(paste0(min(cGnrSectorPct), "%"), style = "font-size: 75%;"), icon = icon("minus"), color = "maroon", width=4),
        valueBox(subtitle = paste0("in least common Job Type: ", names(which(cGnrTypePct==min(cGnrTypePct)))), value = tags$p(paste0(min(cGnrTypePct), "%"), style = "font-size: 75%;"), icon = icon("minus"), color = "yellow", width=4),
        valueBox(subtitle = paste0("in least common Job Specifics: ", names(which(cGnrSpecPct==min(cGnrSpecPct)))), value = tags$p(paste0(min(cGnrSpecPct), "%"), style = "font-size: 75%;"), icon = icon("minus"), color = "blue", width=4)
      )
    )
  })
  
  
  #>>> general career plot start
  colorJSect = c("Academic institution"="#94363a",
                 "For-profit company"="#f19493",
                 "Government agency"="#b15426",
                 "Indep./self-employed"="#f285a8",
                 "Non-profit organization"="#933761",
                 "Unknown or Undecided"="#da9a54")
  
  colorJType = c("Management"="#24594d",
                 "Non-tenure track faculty" ="#8acfbf",
                 "Professional staff"="#255a2d",
                 "Support staff"="#23787a",
                 "Tenure track faculty"="#6cc06d",
                 "Trainee" ="#5e7e37", 
                 "Unknown or Undecided"="#89c658")
  
  colorJSpec = c("Additional postdoc"="#225f7b",
                 "Computation/informatics"="#a792c5",
                 "Primarily applied research"="#8b9fd1",
                 "Primarily basic research"="#243e7d",
                 "Primarily clinical research"="#747fbe",
                 "Primarily teaching"="#6b417d",
                 "Regulatory affairs"="#23787a",
                 "REST COMBINED"="#47427e",
                 "Science admin./PMT"="#85357a",
                 "Science writing or comm."="#c06aaa",
                 "Technical/customer support"="#4bc4d2",
                 "Unknown or Undecided"="#8dbbd8")
  
  # donut plot job sector data
  output$jobSectDonut <- renderPlotly({
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectGnrTp
    if (slctYear == 'Left NIEHS in 2000-2014') {
      p <- dfJobSectAll %>% plot_ly(labels = ~Sector, values = ~Count, insidetextfont = list(color = '#FFFFFF'),
                                    marker = list(colors = colorJSect, line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>%
              add_pie(hole = 0.6) %>%
              layout(title = NULL,  showlegend = F,
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% add_annotations(x= 0.5, y= 0.5, text = paste0("<b>Job Sector<br>", "(", totalYrs, ")</b>"),
              font = list(color = 'black', size = 18), showarrow = F)
      p
    }
    else if (slctYear == 'Trend') {
      nTimeYrs = nlevels(factor(dfJobSectYrs$years))
      
      # number of rows for two plots in one row
      nPlotRws = ceiling(nTimeYrs / 2)
      rTraceHgt = round(1 / nPlotRws, digits = 2)
      
      # set layout domain list
      annoList = vector("list", nTimeYrs)
      
      p <- plot_ly(width = input$sldWidthGnr, height = input$sldHeightGnr, insidetextfont = list(color = '#FFFFFF'))
      for (i in 1:nTimeYrs) {
        iSlctYear = levels(factor(dfJobSectYrs$years))[i]
        iData <- dfJobSectYrs[dfJobSectYrs$years == iSlctYear, ]
        
        x1 = 0 + ((i+1) %% 2)*0.55
        x2 = x1 + 0.45
        # trace layer number
        iTraceLyr = ceiling(i / 2) - 1
        y1 = 1 - iTraceLyr * rTraceHgt - 0.1
        y2 = y1 - rTraceHgt + 0.1
        # domain
        iDomain = list(x = c(x1, x2), y = c(y1, y2))
        
        p <- p %>% add_pie(data = iData, labels = ~Sector, values = ~Count,
                           marker = list(colors = colorJSect, line = list(color = '#FFFFFF', width = 1)),
                           domain = iDomain, hole = 0.6)
        iAnno = list(x=x1+0.25, y=y1-rTraceHgt/2, text = paste0("<b>Job Sector<br>", "(", iSlctYear, ")</b>"),
                     font = list(size = 18), showarrow = F, xanchor="center")
        annoList[[i]] = iAnno
      }
      p <- p %>% layout(title = NULL,  showlegend = F,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% layout(annotations = annoList)
      p
    }
    else {
      jscData <- dfJobSectYrs[dfJobSectYrs$years == timeConvert(slctYear), ]
      p <- jscData %>% plot_ly(labels = ~Sector, values = ~Count, insidetextfont = list(color = '#FFFFFF'),
                                    marker = list(colors = colorJSect, line = list(color = '#FFFFFF', width = 1)),width = input$sldWidthGnr, height = input$sldHeightGnr) %>%
        add_pie(hole = 0.6) %>%
        layout(title = NULL,  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% add_annotations(x= 0.5, y= 0.5, text = paste0("<b>Job Sector<br>", "(", timeConvert(slctYear), ")</b>"),
               font = list(color = 'black', size = 18), showarrow = F)
      p
    }
  })
  
  output$jobSectDonutTxt  <- renderText({
    slctYear <- timeConvert(input$selectGnrTp)
    if (input$selectGnrTp == 'Left NIEHS in 2000-2014') {
      gnrData = dataGroup
    }
    else {
      gnrData = subset(dataGroup, years == slctYear)
    }
    
    talDAT = nrow(gnrData)
    cntACA = sum(gnrData$job_sector == 'Academic institution')
    cntUNK = sum(gnrData$job_sector == 'Unknown or Undecided')
    pctACA = round(cntACA / talDAT, 3) * 100
    pctUNK = round(cntUNK / talDAT, 3) * 100
    
    HTML(paste0("<p><strong>Career Outcomes in Different Job Sectors (Donut Plot)</strong> In ", slctYear, ", ", pctACA, 
         "% of all alumni enter the academic sector, while the remainder largely enter the for-profit or government sector. 
         Approximately ", pctUNK, "% are unknown or undecided.</p><p>&nbsp;</p>"))
  })
  
  # bar plot job sector data
  output$jobSectBar <- renderPlotly({
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectGnrTp
    if (slctYear == 'Left NIEHS in 2000-2014') {
      jscData = dfJobSectAll
      jscData$Text = paste0(jscData$Sector, ": ", jscData$Count, " (", round(jscData$Postdoc*100,2), "%)")
      p <- plot_ly(jscData,x = "", y = ~Postdoc*100, type = 'bar',color = ~Sector,colors = colorJSect,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
           layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
    else if (slctYear == 'Trend') {
      jscData <- dfJobSectYrs
      jscData$Text = paste0(jscData$Sector,"|",jscData$years,":<br> ", jscData$Count, " (", round(jscData$Postdoc*100,2), "%)")
      p <- plot_ly(jscData,x = ~years, y = ~Postdoc*100, type = 'bar',color = ~Sector,colors = colorJSect,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
    else {
      jscData <- dfJobSectYrs[dfJobSectYrs$years == timeConvert(slctYear), ]
      jscData$Text = paste0(jscData$Sector, ": ", jscData$Count, " (", round(jscData$Postdoc*100,2), "%)")
      p <- plot_ly(jscData,x = "", y = ~Postdoc*100, type = 'bar',color = ~Sector,colors = colorJSect,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
  })
  
  output$jobSectBarTxt  <- renderText({
    slctYear <- timeConvert(input$selectGnrTp)
    if (input$selectGnrTp == 'Left NIEHS in 2000-2014') {
      gnrData = dataGroup
    }
    else {
      gnrData = subset(dataGroup, years == slctYear)
    }
    
    talDAT = nrow(gnrData)
    cntACA = sum(gnrData$job_sector == 'Academic institution')
    cntUNK = sum(gnrData$job_sector == 'Unknown or Undecided')
    pctACA = round(cntACA / talDAT, 3) * 100
    pctUNK = round(cntUNK / talDAT, 3) * 100
    
    HTML(paste0("<p><strong>Career Outcomes in Different Job Sectors (Bar Chart)</strong> In ", slctYear, ", ", pctACA, 
                "% of all alumni enter the academic sector, while the remainder largely enter the for-profit or government sector. 
                Approximately ", pctUNK, "% are unknown or undecided.</p><p>&nbsp;</p>"))
  })
  
  # output job sector table
  output$jobSectTable <- renderTable({
    if (input$selectGnrTp == 'Left NIEHS in 2000-2014') {
      jscData <- changeTableHeader(dfJobSectAll,c('Job_Sector','Alumni_Count','Alumni_Portion'))
    }
    else if (input$selectGnrTp == 'Trend') {
      jscData <- changeTableHeader(dfJobSectYrs,c('Years','Job_Sector','Alumni_Count','Alumni_Portion'))
    }
    else {
      jscData <- changeTableHeader(dfJobSectYrs[dfJobSectYrs$years == timeConvert(input$selectGnrTp), ],c('Years','Job_Sector','Alumni_Count','Alumni_Portion'))
    }
    jscData
  })
  
  # donut plot job type data
  output$jobTypeDonut <- renderPlotly({
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectGnrTp
    if (slctYear == 'Left NIEHS in 2000-2014') {
      p <- dfJobTypeAll %>% plot_ly(labels = ~Type, values = ~Count, insidetextfont = list(color = '#FFFFFF'),
                                    marker = list(colors = colorJType, line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>%
        add_pie(hole = 0.6) %>%
        layout(title = NULL,  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% add_annotations(x= 0.5, y= 0.5, text = paste0("<b>Job Type<br>", "(", totalYrs, ")</b>"),
                                 font = list(color = 'black', size = 18), showarrow = F)
      p
    }
    else if (slctYear == 'Trend') {
      nTimeYrs = nlevels(factor(dfJobTypeYrs$years))
      
      # number of rows for two plots in one row
      nPlotRws = ceiling(nTimeYrs / 2)
      rTraceHgt = round(1 / nPlotRws, digits = 2)
      
      # set layout domain list
      annoList = vector("list", nTimeYrs)
      
      p <- plot_ly(width = input$sldWidthGnr, height = input$sldHeightGnr, insidetextfont = list(color = '#FFFFFF'))
      for (i in 1:nTimeYrs) {
        iSlctYear = levels(factor(dfJobTypeYrs$years))[i]
        iData <- dfJobTypeYrs[dfJobTypeYrs$years == iSlctYear, ]
        
        x1 = 0 + ((i+1) %% 2)*0.55
        x2 = x1 + 0.45
        # trace layer number
        iTraceLyr = ceiling(i / 2) - 1
        y1 = 1 - iTraceLyr * rTraceHgt - 0.1
        y2 = y1 - rTraceHgt + 0.1
        # domain
        iDomain = list(x = c(x1, x2), y = c(y1, y2))
        
        p <- p %>% add_pie(data = iData, labels = ~Type, values = ~Count,
                           marker = list(colors = colorJType, line = list(color = '#FFFFFF', width = 1)),
                           domain = iDomain, hole = 0.6)
        iAnno = list(x=x1+0.25, y=y1-rTraceHgt/2, text = paste0("<b>Job Type<br>", "(", iSlctYear, ")</b>"),
                     font = list(size = 18), showarrow = F, xanchor="center")
        annoList[[i]] = iAnno
      }
      p <- p %>% layout(title = NULL,  showlegend = F,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% layout(annotations = annoList)
      p
    }
    else {
      jtpData <- dfJobTypeYrs[dfJobTypeYrs$years == timeConvert(slctYear), ]
      p <- jtpData %>% plot_ly(labels = ~Type, values = ~Count, insidetextfont = list(color = '#FFFFFF'),
                               marker = list(colors = colorJType, line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>%
        add_pie(hole = 0.6) %>%
        layout(title = NULL,  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% add_annotations(x= 0.5, y= 0.5, text = paste0("<b>Job Type<br>", "(", timeConvert(slctYear), ")</b>"),
                                 font = list(color = 'black', size = 18), showarrow = F)
      p
    }
  })
  
  output$jobTypeDonutTxt  <- renderText({
    slctYear <- timeConvert(input$selectGnrTp)
    if (input$selectGnrTp == 'Left NIEHS in 2000-2014') {
      gnrData = dataGroup
    }
    else {
      gnrData = subset(dataGroup, years == slctYear)
    }
    
    talDAT = nrow(gnrData)
    cntTNT = sum(gnrData$job_type == 'Tenure track faculty')
    cntPFN = sum(gnrData$job_type == 'Professional staff')
    pctTNT = round(cntTNT / talDAT, 3) * 100
    pctPFN = round(cntPFN / talDAT, 3) * 100
    
    HTML(paste0("<p><strong>Career Outcomes in Different Job Types</strong> In ", slctYear, ", ", pctTNT,
         "% are in tenure-track job types, while ", pctPFN, "% are in professional staff job types, 
         and the rest are in non-tenure-track, support, or trainee job types.</p><p>&nbsp;</p>"))
  })
  
  # bar plot job type data
  output$jobTypeBar <- renderPlotly({
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectGnrTp
    if (slctYear == 'Left NIEHS in 2000-2014') {
      jtpData = dfJobTypeAll
      jtpData$Text = paste0(jtpData$Type, ": ", jtpData$Count, " (", round(jtpData$Postdoc*100,2), "%)")
      p <- plot_ly(jtpData,x = "", y = ~Postdoc*100, type = 'bar',color = ~Type,colors = colorJType,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
    else if (slctYear == 'Trend') {
      jtpData <- dfJobTypeYrs
      jtpData$Text = paste0(jtpData$Type,"|",jtpData$years,":<br> ", jtpData$Count, " (", round(jtpData$Postdoc*100,2), "%)")
      p <- plot_ly(jtpData,x = ~years, y = ~Postdoc*100, type = 'bar',color = ~Type,colors = colorJType,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
    else {
      jtpData <- dfJobTypeYrs[dfJobTypeYrs$years == timeConvert(slctYear), ]
      jtpData$Text = paste0(jtpData$Type, ": ", jtpData$Count, " (", round(jtpData$Postdoc*100,2), "%)")
      p <- plot_ly(jtpData,x = "", y = ~Postdoc*100, type = 'bar',color = ~Type,colors = colorJType,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
  })
  
  output$jobTypeBarTxt  <- renderText({
    slctYear <- timeConvert(input$selectGnrTp)
    if (input$selectGnrTp == 'Left NIEHS in 2000-2014') {
      gnrData = dataGroup
    }
    else {
      gnrData = subset(dataGroup, years == slctYear)
    }
    
    talDAT = nrow(gnrData)
    cntTNT = sum(gnrData$job_type == 'Tenure track faculty')
    cntPFN = sum(gnrData$job_type == 'Professional staff')
    pctTNT = round(cntTNT / talDAT, 3) * 100
    pctPFN = round(cntPFN / talDAT, 3) * 100
    
    HTML(paste0("<p><strong>Career Outcomes in Different Job Types</strong> In ", slctYear, ", ", pctTNT,
                "% are in tenure-track job types, while ", pctPFN, "% are in professional staff job types, 
                and the rest are in non-tenure-track, support, or trainee job types.</p><p>&nbsp;</p>"))
  })
  
  # output job type table
  output$jobTypeTable <- renderTable({
    if (input$selectGnrTp == 'Left NIEHS in 2000-2014') {
      jtpData <- changeTableHeader(dfJobTypeAll,c('Job_Type','Alumni_Count','Alumni_Portion'))
    }
    else if (input$selectGnrTp == 'Trend') {
      jtpData <- changeTableHeader(dfJobTypeYrs,c('Years','Job_Type','Alumni_Count','Alumni_Portion'))
    }
    else {
      jtpData <- changeTableHeader(dfJobTypeYrs[dfJobTypeYrs$years == timeConvert(input$selectGnrTp), ],c('Years','Job_Type','Alumni_Count','Alumni_Portion'))
    }
    jtpData
  })

    
  # donut plot job specifics data
  output$jobSpecDonut <- renderPlotly({
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectGnrTp
    if (slctYear == 'Left NIEHS in 2000-2014') {
      p <- dfJobSpecAll %>% plot_ly(labels = ~Specifics, values = ~Count, insidetextfont = list(color = '#FFFFFF'),
                                    marker = list(colors = colorJSpec, line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>%
        add_pie(hole = 0.6) %>%
        layout(title = NULL,  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% add_annotations(x= 0.5, y= 0.5, text = paste0("<b>Job Specifics<br>", "(", totalYrs, ")</b>"),
                                 font = list(color = 'black', size = 16), showarrow = F)
      p
    }
    else if (slctYear == 'Trend') {
      nTimeYrs = nlevels(factor(dfJobSpecYrs$years))
      
      # number of rows for two plots in one row
      nPlotRws = ceiling(nTimeYrs / 2)
      rTraceHgt = round(1 / nPlotRws, digits = 2)
      
      # set layout domain list
      annoList = vector("list", nTimeYrs)
      
      p <- plot_ly(width = input$sldWidthGnr, height = input$sldHeightGnr, insidetextfont = list(color = '#FFFFFF'))
      for (i in 1:nTimeYrs) {
        iSlctYear = levels(factor(dfJobSpecYrs$years))[i]
        iData <- dfJobSpecYrs[dfJobSpecYrs$years == iSlctYear, ]
        
        x1 = 0 + ((i+1) %% 2)*0.55
        x2 = x1 + 0.45
        # trace layer number
        iTraceLyr = ceiling(i / 2) - 1
        y1 = 1 - iTraceLyr * rTraceHgt - 0.1
        y2 = y1 - rTraceHgt + 0.1
        # domain
        iDomain = list(x = c(x1, x2), y = c(y1, y2))
        
        p <- p %>% add_pie(data = iData, labels = ~Specifics, values = ~Count,
                           marker = list(colors = colorJSpec, line = list(color = '#FFFFFF', width = 1)),
                           domain = iDomain, hole = 0.6)
        iAnno = list(x=x1+0.25, y=y1-rTraceHgt/2, text = paste0("<b>Job Specifics<br>", "(", iSlctYear, ")</b>"),
                     font = list(size = 16), showarrow = F, xanchor="center")
        annoList[[i]] = iAnno
      }
      p <- p %>% layout(title = NULL,  showlegend = F,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% layout(annotations = annoList)
      p
    }
    else {
      jspData <- dfJobSpecYrs[dfJobSpecYrs$years == timeConvert(slctYear), ]
      p <- jspData %>% plot_ly(labels = ~Specifics, values = ~Count, insidetextfont = list(color = '#FFFFFF'),
                               marker = list(colors = colorJSpec, line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>%
        add_pie(hole = 0.6) %>%
        layout(title = NULL,  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% add_annotations(x= 0.5, y= 0.5, text = paste0("<b>Job Specifics<br>", "(", timeConvert(slctYear), ")</b>"),
                                 font = list(color = 'black', size = 16), showarrow = F)
      p
    }
  })
  
  output$jobSpecDonutTxt  <- renderText({
    slctYear <- timeConvert(input$selectGnrTp)
    if (input$selectGnrTp == 'Left NIEHS in 2000-2014') {
      gnrData = dataGroup
    }
    else {
      gnrData = subset(dataGroup, years == slctYear)
    }
    
    talDAT = nrow(gnrData)
    cntRSC = sum(gnrData$specifics %in% c('Primarily basic research','Primarily applied research','Primarily clinical research','Computation/informatics','Additional postdoc'))
    pctRSC = round(cntRSC / talDAT, 3) * 100

    HTML(paste0("<p><strong>Career Outcomes in Different Job Specifics</strong> In ", slctYear, ", ", pctRSC,
         "% of all alumni enter into research-based positions, whether basic, applied, clinical, computation/informatics, or whether continuing research in another postdoc position. 
         The remainder are mostly involved in science-related non-research positions.
         Each job specifics category in Career classification - Job specifics table that was populated with a small number of alumni was grouped into a category termed ‘REST COMBINED’ for ease of illustration. 
         These categories (denoted by * within the table) include: Additional Degree, Business Development/Operations, Clinical Practice, Consulting, Grants Management, IP/Patenting, Sales, Science Policy.</p><p>&nbsp;</p>"))
  })
  
  # bar plot job specifics data
  output$jobSpecBar <- renderPlotly({
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectGnrTp
    if (slctYear == 'Left NIEHS in 2000-2014') {
      jspData = dfJobSpecAll
      jspData$Text = paste0(jspData$Specifics, ": ", jspData$Count, " (", round(jspData$Postdoc*100,2), "%)")
      p <- plot_ly(jspData,x = "", y = ~Postdoc*100, type = 'bar',color = ~Specifics,colors = colorJSpec,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
    else if (slctYear == 'Trend') {
      jspData <- dfJobSpecYrs
      jspData$Text = paste0(jspData$Specifics,"|",jspData$years,":<br> ", jspData$Count, " (", round(jspData$Postdoc*100,2), "%)")
      p <- plot_ly(jspData,x = ~years, y = ~Postdoc*100, type = 'bar',color = ~Specifics,colors = colorJSpec,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
    else {
      jspData <- dfJobSpecYrs[dfJobSpecYrs$years == timeConvert(slctYear), ]
      jspData$Text = paste0(jspData$Specifics, ": ", jspData$Count, " (", round(jspData$Postdoc*100,2), "%)")
      p <- plot_ly(jspData,x = "", y = ~Postdoc*100, type = 'bar',color = ~Specifics,colors = colorJSpec,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
  })
  
  output$jobSpecBarTxt  <- renderText({
    slctYear <- timeConvert(input$selectGnrTp)
    if (input$selectGnrTp == 'Left NIEHS in 2000-2014') {
      gnrData = dataGroup
    }
    else {
      gnrData = subset(dataGroup, years == slctYear)
    }
    
    talDAT = nrow(gnrData)
    cntRSC = sum(gnrData$specifics %in% c('Primarily basic research','Primarily applied research','Primarily clinical research','Computation/informatics','Additional postdoc'))
    pctRSC = round(cntRSC / talDAT, 3) * 100
    
    HTML(paste0("<p><strong>Career Outcomes in Different Job Specifics (Bar Chart)</strong> In ", slctYear, ", ", pctRSC,
                "% of all alumni enter into research-based positions, whether basic, applied, clinical, computation/informatics, or whether continuing research in another postdoc position. 
                The remainder are mostly involved in science-related non-research positions.
                Each job specifics category in Career classification - Job specifics table that was populated with a small number of alumni was grouped into a category termed ‘REST COMBINED’ for ease of illustration. 
                These categories (denoted by * within the table) include: Additional Degree, Business Development/Operations, Clinical Practice, Consulting, Grants Management, IP/Patenting, Sales, Science Policy.</p><p>&nbsp;</p>"))
  })
  # https://stackoverflow.com/questions/34315485/linking-to-a-tab-or-panel-of-a-shiny-app
  # https://stackoverflow.com/questions/37169039/direct-link-to-tabitem-with-r-shiny-dashboard
  # https://stackoverflow.com/questions/33021757/externally-link-to-specific-tabpanel-in-shiny-app
  # https://stackoverflow.com/questions/28605185/create-link-to-the-other-part-of-the-shiny-app/28605517#28605517
  # https://stackoverflow.com/questions/27303526/r-shiny-build-links-between-tabs
  # https://github.com/rstudio/shiny/issues/772#issuecomment-112919149
  # https://groups.google.com/forum/#!msg/shiny-discuss/sJlasQf71fY/RW7Xc8F02IoJ
  
  # output job specifics table
  output$jobSpecTable <- renderTable({
    if (input$selectGnrTp == 'Left NIEHS in 2000-2014') {
      jspData <- changeTableHeader(dfJobSpecAll,c('Job_Specifics','Alumni_Count','Alumni_Portion'))
    }
    else if (input$selectGnrTp == 'Trend') {
      jspData <- changeTableHeader(dfJobSpecYrs,c('Years','Job_Specifics','Alumni_Count','Alumni_Portion'))
    }
    else {
      jspData <- changeTableHeader(dfJobSpecYrs[dfJobSpecYrs$years == timeConvert(input$selectGnrTp), ],c('Years','Job_Specifics','Alumni_Count','Alumni_Portion'))
    }
    jspData
  })
  
  
  output$genrDynamicUI<-renderUI({
    # zoom job sector plot
    if (input$selectShowGnr == 2) {
      fluidRow(
        tabBox(
          title = "Job Sector", width = 12,
          # The id lets us use input$tab_genJSector on the server to find the current tab
          id = "tab_genJSector",
          tabPanel("Job sector chart", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("jobSectDonutTxt"), plotlyOutput("jobSectDonut")),
          # tabPanel("Job sector bar chart", style = "overflow-x:scroll; overflow-y:scroll; max-height: 1200px", htmlOutput("jobSectBarTxt"), plotlyOutput("jobSectBar")),
          tabPanel("Job sector data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobSectTable"))
        )
      )
    }
    # zoom job type plot
    else if (input$selectShowGnr == 3) {
      fluidRow(
        tabBox(
          title = "Job Type", width = 12,
          # The id lets us use input$tab_genJType on the server to find the current tab
          id = "tab_genJType",
          tabPanel("Job type chart", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("jobTypeDonutTxt"), plotlyOutput("jobTypeDonut")),
          # tabPanel("Job type bar chart", style = "overflow-x:scroll; overflow-y:scroll; max-height: 1200px", htmlOutput("jobTypeBarTxt"), plotlyOutput("jobTypeBar")),
          tabPanel("Job type data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobTypeTable"))
        )
      )
    }
    # zoom job type plot
    else if (input$selectShowGnr == 4) {
      fluidRow(
        tabBox(
          title = "Job Specifics", width = 12,
          # The id lets us use input$tab_genJSpec on the server to find the current tab
          id = "tab_genJSpec",
          tabPanel("Job specifics chart", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("jobSpecDonutTxt"), plotlyOutput("jobSpecDonut")),
          # tabPanel("Job specifics bar chart", style = "overflow-x:scroll; overflow-y:scroll; max-height: 1200px", htmlOutput("jobSpecBarTxt"), plotlyOutput("jobSpecBar")),
          tabPanel("Job specifics data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobSpecTable"))
        )
      )
    }
    # default to show all
    else {
      fluidRow(
        column(width = 12,
          tabBox(
            title = "Job Sector",
            # The id lets us use input$tab_genJSectorSml on the server to find the current tab
            id = "tab_genJSectorSml",
            tabPanel("Sector chart", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("jobSectDonutTxt"), plotlyOutput("jobSectDonut")),
            # tabPanel("Sector bar chart", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("jobSectBarTxt"), plotlyOutput("jobSectBar")),
            tabPanel("Sector data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobSectTable"))
          ),
          tabBox(
            title = "Job Type", width = 6,
            # The id lets us use input$tab_genJTypeSml on the server to find the current tab
            id = "tab_genJTypeSml",
            tabPanel("Type chart", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("jobTypeDonutTxt"), plotlyOutput("jobTypeDonut")),
            # tabPanel("Type bar chart", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("jobTypeBarTxt"), plotlyOutput("jobTypeBar")),
            tabPanel("Type data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobTypeTable"))
          )
        ),
        column(width = 12,
          tabBox(
            title = "Job Specifics",
            # The id lets us use input$tab_genJSpecSml on the server to find the current tab
            id = "tab_genJSpecSml",
            tabPanel("Specifics chart", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("jobSpecDonutTxt"), plotlyOutput("jobSpecDonut")),
            # tabPanel("Specifics bar chart", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("jobSpecBarTxt"), plotlyOutput("jobSpecBar")),
            tabPanel("Specifics data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobSpecTable"))
          )
        )
      )
    }
  })
  
  #<<< general career plot
  
  

  #############################################################################
  # Relationship >>>
  #############################################################################
  
  # Sankey plot
  output$jobSankeyPlot <- renderPlotly({
    sankeyData_gvisTOnetD3 <- function(gvisData, from="from", to="to", weight="weight") {
      # create nodes data frame
      vNode = unique(c(gvisData[,from], gvisData[,to]))
      nodesDF = data.frame(name=vNode)
      
      # add sequence number into nodes data frame extension
      nodes_ext = nodesDF
      nodes_ext$id = seq.int(nrow(nodes_ext))-1
      nodesDF$name <- as.character(nodesDF$name)
      
      # create links data frame by matching node name
      # http://stackoverflow.com/questions/11530184/match-values-in-data-frame-with-values-in-another-data-frame-and-replace-former
      linksDF = data.frame(source=nodes_ext[match(gvisData[,from], nodes_ext$name), 2],
                           target=nodes_ext[match(gvisData[,to], nodes_ext$name), 2], value=gvisData$weight)
      
      netD3Data = list(nodes=nodesDF, links=linksDF)
      
      return(netD3Data)
    }
    
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectRlnTp
    if (slctYear == 'Left NIEHS in 2000-2014') {
      skData = dataGroup
    }
    else if (slctYear == 'Trend') {
      # set trend data as total data
      skData = dataGroup
    }
    else {
      skData = dataGroup[dataGroup$years == timeConvert(slctYear), ]
    }
    
    dataSecTyp <- skData %>% group_by(job_sector,job_type) %>% summarise (weight = n())
    dataSecTyp$job_sector <- as.character(dataSecTyp$job_sector)
    dataSecTyp$job_sector[dataSecTyp$job_sector == 'Unknown or Undecided'] <- "Unknown (sector)"
    dataSecTyp$job_sector <- factor(dataSecTyp$job_sector)
    dataSecTyp$job_type <- as.character(dataSecTyp$job_type)
    dataSecTyp$job_type[dataSecTyp$job_type == 'Unknown or Undecided'] <- "Unknown (type)"
    dataSecTyp$job_type <- factor(dataSecTyp$job_type)
    colnames(dataSecTyp)[colnames(dataSecTyp)=="job_sector"] <- "from"
    colnames(dataSecTyp)[colnames(dataSecTyp)=="job_type"] <- "to"
    
    dataTypSpe <- skData %>% group_by(job_type, specifics) %>% summarise (weight = n())
    dataTypSpe$job_type <- as.character(dataTypSpe$job_type)
    dataTypSpe$job_type[dataTypSpe$job_type == 'Unknown or Undecided'] <- "Unknown (type)"
    dataTypSpe$job_type <- factor(dataTypSpe$job_type)
    dataTypSpe$specifics <- as.character(dataTypSpe$specifics)
    dataTypSpe$specifics[dataTypSpe$specifics == 'Unknown or Undecided'] <- "Unknown (specifics)"
    dataTypSpe$specifics <- factor(dataTypSpe$specifics)
    colnames(dataTypSpe)[colnames(dataTypSpe)=="job_type"] <- "from"
    colnames(dataTypSpe)[colnames(dataTypSpe)=="specifics"] <- "to"
    
    dataSK <- bind_rows(as.data.frame(dataSecTyp), as.data.frame(dataTypSpe))
    netD3 <- sankeyData_gvisTOnetD3(dataSK)
    
    p <- plot_ly(
      type = "sankey",
      domain = c(
        x =  c(0,1),
        y =  c(0,1)
      ),
      orientation = "h",
      valueformat = ".0f",
      
      node = list(
        label = netD3$nodes$name,
        color = c("#8a2105","#f19898","#ef86ab","#99335c","#b44e11","#df9f4d",
                  "#295a29","#89f3d2","#04e0e8","#295a4c","#61d661","#617d0d","#91c844",
                  "#9bc2df","#97a9f5","#6c3a77","#b399c9","#8787fb","#483a77","#215c7a","#21327a","#21777a","#04e0e8","#e461c4","#892f73"),
        pad = 15,
        thickness = 15,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      link = list(
        source = netD3$links$source,
        target = netD3$links$target,
        value =  netD3$links$value,
        label =  netD3$links$job_cat
      ),
      width = input$sldWidthRln, height = input$sldHeightRln
    ) %>% 
      layout(
        title = "Connection Between Job Sectors, Job Types, and Job Specifics",
        font = list(
          size = 12
        )
        # xaxis = list(showgrid = F, zeroline = F),
        # yaxis = list(showgrid = F, zeroline = F)
      )
    
    p
  })
  
  # output$jobSankeyTxt  <- renderText({HTML("<p><strong>Relationship Between Job Sectors, Job Types and Job Specifics</strong> 
  #                                          The width of the lines is proportional to the relative quantity of scholars within each group. 
  #                                          <i>(Left & Middle) Division of job sectors by job type.</i> Focusing on the academic sector as an example, it can be seen that not all academic positions are tenure-track. The remainder of those in the academic sector are divided between professional, management, support, non-tenure-track, or trainee job types. 
  #                                          <i>(Middle & Right) Division of job types by job specifics.</i> Focusing on tenure-track positions as an example, it can be seen that the majority are conducting basic research in tenure-track positions. Most of the remaining individuals in tenure-track positions are either teaching, or conducting applied or clinical research.</p><p>&nbsp;</p>")})
  
  # output Sankey data table
  output$jobSankeyTable <- renderTable({
    slctYear <- input$selectRlnTp
    if (slctYear == 'Left NIEHS in 2000-2014') {
      skData = dataGroup
    }
    else if (slctYear == 'Trend') {
      # set trend data as total data
      skData = dataGroup
    }
    else {
      skData = dataGroup[dataGroup$years == timeConvert(slctYear), ]
    }
    
    dataSecTyp <- skData %>% group_by(job_sector,job_type) %>% summarise (weight = n())
    dataSecTyp$job_sector <- as.character(dataSecTyp$job_sector)
    dataSecTyp$job_sector[dataSecTyp$job_sector == 'Unknown or Undecided'] <- "Unknown (sector)"
    dataSecTyp$job_type <- as.character(dataSecTyp$job_type)
    dataSecTyp$job_type[dataSecTyp$job_type == 'Unknown or Undecided'] <- "Unknown (type)"
    colnames(dataSecTyp)[colnames(dataSecTyp)=="job_sector"] <- "from"
    colnames(dataSecTyp)[colnames(dataSecTyp)=="job_type"] <- "to"
    
    dataTypSpe <- skData %>% group_by(job_type, specifics) %>% summarise (weight = n())
    dataTypSpe$job_type <- as.character(dataTypSpe$job_type)
    dataTypSpe$job_type[dataTypSpe$job_type == 'Unknown or Undecided'] <- "Unknown (type)"
    dataTypSpe$specifics <- as.character(dataTypSpe$specifics)
    dataTypSpe$specifics[dataTypSpe$specifics == 'Unknown or Undecided'] <- "Unknown (specifics)"
    colnames(dataTypSpe)[colnames(dataTypSpe)=="job_type"] <- "from"
    colnames(dataTypSpe)[colnames(dataTypSpe)=="specifics"] <- "to"
    
    dataSK <- bind_rows(as.data.frame(dataSecTyp), as.data.frame(dataTypSpe))
    colnames(dataSK) <- c('Job_Population','Job_Subpopulation','Alumni_Count')
    dataSK
  })
  
  output$rltnDynamicUI<-renderUI({
    fluidRow(
      tabBox(
        title = "Relationship between categories", width = 12,
        # The id lets us use input$tab_rlnSankey on the server to find the current tab
        id = "tab_rlnSankey",
        tabPanel("Relationship sankey plot", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", plotlyOutput("jobSankeyPlot")),
        tabPanel("Relationship data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobSankeyTable"))
      )
    )
  })
  
  
  #############################################################################
  # Job highlight >>>
  #############################################################################
  # Font size of legend - https://github.com/timelyportfolio/sunburstR/issues/35
  
  #>>> job distribution highlight
  colorHL <- list(
    range = c("#C295C9","#867cb6","#b82e91","#97D6B4","#54b649","#2f9d49"),
    domain= c("Intn'l","Intn'l abroad","Intn'l in US","US","US abroad","US in US")
  )
  # Controlling colours across plots  --- https://github.com/timelyportfolio/sunburstR/issues/17
  # colorHL <- c("#C295C9","#362586","#A22980","#97D6B4","#51B447","#27843C")
  
  # tenure-track plot
  output$hlTenurePlot <- renderSunburst({
    # choose data based on input$selectHlTp from ui.R
    slctYear <- input$selectHlTp
    if (slctYear == 'Left NIEHS in 2000-2014') {
      skData = dataGroup
    }
    else {
      skData = dataGroup[dataGroup$years == timeConvert(slctYear), ]
    }
    
    # count tenure-track by international fellow abroad, international fellow in US, US fellow abroad, US fellow in US
    skData_tt = skData[skData$job_type == "Tenure track faculty", ]
    skData_tt$job_country = as.character(skData_tt$job_country)
    skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l abroad"
    skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'US')] = "US abroad"
    skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l in US"
    skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'US')] = "US in US"
    skData_tt$job_country = factor(skData_tt$job_country)
    skData_tt$citizenship = as.character(skData_tt$citizenship)
    skData_tt$citizenship[skData_tt$citizenship == 'International'] = "Intn'l"
    skData_tt$citizenship = factor(skData_tt$citizenship)
    skData_tbl <- skData_tt %>% group_by(citizenship, job_country) %>% summarise(cnt = n())
    skData_tbl <- data.frame(skData_tbl) %>% mutate(freq=cnt/sum(cnt))
    
    p <- skData_tbl %>% mutate(path=paste(citizenship,job_country,sep='-')) %>% select(path,freq) %>% sunburst(colors = colorHL)
    p
  })
  
  output$hlTenureTxt  <- renderText({
    HTML("<p><strong>Closer Look at All Tenure-Track Positions</strong> 
         We examined the relative proportion of U.S. versus international scholars in tenure-track positions, and found that majority of tenure-track faculty positions are held by international scholars (center circle, light magenta). 
         However, most hold these positions abroad (outer circle, dark purple). 
         Of the U.S. scholars, most hold tenure-track positions in the United States (outer circle, dark green), with only small percentage of tenure-track positions being held by U.S. scholars abroad (outer circle, light green).</p><p>&nbsp;</p>")
  })
  
  # tenure-track table
  output$hlTenureTable <- renderTable({
    # choose data based on input$selectHlTp from ui.R
    slctYear <- input$selectHlTp
    if (slctYear == 'Left NIEHS in 2000-2014') {
      skData = dataGroup
    }
    else {
      skData = dataGroup[dataGroup$years == timeConvert(slctYear), ]
    }
    
    # count tenure-track by international fellow abroad,  international fellow in US, US fellow abroad, US fellow in US
    skData_tt = skData[skData$job_type == "Tenure track faculty", ]
    skData_tt$job_country = as.character(skData_tt$job_country)
    skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l Scholar abroad"
    skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'US')] = "US Scholar abroad"
    skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l Scholar in US"
    skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'US')] = "US Scholar in US"
    skData_tt$job_country = factor(skData_tt$job_country)
    skData_tbl <- skData_tt %>% group_by(citizenship, job_country) %>% summarise(cnt = n())
    skData_tbl <- data.frame(skData_tbl) %>% mutate(freq=cnt/sum(cnt))
    changeTableHeader(skData_tbl,c('Origin_Country','Job_Country','Alumni_Count','Alumni_Portion'))
  })
  
  # additional training plot
  output$hlTrainPlot <- renderSunburst({
    # choose data based on input$selectHlTp from ui.R
    slctYear <- input$selectHlTp
    if (slctYear == 'Left NIEHS in 2000-2014') {
      skData = dataGroup
    }
    else {
      skData = dataGroup[dataGroup$years == timeConvert(slctYear), ]
    }
    
    # count tenure-track by international fellow abroad, international fellow in US, US fellow abroad, US fellow in US
    skData_tt = skData[skData$job_type == "Trainee", ]
    skData_tt$job_country = as.character(skData_tt$job_country)
    skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l abroad"
    skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'US')] = "US abroad"
    skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l in US"
    skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'US')] = "US in US"
    skData_tt$job_country = factor(skData_tt$job_country)
    skData_tt$citizenship = as.character(skData_tt$citizenship)
    skData_tt$citizenship[skData_tt$citizenship == 'International'] = "Intn'l"
    skData_tt$citizenship = factor(skData_tt$citizenship)
    skData_tbl <- skData_tt %>% group_by(citizenship, job_country) %>% summarise(cnt = n())
    skData_tbl <- data.frame(skData_tbl) %>% mutate(freq=cnt/sum(cnt))
    
    p <- skData_tbl %>% mutate(path=paste(citizenship,job_country,sep='-')) %>% select(path,freq) %>% sunburst(colors = colorHL)
    p
  })
  
  output$hlTrainTxt  <- renderText({
    slctYear <- timeConvert(input$selectHlTp)
    if (slctYear %in% c('2000-2014','2010-2014')) {
      HTML("<p><strong>Closer Look at All Individuals Engaged in Additional Postdoctoral Training</strong> 
           We examined everyone that was engaged in additional postdoctoral training and found that the majority were international scholars (center circle, light magenta). 
           Most fellows conducted additional postdoctoral training within the United States, regardless of whether they were international or U.S. (outer circle, dark magenta & dark green, respectively).</p><p>&nbsp;</p>")
    }
    else if (slctYear == '2000-2004') {
      HTML("<p><strong>Closer Look at All Individuals Engaged in Additional Postdoctoral Training</strong> 
           We examined everyone that was engaged in additional postdoctoral training and found that the majority were international scholars (center circle, light magenta). 
           All fellows conducted additional postdoctoral training within the United States, regardless of whether they were international or U.S. (outer circle, dark magenta & dark green, respectively).</p><p>&nbsp;</p>")
    }
    else if (slctYear == '2005-2009') {
      HTML("<p><strong>Closer Look at All Individuals Engaged in Additional Postdoctoral Training</strong> 
           We examined everyone that was engaged in additional postdoctoral training and found that the majority were international scholars (center circle, light magenta). 
           All US fellows conducted additional postdoctoral training within the United States (outer circle, dark green). 
           Most international fellows conducted additional postdoctoral training within the United States (outer circle, dark magenta), 
           while a small portion of international fellows had additional postdoctoral training out side of the United States (outer circle, purple).</p><p>&nbsp;</p>")
    }
  })
  
  # additional training table
  output$hlTrainTable <- renderTable({
    # choose data based on input$selectHlTp from ui.R
    slctYear <- input$selectHlTp
    if (slctYear == 'Left NIEHS in 2000-2014') {
      skData = dataGroup
    }
    else {
      skData = dataGroup[dataGroup$years == timeConvert(slctYear), ]
    }
    
    # count tenure-track by international fellow abroad, international fellow in US, US fellow abroad, US fellow in US
    skData_tt = skData[skData$job_type == "Trainee", ]
    skData_tt$job_country = as.character(skData_tt$job_country)
    skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l Scholar abroad"
    skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'US')] = "US Scholar abroad"
    skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l Scholar in US"
    skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'US')] = "US Scholar in US"
    skData_tt$job_country = factor(skData_tt$job_country)
    skData_tbl <- skData_tt %>% group_by(citizenship, job_country) %>% summarise(cnt = n())
    skData_tbl <- data.frame(skData_tbl) %>% mutate(freq=cnt/sum(cnt))
    changeTableHeader(skData_tbl,c('Origin_Country','Job_Country','Alumni_Count','Alumni_Portion'))
  })
  
  output$hlDynamicUI<-renderUI({
    # zoom tenure track plot
    if (input$selectShowHl == 1) {
      fluidRow(
        tabBox(
          title = "Tenure track", width = 12,
          # The id lets us use input$tab_hlTT on the server to find the current tab
          id = "tab_hlTT",
          tabPanel("Tenure track plot", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("hlTenureTxt"), sunburstOutput("hlTenurePlot", width = input$sldWidthHl, height = input$sldHeightHl)),
          tabPanel("Tenure track data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("hlTenureTable"))
        )
      )
    }
    # zoom visiting plot
    else if (input$selectShowHl == 3) {
      fluidRow(
        tabBox(
          title = "Additional postdoc", width = 12,
          # The id lets us use input$tab_hlAT on the server to find the current tab
          id = "tab_hlAT",
          tabPanel("Addt'l postdoc plot", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("hlTrainTxt"), sunburstOutput("hlTrainPlot", width = input$sldWidthHl, height = input$sldHeightHl)),
          tabPanel("Addt'l postdoc data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("hlTrainTable"))
        )
      )
    }
    # default to show both
    else {
      fluidRow(
        tabBox(
          title = "Tenure track",
          # The id lets us use input$tab_hlTTSml on the server to find the current tab
          id = "tab_hlTTSml",
          tabPanel("Tenure track plot", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("hlTenureTxt"), sunburstOutput("hlTenurePlot", width = input$sldWidthHl, height = input$sldHeightHl)),
          tabPanel("Tenure track data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("hlTenureTable"))
        ),
        tabBox(
          title = "Additional postdoc",
          # The id lets us use input$tab_hlATSml on the server to find the current tab
          id = "tab_hlATSml",
          tabPanel("Addt'l postdoc plot", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("hlTrainTxt"), sunburstOutput("hlTrainPlot", width = input$sldWidthHl, height = input$sldHeightHl)),
          tabPanel("Addt'l postdoc data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("hlTrainTable"))
        )
      )
    }
  })
  #<<< job distribution highlight
  
  
  #############################################################################
  # Training time >>>
  #############################################################################
  
  
  #>>> training time
  # training time job sector plot
  output$tmSectPlot <- renderPlotly({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Left NIEHS in 2000-2014') {
      tmSect <- dataGroup
      time.sct = aggregate(months_postdoc ~ job_sector, tmSect, median)
      colnames(time.sct)[colnames(time.sct)=="months_postdoc"] <- "Median_time"
      time.sct$sct_num = as.numeric(time.sct$job_sector)
      p <- ggplot(tmSect, aes(x = job_sector, y = months_postdoc, fill = job_sector)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.sct, aes(label=round(Median_time,1), x=sct_num+0.4, y = Median_time))
      p <- p + scale_fill_manual(values=colorJSect)
    }
    else if (input$selectTmTp == 'Trend') {
      tmSect <- dataGroup
      time.sct = aggregate(months_postdoc ~ years + job_sector, tmSect, median)
      colnames(time.sct)[colnames(time.sct)=="months_postdoc"] <- "Median_time"
      time.sct$sct_num = as.numeric(time.sct$job_sector)
      p <- ggplot(tmSect, aes(x = years, y = months_postdoc, fill = years)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.sct, aes(label=round(Median_time,1), x=sct_num+0.4, y = Median_time))
      p <- p + facet_grid(job_sector ~ .)
      p <- p + theme(strip.text.y = element_text(size=10, face="bold"))
    }
    else {
      tmSect <- dataGroup[dataGroup$years == timeConvert(input$selectTmTp), ]
      time.sct = aggregate(months_postdoc ~ job_sector, tmSect, median)
      colnames(time.sct)[colnames(time.sct)=="months_postdoc"] <- "Median_time"
      time.sct$sct_num = as.numeric(time.sct$job_sector)
      p <- ggplot(tmSect, aes(x = job_sector, y = months_postdoc, fill = job_sector)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.sct, aes(label=round(Median_time,1), x=sct_num+0.4, y = Median_time))
      p <- p + scale_fill_manual(values=colorJSect)
    }
    p <- p + ggtitle("Training Time (months)") + theme(legend.position="none") +
          labs(y='Months', x='')
    gp <- ggplotly(p, width = input$sldWidthTm, height = input$sldHeightTm, tooltip=c("Median_time"))
    gp # %>% layout(margin = list(l = 75))
  })
  
  output$tmSectTxt  <- renderText({
    slctYear <- timeConvert(input$selectTmTp)
    if (input$selectTmTp == 'Left NIEHS in 2000-2014') {
      tmData = dataGroup
    }
    else {
      tmData = subset(dataGroup, years == slctYear)
    }
    
    tmSCT = aggregate(months_postdoc ~ job_sector, tmData, median)
    lgstSCT <- tolower(as.character(tmSCT$job_sector[tmSCT$months_postdoc == max(tmSCT$months_postdoc)]))
    
    HTML(paste0("<p><strong>Summary of Training Time Difference among Different Job Sectors</strong> In ", slctYear, 
                ", alumni entering into a(n) <i>", lgstSCT, "</i> have the longest median training time (", max(tmSCT$months_postdoc), " months).</p><p>&nbsp;</p>"))
   })
  
  # training time job sector table
  output$tmSectTable <- renderTable({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Left NIEHS in 2000-2014') {
      tmSect <- changeTableHeader(dfTimeSectAll,c('Job_Sector','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else if (input$selectTmTp == 'Trend') {
      tmSect <- changeTableHeader(dfTimeSectYrs,c('Years','Job_Sector','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else {
      tmSect <- changeTableHeader(dfTimeSectYrs[dfTimeSectYrs$years ==timeConvert( input$selectTmTp), ],c('Years','Job_Sector','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    tmSect
  })
  
  # training time job type plot
  output$tmTypePlot <- renderPlotly({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Left NIEHS in 2000-2014') {
      tmType <- dataGroup
      time.typ = aggregate(months_postdoc ~ job_type, tmType, median)
      colnames(time.typ)[colnames(time.typ)=="months_postdoc"] <- "Median_time"
      time.typ$typ_num = as.numeric(time.typ$job_type)
      p <- ggplot(tmType, aes(x = job_type, y = months_postdoc, fill = job_type)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.typ, aes(label=round(Median_time,1), x=typ_num+0.4, y = Median_time))
      p <- p + scale_fill_manual(values=colorJType)
    }
    else if (input$selectTmTp == 'Trend') {
      tmType <- dataGroup
      time.typ = aggregate(months_postdoc ~ years + job_type, tmType, median)
      colnames(time.typ)[colnames(time.typ)=="months_postdoc"] <- "Median_time"
      time.typ$typ_num = as.numeric(time.typ$job_type)
      p <- ggplot(tmType, aes(x = years, y = months_postdoc, fill = years)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.typ, aes(label=round(Median_time,1), x=typ_num+0.4, y = Median_time))
      p <- p + facet_grid(job_type ~ .)
      p <- p + theme(strip.text.y = element_text(size=10, face="bold"))
    }
    else {
      tmType <- dataGroup[dataGroup$years == timeConvert(input$selectTmTp), ]
      time.typ = aggregate(months_postdoc ~ job_type, tmType, median)
      colnames(time.typ)[colnames(time.typ)=="months_postdoc"] <- "Median_time"
      time.typ$typ_num = as.numeric(time.typ$job_type)
      p <- ggplot(tmType, aes(x = job_type, y = months_postdoc, fill = job_type)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.typ, aes(label=round(Median_time,1), x=typ_num+0.4, y = Median_time))
      p <- p + scale_fill_manual(values=colorJType)
    }
    p <- p + ggtitle("Training Time (months)") + theme(legend.position="none") +
      labs(y='Months', x='')
    gp <- ggplotly(p, width = input$sldWidthTm, height = input$sldHeightTm, tooltip=c("Median_time"))
    gp # %>% layout(margin = list(l = 75))
  })
  
  output$tmTypeTxt  <- renderText({
    slctYear <- timeConvert(input$selectTmTp)
    if (input$selectTmTp == 'Left NIEHS in 2000-2014') {
      tmData = dataGroup
    }
    else {
      tmData = subset(dataGroup, years == slctYear)
    }
    
    tmTYP = aggregate(months_postdoc ~ job_type, tmData, median)
    lgstTYP <- tolower(as.character(tmTYP$job_type[tmTYP$months_postdoc == max(tmTYP$months_postdoc)]))
    
    HTML(paste0("<p><strong>Summary of Training Time Difference among Different Job Types</strong> In ", slctYear, 
                ", alumni who enter into <i>", lgstTYP, "</i> positions have the longest median training time (", max(tmTYP$months_postdoc), " months).</p><p>&nbsp;</p>"))
  })
  
  # training time job type table
  output$tmTypeTable <- renderTable({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Left NIEHS in 2000-2014') {
      tmType <- changeTableHeader(dfTimeTypeAll,c('Job_Type','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else if (input$selectTmTp == 'Trend') {
      tmType <- changeTableHeader(dfTimeTypeYrs,c('Years','Job_Type','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else {
      tmType <- changeTableHeader(dfTimeTypeYrs[dfTimeTypeYrs$years == timeConvert(input$selectTmTp), ],c('Years','Job_Type','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    tmType
  })
  
  # training time job specifics plot
  output$tmSpecPlot <- renderPlotly({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Left NIEHS in 2000-2014') {
      tmSpec <- dataGroup
      time.spc = aggregate(months_postdoc ~ specifics, tmSpec, median)
      colnames(time.spc)[colnames(time.spc)=="months_postdoc"] <- "Median_time"
      time.spc$spc_num = as.numeric(time.spc$specifics)
      p <- ggplot(tmSpec, aes(x = specifics, y = months_postdoc, fill = specifics)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.spc, aes(label=round(Median_time,1), x=spc_num+0.4, y = Median_time))
      p <- p + scale_fill_manual(values=colorJSpec)
    }
    else if (input$selectTmTp == 'Trend') {
      tmSpec <- dataGroup
      time.spc = aggregate(months_postdoc ~ years + specifics, tmSpec, median)
      colnames(time.spc)[colnames(time.spc)=="months_postdoc"] <- "Median_time"
      time.spc$spc_num = as.numeric(time.spc$specifics)
      p <- ggplot(tmSpec, aes(x = years, y = months_postdoc, fill = years)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.spc, aes(label=round(Median_time,1), x=spc_num+0.4, y = Median_time))
      p <- p + facet_grid(specifics ~ .)
      p <- p + theme(strip.text.y = element_text(size=10, face="bold"))
    }
    else {
      tmSpec <- dataGroup[dataGroup$years == timeConvert(input$selectTmTp), ]
      time.spc = aggregate(months_postdoc ~ specifics, tmSpec, median)
      colnames(time.spc)[colnames(time.spc)=="months_postdoc"] <- "Median_time"
      time.spc$spc_num = as.numeric(time.spc$specifics)
      p <- ggplot(tmSpec, aes(x = specifics, y = months_postdoc, fill = specifics)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.spc, aes(label=round(Median_time,1), x=spc_num+0.4, y = Median_time))
      p <- p + scale_fill_manual(values=colorJSpec)
    }
    p <- p + ggtitle("Training Time (months)") + theme(legend.position="none") +
      labs(y='Months', x='')
    gp <- ggplotly(p, width = input$sldWidthTm, height = input$sldHeightTm, tooltip=c("Median_time"))
    gp # %>% layout(margin = list(l = 75))
  })
  
  output$tmSpecTxt  <- renderText({
    slctYear <- timeConvert(input$selectTmTp)
    if (input$selectTmTp == 'Left NIEHS in 2000-2014') {
      tmData = dataGroup
    }
    else {
      tmData = subset(dataGroup, years == slctYear)
    }
    
    tmSPC = aggregate(months_postdoc ~ specifics, tmData, median)
    lgstSPC <- tolower(as.character(tmSPC$specifics[tmSPC$months_postdoc == max(tmSPC$months_postdoc)]))
    
    HTML(paste0("<p><strong>Summary of Training Time Difference among Different Job Specifics</strong> In ", slctYear, 
        ", alumni who enter into <i>", lgstSPC, "</i> positions have the longest median training time (", max(tmSPC$months_postdoc), " months).</p><p>&nbsp;</p>"))
  })
  
  # training time job specifics table
  output$tmSpecTable <- renderTable({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Left NIEHS in 2000-2014') {
      tmSpec <- changeTableHeader(dfTimeSpecAll,c('Job_Specifics','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else if (input$selectTmTp == 'Trend') {
      tmSpec <- changeTableHeader(dfTimeSpecYrs,c('Years','Job_Specifics','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else {
      tmSpec <- changeTableHeader(dfTimeSpecYrs[dfTimeSpecYrs$years == timeConvert(input$selectTmTp), ],c('Years','Job_Specifics','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    
    tmSpec
  })  
  
  # https://stackoverflow.com/questions/19440069/ggplot2-facet-wrap-strip-color-based-on-variable-in-data-set
  # https://stats.stackexchange.com/questions/8206/labeling-boxplots-in-r
  # https://stackoverflow.com/questions/32342616/ggplot-increase-distance-between-boxplots
  # training time gender plot
  output$tmGndrPlot <- renderPlotly({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Left NIEHS in 2000-2014') {
      tmGndr <- dataGroup
      time.gnd = aggregate(months_postdoc ~ gender, tmGndr, median)
      colnames(time.gnd)[colnames(time.gnd)=="months_postdoc"] <- "Median_time"
      time.gnd$g_num = as.numeric(time.gnd$gender)
      p <- ggplot(tmGndr, aes(x = gender, y = months_postdoc, fill = gender)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.gnd, aes(label=round(Median_time,1), x=g_num+0.3, y = Median_time))
      p <- p + scale_fill_manual(values=genderColors)
    }
    else if (input$selectTmTp == 'Trend') {
      tmGndr <- dataGroup
      time.gnd = aggregate(months_postdoc ~ years + gender, tmGndr, median)
      colnames(time.gnd)[colnames(time.gnd)=="months_postdoc"] <- "Median_time"
      time.gnd$g_num = as.numeric(time.gnd$gender)
      p <- ggplot(tmGndr, aes(x = years, y = months_postdoc, fill = years)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.gnd, aes(label=round(Median_time,1), x=g_num+0.3, y = Median_time))
      p <- p + facet_grid(gender ~ .)
      p <- p + theme(strip.text.y = element_text(size=10, face="bold"))
    }
    else {
      tmGndr <- dataGroup[dataGroup$years == timeConvert(input$selectTmTp), ]
      time.gnd = aggregate(months_postdoc ~ gender, tmGndr, median)
      colnames(time.gnd)[colnames(time.gnd)=="months_postdoc"] <- "Median_time"
      time.gnd$g_num = as.numeric(time.gnd$gender)
      p <- ggplot(tmGndr, aes(x = gender, y = months_postdoc, fill = gender)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.gnd, aes(label=round(Median_time,1), x=g_num+0.3, y = Median_time))
      p <- p + scale_fill_manual(values=genderColors)
    }
    p <- p + ggtitle("Training Time (months)") + theme(legend.position="none") +
      labs(y='Months', x='')
    gp <- ggplotly(p, width = input$sldWidthTm, height = input$sldHeightTm, tooltip=c("Median_time"))
    gp %>% layout(margin = list(l = 75))
  })
  
  output$tmGndrTxt  <- renderText({
    slctYear <- timeConvert(input$selectTmTp)
    if (input$selectTmTp == 'Left NIEHS in 2000-2014') {
      tmData = dataGroup
    }
    else {
      tmData = subset(dataGroup, years == slctYear)
    }
    
    tmMale = tmData$months_postdoc[tmData$gender == 'Male']
    tmFemale = tmData$months_postdoc[tmData$gender == 'Female']
    tTst = t.test(tmMale, tmFemale)
    
    if (tTst$p.value < 0.05){
      HTML(paste0("<p><strong>Summary of Training Time Difference between Males and Females</strong> In ", slctYear,
                  ", the difference in training time between male and female alumni is significant (p-value = ", round(tTst$p.value, 3), ").</p><p>&nbsp;</p>"))
    }
    else {
      HTML(paste0("<p><strong>Summary of Training Time Difference between Males and Females</strong> In ", slctYear,
           ", the difference in training time between male and female alumni is not significant (p-value = ", round(tTst$p.value, 3), ").</p><p>&nbsp;</p>"))
    }
  })
  
  # training time gender table
  output$tmGndrTable <- renderTable({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Left NIEHS in 2000-2014') {
      tmGndr <- changeTableHeader(gendTimeAll,c('Gender','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else if (input$selectTmTp == 'Trend') {
      tmGndr <- changeTableHeader(gendTimeGrp,c('Years','Gender','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else {
      tmGndr <- changeTableHeader(gendTimeGrp[gendTimeGrp$years == timeConvert(input$selectTmTp), ],c('Years','Gender','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    tmGndr
  })
  
  # training time dynamic UI
  output$tmDynamicUI<-renderUI({
    # zoom job sector training time plot
    if (input$selectShowTm == 2) {
      fluidRow(
        tabBox(
          title = "Training time (Job sector)", width = 12,
          # The id lets us use input$tab_tmSect on the server to find the current tab
          id = "tab_tmSect",
          tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmSectTxt"), plotlyOutput("tmSectPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmSectTable"))
        )
      )
    }
    # zoom job type training time plot
    else if (input$selectShowTm == 3) {
      fluidRow(
        tabBox(
          title = "Training time (Job type)", width = 12,
          # The id lets us use input$tab_tmType on the server to find the current tab
          id = "tab_tmType",
          tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmTypeTxt"), plotlyOutput("tmTypePlot", height=input$sldHeightTm)),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmTypeTable"))
        )
      )
    }
    # zoom job specifics training time plot
    else if (input$selectShowTm == 4) {
      fluidRow(
        tabBox(
          title = "Training time (Job specifics)", width = 12,
          # The id lets us use input$tab_tmSpec on the server to find the current tab
          id = "tab_tmSpec",
          tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmSpecTxt"), plotlyOutput("tmSpecPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmSpecTable"))
        )
      )
    }
    # zoom gender specifics training time plot
    else if (input$selectShowTm == 5) {
      fluidRow(
        tabBox(
          title = "Training time (Gender differences)", width = 12,
          # The id lets us use input$tab_tmGndr on the server to find the current tab
          id = "tab_tmGndr",
          tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmGndrTxt"), plotlyOutput("tmGndrPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmGndrTable"))
        )
      )
    }
    # default to show all
    else {
      fluidRow(
        column(width = 12,
          tabBox(
            title = "Training time (Job sector)", width = 6,
            # The id lets us use input$tab_tmSectSml on the server to find the current tab
            id = "tab_tmSectSml",
            tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmSectTxt"), plotlyOutput("tmSectPlot", height="400px")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmSectTable"))
          ),
          tabBox(
            title = "Training time (Job type)", width = 6,
            # The id lets us use input$tab_tmTypeSml on the server to find the current tab
            id = "tab_tmTypeSml",
            tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmTypeTxt"), plotlyOutput("tmTypePlot", height="400px")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmTypeTable"))
          )
        ),
        column(width = 12,
          tabBox(
            title = "Training time (Job specifics)", width = 6,
            # The id lets us use input$tab_tmSpecSml on the server to find the current tab
            id = "tab_tmSpecSml",
            tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmSpecTxt"), plotlyOutput("tmSpecPlot", height="400px")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmSpecTable"))
          ),
          tabBox(
            title = "Training time (Gender differences)", width = 6,
            # The id lets us use input$tab_tmGndrSml on the server to find the current tab
            id = "tab_tmGndrSml",
            tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmGndrTxt"), plotlyOutput("tmGndrPlot", height="400px")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmGndrTable"))
          )
        )
      )
    }
  })
  
  # https://stackoverflow.com/questions/17838709/scale-and-size-of-plot-in-rstudio-shiny
  # https://github.com/ropensci/plotly/issues/1036
  #<<< training time
  
  
  #############################################################################
  # Top countries >>>
  #############################################################################
  
  
  #>>> top countries
  ctryData <- subset(dataGroup, country_origin %in% c('United States','China','Japan','India','South Korea'))
  ctryData$country_origin <- as.character(ctryData$country_origin)
  ctryData$country_origin[ctryData$country_origin == 'United States'] <- "U.S."
  ctryData$country_origin[ctryData$country_origin == 'South Korea'] <- "S. Korea"
  ctryData$country_origin <- factor(ctryData$country_origin)
  ctHeight <- function() {
    input$sldHeightCt
  }
  ctWidth <- function() {
    input$sldWidthCt
  } 
  
  # (3/22/18) hard code top sector, top type, top specifics
  setSect <- 'Academic institution'
  setType <- 'Tenure track faculty'
  setSpec <- 'Primarily basic research'
  
  
  # job category plot for top countries
  output$cntrJobPlot <- renderPlot({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Left NIEHS in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    # count job_sector
    ccSctCnt <- topData %>% group_by(job_sector) %>% summarise (cnt = n())
    # topSect <- ccSctCnt$job_sector[ccSctCnt$cnt == max(ccSctCnt$cnt)]
    topSect <- setSect
    # count job_type
    ccTypCnt <- topData %>% group_by(job_type) %>% summarise (cnt = n())
    # topType <- ccTypCnt$job_type[ccTypCnt$cnt == max(ccTypCnt$cnt)]
    topType <- setType
    # count job_specifics
    ccSpcCnt <- topData %>% group_by(specifics) %>% summarise (cnt = n())
    # topSpec <- ccSpcCnt$specifics[ccSpcCnt$cnt == max(ccSpcCnt$cnt)]
    topSpec <- setSpec
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      
      ccSct1 <- ccSctAll
      psctLabel=paste0(round(ccSct1$percent,2) * 100,"%")
      ccSct1$lab <- psctLabel
      # set label of job sector other than "Academic institution" to blank
      ccSct1[ccSct1$job_sector != topSect, ]$lab <- ""
      # add number info to country
      ccSct1$country_origin <- as.character(ccSct1$country_origin)
      ccSct1$country_origin <- paste(ccSct1$country_origin, paste0("N=",ccSct1$catcnt), sep="\n")
      ccSct1$country_origin <- factor(ccSct1$country_origin)
      
      p1 <- ggplot(ccSct1,aes(x=country_origin,y=job_sector)) +
        geom_point(aes(size=percent,color=job_sector))+
        scale_size_area(name="Job Sector\n(% within\nCountry)",max_size=20,labels=scales::percent,breaks = seq(0, 1, 0.2)) + 
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p1 <- p1 + geom_text(aes(x=country_origin,y=job_sector,label=lab))
      p1 <- p1 + scale_color_manual(values=c("Academic institution"="#04E0E8",
                                           "For-profit company"="#295A29",
                                           "Government agency"= "#61D661",
                                           "Indep./self-employed"="#295A4C",
                                           "Non-profit organization" = "#89F3D2",
                                           "Unknown or Undecided"="#91C844"), guide = FALSE)
      # p1 <- p1 + labs(x="Country of Origin", y= "Job Sector")
      p1 <- p1 + labs(x="Country of Origin", y= "")
      p1 <- p1 + title("Job Sector Distribution in Top 5 Countries")
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, job_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      
      ccJtp1 <- ccJtpAll
      ptypLabel=paste0(round(ccJtp1$percent,2) * 100,"%")
      ccJtp1$lab <- ptypLabel
      # set label of job type other than "Tenure track" to blank
      ccJtp1[ccJtp1$job_type != topType, ]$lab <- ""
      # add number info to country
      ccJtp1$country_origin <- as.character(ccJtp1$country_origin)
      ccJtp1$country_origin <- paste(ccJtp1$country_origin, paste0("N=",ccJtp1$catcnt), sep="\n")
      ccJtp1$country_origin <- factor(ccJtp1$country_origin)
      
      p1 <- ggplot(ccJtp1,aes(x=country_origin,y=job_type)) +
        geom_point(aes(size=percent,color=job_type))+
        scale_size_area(name="Job Type\n(% within\nCountry)",max_size=20,labels=scales::percent,breaks = seq(0, 1, 0.1)) + 
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p1 <- p1 + geom_text(aes(x=country_origin,y=job_type,label=lab))
      p1 <- p1 + scale_color_manual(values=c("Professional staff"="#04E0E8",
                                           "Management"="#295A29",
                                           "Tenure track faculty"= "#61D661",
                                           "Support staff"="#295A4C",
                                           "Non-tenure track faculty" = "#89F3D2",
                                           "Trainee" = "#617D0D" , 
                                           "Unknown or Undecided"="#91C844"), guide = FALSE)
      # p1 <- p1 + labs(x="Country of Origin", y= "Job Type")
      p1 <- p1 + labs(x="Country of Origin", y= "")
      p1 <- p1 + title("Job Type Distribution in Top 5 Countries")
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, specifics) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      
      ccSpc1 <- ccSpcAll
      psctLabel=paste0(round(ccSpc1$percent,2) * 100,"%")
      ccSpc1$lab <- psctLabel
      # set label of job specifics other than "Primarily basic research" to blank
      ccSpc1[ccSpc1$specifics != topSpec, ]$lab <- ""
      # add number info to country
      ccSpc1$country_origin <- as.character(ccSpc1$country_origin)
      ccSpc1$country_origin <- paste(ccSpc1$country_origin, paste0("N=",ccSpc1$catcnt), sep="\n")
      ccSpc1$country_origin <- factor(ccSpc1$country_origin)
      
      p1 <- ggplot(ccSpc1,aes(x=country_origin,y=specifics)) +
        geom_point(aes(size=percent,color=specifics))+
        scale_size_area(name="Job Specifics\n(% within\nCountry)",max_size=20,labels=scales::percent,breaks = seq(0, 1, 0.1)) + 
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p1 <- p1 + geom_text(aes(x=country_origin,y=specifics,label=lab))
      p1 <- p1 + scale_color_manual(values=c("Science writing or communications"="#21327A",
                                           "Primarily applied research"="#97A9F5",
                                           "REST COMBINED"= "#483A77",
                                           "Regulatory affairs" = "#8787FB",
                                           "Science admin./PMT"="#215C7A",
                                           "Computation/informatics"="#9BC2DF",
                                           "Primarily clinical research"="#B399C9",
                                           "Technical/customer support"="#21777A",
                                           "Primarily teaching"="#04E0E8",
                                           "Primarily applied research"="#295A4C",
                                           "Primarily basic research" = "#6C3A77",
                                           "Additional postdoc" = "#892F73" , 
                                           "Unknown or Undecided"="#E461C4"), guide = FALSE)
      # p1 <- p1 + labs(x="Country of Origin", y= "Job Specifics")
      p1 <- p1 + labs(x="Country of Origin", y= "")
      p1 <- p1 + title("Job Specifics Distribution in Top 5 Countries")
    }
    # p1 <- p1 + theme(axis.text = element_text(size=12))
    p1
  },height=ctHeight,width=ctWidth)
  
  output$cntrJobTxt  <- renderText({
    if (input$selectCtPc == 1) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Job Sector</strong>
           A higher percentage of alumni from South Korea and Japan enter into academic institutes versus the remainder of the 'top five' countries based on number of alumni.
           The legend depicting the size of the circles applies to all panels because the job category data is repeated and overlaid with another dimension in the subsequent panels. 
           Percentages are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job sectors within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job sectors within China sums to 100% of Chinese alumni, etc.</p><p>&nbsp;</p>")
    }
    else if (input$selectCtPc == 2) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Job Type</strong>
           A higher percentage of alumni from South Korea and Japan enter into tenure-track positions versus the remainder of the 'top five' countries based on number of alumni.
           The legend depicting the size of the circles applies to all panels because the job category data is repeated and overlaid with another dimension in the subsequent panels. 
           Percentages are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job types within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job types within China sums to 100% of Chinese alumni, etc.</p><p>&nbsp;</p>")
    }
    else {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Job Specifics</strong>
           A higher percentage of alumni from South Korea and Japan do basic research versus the remainder of the 'top five' countries based on number of alumni.
           The legend depicting the size of the circles applies to all panels because the job category data is repeated and overlaid with another dimension in the subsequent panels. 
           Percentages are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job specifics within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job specifics within China sums to 100% of Chinese alumni, etc.</p><p>&nbsp;</p>")
    }
  })
  
  # job category data table for top countries
  output$cntrJobTable <- renderTable({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Left NIEHS in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      changeTableHeader(ccSctAll,c('Country_Origin','Job_Sector','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin'))
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, job_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      changeTableHeader(ccJtpAll,c('Country_Origin','Job_Type','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin'))
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, specifics) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      changeTableHeader(ccSpcAll,c('Country_Origin','Job_Specifics','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin'))
    }
  })  
  
  # training time plot for top countries
  output$cntrTimePlot <- renderPlot({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Left NIEHS in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    # count job_sector
    ccSctCnt <- topData %>% group_by(job_sector) %>% summarise (cnt = n())
    topSect <- setSect
    # count job_type
    ccTypCnt <- topData %>% group_by(job_type) %>% summarise (cnt = n())
    # topType <- ccTypCnt$job_type[ccTypCnt$cnt == max(ccTypCnt$cnt)]
    topType <- setType
    # count job_specifics
    ccSpcCnt <- topData %>% group_by(specifics) %>% summarise (cnt = n())
    topSpec <- setSpec
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      
      # mean time
      mt2 <- aggregate(months_postdoc~country_origin+job_sector,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSctAll,mt2,by=c("country_origin","job_sector"))
      
      # Label
      pmntLabel=paste0(round(ccMntAll$months_postdoc,0),"mo")
      ccMntAll$lab <- pmntLabel
      # set label of job sector other than "Academic institution" to blank
      ccMntAll[ccMntAll$job_sector != topSect, ]$lab <- ""
      
      # add number info to country
      ccMntAll$country_origin <- as.character(ccMntAll$country_origin)
      ccMntAll$country_origin <- paste(ccMntAll$country_origin, paste0("N=",ccMntAll$catcnt), sep="\n")
      ccMntAll$country_origin <- factor(ccMntAll$country_origin)
      # (3/27/18)
      labMnt <- subset(ccMntAll, job_sector == topSect)
      
      p2 <- ggplot(ccMntAll,aes(x=country_origin,y=job_sector)) +
        geom_point(aes(size=percent,fill=months_postdoc),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Job Sector\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p2 <- p2 + geom_label(data=labMnt, aes(x=country_origin,y=job_sector,label=lab))
      p2 <- p2 + scale_fill_gradient2(name="Mean Training\nTime (months)", low="green",mid="purple", high="orange", midpoint=40)
      # p2 <- p2 + labs(x="Country of Origin", y= "Job Sector")
      p2 <- p2 + labs(x="Country of Origin", y= "")
      p2 <- p2 + title("Mean Training Time in Different Job Sectors\nacross Top 5 Countries")
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, job_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      
      # mean time
      mt2 <- aggregate(months_postdoc~country_origin+job_type,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccJtpAll,mt2,by=c("country_origin","job_type"))
      
      # Label
      pmntLabel=paste0(round(ccMntAll$months_postdoc,0),"mo")
      ccMntAll$lab <- pmntLabel
      # set label of job type other than "Tenure track" to blank
      ccMntAll[ccMntAll$job_type != topType, ]$lab <- ""
      
      # add number info to country
      ccMntAll$country_origin <- as.character(ccMntAll$country_origin)
      ccMntAll$country_origin <- paste(ccMntAll$country_origin, paste0("N=",ccMntAll$catcnt), sep="\n")
      ccMntAll$country_origin <- factor(ccMntAll$country_origin)
      # (3/27/18)
      labMnt <- subset(ccMntAll, job_type == topType)
      
      p2 <- ggplot(ccMntAll,aes(x=country_origin,y=job_type)) +
        geom_point(aes(size=percent,fill=months_postdoc),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Job Type\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p2 <- p2 + geom_label(data=labMnt, aes(x=country_origin,y=job_type,label=lab))
      p2 <- p2 + scale_fill_gradient2(name="Mean Training\nTime (months)", low="green",mid="purple", high="orange", midpoint=40)
      # p2 <- p2 + labs(x="Country of Origin", y= "Job Type")
      p2 <- p2 + labs(x="Country of Origin", y= "")
      p2 <- p2 + title("Mean Training Time in Different Job Type\nacross Top 5 Countries")
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, specifics) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      
      # mean time
      mt2 <- aggregate(months_postdoc~country_origin+specifics,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSpcAll,mt2,by=c("country_origin","specifics"))
      
      # Label
      pmntLabel=paste0(round(ccMntAll$months_postdoc,0),"mo")
      ccMntAll$lab <- pmntLabel
      # set label of job specifics other than "Primarily basic research" to blank
      ccMntAll[ccMntAll$specifics != topSpec, ]$lab <- ""
      
      # add number info to country
      ccMntAll$country_origin <- as.character(ccMntAll$country_origin)
      ccMntAll$country_origin <- paste(ccMntAll$country_origin, paste0("N=",ccMntAll$catcnt), sep="\n")
      ccMntAll$country_origin <- factor(ccMntAll$country_origin)
      # (3/27/18)
      labMnt <- subset(ccMntAll, specifics == topSpec)
      
      p2 <- ggplot(ccMntAll,aes(x=country_origin,y=specifics)) +
        geom_point(aes(size=percent,fill=months_postdoc),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Job Specifics\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p2 <- p2 + geom_label(data=labMnt, aes(x=country_origin,y=specifics,label=lab))
      p2 <- p2 + scale_fill_gradient2(name="Mean Training\nTime (months)", low="green",mid="purple", high="orange", midpoint=40)
      # p2 <- p2 + labs(x="Country of Origin", y= "Job Specifics")
      p2 <- p2 + labs(x="Country of Origin", y= "")
      p2 <- p2 + title("Mean Training Time in Different Job Specifics\nacross Top 5 Countries")
    }
    p2
  },height=ctHeight,width=ctWidth)
  
  output$cntrTimeTxt  <- renderText({
    if (input$selectCtPc == 1) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Training Time</strong>
           Alumni from China, South Korea and Japan have shorter training time versus alumni from India and US.
           The legend depicting the size of the circles (from the job category panel) applies to all panels because <u>the job category data is repeated and overlaid with the average training time</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job sectors within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job sectors within China sums to 100% of Chinese alumni, etc. 
           The mean training time (in months) is displayed in text for all alumni within the total selected job sector.</p><p>&nbsp;</p>")
    }
    else if (input$selectCtPc == 2) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Training Time</strong>
           Alumni from China, South Korea and Japan have shorter training time versus alumni from India and US.
           The legend depicting the size of the circles (from the job category panel) applies to all panels because <u>the job category data is repeated and overlaid with the average training time</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job types within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job types within China sums to 100% of Chinese alumni, etc. 
           The mean training time (in months) is displayed in text for all alumni within the total selected job type.</p><p>&nbsp;</p>")
    }
    else {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Training Time</strong>
           Alumni from China, South Korea and Japan have shorter training time versus alumni from India and US.
           The legend depicting the size of the circles (from the job category panel) applies to all panels because <u>the job category data is repeated and overlaid with the average training time</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job specifics within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job specifics within China sums to 100% of Chinese alumni, etc. 
           The mean training time (in months) is displayed in text for all alumni within the total selected job specifics.</p><p>&nbsp;</p>")
    }
  })
  
  # training time data table for top countries
  output$cntrTimeTable <- renderTable({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Left NIEHS in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      # mean time
      mt2 <- aggregate(months_postdoc~country_origin+job_sector,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSctAll,mt2,by=c("country_origin","job_sector"))
      ccMntAll <- changeTableHeader(ccMntAll,c('Country_Origin','Job_Sector','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Avg_Time(Months)'))
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, job_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      # mean time
      mt2 <- aggregate(months_postdoc~country_origin+job_type,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccJtpAll,mt2,by=c("country_origin","job_type"))
      ccMntAll <- changeTableHeader(ccMntAll,c('Country_Origin','Job_Type','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Avg_Time(Months)'))
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, specifics) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      # mean time
      mt2 <- aggregate(months_postdoc~country_origin+specifics,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSpcAll,mt2,by=c("country_origin","specifics"))
      ccMntAll <- changeTableHeader(ccMntAll,c('Country_Origin','Job_Specifics','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Avg_Time(Months)'))
    }
    ccMntAll
  })  

  # gender plot for top countries
  output$cntrGenderPlot <- renderPlot({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Left NIEHS in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    # count job_sector
    ccSctCnt <- topData %>% group_by(job_sector) %>% summarise (cnt = n())
    topSect <- setSect
    # count job_type
    ccTypCnt <- topData %>% group_by(job_type) %>% summarise (cnt = n())
    # topType <- ccTypCnt$job_type[ccTypCnt$cnt == max(ccTypCnt$cnt)]
    topType <- setType
    # count job_specifics
    ccSpcCnt <- topData %>% group_by(specifics) %>% summarise (cnt = n())
    topSpec <- setSpec
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      
      # gender count
      ccGenAll <- topData %>% group_by(country_origin, job_sector, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSctAll,by=c("country_origin","job_sector"))
      
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # Label
      pgenLabel=paste0(round(ccGenAll$mpct,2)*100,"%M")
      ccGenAll$lab <- pgenLabel
      # set label of job sector other than "Academic institution" to blank
      ccGenAll[ccGenAll$job_sector != topSect, ]$lab <- ""
      
      # add number info to country
      ccGenAll$country_origin <- as.character(ccGenAll$country_origin)
      ccGenAll$country_origin <- paste(ccGenAll$country_origin, paste0("N=",ccGenAll$catcnt), sep="\n")
      ccGenAll$country_origin <- factor(ccGenAll$country_origin)
      # (3/27/18)
      labGen <- subset(ccGenAll, job_sector == topSect)
      
      p3 <- ggplot(ccGenAll,aes(x=country_origin,y=job_sector)) +
        geom_point(aes(size=percent,fill=mpct),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Job Sector\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p3 <- p3 + geom_label(data=labGen, aes(x=country_origin,y=job_sector,label=lab))
      p3 <- p3 + scale_fill_gradient2(name="Gender\n(% Male)",low="deeppink2",mid="white", high="dodgerblue1", midpoint=0.5, labels=scales::percent)
      # p3 <- p3 + labs(x="Country of Origin", y= "Job Sector")
      p3 <- p3 + labs(x="Country of Origin", y= "")
      p3 <- p3 + title("Gender Distribution in Different Job Sectors\nacross Top 5 Countries")
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, job_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      
      # gender count
      ccGenAll <- topData %>% group_by(country_origin, job_type, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccJtpAll,by=c("country_origin","job_type"))
      
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # Label
      pgenLabel=paste0(round(ccGenAll$mpct,2)*100,"%M")
      ccGenAll$lab <- pgenLabel
      # set label of job type other than "Tenure track" to blank
      ccGenAll[ccGenAll$job_type != topType, ]$lab <- ""
      
      # add number info to country
      ccGenAll$country_origin <- as.character(ccGenAll$country_origin)
      ccGenAll$country_origin <- paste(ccGenAll$country_origin, paste0("N=",ccGenAll$catcnt), sep="\n")
      ccGenAll$country_origin <- factor(ccGenAll$country_origin)
      # (3/27/18)
      labGen <- subset(ccGenAll, job_type == topType)
      
      p3 <- ggplot(ccGenAll,aes(x=country_origin,y=job_type)) +
        geom_point(aes(size=percent,fill=mpct),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Gender\n(% Male)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p3 <- p3 + geom_label(data=labGen, aes(x=country_origin,y=job_type,label=lab))
      p3 <- p3 + scale_fill_gradient2(name="Male \n(% within Country)",low="deeppink2",mid="white", high="dodgerblue1", midpoint=0.5, labels=scales::percent)
      # p3 <- p3 + labs(x="Country of Origin", y= "Job Type")
      p3 <- p3 + labs(x="Country of Origin", y= "")
      p3 <- p3 + title("Gender Distribution in Different Job Type\nacross Top 5 Countries")
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, specifics) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      
      # gender count
      ccGenAll <- topData %>% group_by(country_origin, specifics, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSpcAll,by=c("country_origin","specifics"))
      
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # Label
      pgenLabel=paste0(round(ccGenAll$mpct,2)*100,"%M")
      ccGenAll$lab <- pgenLabel
      # set label of job specifics other than "Primarily basic research" to blank
      ccGenAll[ccGenAll$specifics != topSpec, ]$lab <- ""
      
      # add number info to country
      ccGenAll$country_origin <- as.character(ccGenAll$country_origin)
      ccGenAll$country_origin <- paste(ccGenAll$country_origin, paste0("N=",ccGenAll$catcnt), sep="\n")
      ccGenAll$country_origin <- factor(ccGenAll$country_origin)
      # (3/27/18)
      labGen <- subset(ccGenAll, specifics == topSpec)
      
      p3 <- ggplot(ccGenAll,aes(x=country_origin,y=specifics)) +
        geom_point(aes(size=percent,fill=mpct),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Job Specifics\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p3 <- p3 + geom_label(data=labGen, aes(x=country_origin,y=specifics,label=lab))
      p3 <- p3 + scale_fill_gradient2(name="Gender\n(% Male)",low="deeppink2",mid="white", high="dodgerblue1", midpoint=0.5, labels=scales::percent)
      # p3 <- p3 + labs(x="Country of Origin", y= "Job Specifics")
      p3 <- p3 + labs(x="Country of Origin", y= "")
      p3 <- p3 + title("Gender Distribution in Different Job Specifics\nacross Top 5 Countries")
    }
    p3
  },height=ctHeight,width=ctWidth)
  
  output$cntrGenderTxt <- renderText({
    if (input$selectCtPc == 1) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Gender Ratio</strong>
           A higher percentage of alumni from South Korea and Japan are male versus the remainder of the 'top five' countries based on number of alumni.
           The legend depicting the size of the circles (from the job category panel) applies to all panels because the <u>job category data is repeated and overlaid with gender composition (100% male to 0% male)</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job sectors within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job sectors within China sums to 100% of Chinese alumni, etc. 
           Percentages shown as text indicate the relative percentage of males within the total selected job sector.</p><p>&nbsp;</p>")
    }
    else if (input$selectCtPc == 2) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Gender Ratio</strong>
           A higher percentage of alumni from South Korea and Japan are male versus the remainder of the 'top five' countries based on number of alumni.
           The legend depicting the size of the circles (from the job category panel) applies to all panels because the <u>job category data is repeated and overlaid with gender composition (100% male to 0% male)</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job types within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job types within China sums to 100% of Chinese alumni, etc. 
           Percentages shown as text indicate the relative percentage of males within the total selected job type.</p><p>&nbsp;</p>")
    }
    else {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Gender Ratio</strong>
           A higher percentage of alumni from South Korea and Japan are male versus the remainder of the 'top five' countries based on number of alumni.
           The legend depicting the size of the circles (from the job category panel) applies to all panels because the <u>job category data is repeated and overlaid with gender composition (100% male to 0% male)</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job specifics within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job specifics within China sums to 100% of Chinese alumni, etc. 
           Percentages shown as text indicate the relative percentage of males within the total selected job specifics.</p><p>&nbsp;</p>")
    }
  })
  
  # gender data table for top countries
  output$cntrGenderTable <- renderTable({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Left NIEHS in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      # gender count
      ccGenAll <- topData %>% group_by(country_origin, job_sector, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSctAll,by=c("country_origin","job_sector"))
      
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # Label
      pgenLabel=paste0(round(ccGenAll$mpct,2)*100,"%")
      ccGenAll$Male_Percent <- pgenLabel
      ccGenAll$mpct <- NULL
      ccGenAll <- changeTableHeader(ccGenAll,c('Country_Origin','Job_Sector','Gender','Gender_Count','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Percent_Male'))
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, job_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      # gender count
      ccGenAll <- topData %>% group_by(country_origin, job_type, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccJtpAll,by=c("country_origin","job_type"))
      
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # Label
      pgenLabel=paste0(round(ccGenAll$mpct,2)*100,"%")
      ccGenAll$Male_Percent <- pgenLabel
      ccGenAll$mpct <- NULL
      ccGenAll <- changeTableHeader(ccGenAll,c('Country_Origin','Job_Type','Gender','Gender_Count','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Percent_Male'))
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, specifics) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      # gender count
      ccGenAll <- topData %>% group_by(country_origin, specifics, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSpcAll,by=c("country_origin","specifics"))
      
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # Label
      pgenLabel=paste0(round(ccGenAll$mpct,2)*100,"%")
      ccGenAll$Male_Percent <- pgenLabel
      ccGenAll$mpct <- NULL
      ccGenAll <- changeTableHeader(ccGenAll,c('Country_Origin','Job_Specifics','Gender','Gender_Count','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Percent_Male'))
    }
    ccGenAll
  })  

  # job location plot for top countries
  output$cntrLocationPlot <- renderPlot({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Left NIEHS in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    # count job_sector
    ccSctCnt <- topData %>% group_by(job_sector) %>% summarise (cnt = n())
    topSect <- setSect
    # count job_type
    ccTypCnt <- topData %>% group_by(job_type) %>% summarise (cnt = n())
    # topType <- ccTypCnt$job_type[ccTypCnt$cnt == max(ccTypCnt$cnt)]
    topType <- setType
    # count job_specifics
    ccSpcCnt <- topData %>% group_by(specifics) %>% summarise (cnt = n())
    topSpec <- setSpec
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      
      # count location in US
      ccLocAll <- clocData %>% group_by(country_origin, job_sector, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSctAll,by=c("country_origin","job_sector"))
      
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      # Label
      plocLabel=paste0(round(ccLocAll$upct,2)*100,"%US")
      ccLocAll$lab <- plocLabel
      # set label of job sector other than "Academic institution" to blank
      ccLocAll[ccLocAll$job_sector != topSect, ]$lab <- ""
      
      # add number info to country
      ccLocAll$country_origin <- as.character(ccLocAll$country_origin)
      ccLocAll$country_origin <- paste(ccLocAll$country_origin, paste0("N=",ccLocAll$catcnt), sep="\n")
      ccLocAll$country_origin <- factor(ccLocAll$country_origin)
      # (3/27/18)
      labLoc <- subset(ccLocAll, job_sector == topSect)
      
      p4 <- ggplot(ccLocAll,aes(x=country_origin,y=job_sector)) +
        geom_point(aes(size=percent,fill=upct),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Job Sector\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p4 <- p4 + geom_label(data=labLoc, aes(x=country_origin,y=job_sector,label=lab))
      p4 <- p4 + scale_fill_gradient2(name="Job Location\n(% working\nin US)",low="purple4",mid="white", high="green4", midpoint=0.5, labels=scales::percent)
      # p4 <- p4 + labs(x="Country of Origin", y= "Job Sector")
      p4 <- p4 + labs(x="Country of Origin", y= "")
      p4 <- p4 + title("Job Location in Different Job Sectors\nacross Top 5 Countries")
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, job_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      
      # count location in US
      ccLocAll <- clocData %>% group_by(country_origin, job_type, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccJtpAll,by=c("country_origin","job_type"))
      
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      # Label
      plocLabel=paste0(round(ccLocAll$upct,2)*100,"%US")
      ccLocAll$lab <- plocLabel
      # set label of job type other than "Tenure track" to blank
      ccLocAll[ccLocAll$job_type != topType, ]$lab <- ""
      
      # add number info to country
      ccLocAll$country_origin <- as.character(ccLocAll$country_origin)
      ccLocAll$country_origin <- paste(ccLocAll$country_origin, paste0("N=",ccLocAll$catcnt), sep="\n")
      ccLocAll$country_origin <- factor(ccLocAll$country_origin)
      # (3/27/18)
      labLoc <- subset(ccLocAll, job_type == topType)
      
      p4 <- ggplot(ccLocAll,aes(x=country_origin,y=job_type)) +
        geom_point(aes(size=percent,fill=upct),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Job Type\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p4 <- p4 + geom_label(data=labLoc, aes(x=country_origin,y=job_type,label=lab))
      p4 <- p4 + scale_fill_gradient2(name="Job Location\n(% working\nin US)",low="purple4",mid="white", high="green4", midpoint=0.5, labels=scales::percent)
      # p4 <- p4 + labs(x="Country of Origin", y= "Job Type")
      p4 <- p4 + labs(x="Country of Origin", y= "")
      p4 <- p4 + title("Job Location in Different Job Type\nacross Top 5 Countries")
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, specifics) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      
      # count location in US
      ccLocAll <- clocData %>% group_by(country_origin, specifics, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSpcAll,by=c("country_origin","specifics"))
      
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      # Label
      plocLabel=paste0(round(ccLocAll$upct,2)*100,"%US")
      ccLocAll$lab <- plocLabel
      # set label of job specifics other than "Primarily basic research" to blank
      ccLocAll[ccLocAll$specifics != topSpec, ]$lab <- ""
      
      # add number info to country
      ccLocAll$country_origin <- as.character(ccLocAll$country_origin)
      ccLocAll$country_origin <- paste(ccLocAll$country_origin, paste0("N=",ccLocAll$catcnt), sep="\n")
      ccLocAll$country_origin <- factor(ccLocAll$country_origin)
      # (3/27/18)
      labLoc <- subset(ccLocAll, specifics == topSpec)
      
      p4 <- ggplot(ccLocAll,aes(x=country_origin,y=specifics)) +
        geom_point(aes(size=percent,fill=upct),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Job Specifics\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p4 <- p4 + geom_label(data=labLoc, aes(x=country_origin,y=specifics,label=lab))
      p4 <- p4 + scale_fill_gradient2(name="Job Location\n(% working\nin US)",low="purple4",mid="white", high="green4", midpoint=0.5, labels=scales::percent)
      # p4 <- p4 + labs(x="Country of Origin", y= "Job Specifics")
      p4 <- p4 + labs(x="Country of Origin", y= "")
      p4 <- p4 + title("Job Location in Different Job Specifics\nacross Top 5 Countries")
    }
    p4
  },height=ctHeight,width=ctWidth)
  
  output$cntrLocationTxt <- renderText({
    if (input$selectCtPc == 1) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Work Location</strong>
           A lower percentage of alumni from South Korea and Japan remain in US after their training versus the remainder of the 'top five' countries based on number of alumni.
           The legend depicting the size of the circles (from the job category panel) applies to all panels because the <u>job category data is repeated and overlaid with work location (100% working in US to 0% working in the US)</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job sectors within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job sectors within China sums to 100% of Chinese alumni, etc. 
           Percentages shown as text indicate the relative percentage of those working in the US within the total selected job sector.</p><p>&nbsp;</p>")
    }
    else if (input$selectCtPc == 2) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Work Location</strong>
           A lower percentage of alumni from South Korea and Japan remain in US after their training versus the remainder of the 'top five' countries based on number of alumni.
           The legend depicting the size of the circles (from the job category panel) applies to all panels because the <u>job category data is repeated and overlaid with work location (100% working in US to 0% working in the US)</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job types within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job types within China sums to 100% of Chinese alumni, etc. 
           Percentages shown as text indicate the relative percentage of those working in the US within the total selected job type.</p><p>&nbsp;</p>")
    }
    else {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Work Location</strong>
           A lower percentage of alumni from South Korea and Japan remain in US after their training versus the remainder of the 'top five' countries based on number of alumni.
           The legend depicting the size of the circles (from the job category panel) applies to all panels because the <u>job category data is repeated and overlaid with work location (100% working in US to 0% working in the US)</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job specifics within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job specifics within China sums to 100% of Chinese alumni, etc. 
           Percentages shown as text indicate the relative percentage of those working in the US within the total selected job specifics.</p><p>&nbsp;</p>")
    }
  })

  # job location data table for top countries
  output$cntrLocationTable <- renderTable({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Left NIEHS in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      
      # count location in US
      ccLocAll <- clocData %>% group_by(country_origin, job_sector, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSctAll,by=c("country_origin","job_sector"))
      
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      # Label
      plocLabel=paste0(round(ccLocAll$upct,2)*100,"%")
      ccLocAll$Percent_in_US <- plocLabel
      ccLocAll$upct <- NULL
      ccLocAll <- changeTableHeader(ccLocAll,c('Country_Origin','Job_Sector','Job_Location','Location_Count','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Percent_InUS'))
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, job_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      
      # count location in US
      ccLocAll <- clocData %>% group_by(country_origin, job_type, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccJtpAll,by=c("country_origin","job_type"))
      
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      # Label
      plocLabel=paste0(round(ccLocAll$upct,2)*100,"%")
      ccLocAll$Percent_in_US <- plocLabel
      ccLocAll$upct <- NULL
      ccLocAll <- changeTableHeader(ccLocAll,c('Country_Origin','Job_Type','Job_Location','Location_Count','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Percent_InUS'))
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, specifics) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      
      # count location in US
      ccLocAll <- clocData %>% group_by(country_origin, specifics, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSpcAll,by=c("country_origin","specifics"))
      
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      # Label
      plocLabel=paste0(round(ccLocAll$upct,2)*100,"%")
      ccLocAll$Percent_in_US <- plocLabel
      ccLocAll$upct <- NULL
      ccLocAll <- changeTableHeader(ccLocAll,c('Country_Origin','Job_Specifics','Job_Location','Location_Count','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Percent_InUS'))
    }
    ccLocAll
  })  

  # top countries dynamic UI
  output$cntrDynamicUI<-renderUI({
    # zoom job category plot
    if (input$selectShowCt == 2) {
      fluidRow(
        tabBox(
          title = "Job category", width = 12,
          # The id lets us use input$ctJob on the server to find the current tab
          id = "tab_ctJob",
          tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrJobTxt"), plotOutput("cntrJobPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrJobTable"))
        )
      )
    }
    # zoom training time plot
    else if (input$selectShowCt == 3) {
      fluidRow(
        tabBox(
          title = "Training time", width = 12,
          # The id lets us use input$tab_ctTime on the server to find the current tab
          id = "tab_ctTime",
          tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrTimeTxt"), plotOutput("cntrTimePlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrTimeTable"))
        )
      )
    }
    # zoom gender plot
    else if (input$selectShowCt == 4) {
      fluidRow(
        tabBox(
          title = "Gender", width = 12,
          # The id lets us use input$tab_ctGender on the server to find the current tab
          id = "tab_ctGender",
          tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrGenderTxt"), plotOutput("cntrGenderPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrGenderTable"))
        )
      )
    }
    # zoom gender specifics training time plot
    else if (input$selectShowCt == 5) {
      fluidRow(
        tabBox(
          title = "Location", width = 12,
          # The id lets us use input$tab_ctLocation on the server to find the current tab
          id = "tab_ctLocation",
          tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrLocationTxt"), plotOutput("cntrLocationPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrLocationTable"))
        )
      )
    }
    # default to show all
    else {
      fluidRow(
        column(width = 12,
          tabBox(
            title = "Job category", width = 6,
            # The id lets us use input$ctJobSml on the server to find the current tab
            id = "tab_ctJobSml",
            tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrJobTxt"), plotOutput("cntrJobPlot")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrJobTable"))
          ),
          tabBox(
            title = "Training time", width = 6,
            # The id lets us use input$ctTimeSml on the server to find the current tab
            id = "tab_ctTimeSml",
            tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrTimeTxt"), plotOutput("cntrTimePlot")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrTimeTable"))
          )
        ),
        column(width = 12,
          tabBox(
            title = "Gender", width = 6,
            # The id lets us use input$tab_ctGenderSml on the server to find the current tab
            id = "tab_ctGenderSml",
            tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrGenderTxt"), plotOutput("cntrGenderPlot")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrGenderTable"))
          ),
          tabBox(
            title = "Location", width = 6,
            # The id lets us use input$ctLocationSml on the server to find the current tab
            id = "tab_ctLocationSml",
            tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrLocationTxt"), plotOutput("cntrLocationPlot")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrLocationTable"))
          )
        )
      )
    }
  })
  
  # top countries information boxes
  output$cntrInfoBox<-renderUI({
    if (input$selectCtTp == 'Left NIEHS in 2000-2014') {
      topData = dataGroup
    }
    else {
      topData = dataGroup[dataGroup$years == timeConvert(input$selectCtTp), ]
    }
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt))
      # topSect <- ccSctAll$job_sector[ccSctAll$cnt == max(ccSctAll$cnt)]
      topSect <- setSect

      # mean time
      mt2 <- aggregate(months_postdoc~job_sector,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSctAll,mt2,by=c("job_sector"))

      # gender count
      ccGenAll <- topData %>% group_by(job_sector, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSctAll,by=c("job_sector"))
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      # count location in US
      ccLocAll <- clocData %>% group_by(job_sector, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSctAll,by=c("job_sector"))
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      boxData = list(paste0("Sector: '",topSect,"'"),
                     ccSctAll$percent[ccSctAll$job_sector==topSect],
                     ccMntAll$months_postdoc[ccMntAll$job_sector==topSect],
                     ccGenAll$mpct[ccGenAll$job_sector==topSect],
                     ccLocAll$upct[ccLocAll$job_sector==topSect])
    }
    else if (input$selectCtPc == 2) {
      ccSctAll <- topData %>% group_by(job_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt))
      # topType <- ccSctAll$job_type[ccSctAll$cnt == max(ccSctAll$cnt)]
      topType <- setType
      
      # mean time
      mt2 <- aggregate(months_postdoc~job_type,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSctAll,mt2,by=c("job_type"))
      
      # gender count
      ccGenAll <- topData %>% group_by(job_type, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSctAll,by=c("job_type"))
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      # count location in US
      ccLocAll <- clocData %>% group_by(job_type, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSctAll,by=c("job_type"))
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)

      boxData = list(paste0("Type: '",topType,"'"),
                     ccSctAll$percent[ccSctAll$job_type==topType],
                     ccMntAll$months_postdoc[ccMntAll$job_type==topType],
                     ccGenAll$mpct[ccGenAll$job_type==topType],
                     ccLocAll$upct[ccLocAll$job_type==topType])
    }
    else {
      ccSctAll <- topData %>% group_by(specifics) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt))
      # topSpec <- ccSctAll$specifics[ccSctAll$cnt == max(ccSctAll$cnt)]
      topSpec <- setSpec
      
      # mean time
      mt2 <- aggregate(months_postdoc~specifics,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSctAll,mt2,by=c("specifics"))
      
      # gender count
      ccGenAll <- topData %>% group_by(specifics, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSctAll,by=c("specifics"))
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      # count location in US
      ccLocAll <- clocData %>% group_by(specifics, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSctAll,by=c("specifics"))
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)

      boxData = list(paste0("Specifics: '", topSpec, "'"),
                     ccSctAll$percent[ccSctAll$specifics==topSpec],
                     ccMntAll$months_postdoc[ccMntAll$specifics==topSpec],
                     ccGenAll$mpct[ccGenAll$specifics==topSpec],
                     ccLocAll$upct[ccLocAll$specifics==topSpec])
    }
    

    box(title = paste0("General Distribution of Selected ", boxData[[1]]),
      width = 12,
      fluidRow(
        valueBox(subtitle="Percent in this selected job", value=tags$p(paste0(round(boxData[[2]],3)*100,"%"),style="font-size:75%;"), icon = icon("th-large"), color = "maroon", width=3),
        valueBox(subtitle="Average training time (months)", value=tags$p(round(boxData[[3]],1),style="font-size:75%;"), icon = icon("hourglass-3"), color = "purple", width=3),
        valueBox(subtitle="Percent male", value=tags$p(paste0(round(boxData[[4]],3)*100,"%"),style="font-size:75%;"), icon = icon("male"), color = "blue", width=3),
        valueBox(subtitle="Percent working in US ", value=tags$p(paste0(round(boxData[[5]],3)*100,"%"),style="font-size:75%;"), icon = icon("home"), color = "green", width=3)
      )
    )
  })
  #<<< top countries
  
  
  #############################################################################
  # Degree >>>
  #############################################################################
  
  
  #>>> degree
  # https://moderndata.plot.ly/easy-error-bars-with-r-and-plotly/; http://rpubs.com/chelsea/68601
  # https://plot.ly/r/error-bars/
  # https://stackoverflow.com/questions/42905686/error-bars-in-plot-ly
  # https://plotly-book.cpsievert.me/scatter-traces.html
  # https://stackoverflow.com/questions/44638590/plotly-in-r-format-axis-tick-labels-to-percentage
  # https://plot.ly/r/bar-charts/; https://plot.ly/r/shiny-gallery/
  dgHeight <- function() {
    input$sldHeightDg
  }
  dgWidth <- function() {
    input$sldWidthDg
  }
  
  # job sector plot for study degree fields
  output$degrJSectPlot <- renderPlot({
    if (input$selectDgTp == 'Left NIEHS in 2000-2014') {
      degrData = dataGroup
    }
    else {
      degrData = dataGroup[dataGroup$years == timeConvert(input$selectDgTp), ]
    }
    
    # make the degree field names match to paper
    degrData$degree_category <- as.character(degrData$degree_category)
    degrData$degree_category[degrData$degree_category=='Biomathematics/Bioinformatics/Computational Biology'] <- 'Biostatistics/Computational Biology*'
    degrData$degree_category <- factor(degrData$degree_category)
    
    
    degrCnt <- degrData %>% group_by(job_sector) %>% summarise (cnt = n())
    # topSect <- degrCnt$job_sector[degrCnt$cnt == max(degrCnt$cnt)]
    topSect <- 'Academic institution'
    
    dgcTypeAll <- degrData %>% group_by(degree_category, job_sector) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
    degrType <- dgcTypeAll %>% group_by(degree_category) %>% mutate(catcnt=sum(cnt))
    if (input$selectDgTp == 'Left NIEHS in 2000-2014') {
      degrType <- subset(degrType, catcnt >= 10)
    }
    else {
      degrType <- subset(degrType, catcnt >= 5)
    }
    degrType_TT <- subset(degrType, job_sector == topSect)
    # (1/8/18: change test from prop.test to binom.test due to error warning 'In prop.test(cnt, catcnt) : Chi-squared approximation may be incorrect')
    # degrType_TTci <- degrType_TT %>% mutate(low=prop.test(cnt,catcnt)$conf.int[1], upper=prop.test(cnt,catcnt)$conf.int[2])
    degrType_TTci <- degrType_TT %>% mutate(low=binom.test(cnt,catcnt)$conf.int[1], upper=binom.test(cnt,catcnt)$conf.int[2])
    
    # sort degree by percentage in tenure track
    # http://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html
    #   x$name <- factor(x$name, levels = x$name[order(x$val)])
    degrType_TTci$degree_category <- as.character(degrType_TTci$degree_category)
    degrType_TTci$degree_category <- wrap_strings(degrType_TTci$degree_category,30)
    srtDegrTTpct <- factor(degrType_TTci$degree_category, levels=degrType_TTci$degree_category[order(degrType_TTci$freq)])
    degrType_TTci$degree_category <- srtDegrTTpct
    pctLabel=paste0(round(degrType_TTci$freq,2) * 100,"%")
    
    p <- ggplot(degrType_TTci,aes(degree_category, y=freq, ymin=low, ymax=upper)) + 
      geom_errorbar(color="grey", width=0.7) + geom_point(stat="identity", size=3) + coord_flip() +
      scale_y_continuous(labels=scales::percent, breaks=pretty_breaks(n=5))
    # (1/8/18: fix the ggplot2 text label problem: "Error in eval(expr, envir, enclos) : object '.group' not found")
    # p <- p + geom_text(data=degrType_TTci, aes(y = freq+0.05, label = pctLabel)) + theme_classic() + 
    p <- p + geom_text(aes(label = pctLabel), hjust=-0.2,vjust=-0.2) + theme_classic() + 
      theme(axis.text=element_text(size=12, face = "bold"), axis.title=element_text(size=14,face="bold")) +
      # labs(x="Degree Study Field", y= paste0("Proportion in '", topSect, "'"),
      labs(x="", y= paste0("Proportion in '", topSect, "'"),
           panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    p
  },height=dgHeight,width=dgWidth)
  output$degrJSectTxt  <- renderText({HTML("<p><strong>Percentage of Alumni that Enter into Academic Institution Positions Based on Their Doctoral Degree Field</strong> 
                                           Alumni doctoral degree fields were standardized according to the main program groups defined within NCES' Biological and Biomedical Sciences instructional programs. 
                                           The relative percentage of alumni within each degree field entering into academic institute positions is shown. 
                                           The 95% confidence intervals for the binomial proportion are shown here.
                                           *, the main NCES program group is ‘<i>Biomathematics, Bioinformatics, and Computational Biology</i>;’ ‘<i>Biostatistics</i>’ was substituted for <i>‘Biomathematics’ and ‘Bioinformatics’</i> within the title because nearly all alumni in this category possessed a degree in statistics or biostatistics.</p><p>&nbsp;</p>")})
  
  # job sector data table for study degree fields
  output$degrJSectTable <- renderTable({
    if (input$selectDgTp == 'Left NIEHS in 2000-2014') {
      degrData = dataGroup
    }
    else {
      degrData = dataGroup[dataGroup$years == timeConvert(input$selectDgTp), ]
    }
    
    dgcTypeAll <- degrData %>% group_by(degree_category, job_sector) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
    degrType <- dgcTypeAll %>% group_by(degree_category) %>% mutate(catcnt=sum(cnt))
    changeTableHeader(degrType,c('Degree_Field','Job_Sector','Alumni_Count','Alumni_Portion','TotalAlmumni_InDegree'))
  })  
  
  # job type plot for study degree fields
  output$degrJTypePlot <- renderPlot({
    if (input$selectDgTp == 'Left NIEHS in 2000-2014') {
      degrData = dataGroup
    }
    else {
      degrData = dataGroup[dataGroup$years == timeConvert(input$selectDgTp), ]
    }
    
    # make the degree field names match to paper
    degrData$degree_category <- as.character(degrData$degree_category)
    degrData$degree_category[degrData$degree_category=='Biomathematics/Bioinformatics/Computational Biology'] <- 'Biostatistics/Computational Biology*'
    degrData$degree_category <- factor(degrData$degree_category)
    
    degrCnt <- degrData %>% group_by(job_type) %>% summarise (cnt = n())
    # topType <- degrCnt$job_type[degrCnt$cnt == max(degrCnt$cnt)]
    topType <- 'Tenure track faculty'
    
    dgcTypeAll <- degrData %>% group_by(degree_category, job_type) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
    degrType <- dgcTypeAll %>% group_by(degree_category) %>% mutate(catcnt=sum(cnt))
    if (input$selectDgTp == 'Left NIEHS in 2000-2014') {
      degrType <- subset(degrType, catcnt >= 10)
    }
    else {
      degrType <- subset(degrType, catcnt >= 5)
    }
    degrType_TT <- subset(degrType, job_type == topType)
    # (1/8/18: change test from prop.test to binom.test due to error warning 'In prop.test(cnt, catcnt) : Chi-squared approximation may be incorrect')
    # degrType_TTci <- degrType_TT %>% mutate(low=prop.test(cnt,catcnt)$conf.int[1], upper=prop.test(cnt,catcnt)$conf.int[2])
    degrType_TTci <- degrType_TT %>% mutate(low=binom.test(cnt,catcnt)$conf.int[1], upper=binom.test(cnt,catcnt)$conf.int[2])
    
    # sort degree by percentage in tenure track
    # http://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html
    #   x$name <- factor(x$name, levels = x$name[order(x$val)])
    degrType_TTci$degree_category <- as.character(degrType_TTci$degree_category)
    degrType_TTci$degree_category <- wrap_strings(degrType_TTci$degree_category,30)
    srtDegrTTpct <- factor(degrType_TTci$degree_category, levels=degrType_TTci$degree_category[order(degrType_TTci$freq)])
    degrType_TTci$degree_category <- srtDegrTTpct
    pctLabel=paste0(round(degrType_TTci$freq,2) * 100,"%")
    
    p <- ggplot(degrType_TTci,aes(degree_category, y=freq, ymin=low, ymax=upper)) + 
      geom_errorbar(color="grey", width=0.7) + geom_point(stat="identity", size=3) + coord_flip() +
      scale_y_continuous(labels=scales::percent, breaks=pretty_breaks(n=5))
    # (1/8/18: fix the ggplot2 text label problem: "Error in eval(expr, envir, enclos) : object '.group' not found")
    # p <- p + geom_text(data=degrType_TTci, aes(y = freq+0.05, label = pctLabel)) + theme_classic() + 
    p <- p + geom_text(aes(label = pctLabel), hjust=-0.2,vjust=-0.2) + theme_classic() + 
      theme(axis.text=element_text(size=12, face = "bold"), axis.title=element_text(size=14,face="bold")) +
      # labs(x="Degree Study Field", y= paste0("Proportion in '", topType, "'"),
      labs(x="", y= paste0("Proportion in '", topType, "'"),
           panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    p
  },height=dgHeight,width=dgWidth)
  output$degrJTypeTxt  <- renderText({HTML("<p><strong>Percentage of Alumni that Enter into Tenure-Track Positions Based on Their Doctoral Degree Field</strong> 
                                           Alumni doctoral degree fields were standardized according to the main program groups defined within NCES' Biological and Biomedical Sciences instructional programs. 
                                           The relative percentage of alumni within each degree field entering into tenure-track positions is shown. 
                                           The 95% confidence intervals for the binomial proportion are shown here.
                                           *, the main NCES program group is ‘<i>Biomathematics, Bioinformatics, and Computational Biology</i>;’ ‘<i>Biostatistics</i>’ was substituted for <i>‘Biomathematics’ and ‘Bioinformatics’</i> within the title because nearly all alumni in this category possessed a degree in statistics or biostatistics.</p><p>&nbsp;</p>")})
  
  # job type data table for study degree fields
  output$degrJTypeTable <- renderTable({
    if (input$selectDgTp == 'Left NIEHS in 2000-2014') {
      degrData = dataGroup
    }
    else {
      degrData = dataGroup[dataGroup$years == timeConvert(input$selectDgTp), ]
    }
    
    dgcTypeAll <- degrData %>% group_by(degree_category, job_type) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
    degrType <- dgcTypeAll %>% group_by(degree_category) %>% mutate(catcnt=sum(cnt))
    changeTableHeader(degrType,c('Degree_Field','Job_Type','Alumni_Count','Alumni_Portion','TotalAlmumni_InDegree'))
  })
  
  # job specifics plot for study degree fields
  output$degrJSpecPlot <- renderPlot({
    if (input$selectDgTp == 'Left NIEHS in 2000-2014') {
      degrData = dataGroup
    }
    else {
      degrData = dataGroup[dataGroup$years == timeConvert(input$selectDgTp), ]
    }
    
    # make the degree field names match to paper
    degrData$degree_category <- as.character(degrData$degree_category)
    degrData$degree_category[degrData$degree_category=='Biomathematics/Bioinformatics/Computational Biology'] <- 'Biostatistics/Computational Biology*'
    degrData$degree_category <- factor(degrData$degree_category)
    
    degrCnt <- degrData %>% group_by(specifics) %>% summarise (cnt = n())
    # topSpec <- degrCnt$specifics[degrCnt$cnt == max(degrCnt$cnt)]
    topSpec <- 'Primarily basic research'
    
    dgcTypeAll <- degrData %>% group_by(degree_category, specifics) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
    degrType <- dgcTypeAll %>% group_by(degree_category) %>% mutate(catcnt=sum(cnt))
    if (input$selectDgTp == 'Left NIEHS in 2000-2014') {
      degrType <- subset(degrType, catcnt >= 10)
    }
    else {
      degrType <- subset(degrType, catcnt >= 5)
    }
    degrType_TT <- subset(degrType, specifics == topSpec)
    # (1/8/18: change test from prop.test to binom.test due to error warning 'In prop.test(cnt, catcnt) : Chi-squared approximation may be incorrect')
    #   http://www.sthda.com/english/wiki/one-proportion-z-test-in-r
    # degrType_TTci <- degrType_TT %>% mutate(low=prop.test(cnt,catcnt)$conf.int[1], upper=prop.test(cnt,catcnt)$conf.int[2])
    degrType_TTci <- degrType_TT %>% mutate(low=binom.test(cnt,catcnt)$conf.int[1], upper=binom.test(cnt,catcnt)$conf.int[2])
    
    # sort degree by percentage in tenure track
    # http://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html
    #   x$name <- factor(x$name, levels = x$name[order(x$val)])
    degrType_TTci$degree_category <- as.character(degrType_TTci$degree_category)
    degrType_TTci$degree_category <- wrap_strings(degrType_TTci$degree_category,30)
    srtDegrTTpct <- factor(degrType_TTci$degree_category, levels=degrType_TTci$degree_category[order(degrType_TTci$freq)])
    degrType_TTci$degree_category <- srtDegrTTpct
    pctLabel=paste0(round(degrType_TTci$freq,2) * 100,"%")
    
    p <- ggplot(degrType_TTci,aes(degree_category, y=freq, ymin=low, ymax=upper)) + 
      geom_errorbar(color="grey", width=0.7) + geom_point(stat="identity", size=3) + coord_flip() +
      scale_y_continuous(labels=scales::percent, breaks=pretty_breaks(n=5))
    # (1/8/18: fix the ggplot2 text label problem: "Error in eval(expr, envir, enclos) : object '.group' not found")
    # p <- p + geom_text(data=degrType_TTci, aes(y = freq+0.05, label = pctLabel)) + theme_classic() + 
    p <- p + geom_text(aes(label = pctLabel), hjust=-0.2,vjust=-0.2) + theme_classic() + 
      theme(axis.text=element_text(size=12, face = "bold"), axis.title=element_text(size=14,face="bold")) +
      # labs(x="Degree Study Field", y= paste0("Proportion in '", topSpec, "'"),
      labs(x="", y= paste0("Proportion in '", topSpec, "'"),
           panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    p
  },height=dgHeight,width=dgWidth)
  output$degrJSpecTxt  <- renderText({HTML("<p><strong>Percentage of Alumni that Enter into Basic Research Positions Based on Their Doctoral Degree Field</strong> 
                                           Alumni doctoral degree fields were standardized according to the main program groups defined within NCES' Biological and Biomedical Sciences instructional programs. 
                                           The relative percentage of alumni within each degree field entering into basic research positions is shown. 
                                           The 95% confidence intervals for the binomial proportion are shown here.
                                           *, the main NCES program group is ‘<i>Biomathematics, Bioinformatics, and Computational Biology</i>;’ ‘<i>Biostatistics</i>’ was substituted for <i>‘Biomathematics’ and ‘Bioinformatics’</i> within the title because nearly all alumni in this category possessed a degree in statistics or biostatistics.</p><p>&nbsp;</p>")})
  
  # job specifics data table for study degree fields
  output$degrJSpecTable <- renderTable({
    if (input$selectDgTp == 'Left NIEHS in 2000-2014') {
      degrData = dataGroup
    }
    else {
      degrData = dataGroup[dataGroup$years == (input$selectDgTp), ]
    }
    
    dgcTypeAll <- degrData %>% group_by(degree_category, specifics) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
    degrType <- dgcTypeAll %>% group_by(degree_category) %>% mutate(catcnt=sum(cnt))
    changeTableHeader(degrType,c('Degree_Field','Job_Specifics','Alumni_Count','Alumni_Portion','TotalAlmumni_InDegree'))
  })  
  
  # degree study field dynamic UI
  output$degrDynamicUI<-renderUI({
    # zoom job category plot
    if (input$selectShowDg == 2) {
      fluidRow(
        tabBox(
          title = "Job sector", width = 12,
          # The id lets us use input$tab_dgSect on the server to find the current tab
          id = "tab_dgSect",
          tabPanel("Pointrange plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("degrJSectTxt"), plotOutput("degrJSectPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("degrJSectTable"))
        )
      )
    }
    # zoom training time plot
    else if (input$selectShowDg == 3) {
      fluidRow(
        tabBox(
          title = "Job type", width = 12,
          # The id lets us use input$tab_dgType on the server to find the current tab
          id = "tab_dgType",
          tabPanel("Pointrange plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("degrJTypeTxt"), plotOutput("degrJTypePlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("degrJTypeTable"))
        )
      )
    }
    # zoom gender plot
    else if (input$selectShowDg == 4) {
      fluidRow(
        tabBox(
          title = "Job specifics", width = 12,
          # The id lets us use input$tab_dgSpec on the server to find the current tab
          id = "tab_dgSpec",
          tabPanel("Pointrange plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("degrJSpecTxt"), plotOutput("degrJSpecPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("degrJSpecTable"))
        )
      )
    }
    # default to show all
    else {
      fluidRow(
        column(width = 12,
               tabBox(
                 title = "Job sector", width = 6,
                 # The id lets us use input$tab_dgSectSml on the server to find the current tab
                 id = "tab_dgSectSml",
                 tabPanel("Pointrange plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("degrJSectTxt"), plotOutput("degrJSectPlot")),
                 tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("degrJSectTable"))
               ),
               tabBox(
                 title = "Job type", width = 6,
                 # The id lets us use input$tab_dgTypeSml on the server to find the current tab
                 id = "tab_dgTypeSml",
                 tabPanel("Pointrange plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("degrJTypeTxt"), plotOutput("degrJTypePlot")),
                 tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("degrJTypeTable"))
               )
        ),
        column(width = 12,
               tabBox(
                 title = "Job specifics", width = 6,
                 # The id lets us use input$tab_dgSpecSml on the server to find the current tab
                 id = "tab_dgSpecSml",
                 tabPanel("Pointrange plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("degrJSpecTxt"), plotOutput("degrJSpecPlot")),
                 tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("degrJSpecTable"))
               )
        )
      )
    }
  })
  
  # degree study field information boxes
  output$degrInfoBox<-renderUI({
    if (input$selectDgTp == 'Left NIEHS in 2000-2014') {
      dgrData = dataGroup
    }
    else {
      dgrData = dataGroup[dataGroup$years == timeConvert(input$selectDgTp), ]
    }
    
    ccSctAll <- dgrData %>% group_by(job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt))
    # topSect <- ccSctAll$job_sector[ccSctAll$cnt == max(ccSctAll$cnt)]
    topSect <- 'Academic institution'
    topSectPct <- ccSctAll$percent[ccSctAll$cnt == max(ccSctAll$cnt)]
      
    ccTypAll <- dgrData %>% group_by(job_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt))
    # topType <- ccTypAll$job_type[ccTypAll$cnt == max(ccTypAll$cnt)]
    topType <- 'Tenure track faculty'
    topTypePct <- ccTypAll$percent[ccTypAll$cnt == max(ccTypAll$cnt)]

    ccSpcAll <- dgrData %>% group_by(specifics) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt))
    # topSpec <- ccSpcAll$specifics[ccSpcAll$cnt == max(ccSpcAll$cnt)]
    topSpec <- 'Primarily basic research'
    topSpecPct <- ccSpcAll$percent[ccSpcAll$cnt == max(ccSpcAll$cnt)]

    box(title = "General distribution of selected job category",
        width = 12,
        fluidRow(
          valueBox(subtitle=paste0("Proportion in Sector: '",topSect,"'"), value=tags$p(paste0(round(topSectPct,3)*100,"%"),style="font-size:75%;"), icon = icon("th-large"), color = "maroon", width=4),
          valueBox(subtitle=paste0("Proportion in Type: '",topType,"'"), value=tags$p(paste0(round(topTypePct,3)*100,"%"),style="font-size:75%;"), icon = icon("th-list"), color = "blue", width=4),
          valueBox(subtitle=paste0("Proportion in Specifics: '",topSpec,"'"), value=tags$p(paste0(round(topSpecPct,3)*100,"%"),style="font-size:75%;"), icon = icon("th"), color = "green", width=4)
        )
    )
  })
  #<<< degree
  
  
  #############################################################################
  #############################################################################
  # Download >>>
  #############################################################################
  
  output$downloadCsv <- downloadHandler(
    filename = "rawData.csv",
    content = function(file) {
      write.csv(dataGroup, file)
    },
    contentType = "text/csv"
  )
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(head(dataGroup, 5))
    options(orig)
  })
}



#### Use custom local image files as icons in a Shiny Dashboard value box 
#>>> https://gist.github.com/hrbrmstr/605e62c5bf6deadf304d80cf4b1f0239
#### R shiny dashboard how to use animated icons?
#>>> https://stackoverflow.com/questions/41454394/r-shiny-dashboard-how-to-use-animated-icons
#### Splunk shiny icons
#>>> https://www.splunk.com/blog/2015/06/25/shiny-icons.html
#### Likert plot by ggplot2
#>>> http://rnotr.com/likert/ggplot/barometer/likert-plots/
