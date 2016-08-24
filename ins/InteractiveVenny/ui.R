library(shiny)
library(shinysky)
library(colourpicker)

#difined a text area input
inputTextarea <- function(inputId, value="", nrows, ncols) {
  tagList(
    singleton(tags$head(tags$script(src = "textarea.js"))),
    tags$textarea(id = inputId,
                  class = "inputtextarea",
                  rows = nrows,
                  cols = ncols,
                  as.character(value))
  )
}

shinyUI(pageWithSidebar(
     headerPanel("Venn"),
     
     sidebarPanel(
       conditionalPanel(condition="input.conditionedPanels == 'About'",
                        titlePanel("Introduction"),
                        helpText("右图是从N(0,1)中随机抽取的10个n=100,的样本所估计的100个")
                        ),
       
       conditionalPanel(condition="input.conditionedPanels == 'InputData'",        
           radioButtons("dataset", h3("Dataset"), c(Example = "example", Upload = "upload",Input="Inputlist"),selected = 'example'),
    
           conditionalPanel(
             condition = "input.dataset == 'upload'",
             div(class="container span"
                 ,shinyalert("shinyalert3")
             ),
             fileInput('file1', 'Choose CSV/text File',
                     accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
             checkboxInput('header', 'Header', FALSE),
             radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     '\t'),
             radioButtons('quote', 'Quote',
                       c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                       '')
           ), 
           actionButton("goButton3", "Submit",styleclass="primary"),
           p("This application was created by ",strong("Qi Zhao")," from Ren's lab in SYSU."),
           img(src="CUCKOO.jpg", height = "200", width = "200")
      ),
      #Plot condition panel
      conditionalPanel(condition="input.conditionedPanels == 'VenDiagram'",
         h4('Color Option'), 
            wellPanel(
              conditionalPanel(condition="input.dataset != 'Inputlist'",
                select2Input("select2Input1",strong("Fill Color"),
                             choices=c("black","red","green","blue","yellow","orange","purple","brown","grey","cyan","tomato"),
                             selected=c("red","green","blue","yellow","grey")),
                shinyalert("shinyalert4")
              ),
              select2Input("select2Input2",strong("Text Color"),
                           choices=c("black","red","green","blue","yellow","orange","purple","brown","grey","cyan","tomato"),
                           selected=c("red","green","blue","yellow","grey")),
              shinyalert("shinyalert5"),
              actionButton("goButton2", "Submit",styleclass="success"),
              sliderInput("fontsize",
                          h5("Font size of numer text:"),
                          min = 1,
                          max = 2,
                          value = 1.5,step=0.1),
              sliderInput("namedist",
                          h5("Distance (in npc units) of each category name from the edge of the circle"),
                          min = 0,
                          max = 1,
                          value = 0.23,step=0.01)
          ),
         #display text option when adopted manual input option
         conditionalPanel(condition="input.dataset != 'Inputlist'",
          h4('Text Option'), 
              wellPanel(
                textInput('samplenametxt', "Enter group names displayed in Venplot (seperated by comma)","Sample1,Sample2,Sample3,Sample4,Sample5"),
                shinyalert("shinyalerttext"),
                actionButton("goButton", "Submit",styleclass="success")
              )
        ), 
        selectInput('bordercolinput', strong('Specity border Color'),c("transparent","black","red","green","blue","yellow","orange","purple","brown","grey","cyan","tomato")),
        h4("Fecth plot"),
          wellPanel(   
            downloadButton('downloadDataPNG', 'Download PNG-file'),
            downloadButton('downloadDataPDF', 'Download PDF-file'),
            downloadButton('downloadDataEPS', 'Download EPS-file')
          )
      ),
      #Result Condition panel
      conditionalPanel(condition="input.conditionedPanels == 'AnalysisResult'",
        #gsub("label class=\"radio\"", "label class=\"radio inline\"",radioButtons("overlapnumber", h6("Get over lap of your data list"), c(twoList = 2, threeList = 3,fourList=4,fiveList = 5),selected = 5)),
        uiOutput("analysisUI"),
        shinyalert("shinyalert7"),
        h4("Fecth list"),
        wellPanel(   
          downloadButton('downloadDataCSV', 'Download CSV'),
          downloadButton('downloadDataTXT', 'Download TXT')
        )
      )
      ),
     mainPanel(
       tabsetPanel(
         tabPanel("About",
                  p(tags$a(href="http://en.wikipedia.org/wiki/Venn_diagram", "Venn diagram"),
                    "is known as a graph that employs closed curves and especially circles to represent logical relations between and operations on sets and the terms of propositions by the inclusion, exclusion, or intersection of the curves. It is commonly featured by overlapping circles representing a finite collection of sets. With great understandability and coverage of all possible relations of sets, Venn diagram is an efficient diagram for demonstrating set relations, which can be applied in many aspects of science work. ",
                    br(),
                    br(),
                    "Inspired by a well-designed R package (",
                    tags$a(href="http://cran.r-project.org/web/packages/VennDiagram/index.html ", "VennDiagram:1.6.0"),
                    "), we developed this web application to help users plot a publishable venn picture without any R cmd operation. In the diagram provided, each circle/ellipse is characterized by certain color for better recognition and each region is tagged by a number indicating the sum of elements in a specific relation of sets.",
                    br(),
                    h4("Reference"),
                    "R Development Core Team. ",
                    tags$a(href="http://www.r-project.org ", "R"),
                    ": A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, Vienna (2014)",
                    br(),
                    "RStudio and Inc. ",
                    tags$a(href="http://www.rstudio.com/shiny/", "shiny"),
                    ": Web Application Framework for R. R package version 0.9.1 (2014)",
                    br(),
                    "Hanbo Chen. ",
                    tags$a(href="http://cran.r-project.org/web/packages/VennDiagram/index.html", "VennDiagram"),
                    ": Generate high-resolution Venn and Euler plots. R package version 1.6.0 (2013)",
                    br(),
                    '"Venn Diagram." Merriam-Webster.com. Merriam-Webster, n.d. Web. 16 May 2014. ',
                    br(),
                    br(),
                    strong("This application was created by "),
                    tags$a(href="mailto:zhaoqi3@mail2.sysu.edu.cn",strong("Qi Zhao")),
                    strong(', and tested by '),
                    tags$a(href="mailto:diaorucheng@SuiBianChuo",strong("Rucheng Diao")),
                    strong(' from '),
                    tags$a(href="http://gps.biocuckoo.org/",strong("Ren Lab")),
                    strong(" in "),
                    tags$a(href="http://www.sysu.edu.cn/2012/en/index.htm",strong("SYSU")),
                    strong('. Please let us know if you find bugs or have new feature request.This application uses the'),
                    tags$a(href="http://www.rstudio.com/shiny/",strong("shiny package from RStudio."))
                    
                    
                  )
                    
                    
                    
                    
                    ),
         tabPanel("InputData",
                  h4("Input List for Ven"),
                  conditionalPanel(
                    gsub("label class=\"radio\"", "label class=\"radio inline\"",radioButtons("listnumber", h6("How many lists you want to plot"), c(twoList = 2, threeList = 3,fourList=4,fiveList = 5),selected = 3)),
                    condition = "input.dataset == 'Inputlist'",
                    #list1
                    div(class = "well container-fluid",
                      div(class="row-fluid",
                        div(class="span6",
                            h6("Input List1"),
                            div(class="row-fluid" , 
                                div(class="container span5",
                                  inputTextarea('samplelist1','a\nb\nc\nd\ne',2,50),
                                  shinyalert("shinyalert6")
                                  ),
                                      div(class="container span3",textInput("Samplename1",strong("List1"),"Sample1")),
                                      div(class="container span3",colourInput("jscolorInput1","Choose colour","red"))
                            )
                        ) 
                      )
                    ),
                    #list2
                    div(class = "well container-fluid",
                        div(class="row-fluid",
                            div(class="span6",
                                h6("Input List2"),
                                div(class="row-fluid" , 
                                    div(class="container span5",
                                        inputTextarea('samplelist2','a\nb\nc\nd\ne\nf',2,50)
                                    ),
                                    div(class="container span3",textInput("Samplename2",strong("List2"),"Sample2")),
                                    div(class="container span3",colourInput("jscolorInput2","Choose colour","blue"))
                                )
                            ) 
                        )
                    ),
                    #list3
                    conditionalPanel(
                      condition = "input.listnumber >= 3",
                      div(class = "well container-fluid",
                          div(class="row-fluid",
                              div(class="span6",
                                  h6("Input List3"),
                                  div(class="row-fluid" , 
                                      div(class="container span5",
                                          inputTextarea('samplelist3','b\nc\nd\ne\nf',2,50)
                                      ),
                                      div(class="container span3",textInput("Samplename3",strong("List3"),"Sample3")),
                                      div(class="container span3",colourInput("jscolorInput3","Choose colour","green"))
                                  )
                              ) 
                          )
                      )
                    ),
                    #list4
                    conditionalPanel(
                      condition = "input.listnumber >= 4",
                      div(class = "well container-fluid",
                          div(class="row-fluid",
                              div(class="span6",
                                  h6("Input List4"),
                                  div(class="row-fluid" , 
                                      div(class="container span5",
                                          inputTextarea('samplelist4','a\nb\nc',2,50)
                                      ),
                                      div(class="container span3",textInput("Samplename4",strong("List4"),"Sample4")),
                                      div(class="container span3",colourInput("jscolorInput4","Choose colour","yellow"))
                                  )
                              ) 
                          )
                      )
                    ),
                    #list5
                    conditionalPanel(
                      condition = "input.listnumber == 5",
                      div(class = "well container-fluid",
                          div(class="row-fluid",
                              div(class="span6",
                                  h6("Input List5"),
                                  div(class="row-fluid" , 
                                      div(class="container span5",
                                          inputTextarea('samplelist5','a\nb\nc\nde\nf',2,50)
                                      ),
                                      div(class="container span3",textInput("Samplename5",strong("List5"),"Sample5")),
                                      div(class="container span3",colourInput("jscolorInput5","Choose colour","brown"))
                                  )
                              ) 
                          )
                      )
                      
                    )
                  ),  
                  conditionalPanel(
                    condition = "input.dataset != 'Inputlist'",
                    tableOutput("summary")
                  ),
                  verbatimTextOutput("dim")
          ), 
         tabPanel("VenDiagram", plotOutput("VenPlot",height= "auto",width="auto")),
         tabPanel("AnalysisResult", h4("Summary"),textOutput("text1"),h4("Overlap List"),tableOutput("overlapout")),
         tabPanel("Contributors"),
         tabPanel("Status",tags$script(type="text/javascript",src="//rh.revolvermaps.com/0/0/6.js?i=72ugies35jb&amp;m=0&amp;s=341&amp;c=ff0000&amp;cr1=54ff00&amp;f=arial&amp;l=0",async="async")
         ),
         id = "conditionedPanels" 
       )
     )
  ))
