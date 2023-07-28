## ---------------------------
##
## Script name: RMSPA
##
## Purpose of script: Morphological Spatial Pattern Analysis. Analysis and description of image objects including morphometric features and connecting pathways.
##
## Author: Diego Migliavacca (https://github.com/diegomigliavacca)
##
## Date Created: 2023-07-28
##
##
## ---------------------------

if ("shiny" %in% rownames(installed.packages()) == FALSE)
  install.packages("shiny")
if ("shinyFiles" %in% rownames(installed.packages()) == FALSE)
  install.packages("shinyFiles")
if ("shinyjs" %in% rownames(installed.packages()) == FALSE)
  install.packages("shinyjs")
#if ("sf" %in% rownames(installed.packages()) == FALSE)
#install.packages("sf")
if ("terra" %in% rownames(installed.packages()) == FALSE)
  install.packages("terra")

library(fs)
library(shiny)
library(shinyFiles)
library(shinyjs)
#library(sf)
library(terra)

jscode_upload_msg <-
  "Shiny.addCustomMessageHandler('upload_msg', function(msg) {
  var target = $(\"#btnSelectFile_progress\").children()[0];
  target.innerHTML = msg;
});"

version <- "2.0"

panelStyle <- "border-style: solid;
               border-width: 1px;
               border-radius: 4px;
               border-color: #112446;
               padding-top: 7px;
               padding-bottom: 11px;"

ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  tags$head(tags$style(
    HTML(
      ".selectize-input, .selectize-dropdown, .control-label {
        font-size: 12px;
      }
      .form-group, .progress {
        margin-bottom: 10px;
      }"
    )
  )),
  
  tags$script(jscode_upload_msg),
  
  titlePanel(paste0("RMSPA - ", version),
             windowTitle = paste0("RMSPA - ", version)),
  
  wellPanel(
    style = panelStyle,
    h5(strong("Step 1: Input image - [(0),1,2] byte")),
    fileInput(
      "btnSelectFile",
      NULL,
      accept = c("image/tiff", ".tiff", ".tif"),
      buttonLabel = list(icon("folder"), " ", "Browse...")
    )
  ),
  
  wellPanel(
    style = panelStyle,
    h5(strong("Step 2: MSPA parameters")),
    fluidRow(
      column(
        tags$div(
          title = "Foreground connectivity",
          selectInput(
            inputId = "fgconn",
            label = "FGconn",
            choices = c("8", "4")
          )
        ),
        width = 3
      ),
      column(tags$div(
        title = "Effective edge width",
        selectInput(
          inputId = "edgewidth",
          label = "EdgeWidth",
          choices = c("1", "2", "3", "4", "5")
        )
      ),
      width = 3),
      column(tags$div(
        title = "Transition flag",
        selectInput(
          inputId = "transition",
          label = "Transition",
          choices = c("1", "0")
        )
      ),
      width = 3),
      column(tags$div(
        title = "Internal flag",
        selectInput(
          inputId = "intext",
          label = "Intext",
          choices = c("1", "0")
        )
      ),
      width = 3)
    )
  ),
  
  wellPanel(
    style = panelStyle,
    h5(strong("Step 3: Process the image")),
    p(
      style = "margin: 0 0 5px;",
      "Select the folder where to save the output file",
      align = "center"
    ),
    p(
      shinyDirButton(
        "selectFolder",
        " Select a folder",
        "Select the folder where to save the output file",
        icon = icon("floppy-disk"),
        style = "width:70%;"
      ),
      align = "center"
    ),
    div(style = "width:70%; margin:auto;", verbatimTextOutput("selectFolder", placeholder = TRUE)),
    p(
      actionButton("btnStart", "Start", style = "width:70%; background-color:#004494; border-color:#112446; color:white;"),
      align = "center"
    )
  ),
  
  wellPanel(style = panelStyle,
            h5(strong("Help")),
            fluidRow(column(
              a(
                "Extension guide",
                href = "MSPA-extensionR.pdf",
                target = "_blank",
                class = "btn btn-info",
                style = "width:100%; border-color:#112446"
              ),
              width = 6
            ),
            column(
              a(
                "MSPA guide",
                href = "MSPA_Guide.pdf",
                target = "_blank",
                class = "btn btn-info",
                style = "width:100%; border-color:#112446"
              ),
              width = 6
            ))),
  
  wellPanel(
    style = panelStyle,
    "Extended functionality is available in the free software",
    actionLink(inputId = "openGtb", label = "GuidosToolbox"),
    "."
  )
)

#scriptDir <- getSrcDirectory(function() {})
scriptDir <- getwd()

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1e9)
  
  shinyjs::disable("btnStart")
  
  getDialog <- function(id, icn, str)
  {
    if (id == "") {
      footer <- modalButton("Close")
    } else {
      footer <- actionButton(id, "Close")
    }
    showModal(modalDialog(div(list(
      icon(icn), " ", str
    ), style = "font-size:145%"),
    footer = footer))
  }
  
  checkVersion <- function()
  {
    df <-
      read.table(
        url(
          "https://ies-ows.jrc.ec.europa.eu/gtb/GTB/MSPA-plugin/version_mspaRplugin.txt",
          blocking = FALSE
        )
      )
    if (df > version) {
      string <-
        paste(
          "There is a new version of the extension available. Visit https://forest.jrc.ec.europa.eu/en/activities/lpa/mspa/ to get your copy of version",
          df
        )
      getDialog("", "triangle-exclamation", string)
    }
  }
  try(checkVersion())
  
  sys <- Sys.info()['sysname']
  if (sys == "Linux") {
    exec <- TRUE
    tryCatch({
      exec <-
        file_test("-x", file.path(scriptDir, "resources", "mspa_lin64"))
    }, error = function(e) {
      getDialog("", "ban", e$message)
    }, finally = {
      if (exec == FALSE) {
        getDialog(
          "closeNoExecutable",
          "ban",
          "The file 'mspa_lin64' in the 'resources' folder has no executable rights. Please use 'chmod u+x mspa_lin64' to make it executable. Exiting..."
        )
        observeEvent(input$closeNoExecutable, {
          stopApp()
        })
      }
    })
  } else if (sys == "Darwin") {
    exec <- TRUE
    tryCatch({
      exec <-
        file_test("-x", file.path(scriptDir, "resources", "mspa_mac"))
    }, error = function(e) {
      getDialog("", "ban", e$message)
    }, finally = {
      if (exec == FALSE) {
        getDialog(
          "closeNoExecutable",
          "ban",
          "The file 'mspa_mac' in the 'resources' folder has no executable rights. Please use 'chmod u+x mspa_mac' to make it executable. Exiting..."
        )
        observeEvent(input$closeNoExecutable, {
          stopApp()
        })
      }
    })
  }
  
  observe({
    req(input$btnSelectFile)
    session$sendCustomMessage("upload_msg", "")
  })
  
  doChecking <- function(filepath)
  {
    filetolower <- tolower(filepath)
    if (endsWith(filetolower, ".tiff") |
        endsWith(filetolower, ".tif")) {
      tryCatch({
        rasterfile <- rast(filepath)
        prj <<- crs(rasterfile)
        if (prj == "") {
          shinyjs::disable("btnStart")
          getDialog("",
                    "ban",
                    "The image has no projection. Please load a mspa-compliant image")
        } else {
          bandtype <<- datatype(rasterfile)
          if (max(sources(rasterfile, bands = TRUE)$bands) != 1) {
            shinyjs::disable("btnStart")
            getDialog(
              "",
              "ban",
              "The image is not a single band image. Please load a mspa-compliant image"
            )
          }
          else if (bandtype != "INT1U" & bandtype != "INT1S") {
            shinyjs::disable("btnStart")
            string <-
              paste0(
                "The band type is not Byte, it is ",
                bandtype,
                ". Please load a mspa-compliant image"
              )
            getDialog("", "ban", string)
          }
          else if (as.numeric(nrow(rasterfile)) * as.numeric(ncol(rasterfile)) > 65000000) {
            shinyjs::disable("btnStart")
            getDialog("",
                      "ban",
                      "The image is bigger than 65 MB. Please load a mspa-compliant image")
          }
          else if (nrow(rasterfile) < 50 & ncol(rasterfile) < 50) {
            shinyjs::disable("btnStart")
            getDialog(
              "",
              "ban",
              "The x and/or the y size of the image is smaller than 50 pixels. Please load a mspa-compliant image"
            )
          }
          else {
            maxval <- max(hist(rasterfile, plot = FALSE)$breaks)
            intmax <- as.integer(maxval)
            if (intmax > 2) {
              shinyjs::disable("btnStart")
              string <-
                paste0(
                  "The max value for the band in the dataset is bigger than 2 (",
                  maxval,
                  "). Please load a mspa-compliant image"
                )
              getDialog("", "ban", string)
            }
            else if (intmax == 2) {
              minval <- as.integer(min(hist(rasterfile, plot = FALSE)$breaks))
              if (minval == 0) {
                secondhistval <- hist(rasterfile, plot = FALSE)$count[10]
              } else {
                secondhistval <- hist(rasterfile, plot = FALSE)$count[1]
              }
              if (secondhistval == 0) {
                shinyjs::disable("btnStart")
                getDialog(
                  "",
                  "ban",
                  "The histogram values for the band in the dataset are not mspa-compliant. Please load a mspa-compliant image"
                )
              } else {
                shinyjs::enable("btnStart")
              }
            }
          }
        }
      }, error = function(e) {
        getDialog("",
                  "ban",
                  "Unable to process the image")
      })
    } else {
      getDialog("",
                "ban",
                "The image is not a tif file. Please load a mspa-compliant image")
    }
  }
  
  observe({
    file <- input$btnSelectFile
    req(file)
    doChecking(file$datapath)
  })
  
  volumes <-
    c(Home = fs::path_home(),
      "R Installation" = R.home(),
      getVolumes()())
  shinyDirChoose(input,
                 "selectFolder",
                 roots = volumes,
                 session = session)
  
  output$selectFolder <- renderText({
    if (is.integer(input$selectFolder)) {
      "No directory has been selected"
    } else {
      parseDirPath(volumes, input$selectFolder)
    }
  })
  
  doProcessing <-
    function(filename,
             filepath,
             outputDir,
             fgcn,
             ewdt,
             tnst,
             itxt)
    {
      resourcesDir <- paste0(scriptDir, "/resources")
      if (sys == "Linux") {
        gdalcommand <- "gdal_translate"
      } else if (sys == "Windows") {
        gdalcommand <-
          paste0(resourcesDir,
                 "/gdal_win",
                 "/gdal_translate.exe")
      } else {
        gdalcommand <-
          "/Library/Frameworks/GDAL.framework/Programs/gdal_translate"
      }
      outfile <- paste0(resourcesDir, "/out.tif")
      tryCatch({
        system2(
          gdalcommand,
          args = c(
            "-strict",
            "-co",
            '"TILED=NO"',
            "-co",
            '"PROFILE=BASELINE"',
            "-co",
            '"COMPRESS=LZW"',
            filepath,
            outfile
          ),
          stdout = TRUE,
          stderr = TRUE
        )
      }, error = function(e) {
        getDialog("closeErrorGdalCommand",
                  "ban",
                  "Unable to process the image")
        observeEvent(input$closeErrorGdalCommand, {
          stopApp()
        })
      })
      if (!file.exists(outfile)) {
        setwd(path.expand('~'))
        getDialog("", "ban", "Unable to process the image")
      } else {
        rasterfile <- rast(filepath)
        extnt <- ext(rasterfile)
        if (sys == "Linux") {
          executable <- paste0(resourcesDir, "/mspa_lin64")
        } else if (sys == "Windows") {
          executable <- paste0(resourcesDir, "/mspa_win64.exe")
        } else {
          executable <- paste0(resourcesDir, "/mspa_mac")
        }
        tryCatch({
          system2(
            executable,
            args = c(
              '-i',
              outfile,
              '-graphfg',
              fgcn,
              '-eew',
              ewdt,
              '-transition',
              tnst,
              '-internal',
              itxt,
              ' -odir ./ -o out.tif'
            ),
            stdout = TRUE,
            stderr = TRUE
          )
        }, error = function(e) {
          getDialog("closeErrorMspaExec",
                    "ban",
                    "Unable to process the image")
          observeEvent(input$closeErrorMspaExec, {
            stopApp()
          })
        }, finally = {
          file.remove(outfile)
          xmlFile <- paste0(outfile, ".aux.xml")
          if (file.exists(xmlFile)) {
            file.remove(xmlFile)
          }
        })
        if (!file.exists("out.tif")) {
          setwd(path.expand('~'))
          getDialog("", "ban", "Unable to process the image")
        } else {
          outputFile <-
            paste0(filename,
                   "_",
                   fgcn,
                   "_",
                   ewdt,
                   "_",
                   tnst,
                   "_",
                   itxt)
          outif <- paste0(outputFile, ".tif")
          imgrast <-
            paste0(scriptDir, "/resources/images/", outif)
          rst <-
            rast("out.tif")
          crs(rst) <- prj
          ext(rst) <- extnt
          writeRaster(
            rst,
            imgrast,
            overwrite = TRUE,
            datatype = bandtype,
            filetype = "GTiff",
            NAflag = NA
          )
          if (!file.exists(imgrast)) {
            getDialog("", "ban", "Unable to process the image")
          } else {
            file.remove("out.tif")
            imgout <- paste0(outputDir, "/", outif)
            tryCatch({
              system2(
                gdalcommand,
                args = c(
                  "-strict",
                  "-mo",
                  paste0('"TIFFTAG_DATETIME=', Sys.time(), '"'),
                  "-mo",
                  paste0('"TIFFTAG_DOCUMENTNAME=', outif, '"'),
                  "-mo",
                  paste0(
                    '"TIFFTAG_IMAGEDESCRIPTION=GTB_MSPA, https://forest.jrc.ec.europa.eu/en/activities/lpa/gtb/ ',
                    fgcn,
                    '_',
                    ewdt,
                    '_',
                    tnst,
                    '_',
                    itxt,
                    '"'
                  ),
                  "-co",
                  '"COMPRESS=LZW"',
                  imgrast,
                  imgout
                ),
                stdout = TRUE,
                stderr = TRUE
              )
            }, error = function(e) {
              getDialog("closeErrorGdalCommand",
                        "ban",
                        "Unable to process the image")
              observeEvent(input$closeErrorGdalCommand, {
                stopApp()
              })
            })
            setwd(path.expand('~'))
            if (!file.exists(imgout)) {
              if (sys == "Windows") {
                getDialog(
                  "",
                  "ban",
                  "The file could not be saved in the selected directory. Please check if the directory is writable or select another directory"
                )
              } else {
                getDialog(
                  "",
                  "ban",
                  "Unable to process the image. Please check if the selected directory is writable. Besides, check if GDAL is installed on your system"
                )
              }
            } else {
              string <-
                paste(
                  "GeoTiff image processed successfully!\n\nYou can find it at the following path:\n",
                  outputDir
                )
              getDialog("", "circle-info", string)
            }
            file.remove(imgrast)
            xmlFile <- paste0(imgout, ".aux.xml")
            if (file.exists(xmlFile)) {
              file.remove(xmlFile)
            }
          }
        }
      }
    }
  
  observeEvent(input$btnStart, {
    file <- input$btnSelectFile
    req(file)
    selectedFolder <- input$selectFolder
    if (is.null(file)) {
      getDialog("", "ban", "Please load a mspa-compliant image")
    } else if (is.integer(selectedFolder)) {
      getDialog("",
                "ban",
                "Please choose a directory for the output file")
    } else {
      doProcessing(
        substring(file$name, 1, nchar(file$name) - 4),
        file$datapath,
        parseDirPath(volumes, selectedFolder),
        as.numeric(input$fgconn),
        as.numeric(input$edgewidth),
        as.numeric(input$transition),
        as.numeric(input$intext)
      )
    }
  })
  
  observeEvent(input$openGtb, {
    browseURL("https://forest.jrc.ec.europa.eu/en/activities/lpa/gtb")
  })
}

shinyApp(ui = ui, server = server)