library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bsplus)
library(ggplot2)
library(dplyr)

varNameLabelMap <- list(
  "PDI" = "PDI",
  "PPI" = "PPI",
  "SPEED" = "Speed",
  "LANES" = "Lanes",
  "WIDTH" = "Width",
  "DISTANCE" = "Closest crosswalk",
  "PATTERN" = "Ped crash",
  "PRED_NON_PED_AADT" = "Vehicle AADT",
  "WALKWAY_DISTANCE" = "Walkway",
  "ELEMENTARY_DISTANCE" = "Elementary school",
  "SECONDARY_DISTANCE" = "Secondary school",
  "BUS_STOP_DISTANCE" = "Transit stop",
  "PARK_DISTANCE" = "Park",
  "COMMUNITY_CENTRE_DISTANCE" = "Community centre",
  "EMPLOYMENT_DISTANCE" = "Employment",
  "COMMERCIAL_KDE" = "Commercial",
  "BUS_USAGE_KNN" = "Bus usage (KNN)",
  "BUS_USAGE_NEAREST" = "Bus usage (nearest)",
  "LOCAL_INTEREST_DISTANCE" = "Local interest"
)

hotSpotCategoryDefnMap <- list(
  "New Hot Spot" = "A location that is a statistically significant hot spot for the final time step and has never been a statistically significant hot spot before.",
  "Consecutive Hot Spot" = "A location with a single uninterrupted run of statistically significant hot spot bins in the final time-step intervals. The location has never been a statistically significant hot spot prior to the final hot spot run and less than ninety percent of all bins are statistically significant hot spots.",
  "Intensifying Hot Spot" = "A location that has been a statistically significant hot spot for ninety percent of the time-step intervals, including the final time step. In addition, the intensity of clustering of high counts in each time step is increasing overall and that increase is statistically significant.",
  "Persistent Hot Spot" = "A location that has been a statistically significant hot spot for ninety percent of the time-step intervals with no discernible trend indicating an increase or decrease in the intensity of clustering over time.",
  "Diminishing Hot Spot" = "A location that has been a statistically significant hot spot for ninety percent of the time-step intervals, including the final time step. In addition, the intensity of clustering in each time step is decreasing overall and that decrease is statistically significant.",
  "Sporadic Hot Spot" = "A location that is an on-again then off-again hot spot. Less than ninety percent of the time-step intervals have been statistically significant hot spots and none of the time-step intervals have been statistically significant cold spots.",
  "Oscillating Hot Spot" = "A statistically significant hot spot for the final time-step interval that has a history of also being a statistically significant cold spot during a prior time step. Less than ninety percent of the time-step intervals have been statistically significant hot spots.",
  "Historical Hot Spot" = "The most recent time period is not hot, but at least ninety percent of the time-step intervals have been statistically significant hot spots."
)

varDescriptionMap <- list(
  "SPEED" = "Maximum speed limit of the roads within 1m radius of the intersection",
  "LANES" = "Maximum number of lanes of the roads within 1m radius of the intersection",
  "WIDTH" = "Maximum width of the roads within 1m radius of the intersection, estimated from twice the nearest distance from road centreline to road edges and curbs",
  "DISTANCE" = "Euclidean distance to the nearest crosswalk",
  "PATTERN" = "Emerging hot spot analysis of pedestrian crash from 2010 to 2019",
  "PRED_NON_PED_AADT" = "Predicted traffic AADT in 2019",
  "WALKWAY_DISTANCE" = "Euclidean distance to the nearest walkway (including greenways/trails/paths, sidewalks, and non-motorized routes)",
  "ELEMENTARY_DISTANCE" = "Euclidean distance to the nearest elementary school",
  "SECONDARY_DISTANCE" = "Euclidean distance to the nearest secondary school",
  "BUS_STOP_DISTANCE" = "Euclidean distance to the nearest bus stop",
  "PARK_DISTANCE" = "Euclidean distance to the nearest park (including water park, park, skate park, and bike park)",
  "COMMUNITY_CENTRE_DISTANCE" = "Euclidean distance to the nearest community centre (including recreation centre, library, seniors centre, community hall/centre, and youth centre)",
  "EMPLOYMENT_DISTANCE" = "Euclidean distance to the nearest employment with more than 20 staff",
  "COMMERCIAL_KDE" = "Commercial density in a neighbourhood of 500m",
  "BUS_USAGE_KNN" = "Emerging hot spot analysis of bus stop usage using the majority category from 5 nearest bus stops",
  "BUS_USAGE_NEAREST" = "Emerging hot spot analysis of bus stop usage using the category from the nearest bus stop",
  "LOCAL_INTEREST_DISTANCE" = "Euclidean distance to the nearest local interest (including attraction and movie theatre)"
)

## Helper functions ----
renderHistogram <- function(df, colname) {
  ggplot(df, aes_string(x = colname)) +
    theme_minimal() +
    labs(x = varNameLabelMap[colname]) +
    geom_histogram(breaks = seq(floor(min(df[colname])), ceiling(max(df[colname])), length.out = 50)) +
    theme(legend.position = "none")
}

renderBar <- function(df, colname) {
  ggplot(df, aes_string(x = colname)) +
    theme_minimal() +
    labs(x = varNameLabelMap[colname]) +
    geom_bar() +
    geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
    {if (is.factor(df[[colname]])) theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) # rotates x-axis label if the column is string/factor
      else scale_x_continuous(breaks = as.numeric(levels(as.factor(df[[colname]]))))}
}

overlayScorePlot <- function(plot, scoreDF, isPoint = FALSE) {
  renderPlot({
    # overlay the plot with score function
    max_count <- max(ggplot_build(plot)$data[[1]]$count)
    max_score <- max(scoreDF$score)

    plot +
      {if (isPoint) geom_point(data = scoreDF, aes(x = knots, y = score*max_count/max_score), color = "#A81E32", size = 3) # need to * max scale value for count / max score
        else geom_step(data = scoreDF, aes(x = knots, y = score*max_count/max_score), color = "#A81E32", size = 2)} +
      scale_y_continuous(sec.axis = sec_axis(~./(max_count/max_score), name = "score"))  # need to / (max scale value for count / max score)
  })
}

scoreRowNumerical <- function(ns, rowId, start, end = NA, score = 0, min = NA, max = NA, disableEnd = T, removeId = NA) {
  minValue <- floor(min)
  maxValue <- ceiling(max)
  end <- ifelse(is.na(end), maxValue, end)
  endInput <- numericInput(paste0(ns(rowId), "_end"), label = "End value", value = end, min = minValue, max = maxValue, step = 1)
  if (disableEnd) endInput <- disabled(endInput)
  scoreInput <- numericInput(paste0(ns(rowId), "_score"), label = "Score", value = score)
  # add info tooltip to first row
  if (rowId == 1)
    scoreInput <- scoreInput %>%
    shinyInput_label_embed(icon("info-sign", lib = "glyphicon") %>%
                             bs_embed_popover(content = "Score for End will be overwritten by the next row"))

  splitLayout(id = ns(rowId),
              cellWidths = c("30%", "30%", "30%", "10%"),
              numericInput(paste0(ns(rowId), "_start"), label = "Start value", value = floor(start), min = minValue, max = maxValue, step = 1),
              endInput,
              scoreInput,
              if (!is.na(removeId)) circleButton(removeId, icon = icon("remove"), size = "xs", status = "danger")
  )
}

scoreRowCategorical <- function(levelId, levelText, score = 0) {
  categoryText <- disabled(textInput(inputId = levelId, label = "Category", value = levelText))
  if (levelText %in% names(hotSpotCategoryDefnMap))
    categoryText <- categoryText %>%
      shinyInput_label_embed(icon("info-sign", lib = "glyphicon") %>%
                               bs_embed_popover(title = paste0("Definition of ", levelText), hotSpotCategoryDefnMap[[levelText]]))

  splitLayout(id = levelText,
              cellWidths = c("50%", "10%", "30%", "10%"),
              categoryText,
              div(),
              numericInput(inputId = paste0(levelId, "_score"),
                           label = "Score",
                           value = score)
  )
}

renderVariableDescription <- function(varName){
  description <- varDescriptionMap[[varName]]
  if (varName %in% c("BUS_USAGE_NEAREST", "BUS_USAGE_KNN", "PATTERN")) {
    return (div(
      class = "var-desc no-whitespace",
      paste0(description, " ("),
      span(
        class = "pseudolink",
        onclick = "window.open('https://pro.arcgis.com/en/pro-app/tool-reference/space-time-pattern-mining/learnmoreemerging.htm')",
        "ESRI ArcGIS: How Emerging Hot Spot Analysis works"),
      ")."
    ))
  } else if (varName == "COMMERCIAL_KDE") {
    return (div(
      class = "var-desc no-whitespace",
      paste0(description, " ("),
      span(
        class = "pseudolink",
        onclick = "window.open('https://desktop.arcgis.com/en/arcmap/10.3/tools/spatial-analyst-toolbox/how-kernel-density-works.htm')",
        "ESRI ArcGIS: How Kernel Density works"),
      ")."
    ))
  }
  return (div(class = "var-desc", paste0(description, ".")))
}


# validate and sync prevous end value with current start value after 0.5s delay
syncStartEnd <- function(input, currStartId, prevEndId, prevStartId, currEndId, session) {
  currStart <- debounce(reactive(input[[currStartId]]), 500)
  prevEnd <- debounce(reactive(input[[prevEndId]]), 500)
  
  observeEvent(currStart(), {
    validValue <- min(max(currStart(), input[[prevStartId]]), input[[currEndId]], na.rm = T)
    updateNumericInput(session, prevEndId, value = validValue)
    updateNumericInput(session, currStartId, value = validValue)
  })
  
  observeEvent(prevEnd(), {
    validValue <- min(max(prevEnd(), input[[prevStartId]]), input[[currEndId]], na.rm = T)
    updateNumericInput(session, prevEndId, value = validValue)
    updateNumericInput(session, currStartId, value = validValue)
  })
}

## Inner UI and server ----
scoreNumericalUI <- function(id, min, max, defaultScore) {
  ns <- NS(id)
  variableName <- tail(strsplit(id, "-")[[1]], 1)

  fluidRow(
    box(title = varNameLabelMap[variableName], status = "success", solidHeader = T, collapsible = T, width = 12, id = id,
        renderVariableDescription(variableName),
        column(width = 8,
               plotOutput(ns("plot"))),
        column(width = 4,
               lapply(seq_len(nrow(defaultScore)), function(i) {
                 isLastRow = i == nrow(defaultScore)
                 scoreRowNumerical(ns = ns,
                                   rowId = i,
                                   start = defaultScore[i, "start"],
                                   end = defaultScore[i, "end"],
                                   score = defaultScore[i, "score"],
                                   min = min,
                                   max = max,
                                   disableEnd = isLastRow,
                                   removeId = ifelse(isLastRow, ns("remove"), NA))
               }),
               circleButton(ns("add"), icon("plus"), "sm"),
               actionButton(ns("update"), "Update")))
  )
}

scoreNumericalServer <- function(varName, df, nInitRows) {
  moduleServer(
    varName,
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns
      minVal <- min(df[varName])
      maxVal <- max(df[varName])

      # render SPEED and LANES as bar diagram but still allow the stepwise scoring function
      plotFunction <- ifelse (varName %in% c("SPEED", "LANES"), renderBar, renderHistogram)

      output$plot <- renderPlot(plotFunction(df, varName))

      # disable the start value for the first row
      disable("1_start")

      # keep track of elements inserted and not yet removed
      inserted <- seq_len(nInitRows)
      lapply(2:nInitRows, function(i) {
        syncStartEnd(input, paste0(i, "_start"), paste0(i-1, "_end"), paste0(i-1, "_start"), paste0(i, "_end"), session)
      })

      ## Handle add row
      observeEvent(input$add, {
        prevId <- tail(inserted, 1)
        newId <- prevId + 1
        currStartId <- paste0(newId, "_start")
        currEndId <- paste0(newId, "_end")
        prevStartId <- paste0(prevId, "_start")
        prevEndId <- paste0(prevId, "_end")

        # remove remove button from previous row
        removeUI(
          selector = paste0("#", ns("remove")),
        )
        # insert a new scoreRow
        insertUI(
          selector = paste0("#", ns("add")),
          where = "beforeBegin",
          ui = scoreRowNumerical(ns, newId, start = maxVal, min = minVal, max = maxVal, removeId = ns("remove"))
        )

        # enable changes to previous end value
        enable(prevEndId)
        inserted <<- c(inserted, newId)

        syncStartEnd(input, currStartId, prevEndId, prevStartId, currEndId, session)
      })

      ## Handle remove row
      observeEvent(input$remove, {
        toRemoveId <- tail(inserted, 1)
        prevEndId <- paste0(toRemoveId - 1, "_end")

        # remove the last row
        removeUI(
          selector = paste0('#', ns(inserted[toRemoveId]))
        )

        # add remove button to the previous row
        if (length(inserted) > 2) {
          insertUI(
            selector = paste0("#", ns(toRemoveId - 1), " > div:last-child"),
            where = "afterBegin",
            ui = circleButton(ns("remove"), icon = icon("remove"), size = "xs", status = "danger")
          )
        }

        # force previous end value to be the maximum
        updateNumericInput(session, prevEndId, value = ceiling(maxVal))
        disable(prevEndId)

        inserted <<- inserted[-length(inserted)]
      })

      # build scoreDF from user input values
      scoreDF <- reactive({
        start <- c()
        end <- c()
        score <- c()
        for (id in inserted) {
          start <- c(start, input[[paste0(id, "_start")]])
          end <- c(end, input[[paste0(id, "_end")]])
          score <- c(score, input[[paste0(id, "_score")]])
        }
        data.frame(start, end, score)
      })

      observeEvent(input$update, {
        intputScores <- scoreDF()
        start <- input[[paste0(1, "_start")]]
        score <- c(intputScores$score, tail(intputScores$score, n = 1))
        scoreStepDF <- data.frame(knots = c(start, intputScores$end), score = score)
        output$plot <- overlayScorePlot(plotFunction(df, varName), scoreStepDF)
      })

      return (scoreDF)
    }
  )
}


scoreCategoricalUI <- function(id, defaultScore) {
  ns <- NS(id)
  variableName <- tail(strsplit(id, "-")[[1]], 1)

  fluidRow(
    box(title = varNameLabelMap[variableName], status = "success", solidHeader = T, collapsible = T, width = 12, id = id,
        renderVariableDescription(variableName),
        column(width = 8,
               plotOutput(ns("plot"))),
        column(width = 4,
               lapply(seq_len(nrow(defaultScore)), function(i) {
                 levelText <- as.character(defaultScore[i, "category"])
                 scoreRowCategorical(ns(levelText), levelText, defaultScore[i, "score"])
               }),
               actionButton(ns("update"), "Update")))
  )
}

scoreCategoricalServer <- function(varName, df) {
  moduleServer(
    varName,
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns

      output$plot <- renderPlot({
        renderBar(df, varName)
      })

      # build scoreDF from user input values
      scoreDF <- reactive({
        levels <- levels(df[[varName]])
        score <- sapply(levels, function(levelText) input[[paste0(levelText, "_score")]])
        data.frame(category = as.character(levels), score = score)
      })

      observeEvent(input$update, {
        inputScores <- scoreDF()
        names(inputScores)[names(inputScores) == "category"] <- "knots"
        output$plot <- overlayScorePlot(renderBar(df, varName), inputScores, isPoint = T)
      })

      return (scoreDF)
    }
  )
}

## Module UI and server ----
variableScoreUI <- function(id, indexDF) {
  ns <- NS(id)
  indexDFVars <- indexDF[ , (names(indexDF) != "INTERSECTION_ID")]

  fluidPage(
    useShinyjs(),
    use_bs_popover(),
    lapply(colnames(indexDFVars), function(var) {
      # one module for each variable
      defaultScore <- read.csv(paste0("default/", id, "/", var, ".csv"))
      column <- indexDFVars[[var]]
      if (is.numeric(column)) {
        a(name = var, scoreNumericalUI(
          id = ns(var),
          min = min(column),
          max = max(column),
          defaultScore
        ))
      } else {
        a(name = var, scoreCategoricalUI(id = ns(var), defaultScore))
      }
    })
  )
}

variableScoreServer <- function(id, indexDF) {
  moduleServer(
    id,
    function(input, output, session) {
      indexDFVars <- indexDF[ , (names(indexDF) != "INTERSECTION_ID")]
      scoreDFs <- reactiveValues()
      lapply(colnames(indexDFVars), function(var) {
        # one module for each variable
        defaultScore <- read.csv(paste0("default/", id, "/", var, ".csv"))
        if (is.numeric(indexDFVars[, var])) {
          scoreDFs[[var]] <- scoreNumericalServer(var, indexDFVars, nrow(defaultScore))
        } else {
          scoreDFs[[var]] <- scoreCategoricalServer(var, indexDFVars)
        }
      })

      return (scoreDFs)
    }
  )
}
