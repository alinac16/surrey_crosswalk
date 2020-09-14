## Helper functions ----
# Factor value less than the minimum gets the same score as the minimum,
# factor value greater than the maximum gets the same score as the maximum.
get_scores_continuous <- function(factor, conditions) {
  string <- c("case_when( factor <", conditions[1, "start"], "~", conditions[1, "score"], ",")
  for (i in 1:nrow(conditions)) {
    string <- c(string, "factor", ">=", conditions[i, c("start")], "&",
                "factor", "<=", conditions[i, c("end")],
                "~", conditions[i, c("score")], ",")
  }
  string <- c(string, "factor >", conditions[nrow(conditions), "end"], "~", conditions[nrow(conditions), "score"], ")")
  exp <- paste0(string, collapse = " ")
  eval(parse(text = exp))
}

# Factor value not in any category gets 0
get_scores_categorical <- function(factor, conditions) {
  factor <- as.character(factor)
  string <- c("case_when(")
  for (i in 1:nrow(conditions)) {
    string <- c(string, "factor", "==", paste0('"', as.character(conditions[i, c("category")]), '"'),
                "~", conditions[i, c("score")], ",")
  }
  string <- c(string, "TRUE ~ 0)")
  exp <- paste0(string, collapse = " ")
  eval(parse(text = exp))
}

## UI and server ----
saveVariableScoresUI <- function(id) {
  ns <- NS(id)
  tagList(useSweetAlert(),
          actionButton(ns("save"), label = "Update and save", style="color: #fff; background-color: #495057;"))
}

saveVariableScoresServer <- function(id, indexDF, scoreDFs, noCrosswalkIntId) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      scores <- reactiveValues()

      observeEvent(input$save, {
        # initialize progress bar
        progressSweetAlert(
          session = session,
          id = "savingScore",
          title = "Saving scores",
          display_pct = TRUE,
          value = 0
        )
        setwd("output/") # change working directory to output folder
        # Compute PPI/PDI and save scores to output folder
        scoreDFList <- reactiveValuesToList(scoreDFs)
        varNames <- names(scoreDFList)
        newScore <- indexDF[, c("INTERSECTION_ID", varNames)]
        nVars <- length(varNames)
        for (i in seq_len(nVars)) {
          score <- scoreDFList[[i]]
          varName <- varNames[i]
          var <- indexDF[, varName]
          if (is.numeric(var)) {
            newScore[, varName] <- get_scores_continuous(var, score())
          } else {
            newScore[, varName] <- get_scores_categorical(var, score())
          }
          write.csv(score(), paste0(id, "/", varName, ".csv"), row.names = F)
          updateProgressBar(session = session,
                            id = "savingScore",
                            value = i / (nVars + 2) * 100)
        }
        # Save total score, rank, and rank percentile for all intersections
        newScore <- newScore %>%
          mutate(SCORE = rowSums(.[,-1]),
                 RANK = rank(SCORE, ties.method = "min")) %>%
          group_by(RANK) %>%
          mutate(RANK_PERCENTILE = round((RANK - 1 + 0.5 * n()) / nrow(.) * 100, digits = 2)) %>%
          ungroup()
        write.csv(newScore, paste0(id, "/", id, "_score_all_intersections.csv"), row.names = F)
        scores$all <- newScore
        updateProgressBar(session = session, id = "savingScore", value = (nVars + 1) / (nVars + 2) * 100)

        # Save intersections without crosswalk with updated rank and rank percentile
        newScoreFiltered <- newScore %>%
          filter(INTERSECTION_ID %in% noCrosswalkIntId) %>%
          mutate(RANK = rank(SCORE, ties.method = "min")) %>%
          group_by(RANK) %>%
          mutate(RANK_PERCENTILE = round((RANK - 1 + 0.5 * n()) / nrow(.) * 100, digits = 2)) %>%
          ungroup()
        scores$filtered <- newScoreFiltered
        write.csv(newScoreFiltered, paste0(id, "/", id, "_score_no_crosswalk.csv"), row.names = F)

        # compress the files in folder
        tar(paste0(id, "_scores.tgz"), files = id, compression = "gzip")
        setwd("..") # change working directory back
        updateProgressBar(session = session, id = "savingScore", value = 100)
        closeSweetAlert(session = session)

        sendSweetAlert(session = session,
                       title = "Completed!",
                       type = "success",
                       text = downloadButton(session$ns("download")),
                       btn_labels = NA,
                       showCloseButton = T,
                       closeOnClickOutside = T)
      })

      output$download <- downloadHandler(
        filename = paste0(id, "_scores.zip"),
        content = function(file) {
          file.copy(paste0("output/", id, "_scores.tgz"), file)
        },
        contentType = "application/zip"
      )

      return (scores)
    }
  )
}
