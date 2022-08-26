# PNADc faixas -----------------------------------------------------------
#' pnadc_tracks
#'
#' @description Regroups a numeric or non-numeric variable from the PNADc
#'
#' @param variable Variable of interest that will be used to calculate the data. It must be a formula, that is, have ~ in front of the variable.
#' @param filter Variable that defines the aggregation level of the variable of interest. It can contain more than one level of aggregation. It must be a formula, i.e. have ~ in front of the variable.
#' @param year The year you want to review. Must be a number between 2012 and the current year. It does not accept a vector of years.
#' @param quartile The quartile of the year you want to analyze. Must be a number between 1 and 4. It does not accept a vector of quartiles.
#' @param calculation calculation what you want to do. Must be mean, total or percentage and come between "". It does not accept a list of calculations.
#' @param group Variable used to group a set of data. It must be one of the variables listed in the "filter". Must be a formula, that is, have ~ in front of the variable.
#' @param cluster How the grouping strip will be made. In the numerical case it must be a vector calculationining the minimum, maximum and size of the range. In the categorical case it must be a list calculationining the old names and the new name that will replace them.
#' @param path Path to the directory where the Design was created through the function "pnadc_download". It only accepts logical values True or False.
#' @param export Export the table to "html", "pdf", "png" and "rtf" formats. If you want to return a data frame in R space, use "df". If it has not been filled, it returns a gt table in R space. It must be a string and be enclosed in " ".
#'
#' @importFrom gt tab_header
#' @importFrom gt md
#' @importFrom gt tab_options
#' @importFrom gt tab_source_note
#' @importFrom gt sub_missing
#' @importFrom gt gt_save_webshot
#'
#' @return gt table
#' @export
#'
#' @examples
#' #For characters
#' pnadc_tracks(~V403312, ~V2010, 2019, 1, calculation = "mean", group = ~V2010,
#' cluster = list('Negra' = c('Preta', 'Parda'), 'Outros' = c('Amarela', 'Indigena')))
#' #For numbers
#' pnadc_tracks(~V403312, ~V2009, 2019, 1, calculation = "mean", group = ~V2009, cluster = c(0, 100, 5))
#'
pnadc_tracks <- function(variable, filter, year, quartile, calculation, group, cluster, path = F, export = F) {
  design_PNADc <- NULL
  var <- as.character(variable)
  grp <- as.character(group)
  fil <- as.character(filter)
  fil <- unlist(strsplit(fil, split = " "))

  if (path == F) {
    if (file.exists(fs::path_home(paste("Design","PNADc", year, quartile, sep = "_"))) == T) {
      Design <- paste("Design","PNADc", year, quartile, sep = "_")
      load(fs::path_home(Design))

      grp_tip <- design_PNADc$variables[, grp[2]]

      if (is.null(levels(grp_tip)) == F) {
        grp_sub <- as.character(grp_tip)
        for (h in 1:length(cluster)) {
          aux <- as.data.frame(cluster[h])
          for (i in 1:nrow(aux)) {
            for (j in 1:ncol(aux)) {
              grp_sub[grp_tip == aux[i,j]] <- colnames(aux)
            }
          }
        }
        grp_sub <- as.factor(grp_sub)

        if (calculation == "mean") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(grp_sub)
            frml <- formula_1$grp_sub
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svymean, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(fs::path_home(),"/Tabela_pnadc_mean-faixa_", year, "_", quartile,".csv", sep = ""))
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_pnadc_mean-faixa_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          }

        }
        if (calculation == "total") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(grp_sub)
            frml <- formula_1$grp_sub
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(fs::path_home(),"/Tabela_pnadc_total-faixa_", year, "_", quartile,".csv", sep = ""))
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_pnadc_total-faixa_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          }

        }
        if (calculation == "percentage") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(grp_sub)
            frml <- formula_1$grp_sub
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)
          tabela <- as.data.frame(tabela)

          tabela <- as.data.frame(tabela)
          tot.geral <- as.data.frame(tot.geral)

          for (i in 1:nrow(tabela)) {
            tabela[i,(ncol(tabela) - 1)] <- (tabela[i,(ncol(tabela) - 1)]/tot.geral[1,1])
            tabela[i,ncol(tabela)] <- (tabela[i,ncol(tabela)]/tot.geral[1,2])
          }

          rm(tot.geral)

          if (length(cluster) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = cluster[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(fs::path_home(),"/Tabela_pnadc_porcentagem_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_porcentagem_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          }

        }

        gc()

      } else {

        faixa <- cut(grp_tip, c(seq(cluster[1], cluster[2] , by = cluster[3]), max(grp_tip)), include.lowest = T, right = F)

        if (calculation == "mean") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(faixa)
            frml <- formula_1$faixa
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svymean, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(fs::path_home(),"/Tabela_pnadc_mean-faixa_", year, "_", quartile,".csv", sep = ""))
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_pnadc_mean-faixa_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          }

        }
        if (calculation == "total") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(faixa)
            frml <- formula_1$faixa
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(fs::path_home(),"/Tabela_pnadc_total-faixa_", year, "_", quartile,".csv", sep = ""))
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_pnadc_total-faixa_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          }

        }
        if (calculation == "percentage") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(faixa)
            frml <- formula_1$faixa
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)
          tabela <- as.data.frame(tabela)

          tabela <- as.data.frame(tabela)
          tot.geral <- as.data.frame(tot.geral)

          for (i in 1:nrow(tabela)) {
            tabela[i,(ncol(tabela) - 1)] <- (tabela[i,(ncol(tabela) - 1)]/tot.geral[1,1])
            tabela[i,ncol(tabela)] <- (tabela[i,ncol(tabela)]/tot.geral[1,2])
          }

          rm(tot.geral)

          if (length(cluster) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = cluster[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(fs::path_home(),"/Tabela_pnadc_porcentagem_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_porcentagem_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          }
        }

        gc()

      }
    } else {
      pnadc_download(year = year, quartile = quartile)

      Design <- paste("Design","PNADc", year, quartile, sep = "_")
      load(fs::path_home(Design))

      grp_tip <- design_PNADc$variables[, grp[2]]

      if (is.null(levels(grp_tip)) == F) {
        grp_sub <- as.character(grp_tip)

        for (h in 1:length(cluster)) {
          aux <- as.data.frame(cluster[h])
          for (i in 1:nrow(aux)) {
            for (j in 1:ncol(aux)) {
              grp_sub[grp_tip == aux[i,j]] <- colnames(aux)
            }
          }
        }

        grp_sub <- as.factor(grp_sub)

        if (calculation == "mean") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(grp_sub)
            frml <- formula_1$grp_sub
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svymean, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(fs::path_home(),"/Tabela_pnadc_mean-faixa_", year, "_", quartile,".csv", sep = ""))
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_pnadc_mean-faixa_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          }

        }
        if (calculation == "total") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(grp_sub)
            frml <- formula_1$grp_sub
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(fs::path_home(),"/Tabela_pnadc_total-faixa_", year, "_", quartile,".csv", sep = ""))
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_pnadc_total-faixa_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          }

        }
        if (calculation == "percentage") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(grp_sub)
            frml <- formula_1$grp_sub
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)
          tabela <- as.data.frame(tabela)

          tabela <- as.data.frame(tabela)
          tot.geral <- as.data.frame(tot.geral)

          for (i in 1:nrow(tabela)) {
            tabela[i,(ncol(tabela) - 1)] <- (tabela[i,(ncol(tabela) - 1)]/tot.geral[1,1])
            tabela[i,ncol(tabela)] <- (tabela[i,ncol(tabela)]/tot.geral[1,2])
          }

          rm(tot.geral)

          if (length(cluster) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = cluster[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(fs::path_home(),"/Tabela_pnadc_porcentagem_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_porcentagem_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          }
        }

        gc()

      } else {

        faixa <- cut(grp_tip, c(seq(cluster[1], cluster[2] , by = cluster[3]), max(grp_tip)), include.lowest = T, right = F)

        if (calculation == "mean") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(faixa)
            frml <- formula_1$faixa
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svymean, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(fs::path_home(),"/Tabela_pnadc_mean-faixa_", year, "_", quartile,".csv", sep = ""))
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_pnadc_mean-faixa_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          }

        }
        if (calculation == "total") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(faixa)
            frml <- formula_1$faixa
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)


          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(fs::path_home(),"/Tabela_pnadc_total-faixa_", year, "_", quartile,".csv", sep = ""))
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_pnadc_total-faixa_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          }

        }
        if (calculation == "percentage") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(faixa)
            frml <- formula_1$faixa
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)
          tabela <- as.data.frame(tabela)

          tabela <- as.data.frame(tabela)
          tot.geral <- as.data.frame(tot.geral)

          for (i in 1:nrow(tabela)) {
            tabela[i,(ncol(tabela) - 1)] <- (tabela[i,(ncol(tabela) - 1)]/tot.geral[1,1])
            tabela[i,ncol(tabela)] <- (tabela[i,ncol(tabela)]/tot.geral[1,2])
          }

          rm(tot.geral)

          if (length(cluster) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = cluster[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(fs::path_home(),"/Tabela_pnadc_porcentagem-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_porcentagem-faixa_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
            print(paste("Saved in directory: ",fs::path_home(), sep = ""))
          }

        }

        gc()

      }
    }
  } else {
    path_file <- fs::path_home(paste("path_PNADcTABLE", year, quartile, sep = "_"))

    if (file.exists(path_file) == T) {
      load(path_file)
      local_file <- paste(path, "/Design_PNADc_", year, "_", quartile, sep = "")
      load(local_file)

      grp_tip <- design_PNADc$variables[, grp[2]]

      if (is.null(levels(grp_tip)) == F) {
        grp_sub <- as.character(grp_tip)

        for (h in 1:length(cluster)) {
          aux <- as.data.frame(cluster[h])
          for (i in 1:nrow(aux)) {
            for (j in 1:ncol(aux)) {
              grp_sub[grp_tip == aux[i,j]] <- colnames(aux)
            }
          }
        }

        grp_sub <- as.factor(grp_sub)

        if (calculation == "mean") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(grp_sub)
            frml <- formula_1$grp_sub
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svymean, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(path, "/Tabela_pnadc_mean-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ", path, sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_mean-faixa_", year, "_", quartile,".", export, sep = ""), path = path)
            print(paste("Saved in directory: ", path, sep = ""))
          }

        }
        if (calculation == "total") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(grp_sub)
            frml <- formula_1$grp_sub
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(path, "/Tabela_pnadc_total-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ", path, sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_total-faixa_", year, "_", quartile,".", export, sep = ""), path = path)
            print(paste("Saved in directory: ", path, sep = ""))
          }

        }
        if (calculation == "percentage") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(grp_sub)
            frml <- formula_1$grp_sub
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)
          tabela <- as.data.frame(tabela)

          tabela <- as.data.frame(tabela)
          tot.geral <- as.data.frame(tot.geral)

          for (i in 1:nrow(tabela)) {
            tabela[i,(ncol(tabela) - 1)] <- (tabela[i,(ncol(tabela) - 1)]/tot.geral[1,1])
            tabela[i,ncol(tabela)] <- (tabela[i,ncol(tabela)]/tot.geral[1,2])
          }

          rm(tot.geral)

          if (length(cluster) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = cluster[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(path, "/Tabela_pnadc_porcentagem-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ", path, sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_porcentagem-faixa_", year, "_", quartile,".", export, sep = ""), path = path)
            print(paste("Saved in directory: ", path, sep = ""))
          }


        }

        gc()

      } else {

        faixa <- cut(grp_tip, c(seq(cluster[1], cluster[2] , by = cluster[3]), max(grp_tip)), include.lowest = T, right = F)

        if (calculation == "mean") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(faixa)
            frml <- formula_1$faixa
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svymean, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(path, "/Tabela_pnadc_mean-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ", path, sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_mean-faixa_", year, "_", quartile,".", export, sep = ""), path = path)
            print(paste("Saved in directory: ", path, sep = ""))
          }

        }
        if (calculation == "total") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(faixa)
            frml <- formula_1$faixa
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(path, "/Tabela_pnadc_total-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ", path, sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_total-faixa_", year, "_", quartile,".", export, sep = ""), path = path)
            print(paste("Saved in directory: ", path, sep = ""))
          }

        }
        if (calculation == "percentage") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(faixa)
            frml <- formula_1$faixa
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)
          tabela <- as.data.frame(tabela)

          tabela <- as.data.frame(tabela)
          tot.geral <- as.data.frame(tot.geral)

          for (i in 1:nrow(tabela)) {
            tabela[i,(ncol(tabela) - 1)] <- (tabela[i,(ncol(tabela) - 1)]/tot.geral[1,1])
            tabela[i,ncol(tabela)] <- (tabela[i,ncol(tabela)]/tot.geral[1,2])
          }

          rm(tot.geral)

          if (length(cluster) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = cluster[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(path, "/Tabela_pnadc_porcentagem-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ", path, sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_porcentagem-faixa_", year, "_", quartile,".", export, sep = ""), path = path)
            print(paste("Saved in directory: ", path, sep = ""))
          }

        }

        gc()

      }
    } else {
      if (file.exists(fs::path_home(paste("Design","PNADc", year, quartile, sep = "_"))) == T) {
        Design <- paste("Design","PNADc", year, quartile, sep = "_")
        load(fs::path_home(Design))
      } else {
        pnadc_download(year = year, quartile = quartile, path = path)

        Design <- paste("/Design","PNADc", year, quartile, sep = "_")
        load(paste(path, Design, sep = ""))
      }

      grp_tip <- design_PNADc$variables[, grp[2]]

      if (is.null(levels(grp_tip)) == F) {
        grp_sub <- as.character(grp_tip)

        for (h in 1:length(cluster)) {
          aux <- as.data.frame(cluster[h])
          for (i in 1:nrow(aux)) {
            for (j in 1:ncol(aux)) {
              grp_sub[grp_tip == aux[i,j]] <- colnames(aux)
            }
          }
        }

        grp_sub <- as.factor(grp_sub)

        if (calculation == "mean") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(grp_sub)
            frml <- formula_1$grp_sub
          }


          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svymean, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)


          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(path, "/Tabela_pnadc_mean-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ", path, sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_mean-faixa_", year, "_", quartile,".", export, sep = ""), path = path)
            print(paste("Saved in directory: ", path, sep = ""))
          }

        }
        if (calculation == "total") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(grp_sub)
            frml <- formula_1$grp_sub
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(path, "/Tabela_pnadc_total-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ", path, sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_total-faixa_", year, "_", quartile,".", export, sep = ""), path = path)
            print(paste("Saved in directory: ", path, sep = ""))
          }

        }
        if (calculation == "percentage") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              grp_sub_df <- as.data.frame(grp_sub)
              formula_1 <- paste("~grp_sub_df$grp_sub+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(grp_sub)
            frml <- formula_1$grp_sub
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.all = T)
          tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)
          tabela <- as.data.frame(tabela)

          tabela <- as.data.frame(tabela)
          tot.geral <- as.data.frame(tot.geral)

          for (i in 1:nrow(tabela)) {
            tabela[i,(ncol(tabela) - 1)] <- (tabela[i,(ncol(tabela) - 1)]/tot.geral[1,1])
            tabela[i,ncol(tabela)] <- (tabela[i,ncol(tabela)]/tot.geral[1,2])
          }

          rm(tot.geral)

          if (length(cluster) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = cluster[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(path, "/Tabela_pnadc_porcentagem-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ", path, sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_porcentagem-faixa_", year, "_", quartile,".", export, sep = ""), path = path)
            print(paste("Saved in directory: ", path, sep = ""))
          }

        }

        gc()

      } else {

        faixa <- cut(grp_tip, c(seq(cluster[1], cluster[2] , by = cluster[3]), max(grp_tip)), include.lowest = T, right = F)

        if (calculation == "mean") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(faixa)
            frml <- formula_1$faixa
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svymean, na.rm = T, na.rm.all = T)
          tabela <- as.data.frame(tabela)

          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "general average")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(path, "/Tabela_pnadc_mean-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ", path, sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_mean-faixa_", year, "_", quartile,".", export, sep = ""), path = path)
            print(paste("Saved in directory: ", path, sep = ""))
          }

        }
        if (calculation == "total") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(faixa)
            frml <- formula_1$faixa
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T)
          tabela <- as.data.frame(tabela)


          if (length(fil) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = fil[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total")
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(path, "/Tabela_pnadc_total-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ", path, sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_total-faixa_", year, "_", quartile,".", export, sep = ""), path = path)
            print(paste("Saved in directory: ", path, sep = ""))
          }

        }
        if (calculation == "percentage") {
          if (length(fil) > 3) {
            if (grp[2] == fil[2]) {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[4], sep = "")

              frml <- stats::as.formula(formula_1)
            } else {
              faixa_df <- as.data.frame(faixa)
              formula_1 <- paste("~faixa_df$faixa+", fil[2], sep = "")
              frml <- stats::as.formula(formula_1)
            }
          } else {
            formula_1 <- as.data.frame(faixa)
            frml <- formula_1$faixa
          }

          tabela <- survey::svyby(formula = variable, by = frml , design = design_PNADc, FUN = survey::svytotal, na.rm = T)
          tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)
          tabela <- as.data.frame(tabela)

          tabela <- as.data.frame(tabela)
          tot.geral <- as.data.frame(tot.geral)

          for (i in 1:nrow(tabela)) {
            tabela[i,(ncol(tabela) - 1)] <- (tabela[i,(ncol(tabela) - 1)]/tot.geral[1,1])
            tabela[i,ncol(tabela)] <- (tabela[i,ncol(tabela)]/tot.geral[1,2])
          }

          rm(tot.geral)

          if (length(cluster) > 3) {
            tabela_final <- gt::gt(tabela, groupname_col = cluster[4]) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          } else {
            tabela_final <- gt::gt(tabela) %>%
              tab_header(
                title = md(paste(variable))
              ) %>%
              tab_options(
                table.align = "left",
                heading.background.color = "lightblue3",
                column_labels.background.color = "lightcyan",
                stub.background.color = "lightcyan",
                source_notes.background.color = "lightcyan"
              ) %>%
              tab_source_note(
                source_note = md(paste("Dados da PNADc ", year,".", quartile, sep = "" ))
              ) %>%
              sub_missing(
                columns = 1:ncol(tabela),
                missing_text = "Grand total") %>%
              fmt_percent(
                columns = (ncol(tabela) - 1):ncol(tabela)
              )
          }

          if (export == F) {
            return(tabela_final)
          }
          if (export == "df") {
            return(tabela)
          }
          if (export == "csv") {
            utils::write.csv2(tabela, file = paste(path, "/Tabela_pnadc_porcentagem-faixa_", year, "_", quartile,".csv", sep = ""), col.names = T)
            print(paste("Saved in directory: ", path, sep = ""))
          } else {
            gt::gtsave(tabela_final, filename = paste("Tabela_PNADcTABLE_porcentagem-faixa_", year, "_", quartile,".", export, sep = ""), path = path)
            print(paste("Saved in directory: ", path, sep = ""))
          }

        }

        gc()

      }
    }
  }
}
