# Tabela Total ------------------------------------------------------------
#' pnadc_total
#'
#' @description Create PNADc total tables
#'
#' @param variable Variable of interest that will be used to calculate the Total. It can be a vector of variables.
#' @param filter Variable that defines the aggregation level of the variable of interest. It can contain more than one level of aggregation.
#' @param year The year you want to review. Must be a number between 2012 and the current year. It does not accept a vector of years.
#' @param quartile The quartile of the year you want to analyze. Must be a number between 1 and 4. It does not accept a vector of quartiles.
#' @param path Path to the directory where the Design was created through the low_pnadc function. It only accepts logical values True or False.
#' @param export Export the table to "html", "pdf" and "png" formats. If you want to return a data frame in R space, use "df". If it has not been filled, it returns a gt table in R space. It must be a string and be enclosed in " ".
#'
#' @importFrom gt tab_header
#' @importFrom gt md
#' @importFrom gt tab_options
#' @importFrom gt tab_source_note
#' @importFrom gt sub_missing
#'
#' @return gt table.
#' @export
#'
#' @examples pnadc_total(~V403312, ~UF+V2010, 2019, 1)
pnadc_total <- function(variable, filter, year, quartile, path = F, export = F) {
  design_PNADc <- NULL
  if (path == F) {
    if (file.exists(fs::path_home(paste("Design","PNADc", year, quartile, sep = "_"))) == T) {
      Design <- paste("Design","PNADc", year, quartile, sep = "_")
      load(fs::path_home(Design))

      tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.by = T, na.rm.all = T)
      tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)
      tabela <- as.data.frame(tabela)
      tabela <- rbind(tabela, tot.geral)

      rm(list = c("tot.geral", "design_PNADc"))
      gc()

      grupo <- as.character(filter)
      grupo <- unlist(strsplit(grupo, split = " "))

      if (length(grupo) > 3) {
        tabela_final <- gt::gt(tabela, groupname_col = grupo[4]) %>%
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
        utils::write.csv2(tabela_final, file = paste(fs::path_home(),"/Tabela_pnadc_total_", year, "_", quartile,".csv", sep = ""))
        print(paste("Saved in directory: ",fs::path_home(), sep = ""))
      } else {
        gt::gtsave(tabela_final, filename = paste("Tabela_pnadc_total_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
        print(paste("Saved in directory: ",fs::path_home(), sep = ""))
      }

      rm(list = c("tabela", "tabela_final", "grupo"))
      gc(reset = T)
    }
    else {
      pnadc_download(year = year, quartile = quartile)

      Design <- paste("Design","PNADc", year, quartile, sep = "_")
      load(fs::path_home(Design))

      tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.by = T, na.rm.all = T)
      tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)
      tabela <- as.data.frame(tabela)
      tabela <- rbind(tabela, tot.geral)

      rm(list = c("tot.geral", "design_PNADc"))
      gc()

      grupo <- as.character(filter)
      grupo <- unlist(strsplit(grupo, split = " "))

      if (length(grupo) > 3) {
        tabela_final <- gt::gt(tabela, groupname_col = grupo[4]) %>%
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
        utils::write.csv2(tabela_final, file = paste(fs::path_home(),"/Tabela_pnadc_total_", year, "_", quartile,".csv", sep = ""))
        print(paste("Saved in directory: ",fs::path_home(), sep = ""))
      } else {
        gt::gtsave(tabela_final, filename = paste("Tabela_pnadc_total_", year, "_", quartile,".", export, sep = ""), path = fs::path_home())
        print(paste("Saved in directory: ",fs::path_home(), sep = ""))
      }
      rm(list = c("tabela", "tabela_final", "grupo"))
      gc(reset = T)
    }
  }
  else {
    path_file <- fs::path_home(paste("path_PNADcTABLE", year, quartile, sep = "_"))

    if (file.exists(path_file) == T) {
      load(path_file)
      local_file <- paste(path, "/design_PNADc_", year, "_", quartile, sep = "")
      load(local_file)

      tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.by = T, na.rm.all = T)
      tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)
      tabela <- as.data.frame(tabela)
      tabela <- rbind(tabela, tot.geral)

      rm(list = c("tot.geral", "design_PNADc"))
      gc()

      grupo <- as.character(filter)
      grupo <- unlist(strsplit(grupo, split = " "))

      if (length(grupo) > 3) {
        tabela_final <- gt::gt(tabela, groupname_col = grupo[4]) %>%
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
        utils::write.csv2(tabela_final, file = paste(path, "/Tabela_pnadc_total_", year, "_", quartile,".csv", sep = ""))
        print(paste("Saved in directory: ", path, sep = ""))
      } else {
        gt::gtsave(tabela_final, filename = paste("Tabela_pnadc_total_", year, "_", quartile,".", export, sep = ""), path = path)
        print(paste("Saved in directory: ", path, sep = ""))
      }

      rm(list = c("tabela", "tabela_final", "grupo"))
      gc(reset = T)
    }
    else {
      if (file.exists(fs::path_home(paste("Design","PNADc", year, quartile, sep = "_"))) == T) {
        Design <- paste("Design","PNADc", year, quartile, sep = "_")
        load(fs::path_home(Design))
      } else {
        pnadc_download(year = year, quartile = quartile, path = path)

        Design <- paste("/Design","PNADc", year, quartile, sep = "_")
        load(paste(path, Design, sep = ""))
      }

      tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.by = T, na.rm.all = T)
      tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)
      tabela <- as.data.frame(tabela)
      tabela <- rbind(tabela, tot.geral)

      rm(list = c("tot.geral", "design_PNADc"))
      gc()

      grupo <- as.character(filter)
      grupo <- unlist(strsplit(grupo, split = " "))

      if (length(grupo) > 3) {
        tabela_final <- gt::gt(tabela, groupname_col = grupo[4]) %>%
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
        utils::write.csv2(tabela_final, file = paste(path, "/Tabela_pnadc_total_", year, "_", quartile,".csv", sep = ""))
        print(paste("Saved in directory: ", path, sep = ""))
      } else {
        gt::gtsave(tabela_final, filename = paste("Tabela_pnadc_total_", year, "_", quartile,".", export, sep = ""), path = path)
        print(paste("Saved in directory: ", path, sep = ""))
      }

      rm(list = c("tabela", "tabela_final", "grupo"))
      gc(reset = T)
    }
  }
}
