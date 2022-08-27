# PNADc Grafico -----------------------------------------------------------
#' pnadc_graph
#'
#' @description Creates PNADc bar or dot graphics
#'
#' @param variable Variable of interest that will be used to calculate the Total. It can be a vector of variables.
#' @param filter Variable that defines the aggregation level of the variable of interest. It can contain more than one level of aggregation.
#' @param year The year you want to review. Must be a number between 2012 and the current year. It does not accept a vector of years.
#' @param quartile The quartile of the year you want to analyze. Must be a number between 1 and 4. It does not accept an array of quartiles.
#' @param calculation calculation what you want to do. Must be mean, total or percentage and come between "". It does not accept a list of calculations.
#' @param classifier One of the variables that were used in the 'variable' or 'filter' parameter that will serve as a classifier filter in the legend and graph. It must come between "".
#' @param path Path to the directory where the Design was created through the function "pnadc_download". It only accepts logical values True or False.
#' @param export Export the chart to "pdf" and "png" formats. If it has not been filled, it returns a ggplot2 graph in R space. It must be a string and be enclosed in " ".
#' @param type Type of chart you want to generate. 1 for Bar Graph and 2 for Dot Graph. Does not accept an array of types.
#'
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 guide_axis
#'
#' @return ggplot2 graphic
#' @export
#'
#' @examples pnadc_graph(~V403312, ~UF, 2019, 1, calculation = "mean", classifier = "V403312")
pnadc_graph <- function(variable, filter, year, quartile, calculation, classifier, path = F, export = F, type = 1) {
  var <- as.character(variable)
  fil <- as.character(filter)
  fil <- unlist(strsplit(fil, split = " "))
  design_PNADc <- NULL

  if (path == F) {
    if (file.exists(fs::path_home(paste("Design","PNADc", year, quartile, sep = "_"))) == T) {
      Design <- paste("Design","PNADc", year, quartile, sep = "_")
      load(fs::path_home(Design))

      if (calculation == "mean") {
        tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svymean, na.rm = T, na.rm.by = T, na.rm.all = T)
        tabela <- as.data.frame(tabela)
        tabela <- as.data.frame(c(tabela[fil[2]], tabela[var[2]], tabela[classifier]))

        rm(design_PNADc)
        gc()

        if (type == 1) {
          graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], fill = tabela[,3])) +
            geom_col() +
            facet_grid() +
            guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))


          graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_mean_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = fs::path_home())
            print(paste("Saved in directory: ", fs::path_home(), sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
        if (type == 2) {
          if (typeof(tabela[1,3]) == "integer") {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], colour = factor(tabela[,3]))) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), col = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          } else {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], size = tabela[,3])) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), size = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          }


          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_mean_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = fs::path_home())
            print(paste("Saved in directory: ", fs::path_home(), sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
      }
      if (calculation == "total") {
        tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.by = T, na.rm.all = T)
        tabela <- as.data.frame(tabela)
        tabela <- as.data.frame(c(tabela[fil[2]], tabela[var[2]], tabela[classifier]))

        rm(design_PNADc)
        gc()

        if (type == 1) {
          graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], fill = tabela[,3])) +
            geom_col() +
            facet_grid() +
            guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))

          graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Total_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = fs::path_home())
            print(paste("Saved in directory: ", fs::path_home(), sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
        if (type == 2) {
          if (typeof(tabela[1,3]) == "integer") {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], colour = factor(tabela[,3]))) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), col = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          } else {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], size = tabela[,3])) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), size = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          }

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Total_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = fs::path_home())
            print(paste("Saved in directory: ", fs::path_home(), sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
      }
      if (calculation == "percentage") {
        tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.by = T, na.rm.all = T)
        tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)

        tabela <- as.data.frame(tabela)
        tot.geral <- as.data.frame(tot.geral)

        rm(design_PNADc)
        gc()

        for (i in 1:nrow(tabela)) {
          tabela[i,(ncol(tabela) - 1)] <- (tabela[i,(ncol(tabela) - 1)]/tot.geral[1,1])
          tabela[i,ncol(tabela)] <- (tabela[i,ncol(tabela)]/tot.geral[1,2])
        }

        rm(tot.geral)
        gc()

        tabela <- as.data.frame(c(tabela[fil[2]], tabela[var[2]], tabela[classifier]))

        if (type == 1) {
          graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], fill = tabela[,3])) +
            geom_col() +
            facet_grid() +
            guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))

          graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Porcentagem_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = fs::path_home())
            print(paste("Saved in directory: ", fs::path_home(), sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
        if (type == 2) {
          if (typeof(tabela[1,3]) == "integer") {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], colour = factor(tabela[,3]))) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), col = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          } else {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], size = tabela[,3])) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), size = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          }

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Porcentagem_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = fs::path_home())
            print(paste("Saved in directory: ", fs::path_home(), sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
      }
    } else {
      pnadc_download(year = year, quartile = quartile)

      Design <- paste("Design","PNADc", year, quartile, sep = "_")
      load(fs::path_home(Design))

      if (calculation == "mean") {
        tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svymean, na.rm = T, na.rm.by = T, na.rm.all = T)
        tabela <- as.data.frame(tabela)
        tabela <- as.data.frame(c(tabela[fil[2]], tabela[var[2]], tabela[classifier]))

        rm(design_PNADc)
        gc()

        if (type == 1) {
          graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], fill = tabela[,3])) +
            geom_col() +
            facet_grid() +
            guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))

          graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_mean_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = fs::path_home())
            print(paste("Saved in directory: ", fs::path_home(), sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
        if (type == 2) {
          if (typeof(tabela[1,3]) == "integer") {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], colour = factor(tabela[,3]))) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), col = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          } else {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], size = tabela[,3])) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), size = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          }

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_mean_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = fs::path_home())
            print(paste("Saved in directory: ", fs::path_home(), sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
      }
      if (calculation == "total") {
        tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.by = T, na.rm.all = T)
        tabela <- as.data.frame(tabela)
        tabela <- as.data.frame(c(tabela[fil[2]], tabela[var[2]], tabela[classifier]))

        rm(design_PNADc)
        gc()

        if (type == 1) {
          graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], fill = tabela[,3])) +
            geom_col() +
            facet_grid() +
            guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))

          graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Total_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = fs::path_home())
            print(paste("Saved in directory: ", fs::path_home(), sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
        if (type == 2) {
          if (typeof(tabela[1,3]) == "integer") {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], colour = factor(tabela[,3]))) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), col = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          } else {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], size = tabela[,3])) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), size = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          }

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Total_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = fs::path_home())
            print(paste("Saved in directory: ", fs::path_home(), sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
      }
      if (calculation == "percentage") {
        tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.by = T, na.rm.all = T)
        tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)

        tabela <- as.data.frame(tabela)
        tot.geral <- as.data.frame(tot.geral)

        rm(design_PNADc)
        gc()

        for (i in 1:nrow(tabela)) {
          tabela[i,(ncol(tabela) - 1)] <- (tabela[i,(ncol(tabela) - 1)]/tot.geral[1,1])
          tabela[i,ncol(tabela)] <- (tabela[i,ncol(tabela)]/tot.geral[1,2])
        }

        rm(tot.geral)
        gc()

        tabela <- as.data.frame(c(tabela[fil[2]], tabela[var[2]], tabela[classifier]))

        if (type == 1) {
          graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], fill = tabela[,3])) +
            geom_col() +
            facet_grid() +
            guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))

          graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Porcentagem_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = fs::path_home())
            print(paste("Saved in directory: ", fs::path_home(), sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
        if (type == 2) {
          if (typeof(tabela[1,3]) == "integer") {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], colour = factor(tabela[,3]))) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), col = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          } else {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], size = tabela[,3])) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), size = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          }

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Porcentagem_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = fs::path_home())
            print(paste("Saved in directory: ", fs::path_home(), sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
      }

    }
  }
  else {
    path_file <- fs::path_home(paste("path_PNADcTABLE", year, quartile, sep = "_"))

    if (file.exists(path_file) == T) {
      load(path_file)
      local_file <- paste(path, "/Design_PNADc_", year, "_", quartile, sep = "")
      load(local_file)

      if (calculation == "mean") {
        tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svymean, na.rm = T, na.rm.by = T, na.rm.all = T)
        tabela <- as.data.frame(tabela)
        tabela <- as.data.frame(c(tabela[fil[2]], tabela[var[2]], tabela[classifier]))

        rm(design_PNADc)
        gc()

        if (type == 1) {
          graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], fill = tabela[,3])) +
            geom_col() +
            facet_grid() +
            guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))

          graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_mean_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = path)
            print(paste("Saved in directory: ", path, sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
        if (type == 2) {
          if (typeof(tabela[1,3]) == "integer") {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], colour = factor(tabela[,3]))) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), col = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          } else {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], size = tabela[,3])) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), size = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          }

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_mean_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = path)
            print(paste("Saved in directory: ", path, sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
      }
      if (calculation == "total") {
        tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.by = T, na.rm.all = T)
        tabela <- as.data.frame(tabela)
        tabela <- as.data.frame(c(tabela[fil[2]], tabela[var[2]], tabela[classifier]))

        rm(design_PNADc)
        gc()

        if (type == 1) {
          graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], fill = tabela[,3])) +
            geom_col() +
            facet_grid() +
            guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))

          graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Total_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = path)
            print(paste("Saved in directory: ", path, sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
        if (type == 2) {
          if (typeof(tabela[1,3]) == "integer") {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], colour = factor(tabela[,3]))) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), col = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          } else {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], size = tabela[,3])) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), size = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          }

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Total_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = path)
            print(paste("Saved in directory: ", path, sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
      }
      if (calculation == "percentage") {
        tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.by = T, na.rm.all = T)
        tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)

        rm(design_PNADc)
        gc()

        tabela <- as.data.frame(tabela)
        tot.geral <- as.data.frame(tot.geral)

        for (i in 1:nrow(tabela)) {
          tabela[i,(ncol(tabela) - 1)] <- (tabela[i,(ncol(tabela) - 1)]/tot.geral[1,1])
          tabela[i,ncol(tabela)] <- (tabela[i,ncol(tabela)]/tot.geral[1,2])
        }

        rm(tot.geral)
        gc()

        tabela <- as.data.frame(c(tabela[fil[2]], tabela[var[2]], tabela[classifier]))

        if (type == 1) {
          graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], fill = tabela[,3])) +
            geom_col() +
            facet_grid() +
            guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))

          graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Porcentagem_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = path)
            print(paste("Saved in directory: ", path, sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
        if (type == 2) {
          if (typeof(tabela[1,3]) == "integer") {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], colour = factor(tabela[,3]))) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), col = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          } else {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], size = tabela[,3])) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), size = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          }

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Porcentagem_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = path)
            print(paste("Saved in directory: ", path, sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
      }
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

      if (calculation == "mean") {
        tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svymean, na.rm = T, na.rm.by = T, na.rm.all = T)
        tabela <- as.data.frame(tabela)
        tabela <- as.data.frame(c(tabela[fil[2]], tabela[var[2]], tabela[classifier]))

        rm(design_PNADc)
        gc()

        if (type == 1) {
          graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], fill = tabela[,3])) +
            geom_col() +
            facet_grid() +
            guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))

          graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_mean_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = path)
            print(paste("Saved in directory: ", path, sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
        if (type == 2) {
          if (typeof(tabela[1,3]) == "integer") {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], colour = factor(tabela[,3]))) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), col = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          } else {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], size = tabela[,3])) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), size = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          }

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_mean_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = path)
            print(paste("Saved in directory: ", path, sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
      }
      if (calculation == "total") {
        tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.by = T, na.rm.all = T)
        tabela <- as.data.frame(tabela)
        tabela <- as.data.frame(c(tabela[fil[2]], tabela[var[2]], tabela[classifier]))

        rm(design_PNADc)
        gc()

        if (type == 1) {
          graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], fill = tabela[,3])) +
            geom_col() +
            facet_grid() +
            guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))

          graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Total_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = path)
            print(paste("Saved in directory: ", path, sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
        if (type == 2) {
          if (typeof(tabela[1,3]) == "integer") {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], colour = factor(tabela[,3]))) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), col = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          } else {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], size = tabela[,3])) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), size = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          }

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Total_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = path)
            print(paste("Saved in directory: ", path, sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
      }
      if (calculation == "percentage") {
        tabela <- survey::svyby(formula = variable, by = filter, design = design_PNADc, FUN = survey::svytotal, na.rm = T, na.rm.by = T, na.rm.all = T)
        tot.geral <- survey::svytotal(x = variable, design = design_PNADc, na.rm = T)

        rm(design_PNADc)
        gc()

        tabela <- as.data.frame(tabela)
        tot.geral <- as.data.frame(tot.geral)

        for (i in 1:nrow(tabela)) {
          tabela[i,(ncol(tabela) - 1)] <- (tabela[i,(ncol(tabela) - 1)]/tot.geral[1,1])
          tabela[i,ncol(tabela)] <- (tabela[i,ncol(tabela)]/tot.geral[1,2])
        }

        rm(tot.geral)
        gc()

        tabela <- as.data.frame(c(tabela[fil[2]], tabela[var[2]], tabela[classifier]))

        if (type == 1) {
          graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], fill = tabela[,3])) +
            geom_col() +
            facet_grid() +
            guides(x = guide_axis(angle = 45), fill = guide_legend(title = classifier))

          graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Porcentagem_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = path)
            print(paste("Saved in directory: ", path, sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
        if (type == 2) {
          if (typeof(tabela[1,3]) == "integer") {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], colour = factor(tabela[,3]))) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), col = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          } else {
            graf_1 <- tabela %>% ggplot2::ggplot(ggplot2::aes(tabela[,1], tabela[,2], size = tabela[,3])) +
              geom_point() +
              facet_grid() +
              guides(x = guide_axis(angle = 45), size = guide_legend(title = classifier))

            graf_1 <- ggplot2::update_labels(graf_1, list(x = fil[2], y = var[2]))
          }

          if (export != F) {
            ggplot2::ggsave(filename = paste("Grafico_PNADcTABLE_Porcentagem_",year,"_",quartile,".",export, sep = ""), plot = graf_1, path = path)
            print(paste("Saved in directory: ", path, sep = ""))
            return(graf_1)

          } else {
            print("No export format was selected, the graph was generated in R environment")
            return(graf_1)
          }
          rm(list = c("tabela", "graf_1", "var", "fil"))
          gc(reset = T)
        }
      }
    }
  }
}
