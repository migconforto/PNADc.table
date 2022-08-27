# Download PNADc -------------------------------------------------------------
#' pnadc_download
#'
#' @description download the PNADc version and create a design directly on the computer.
#'
#' @param year The year you want to download. Must be a number between 2012 and the current year. It does not accept a vector of years.
#' @param quartile The quartile of the year you want to download. Must be a number between 1 and 4. It does not accept a vector of quartiles.
#' @param path Path of the local directory where the PNAD files are or where you want to save the data. If it does not exist, a new directory will be created.
#'
#' @importFrom PNADcIBGE get_pnadc
#' @importFrom PNADcIBGE read_pnadc
#' @importFrom PNADcIBGE pnadc_design
#'
#' @export
#'
#' @examples pnadc_download(2019, 1)
pnadc_download <- function(year, quartile, path = NULL) {
  a <- webshot2::`%>%`(1,sum())
  attachNamespace("PNADcIBGE")
  if (is.null(path)) {
    if (file.exists(fs::path_home(paste("Design_PNADc", year, quartile, sep = "_"))) == T) {
      print("This edition of the PNADc has already been downloaded and can be used.")
    }
    else {
      design_PNADc <- PNADcIBGE::get_pnadc(year, quarter = quartile, design = T)

      file <- paste("Design_PNADc", year, quartile, sep = "_")
      save(design_PNADc, file = fs::path_home(file))
    }
  }
  else {
    if (file.exists(paste(path, "/Design_PNADc_", year, "_", quartile, sep = "")) == T) {
      print("This edition of the PNADc has already been downloaded and can be used.")

      save(path, file = fs::path_home(paste("path_PNADcTABLE", year, quartile, sep = "_")))
    }
    else {
      if (file.exists(paste(path, "/PNADC_0", quartile, year, ".txt", sep = "")) == T) {
        PNAD_txt <- paste(path, "/PNADC_0", quartile, year, ".txt", sep = "")
        INPUT_sas <- paste(path, "/input_PNADC_trimestral.sas", sep = "")
        pnadc <- PNADcIBGE::read_pnadc(PNAD_txt, INPUT_sas)
        design_PNADc <- PNADcIBGE::pnadc_design(pnadc)

        path <- paste(path, "/Design_PNADc_", year, "_", quartile, sep = "")
        save(design_PNADc, file = path)
        save(path, file = fs::path_home(paste("path_PNADcTABLE", year, quartile, sep = "_")))
      }
      else {
        design_PNADc <- PNADcIBGE::get_pnadc(year, quarter = quartile, design = T)

        path <- paste(path, "/Design_PNADc_", year, "_", quartile, sep = "")
        save(design_PNADc, file = path)
        save(path, file = fs::path_home(paste("path_PNADcTABLE", year, quartile, sep = "_")))
      }
    }
  }
}
