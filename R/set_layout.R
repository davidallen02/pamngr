#' Set the layout for a page of the Economic & Market Commentary
#'
#' @param x an integer corresponding to the desired layout
#'
#' @return a matrix
#'
set_layout <- function(x){

  path <- if(Sys.info()["nodename"] == "BBDA"){
    "C:/Users/David/PAM Research Dropbox/David/projects/tamarac/economic-market-commentary/inputs/layouts.xlsx"
  } else{
    "~/onedrive/pamgmt/projects/tamarac/economic-market-commentary/layouts.xlsx"
  }

  layout_sheet = paste0("lay-", x)

  layout <- readxl::read_excel(path,
                               sheet     = layout_sheet,
                               range     = "A1:AN28",
                               col_names = FALSE) %>%
    as.matrix()

  return(layout)
}
