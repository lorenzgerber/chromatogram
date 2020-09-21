#' load_openchrom_csv
#'
#' function to read openchrom csv
#' exported files directly into
#' mcrarvalidator array format
#' @export
load_openchrom_csv <- function( csv_file, uvdad = FALSE, range = NULL ) {
  con <- file(csv_file,"r")
  first_line <- readLines(con,n=1)
  close(con)

  mzs <- as.numeric(strsplit(first_line, ',')[[1]][-c(1:3)])
  input_matrix <- as.matrix(read.csv(file = csv_file, skip=1))
  input_matrix <- input_matrix[,-c(1:3)]

  if ( uvdad ){
    remove <- seq(2, 201, 2)
    mzs <- mzs[-remove]
    input_matrix <- input_matrix[,-remove]
  }

  if (!is.null(range)){
    below_range <-which(mzs < range[1] )
    mzs <- mzs[-below_range]
    input_matrix[,-below_range]

    above_range <- which(mzs > range[2] )
    mzs <- mzs[-above_range]
    input_matrix[,-above_range]
  }

  dim_scans <- dim(input_matrix)[1]
  dim_mzs <- length(mzs)
  dim_samples <- 1

  data_array <- array(0, dim=c(dim_scans, dim_mzs, dim_samples), dimnames = list(seq(1,dim_scans), mzs, seq(1,dim_samples)))

  for(i in 1:dim_samples){
    for(j in 1:dim_scans){
      for(k in 1:dim_mzs){
        data_array[j, k, i] <- input_matrix[ j + ( i - 1 ) * dim_scans , k ]
      }
    }
  }
  return(data_array)

}
