#' remove_background
#'
#' sweeps out the minimum of each column.
#' works on one and multiple chromatogram.
#' @param data_array mz channels in columns
#' @export
remove_background <- function(data_array){
  data_array.min <- apply(data_array,2, min)
  sweep(data_array,2, data_array.min)
}

#' remove_baseline
#'
#' removes the baseline linear from first to last point
#' along columns, works on one and multiple chromatogram
#' @param data_matrix contains signals along columns
#' @export
remove_baseline <- function(data_matrix){

  base_line <- array(apply(data_matrix, 2, function(data_matrix) approx(c(1,length(data_matrix)), data_matrix[c(1,length(data_matrix))],1:length(data_matrix),ties = "ordered")$y), dim = dim(data_matrix), dimnames = dimnames(data_matrix))
  data_matrix <- data_matrix - base_line
  data_matrix[data_matrix<0] 	<-	0
  return(data_matrix)
}


#' extract_scan_range
#'
#' function to extract a
#' particualr scan range of
#' the data array
#' @export
extract_scan_range <- function(data_array, start_scan, end_scan){

  if(end_scan > dim(data_array)[1] || start_scan >= end_scan || end_scan <= 0){
    stop('invalid scan selection')
  }

  extract <- array(data_array[start_scan:end_scan,,], c(length(start_scan:end_scan), dim(data_array)[2], dim(data_array)[3]))

  arraynames <- list()
  arraynames[[1]] <- dimnames(data_array)[[1]][start_scan:end_scan]
  arraynames[[2]] <- dimnames(data_array)[[2]]
  arraynames[[3]] <- dimnames(data_array)[[3]]
  dimnames(extract) <- arraynames
  return(extract)
}

#' calculate_tic
#'
#' function that takes a
#' data_array and calculates
#' the TIC
#' @export
calculate_tic <- function(data_array, sgolay_order = NULL, sgolay_length = 3){


  permuted <- matrix(data_array, nrow = dim(data_array)[1], ncol = dim(data_array)[2] * dim(data_array)[3])
  tic <- apply(permuted,1,sum)/dim(data_array)[3]
  if(!is.null(sgolay_order)){
    tic <- signal::sgolayfilt(tic, p = sgolay_order, n = sgolay_length )
  }
  names(tic) <- rownames(data_array)

  return (tic)
}
