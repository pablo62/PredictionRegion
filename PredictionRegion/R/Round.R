#' Rounds differently than 0.5 cutoff
#'
#' Rounds a list of numbers between 0 & 1 based on specified cutoff
#' @param x A list of numbers between 0 and 1 to be rounded
#' @param r.val Cutoff value to round up
#' @return a vector of rounded values
#' @export

round.func = function(x, r.val){
  unlist(lapply(x, function(i){
    if(i >= r.val){
      1
    } else{
      0
    }
  }))
}
