sp_frame <- function(frame) {
  new_sp_frame <- structure(frame, class = c("sp_frame", class(frame)))
}

sp_unframe <- function(sp_frame) {
  new_sp_unframe <- structure(sp_frame, class = setdiff(class(sp_frame), "sp_frame"))
}