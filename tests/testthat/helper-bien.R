# Shared test helpers for BIEN integration tests

bien_connection_available <- local({
  available <- NULL
  
  function(){
    if(!is.null(available)){
      return(available)
    }
    
    available <<- !is.null(
      tryCatch(expr = BIEN:::.BIEN_sql("SELECT 1;"),
               error = function(e){NULL})
    )
    
    available
  }
})

skip_if_bien_unavailable <- function(){
  testthat::skip_if_offline()
  
  if(!bien_connection_available()){
    testthat::skip("BIEN database is unavailable or refusing connections.")
  }
}

expect_bien_class <- function(expr, class){
  result <- tryCatch(expr = eval.parent(substitute(expr)),
                     error = function(e){e})
  
  if(inherits(result, "error")){
    message <- conditionMessage(result)
    connection_error <- grepl(
      pattern = "could not connect|connection to server|remaining connection slots|connection refused|timed out|timeout|reset by peer|server closed the connection",
      x = message,
      ignore.case = TRUE
    )
    
    if(connection_error){
      testthat::skip(paste("BIEN database became unavailable during test:", message))
    }
    
    stop(result)
  }
  
  if(is.null(result)){
    testthat::skip("BIEN database became unavailable during test (NULL returned).")
  }
  
  testthat::expect_true(inherits(result, class))
  invisible(result)
}
