library(magrittr)
library(glue)
library(zeallot)

lget <- function(l, field, default=NULL){
  field <- deparse(substitute(field))
  val <- l[[field]]
  if( is.null(val) ){
    return(default)
  }else{
    return(val)
  }
}

lget_ <- function(l, field, default=NULL){
  val <- l[[field]]
  if( is.null(val) ){
    return(default)
  }else{
    return(val)
  }
}

"%¡in%" <- function(v, w){
  sapply(v, function(x){
    x %in% w
  }) ->
    inds
  return(v[inds])
} 

"%!in%" <- function(v, w){
  sapply(v, function(x){
    !(x %in% w)
  }) ->
    inds
  return(v[inds])
} 

"%is%" <- function(x, y){
  if( is.null(x) & is.null(y) ){
    return(TRUE)
  }else if( is.null(x) | is.null(y) ){
    return(FALSE)
  }else{
    x == y
  }
}



lswitch <- function(res){
  
  names_level_1 <- names(res)
  names_level_2 <- sapply(res, names) %>% as.character %>% unique
  
  res_switch <- lapply(names_level_2, function(name_2){
    lapply(res, function(x) lget_(x, name_2, NULL))
  })
  names(res_switch) <- names_level_2
  
  return(res_switch)
}

len <- length
f <- glue

check_list_equal <- function(a, b, path=NULL, are_equal=TRUE, precision=1e-6){
  if(len(a) == 0 & len(b) == 0){
    return(are_equal)
  }else if( identical(sort(names(a)), sort(names(b))) == TRUE ){
    for( name in names(a) ){
      if( class(a[[name]]) == 'list' ){
        path <- c(path, name)
        are_equal <- check_list_equal(a[[name]], b[[name]], path, are_equal)
      }else{
        if( class(a[[name]]) == 'numeric' | class(a[[name]]) == 'integer'){
          if(all(abs(a[[name]] - b[[name]]) > precision)){
            print(f('Differences in {paste(path, collapse="$")} with {name}'))
            are_equal <- FALSE
          }
        }else if(!identical(a[[name]], b[[name]])){
          print(f('Differences in {paste(path, collapse="$")} with {name}'))
          are_equal <- FALSE
        }
      }
    }
  }else{
    print(f('Differences in {paste(path, collapse="$")}'))
    print(f('with {names(a)} and {names(b)}'))
    are_equal <- FALSE
  }
  
  return(are_equal)
}