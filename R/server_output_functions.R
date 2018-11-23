

# Server: OUTPUT-Funktionen --------------------------------------------------------



#' recursive indicating wether element or one of parents is false
#'
#'
#' @param names vector of names (character or integer)
#' @param states boolean vector
#' @param parents vector of parents (character or integer), only first one is used, if several parents
#'
#' @return named vector, ordered by data$names, of logical indicating if one of the parents`state or state itself is false
#' @export
#'
#' @examples
#'
recursiveTrue <- function(names, states, parents){
  data<- data.table(names=names,
                    states=states,
                    parents=parents)

  rTrue<- function(x,data){

    #only first parent
    parent<- data[names==x, first(parents)]
    print(paste0("parent=",parent))

    parentTrue<- ifelse(nrow(data[names==parent])==1,
                        rTrue(parent, data),
                        TRUE)
    print(paste0("parentTrue=",parentTrue))

    return (data[names==x, states] & parentTrue)
  }

  sapply(data$names,function(x) rTrue(x,data))


}
