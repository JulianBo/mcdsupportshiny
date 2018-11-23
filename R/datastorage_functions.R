
# Data-Storage-Funktionen -------------------------------------------------
#' Testing if googlesheet exists in open Googlesheet-location.
#'
#' @param place Sheet to be tested for existence.
#'
#' @return True if sheet exists, False otherwise.
#'
#' @note Datastorage must have been initialised before using
#'       \code{\link{initialize_datastorage()}}.
#'
#' @export
#'
#' @examples
#'
gs_sheetexists<-function(place){
  sheetlist <-gs_ls()
  return( sheet  %in% sheetlist$sheet_title)
}

#TODO: Es ist möglich, dass Ergebnis mehrere Werte zurückliefert Ergebnis1,Ergebnis2, etc.
#Im moment funktioniert dies bei CSV beim Speichern, aber nicht beim Laden: Ungleiche Spaltenanzahl.

#' Initializes external data storage.
#'
#' @param data
#' @param method
#' @param place
#'
#' @return
#' @export
#'
#' @examples
initialize_datastorage <-function(data, method, place){
  switch(method,
         CSV={
           #See https://stackoverflow.com/questions/4216753/check-existence-of-directory-and-create-if-doesnt-exist
           ifelse(!dir.exists(place), dir.create(place), FALSE)
         },
         GoogleSheets= {
           if(gs_sheetexists(place)){
             print("sheet exists")
             return(TRUE)
           }else{
             sheet <- gs_new(title=place, ws_title="Daten")
             gs_edit_cells(sheet,ws = "Daten",
                           input= data,
                           #input= names(data),
                           byrow=TRUE )
             return(FALSE)
           }

         } ,{
           stop(paste0("method -",method,"- not supported" ) )
         }

  )
}

#' Saves Data to place
#'
#' @param data
#' @param method
#' @param place
#'
#' @return
#' @export
#'
#' @examples
saveData <- function(data, method, place) {
  switch(method,
         CSV={
           # Create a unique file name
           fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
           # Write the file to the local system
           write.csv(
             x = data,
             file = file.path(place, fileName),
             row.names = FALSE, quote = TRUE
           )
         },
         GoogleSheets= {
           # Grab the Google Sheet
           sheet <- gs_title(place)
           # Add the data as a new row
           gs_add_row(sheet, input = data, ws ="Daten")

         } ,{
           stop(paste0("method -",method,"- not supported" ) )
         }
  )

}

#' Loads Data
#'
#' @param method
#' @param place
#'
#' @return
#' @export
#'
#' @examples
loadData <- function(method, place) {
  switch(method,
         CSV={
           # Read all the files into a list
           files <- list.files(place, full.names = TRUE)
           data <- lapply(files, read.csv, stringsAsFactors = FALSE)
           # Concatenate all data together into one data.frame
           data <- do.call(rbind, data)
           data
         },
         GoogleSheets= {
           # Grab the Google Sheet
           sheet <- gs_title(place)
           # Read the data
           gs_read_csv(sheet, ws ="Daten")

         } ,{
           stop(paste0("method -",method,"- not supported" ) )
         }
  )

}


##Test
# testdata.table=data.table(a="sad", b=c(1,2,3,44), test="google")
# initialize_datastorage(testdata.table,method="GoogleSheets", "testtabelle")
# saveData(testdata.table,method="GoogleSheets", "testtabelle")
# loadData(method="GoogleSheets", "testtabelle")

# initialize_datastorage(testdata.table,method="CSV", "testtabelle")
# saveData(testdata.table,method="CSV", "testtabelle")
# loadData(method="CSV", "testtabelle")

