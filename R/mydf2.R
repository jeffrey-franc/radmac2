mydf2<-function(sql,db=NULL,user=NULL,password=NULL,host=NULL) {




  if(is.null(db) | is.null(user) | is.null(password) | is.null(host)){
    if(!exists('mydfconfig')){
      if(file.exists('mydfconfig.Rd')){
        load('mydfconfig.Rd');
      }else{
        cat('ERROR: mydfconfig not found\n');
        cat('See ?mydfconfig_make for details\n');
        return(1);
      }

    }
  }


  if(is.null(db)) {
    db=mydfconfig$db;
  }

  if(is.null(user)) {
    user=mydfconfig$user;
  }

  if(is.null(password)) {
    password=mydfconfig$password;
  }

  if(is.null(host)) {
    host=mydfconfig$host;
  }


  con<-DBI::dbConnect(RMySQL::MySQL(),user=user,password=password,dbname=db,host=host);
  x<-DBI::dbGetQuery(con,sql);

  y<-data.frame(x);
  DBI::dbDisconnect(con);
  return(y);
}


#' Write configuration object for the mydf2 function.
#'
#' Radmac2 requires that the mydfconfig object be either available in the current R workspace as a list
#' or in the current working directory saved as 'mydfconfig.Rd'.  By default, this function creates both the
#' object in the current workspace and the saved object in the current working directory.
#'
#' @param user Default username.
#' @param password Password for default user.
#' @param host Hostname for MySQL database
#' @param db Optional default database
#' @param saveconfig If TRUE saves the data object to the current working directory
#' @return Null.  Used for its side effect of writing the mydf2config object
#' @examples
#' mydf2config_make(user='jeffrey',password='34tgh',host='localhost',db='foo')
#' mydf2config_make(user='jeffrey',password='34tgh',host='localhost',saveconfig=FALSE)
#' @export
mydf2config_make<-function(user,password,host,db=NULL,saveconfig=TRUE) {

  mydfconfig<<-list(user=user,password=password,host=host,db=db);

  if(saveconfig){
      save(mydfconfig,file='mydfconfig.Rd');
  }

}
