mydf2<-function(sql,db=NULL,user=NULL,password=NULL,host=NULL) {
  if(is.null(db) | is.null(user) | is.null(password) | is.null(host)){
    if(!exists('mydfconfig')){
               cat('ERROR: mydfconfig not found\n');
               cat('See ?mydfconfig_make for details\n');
               return(1);
        }

}
    if(is.null(db)){
        db=mydfconfig$db;
    }

    if(is.null(user)){
        user=mydfconfig$user;
    }

    if(is.null(password)){
        password=mydfconfig$password;
    }

    if(is.null(host)){
        host=mydfconfig$host;
    }





  con<-RMySQL::dbConnect(MySQL(),user=user,password=password,dbname=db,host=host);
  x<-dbGetQuery(con,sql);

  y<-data.frame(x);
  dbDisconnect(con);
  return(y);
    }


mydf2config_make<-function(user,password,host,db=NULL) {
        mydfconfig<<-list(user=user,password=password,host=host,db=db);

    }
