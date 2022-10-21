variables<-variable.names(basee)

nvar <- function(x){
  vvar <- vector(mode = "logical", length = ncol(x))
  for (y in 1:ncol(x)) {
    vvar[[y]]<-typeof(x[, y])
  }
  vvar
  for (x in vvar) {
    switch (x,
      "character" = print("meh"),
      "double" = print("great")
    )
  }
}
nvar(basee)
