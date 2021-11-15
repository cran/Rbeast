.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  requireNamespace("utils",quitely=TRUE)
  if (interactive())
  {
    v = utils::packageVersion("Rbeast")
    packageStartupMessage("Rbeast v", 
	v, 
	". For help or bug reporting, contact Kai Zhao at zhao.1423@osu.edu. ",
	"Major functions available:\n",
	"  ?beast       - for a single regular time series (ts)\n",
	"  ?beast.irreg - for a single irregular time series\n",
	"  ?beast123    - for one, multiple, or 3D array of regular/irregular ts\n",
	"  ?minesweeper - a poor man\'s implementation of the minesweeper game\n"
	)
  }
   
   
}

.onLoad <- function(libname, pkgname) {
   #library.dynam("beast", pkgname, libname )
   utils::data(simdata, package=pkgname,         envir=parent.env(environment())) 
   #utils::data(modis_ohio, package=pkgname,      envir=parent.env(environment())) 
   #utils::data(simAnnualData01, package=pkgname,    envir=parent.env(environment())) 
}

.onUnload <- function(libpath) {
  library.dynam.unload("Rbeast", libpath)
}

 