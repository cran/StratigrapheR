#' @title Remembers and outputs the result of a slow function
#'
#' @description Memento mori; you do not have time to lose on unnecessary
#' calculations. This function remembers the output of a slow function, for
#' given arguments and, if asked politely, given files and a given random seed.
#' If they match the previous arguments, files and seeds, the output is provided
#' without delay, otherwise the function runs, and all the parameters are saved
#' for next time. The trade-off is to assign a folder to store the data (see
#' also details). The function can also be forced to rerun.
#'
#' @param what a (slow) function
#' @param args a list of the the arguments to give to the function. If they
#' differ from saved values, the function will run again.
#' @param name the name of the folder where to store the info. THIS NEEDS TO BE
#' DIFFERENT FOR EACH IMPLEMENTATION OF THE FUNCTION IN IDENTICAL DIRECTORIES.
#' @param dir the directory. You can set it as the working directory via
#' \code{\link{getwd}}.
#' @param subdir a name for a subdirectory (useful when the function is used
#' several time in a script)
#' @param rerun if TRUE, the function is rerun no matter what. This is useful
#' to update information that is not present in the R environment, for instance
#' if you load data from external files that have been updated.
#' @param check.files a list of files to check changes in (see details). If the
#' list is of length 0, no file is checked.
#' @param files.dir directory for teh files to be checked.
#' @param check.seed if TRUE, the value of the random seed in effect will be
#' taken into account; if it changes, the function will run again.
#' @param speak whether to signify when the (slow) function is running
#'
#' @details file data is summarized using MD5sum, which can have limitations in
#' data size (2^64 bits) and in cryptographic purposes.
#'
#' @return the output of the function
#'
#' @examples
#' \donttest{tf <- tempdir()
#' if(exists("run.number")) run.number <- run.number + 1 else run.number <-  1
#' name <- paste("T",run.number)
#'
#' testfun <- function(a = 1, time = 3){
#'
#'   Sys.sleep(time)
#'
#'   return(a  + 0.1 * abs(rnorm(1)))
#'
#' }
#'
#' # First time running; the function takes some time, memento needs the
#' # output to be generated, and will remember for later.
#' set.seed(43)
#' memento(testfun,  args = list(a = 7), name = name, dir = tf)
#'
#' set.seed(43)
#' testfun(7, time = 0)
#'
#' # Second time running: memento directly outputs the remembered results.
#' # In this case, the seed is ignored, so the result is different from what
#' # would be obtained with a different seed
#' set.seed(45)
#' memento(testfun,  args = list(a = 7), name = name, dir = tf)
#'
#' set.seed(45)
#' testfun(7, time = 0)
#'
#' # First time running while taking into account the random seed;
#' # the function takes some time to generate the result
#' set.seed(42)
#' memento(testfun,  args = list(a = 7), name = name, dir = tf, check.seed = TRUE)
#'
#' # Second time running with an identical random seed;
#' # memento directly outputs the results
#' set.seed(42)
#' memento(testfun,  args = list(a = 7), name = name, dir = tf, check.seed = TRUE)
#'
#' # The seed is changed: the result is computed anew
#' set.seed(47)
#' memento(testfun,  args = list(a = 7), name = name, dir = tf, check.seed = TRUE)
#' }
#' @importFrom tools md5sum
#' @export

memento <- function(what, args, name, dir = getwd(), subdir = "memento",
                    rerun = F, check.files = list(), files.dir = getwd(),
                    check.seed = F, speak = T)
{

  # Conditions ----

  if(!inherits(what, "function")){
    stop("The 'what' argument should refer to a function")
  }

  if(!inherits(args, "list")){
    stop("The 'args' argument should refer to a list of arguments")
  }

  if(!inherits(name, "character")){
    stop("The 'name' argument should refer to a string of characters")
  }

  if(!inherits(dir, "character")){
    stop("The 'dir' argument should refer to a string of characters")
  }

  if(!(inherits(subdir, "character") | is.null(subdir))){
    stop("The 'subdir' argument should refer to a string of characters or be NULL")
  }

  if(!(isTRUE(rerun) | isFALSE(rerun))){
    stop("The 'rerun' argument should be TRUE or FALSE")
  }

  if(!inherits(check.files, "list")){
    stop("The 'check.files' argument should be a list")
  }

  if(length(check.files) != 0){

    are.files.character <- unlist(lapply(check.files, inherits,
                                         what = "character"))
    if(any(!are.files.character)){
      stop("The 'check.files' argument should be a list of characters")
    }

    do.files.exist <- unlist(lapply(check.files, file.exists))

    if(any(!do.files.exist)){
      stop("The files in the 'check.files' argument are not found in the provided directory")
    }

  }

  if(!(isTRUE(check.seed) | isFALSE(check.seed))){
    stop("The 'check.seed' argument should be TRUE or FALSE")
  }

  # Function to be run ----

  folderdir  <- paste(dir, subdir, name, sep="/")

  run.function <- function(gloVar.what.save = what, gloVar.args.save = args){

    what.run <- gloVar.what.save

    gloVar.what.save <- list(f = formals(gloVar.what.save),
                             b = toString(body(gloVar.what.save)))

    save(gloVar.args.save, file = paste(folderdir, "gloVar.args.save", sep="/"))
    save(gloVar.what.save, file = paste(folderdir, "gloVar.what.save", sep="/"))

    if(isTRUE(check.seed)) {

      gloVar.seed.save <- .Random.seed

      save(gloVar.seed.save, file = paste(folderdir, "gloVar.seed.save", sep="/"))

    }

    if(length(check.files) != 0){

      gloVar.files.save <- md5sum(paste(files.dir, check.files, sep = "/"))

      save(gloVar.files.save, file = paste(folderdir, "gloVar.files.save", sep="/"))

    }

    if(isTRUE(speak)) print("Function running")

    gloVar.out <- do.call(what.run, gloVar.args.save)

    save(gloVar.out, file = paste(folderdir, "gloVar.out", sep="/"))

    return(gloVar.out)

  }

  # Run because rerun is asked -----

  if(rerun){

    if(!is.null(subdir)){
      if(!file.exists(paste(dir, subdir, sep="/"))) {
        dir.create(paste(dir, subdir, sep="/"))
      }
    }

    dir.create(folderdir)

    print("Forced Rerun")

    out.now <- run.function()

    return(out.now)
    # print(out.now)

  }

  # Run because there is no memento file ----

  if(!file.exists(folderdir)) {

    if(!is.null(subdir)){
      if(!file.exists(paste(dir, subdir, sep="/"))) {
        dir.create(paste(dir, subdir, sep="/"))
      }
    }

    dir.create(folderdir)

    print("No earlier run detected (general file missing)")

    out.now <- run.function()

    return(out.now)
    # print(out.now)

  }

  # Run because files are lacking ----

  lf <- list.files(folderdir)

  if(any(!(c("gloVar.args.save", "gloVar.what.save",
             "gloVar.out") %in% lf))){

    print("No earlier run detected (specific files missing)")

    out.now <- run.function()

    return(out.now)
    # print(out.now)

  }

  if(isTRUE(check.seed) & !("gloVar.seed.save" %in% lf)){

    print("No earlier run detected (seed file missing)")

    out.now <- run.function()

    return(out.now)
    # print(out.now)

  }

  if(length(check.files) != 0 & !("gloVar.files.save" %in% lf)){

    out.now <- run.function()

    print("No earlier run detected (the data to check the update of filed is missing)")

    return(out.now)
    # print(out.now)

  }

  # Run because environment, files, and/or seed have been updated ----

  what.now <- what

  what.now.compare <- list(f = formals(what.now),
                           b = toString(body(what.now)))

  args.now <- args

  load(paste(folderdir, "gloVar.args.save", sep="/"))
  load(paste(folderdir, "gloVar.what.save", sep="/"))

  if(!(identical(what.now.compare, gloVar.what.save) &
       identical(args.now, gloVar.args.save))){

    print("Update (R environment; function and arguments)")

    out.now <- run.function(what.now, args.now)

    return(out.now)
    # print(out.now)

  }

  if(check.seed){

    seed.now <- .Random.seed

    load(paste(folderdir, "gloVar.seed.save", sep="/"))

    if(!(identical(seed.now, gloVar.seed.save))){

      print("Update (R environment; seed)")

      out.now <- run.function(what.now, args.now)

      return(out.now)
      # print(out.now)

    }

  }

  if(length(check.files) != 0){

    files.now <- md5sum(paste(files.dir, check.files, sep = "/"))

    load(paste(folderdir, "gloVar.files.save", sep="/"))

    if(!(identical(files.now, gloVar.files.save))){

      print("Update (exterior files)")

      out.now <- run.function(what.now, args.now)

      return(out.now)
      # print(out.now)

    }

  }

  # Load the saved content if all the preceding conditions have been met ----

  load(paste(folderdir, "gloVar.out", sep="/"))

  return(gloVar.out)
  # print(gloVar.out)

}
