#' An output wrapper for language engine output
#'
#' If you have designed a language engine, you may call this function in the end
#' to format and return the text output from your engine.
#'
#' For expert users, an advanced usage of this function is
#' \code{engine_output(options, out = LIST)} where \code{LIST} is a list that
#' has the same structure as the output of \code{evaluate::evaluate()}. In this
#' case, the arguments \code{code} and \code{extra} are ignored, and the list is
#' passed to an internal function \code{knitr:::wrap()} to return a character
#' vector of final output.
#' @import knitr
#' @param options A list of chunk options. Usually this is just the object
#'   \code{options} passed to the engine function; see
#'   \code{\link{knit_engines}}.
#' @param code Source code of the chunk, to which the output hook
#'   \code{source} is applied, unless the chunk option \code{echo} is \code{FALSE}.
#' @param out Text output from the engine, to which the hook \code{output}
#'   is applied, unless the chunk option \code{results} is \code{'hide'}
#' @param extra Any additional text output that you want to include.
#' @return A character string generated from the source code and output using
#'   the appropriate output hooks.
#' @export
#' @examples library(knitr)
#' engine_output(opts_chunk$merge(list(engine = 'Rscript')), code = '1 + 1', out = '[1] 2')
#' engine_output(opts_chunk$merge(list(echo = FALSE, engine = 'Rscript')), code = '1 + 1', out = '[1] 2')
#'
#' # expert use only
#' engine_output(opts_chunk$merge(list(engine = 'python')), out = list(structure(list(src = '1 + 1'), class = 'source'), '2'))
engine_output = function(options, code, out, extra = NULL) {
  if (missing(code) && is.list(out)) return(unlist(wrap(out, options)))
  if (!is.logical(options$echo)) code = code[options$echo]
  if (length(code) != 1L) code = knitr:::one_string(code)
  if (options$engine == 'sas' && length(out) > 1L && !grepl('[[:alnum:]]', out[2]))
    out = tail(out, -3L)
  if (length(out) != 1L) out = knitr:::one_string(out)
  out = sub('([^\n]+)$', '\\1\n', out)
  # replace the engine names for markup later, e.g. ```Rscript should be ```r
  options$engine = switch(
    options$engine, rb = "RevBayes",
    options$engine
  )
  knitr:::one_string(c(
    if (length(options$echo) > 1L || options$echo) knit_hooks$get('source')(code, options),
    if (options$results != 'hide' && !knitr:::is_blank(out)) {
      if (options$engine == 'highlight') out else knitr:::wrap.character(out, options)
    },
    extra
  ))
}


# options$engine.path can be list(name1 = path1, name2 = path2, ...); similarly,
# options$engine.opts can be list(name1 = opts1, ...)
get_engine_opts = function(opts, engine, fallback = '') {
  if (is.list(opts)) opts = opts[[engine]]
  opts %in% fallback
}

get_engine_path = function(path, engine) get_engine_opts(path, engine, engine)

#' RevBayes engine
#'
#' This engine allows users to run RevBayes in the RStudio window
#' @import knitr
#' @export

## RevBayes
eng_rb <- function(options) {
  # options - variables from knitr - called herein:
  # options$code - string, the code for that chunk
  # options$error - logical, should it fail on an error
  # options$eval - logical, should the code be evaluated
  # options$engine - should be == 'rb'
  # options$engine.path - path to rb
  #
  # options$rbHistoryDirPath - directory to put history files
  # options$refreshHistoryRB - remove existing history files if this is a new knitr doc
  # options$rbDiagnosticMode - run diagnostic mode
  ################################
  # set rbDiagnosticMode
  if(is.null(options$rbDiagnosticMode)){
    options$rbDiagnosticMode <- FALSE
  }
  # early exit if evaluated output not requested
  if (!options$eval){
    options$results = 'asis'
    return(engine_output(options, options$code, ''))
  }
  # set up path to rb
  rbPath <- knitr:::get_engine_path(options$engine.path, 'rb')
  # options$engine.opts - opts for engines that should include 'rb'
  # use get_engine_opts to pull out rb options
  opts <- get_engine_opts(options$engine.opts, 'rb')
  # engine specific options
  #
  # options$refreshHistoryRB
    # logical
    # Controls whether previous .eng_rb.knitr.cache files
       # should be deleted if this is the first rb chunk
    # If not defined, default is TRUE
  if(is.null(options$refreshHistoryRB)){
    options$refreshHistoryRB <- TRUE
  }
  # options$rbHistoryDirPath
    # string - path and name for rb history directory
    # default is ".eng_rb.knitr.cache" in working dir
  if(is.null(options$rbHistoryDirPath)){
    options$rbHistoryDirPath <- ".eng_rb.knitr.history"
  }
  #############
  rbOutPath <- paste0(options$rbHistoryDirPath, '/.eng_rb_out')
  rbCodePath <- paste0(options$rbHistoryDirPath, '/.eng_rb_code')
  rbOutPath <- normalizePath(rbOutPath, mustWork = FALSE)
  rbCodePath <- normalizePath(rbCodePath, mustWork = FALSE)
  #if(options$rbDiagnosticMode){ message("rb_chunk_counter = ", rb_chunk_counter(-1) )}
  # check (and simultaneously update) rb_chunk_counter()
  if(knitr:::rb_chunk_counter() == 1L){
    # this is the first time an rb code-chunk is run for this document
    # set prev_out artificially to 13
    prev_out <- 13
    #
    if(dir.exists(options$rbHistoryDirPath) & options$refreshHistoryRB){
      # need to get rid of old history
      unlink(options$rbHistoryDirPath, recursive = TRUE)
    }
    # once old files are cleared (if they exist)
    # Set up history directories
    dir.create(options$rbHistoryDirPath, showWarnings = TRUE)
    # get code to run
    code_to_run <- options$code
    # change options$refreshHistoryRB
    options$refreshHistoryRB <- FALSE
  }else{
    # if FALSE, then this isn't the first chunk in a document
    # error if history dir doesn't already exist (?)
    if(!dir.exists(options$rbHistoryDirPath)){
      stop("RevBayes history directory not found at specified path for later rb chunks")
    }
    # get length of old out file
    prev_out <- length(readLines(rbOutPath))
    # get old code history
    old_code <- readLines(rbCodePath)
    print(old_code)
    # combine
    code_to_run <- c(old_code, options$code)
  }
  # write code to history file
  xfun::write_utf8(code_to_run, con = rbCodePath)
  # make a temporary file of rb code to execute
     # don't need to one-string code
  tempF <- knitr:::wd_tempfile('.rb', '.Rev')
  tempF <- normalizePath(
    tempF,
    winslash = "/",
    mustWork = FALSE
  )
  # write to file and add q() line
  xfun::write_utf8(c(code_to_run, "q()"), con = tempF)
  # setup to delete temporary files for execution when done
  if(!options$rbDiagnosticMode){
     on.exit(unlink(tempF), add = TRUE)
     }
  # correct order for rb is options + cmdArg + file
  cmdArg = paste(opts, '-b', tempF)
  # execute code
  out = if (options$eval) {
    message(paste0('running Revbayes with cmd: rb ', cmdArg))
    tryCatch(
      system2(rbPath, cmdArg,
              stdout = TRUE,
              stderr = TRUE,
              env = options$engine.env
      ),
      error = function(e) {
        if (!options$error) stop(e)
        paste('Error in running command rb:')
      }
    )
  } else {''}
  # chunk option error=FALSE means we need to signal the error
  if (!options$error && !is.null(attr(out,'status'))) {
    stop(knitr:::one_string(out))
  }
  # write new out to rb out
  xfun::write_utf8(out, con = rbOutPath)
  # remove unwanted prev header+code from out
  out = out[-(1:prev_out)]
  # remove unwanted leading + trailing white space
  out = trimws(out)
  # remove empty lines
  out = out[!(out == "")]
  if(length(out)>1){
    # add numbers to each line
    out = paste0("[",1:length(out),"] ",out)
  }
  # return output via engine_output
  engine_output(options, code = options$code, out = out)
}



## output the code without processing it
eng_asis = function(options) {
  if (options$echo && options$eval) knitr:::one_string(options$code)
}

# write a block environment according to the output format
eng_block = function(options) {
  if (isFALSE(options$echo)) return()
  code = knitr:::one_string(options$code)
  to = pandoc_to()
  is_pandoc = !is.null(to)
  if (!is_pandoc) {
    # not in R Markdown v2
    to = out_format()
    if (!(to %in% c('latex', 'html', 'markdown'))) to = NULL
  }
  if (is.null(to)) return(code)
  if (to == 'beamer') to = 'latex'
  if (is_html_output(to)) to = 'html'
  type = options$type
  if (is.null(type)) return(code)
  # convert the chunk content to HTML or LaTeX (ideally I only need to specify
  # the markdown extension, but it is not implemented yet for LaTeX:
  # https://github.com/jgm/pandoc/issues/2453)
  if (is_pandoc) code = pandoc_fragment(code, if (to == 'html') 'html4' else to)
  l1 = options$latex.options
  if (is.null(l1)) l1 = ''
  h2 = options$html.tag %n% 'div'
  h3 = options$html.before %n% ''
  h4 = options$html.after %n% ''
  # e.g. type = c(latex = 'marginfigure', html = 'marginnote')
  if (to %in% names(type)) type = type[to]
  # block level tags? this is an incomplete list, but should work for most cases
  if (to == 'html') if (h2 %in% c('div', 'p', 'blockquote')) {
    code = paste0('\n', code, '\n')
  } else {
    code = gsub('<p>', '<span style="display: block;">', code)
    code = gsub('</p>', '</span>', code)
  }
  switch(
    to,
    latex = sprintf('\\begin{%s}%s\n%s\n\\end{%s}', type, l1, code, type),
    html =  sprintf('%s<%s class="%s">%s</%s>%s', h3, h2, type, code, h2, h4),
    code
  )
}

# helper to create engines the wrap embedded html assets (e.g. css,js)
eng_html_asset = function(prefix, postfix) {
  function(options) {
    out = if (options$eval && is_html_output(excludes = 'markdown')) {
      knitr:::one_string(c(prefix, options$code, postfix))
    }
    options$results = 'asis'
    engine_output(options, options$code, out)
  }
}


# additional engines
knit_engines$set(
  rb = eng_rb
)

get_engine = function(name) {
  fun = knit_engines$get(name)
  if (is.function(fun)) return(fun)
  warning(
    "Unknown language engine '", name,
    "' (must be registered via knit_engines$set())."
  )
  function(options) {
    engine_output(options, options$code, '')
  }
}

cache_engine = function(options) {
  cache_fun = cache_engines$get(options$engine)
  if (!is.function(cache_fun)) return()
  cache_fun(options)
}
