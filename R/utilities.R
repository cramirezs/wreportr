#' @title Address
#' @description This transforms an address in the server to a path.
#' @param x Address.
#' @param address Server address, Default: 'file://'.
#' @param path Path equivalent of the address, Default: ''.
#' @return Path.
#' @examples
#' \dontrun{
#'  if(interactive()){
#'   address2path("file://Groups/mypath/step")
#'   address2path(
#'     x = "https://my_website/Groups/mypath/step",
#'     address = "https://my_website/Groups",
#'     path = "/mnt/Groups"
#'   )
#'  }
#' }
#' @rdname address_path
#' @export
address2path = function(
  x,
  address = "file://",
  path = ""
){
  y = if(dir.exists(x)) normalizePath(x) else x
  if(path != "") path = paste0(normalizePath(path), "/")
  y = sub(pattern = paste0(".*", basename(address), "\\/"), replacement = path, x = y)
  gsub("\\/{2,}", "/", y)
}

#' @title Path as sufix
#' @description Transform a path to a server address sufix (after object@address)
#' @param x Path in disk.
#' @param address Server address, Default: 'file://'.
#' @return Path ready to go after 'address'.
#' @examples
#' \dontrun{
#'  if(interactive()) path2address_sufix("/mnt/Groups/mypath/step")
#' }
#' @rdname address_path
#' @export
path2address_sufix = function(
  x,
  address = "file://"
){
  gsub(paste0(".*", basename(address), "\\/"), "/", normalizePath(x))
}

#' @title Path to address
#' @description Takes a path and transforms it into and address.
#' @param x Path.
#' @param address Server address, Default: 'file://'.
#' @return Address.
#' @examples
#' \dontrun{
#'  if(interactive()) path2address("/mnt/Groups/mypath/step")
#' }
#' @rdname address_path
#' @export
path2address = function(
  x,
  address = "file://"
){
  paste0(address, path2address_sufix(x, address))
}

#' @title Path tree
#' @description Takes a path and transforms it into a tree.
#' @param x Path.
#' @param hide Make it collapsible, Default: FALSE.
#' @return Prints the tree.
#' @examples
#' \dontrun{
#'  if(interactive()) path_tree("~/Documents", hide = TRUE)
#' }
#' @rdname address_path
#' @export
#' @importFrom utils capture.output
path_tree = function(
  path,
  hide = FALSE
){
  if (requireNamespace("data.tree", quietly = TRUE)){
    y = list.files(path = path, full.names = TRUE)
    # unname(unlist(sapply(
    #   X = list.files(path = path, full.names = TRUE),
    #   FUN = list.files, full.names = TRUE
    # )))
    y <- gsub(normalizePath(path), ".", y)
    tmp = tempfile()
    capture.output(
      print(data.tree::as.Node(data.frame(pathString = y))),
      file = tmp
    ); y <- c(paste0(path, "\n"), paste0(readLines(tmp)[-1], "\n"))
    cat("\nContent tree\n")
    if(isTRUE(hide)) cat("<details>\n  <summary>Show details</summary>\n    ")
    if(isTRUE(hide)) cat(gsub("\\\n", "\\\n    ", y))
    if(!isTRUE(hide)) cat(y)
    if(isTRUE(hide)) cat("\n</details>")
    cat("\n\n")
  }
  invisible(x = NULL)
}

#' @title Step output
#' @description Finding step output path.
#' @param object Reportr object.
#' @param name Name of the step to process, Default: NULL.
#' @param address Server address, Default: 'file://'.
#' @param path Path equivalent of the address, Default: '/mnt/BioAdHoc/Groups/'.
#' @return List with output path and config (the rest of the information in the)
#'   the step element).
#' @details It can also take a yaml file with output_dir(followed by /project).
#' @examples
#' \dontrun{
#'  if(interactive()){
#'   process_step("~")
#'  }
#' }
#' @seealso
#'  \code{\link[yaml]{read_yaml}}
#' @rdname process_step
#' @export
#' @importFrom yaml read_yaml
process_step = function(
  object,
  name = NULL,
  address = "file://",
  path = ""
){
  if(class(object) == "Reportr"){
    address = object@address
    path = object@path
    object = object@steps
  }
  if(is.null(name)) name = 1
  if(isTRUE(names(object)[1] == "")) names(object)[1] = "output"
  if(names(object)[1] != "output") object = c(output = "no_path/found", object)
  config = list()
  if(is.null(object[[name]]))
    return(list(output = "no_path/found", config = config))
  output = NULL; step_i = object[[name]]
  # find the output path(s). Usually unnamed first element in list
  path_x = suppressWarnings(min(which(names(step_i) %in% c("", "output"))))
  if(is.infinite(path_x)) path_x = 1
  keep_x = if(is.list(step_i)) step_i[-path_x] # it will be sent to config
  # it can be a vector of paths and or YAMLs (only the last is sent to config)
  path2find = if(is.list(step_i)) step_i[[path_x]] else step_i

  for(i in path2find){
    path_i = if(file.exists(i) && grepl("\\.yml$|\\.yaml$", i)){
      config = yaml::read_yaml(i); y = config$output_dir
      if(!is.null(config$project)) y = paste0(y, "/", config$project)
      if(!is.null(config$demux) && !dir.exists(y)){ # silly pipeline output
        paste0(y, "_", config$demux$max_count_min, "th")
      }else if(!dir.exists(y)){ config$output_dir }else{ y }
    }else if(is.character(i) && !grepl("\\.yml$|\\.yaml$", i)){
      # If it's already an address in the server, it needs to find the disk path
      if(dir.exists(i)) i else address2path(i, address, path)
    }else{ "no_path/found" }
    output = unique(c(output, suppressWarnings(normalizePath(path = path_i))))
  }
  if(length(output) > 1){ # finding names for each path
    str_shared = do.call(intersect, strsplit(split = "\\/", x = output))
    out_names = sapply(X = str_shared, FUN = function(s){
      y <- setdiff(x = unlist(x = strsplit(split = "\\/", x = s)), y = str_shared)
      basename(path = paste0(y, collapse = "/"))
    }); if(sum(out_names != "") > 0) names(output) <- out_names
  } # add the rest of the list to config
  if(!is.null(keep_x)) config = c(config, keep_x[!names(keep_x) %in% names(config)])
  return(list(output = output, config = config))
}

#' @title Title format
#' @description Create title format.
#' @param x String to format.
#' @return Adapted string.
#' @examples
#' \dontrun{
#'  if(interactive()) make_title("the title of my famous book")
#' }
#' @seealso
#'  \code{\link[stringr]{case}}
#' @rdname make_title
#' @export
#' @importFrom stringr str_to_title
make_title = function(x){
  y <- gsub("orig|\\.", "_", casefold(x, upper = TRUE), ignore.case = TRUE)
  y <- gsub("_", " ", gsub("_{2,}", "_", y))
  y <- gsub(" {1,}", " ", y)
  y <- gsub(" $|^ ", "", y)
  stringr::str_to_title(gsub("_$|^_", "", y))
}

#' @title Step title
#' @description Show a title given to the step.
#' @param x Name.
#' @param conf Step's list/config.
#' @param hlevel Header level, Default: '###'.
#' @return Prints the title using `cat`.
#' @examples
#' \dontrun{
#'  if(interactive()) {
#'   report_title(x = "Step N", conf = list("~/Documents/liai/literature"))
#'  }
#' }
#' @rdname show-routine
#' @export
report_title = function(
  x,
  conf,
  hlevel = "###"
){
  .clunames = list(clu1 = "1. Loose", clu2 = "2. Strict", clu3 = "3. Cleaning")
  tnames = c("name", "title")
  y <- if(x %in% names(.clunames)){
    .clunames[[x]] # if alias exists already
  }else if(any(tnames %in% names(conf$lock))){
    print(tnames %in% names(conf$lock))
    print(names(conf$lock))
    conf$lock[[which(names(conf$lock) %in% tnames)]] # it's locked! so take the name
  }else if(any(tnames %in% names(conf))){
    conf[[which(names(conf) %in% tnames)]] # wow, you decided to put a name in the step's list
  }else if(grepl("clu[A-z]{1,}", x)){
    x # if the name _is_ the step
  }else if(!is.null(conf$project)){
    conf$project # take the name of the project
  }
  # y <- if(is.null(y)) x else y
  if(!is.null(y)) cat(hlevel, sub("^clu", "", y), '\n\n')
  invisible(x = NULL)
}

#' @title File definition
#' @description Define the `regex` pattern to the files to highlight.
#' @param x Step config or just the path to the step's output.
#' @param title_pattern File `regex`, Default: c("pdf$")
#' @param path Output path, Default: 'no_path/found'.
#' @param lock Is a "locked" analysis?, Default: FALSE.
#' @return Config list with the highlighting `regex` patterns.
#' @examples
#' \dontrun{
#'  if(interactive()) highlight_f_def("~/Documents/liai/literature")
#' }
#' @rdname highlight
#' @export
#' @importFrom utils head
highlight_f_def = function(
  x,
  title_pattern = c("pdf$"),
  path = "no_path/found",
  lock = FALSE
){
  if(is.null(title_pattern)) title_pattern = c("pdf$")
  fun = if(is.list(x)) list(c, list) else list(list, c)
  names(fun) = c("object", "files")
  if(!"hfiles" %in% names(x) && isTRUE(lock)){ # too tailored instructions
    if("lock" %in% names(x)) x = c(x[-(which(names(x) == "lock"))], x$lock)
    if(!"res" %in% names(x)) x = c(x, percentage = "_pct", pc = "_pc[0-9]", res = "0.2")
    patty = paste0(unlist(sapply(x[c("percentage", "pc")], head, 1)), collapse = ".*")
    res_files = list.files(path = path[[1]], pattern = patty, full.names = TRUE)
    p2show = grep(paste0("_qc$|_res", x[["res"]][1], "$"), res_files, value = TRUE)
    p2show = list.files(
      path = p2show,
      pattern = "dimen.*umap|^umap|percentage|_qc_violins",
      full.names = TRUE
    )
    p2show = suppressWarnings(normalizePath(p2show[!grepl("grid", p2show)]))
    title_pattern = c(
      "Clustering" = grep("dimen.*umap", p2show, value = TRUE)[1],
      "QC metrics" = grep("qc\\/", p2show, value = TRUE)[1],
      "QC Cluster" = grep("_qc_violins", p2show, value = TRUE)[1],
      "Markers" = grep("percentage", p2show, value = TRUE)[1]
    )
    title_pattern = title_pattern[!is.na(title_pattern)]
    if(length(title_pattern) == 0 ) title_pattern=NULL
  }
  if(!"hfiles" %in% names(x)) x = fun$object(x, hfiles = fun$files(title_pattern))
  if(!is.null(x$hfiles) && is.null(names(x$hfiles))){
    names(x$hfiles) <- make_title(make.names(gsub("pdf", "", basename(x$hfiles))))
  }
  return(x)
}

#' @title Deploy relevant plots
#' @description It finds plots given a vector of regex paths/
#' @param confs List of config steps.
#' @param name Name in the 'confs' list.
#' @param path Path where to find the plots, Default: '/no/path'.
#' @param lock Whethe to look for 'locking plots in clustering', Default: FALSE.
#' @param ... More parameters for 'highlight_f'.
#' @return All lines created are showed with 'cat'.
#' @details This might not work without {r, result='asis'}.
#' @examples
#' \dontrun{
#'  if(interactive()){
#'    report_files(
#'      confs = list(demx = list("/path/2/output", hfiles = 'pdf$')),
#'      name = "demx", path = "/path/2/output"
#'    )
#'  }
#' }
#' @rdname highlight
#' @export
report_files = function(
  confs,
  name,
  path = "/no/path",
  lock = FALSE,
  ...
){
  hashtags = c("acap", "cite", "hash")
  if(name %in% hashtags) name = hashtags[hashtags %in% names(confs)][1]
  if(name %in% names(confs)){
    tvar <- if("hfiles_add" %in% names(confs[[name]])){
      tmp <- confs[[name]][["hfiles_add"]]
      c(highlight_defaults[[name]], tmp[!names(tmp) %in% names(highlight_defaults[[name]])])
    }else{ highlight_defaults[[name]] }
    confs[[name]] = highlight_f_def(
      x = confs[[name]], title_pattern = tvar, path = path, lock = lock
    )
    highlight_f(hfiles = confs[[name]]$hfiles, path = path, ...)
  }
  invisible(x = NULL)
}

#' @title Highlighted files
#' @description Add highlighted files (mainly tables and Graphics).
#' @param hfiles `regex` to find the file.
#' @param path Step's output path.
#' @param hlevel Header level, Default: '###'.
#' @param close_tabset In case you need something to show right after the
#' last tab, Default: FALSE.
#' @param address Server address, Default: 'file://'.
#' @param verbose Show progress, Default: FALSE.
#' @return Adds the file to the report.
#' @details Tries to find the file using the `regex`.
#' @examples
#' \dontrun{
#'  if(interactive()) highlight_f("pdf", "~/Documents/liai/literature")
#' }
#' @seealso
#'  \code{\link[stringr]{case}}
#'  \code{\link[data.table]{fread}}
#'  \code{\link[knitr]{kable}}
#' @rdname highlight
#' @export
#' @importFrom stringr str_to_sentence
#' @importFrom utils str tail
#' @importFrom data.table fread
#' @importFrom knitr kable
highlight_f = function(
  hfiles,
  path,
  hlevel = "###",
  close_tabset = FALSE,
  address = "file://",
  verbose = FALSE
){
  .is_file = function(x) file.exists(x) && !dir.exists(x) && length(x) > 0
  verbose = verbose && interactive()
  for(i in 1:length(path)){
    init_tabset = TRUE # to check when to add the tabset
    if(!dir.exists(path[[i]])) next #warning("Path not found: ", path[[i]])
    result_name = if(is.null(names(path))) "Highlighted" else stringr::str_to_sentence(make_title(names(path)[i]))
    for(j in 1:length(hfiles)){
      p2find = hfiles[[j]][1]; p2name = names(hfiles)[j]; if(is.null(p2name)) p2name = ""
      p2find <- gsub("\\.\\*", "*", p2find)
      path_j = ifelse(.is_file(path[[i]]) || .is_file(p2find), "", paste0(path[[i]], "/"))
      if(length(p2find) == 0) next
      if(verbose){ cat("Path and pattern"); str(path_j); str(p2find) }
      fname_i <- suppressWarnings(try( # in case it's a deeper path (the file is not in path/.)
        system(paste0("ls ", path_j, p2find), intern = TRUE, ignore.stderr = TRUE), silent = TRUE
      ))
      if(verbose){ cat("From system:"); print(fname_i); }
      if(!.is_file(fname_i)) fname_i <- sort(list.files(path = path_j, pattern = p2find, full.names = TRUE))
      if(verbose){ cat("From list.files:"); str(fname_i); }
      if(.is_file(fname_i)){
        if(verbose){ cat("It is a file\n"); }
        if(length(fname_i) > 1) fname_i <- tail(fname_i[order(file.info(fname_i)$ctime)], 1)
        if(p2name == "") p2name <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(fname_i))
        # if(!grepl("csv$|txt$|pdf$|png$", fname_i)) next
        if(init_tabset){
          results_i = if(result_name != "Highlighted"){
            paste0("[results](", path2address(path_j, address), ")")
          }else{ "results" }
          cat(hlevel, "-", result_name, results_i, "{.tabset}\n\n"); init_tabset = FALSE
        }
        cat(hlevel, "# ", make_title(p2name), "\n\n", sep = "");
        if(!grepl("csv$|txt$|tsv$", fname_i)){ # same as ![]() but re-sizing works
          cat("<embed src='", fname_i, "' width='80%' height='600px'/>", sep = "")
          cat("\n\n", sep = "") # https://stackoverflow.com/questions/17784037/how-to-display-pdf-file-in-html
        }else{
          mytab = data.frame(data.table::fread(fname_i)) # it's a table
          if(!any(duplicated(mytab[, 1]))){ rownames(mytab) = mytab[, 1]; mytab = mytab[, -1, drop = FALSE] }
          print(knitr::kable(mytab, align = "l", digits = 2)); cat("\n\n")
        }
      }#else{ warning(p2find, "not found") }
    }
  }; if(!is.null(names(hfiles)) && isTRUE(close_tabset)) cat(hlevel, "{-}\n")
  invisible(x = NULL)
}

#' @title Filters
#' @description Add a line or table of used filters in a given step. Since a
#' step should probably come from a YAML file, then it'll iteratively look for
#' filtering, filters, filter, in that order. Lastly, it looks for subset.
#' @param x Filtering list.
#' @return None (invisible 'NULL'). If one filter it will show a line, if not
#'   it will use 'knitr::kable' after reshape2::melt.
#' @details It accepts a single filtering criteria or multiple (melted).
#' @examples
#' \dontrun{
#'   if(interactive()){
#'     report_filters(x = list(filtering = "n_umi >= 1000"))
#'   }
#' }
#' @seealso
#'  \code{\link[reshape2]{melt}}
#'  \code{\link[knitr]{kable}}
#' @rdname show-routine
#' @export
#' @importFrom reshape2 melt
#' @importFrom knitr kable
report_filters = function(x){
  x <- as.list(x)
  hierarchy = c("filtering", "filters", "filter", "subset")
  any_filter = FALSE
  for(i in hierarchy){
    if(i %in% names(x)){ x = x[[i]]; any_filter = TRUE }
  }
  if(any_filter){
    cat('<span style="color:red">**Filter:**</span> ')
    if(length(x) > 1){
      mytab = reshape2::melt(x); mytab <- mytab[, ncol(mytab):1]
      print(knitr::kable(mytab))
    }else{ cat(x[[1]]) }; cat("\n\n")
  }
  invisible(x = NULL)
}

#' @title Parameter selection
#' @description Show parameters used for a step.
#' @param x List of parameters used: name, Variance,
#' @return Prints a kable.
#' @details It accepts a list, looks for "lock" or "parameters" and takes the
#' first one it finds.
#' @examples
#' \dontrun{
#'   if(interactive()) report_parameters(list(parameters = NULL))
#' }
#' @seealso
#'  \code{\link[knitr]{kable}}
#' @rdname show-routine
#' @export
#' @importFrom knitr kable
report_parameters = function(
  x
){
  params = c("lock", "parameters")
  if(any(params %in% names(x))){
    x = x[[which(names(x) %in% params)[1]]]
    if(is.null(x))
      x = list(name = "Cell type", percentage = "PP", pc = "CC", res = "R.R")
    x = as.list(x); total_rows = max(sapply(x, length))
    x = lapply(X = x, FUN = function(y) y[1:total_rows] )
    names(x) = stringr::str_to_title(names(x))
    mytab <- data.frame(x)
    print(knitr::kable(mytab, caption = "Parameters", align = "c")); cat("\n\n")
  }
  invisible(x = NULL)
}

#' @title Summarise table
#' @description `get_summary_table` summarise each column of a table.
#' @param mdata Object of class data.frame.
#' @param min_n Filter columns with this number of objects or less, Default: 1.
#' @return data.frame.
#' @details It only takes the columns of class character or factor.
#' @examples
#' \dontrun{
#'  if(interactive()) get_summary_table(reshape2::melt(Titanic))
#' }
#' @seealso
#'  \code{\link[reshape2]{melt}}
#' @rdname get_summary_table
#' @export
#' @importFrom reshape2 melt
get_summary_table = function(
  mdata,
  min_n = 1
){
  cnames <- sapply(mdata, is.character) | sapply(mdata, is.factor)
  if(all(!cnames)) cnames = head(colnames(mdata), 2)
  mdata_summ <- reshape2::melt(lapply(mdata[, cnames, drop = FALSE], table))
  tvar <- reshape2::melt(table(mdata_summ$L1)); tvar$L1 <- paste0("v.", tvar$Var1)
  mdata_summ <- rbind(tvar, mdata_summ); mdata_summ$Var1 <- as.character(mdata_summ$Var1)
  var2filt <- sapply(unique(as.character(mdata_summ$L1)), function(x){
    y <- mdata_summ[mdata_summ$L1 == x, 2]
    any(y > min_n & sum(y))
  }); mdata_summ <- mdata_summ[mdata_summ$L1 %in% names(which(var2filt)), ]
  mdata_summ$Var1[grepl("^v\\.", mdata_summ$L1)] <- ""
  mdata_summ$L1 <- gsub("^v\\.", "", mdata_summ$L1)
  rownames(mdata_summ) <- NULL # Var1, value, L1
  colnames(mdata_summ) <- c("Value", "Number", "Category"); mdata_summ[, 3:1]
}

#' @title Tables
#' @description Show tables with DT::datatable or knitr::kable.
#' @param x File path(s).
#' @param interactive Use DT?, Default: 'choose' (if No. of rows > 15).
#' @param table_summarise Make a summary of the table, Default: FALSE.
#' @return Creates a table.
#' @examples
#' \dontrun{
#'  if(interactive()) {
#'   report_tables(x = "~/Documents/liai/asthma/biopsy/metadata_library.csv")
#'  }
#' }
#' @seealso
#'  \code{\link[data.table]{fread}}, \code{\link[data.table]{rbindlist}}
#'  \code{\link[DT]{datatable}}
#'  \code{\link[knitr]{kable}}
#' @rdname show-routine
#' @export
#' @importFrom data.table fread rbindlist
#' @importFrom DT datatable
#' @importFrom knitr kable
report_tables = function(
  x,
  interactive = "choose",
  table_summarise = FALSE
){
  dt_datatable = paste0("dtdf = lapply(X = myfiles, data.table::fread)\n",
  "dtdf <- as.data.frame(data.table::rbindlist(dtdf, fill = TRUE))\n",
  "DT::datatable(\n",
  "  data = dtdf, rownames = FALSE, filter = 'top',\n",
  "  extensions = list('Buttons' = NULL, 'FixedColumns' = list(leftColumns = 1)),\n",
  "  options = list(\n",
  "    dom = 'BRrltpi',\n",
  "    buttons = list(list(\n",
  "      extend = 'collection', buttons = c('csv', 'excel', 'pdf'),\n",
  "      text = 'Download'\n",
  "    )), pageLength = 5,\n",
  "    lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', 'All')),\n",
  "    scrollX = TRUE\n",
  "  )\n",
  ")\n```\n\n")
  java_toggle_f = paste0("<script type=\"text/javascript\">\n",
    "  function toggle(id) {\n",
    "    var e = document.getElementById(id);\n",
    "    if(e.style.display == 'block')\n",
    "       e.style.display = 'none';\n",
    "    else\n",
    "       e.style.display = 'block';\n",
    "  }\n",
    "</script>\n\n")
  java_toggle_table_f = function(x){
    paste0("<div id=\"msum", x, "\" style=\"display:none\">\n",
    "```{r metadata_summary", x, ", echo = FALSE, eval = TRUE}\n",
    "knitr::kable(wreportr::get_summary_table(dtdf), align = 'l')\n",
    "```\n\n",
    "</div>\n\n",
    "<button title=\"Click to show\" type=\"button\" onclick=\"toggle('msum", x, "')\">",
    "Summary</button>\n<p> </p>\n")
  }

  mytab_files <- if(!is.null(x)) x[file.exists(x)]
  if(is.null(interactive)) interactive = "choose"
  if(isTRUE(table_summarise)) cat(java_toggle_f)
  if(length(mytab_files) > 0){
    for(i in 1:length(mytab_files)){
      mytab_df = lapply(X = mytab_files[[i]], data.table::fread)
      mytab_df <- as.data.frame(data.table::rbindlist(mytab_df, fill = TRUE))
      if(isTRUE(interactive == "choose")) interactive = isTRUE(nrow(mytab_df) > 15)
      if(isTRUE(interactive)){
        tmp = tempfile(fileext = ".rmd") # mytab_files[[i]] = c("f1", "f2")
        cat(paste0("```{r childish", i, ", echo=FALSE}\n",
          "myfiles = c('", paste0(mytab_files[[i]], collapse = "', '"), "')\n",
          dt_datatable), file = tmp)
        if(isTRUE(table_summarise)){
          cat(java_toggle_table_f(i), append = TRUE, file = tmp)
        }
        res <- if(!is.null(knitr::current_input())){
          knitr::knit_child(tmp, quiet = TRUE)
        }else{ readLines(tmp) }
        cat(res, sep = '\n'); cat("\n\n")
      }else{
        print(knitr::kable(mytab_df, align = "c", digits = 2))
      }; cat("\n\n")
    }
  }
  invisible(x = NULL)
}

#' @title Link output
#' @description Create link to output path.
#' @param x Path to step's output.
#' @param address Server address, Default: 'file://'.
#' @return Prints markdown link.
#' @examples
#' \dontrun{
#'  if(interactive()) report_link("~/Documents/liai/literature")
#' }
#' @seealso
#'  \code{\link[stringr]{case}}
#' @rdname report_link
#' @export
#' @importFrom stringr str_to_sentence
report_link = function(
  x,
  address = "file://"
){
  if(!is.null(x)){
    if(is.list(x)) x = x[[1]]
    for(i in 1:length(x)){
      if(!dir.exists(x[i])) next
      res_name = if(!is.null(names(x))) paste(stringr::str_to_sentence(make_title(names(x)[i])), "[results")
      cat(paste0("", ifelse(is.null(res_name), "[Results", res_name), "](", path2address(x[i], address), ")"))
      fname = list.files(x[i], pattern = "_results_outline", full.names = TRUE)
      if(length(fname) > 0) cat(paste0(" / Parameter [tweaking](", path2address(fname, address), ")"))
      cat("\n\n")
    }
  }
  invisible(x = NULL)
}

#' @title Description
#' @description Add description from a step element.
#' @param x Description or name in 'conf'.
#' @param conf Step's arguments, Default: NULL.
#' @param hide Make it collapsible, Default: FALSE.
#' @return Prints the description.
#' @examples
#' \dontrun{
#'  if(interactive()){
#'   report_description("Description")
#'   report_description(list(description = "Brief description"))
#'   report_description("Description", hide = TRUE)
#'  }
#' }
#' @rdname show-routine
#' @export
report_description = function(
  x,
  conf = NULL,
  hide = FALSE
){
  # look in preset_description and fetch the indicated from the list
  x_name = if(!is.list(x)) if(!is.null(preset_description[[x]])) preset_description[[x]] else x
  x_name = if(is.character(x_name)){ if(exists(x_name)) eval(parse(text = x)) else x_name }else x_name
  # check if it's a list already and get $description from it or finally just take the input "x"
  y = if(is.list(x_name)){ if(is.null(x_name$descri)) x_name else x$descri }else x_name
  if(isTRUE(y %in% c(names(conf), x))) y = NULL
  if(is.character(y)) if(any(file.exists(y))) y <- NULL
  if(!is.null(y)){
    if(isTRUE(hide)) cat("<details>\n  <summary>Show details</summary>\n    ")
    if(isTRUE(hide)) cat(gsub("\\\n", "\\\n    ", y))
    if(!isTRUE(hide)) cat(y)
    if(isTRUE(hide)) cat("\n</details>")
    cat("\n\n")
  }
  invisible(x = NULL)
}

#' @title Google doc
#' @description Connect google document.
#' @param x Google path to document.
#' @return Prints an iframe object.
#' @examples
#' \dontrun{
#'  if(interactive()) {
#'   report_gsheet("spreadsheets/d/1v3ff8KKWGK5iCAyMa0v45wiw5El4Lcy0HVjTAbuYguU")
#'  }
#' }
#' @rdname show-routine
#' @export
report_gsheet = function(
  x
){
  find_google_id = function(x){
    ids <- base::strsplit(x, "/")[[1]]; ids <- ids[ids!="" | !grepl("drive", ids)]
    my_id <- ids[which.max(nchar(ids))]
    my_id <- gsub(".*id=", "", my_id)
    gsub("\\?..*|&.*", "", my_id)
  }
  if(!is.null(x)){
    x = unlist(x); x = x[grep("google", x)]
    cat(paste0(
      'We can continue discussions and documenting tasks or [ready to publish] ',
      'figure in our Google Drive <a href="', x,
      '" target="_blank">tracker sheet</a>.'
    ))
    if(grepl("spreadsheets", x)){
      cat(paste0('\n<iframe src="https://docs.google.com/spreadsheets/d/',
      find_google_id(x),
      '/preview" height="600px" width="800px" allowfullscreen></iframe>'))
   }; cat("\n\n")
  }
}

#' @title Issues
#' @description Show step's issues.
#' @param x Step's config or list of steps.
#' @param name Name in 'x', Default: ''.
#' @param hlevel Header level, Default: '###'.
#' @return Prints the issues (if found).
#' @examples
#' \dontrun{
#'  if(interactive()) {
#'   report_issues(list(issues = "Red is not a colour"))
#'   report_issues(list(issues = "Solved: red _is_ a colour"))
#'   report_issues(list(issues = "Fatal: red _is_ blue!"))
#'  }
#' }
#' @rdname show-routine
#' @export
report_issues = function(
  x,
  name = "",
  hlevel = "###"
){
  step_names_f = function(x){
    if(is.null(preset_titles[[x]])){
      step_types = paste0("(^", paste0(names(preset_titles), collapse = "|^"), ")")
      type <- preset_titles[[gsub(paste0(step_types, ".*"), "\\1", x)]]
      y <- gsub(step_types, ifelse(is.null(type), "", " "), x)
      make_title(paste0(type, y))
    }else{ preset_titles[[x]] }
    invisible(x = NULL)
  }
  is_config = function(x){
    if(!is.list(x)){
      FALSE
    }else sum(unlist(sapply(x, function(x){
      if(is.character(x)) file.exists(unlist(x)[[1]]) else 0
    }))) > 1
  }
  .issue_types = c(
    Solved = "[&#10004;]", solved = "[&#10003;]", # bold and thin ticks
    Unsolved = "[&#10006;]", unsolved = "[&#10005;]", # bold and thin crosses
    fatal = "[&#9888;]", Fatal = "<span style='color:red'>[&#9888;]</span>" # warning sign
  )

  if(is.list(x) && is_config(x)){
    for(z in names(x)) report_issues(x[[z]], name = step_names_f(z))
  }
  if("issues" %in% names(x)){
    if(sum(grepl("^solved", casefold(unlist(x$issues)))) == length(x$issues)){
      cat(hlevel, "-", name, "Solved issues\n\n")
    }else if(any(grepl("fatal", casefold(unlist(x$issues))))){
      cat(hlevel, "-", name, "Issues <span style='color:red'>&#9888;</span>\n\n")
    }else{ cat(hlevel, "-", name, "Issues &#9888;\n\n") }
    for(i in x[["issues"]]){
      i <- as.list(i); status = if(is.null(i$status)) "[&#10006;]" else .issue_types[i$status]
      y <- unlist(i); y <- y[!y %in% names(.issue_types)]
      if(any(grepl("solved", casefold(y)))) status = "[&#10003;]"
      cat("-", status, y, "\n\n")
    }
  }
  invisible(x = NULL)
}

#' @title Command
#' @description Show step's command(s).
#' @param x Step's config.
#' @param type Type of chunk (r, bash, python).
#' @return Prints the command into a selected chunk type.
#' @details Creates a temporary Rmd file and calls it with `knit_child`.
#' @examples
#' \dontrun{
#'  if(interactive()) report_command(list(command = "sh /path/to/script.sh"))
#' }
#' @seealso
#'  \code{\link[knitr]{knit_child}}
#' @rdname show-routine
#' @export
#' @importFrom knitr knit_child
report_command = function(
  x,
  type = "bash"
){
  if("command" %in% names(x)){# && !any(grepl("\\{bash", tvar))){
    tmp = tempfile(fileext = ".rmd")
    cat("```{", type, ", eval = FALSE}\n", file = tmp)
    cat(x[["command"]], "\n", file = tmp, append = TRUE)
    cat("```\n\n", file = tmp, append = TRUE)
    res <- if(!is.null(knitr::current_input())) knitr::knit_child(tmp, quiet = TRUE) else readLines(tmp)
    cat(res, sep = '\n'); cat("\n\n")
  }
  invisible(x = NULL)
}

#' @title Show section
#' @description Create step's section.
#' @param object Reportr object.
#' @param name Step's name, Default: 1.
#' @param hide Collapsible description, Default: FALSE.
#' @param lock Is it a step with "locked" parameters, Default: FALSE.
#' @param hlevel Header level, Default: '###'.
#' @param close_tabset In case you need something to show right after the
#' last tab, Default: FALSE.
#' @return Prints all the elements of a step.
#' @examples
#' \dontrun{
#'  if(interactive()) {
#'   mysteps = list(s1 = list("~/Documents/liai/literature", issues = "Oh no!"))
#'   mysteps$s1$hfiles = c(`Cool review` = "immunology.pdf")
#'   ws = create_wsteps(mysteps)
#'   report_section(ws)
#'  }
#' }
#' @rdname show-routine
#' @export
#' @importFrom utils tail
report_section = function(
  object,
  name = 1,
  hide = FALSE,
  lock = FALSE,
  hlevel = "###",
  close_tabset = FALSE
){
  if(is.numeric(name)) name = names(object@steps)[name]
  processed = process_step(object, name)

  report_title(x = name, conf = processed$config, hlevel = hlevel)
  report_description(x = name, conf = processed$config, hide = hide)
  report_command(x = processed$config)
  report_filters(x = processed$config)
  report_parameters(x = processed$config)
  report_tables(
    x = processed$config$table,
    interactive = processed$config$interactive,
    table_summarise = processed$config$table_summarise
  )
  report_issues(x = processed$config, hlevel = paste0(hlevel, "#"))
  report_link(x = processed$output, address = object@address)
  path_tree(path = processed$output, hide = TRUE)
  report_files(
    confs = object@steps, name = name,
    path = processed$output,
    lock = grepl("lock|parameters", names(processed$config)) || lock,
    hlevel = paste0(hlevel, "#"),
    close_tabset = close_tabset,
    address = object@address
  )
  if(isTRUE(name == "trac")) report_gsheet(object@steps[[name]])
  invisible(x = NULL)
}

#' @title Create step titles
#' @description `report_set_titles` takes the list of steps and uses the names
#' as titles.
#' @param object Reportr object.
#' @return Reportr object with titles in each step.
#' @examples
#' \dontrun{
#'  if(interactive()) report_set_titles(create_wsteps(list(`step n`="path")))@steps
#' }
#' @rdname report_set_titles
#' @export
report_set_titles = function(
  object
) {
  if(is.null(names(object@steps)))
    names(object@steps) = paste0("step_", 1:length(object@steps))
  nameless <- which(names(object@steps) == "")
  names(object@steps)[nameless] <- paste0("step_", nameless)
  for(i in names(object@steps)){
    i_t = if(is.null(preset_titles[[i]])) make_title(i) else preset_titles[[i]]
    if(!grepl("^$", i_t)) paste("$\\Box$", i_t)
    if(!"title" %in% names(object@steps[[i]]))
      object@steps[[i]] <- c(as.list(object@steps[[i]]), title = i_t)
  }
  return(object)
}

#' @title Markdown header
#' @description Check all necessary section of the YAML header are present.
#' @param object Reportr object.
#' @param verbose Show progress, Default: FALSE.
#' @return Text representation of an R markdown file's header.
#' @details If sections are not in the YAML, adds them from the object.
#' @examples
#' \dontrun{
#'  if(interactive()){
#'   cat(markdown_check(object = create_wsteps(list())))
#'   cat(markdown_check(object = create_wsteps(list(),
#'     markdown = list("~/Documents/liai/scripts/reportr_pkg/reportr_webspec.yaml")
#'   )))
#'   cat(markdown_check(object = create_wsteps(list(),
#'     markdown = list(output=list(html_document=list(theme='journal')))
#'   )))
#'  }
#' }
#' @seealso
#'  \code{\link[yaml]{read_yaml}}, \code{\link[yaml]{as.yaml}}
#' @rdname markdown_check
#' @export
#' @importFrom yaml read_yaml as.yaml
#'
markdown_check = function(
  object,
  verbose = FALSE
) {
  if(verbose) cat("Markdown specs\n");
  markdown_build = if(is.null(names(object@markdown))){
    if(verbose) cat(" - none\n"); list()
  }else if(file.exists(as.character(object@markdown[[1]][[1]]))){
    if(verbose) cat(" - from file\n"); tvar <- object@markdown[[1]][[1]]
    # tvar <- "~/Documents/liai/scripts/reportr_pkg/reportr_webspec.yaml"
    yaml::read_yaml(tvar)
  }else if(!is.null(names(object@markdown))){
    if(verbose) cat(" - from list\n"); object@markdown
  }else if(is.character(object@markdown[[1]])){
    if(verbose) cat(" - from text\n"); tmp = tempfile(fileext = ".rmd")
    cat(object@markdown[[1]][[1]], file = tmp)
    yaml::read_yaml(tmp)
  }else{ if(verbose) cat(" - none\n"); list() }
  if(verbose) str(markdown_build)
  if(is.null(markdown_build$title)) markdown_build$title = object@project.name
  if(verbose) cat(" * Author\n")
  if(length(markdown_build$author) == 0){
    if(verbose) cat("  > Adding\n")
    markdown_build$author = list(
      name = paste0("[", object@author, "](", object@link, ")"),
      affiliation = object@affiliation,
      email = object@email
    )
  }else if(is.character(markdown_build$author)){
    if(verbose) cat("  > Present\n")
    markdown_build$author = list(name = unlist(markdown_build$author)[1])
  }
  if(verbose) cat(" * Date\n")
  if(is.null(markdown_build$date)){
    markdown_build$date = paste(
      "Last update:", format(Sys.time(), '%Y, %B %d, at %H:%M')
    )
  }
  if(is.null(markdown_build$output)){
    if(verbose) cat(" * Output\n")
    tmp = tempfile(fileext = ".rmd")
    markdown_build$output = list(html_document = list(
      toc = 'true',
      toc_float = list(collapsed = 'false', smooth_scroll = 'false'),
      number_sections = 'false',
      fig_caption = 'true',
      theme = 'cosmo',
      code_folding = 'hide'
    ))
  }
  y <- yaml::as.yaml(markdown_build)
  y <- gsub(" yes\\\n", " true\n", gsub(" no\\\n", " false\n", y))
  y <- gsub(" 'true'\\\n", " true\n", gsub(" 'false'\\\n", " false\n", y))
  paste0("---\n", y, "---\n\n")
}

#' @title Step's report
#' @description `report_steps` goes over the steps of a Reportr object and.
#' deployes their elements.
#' @param object Reportr object.
#' @return Rendered Reportr object into r markdown.
#' @examples
#' \dontrun{
#'  if(interactive()) report_steps(create_wsteps(list(`step n`="path")))
#' }
#' @rdname report_steps
#' @export
report_steps = function(
  object
) {
  for(i in names(object@steps)){
    report_section(
      object = object,
      name = i,
      hlevel = "##"
    )
  }
}

#' @title Report from YAML
#' @description Takes a YAML file with steps as a dictionary/list.
#' @param file Path to the file.
#' @param rmd_file Preserve the generated Rmd, Default: NULL.
#' @param return_object Whether or not to retrun the object, Default: FALSE.
#' @param ... Extra parameters for render from rmarkdown.
#' @return NULL
#' @details A Reportr object saved in a temporary file as wells as the Rmd.
#' @examples
#' \dontrun{
#'  if(interactive()) report_yaml('vignette/example1.yaml')
#' }
#' @seealso
#'  \code{\link[yaml]{read_yaml}}
#' @rdname report_yaml
#' @export
#' @importFrom yaml read_yaml
report_yaml = function(
  file,
  rmd_file = NULL,
  return_object = FALSE,
  ...
) {
  report_yaml_str = yaml::read_yaml(file)
  my_steps = report_yaml_str[[which(names(report_yaml_str) == "steps")]]
  reportr_args = my_steps[which(names(my_steps) %in% c("address", "path"))]
  reportr = create_wsteps(
    steps = my_steps,
    markdown = report_yaml_str[-which(names(report_yaml_str) == "steps")],
    address = reportr_args$address,
    path = reportr_args$path
  )
  if(isTRUE(return_object)) return(reportr)
  report_render(reportr, rmd_file = rmd_file, ...)
}

#' @title Render Reportr object
#' @description `report_render` parses the Reportr object, creates a temporary
#' Rmd file and then calls render from markdown to create the report file.
#' @param object Reportr object.
#' @param rmd_file Preserve the generated Rmd, Default: NULL.
#' @param ... Extra parameters for render from rmarkdown.
#' @return NULL. It renders the object.
#' @examples
#' \dontrun{
#'  if(interactive()){
#'   report_render(create_wsteps(as.list(list.dirs("~/Documents", recursive = FALSE))))
#'  }
#' }
#' @seealso
#'  \code{\link[rmarkdown]{render}}
#' @rdname report_render
#' @export
#' @importFrom rmarkdown render
report_render = function(
  object,
  rmd_file = NULL,
  ...
) {
  if(is.null(rmd_file))
    rmd_file = tempfile(fileext = paste0(make.names(object@project.name), ".rmd"))
  object = report_set_titles(object)
  ofile = tempfile(fileext = ".rds")
  saveRDS(object, file = ofile)
  cat(markdown_check(object), file = rmd_file)
  cat("```{r, echo=FALSE, results='asis'}\n", append = TRUE, file = rmd_file)
  cat(paste0("reportr = readRDS('",ofile,"')\n"), append = TRUE, file = rmd_file)
  cat("report_steps(object = reportr)\n", append = TRUE, file = rmd_file)
  cat("```\n", append = TRUE, file = rmd_file)
  cat("rmd file:", rmd_file, "\n")
  rmarkdown::render(input = rmd_file, ...)
  invisible(x = NULL)
}
