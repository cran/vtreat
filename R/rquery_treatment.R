

#' @importFrom wrapr %:=% %.>%
NULL

# don't look unitialized
. <- NULL

#' Flatten a list of functions onto d.
#' 
#' @param d object (usually a data souce)
#' @param fnlist a list of functions
#' @return fnlist[[length(fnlist)]](flatten_fn_list(d, fnlist[[-length(fnlist)]]) (or d if length(fnlist)<1)
#' 
#' @seealso \code{\link{as_rquery_plan}}
#' 
#' @keywords internal
#' 
#' @export
#' 
flatten_fn_list <- function(d, fnlist) {
  for(i in seq_len(length(fnlist))) {
    d <- fnlist[[i]](d)
  }
  d
}


#' Build a query mapping NaN, Infinity and -Infinity to NULL.
#' 
#' Using PostgreSQL type design where math is altered to pick up 
#' these values by equality: 
#' \url{https://www.postgresql.org/docs/9.0/static/datatype-numeric.html}.
#' 
#' @param rqplan an query plan produced by as_rquery_plan().
#' @param data_source relop, data source (usually a relop_table_source).
#' @param col_sample sample of data to determine column types.
#' @return sql_node conversion
#' 
#' @keywords internal
#' 
#' @noRd
zap_bad_numeric_rqplan_vars_q <- function(rqplan, 
                                          data_source, 
                                          col_sample) {
  numvars <- lapply(
    rqplan$treatmentplans,
    function(tp) {
      tp$scoreFrame$origName[tp$scoreFrame$code %in% c("clean", "isBAD")]
    })
  numvars <- unique(unlist(numvars))
  numvars <- intersect(rquery::column_names(data_source), numvars)
  if(length(col_sample)>0) {
    check <- intersect(numvars, colnames(col_sample)) 
    drop <- vapply(check,
                   function(ci) {
                     (!is.numeric(col_sample[[ci]])) || (is.integer(col_sample[[ci]]))
                   }, logical(1))
    drop <- check[drop]
    numvars <- setdiff(numvars, drop)
  }
  if(length(numvars)<=0) {
    return(data_source)
  }
  exprs <- lapply(
    numvars,
    function(vi) {
      list("(CASE WHEN",
           "(", as.name(vi), "=", list("NaN"), ") OR",
           "(", as.name(vi), "=", list("Infinity"), ") OR ",
           "(", as.name(vi), "= -", list("Infinity"), ")",
           "THEN NULL ELSE", as.name(vi), "END)")
    })
  names(exprs) <- numvars
  rquery::sql_node(data_source, exprs, orig_columns = TRUE)
}


#' Materialize a treated data frame remotely.
#' 
#' @param db a db handle.
#' @param rqplan an query plan produced by as_rquery_plan().
#' @param data_source relop, data source (usually a relop_table_source).
#' @param result_table_name character, table name to land result in
#' @param ... force later arguments to bind by name.
#' @param extracols extra columns to copy.
#' @param temporary logical, if TRUE try to make result temporary.
#' @param overwrite logical, if TRUE try to overwrite result.
#' @param attempt_nan_inf_mapping logical, if TRUE attempt to map NaN and Infnity to NA/NULL (goot on PostgreSQL, not on Spark).
#' @param col_sample sample of data to determine column types.
#' @param return_ops logical, if TRUE return operator tree instead of materializing.
#' @return description of treated table.
#' 
#' @seealso \code{\link{as_rquery_plan}}, \code{\link{rqdatatable_prepare}}
#' 
#' @export
#' 
rquery_prepare <- function(db, rqplan, data_source, result_table_name,
                           ...,
                           extracols = NULL,
                           temporary = FALSE,
                           overwrite = TRUE,
                           attempt_nan_inf_mapping = FALSE,
                           col_sample = NULL,
                           return_ops = FALSE) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::rquery_prepare requires the rquery package.")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::rquery_prepare")
  for(ni in names(rqplan$tables)) {
    rquery::rq_copy_to(db, ni, rqplan$tables[[ni]], 
                       overwrite = TRUE, temporary = TRUE)
  }
  if(!("relop" %in% class(data_source))) {
    stop("vtreat::rquery_prepare data_source must be an rquery::relop tree")
  }
  if(attempt_nan_inf_mapping) {
    data_source <- zap_bad_numeric_rqplan_vars_q(rqplan, data_source, col_sample)
  }
  ops <- flatten_fn_list(data_source, rqplan$optree_generators)
  selcols <- intersect(rquery::column_names(ops), 
                       unique(c(rqplan$outcomename, 
                                rqplan$newvars,
                                extracols)))
  ops <- rquery::select_columns(ops, selcols)
  if(return_ops) {
    return(ops)
  }
  treated <- rquery::materialize(db, ops, 
                                 table_name = result_table_name,
                                 temporary = temporary,
                                 overwrite = overwrite)
  for(ni in names(rqplan$tables)) {
    rquery::rq_remove_table(db, ni)
  }
  treated
}

#' @describeIn rquery_prepare old name for rquery_prepare function
#' @export
materialize_treated <- rquery_prepare





#' Apply a treatment plan using rqdatatable.
#' 
#' Note: does not treat map NaN or +-Infinity.  
#' This function is only for timings and demonstration, not for production use.
#' 
#' @param rqplan an query plan produced by as_rquery_plan().
#' @param data_source a data.frame.
#' @param ... force later arguments to bind by name.
#' @param extracols extra columns to copy.
#' @param partition_column character name of column to partition work by.
#' @param parallelCluster a cluster object, created by package parallel or by package snow. If NULL, use the registered default cluster.
#' @param use_parallel logical, if TRUE use parallel cluster (when available).
#' @param non_join_mapping logical, if TRUE use non-join based column mapping.
#' @param print_rquery logical, if TRUE print the rquery ops.
#' @param env environment to work in.
#' @return treated data.
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{as_rquery_plan}}, \code{\link{rquery_prepare}}
#' 
#' @export
#' 
rqdatatable_prepare <- function(rqplan, data_source,
                                ...,
                                partition_column = NULL,
                                parallelCluster = NULL,
                                use_parallel = use_parallel,
                                extracols = NULL,
                                non_join_mapping = FALSE,
                                print_rquery = FALSE,
                                env = parent.frame()) {
  force(env)
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::rqdatatable_prepare requires the rquery package.")
  }
  if(!requireNamespace("rqdatatable", quietly = TRUE)) {
    stop("vtreat::rqdatatable_prepare requires the rqdatatable package.")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::rqdatatable_prepare")
  source_name <- substitute(data_source)
  if(is.name(source_name)) {
    source_name <- as.character(source_name)
  } else {
    source_name <- "vtreat_rqdatatable_source"
  }
  if(!is.data.frame(data_source)) {
    stop("vtreat::rqdatatable_prepare data_source must be a data.frame")
  }
  tables <- rqplan$tables
  if(non_join_mapping) {
    # in-place changing, will be visible outside
    dat <- data_source
    for(ti in rqplan$tables) {
      colsi <- colnames(ti)
      if(length(colsi)!=2) {
        stop("vtreat::rqdatatable_prepare merge tables must have 2 columns")
      }
      keycol <- intersect(colnames(ti), colnames(dat))
      if(length(keycol)!=1) {
        stop("vtreat::rqdatatable_prepare merge tables must have 1 column in common with data")
      }
      newcol <- setdiff(colnames(ti), keycol)
      mpi <- ti[[newcol]]
      names(mpi) <- ti[[keycol]]
      dat[[newcol]] <- mpi[dat[[keycol]]]
    }
    source_hdl <- rquery::local_td(dat, name = source_name)
    ops <- flatten_fn_list(source_hdl, 
                           rqplan$optree_generators[length(rqplan$optree_generators)])
    tables[[source_name]] <- dat
  } else {
    source_hdl <- rquery::local_td(data_source, name = source_name)
    ops <- flatten_fn_list(source_hdl, rqplan$optree_generators)
    tables[[source_name]] <- data_source
  }
  selcols <- intersect(rquery::column_names(ops), 
                       unique(c(rqplan$outcomename, 
                                rqplan$newvars,
                                extracols,
                                partition_column)))
  ops <- rquery::select_columns(ops, selcols)
  if(print_rquery) {
    cat(format(ops))
  }
  if(is.null(partition_column) || (!use_parallel)) {
    treated <- rqdatatable::ex_data_table(ops, 
                                          tables = tables,
                                          env = env)
  } else {
    treated <- rqdatatable::ex_data_table_parallel(ops, 
                                                   partition_column = partition_column,
                                                   cl = parallelCluster,
                                                   tables = tables,
                                                   env = env)
  }
  treated
}





as_rquery <- function(tstep, 
                      ...,
                      var_restriction = NULL) {
  UseMethod("as_rquery")
}


#' @export
as_rquery.vtreatment <- function(tstep, 
                                 ...,
                                 var_restriction = NULL) {
  warning(paste("vtreat::as_rquery not yet implemented for ",
                format(tstep),
                ", class",
                paste(class(tstep), collapse = ", ")))
  NULL
}





#' Convert vtreatment plans into a sequence of rquery operations.
#' 
#' @param treatmentplans vtreat treatment plan or list of vtreat treatment plan sharing same outcome and outcome type.
#' @param ... not used, force any later arguments to bind to names.
#' @param var_restriction character, if not null restrict to producing these variables.
#' @return list(optree_generator (ordered list of functions), temp_tables (named list of tables))
#' 
#' @examples 
#' 
#' if(requireNamespace("rquery", quietly = TRUE)) {
#'    dTrainC <- data.frame(x= c('a', 'a', 'a', 'b' ,NA , 'b'),
#'                          z= c(1, 2, NA, 4, 5, 6),
#'                          y= c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE),
#'                          stringsAsFactors = FALSE)
#'    dTrainC$id <- seq_len(nrow(dTrainC))
#'    treatmentsC <- designTreatmentsC(dTrainC, c("x", "z"), 'y', TRUE)
#'    print(prepare(treatmentsC, dTrainC))
#'    rqplan <- as_rquery_plan(list(treatmentsC))
#'    ops <- flatten_fn_list(rquery::local_td(dTrainC), rqplan$optree_generators)
#'    cat(format(ops))
#'    if(requireNamespace("rqdatatable", quietly = TRUE)) {
#'       treated <- rqdatatable::ex_data_table(ops, tables = rqplan$tables)
#'       print(treated[])
#'    }
#'    if(requireNamespace("DBI", quietly = TRUE) &&
#'       requireNamespace("RSQLite", quietly = TRUE)) {
#'       db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'       source_data <- rquery::rq_copy_to(db, "dTrainC", dTrainC,
#'                                overwrite = TRUE, temporary = TRUE)
#' 
#'       rest <- rquery_prepare(db, rqplan, source_data, "dTreatedC", 
#'                                   extracols = "id")
#'       resd <- DBI::dbReadTable(db, rest$table_name)
#'       print(resd)
#' 
#'       rquery::rq_remove_table(db, source_data$table_name)
#'       rquery::rq_remove_table(db, rest$table_name)
#'       DBI::dbDisconnect(db)
#'    }
#' }
#' 
#' @seealso \code{\link{rquery_prepare}}
#' 
#' @export
#'
as_rquery_plan <- function(treatmentplans, 
                           ...,
                           var_restriction = NULL) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::as_rquery.treatmentplan requires the rquery package")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::as_rquery")
  if("treatmentplan" %in% class(treatmentplans)) {
    treatmentplans <- list(treatmentplans)
  }
  if((!is.list(treatmentplans)) || (length(treatmentplans)<1)) {
    stop("vtreat::as_rquery_plan treatmentplans must be a non-empty list of treatmentplans")
  }
  res <- list(
    exprs = character(0),
    optree_generators = list(),
    tables = list()
  )
  outcomename <- character(0)
  newvars <- character(0)
  for(tstep in treatmentplans) {
    if(!is.null(tstep)) {
      if(!('treatmentplan' %in% class(tstep))) {
        stop("vtreat::as_rquery_plan treatmentplans must be a non-empty list of treatmentplans")
      }
      newvarsi <- tstep$scoreFrame$varName
      if(!is.null(var_restriction)) {
        newvarsi <- intersect(newvarsi, var_restriction)
      }
      if(length(newvarsi)>0) {
        outcomename <- unique(c(outcomename, tstep$outcomename))
        if(length(outcomename)!=1) {
          stop("vtreat::as_rquery_plan treatmentplans must all share outcomes")
        }
        if(length(intersect(newvarsi, newvars))>0) {
          stop("vtreat::as_rquery_plan treatmentplans must produce disjoint sets of variables")
        }
        newvars <- c(newvars, newvarsi)
        for(ti in tstep$treatments) {
          ri <- as_rquery(ti, var_restriction = var_restriction)
          if(!is.null(ri)) {
            for(fld in c("exprs", "optree_generators", "tables")) {
              res[[fld]] <- c(res[[fld]], ri[[fld]])
            }
          }
        }
      }
    }
  }
  if(length(res$exprs)>0) {
    exprs <- res$exprs # don't get clobbered by res$exprs <- NULL assignment
    f <- function(d) {
      rquery::extend_se(d, exprs)
    }
    res$optree_generators <- c(
      res$optree_generators,      
      list(f))
  }
  res$exprs <- NULL
  res$treatmentplans = treatmentplans
  res$outcomename = outcomename
  res$newvars = newvars
  res
}

#' Build a function that will re-code a categorical value.
#' 
#' @param colname character, name of column to re-code.
#' @param resname character, name of column to produce.
#' @param coding_levels character, levels to not re-map to 'rare'
#' @param effect_values named map to numeric, levels 
#' @param ... not used, force later arguments to be bound by name.
#' @param levRestriction level restriction object.
#' @param default_value numeric, default value used on non-effect_values matches.
#' @param name_source a wrapr::mk_tmp_name_source()
#' @return function generator for rquery pipeline and advisory table.
#' 
#' @noRd
#' 
rquery_code_categorical <- function(colname, resname,
                                    coding_levels,
                                    effect_values,
                                    ...,
                                    levRestriction = NULL,
                                    default_value = 0.0,
                                    name_source = wrapr::mk_tmp_name_source("vtreat_tmp")) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::rquery_code_categorical requires the rquery package")
  }
  effect_values <- unlist(effect_values)
  wrapr::stop_if_dot_args(substitute(list(...)), 
                          "vtreat:::rquery_code_categorical")
  if(length(resname)!=1) {
    stop(paste("vtreat::rquery_code_categorical resname must be a single string",
               colname, "->", resname))
  }
  # work out coding table
  coding_levels <- coding_levels[grep("^x ", as.character(coding_levels))]
  coding_levels <- sort(unique(gsub("^x ", "", coding_levels))) # sort kills NA
  tnum <- 1
  while(TRUE) {
    new_novel_level <- paste0("new_novel_level_", tnum)
    if(!(new_novel_level %in% coding_levels)) {
      break
    }
    tnum <- tnum + 1
  }
  new_novel_value <- as.numeric(effect_values[.preProcCat(new_novel_level, levRestriction)])
  if(is.na(new_novel_value) || is.nan(new_novel_value) || is.infinite(new_novel_value)) {
    new_novel_value <- default_value
  }
  na_value <- as.numeric(effect_values[.preProcCat(NA_character_, levRestriction)])
  if(is.na(na_value) || is.nan(na_value) || is.infinite(na_value)) {
    na_value <- default_value
  }
  ctab <- data.frame(levels = coding_levels,
                     stringsAsFactors = FALSE)
  codes <- .preProcCat(ctab$levels, levRestriction)
  ctab$effect <- as.numeric(effect_values[codes])
  ctab$effect[is.na(ctab$effect) | is.nan(ctab$effect) | is.infinite(ctab$effect)] <- default_value
  if(length(ctab$levels)!=length(unique(ctab$levels))) {
    # should not happen, but let's catch it here so later joins are gauranteed to not blow-up
    stop(paste("vtreat:::rquery_code_categorical encoding levels were not unique, var:",
               colname, "->", resname))
  }
  if(nrow(ctab)>0) {
    names(ctab) <- c(colname, resname)
    code_tab <- name_source()
    ctabd <- rquery::table_source(code_tab, c(colname, resname))
    expr <- resname %:=% paste0("ifelse(is.na(", colname, "), ", na_value, 
                                ", ifelse(is.na(", resname, "), ", new_novel_value, ", ", resname, "))")
    f <- function(d) {
      rquery::natural_join(d, ctabd, jointype = "LEFT", by = colname) 
    }
    tables = list(code_tab = ctab)
    names(tables) <- code_tab
  } else {
    tables <- list()
    f <- list()
    expr <- resname %:=% paste0("ifelse(is.na(", colname, "), ", na_value, ", ", new_novel_value, ")")
  }
  list(
    exprs = expr,
    optree_generators = f, 
    tables = tables)
}



