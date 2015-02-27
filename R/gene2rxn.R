#' @title gene to reaction lookup
#'   
#' @description Takes a sybil model and a data frame or vector of gene activity, and
#' finds active reactions from this.
#' 
#' @details Works by creating a new environment and evaluating gpr rules
#' computationally. Could be a security concern, so check what you're choosing
#' to run. Is also quite slow, if general.
#' 
#' @param genes a data frame or vector of gene activity. If a data frame, columns should be genes.
#' @param model a sybil model.
#' @param env an environment in which to evaluate the gene-reaction mappings.
#' @param exprlist \code{model@@gprRules}, parsed to expressions. Mainly for internal use. 
#' 
#' @return a vector of reaction presence or activity
#' 
#' @import sybil plyr compiler
#' 
#' @export
#' @rdname gene2rxn
gene2rxn <- function(genes, model, env=new.env(emptyenv())){
	UseMethod('gene2rxn')
}

#' @import sybil plyr compiler
#' @export
#' @rdname gene2rxn
gene2rxn.data.frame <- function(genes, model, env=new.env(emptyenv())){
	exprlist <- llply(model@gprRules, function(rule){parse(text=rule)})
	exprlist[model@gprRules==''] <- expression(1)
	
	if(any(grepl('^genotype\\.', names(genes)))){
		genes <- genes[,grepl('^genotype\\.', names(genes))]
		names(genes) <- sub('^genotype\\.', '', names(genes))
	}
	
	if(any(sapply(genes,is.numeric))){
		assign('&', function(x,y){min(x,y)}, env)
		assign('|', function(x,y){max(x,y)}, env)
		toeval <- function(g){gene2rxn.default(model=model, genes=genes[g,], env=env, exprlist=exprlist)}
	}else{
		toeval <- function(g){gene2rxn.default(model=model, genes=genes[g,], env=env, exprlist=exprlist)}
	}
	toeval <- cmpfun(toeval, options=list(optimize=3))
	
	eval(compile(options=list(optimize=3),
							 ldply(1:nrow(genes), toeval)
	))
}

#' @import sybil plyr compiler
#' @export
#' @rdname gene2rxn
gene2rxn.numeric <- function(genes, model, env=new.env(emptyenv()), exprlist=NULL){
	assign('&', function(x,y){min(x,y)}, env)
	assign('|', function(x,y){max(x,y)}, env)
	
	gene2rxn.default(genes, model, env, exprlist)
}

gene2rxn.default <- function(genes, model, env=new.env(emptyenv()), exprlist=NULL){
	# check arguments
	if(length(genes) != length(model@allGenes)){
		stop('genes should have the same length as model@allGenes')
	}
	
	env <- new.env(parent = baseenv())
	assign('x', genes, env)
	
	if(is.null(exprlist)){
		exprlist <- llply(model@gprRules, function(rule){parse(text=rule)})
		exprlist[model@gprRules==''] <- expression(1)
	}
	
	rxnsactivity <- vapply(exprlist, function(expr){eval(expr=expr, envir=env)[[1]]},1)
	
	stopifnot(length(rxnsactivity)==model@react_num)
	
	names(rxnsactivity) <- model@react_id
	return(rxnsactivity)
}