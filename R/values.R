# Strict JSON values and native, whitelisted AST evaluation.
.ra_fail <- function(message, path = NULL, code = "INVALID_RULEPACK") {
  stop(structure(list(message=message, call=NULL, code=code, path=path),
                 class=c("ruleatlas_error", "error", "condition")))
}
.ra_object <- function(x=list()) { if (!length(x)) names(x) <- character(); x }
.ra_kind <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) return(if (is.null(names(x))) "array" else "object")
  if (length(x)!=1L || is.na(x)) .ra_fail("Not a JSON scalar")
  if (is.logical(x)) return("boolean")
  if (is.numeric(x)) return("number")
  if (is.character(x)) return("string")
  .ra_fail("Not a JSON value")
}
.ra_bounded <- function(x, depth=0L, budget=NULL) {
  if (is.null(budget)) { budget <- new.env(); budget$n <- 50000L }
  budget$n <- budget$n-1L
  if (depth>32L || budget$n<0L) .ra_fail("JSON resource limit", code="RESOURCE_LIMIT")
  k <- .ra_kind(x)
  if (k=="number" && (!is.finite(x) || abs(x)>9007199254740991)) .ra_fail("Numeric value outside portable range")
  if (k=="string" && nchar(x,type="bytes")>1048576) .ra_fail("String resource limit",code="RESOURCE_LIMIT")
  if (k %in% c("object","array")) {
    if (k=="object" && anyDuplicated(names(x))) .ra_fail("Duplicate JSON key")
    for (v in x) .ra_bounded(v,depth+1L,budget)
  }
  invisible(x)
}
.ra_equal <- function(a,b) {
  k <- .ra_kind(a)
  if (k!=.ra_kind(b)) return(FALSE)
  if (k=="object") return(setequal(names(a),names(b)) && all(vapply(names(a),function(n) .ra_equal(a[[n]],b[[n]]),logical(1))))
  if (k=="array") return(length(a)==length(b) && all(vapply(seq_along(a),function(i) .ra_equal(a[[i]],b[[i]]),logical(1))))
  if (k=="null") return(TRUE)
  isTRUE(a==b)
}
.ra_get <- function(x,k,default=NULL) { if (k %in% names(x)) x[[k]] else default }
.ra_typed <- function(value,spec,path="") {
  if (is.null(value) && isTRUE(spec$nullable)) return(invisible(NULL))
  k <- .ra_kind(value); t <- spec$type
  valid <- k==t || (t=="integer" && k=="number" && value==floor(value))
  if (t=="enum") valid <- any(vapply(spec$values,function(v) .ra_equal(value,v),logical(1)))
  if ("enum" %in% names(spec) && !any(vapply(spec$enum,function(v) .ra_equal(value,v),logical(1)))) valid <- FALSE
  if (!valid) .ra_fail("Field type mismatch",path)
  if (k=="number" && (value<.ra_get(spec,"minimum",-9007199254740991) || value>.ra_get(spec,"maximum",9007199254740991))) .ra_fail("Numeric bound",path)
  if (k %in% c("string","array")) {
    suffix <- if(k=="string") "Length" else "Items"
    n <- if(k=="string") nchar(value,type="chars") else length(value)
    if (n<.ra_get(spec,paste0("min",suffix),0) || n>.ra_get(spec,paste0("max",suffix),1048576)) .ra_fail("Length bound",path)
  }
  if (k=="array") for(i in seq_along(value)) .ra_typed(value[[i]],spec$items,paste0(path,"/",i-1L))
  if (k=="object") {
    props <- spec$properties
    if (length(setdiff(names(value),names(props))) || length(setdiff(unlist(spec$required),names(value)))) .ra_fail("Object fields mismatch",path)
    for(n in names(value)) .ra_typed(value[[n]],props[[n]],paste0(path,"/",n))
  }
  invisible(NULL)
}
.ra_fields <- function(value,specs,path="") .ra_typed(value,list(type="object",properties=specs,required=as.list(names(specs))),path)
.ra_ops <- c(eq=2L,ne=2L,lt=2L,lte=2L,gt=2L,gte=2L,not=1L,len=1L,add=2L,sub=2L,mul=2L,mod=2L,`in`=2L,`and`=0L,`or`=0L)
.ra_budget <- function(n=50000L) { b<-new.env(); b$n<-n; b }
.ra_expr_check <- function(expr,depth=0L,budget=.ra_budget(2000L)) {
  budget$n<-budget$n-1L
  if(depth>32L || budget$n<0L) .ra_fail("Expression resource limit",code="RESOURCE_LIMIT")
  k<-.ra_kind(expr)
  if(!k %in% c("object","array")) { .ra_bounded(expr); return(invisible(NULL)) }
  if(k!="object") .ra_fail("Expression must be scalar or AST")
  if(identical(names(expr),"ref")) {
    if(.ra_kind(expr$ref)!="string" || !grepl("^(state|params)\\.[A-Za-z0-9_]+(\\.[A-Za-z0-9_]+)*$",expr$ref)) .ra_fail("Invalid reference")
    return(invisible(NULL))
  }
  if(!setequal(names(expr),c("op","args")) || !is.character(expr$op) || !expr$op %in% names(.ra_ops) || .ra_kind(expr$args)!="array") .ra_fail("Invalid operator")
  n<-length(expr$args); arity<-.ra_ops[[expr$op]]
  if(n<1L || n>32L || (arity>0L && n!=arity)) .ra_fail("Invalid operator arity")
  for(a in expr$args) .ra_expr_check(a,depth+1L,budget)
  invisible(NULL)
}
.ra_resolve <- function(value,parts) {
  for(part in parts) {
    k<-.ra_kind(value)
    if(k=="object" && part %in% names(value)) value<-value[[part]]
    else if(k=="array" && grepl("^(0|[1-9][0-9]*)$",part) && as.numeric(part)<length(value)) value<-value[[as.numeric(part)+1L]]
    else .ra_fail("Unresolved reference")
  }
  value
}
.ra_expr <- function(expr,state,params,budget=.ra_budget()) {
  budget$n<-budget$n-1L
  if(budget$n<0L) .ra_fail("Evaluation resource limit",code="RESOURCE_LIMIT")
  if(.ra_kind(expr)!="object") return(expr)
  if("ref" %in% names(expr)) return(.ra_resolve(list(state=state,params=params),strsplit(expr$ref,".",fixed=TRUE)[[1]]))
  op<-expr$op; args<-lapply(expr$args,function(a) .ra_expr(a,state,params,budget))
  if(op %in% c("and","or","not")) {
    if(any(vapply(args,.ra_kind,character(1))!="boolean")) .ra_fail("Boolean operands required")
    return(switch(op,`and`=all(unlist(args)),`or`=any(unlist(args)),not=!args[[1]]))
  }
  if(op=="len") {
    if(!.ra_kind(args[[1]]) %in% c("string","array","object")) .ra_fail("Collection required")
    return(if(.ra_kind(args[[1]])=="string") nchar(args[[1]],type="chars") else length(args[[1]]))
  }
  a<-args[[1]]; b<-args[[2]]
  if(op=="in") {
    if(.ra_kind(b)!="array") .ra_fail("Array required")
    return(any(vapply(b,function(x) .ra_equal(a,x),logical(1))))
  }
  if(.ra_kind(a)!=.ra_kind(b)) .ra_fail("Matching operand types required")
  if(op %in% c("eq","ne")) return(if(op=="eq") .ra_equal(a,b) else !.ra_equal(a,b))
  if(.ra_kind(a)!="number") .ra_fail("Numeric operands required")
  if(op %in% c("lt","lte","gt","gte")) return(switch(op,lt=a<b,lte=a<=b,gt=a>b,gte=a>=b))
  if(op=="mod") {
    if(a!=floor(a) || b!=floor(b) || b<=0) .ra_fail("Modulo requires integers and positive divisor")
    return(a%%b)
  }
  v<-switch(op,add=a+b,sub=a-b,mul=a*b)
  .ra_bounded(v); v
}
