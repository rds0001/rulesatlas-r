.ra_keys <- function(value,required,optional=character()) {
  if(.ra_kind(value)!="object" || length(setdiff(required,names(value))) || length(setdiff(names(value),c(required,optional)))) .ra_fail("Invalid object keys")
}
.ra_load <- function(value) {
  if(is.character(value) && length(value)==1L) {
    if(!file.exists(value) || dir.exists(value) || nzchar(Sys.readlink(value))) .ra_fail("Expected regular local rulepack file")
    if(file.info(value)$size>1048576) .ra_fail("Rulepack size limit",code="RESOURCE_LIMIT")
    value<-jsonlite::fromJSON(value,simplifyVector=FALSE)
  }
  .ra_bounded(value)
  if(nchar(jsonlite::toJSON(value,auto_unbox=TRUE,null="null",digits=NA),type="bytes")>1048576) .ra_fail("Rulepack size limit",code="RESOURCE_LIMIT")
  value
}
.ra_field_spec <- function(spec) {
  .ra_keys(spec,"type",c("minimum","maximum","minLength","maxLength","minItems","maxItems","values","items","properties","required","nullable"))
  if(!spec$type %in% c("integer","number","boolean","string","enum","array","object")) .ra_fail("Unknown field type")
  if("nullable" %in% names(spec) && .ra_kind(spec$nullable)!="boolean") .ra_fail("Invalid nullable")
  for(pair in list(c("minimum","maximum"),c("minLength","maxLength"),c("minItems","maxItems"))) {
    for(k in pair) if(k %in% names(spec) && (.ra_kind(spec[[k]])!="number" || (!k %in% c("minimum","maximum") && (spec[[k]]<0 || spec[[k]]!=floor(spec[[k]]))))) .ra_fail("Invalid field bound")
    if(all(pair %in% names(spec)) && spec[[pair[1]]]>spec[[pair[2]]]) .ra_fail("Reversed field bounds")
  }
  if(spec$type=="enum" && (.ra_kind(spec$values)!="array" || !length(spec$values))) .ra_fail("Empty enum")
  if(spec$type=="array") .ra_field_spec(spec$items)
  if(spec$type=="object") {
    if(.ra_kind(spec$properties)!="object") .ra_fail("Object properties required")
    for(v in spec$properties) .ra_field_spec(v)
    req<-.ra_get(spec,"required",list())
    if(.ra_kind(req)!="array" || any(vapply(req,function(k) .ra_kind(k)!="string" || !k %in% names(spec$properties),logical(1)))) .ra_fail("Invalid required fields")
  }
}
.ra_spec_path <- function(parts,state_fields,params) {
  spec<-list(type="object",properties=state_fields)
  for(part in parts) {
    if(spec$type=="object") {
      if(!part %in% names(spec$properties)) .ra_fail("Unknown effect field")
      spec<-spec$properties[[part]]
    } else if(spec$type=="array") {
      if(grepl("^\\{[A-Za-z0-9_]+\\}$",part)) {
        name<-substr(part,2L,nchar(part)-1L); p<-params[[name]]
        if(is.null(p) || p$type!="integer" || .ra_get(p,"minimum",-1)<0) .ra_fail("Unsafe dynamic index")
      } else if(!grepl("^(0|[1-9][0-9]*)$",part)) .ra_fail("Invalid array index")
      spec<-spec$items
    } else .ra_fail("Effect crosses scalar")
  }
  spec
}
.ra_overlap <- function(a,b) identical(a[seq_len(min(length(a),length(b)))],b[seq_len(min(length(a),length(b)))])
.ra_pack <- function(pack) {
  .ra_schema_validate(pack,.ra_pack_schema)
  .ra_keys(pack,c("schema_version","game_id","name","ruleset_id","ruleset_version","family","participants","state","actions","outcomes","provenance"),c("variants","capabilities","description"))
  if(pack$schema_version!="1.0") .ra_fail("Unknown schema version")
  if(.ra_kind(pack$game_id)!="string" || !grepl("^user\\.[a-z0-9_.-]{1,74}$",pack$game_id)) .ra_fail("Reserved or invalid game namespace")
  for(k in c("name","ruleset_id","ruleset_version")) if(.ra_kind(pack[[k]])!="string" || !nzchar(pack[[k]])) .ra_fail("Missing identifier")
  if(!startsWith(pack$ruleset_id,paste0(pack$game_id,"."))) .ra_fail("Ruleset owner mismatch")
  if(!pack$family %in% c("finite_turn","event_scoring","measured_rank","adjudicated","custom")) .ra_fail("Unknown family")
  .ra_keys(pack$participants,c("min","max")); lo<-pack$participants$min; hi<-pack$participants$max
  if(.ra_kind(lo)!="number" || .ra_kind(hi)!="number" || lo!=floor(lo) || hi!=floor(hi) || lo<1 || hi<lo || hi>512) .ra_fail("Invalid participant bounds")
  if(length(pack$variants)) .ra_fail("Variants not supported by DSL 1.0 runtime")
  .ra_keys(pack$state,c("fields","initial")); sf<-pack$state$fields
  if(.ra_kind(sf)!="object" || !length(sf)) .ra_fail("State fields required")
  for(k in names(sf)) {
    if(!grepl("^[A-Za-z_][A-Za-z0-9_]*$",k)) .ra_fail("Invalid field name")
    .ra_field_spec(sf[[k]])
  }
  .ra_fields(pack$state$initial,sf,"/state/initial")
  if(.ra_kind(pack$actions)!="array" || !length(pack$actions) || length(pack$actions)>512) .ra_fail("Invalid actions")
  action_names<-character(); budget<-.ra_budget(2000L)
  for(action in pack$actions) {
    .ra_keys(action,c("type","params","when","effects"))
    if(.ra_kind(action$type)!="string" || !nzchar(action$type) || action$type %in% action_names) .ra_fail("Duplicate or invalid action type")
    action_names<-c(action_names,action$type)
    if(.ra_kind(action$params)!="object") .ra_fail("Invalid parameters")
    for(k in names(action$params)) {
      if(!grepl("^[A-Za-z_][A-Za-z0-9_]*$",k)) .ra_fail("Invalid parameter name")
      .ra_field_spec(action$params[[k]])
    }
    .ra_expr_check(action$when,budget=budget)
    .ra_references(action$when,sf,action$params)
    if(!identical(.ra_expr_types(action$when,sf,action$params),"boolean")) .ra_fail("Guard must produce boolean")
    if(.ra_kind(action$effects)!="array" || !length(action$effects) || length(action$effects)>100) .ra_fail("Invalid effects")
    paths<-list()
    for(effect in action$effects) {
      op<-effect$op
      if(is.null(op) || !op %in% c("set","inc","append","remove_at")) .ra_fail("Unknown effect")
      field<-if(op=="inc") "by" else if(op=="remove_at") "index" else "value"
      .ra_keys(effect,c("op","path",field)); path<-effect$path
      if(.ra_kind(path)!="string" || !startsWith(path,"/") || grepl("..",path,fixed=TRUE) || grepl("~",path,fixed=TRUE) || grepl("\\",path,fixed=TRUE) || endsWith(path,"/")) .ra_fail("Unsafe effect path")
      parts<-strsplit(substring(path,2L),"/",fixed=TRUE)[[1]]
      target<-.ra_spec_path(parts,sf,action$params)
      if(op=="inc" && !target$type %in% c("integer","number")) .ra_fail("Increment requires numeric field")
      if(op %in% c("append","remove_at") && target$type!="array") .ra_fail("Array effect requires array")
      if(any(vapply(paths,function(p) .ra_overlap(parts,p),logical(1)))) .ra_fail("Overlapping effects")
      paths[[length(paths)+1L]]<-parts; .ra_expr_check(effect[[field]],budget=budget)
      .ra_references(effect[[field]],sf,action$params)
      .ra_expr_types(effect[[field]],sf,action$params)
    }
  }
  if(.ra_kind(pack$outcomes)!="array" || length(pack$outcomes)>512) .ra_fail("Invalid outcomes")
  for(out in pack$outcomes) {
    .ra_keys(out,c("when","result_kind","winners"),"reason")
    if(!out$result_kind %in% c("win","draw","tie","ranking","disqualification","forfeit","no_contest","technical","other")) .ra_fail("Invalid outcome kind")
    if(.ra_kind(out$winners)!="array") .ra_fail("Invalid winners")
    constants<-Filter(function(w) .ra_kind(w)!="object",out$winners)
    if(any(vapply(constants,function(w) .ra_kind(w)!="number" || w!=floor(w) || w<0 || w>=hi,logical(1))) || anyDuplicated(unlist(constants))) .ra_fail("Invalid winners")
    .ra_expr_check(out$when,budget=budget)
    .ra_references(out$when,sf,.ra_object())
    if(!identical(.ra_expr_types(out$when,sf,.ra_object()),"boolean")) .ra_fail("Guard must produce boolean")
    for(expr in out$winners) { .ra_expr_check(expr,budget=budget); .ra_references(expr,sf,.ra_object()); if(!identical(.ra_expr_types(expr,sf,.ra_object()),"number")) .ra_fail("Invalid winners") }
  }
  if(.ra_kind(pack$provenance)!="object" || any(vapply(c("basis","verification_status"),function(k) .ra_kind(pack$provenance[[k]])!="string",logical(1)))) .ra_fail("Missing provenance")
  pack
}
.ra_predicate <- function(expr,state,params,budget) {
  value<-.ra_expr(expr,state,params,budget)
  if(.ra_kind(value)!="boolean") .ra_fail("Guard must produce boolean")
  value
}
.ra_set <- function(data,parts,value) {
  key<-if(.ra_kind(data)=="array") as.numeric(parts[1])+1L else parts[1]
  if(length(parts)==1L) data[key]<-list(value)
  else data[key]<-list(.ra_set(data[[key]],parts[-1L],value))
  data
}
.ra_transition <- function(data,action,params,budget) {
  plan<-list()
  for(effect in action$effects) {
    parts<-strsplit(substring(effect$path,2L),"/",fixed=TRUE)[[1]]
    for(i in seq_along(parts)) if(startsWith(parts[i],"{")) {
      v<-params[[substr(parts[i],2L,nchar(parts[i])-1L)]]
      if(.ra_kind(v)!="number" || v!=floor(v) || v<0) .ra_fail("Invalid index")
      parts[i]<-format(v,scientific=FALSE,trim=TRUE)
    }
    if(any(vapply(plan,function(p) .ra_overlap(parts,p$parts),logical(1)))) .ra_fail("Overlapping resolved effects")
    old<-.ra_resolve(data,parts); op<-effect$op
    field<-if(op=="inc") "by" else if(op=="remove_at") "index" else "value"
    value<-.ra_expr(effect[[field]],data,params,budget)
    if(op=="inc") value<-.ra_expr(list(op="add",args=list(old,value)),.ra_object(),.ra_object(),budget)
    else if(op=="append") value<-c(old,list(value))
    else if(op=="remove_at") {
      if(.ra_kind(value)!="number" || value!=floor(value) || value<0 || value>=length(old)) .ra_fail("Invalid remove index")
      old[value+1L]<-NULL; value<-old
    }
    plan[[length(plan)+1L]]<-list(parts=parts,value=value)
  }
  result<-data
  for(p in plan) result<-.ra_set(result,p$parts,p$value)
  result
}
.ra_references <- function(expr,state_fields,params) {
  if(.ra_kind(expr)!="object") return(invisible(NULL))
  if("ref" %in% names(expr)) {
    parts<-strsplit(expr$ref,".",fixed=TRUE)[[1]]
    .ra_spec_path(parts[-1L],if(parts[1]=="state") state_fields else params,.ra_object())
  } else for(arg in expr$args) .ra_references(arg,state_fields,params)
  invisible(NULL)
}
.ra_expr_types <- function(expr,state_fields,params) {
  if(.ra_kind(expr)!="object") return(.ra_kind(expr))
  if("ref" %in% names(expr)) {
    parts<-strsplit(expr$ref,".",fixed=TRUE)[[1]]
    spec<-.ra_spec_path(parts[-1L],if(parts[1]=="state") state_fields else params,.ra_object())
    types<-if(spec$type=="enum") unique(vapply(spec$values,.ra_kind,character(1))) else if(spec$type=="integer") "number" else spec$type
    if(isTRUE(spec$nullable)) types<-union(types,"null")
    return(types)
  }
  args<-lapply(expr$args,function(a) .ra_expr_types(a,state_fields,params)); op<-expr$op
  need<-function(index,allowed) { if(!length(args[[index]]) || length(setdiff(args[[index]],allowed))) .ra_fail("Expression operand type mismatch") }
  if(op %in% c("and","or","not")) { for(i in seq_along(args)) need(i,"boolean"); return("boolean") }
  if(op %in% c("eq","ne")) {
    if(!length(intersect(args[[1]],args[[2]]))) .ra_fail("Expression operand type mismatch")
    return("boolean")
  }
  if(op=="in") { need(2,"array"); return("boolean") }
  if(op=="len") { need(1,c("array","object","string")); return("number") }
  need(1,"number"); need(2,"number")
  if(op %in% c("lt","lte","gt","gte")) "boolean" else "number"
}
