# Native public API; no Python process, service or foreign interpreter.
.ra_registry <- new.env(parent=emptyenv())
.ra_result <- function(error=NULL) {
  list(valid=is.null(error),scope="structural",issues=if(is.null(error)) list() else list(list(code=error$code,path=if(is.null(error$path)) "" else error$path,message=error$message,severity="error")))
}
validate_rules <- function(rules_or_path) {
  tryCatch({.ra_pack(.ra_load(rules_or_path)); .ra_result()},
    ruleatlas_error=function(e) .ra_result(e),
    error=function(e) .ra_result(list(code="INVALID_RULEPACK",message=conditionMessage(e),path=NULL)))
}
.ra_key <- function(pack) jsonlite::toJSON(list(pack$game_id,pack$ruleset_id,pack$ruleset_version),auto_unbox=TRUE)
register_rules <- function(rules_or_path,overwrite=FALSE) {
  if(.ra_kind(overwrite)!="boolean") .ra_fail("overwrite must be boolean")
  pack<-tryCatch(.ra_pack(.ra_load(rules_or_path)),ruleatlas_error=function(e) stop(e),error=function(e) .ra_fail(conditionMessage(e)))
  key<-.ra_key(pack)
  if(exists(key,envir=.ra_registry,inherits=FALSE)) {
    if(.ra_equal(get(key,envir=.ra_registry),pack)) return(pack)
    .ra_fail("Version is immutable; register a new version",code="RULESET_CONFLICT")
  }
  assign(key,pack,envir=.ra_registry); pack
}
.ra_select <- function(game_id,ruleset_version=NULL,ruleset_id=NULL) {
  choices<-Filter(function(p) identical(p$game_id,game_id),c(as.list(.ra_registry),unname(.ra_builtins)))
  if(!length(choices)) .ra_fail("Unknown game",code="UNKNOWN_GAME")
  choices<-Filter(function(p) (is.null(ruleset_version)||identical(p$ruleset_version,ruleset_version)) && (is.null(ruleset_id)||identical(p$ruleset_id,ruleset_id)),choices)
  if(!length(choices)) .ra_fail("Unknown ruleset",code="UNKNOWN_RULESET")
  if(length(choices)!=1L) .ra_fail("Ambiguous ruleset",code="RULESET_CONFLICT")
  choices[[1]]
}
unregister_rules <- function(game_id,ruleset_version=NULL,ruleset_id=NULL) {
  if(game_id %in% names(.ra_builtins)) .ra_fail("Built-in rulesets cannot be removed",code="RULESET_CONFLICT")
  p<-tryCatch(.ra_select(game_id,ruleset_version,ruleset_id),ruleatlas_error=function(e) {
    if(e$code %in% c("UNKNOWN_GAME","UNKNOWN_RULESET")) return(NULL)
    stop(e)
  })
  if(is.null(p)) return(FALSE)
  rm(list=.ra_key(p),envir=.ra_registry); TRUE
}
list_games <- function(include_candidates=FALSE) {
  if(.ra_kind(include_candidates)!="boolean") .ra_fail("include_candidates must be boolean")
  packs<-as.list(.ra_registry); if(length(packs)) packs<-packs[order(names(packs),method="radix")]
  result<-unname(lapply(packs,function(p) list(game_id=p$game_id,name=p$name,ruleset_id=p$ruleset_id,ruleset_version=p$ruleset_version,implementation_status="user_defined")))
  accepted<-Filter(function(p) identical(p$profile_record$implementation_status,"accepted"),unname(.ra_builtins))
  result<-c(result,lapply(accepted,function(p) c(p$profile_record,list(game_id=p$game_id))))
  if(include_candidates) {
    path<-system.file("extdata/catalog/target_profiles.json",package="ruleatlas")
    if(!nzchar(path)) .ra_fail("Installed catalog missing",code="INTERNAL_ERROR")
    catalog<-jsonlite::fromJSON(path,simplifyVector=FALSE)
    entries<-Filter(function(p) !p$id %in% vapply(accepted,function(g) g$game_id,character(1)),catalog$entries)
    result<-c(result,lapply(entries,function(p) list(game_id=p$id,name=p$name_de,implementation_status="candidate",supported_actions=list(),data_requirements=list(),source_status="needs_verification")))
  }
  result
}
get_game <- function(game_id,ruleset_version=NULL,variant=NULL,ruleset_id=NULL) {
  if(!is.null(variant) && (.ra_kind(variant)!="object" || length(variant))) .ra_fail("Unknown variant",code="UNKNOWN_RULESET")
  p<-.ra_select(game_id,ruleset_version,ruleset_id)
  if(!is.null(p$profile_record)) p$profile_record else p
}
get_capabilities <- function(game_id,ruleset_version=NULL,ruleset_id=NULL) {
  p<-.ra_select(game_id,ruleset_version,ruleset_id)
  if(!is.null(p$adapter)) return(p$profile_record$capabilities)
  list(new_state=TRUE,validate_state=TRUE,legal_actions="bounded_enumeration_or_event_types",apply_action=TRUE,outcome=TRUE,validation_scope="structural",max_enumeration=512L)
}
new_state <- function(game_id,players=NULL,variant=NULL,ruleset_version=NULL,ruleset_id=NULL) {
  get_game(game_id,ruleset_version,variant,ruleset_id)
  p<-.ra_select(game_id,ruleset_version,ruleset_id)
  if(is.null(players)) .ra_fail("Players required",code="MISSING_INFORMATION")
  s<-list(game_id=game_id,ruleset_id=p$ruleset_id,ruleset_version=p$ruleset_version,variant=.ra_object(),players=players,data=p$state$initial)
  .ra_require(s); s
}
.ra_state <- function(state) {
  .ra_bounded(state)
  req<-c("game_id","ruleset_id","ruleset_version","variant","players","data")
  if(.ra_kind(state)!="object" || length(setdiff(req,names(state))) || length(setdiff(names(state),c(req,"metadata")))) .ra_fail("State envelope mismatch",code="INVALID_STATE")
  for(k in c("game_id","ruleset_id","ruleset_version")) if(.ra_kind(state[[k]])!="string" || !nzchar(state[[k]])) .ra_fail("Invalid state identity",code="INVALID_STATE")
  p<-.ra_select(state$game_id,state$ruleset_version,state$ruleset_id)
  if(.ra_kind(state$variant)!="object" || length(state$variant)) .ra_fail("Unknown variant",code="UNKNOWN_RULESET")
  if("metadata" %in% names(state) && .ra_kind(state$metadata)!="object") .ra_fail("Metadata must be object",code="INVALID_STATE")
  ps<-state$players
  if(.ra_kind(ps)!="array" || length(ps)<p$participants$min || length(ps)>p$participants$max) .ra_fail("Participant count",code="INVALID_STATE")
  if(any(vapply(ps,function(x) .ra_kind(x)!="object" || .ra_kind(x$id)!="string" || !nzchar(x$id),logical(1)))) .ra_fail("Invalid participant",code="INVALID_STATE")
  if(anyDuplicated(vapply(ps,function(x) x$id,character(1)))) .ra_fail("Duplicate participant",code="INVALID_STATE")
  tryCatch(.ra_fields(state$data,p$state$fields,"/data"),ruleatlas_error=function(e) .ra_fail(e$message,e$path,"INVALID_STATE"))
  if(!is.null(p$adapter)) .ra_adapter(p,"validate",state$data)
  p
}
validate_state <- function(state) {
  tryCatch({p<-.ra_state(state); .ra_outcome(state,p,.ra_budget()); result<-.ra_result(); if(!is.null(p$adapter)) result$scope<-p$profile_record$capabilities$validation_scope; result},
    ruleatlas_error=function(e) .ra_result(e),
    error=function(e) .ra_result(list(code="INVALID_STATE",message=conditionMessage(e),path=NULL)))
}
.ra_require <- function(state,budget=.ra_budget()) {
  p<-.ra_state(state); .ra_outcome(state,p,budget); p
}

.ra_outcome <- function(state,pack,budget) {
  if(!is.null(pack$adapter)) return(.ra_adapter(pack,"outcome",state,pack))
  matches<-list()
  for(i in seq_along(pack$outcomes)) {
    rule<-pack$outcomes[[i]]
    if(.ra_predicate(rule$when,state$data,.ra_object(),budget)) {
      ws<-lapply(rule$winners,function(e) .ra_expr(e,state$data,.ra_object(),budget))
      if(any(vapply(ws,function(w) .ra_kind(w)!="number" || w!=floor(w) || w<0 || w>=length(state$players),logical(1))) || anyDuplicated(unlist(ws))) .ra_fail("Invalid winners",code="INVALID_STATE")
      winners<-lapply(ws,function(w) state$players[[w+1L]]$id)
      losers<-if(length(winners)) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list()
      matches[[length(matches)+1L]]<-list(status="terminal",result_kind=rule$result_kind,winners=winners,losers=losers,placements=list(),reason=.ra_get(rule,"reason","terminal"),evidence_required=list(),rule_id=paste0(pack$ruleset_id,"@",pack$ruleset_version,"#outcome/",i-1L))
    }
  }
  if(length(matches)>1L) .ra_fail("Conflicting outcome conditions",code="INVALID_STATE")
  if(length(matches)) return(matches[[1]])
  list(status="ongoing",result_kind="none",winners=list(),losers=list(),placements=list(),reason="ongoing",evidence_required=list(),rule_id=NULL)
}
outcome <- function(state) { budget<-.ra_budget(); p<-.ra_require(state,budget); .ra_outcome(state,p,budget) }
apply_action <- function(state,action) .ra_apply(state,action,.ra_budget())
.ra_apply <- function(state,action,budget) {
  p<-.ra_require(state,budget)
  if(.ra_outcome(state,p,budget)$status=="terminal") .ra_fail("Terminal state",code="ILLEGAL_ACTION")
  tryCatch({
    .ra_bounded(action)
    if(.ra_kind(action)!="object" || !all(c("type","params") %in% names(action)) || length(setdiff(names(action),c("type","params","actor","metadata")))) .ra_fail("Action envelope mismatch")
    if("metadata" %in% names(action) && .ra_kind(action$metadata)!="object") .ra_fail("Invalid metadata")
    if(!is.null(action$actor) && !action$actor %in% vapply(state$players,function(p) p$id,character(1))) .ra_fail("Unknown actor")
    if(!is.null(p$adapter)) {
      result<-state; result$data<-.ra_adapter(p,"apply",state,action)
      .ra_require(result,budget); return(result)
    }
    rules<-Filter(function(r) identical(r$type,action$type),p$actions)
    if(!length(rules)) .ra_fail("Unknown action")
    rule<-rules[[1]]; params<-action$params; .ra_fields(params,rule$params,"/params")
    if(!.ra_predicate(rule$when,state$data,params,budget)) .ra_fail("Guard rejected")
    result<-state; result$data<-.ra_transition(state$data,rule,params,budget)
    .ra_require(result,budget); result
  },ruleatlas_error=function(e) {
    if(e$code=="RESOURCE_LIMIT") stop(e)
    .ra_fail(e$message,e$path,"ILLEGAL_ACTION")
  })
}
.ra_domain <- function(spec) {
  if(isTRUE(spec$nullable)) return(NULL)
  if(spec$type=="boolean") return(list(FALSE,TRUE))
  if(spec$type=="enum") return(spec$values)
  if(spec$type=="integer" && all(c("minimum","maximum") %in% names(spec))) {
    lo<-ceiling(spec$minimum); hi<-floor(spec$maximum)
    if(hi<lo) return(list())
    if(hi-lo<512) return(as.list(seq(lo,hi)))
  }
  NULL
}
.ra_canonical <- function(x) {
  if(.ra_kind(x)=="object") { x<-x[order(names(x),method="radix")]; x<-lapply(x,.ra_canonical) }
  else if(.ra_kind(x)=="array") x<-lapply(x,.ra_canonical)
  x
}
.ra_json <- function(x) as.character(jsonlite::toJSON(.ra_canonical(x),auto_unbox=TRUE,null="null",digits=NA))
legal_actions <- function(state) {
  budget<-.ra_budget(); p<-.ra_require(state,budget)
  result<-list(mode="enumerated",actions=list(),action_types=list(),constraints=.ra_object(),completeness=TRUE,reason="complete")
  if(.ra_outcome(state,p,budget)$status=="terminal") { result$reason<-"terminal"; return(result) }
  if(!is.null(p$adapter)) { legal<-.ra_adapter(p,"legal",state); if(.ra_kind(legal)=="object") return(legal); result$actions<-legal; return(result) }
  domains<-lapply(p$actions,function(r) lapply(r$params,.ra_domain))
  sizes<-vapply(domains,function(ds) prod(vapply(ds,function(d) if(is.null(d)) 513 else length(d),numeric(1))),numeric(1))
  if(sum(sizes)>512 || any(vapply(domains,function(ds) any(vapply(ds,is.null,logical(1))),logical(1)))) {
    result$mode<-"event_types"; result$completeness<-FALSE; result$reason<-"parameter_domain_not_enumerable"
    result$action_types<-lapply(p$actions,function(r) list(type=r$type,params_schema=r$params,preconditions=list(.ra_json(r$when))))
    return(result)
  }
  for(i in seq_along(p$actions)) {
    rule<-p$actions[[i]]; ds<-domains[[i]]; combos<-list(.ra_object())
    for(n in names(ds)) {
      next_combos<-list()
      for(combo in combos) for(v in ds[[n]]) {
        item<-combo; item[n]<-list(v); next_combos[[length(next_combos)+1L]]<-item
      }
      combos<-next_combos
    }
    for(params in combos) {
      action<-list(type=rule$type,params=params)
      good<-tryCatch({.ra_apply(state,action,budget); TRUE},ruleatlas_error=function(e) {
        if(e$code=="ILLEGAL_ACTION") return(FALSE)
        stop(e)
      })
      if(good) result$actions[[length(result$actions)+1L]]<-action
    }
  }
  if(length(result$actions)) result$actions<-result$actions[order(vapply(result$actions,.ra_json,character(1)),method="radix")]
  result
}
