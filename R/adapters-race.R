.ra_race_index <- list(type="integer",minimum=0,maximum=7)
.ra_race_time <- list(type="integer",minimum=1,maximum=1000000000000,nullable=TRUE)
.ra_race_controls <- function(d,config) if(isTRUE(config$route_configurable)) d$route else if(!is.null(config$controls)) config$controls else list()
.ra_race_statuses <- function(config) c("finished","dns","dnf","dq",if(isTRUE(config$route_configurable)) c("mp","ot") else character())
.ra_race_validate <- function(d,config) {
  phase<-d$phase;controls<-.ra_race_controls(d,config)
  if(isTRUE(config$route_configurable)) {
    if(anyDuplicated(unlist(controls))) .ra_fail("Route controls must be unique",code="INVALID_STATE")
    if((phase=="racing" || any(vapply(d$runners,function(r) r$status=="finished",logical(1)))) && (!length(controls) || !d$route_locked)) .ra_fail("Race route is not configured",code="INVALID_STATE")
  }
  for(r in d$runners) {
    if((r$status=="finished")!=!is.null(r$time_us)) .ra_fail("Finish status and time mismatch",code="INVALID_STATE")
    if(length(config$controls)>0 || isTRUE(config$route_configurable)) {
      if(r$status=="ready" && r$checkpoints_completed!=0) .ra_fail("Checkpoints exist before the start",code="INVALID_STATE")
      if(r$checkpoints_completed>length(controls) || ((r$checkpoints_completed==0)!=is.null(r$last_checkpoint_time))) .ra_fail("Checkpoint state mismatch",code="INVALID_STATE")
      if(r$status=="finished" && (r$checkpoints_completed!=length(controls) || r$time_us<r$last_checkpoint_time)) .ra_fail("Finish requires all checkpoints in order",code="INVALID_STATE")
    }
    if(isTRUE(config$route_configurable) && r$status=="finished" && !is.null(d$time_limit_us) && r$time_us>d$time_limit_us) .ra_fail("Finish exceeds the configured time limit",code="INVALID_STATE")
    if(!is.null(r$time_us) && r$time_us%%config$time_quantum_us!=0) .ra_fail("Time precision does not match the event",code="INVALID_STATE")
    if(isTRUE(config$walking)) {
      reds<-r$red_cards;yellows<-r$yellow_paddles
      if(anyDuplicated(vapply(reds,function(c) c$judge,numeric(1))) || anyDuplicated(vapply(yellows,function(c) paste(c$judge,c$offence),character(1)))) .ra_fail("Duplicate walking judge card or paddle",code="INVALID_STATE")
      if(length(reds)>=3 && r$status!="dq") .ra_fail("Three independent red cards require disqualification",code="INVALID_STATE")
      if(r$status %in% c("ready","dns") && (length(reds) || length(yellows))) .ra_fail("Walking judgments require a started participant",code="INVALID_STATE")
    }
    if(r$warnings==2 && r$status!="dq") .ra_fail("Two warnings require disqualification",code="INVALID_STATE")
    if(!config$conduct_warnings && r$warnings!=0) .ra_fail("Warning escalation is not part of this variant",code="INVALID_STATE")
  }
  states<-vapply(d$runners,function(r) r$status,character(1))
  if(("ready" %in% states && phase!="ready") || ("running" %in% states && phase!="racing")) .ra_fail("Participant phase mismatch",code="INVALID_STATE")
  if(phase=="ready" && any(states %in% c("finished","dnf"))) .ra_fail("Results exist before a valid start",code="INVALID_STATE")
  if(phase %in% c("ready","racing") && !any(states %in% c("ready","running"))) .ra_fail("Resolved race must enter review",code="INVALID_STATE")
  if(phase %in% c("review","finished") && any(states %in% c("ready","running"))) .ra_fail("Unresolved participant result",code="INVALID_STATE")
}
.ra_race_settle <- function(d) {
  if(!any(vapply(d$runners,function(r) r$status %in% c("ready","running"),logical(1)))) d$phase<-"review"
  d
}
.ra_race_apply <- function(state,action,config) {
  .ra_race_index<-list(type="integer",minimum=0,maximum=length(state$data$runners)-1L)
  d<-state$data;p<-action$params;kind<-action$type;phase<-d$phase
  if(kind %in% c("red_card","yellow_paddle","remove_red_card")) {
    specs<-list(participant=.ra_race_index,judge=list(type="integer",minimum=0,maximum=8))
    if(kind!="remove_red_card") specs$offence<-list(type="string",enum=list("loss_of_contact","bent_knee"))
    .ra_fields(p,specs,"/params")
    if(!isTRUE(config$walking)) .ra_fail("Walking judgments are not part of this profile")
    i<-p$participant+1L;r<-d$runners[[i]]
    if(kind=="remove_red_card") {
      if(phase!="review" || !any(vapply(r$red_cards,function(c) c$judge==p$judge,logical(1)))) .ra_fail("Remove an existing red card only during official result review")
      r$red_cards<-Filter(function(c) c$judge!=p$judge,r$red_cards)
    } else {
      if(!phase %in% c("racing","review") || !r$status %in% c("running","finished")) .ra_fail("Walking judgment requires an active or provisionally finished participant")
      card<-p[c("judge","offence")]
      if(kind=="red_card") {
        if(any(vapply(r$red_cards,function(c) c$judge==p$judge,logical(1)))) .ra_fail("Each judge may issue only one red card per participant")
        r$red_cards<-c(r$red_cards,list(card))
        if(length(r$red_cards)==3) {r$status<-"dq";r["time_us"]<-list(NULL)}
      } else {
        if(any(vapply(r$yellow_paddles,function(c) c$judge==p$judge && c$offence==p$offence,logical(1)))) .ra_fail("A judge may show each offence paddle only once per participant")
        r$yellow_paddles<-c(r$yellow_paddles,list(card))
      }
    }
    d$runners[[i]]<-r;d<-.ra_race_settle(d)
  } else if(kind=="configure_route") {
    .ra_fields(p,list(controls=list(type="array",items=list(type="string",minLength=1,maxLength=64),minItems=1,maxItems=128),time_limit_us=.ra_race_time),"/params")
    if(!isTRUE(config$route_configurable) || phase!="ready" || d$route_locked) .ra_fail("Route cannot be configured now")
    d$route<-p$controls;d["time_limit_us"]<-list(p$time_limit_us)
  } else if(kind=="visit_control") {
    time<-.ra_race_time;time$nullable<-FALSE
    .ra_fields(p,list(participant=.ra_race_index,control=list(type="string",minLength=1,maxLength=64),time_us=time),"/params");i<-p$participant+1L;r<-d$runners[[i]];controls<-.ra_race_controls(d,config);index<-if(is.null(r$checkpoints_completed)) 0L else r$checkpoints_completed
    if(phase!="racing" || r$status!="running" || index>=length(controls) || p$control!=controls[[index+1L]]) .ra_fail("Control is not the next required checkpoint")
    if(!is.null(r$last_checkpoint_time) && p$time_us<r$last_checkpoint_time) .ra_fail("Checkpoint times must not go backwards")
    r$checkpoints_completed<-r$checkpoints_completed+1L;r$last_checkpoint_time<-p$time_us;d$runners[[i]]<-r
  } else if(kind=="start") {
    .ra_fields(p,structure(list(),names=character()),"/params")
    if(phase!="ready") .ra_fail("Race cannot start in this phase")
    if(isTRUE(config$route_configurable)) {if(!length(d$route)) .ra_fail("Configure the route first");d$route_locked<-TRUE}
    for(i in seq_along(d$runners)) if(d$runners[[i]]$status=="ready") d$runners[[i]]$status<-"running"
    d$phase<-"racing"
  } else if(kind=="confirm_result") {
    .ra_fields(p,structure(list(),names=character()),"/params")
    if(phase!="review") .ra_fail("Resolve all participant results first")
    d$phase<-"finished"
  } else if(kind=="amend_result") {
    .ra_fields(p,list(participant=.ra_race_index,status=list(type="enum",values=as.list(.ra_race_statuses(config))),time_us=.ra_race_time,warnings=list(type="integer",minimum=0,maximum=2)),"/params")
    if(phase!="review") .ra_fail("Amendments require result review")
    i<-p$participant+1L;r<-d$runners[[i]];r[c("status","time_us","warnings")]<-p[c("status","time_us","warnings")];d$runners[[i]]<-r
  } else if(kind=="record_finish") {
    time<-.ra_race_time;time$nullable<-FALSE
    .ra_fields(p,list(participant=.ra_race_index,time_us=time),"/params");i<-p$participant+1L;r<-d$runners[[i]]
    if(phase!="racing" || r$status!="running") .ra_fail("Participant cannot finish now")
    controls<-.ra_race_controls(d,config)
    if(length(controls) && !is.null(r$last_checkpoint_time) && p$time_us<r$last_checkpoint_time) .ra_fail("Finish time precedes the last checkpoint")
    if(length(controls) && r$checkpoints_completed!=length(controls)) {
      if(!isTRUE(config$route_configurable)) .ra_fail("Complete all relay exchanges before finishing")
      r$status<-"mp";r["time_us"]<-list(NULL)
    } else if(isTRUE(config$route_configurable) && !is.null(d$time_limit_us) && p$time_us>d$time_limit_us) {r$status<-"ot";r["time_us"]<-list(NULL)} else {
      if(length(controls) && p$time_us<r$last_checkpoint_time) .ra_fail("Finish time precedes the last checkpoint")
      r$status<-"finished";r$time_us<-p$time_us
    }
    d$runners[[i]]<-r;d<-.ra_race_settle(d)
  } else if(kind %in% c("withdraw","disqualify","false_start","warn")) {
    .ra_fields(p,list(participant=.ra_race_index),"/params");i<-p$participant+1L;r<-d$runners[[i]]
    if(kind=="withdraw") {
      if(!phase %in% c("ready","racing") || !r$status %in% c("ready","running")) .ra_fail("Participant cannot withdraw now")
      r$status<-if(phase=="ready") "dns" else "dnf"
    } else if(kind=="disqualify") {
      if(r$status=="dq") .ra_fail("Participant is already disqualified")
      r$status<-"dq";r["time_us"]<-list(NULL)
    } else if(kind=="warn") {
      if(!config$conduct_warnings || !phase %in% (if(isTRUE(config$race_warnings)) c("ready","racing") else "ready") || !r$status %in% c("ready","running")) .ra_fail("A conduct warning is unavailable")
      r$warnings<-r$warnings+1L
      if(r$warnings==2) r$status<-"dq"
    } else {
      if(identical(config$false_start_disqualifies,FALSE)) .ra_fail("False-start disqualification is not part of this variant")
      if(!phase %in% c("ready","racing") || !r$status %in% c("ready","running")) .ra_fail("False-start decision is unavailable")
      if(config$recall && any(vapply(d$runners,function(x) x$status=="finished" || (!is.null(x$checkpoints_completed) && x$checkpoints_completed>0),logical(1)))) .ra_fail("A completed finish cannot be recalled by this event")
      if(isTRUE(config$shared_start_warning)) {
        if(d$start_warning) r$status<-"dq" else d$start_warning<-TRUE
      } else if(isTRUE(config$false_start_warning)) {r$warnings<-r$warnings+1L;if(r$warnings==2) r$status<-"dq"} else r$status<-"dq"
      r["time_us"]<-list(NULL)
      if(phase=="racing" && config$recall) {
        for(j in seq_along(d$runners)) {
          if(d$runners[[j]]$status=="running") d$runners[[j]]$status<-"ready" else if(d$runners[[j]]$status=="dnf") d$runners[[j]]$status<-"dns"
        }
        if(r$status=="running") r$status<-"ready"
        d$phase<-"ready"
      }
    }
    d$runners[[i]]<-r;d<-.ra_race_settle(d)
  } else .ra_fail("Unknown action")
  d
}
.ra_race_legal <- function(state,config) {
  .ra_race_index<-list(type="integer",minimum=0,maximum=length(state$data$runners)-1L)
  d<-state$data;phase<-d$phase;schemas<-list();empty<-structure(list(),names=character())
  if(phase=="ready") {
    schemas<-list(start=empty,withdraw=list(participant=.ra_race_index),false_start=list(participant=.ra_race_index))
    if(isTRUE(config$route_configurable) && !d$route_locked) schemas$configure_route<-list(controls=list(type="array",items=list(type="string",minLength=1,maxLength=64),minItems=1,maxItems=128),time_limit_us=.ra_race_time)
    if(config$conduct_warnings) schemas$warn<-list(participant=.ra_race_index)
  } else if(phase=="racing") {
    time<-.ra_race_time;time$nullable<-FALSE
    schemas<-list(record_finish=list(participant=.ra_race_index,time_us=time),withdraw=list(participant=.ra_race_index),false_start=list(participant=.ra_race_index))
  } else if(phase=="review") schemas<-list(confirm_result=empty,amend_result=list(participant=.ra_race_index,status=list(type="enum",values=as.list(.ra_race_statuses(config))),time_us=.ra_race_time,warnings=list(type="integer",minimum=0,maximum=2)))
  if(isTRUE(config$route_configurable) && !length(d$route)) schemas$start<-NULL
  if(identical(config$false_start_disqualifies,FALSE)) schemas$false_start<-NULL
  if(phase=="racing" && isTRUE(config$race_warnings)) schemas$warn<-list(participant=.ra_race_index)
  if(phase=="racing" && length(.ra_race_controls(d,config))) {time<-.ra_race_time;time$nullable<-FALSE;schemas$visit_control<-list(participant=.ra_race_index,control=list(type="string",minLength=1,maxLength=64),time_us=time)}
  if(isTRUE(config$walking)) {
    card<-list(participant=.ra_race_index,judge=list(type="integer",minimum=0,maximum=8),offence=list(type="string",enum=list("loss_of_contact","bent_knee")))
    if(phase %in% c("racing","review")) {schemas$red_card<-card;schemas$yellow_paddle<-card}
    if(phase=="review") schemas$remove_red_card<-list(participant=.ra_race_index,judge=list(type="integer",minimum=0,maximum=8))
  }
  schemas$disqualify<-list(participant=.ra_race_index)
  list(mode="event_types",actions=list(),action_types=lapply(names(schemas),function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("Participant status and race phase must permit this decision."),external_observations=list("Official start, eligibility, timing or result-review decision."))),constraints=list(phase=phase,time_quantum_us=config$time_quantum_us,statuses=lapply(d$runners,function(r) r$status)),completeness=FALSE,reason="externally_adjudicated_race_events")
}
.ra_race_outcome <- function(state,pack,config) {
  d<-state$data;terminal<-d$phase=="finished";indices<-if(terminal) which(vapply(d$runners,function(r) r$status=="finished",logical(1))) else integer()
  times<-vapply(indices,function(i) d$runners[[i]]$time_us,numeric(1));ord<-order(times,indices);indices<-indices[ord];times<-times[ord]
  winners<-if(length(indices)) lapply(indices[times==times[1]],function(i) state$players[[i]]$id) else list()
  placements<-lapply(seq_along(indices),function(k) list(player_id=state$players[[indices[k]]]$id,place=1L+sum(times<times[k]),time_us=times[k]))
  list(status=if(terminal) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(terminal) {if(length(indices)) "ranking" else "no_contest"} else "none",winners=winners,losers=if(length(winners)) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=placements,reason=if(terminal) "results_confirmed" else if(d$phase=="review") "result_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official result confirmation.") else list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
