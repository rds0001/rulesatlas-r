.ra_cross_country_validate<-function(d) {
 base<-d;for(i in seq_along(base$runners)) base$runners[[i]]$warnings<-0L
 .ra_tr_validate(base)
 for(r in d$runners) {
  if(!r$started && (r$checkpoints_completed>0 || !is.null(r$time_us) || r$penalty_us>0 || r$status %in% c("finished","dnf","mp","ot"))) .ra_fail("An unstarted skier cannot have race observations",code="INVALID_STATE")
  if(r$started && r$status %in% c("ready","dns")) .ra_fail("A started skier cannot be ready or DNS",code="INVALID_STATE")
  if(r$warnings==2 && r$status!="dq") .ra_fail("Two written reprimands require disqualification",code="INVALID_STATE")
  for(k in c("time_us","penalty_us")) if(!is.null(r[[k]]) && r[[k]]%%100000!=0) .ra_fail("Classification times require tenth-second precision",code="INVALID_STATE")
 };invisible(TRUE)
}
.ra_cross_country_apply<-function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;ix<-list(type="integer",minimum=0,maximum=7)
 if(k %in% c("start_participant","written_reprimand","correct_reprimands")) {
  schema<-list(participant=ix);if(k=="correct_reprimands") schema$warnings<-list(type="integer",minimum=0,maximum=2)
  .ra_fields(p,schema,"/params");i<-p$participant+1L;r<-d$runners[[i]]
  if(k=="start_participant") {
   if(d$phase!="racing" || r$status!="running" || r$started) .ra_fail("Start an eligible skier once after opening the race")
   r$started<-TRUE
  } else if(k=="written_reprimand") {
   if(!(d$phase %in% c("ready","racing","review")) || r$status %in% c("dns","dq")) .ra_fail("Written reprimand is unavailable")
   r$warnings<-r$warnings+1L
   if(r$warnings==2) {r$status<-"dq";r["time_us"]<-list(NULL)}
  } else {
   if(d$phase!="review") .ra_fail("Correct written reprimands during review")
   r$warnings<-p$warnings
   if(r$warnings==2) {r$status<-"dq";r["time_us"]<-list(NULL)}
  }
  d$runners[[i]]<-r
  if(!any(vapply(d$runners,function(x) x$status %in% c("ready","running"),logical(1)))) d$phase<-"review"
  return(d)
 }
 if(k=="warn") .ra_fail("Use the written reprimand event")
 if(k %in% c("visit_control","record_finish","assess_penalty","correct_penalty")) {
  types<-.ra_tr_legal(state)$action_types;found<-Filter(function(a) a$type==k,types)
  if(length(found)==0) .ra_fail("Event is unavailable in this phase")
  .ra_fields(p,found[[1]]$params_schema,"/params")
  if(!d$runners[[p$participant+1L]]$started) .ra_fail("This observation requires an individual start")
 }
 if(k=="amend_result") {
  .ra_fields(p,list(participant=ix,status=list(type="enum",values=as.list(c("finished","dns","dnf","dq","mp","ot"))),time_us=list(type="integer",minimum=1,maximum=1e12,nullable=TRUE),warnings=list(type="integer",minimum=0,maximum=2)),"/params")
  if(p$warnings!=d$runners[[p$participant+1L]]$warnings) .ra_fail("Correct reprimands explicitly before amending the result")
 }
 if(k %in% c("record_finish","assess_penalty","correct_penalty","amend_result")) for(key in c("time_us","penalty_us")) if(!is.null(p[[key]]) && p[[key]]%%100000!=0) .ra_fail("Classification times require tenth-second precision")
 result<-.ra_tr_apply(state,action)
 if(k=="withdraw" && !d$runners[[p$participant+1L]]$started) result$runners[[p$participant+1L]]$status<-"dns"
 result
}
.ra_cross_country_legal<-function(state) {
 result<-.ra_tr_legal(state);d<-state$data;ix<-list(type="integer",minimum=0,maximum=7);specs<-list()
 if(d$phase=="racing") specs$start_participant<-list(participant=ix)
 if(d$phase %in% c("ready","racing","review")) specs$written_reprimand<-list(participant=ix)
 if(d$phase=="review") specs$correct_reprimands<-list(participant=ix,warnings=list(type="integer",minimum=0,maximum=2))
 for(k in names(specs)) result$action_types[[length(result$action_types)+1L]]<-list(type=k,params_schema=specs[[k]],preconditions=list("Individual start, eligibility and written reprimand count must permit the event."),external_observations=list("Official start or disciplinary ruling."))
 result$constraints$time_quantum_us<-100000;result$constraints$started<-lapply(d$runners,function(r) r$started);result$constraints$written_reprimands<-lapply(d$runners,function(r) r$warnings);result
}
.ra_cross_country_outcome<-function(state,pack) .ra_tr_outcome(state,pack)
