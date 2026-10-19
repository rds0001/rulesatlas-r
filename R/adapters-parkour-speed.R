.ra_parkour_validate<-function(d) .ra_tr_validate(d)
.ra_parkour_outcome<-function(state,pack) .ra_tr_outcome(state,pack)
.ra_parkour_apply<-function(state,action) {
 if(action$type!="restart_attempt") return(.ra_tr_apply(state,action))
 .ra_fields(action$params,list(participant=list(type="integer",minimum=0,maximum=7)),"/params");d<-state$data;i<-action$params$participant+1L;r<-d$runners[[i]]
 if(d$phase!="racing" || r$status!="running") .ra_fail("Only an unresolved active attempt can be officially restarted")
 d$runners[[i]]$checkpoints_completed<-0L;d$runners[[i]]["last_checkpoint_time"]<-list(NULL);d
}
.ra_parkour_legal<-function(state) {
 result<-.ra_tr_legal(state)
 if(state$data$phase=="racing") result$action_types[[length(result$action_types)+1L]]<-list(type="restart_attempt",params_schema=list(participant=list(type="integer",minimum=0,maximum=7)),preconditions=list("The attempt is active and has no official finish or exclusion."),external_observations=list("Official authorization to void and restart an interrupted attempt."))
 result
}
