.ra_categories_names <- list(type="array",minItems=1,maxItems=12,items=list(type="string",minLength=1,maxLength=64))
.ra_categories_letters <- list(type="array",minItems=1,maxItems=26,items=list(type="string",minLength=1,maxLength=1))
.ra_categories_finish <- function(d) {
  if(d$phase=="judging" && all(unlist(d$rounds[[d$current+1L]]$judgments)!=-1)) {d$current<-d$current+1L;d$phase<-if(d$current==length(d$letters)) "finished" else "writing"}
  d
}
.ra_categories_word <- function(word,letter) grepl("^[A-Z]+([ -][A-Z]+)*$",word) && substr(word,1,1)==letter
.ra_categories_validate <- function(d) {
  if(!d$configured) {if(length(d$categories)+length(d$letters)+length(d$rounds)>0 || d$current!=0 || d$phase!="writing") .ra_fail("Unconfigured game contains data",code="INVALID_STATE");return(invisible(NULL))}
  n<-length(d$categories);total<-length(d$letters)
  if(n==0 || total==0 || anyDuplicated(unlist(d$categories)) || anyDuplicated(unlist(d$letters)) || any(!grepl("^[A-Z]$",unlist(d$letters)))) .ra_fail("Categories and letters must be distinct and valid",code="INVALID_STATE")
  if(length(d$rounds)!=total || d$current>total || (d$phase=="finished")!=(d$current==total)) .ra_fail("Round and phase mismatch",code="INVALID_STATE")
  for(index in seq_along(d$rounds)) {
    r<-d$rounds[[index]]
    if(any(lengths(c(r$answers,r$judgments))!=n)) .ra_fail("Round category count mismatch",code="INVALID_STATE")
    for(p in 1:2) for(c in seq_len(n)) {
      word<-r$answers[[p]][[c]];judgment<-r$judgments[[p]][[c]]
      if(nchar(word)>0 && !.ra_categories_word(word,d$letters[[index]])) .ra_fail("Answer must start with the round letter",code="INVALID_STATE")
      if(nchar(word)==0 && judgment==1) .ra_fail("Empty answer cannot be valid",code="INVALID_STATE")
      if(index-1L<d$current && judgment==-1) .ra_fail("Completed round has an unjudged answer",code="INVALID_STATE")
      if((index-1L>d$current && nchar(word)>0) || ((index-1L>d$current || (index-1L==d$current && d$phase=="writing")) && judgment!=-1)) .ra_fail("Future or open round contains adjudication",code="INVALID_STATE")
    }
    if(index-1L==d$current && d$phase=="judging" && all(unlist(r$judgments)!=-1)) .ra_fail("Completed review must advance",code="INVALID_STATE")
  }
}
.ra_categories_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(!d$configured) {
    if(kind!="configure") .ra_fail("Supply categories and round letters first")
    .ra_fields(p,list(categories=.ra_categories_names,letters=.ra_categories_letters),"/params")
    d$configured<-TRUE;d$categories<-p$categories;d$letters<-p$letters
    d$rounds<-lapply(p$letters,function(l) list(answers=lapply(1:2,function(i) as.list(rep("",length(p$categories)))),judgments=lapply(1:2,function(i) as.list(rep(-1L,length(p$categories))))));return(d)
  }
  r<-d$rounds[[d$current+1L]]
  if(kind=="write") {
    if(d$phase!="writing") .ra_fail("Writing has ended")
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1),category=list(type="integer",minimum=0,maximum=length(d$categories)-1),word=list(type="string",minLength=0,maxLength=64)),"/params");t<-p$player+1L;c<-p$category+1L;word<-p$word
    if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
    if(nchar(word)>0 && !.ra_categories_word(word,d$letters[[d$current+1L]])) .ra_fail("Answer must start with the round letter")
    if(r$answers[[t]][[c]]==word) .ra_fail("Answer is unchanged")
    r$answers[[t]][[c]]<-word;d$rounds[[d$current+1L]]<-r;return(d)
  }
  if(kind %in% c("stop","time_expired")) {
    if(d$phase!="writing") .ra_fail("Writing has already ended")
    .ra_fields(p,if(kind=="stop") list(player=list(type="integer",minimum=0,maximum=1)) else structure(list(),names=character()),"/params")
    if(kind=="stop") {
      t<-p$player+1L
      if(any(unlist(r$answers[[t]])=="")) .ra_fail("Stopping player must fill every category")
      if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
    }
    r$judgments<-lapply(r$answers,function(row) lapply(row,function(w) if(nchar(w)>0) -1L else 0L));d$rounds[[d$current+1L]]<-r;d$phase<-"judging";return(.ra_categories_finish(d))
  }
  if(kind!="judge" || d$phase!="judging") .ra_fail("Action is not permitted in this phase")
  .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1),category=list(type="integer",minimum=0,maximum=length(d$categories)-1),valid=list(type="boolean")),"/params");t<-p$player+1L;c<-p$category+1L
  if(nchar(r$answers[[t]][[c]])==0) .ra_fail("Empty answer is already invalid")
  r$judgments[[t]][[c]]<-as.integer(p$valid);d$rounds[[d$current+1L]]<-r;.ra_categories_finish(d)
}
.ra_categories_legal <- function(state) {
  d<-state$data
  if(!d$configured) schemas<-list(configure=list(categories=.ra_categories_names,letters=.ra_categories_letters)) else {
    person<-list(player=list(type="integer",minimum=0,maximum=1));cell<-c(person,list(category=list(type="integer",minimum=0,maximum=length(d$categories)-1)))
    if(d$phase=="writing") schemas<-list(write=c(cell,list(word=list(type="string",minLength=0,maxLength=64))),stop=person,time_expired=structure(list(),names=character())) else schemas<-list(judge=c(cell,list(valid=list(type="boolean"))))
  }
  types<-lapply(names(schemas),function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("The phase, category, starting letter and answer completeness must permit this action."),external_observations=list("Player answers, an agreed time limit or official category-validity judgments.")))
  list(mode="event_types",actions=list(),action_types=types,constraints=list(round=if(d$configured && d$phase!="finished") d$current+1L else NULL,phase=d$phase),completeness=FALSE,reason="external_category_adjudication")
}
.ra_categories_totals <- function(d) {
  scores<-c(0,0)
  for(r in head(d$rounds,d$current)) for(c in seq_along(d$categories)) {
    valid<-which(vapply(r$judgments,function(row) row[[c]]==1,logical(1)))
    if(length(valid)==1) scores[valid]<-scores[valid]+20 else if(length(valid)==2) scores<-scores+if(r$answers[[1]][[c]]==r$answers[[2]][[c]]) 5 else 10
  }
  scores
}
.ra_categories_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";scores<-.ra_categories_totals(d)
  winners<-if(terminal) lapply(which(scores==max(scores)),function(p) state$players[[p]]$id) else list()
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(scores[1]==scores[2]) "draw" else "win"} else "none",winners=winners,losers=if(terminal) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=if(terminal) lapply(1:2,function(p) list(player_id=state$players[[p]]$id,place=1L+sum(scores>scores[p]),score=scores[p])) else list(),reason=if(terminal) "all_rounds_scored" else if(d$configured) "ongoing" else "awaiting_configuration",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
