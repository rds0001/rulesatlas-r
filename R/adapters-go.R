.ra_go_neighbors <- function(i) {
  r<-i%/%9;c<-i%%9;c(i-9,i+9,i-1,i+1)[c(r>0,r<8,c>0,c<8)]
}
.ra_go_group <- function(b,i) {
  seen<-i;todo<-i
  while(length(todo)) {
    k<-todo[1];todo<-todo[-1]
    for(j in .ra_go_neighbors(k)) if(b[[j+1L]]==b[[i+1L]] && !j %in% seen) {seen<-c(seen,j);todo<-c(todo,j)}
  }
  seen
}
.ra_go_liberties <- function(b,g) {
  n<-unique(unlist(lapply(g,.ra_go_neighbors)));n[vapply(n,function(j) b[[j+1L]]==-1,logical(1))]
}
.ra_go_key <- function(b) paste0(unlist(b)+1,collapse="")
.ra_go_place <- function(d,i) {
  b<-d$board;t<-d$turn
  if(b[[i+1L]]!=-1) .ra_fail("Intersection is occupied")
  b[[i+1L]]<-t
  for(j in .ra_go_neighbors(i)) if(b[[j+1L]]==1-t) {
    g<-.ra_go_group(b,j)
    if(!length(.ra_go_liberties(b,g))) for(k in g) b[[k+1L]]<--1
  }
  if(!length(.ra_go_liberties(b,.ra_go_group(b,i)))) .ra_fail("Suicide is forbidden")
  if(.ra_go_key(b) %in% unlist(d$history)) .ra_fail("Positional superko forbids repeating an earlier board")
  b
}
.ra_go_whole <- function(b,dead) !anyDuplicated(dead) && all(vapply(dead,function(i) b[[i+1L]]!=-1 && all(.ra_go_group(b,i) %in% dead),logical(1)))
.ra_go_validate <- function(d) {
  b<-d$board;history<-unlist(d$history)
  if(!length(history) || tail(history,1)!=.ra_go_key(b) || anyDuplicated(history) || any(!grepl("^[012]{81}$",history))) .ra_fail("Invalid positional superko history",code="INVALID_STATE")
  checked<-integer()
  for(i in 0:80) if(b[[i+1L]]!=-1 && !i %in% checked) {
    g<-.ra_go_group(b,i);checked<-union(checked,g)
    if(!length(.ra_go_liberties(b,g))) .ra_fail("A remaining group has no liberties",code="INVALID_STATE")
  }
  if(is.null(d$resigned) && (d$phase %in% c("scoring","finished"))!=(d$passes==2)) .ra_fail("Two passes are required for area scoring",code="INVALID_STATE")
  if(!is.null(d$resigned) && (d$phase!="finished" || !is.null(d$proposal))) .ra_fail("Invalid resignation state",code="INVALID_STATE")
  if(!is.null(d$proposal) && (!d$phase %in% c("scoring","finished") || !.ra_go_whole(b,unlist(d$proposal$dead)))) .ra_fail("Dead stones must comprise complete occupied groups",code="INVALID_STATE")
  if(d$phase=="finished" && is.null(d$resigned) && is.null(d$proposal)) .ra_fail("Scoring requires an agreed proposal",code="INVALID_STATE")
}
.ra_go_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type;actor<-action$actor;t<-d$turn
  if(kind %in% c("place","pass")) {
    if(d$phase!="playing") .ra_fail("Play is suspended for scoring")
    if(!is.null(actor) && actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
    if(kind=="place") {
      .ra_fields(p,list(cell=list(type="integer",minimum=0,maximum=80)),"/params");d$board<-.ra_go_place(d,p$cell);d$history[[length(d$history)+1L]]<-.ra_go_key(d$board);d$passes<-0L
    } else {
      .ra_fields(p,structure(list(),names=character()),"/params");d$passes<-d$passes+1L;if(d$passes==2) d$phase<-"scoring"
    }
    d$turn<-1L-t;return(d)
  }
  if(!kind %in% c("propose_dead","accept_score","resume","resign")) .ra_fail("Unknown action")
  schema<-list(player=list(type="integer",minimum=0,maximum=1))
  if(kind=="propose_dead") schema$dead<-list(type="array",minItems=0,maxItems=81,items=list(type="integer",minimum=0,maximum=80))
  .ra_fields(p,schema,"/params");player<-p$player
  if(!is.null(actor) && actor!=state$players[[player+1L]]$id) .ra_fail("Wrong actor")
  if(kind=="resign") {d$resigned<-player;d["proposal"]<-list(NULL);d$phase<-"finished";return(d)}
  if(d$phase!="scoring") .ra_fail("Scoring decisions require two consecutive passes")
  if(kind=="propose_dead") {
    dead<-unlist(p$dead)
    if(!.ra_go_whole(d$board,dead)) .ra_fail("Propose whole occupied groups only")
    d$proposal<-list(player=player,dead=as.list(sort(dead)));return(d)
  }
  if(kind=="resume") {d$phase<-"playing";d$passes<-0L;d["proposal"]<-list(NULL);return(d)}
  if(is.null(d$proposal) || d$proposal$player==player) .ra_fail("The opponent must accept the proposed dead groups")
  d$phase<-"finished";d
}
.ra_go_area <- function(d) {
  b<-d$board
  for(i in unlist(d$proposal$dead)) b[[i+1L]]<--1L
  scores<-c(2*sum(unlist(b)==0),2*sum(unlist(b)==1)+15);seen<-integer()
  for(i in 0:80) if(b[[i+1L]]==-1 && !i %in% seen) {
    g<-.ra_go_group(b,i);seen<-union(seen,g);owners<-unique(unlist(b[unique(unlist(lapply(g,.ra_go_neighbors)))+1L]));owners<-owners[owners!=-1]
    if(length(owners)==1) scores[owners+1L]<-scores[owners+1L]+2*length(g)
  }
  scores
}
.ra_go_legal <- function(state) {
  d<-state$data
  if(d$phase=="playing") {
    actions<-c(list(list(type="pass",params=structure(list(),names=character()))),lapply(0:1,function(p) list(type="resign",params=list(player=p))))
    types<-list(list(type="place",params_schema=list(cell=list(type="integer",minimum=0,maximum=80)),preconditions=list("Empty intersection; capture adjacent opponent groups without liberties; own resulting group must have a liberty; resulting board must not occur in positional history."),external_observations=list()))
  } else {
    actions<-list();for(k in c("resume","resign")) for(p in 0:1) actions[[length(actions)+1L]]<-list(type=k,params=list(player=p))
    if(!is.null(d$proposal)) actions[[length(actions)+1L]]<-list(type="accept_score",params=list(player=1-d$proposal$player))
    types<-list(list(type="propose_dead",params_schema=list(player=list(type="integer",minimum=0,maximum=1),dead=list(type="array",minItems=0,maxItems=81,items=list(type="integer",minimum=0,maximum=80))),preconditions=list("Distinct stones forming whole occupied groups; the opponent must accept before scoring."),external_observations=list("Players agree which groups are dead; disagreement resumes play.")))
  }
  list(mode="event_types",actions=actions,action_types=types,constraints=list(turn=d$turn),completeness=FALSE,reason="guarded_move_or_scoring_proposal")
}
.ra_go_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";wins<-integer();scores<-NULL
  if(terminal) {
    if(!is.null(d$resigned)) wins<-2L-d$resigned else {scores<-.ra_go_area(d);wins<-which(scores==max(scores))}
  }
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) "win" else "none",winners=lapply(wins,function(p) state$players[[p]]$id),losers=if(terminal) lapply(setdiff(1:2,wins),function(p) state$players[[p]]$id) else list(),placements=if(!is.null(scores)) lapply(1:2,function(p) list(player_id=state$players[[p]]$id,place=1+sum(scores>scores[p]),score=scores[p])) else list(),reason=if(terminal) if(!is.null(d$resigned)) "resignation" else "agreed_area_score" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
