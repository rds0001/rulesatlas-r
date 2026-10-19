.ra_fanorona_dirs <- list(c(-1,0),c(1,0),c(0,-1),c(0,1),c(-1,-1),c(-1,1),c(1,-1),c(1,1))
.ra_fanorona_at <- function(r,c) if(r>=0 && r<5 && c>=0 && c<9) r*9+c else NULL
.ra_fanorona_ray <- function(b,r,c,dr,dc,enemy) {
  result<-integer();i<-.ra_fanorona_at(r,c)
  while(!is.null(i) && b[[i+1L]]==enemy) {result<-c(result,i);r<-r+dr;c<-c+dc;i<-.ra_fanorona_at(r,c)}
  result
}
.ra_fanorona_options <- function(d,captures_only=FALSE) {
  b<-d$board;t<-d$turn;captures<-list();quiet<-list();pending<-d$pending
  for(i in 0:44) {
    if(b[[i+1L]]!=t || (!is.null(pending) && i!=pending)) next
    r<-i%/%9;c<-i%%9
    for(direction in 0:7) {
      delta<-.ra_fanorona_dirs[[direction+1L]];dr<-delta[1];dc<-delta[2]
      if((direction>=4 && (r+c)%%2==1) || (!is.null(d$last_direction) && direction==d$last_direction)) next
      j<-.ra_fanorona_at(r+dr,c+dc)
      if(is.null(j) || b[[j+1L]]!=-1 || j %in% unlist(d$visited)) next
      approach<-.ra_fanorona_ray(b,r+2*dr,c+2*dc,dr,dc,1-t);withdraw<-.ra_fanorona_ray(b,r-dr,c-dc,-dr,-dc,1-t)
      if(length(approach)) captures[[length(captures)+1L]]<-list(start=i,end=j,mode="approach",direction=direction,victims=approach)
      if(length(withdraw)) captures[[length(captures)+1L]]<-list(start=i,end=j,mode="withdrawal",direction=direction,victims=withdraw)
      if(!length(approach) && !length(withdraw)) quiet[[length(quiet)+1L]]<-list(start=i,end=j,mode="move",direction=direction,victims=integer())
    }
  }
  if(length(captures) || !is.null(pending) || captures_only) captures else quiet
}
.ra_fanorona_key <- function(d) paste0(paste0(unlist(d$board)+1,collapse=""),d$turn)
.ra_fanorona_end <- function(d,captured) {
  d["pending"]<-list(NULL);d$visited<-list();d["last_direction"]<-list(NULL);d$turn<-1L-d$turn
  if(captured) d$history<-list(.ra_fanorona_key(d)) else d$history<-c(d$history,list(.ra_fanorona_key(d)))
  d
}
.ra_fanorona_validate <- function(d) {
  b<-unlist(d$board)
  if(all(b==-1) || any(vapply(0:1,function(p) sum(b==p)>22,logical(1)))) .ra_fail("More than twenty-two stones for one side",code="INVALID_STATE")
  if(is.null(d$pending)) {
    if(length(d$visited) || !is.null(d$last_direction) || !length(d$history) || tail(unlist(d$history),1)!=.ra_fanorona_key(d)) .ra_fail("Completed turn has stale capture sequence or history",code="INVALID_STATE")
  } else {
    path<-unlist(d$visited)
    if(any(b[head(path,-1)+1L]!=-1) || length(path)<2 || anyDuplicated(path) || tail(path,1)!=d$pending || b[d$pending+1L]!=d$turn || is.null(d$last_direction) || length(d$history) || !length(.ra_fanorona_options(d,TRUE))) .ra_fail("Invalid continuation of a capture sequence",code="INVALID_STATE")
    for(k in 2:length(path)) {
      a<-path[k-1];bb<-path[k];r<-a%/%9;c<-a%%9;delta<-c(bb%/%9-r,bb%%9-c)
      connected<-any(vapply(.ra_fanorona_dirs,function(x) all(x==delta),logical(1)))
      if(!connected || (sum(abs(delta))==2 && (r+c)%%2==1)) .ra_fail("Capture path does not follow connected intersections",code="INVALID_STATE")
    }
    a<-path[length(path)-1];bb<-tail(path,1);delta<-c(bb%/%9-a%/%9,bb%%9-a%%9)
    if(any(.ra_fanorona_dirs[[d$last_direction+1L]]!=delta)) .ra_fail("Last capture direction mismatch",code="INVALID_STATE")
  }
  if(any(!grepl("^[012]{45}[01]$",unlist(d$history)))) .ra_fail("Malformed repetition history",code="INVALID_STATE")
}
.ra_fanorona_apply <- function(state,action) {
  d<-state$data;p<-action$params;t<-d$turn
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(action$type=="end_turn") {
    .ra_fields(p,structure(list(),names=character()),"/params")
    if(is.null(d$pending)) .ra_fail("Only a started capture sequence may be stopped")
    return(.ra_fanorona_end(d,TRUE))
  }
  if(action$type!="step") .ra_fail("Unknown action")
  .ra_fields(p,list(start=list(type="integer",minimum=0,maximum=44),end=list(type="integer",minimum=0,maximum=44),mode=list(type="string",enum=list("move","approach","withdrawal"))),"/params")
  match<-Filter(function(m) m$start==p$start && m$end==p$end && m$mode==p$mode,.ra_fanorona_options(d))
  if(!length(match)) .ra_fail("Step violates capture priority, connected lines or capture-sequence restrictions")
  m<-match[[1]];i<-m$start;j<-m$end;d$board[[i+1L]]<--1L;d$board[[j+1L]]<-t
  if(!length(m$victims)) return(.ra_fanorona_end(d,FALSE))
  for(k in m$victims) d$board[[k+1L]]<--1L
  if(is.null(d$pending)) d$visited<-list(i)
  d$visited<-c(d$visited,list(j));d$pending<-j;d$last_direction<-m$direction;d$history<-list()
  if(length(.ra_fanorona_options(d,TRUE))) d else .ra_fanorona_end(d,TRUE)
}
.ra_fanorona_legal <- function(state) {
  d<-state$data;actions<-lapply(.ra_fanorona_options(d),function(m) list(type="step",params=list(start=m$start,end=m$end,mode=m$mode)))
  if(!is.null(d$pending)) actions<-c(actions,list(list(type="end_turn",params=structure(list(),names=character()))))
  actions
}
.ra_fanorona_outcome <- function(state,pack) {
  d<-state$data;b<-unlist(d$board);lost<-!any(b==d$turn) || (is.null(d$pending) && !length(.ra_fanorona_options(d)));other_lost<-!any(b==1-d$turn);draw<-!lost && !other_lost && is.null(d$pending) && sum(unlist(d$history)==.ra_fanorona_key(d))>=3;terminal<-lost || other_lost || draw;winner<-if(other_lost) d$turn else 1L-d$turn
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(draw) "draw" else if(terminal) "win" else "none",winners=if(!terminal || draw) list() else list(state$players[[winner+1L]]$id),losers=if(!terminal || draw) list() else list(state$players[[2L-winner]]$id),placements=list(),reason=if(draw) "threefold_repetition" else if(terminal) "no_stones_or_moves" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
