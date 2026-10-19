.ra_xq_side <- function(piece) {if(piece>0) 0L else if(piece<0) 1L else -1L}
.ra_xq_inside <- function(x,y) x>=0 && x<9 && y>=0 && y<10
.ra_xq_palace <- function(p,x,y) x>=3 && x<=5 && (if(p==0) y>=0 && y<=2 else y>=7 && y<=9)
.ra_xq_targets <- function(b,i) {
  piece<-b[[i+1L]];p<-.ra_xq_side(piece);kind<-abs(piece);x<-i%%9;y<-i%/%9;out<-integer()
  add<-function(a,c) {if(.ra_xq_inside(a,c) && .ra_xq_side(b[[c*9+a+1L]])!=p) out<<-c(out,c*9+a)}
  if(kind==1) {
    for(delta in list(c(1,0),c(-1,0),c(0,1),c(0,-1))) if(.ra_xq_palace(p,x+delta[1],y+delta[2])) add(x+delta[1],y+delta[2])
    dy<-if(p==0) 1L else -1L;row<-y+dy
    while(.ra_xq_inside(x,row)) {
      j<-row*9+x
      if(b[[j+1L]]!=0) {if(b[[j+1L]]== -piece) out<-c(out,j);break}
      row<-row+dy
    }
  } else if(kind==2) {
    for(delta in list(c(1,1),c(1,-1),c(-1,1),c(-1,-1))) if(.ra_xq_palace(p,x+delta[1],y+delta[2])) add(x+delta[1],y+delta[2])
  } else if(kind==3) {
    for(delta in list(c(2,2),c(2,-2),c(-2,2),c(-2,-2))) {
      a<-x+delta[1];c<-y+delta[2]
      if(.ra_xq_inside(a,c) && (if(p==0) c<=4 else c>=5) && b[[(y+delta[2]/2)*9+x+delta[1]/2+1L]]==0) add(a,c)
    }
  } else if(kind==4) {
    for(delta in list(c(2,1),c(2,-1),c(-2,1),c(-2,-1),c(1,2),c(-1,2),c(1,-2),c(-1,-2))) {
      lx<-x+if(abs(delta[1])==2) delta[1]/2 else 0;ly<-y+if(abs(delta[2])==2) delta[2]/2 else 0
      if(.ra_xq_inside(lx,ly) && b[[ly*9+lx+1L]]==0) add(x+delta[1],y+delta[2])
    }
  } else if(kind %in% c(5,6)) {
    for(delta in list(c(1,0),c(-1,0),c(0,1),c(0,-1))) {
      a<-x+delta[1];c<-y+delta[2];screen<-FALSE
      while(.ra_xq_inside(a,c)) {
        j<-c*9+a
        if(kind==5) {
          if(.ra_xq_side(b[[j+1L]])!=p) out<-c(out,j)
          if(b[[j+1L]]!=0) break
        } else if(!screen) {
          if(b[[j+1L]]!=0) screen<-TRUE else out<-c(out,j)
        } else if(b[[j+1L]]!=0) {if(.ra_xq_side(b[[j+1L]])!=p) out<-c(out,j);break}
        a<-a+delta[1];c<-c+delta[2]
      }
    }
  } else if(kind==7) {
    add(x,y+if(p==0) 1L else -1L)
    if(if(p==0) y>=5 else y<=4) {add(x-1,y);add(x+1,y)}
  }
  out
}
.ra_xq_check <- function(b,p) {
  king<-if(p==0) 1L else -1L;k<-match(king,unlist(b))-1L
  if(is.na(k)) return(TRUE)
  for(i in 0:89) if(b[[i+1L]]!=0 && .ra_xq_side(b[[i+1L]])!=p && k %in% .ra_xq_targets(b,i)) return(TRUE)
  FALSE
}
.ra_xq_move_cache <- new.env(parent=emptyenv())
.ra_xq_moves <- function(d) {
  cache_key<-.ra_xq_key(d)
  if(identical(.ra_xq_move_cache$key,cache_key)) return(.ra_xq_move_cache$moves)
  b<-d$board;p<-d$turn;out<-list()
  for(i in 0:89) {
    c<-b[[i+1L]]
    if(c==0 || .ra_xq_side(c)!=p) next
    for(j in .ra_xq_targets(b,i)) {
      if(abs(b[[j+1L]])==1) next
      trial<-b;trial[[j+1L]]<-trial[[i+1L]];trial[[i+1L]]<-0L
      if(!.ra_xq_check(trial,p)) out<-c(out,list(c(i,j)))
    }
  }
  .ra_xq_move_cache$key<-cache_key;.ra_xq_move_cache$moves<-out
  out
}
.ra_xq_key <- function(d) paste0(paste0(intToUtf8(as.integer(unlist(d$board)+104L),multiple=TRUE),collapse=""),d$turn)
.ra_xq_repetition <- function(d) sum(unlist(d$history)==.ra_xq_key(d))>=3
.ra_xq_validate <- function(d) {
  b<-unlist(d$board)
  for(p in 0:1) {
    sign<-if(p==0) 1L else -1L;limits<-c(1L,2L,2L,2L,2L,2L,5L)
    for(kind in 1:7) {
      n<-sum(b==sign*kind)
      if(n>limits[kind] || (kind==1 && n!=1)) .ra_fail("Invalid army composition",code="INVALID_STATE")
    }
    for(i in 0:89) {
      c<-b[i+1L]
      if(c==0 || .ra_xq_side(c)!=p) next
      x<-i%%9;y<-i%/%9;k<-abs(c)
      if(k %in% c(1,2) && !.ra_xq_palace(p,x,y)) .ra_fail("General and advisors must stay in their palace",code="INVALID_STATE")
      if(k==2 && (x+y)%%2!=(if(p==0) 1L else 0L)) .ra_fail("Advisor is off its diagonal palace points",code="INVALID_STATE")
      if(k==3 && !paste(x,if(p==0) y else 9L-y) %in% c("2 0","6 0","0 2","4 2","8 2","2 4","6 4")) .ra_fail("Elephant is off its reachable own-side points",code="INVALID_STATE")
      if(k==7 && ((p==0 && y<3) || (p==1 && y>6) || ((if(p==0) y<5 else y>4) && x%%2!=0))) .ra_fail("Soldier is behind its starting rank or sideways before crossing",code="INVALID_STATE")
    }
  }
  if(.ra_xq_check(d$board,1L-d$turn)) .ra_fail("The nonmoving general cannot be left in check",code="INVALID_STATE")
  if(!length(d$history) || tail(unlist(d$history),1)!=.ra_xq_key(d)) .ra_fail("History must end at the current position",code="INVALID_STATE")
  if(any(!grepl("^[a-o]{90}[01]$",unlist(d$history)))) .ra_fail("Invalid position history encoding",code="INVALID_STATE")
  if(!is.null(d$decision) && ((d$decision$reason=="mutual_repetition")!=is.null(d$decision$offender))) .ra_fail("Repetition decision and offender disagree",code="INVALID_STATE")
  if(!is.null(d$decision) && !.ra_xq_repetition(d)) .ra_fail("Repetition adjudication requires a third occurrence",code="INVALID_STATE")
}
.ra_xq_apply <- function(state,action) {
  d<-state$data;p<-action$params;k<-action$type;t<-d$turn
  if(k=="adjudicate_repetition") {
    .ra_fields(p,list(offender=list(type="integer",minimum=0,maximum=1,nullable=TRUE),reason=list(type="string",enum=list("perpetual_check","perpetual_chase","mutual_repetition"))),"/params")
    if(!.ra_xq_repetition(d)) .ra_fail("No repeated-position dispute is pending")
    if((p$reason=="mutual_repetition")!=is.null(p$offender)) .ra_fail("Mutual repetition draws; a one-sided offence must name the offender")
    d$decision<-p;return(d)
  }
  if(k!="move") .ra_fail("Unknown action")
  .ra_fields(p,list(source=list(type="integer",minimum=0,maximum=89),destination=list(type="integer",minimum=0,maximum=89)),"/params");i<-p$source;j<-p$destination
  if(.ra_xq_repetition(d)) .ra_fail("Resolve repeated-position adjudication before further play")
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(!any(vapply(.ra_xq_moves(d),function(m) m[1]==i && m[2]==j,logical(1)))) .ra_fail("Move must obey piece restrictions and leave the moving general safe")
  captured<-d$board[[j+1L]]!=0;d$board[[j+1L]]<-d$board[[i+1L]];d$board[[i+1L]]<-0L;d$turn<-1L-t
  if(captured) d$history<-list()
  d$history<-c(d$history,list(.ra_xq_key(d)));d
}
.ra_xq_legal <- function(state) {
  d<-state$data
  if(.ra_xq_repetition(d)) return(list(mode="event_types",actions=list(),action_types=list(list(type="adjudicate_repetition",params_schema=list(offender=list(type="integer",minimum=0,maximum=1,nullable=TRUE),reason=list(type="string",enum=list("perpetual_check","perpetual_chase","mutual_repetition"))),preconditions=list("An official adjudicator resolves repeated-position responsibility; null offender only for a mutual draw."),external_observations=list("Perpetual check/chase classification and responsibility."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="repetition_requires_adjudication"))
  lapply(.ra_xq_moves(d),function(m) list(type="move",params=list(source=m[1],destination=m[2])))
}
.ra_xq_outcome <- function(state,pack) {
  d<-state$data;decision<-d$decision;available<-.ra_xq_moves(d);done<-!is.null(decision) || !length(available);pending<-!done && .ra_xq_repetition(d);w<-if(!is.null(decision)) if(is.null(decision$offender)) NULL else 1L-decision$offender else if(done) 1L-d$turn else NULL
  list(status=if(done) "terminal" else if(pending) "awaiting_adjudication" else "ongoing",result_kind=if(done) if(is.null(w)) "draw" else "win" else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(!is.null(decision)) decision$reason else if(done) if(.ra_xq_check(d$board,d$turn)) "checkmate" else "stalemate_loss" else if(pending) "repeated_position" else "ongoing",evidence_required=if(pending) list("Official repeated-position adjudication.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
