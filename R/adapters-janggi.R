.ra_jg_lines <- list(c(3L,13L,23L),c(5L,13L,21L),c(66L,76L,86L),c(68L,76L,84L))
.ra_jg_diagonals <- function(i) {
  out<-list()
  for(line in .ra_jg_lines) if(i %in% line) {
    n<-match(i,line)
    if(n<3) out<-c(out,list(line[seq.int(n+1L,3L)]))
    if(n>1) out<-c(out,list(rev(line[seq_len(n-1L)])))
  }
  out
}
.ra_jg_initial <- function(layouts) {
  b<-as.list(rep(0L,90))
  for(p in 0:1) {
    row<-c(5L,3L,4L,2L,0L,2L,4L,3L,5L);layout<-if(is.null(layouts[[p+1L]])) 0L else layouts[[p+1L]]
    if(bitwAnd(as.integer(layout),1L)!=0) row[c(2,3)]<-row[c(3,2)]
    if(bitwAnd(as.integer(layout),2L)!=0) row[c(7,8)]<-row[c(8,7)]
    offset<-if(p==0) 0L else 81L;s<-if(p==0) 1L else -1L;b[(offset+1L):(offset+9L)]<-as.list(row*s);b[[if(p==0) 14L else 77L]]<-s
    for(i in if(p==0) c(19L,25L) else c(64L,70L)) b[[i+1L]]<-6L*s
    for(i in if(p==0) c(27L,29L,31L,33L,35L) else c(54L,56L,58L,60L,62L)) b[[i+1L]]<-7L*s
  }
  b
}
.ra_jg_targets <- function(b,i) {
  c<-b[[i+1L]];p<-.ra_xq_side(c);k<-abs(c);x<-i%%9;y<-i%/%9;out<-integer()
  add<-function(a,v) {if(.ra_xq_inside(a,v) && .ra_xq_side(b[[v*9+a+1L]])!=p) out<<-c(out,v*9+a)}
  if(k %in% c(1,2)) {
    for(delta in list(c(1,0),c(-1,0),c(0,1),c(0,-1))) if(.ra_xq_palace(p,x+delta[1],y+delta[2])) add(x+delta[1],y+delta[2])
    for(ray in .ra_jg_diagonals(i)) {j<-ray[1];if(.ra_xq_palace(p,j%%9,j%/%9)) add(j%%9,j%/%9)}
  } else if(k==3) {
    for(delta in list(c(2,3),c(2,-3),c(-2,3),c(-2,-3),c(3,2),c(3,-2),c(-3,2),c(-3,-2))) {
      a<-x+delta[1];v<-y+delta[2];sx<-sign(delta[1]);sy<-sign(delta[2]);legs<-if(abs(delta[2])==3) list(c(x,y+sy),c(x+sx,y+2*sy)) else list(c(x+sx,y),c(x+2*sx,y+sy))
      if(.ra_xq_inside(a,v) && all(vapply(legs,function(q) b[[q[2]*9+q[1]+1L]]==0,logical(1)))) add(a,v)
    }
  } else if(k==4) return(.ra_xq_targets(b,i))
  else if(k %in% c(5,6)) {
    rays<-.ra_jg_diagonals(i)
    for(delta in list(c(1,0),c(-1,0),c(0,1),c(0,-1))) {
      ray<-integer();a<-x+delta[1];v<-y+delta[2]
      while(.ra_xq_inside(a,v)) {ray<-c(ray,v*9+a);a<-a+delta[1];v<-v+delta[2]}
      rays<-c(rays,list(ray))
    }
    for(ray in rays) {
      screen<-FALSE
      for(j in ray) {
        if(k==5) {
          if(.ra_xq_side(b[[j+1L]])!=p) out<-c(out,j)
          if(b[[j+1L]]!=0) break
        } else if(!screen) {
          if(b[[j+1L]]!=0) {if(abs(b[[j+1L]])==6) break;screen<-TRUE}
        } else {
          if(.ra_xq_side(b[[j+1L]])!=p && abs(b[[j+1L]])!=6) out<-c(out,j)
          if(b[[j+1L]]!=0) break
        }
      }
    }
  } else if(k==7) {
    f<-if(p==0) 1L else -1L;add(x,y+f);add(x-1,y);add(x+1,y)
    for(ray in .ra_jg_diagonals(i)) {j<-ray[1];if(j%/%9-y==f) add(j%%9,j%/%9)}
  }
  out
}
.ra_jg_check <- function(b,p) {
  k<-match(if(p==0) 1L else -1L,unlist(b))-1L
  if(is.na(k)) return(TRUE)
  for(i in 0:89) if(b[[i+1L]]!=0 && .ra_xq_side(b[[i+1L]])!=p && k %in% .ra_jg_targets(b,i)) return(TRUE)
  FALSE
}
.ra_jg_facing <- function(b) {
  a<-match(1,unlist(b))-1L;z<-match(-1,unlist(b))-1L
  if(a%%9!=z%%9) return(FALSE)
  all(unlist(b)[seq.int(min(a,z)+9L,max(a,z)-9L,by=9L)+1L]==0)
}
.ra_jg_cache <- new.env(parent=emptyenv())
.ra_jg_moves <- function(d) {
  key<-.ra_xq_key(d)
  if(identical(.ra_jg_cache$key,key)) return(.ra_jg_cache$moves)
  b<-d$board;p<-d$turn;must_break<-.ra_jg_facing(b);out<-list()
  for(i in 0:89) {
    c<-b[[i+1L]]
    if(c==0 || .ra_xq_side(c)!=p) next
    for(j in .ra_jg_targets(b,i)) {
      if(abs(b[[j+1L]])==1) next
      trial<-b;trial[[j+1L]]<-trial[[i+1L]];trial[[i+1L]]<-0L
      if(!.ra_jg_check(trial,p) && (!must_break || !.ra_jg_facing(trial))) out<-c(out,list(c(i,j)))
    }
  }
  .ra_jg_cache$key<-key;.ra_jg_cache$moves<-out;out
}
.ra_jg_repeated <- function(d) sum(unlist(d$history)==.ra_xq_key(d))>=3
.ra_jg_validate <- function(d) {
  b<-unlist(d$board)
  if(d$phase=="setup") {
    expected_turn<-if(is.null(d$layouts[[2]])) 1L else 0L
    if(!is.null(d$layouts[[1]]) || d$turn!=expected_turn || any(b!=unlist(.ra_jg_initial(d$layouts))) || length(d$history) || d$passes>0 || !is.null(d$decision)) .ra_fail("Han chooses its layout before Cho, then play begins",code="INVALID_STATE")
    return(invisible(NULL))
  }
  if(any(vapply(d$layouts,is.null,logical(1)))) .ra_fail("Both initial layouts must be selected",code="INVALID_STATE")
  for(p in 0:1) {
    s<-if(p==0) 1L else -1L;limits<-c(1L,2L,2L,2L,2L,2L,5L)
    for(k in 1:7) {n<-sum(b==s*k);if(n>limits[k] || (k==1 && n!=1)) .ra_fail("Invalid army composition",code="INVALID_STATE")}
    for(i in 0:89) if(b[i+1L]!=0 && .ra_xq_side(b[i+1L])==p) {
      c<-b[i+1L]
      if(abs(c) %in% c(1,2) && !.ra_xq_palace(p,i%%9,i%/%9)) .ra_fail("General and guards must stay in their palace",code="INVALID_STATE")
      if(abs(c)==7 && ((p==0 && i%/%9<3) || (p==1 && i%/%9>6))) .ra_fail("Soldier cannot retreat behind its initial rank",code="INVALID_STATE")
    }
  }
  if(.ra_jg_check(d$board,1L-d$turn)) .ra_fail("The nonmoving general cannot remain in check",code="INVALID_STATE")
  if(!length(d$history) || tail(unlist(d$history),1)!=.ra_xq_key(d) || any(!grepl("^[a-o]{90}[01]$",unlist(d$history)))) .ra_fail("Invalid position history",code="INVALID_STATE")
  if(d$passes>0 && (.ra_jg_check(d$board,d$turn) || .ra_jg_facing(d$board))) .ra_fail("Passing cannot leave check or an unresolved bikjang",code="INVALID_STATE")
  if(!is.null(d$decision)) {
    dec<-d$decision;r<-dec$reason
    if((r %in% c("bikjang","mutual_repetition"))!=is.null(dec$offender)) .ra_fail("Decision and offender disagree",code="INVALID_STATE")
    if(r=="bikjang" && (!.ra_jg_facing(d$board) || .ra_jg_check(d$board,d$turn))) .ra_fail("Bikjang acceptance requires facing generals without check",code="INVALID_STATE")
    if(r %in% c("perpetual_check","perpetual_chase","mutual_repetition") && !.ra_jg_repeated(d)) .ra_fail("Repetition decision requires a third occurrence",code="INVALID_STATE")
  }
}
.ra_jg_apply <- function(state,action) {
  d<-state$data;p<-action$params;k<-action$type;t<-d$turn;empty<-structure(list(),names=character())
  if(k=="choose_layout") {
    .ra_fields(p,list(layout=list(type="integer",minimum=0,maximum=3)),"/params")
    if(d$phase!="setup") .ra_fail("Layouts are fixed once play begins")
    if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
    d$layouts[[t+1L]]<-p$layout;d$board<-.ra_jg_initial(d$layouts)
    if(t==1) d$turn<-0L else {d$phase<-"play";d$history<-list(.ra_xq_key(d))}
    return(d)
  }
  if(d$phase!="play") .ra_fail("Choose both initial layouts first")
  if(k=="resign") {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params");q<-p$player
    if(!is.null(action$actor) && action$actor!=state$players[[q+1L]]$id) .ra_fail("Wrong actor")
    d$decision<-list(reason="resignation",offender=q);return(d)
  }
  if(k=="adjudicate_repetition") {
    .ra_fields(p,list(offender=list(type="integer",minimum=0,maximum=1,nullable=TRUE),reason=list(type="string",enum=list("perpetual_check","perpetual_chase","mutual_repetition"))),"/params")
    if(!.ra_jg_repeated(d) || (p$reason=="mutual_repetition")!=is.null(p$offender)) .ra_fail("Resolve an actual repetition with a consistent official decision")
    d$decision<-p;return(d)
  }
  if(.ra_jg_repeated(d)) .ra_fail("Resolve repetition before continuing")
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(k=="accept_bikjang") {
    .ra_fields(p,empty,"/params")
    if(!.ra_jg_facing(d$board) || .ra_jg_check(d$board,t)) .ra_fail("Accept bikjang only with facing generals and no check")
    d$decision<-list(reason="bikjang",offender=NULL);return(d)
  }
  if(k=="pass") {
    .ra_fields(p,empty,"/params")
    if(.ra_jg_check(d$board,t) || .ra_jg_facing(d$board)) .ra_fail("Cannot pass while checked or facing bikjang")
    d$passes<-d$passes+1L
  } else if(k=="move") {
    .ra_fields(p,list(source=list(type="integer",minimum=0,maximum=89),destination=list(type="integer",minimum=0,maximum=89)),"/params");i<-p$source;j<-p$destination
    if(!any(vapply(.ra_jg_moves(d),function(m) m[1]==i && m[2]==j,logical(1)))) .ra_fail("Illegal piece move, general exposure or failure to break bikjang")
    captured<-d$board[[j+1L]]!=0;d$board[[j+1L]]<-d$board[[i+1L]];d$board[[i+1L]]<-0L;d$passes<-0L
    if(captured) d$history<-list()
  } else .ra_fail("Unknown action")
  d$turn<-1L-t;d$history<-c(d$history,list(.ra_xq_key(d)));d
}
.ra_jg_legal <- function(state) {
  d<-state$data
  if(d$phase=="setup") return(lapply(0:3,function(x) list(type="choose_layout",params=list(layout=x))))
  if(.ra_jg_repeated(d)) return(list(mode="event_types",actions=list(),action_types=list(list(type="adjudicate_repetition",params_schema=list(offender=list(type="integer",minimum=0,maximum=1,nullable=TRUE),reason=list(type="string",enum=list("perpetual_check","perpetual_chase","mutual_repetition"))),preconditions=list("Third occurrence requires an official responsibility decision."),external_observations=list("Perpetual-check/chase or mutual-repetition decision."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="repetition_requires_adjudication"))
  actions<-c(lapply(.ra_jg_moves(d),function(m) list(type="move",params=list(source=m[1],destination=m[2]))),lapply(0:1,function(p) list(type="resign",params=list(player=p))))
  if(!.ra_jg_check(d$board,d$turn)) actions<-c(actions,list(list(type=if(.ra_jg_facing(d$board)) "accept_bikjang" else "pass",params=structure(list(),names=character()))))
  actions
}
.ra_jg_outcome <- function(state,pack) {
  d<-state$data;reason<-NULL;w<-NULL;pending<-FALSE
  if(d$phase=="play") {
    if(!is.null(d$decision)) {reason<-d$decision$reason;w<-if(is.null(d$decision$offender)) NULL else 1L-d$decision$offender}
    else if(.ra_jg_check(d$board,d$turn) && !length(.ra_jg_moves(d))) {reason<-"checkmate";w<-1L-d$turn}
    else if(d$passes==2) reason<-"two_consecutive_passes" else pending<-.ra_jg_repeated(d)
  }
  done<-!is.null(reason)
  list(status=if(done) "terminal" else if(pending) "awaiting_adjudication" else "ongoing",result_kind=if(done) if(is.null(w)) "draw" else "win" else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) reason else if(pending) "repeated_position" else "ongoing",evidence_required=if(pending) list("Official repeated-position adjudication.") else list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
