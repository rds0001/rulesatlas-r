.ra_ttt_lines <- list(c(1,2,3),c(4,5,6),c(7,8,9),c(1,4,7),c(2,5,8),c(3,6,9),c(1,5,9),c(3,5,7))
.ra_ttt_winners <- function(board) {
  values<-unlist(board)
  unique(unlist(lapply(.ra_ttt_lines,function(line) if(values[line[1]]!=-1 && length(unique(values[line]))==1L) values[line[1]] else numeric())))
}
.ra_ttt_validate <- function(data) {
  board<-data$board; values<-unlist(board); n0<-sum(values==0); n1<-sum(values==1); win<-.ra_ttt_winners(board)
  if(!n0 %in% c(n1,n1+1) || data$turn!=if(n0==n1) 0 else 1) .ra_fail("Invalid mark counts or turn",code="INVALID_STATE")
  if(length(win)>1L) .ra_fail("Simultaneous winners",code="INVALID_STATE")
  if(length(win)) {
    w<-win[1]
    if(n0-n1!=if(w==0) 1 else 0) .ra_fail("Moves continued after a win",code="INVALID_STATE")
    possible<-vapply(which(values==w),function(i) { previous<-board; previous[[i]]<--1; !length(.ra_ttt_winners(previous)) },logical(1))
    if(!any(possible)) .ra_fail("No legal last winning move",code="INVALID_STATE")
  }
}
.ra_ttt_outcome <- function(state,pack) {
  win<-.ra_ttt_winners(state$data$board); full<-!(-1 %in% unlist(state$data$board)); won<-length(win)>0
  ids<-lapply(state$players,function(p) p$id); ws<-if(won) list(ids[[win[1]+1L]]) else list()
  list(status=if(won||full) "terminal" else "ongoing",result_kind=if(won) "win" else if(full) "draw" else "none",winners=ws,losers=if(won) Filter(function(id) !id %in% unlist(ws),ids) else list(),placements=list(),reason=if(won) "three_in_a_row" else if(full) "board_full" else "ongoing",evidence_required=list(),rule_id=if(won||full) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
.ra_ttt_apply <- function(state,action) {
  if(!identical(action$type,"place")) .ra_fail("Unknown action")
  .ra_fields(action$params,list(cell=list(type="integer",minimum=0,maximum=8)),"/params")
  cell<-action$params$cell+1L; data<-state$data
  if(data$board[[cell]]!=-1) .ra_fail("Occupied square")
  if(!is.null(action$actor) && action$actor!=state$players[[data$turn+1L]]$id) .ra_fail("Wrong actor")
  board<-data$board; board[[cell]]<-data$turn
  list(board=board,turn=1-data$turn)
}
.ra_ttt_legal <- function(state) lapply(which(unlist(state$data$board)==-1)-1L,function(i) list(type="place",params=list(cell=i)))
.ra_adapter <- function(pack,operation,...) {
  if(identical(pack$adapter,"chess")) {
    fn<-switch(operation,validate=.ra_chess_validate,outcome=.ra_chess_outcome,apply=.ra_chess_apply,legal=.ra_chess_legal);return(fn(...))
  }
  if(identical(pack$adapter,"durak")) {
    fn<-switch(operation,validate=.ra_durak_validate,outcome=.ra_durak_outcome,apply=.ra_durak_apply,legal=.ra_durak_legal);return(fn(...))
  }
  if(identical(pack$adapter,"stud")) {
    fn<-switch(operation,validate=.ra_stud_validate,outcome=.ra_stud_outcome,apply=.ra_stud_apply,legal=.ra_stud_legal);return(fn(...))
  }
  if(identical(pack$adapter,"draw_poker")) {
    fn<-switch(operation,validate=.ra_draw_poker_validate,outcome=.ra_draw_poker_outcome,apply=.ra_draw_poker_apply,legal=.ra_draw_poker_legal);return(fn(...))
  }
  if(pack$adapter %in% c("holdem","omaha")) {
    fn<-switch(operation,validate=.ra_poker_validate,outcome=.ra_poker_outcome,apply=.ra_poker_apply,legal=.ra_poker_legal);return(fn(...,hole_count=if(pack$adapter=="holdem") 2 else 4,pot_limit=pack$adapter=="omaha"))
  }
  if(identical(pack$adapter,"pai_gow")) {
    fn<-switch(operation,validate=.ra_pai_gow_validate,outcome=.ra_pai_gow_outcome,apply=.ra_pai_gow_apply,legal=.ra_pai_gow_legal);return(fn(...))
  }
  if(identical(pack$adapter,"blackjack")) {
    fn<-switch(operation,validate=.ra_blackjack_validate,outcome=.ra_blackjack_outcome,apply=.ra_blackjack_apply,legal=.ra_blackjack_legal);return(fn(...))
  }
  if(identical(pack$adapter,"crossminton")) {
    fn<-switch(operation,validate=.ra_crossminton_validate,outcome=.ra_crossminton_outcome,apply=.ra_crossminton_apply,legal=.ra_crossminton_legal);return(fn(...))
  }
  if(identical(pack$adapter,"pickleball")) {
    fn<-switch(operation,validate=.ra_pickleball_validate,outcome=.ra_pickleball_outcome,apply=.ra_pickleball_apply,legal=.ra_pickleball_legal);return(fn(...))
  }
  if(identical(pack$adapter,"fanorona")) {
    fn<-switch(operation,validate=.ra_fanorona_validate,outcome=.ra_fanorona_outcome,apply=.ra_fanorona_apply,legal=.ra_fanorona_legal);return(fn(...))
  }
  if(identical(pack$adapter,"president")) {
    fn<-switch(operation,validate=.ra_president_validate,outcome=.ra_president_outcome,apply=.ra_president_apply,legal=.ra_president_legal);return(fn(...))
  }
  if(identical(pack$adapter,"archery")) {
    fn<-switch(operation,validate=.ra_archery_validate,outcome=.ra_archery_outcome,apply=.ra_archery_apply,legal=.ra_archery_legal);return(fn(...))
  }
  if(identical(pack$adapter,"euchre")) {
    fn<-switch(operation,validate=.ra_euchre_validate,outcome=.ra_euchre_outcome,apply=.ra_euchre_apply,legal=.ra_euchre_legal);return(fn(...))
  }
  if(identical(pack$adapter,"scopa")) {
    fn<-switch(operation,validate=.ra_scopa_validate,outcome=.ra_scopa_outcome,apply=.ra_scopa_apply,legal=.ra_scopa_legal);return(fn(...))
  }
  if(identical(pack$adapter,"draughts")) {
    fn<-switch(operation,validate=.ra_draughts_validate,outcome=.ra_draughts_outcome,apply=.ra_draughts_apply,legal=.ra_draughts_legal);return(fn(...))
  }
  if(identical(pack$adapter,"go")) {
    fn<-switch(operation,validate=.ra_go_validate,outcome=.ra_go_outcome,apply=.ra_go_apply,legal=.ra_go_legal);return(fn(...))
  }
  if(identical(pack$adapter,"maexchen")) {
    fn<-switch(operation,validate=.ra_maexchen_validate,outcome=.ra_maexchen_outcome,apply=.ra_maexchen_apply,legal=.ra_maexchen_legal);return(fn(...))
  }
  if(identical(pack$adapter,"liars_dice")) {
    fn<-switch(operation,validate=.ra_liars_dice_validate,outcome=.ra_liars_dice_outcome,apply=.ra_liars_dice_apply,legal=.ra_liars_dice_legal);return(fn(...))
  }
  if(identical(pack$adapter,"mafia")) {
    fn<-switch(operation,validate=.ra_mafia_validate,outcome=.ra_mafia_outcome,apply=.ra_mafia_apply,legal=.ra_mafia_legal);return(fn(...))
  }
  if(identical(pack$adapter,"sumo")) {
    fn<-switch(operation,validate=.ra_sumo_validate,outcome=.ra_sumo_outcome,apply=.ra_sumo_apply,legal=.ra_sumo_legal);return(fn(...))
  }
  if(identical(pack$adapter,"chinese_checkers")) {
    fn<-switch(operation,validate=.ra_chinese_checkers_validate,outcome=.ra_chinese_checkers_outcome,apply=.ra_chinese_checkers_apply,legal=.ra_chinese_checkers_legal);return(fn(...))
  }
  if(identical(pack$adapter,"spades")) {
    fn<-switch(operation,validate=.ra_spades_validate,outcome=.ra_spades_outcome,apply=.ra_spades_apply,legal=.ra_spades_legal);return(fn(...))
  }
  if(identical(pack$adapter,"hearts")) {
    fn<-switch(operation,validate=.ra_hearts_validate,outcome=.ra_hearts_outcome,apply=.ra_hearts_apply,legal=.ra_hearts_legal);return(fn(...))
  }
  if(identical(pack$adapter,"briscola")) {
    fn<-switch(operation,validate=.ra_briscola_validate,outcome=.ra_briscola_outcome,apply=.ra_briscola_apply,legal=.ra_briscola_legal);return(fn(...))
  }
  if(!is.null(pack$mancala_rules)) {
    fn<-switch(operation,validate=.ra_mancala_validate,outcome=.ra_mancala_outcome,apply=.ra_mancala_apply,legal=.ra_mancala_legal);return(fn(...,config=pack$mancala_rules))
  }
  if(identical(pack$adapter,"sic_bo")) {
    fn<-switch(operation,validate=.ra_sic_bo_validate,outcome=.ra_sic_bo_outcome,apply=.ra_sic_bo_apply,legal=.ra_sic_bo_legal);return(fn(...))
  }
  if(identical(pack$adapter,"keno")) {
    fn<-switch(operation,validate=.ra_keno_validate,outcome=.ra_keno_outcome,apply=.ra_keno_apply,legal=.ra_keno_legal);return(fn(...))
  }
  if(identical(pack$adapter,"roulette")) {
    fn<-switch(operation,validate=.ra_roulette_validate,outcome=.ra_roulette_outcome,apply=.ra_roulette_apply,legal=.ra_roulette_legal);return(fn(...))
  }
  if(identical(pack$adapter,"baccarat")) {
    fn<-switch(operation,validate=.ra_baccarat_validate,outcome=.ra_baccarat_outcome,apply=.ra_baccarat_apply,legal=.ra_baccarat_legal);return(fn(...))
  }
  if(identical(pack$adapter,"morris")) {
    fn<-switch(operation,validate=.ra_morris_validate,outcome=.ra_morris_outcome,apply=.ra_morris_apply,legal=.ra_morris_legal);return(fn(...))
  }
  if(identical(pack$adapter,"vertical_jump")) {
    fn<-switch(operation,validate=.ra_vertical_validate,outcome=.ra_vertical_outcome,apply=.ra_vertical_apply,legal=.ra_vertical_legal);return(fn(...))
  }
  if(identical(pack$adapter,"categories")) {
    fn<-switch(operation,validate=.ra_categories_validate,outcome=.ra_categories_outcome,apply=.ra_categories_apply,legal=.ra_categories_legal);return(fn(...))
  }
  if(identical(pack$adapter,"bingo")) {
    fn<-switch(operation,validate=.ra_bingo_validate,outcome=.ra_bingo_outcome,apply=.ra_bingo_apply,legal=.ra_bingo_legal);return(fn(...))
  }
  if(identical(pack$adapter,"bowling")) {
    fn<-switch(operation,validate=.ra_bowling_validate,outcome=.ra_bowling_outcome,apply=.ra_bowling_apply,legal=.ra_bowling_legal);return(fn(...))
  }
  if(identical(pack$adapter,"cornhole")) {
    fn<-switch(operation,validate=.ra_cornhole_validate,outcome=.ra_cornhole_outcome,apply=.ra_cornhole_apply,legal=.ra_cornhole_legal);return(fn(...))
  }
  if(identical(pack$adapter,"field_distance")) {
    fn<-switch(operation,validate=.ra_field_validate,outcome=.ra_field_outcome,apply=.ra_field_apply,legal=.ra_field_legal);return(fn(...))
  }
  if(identical(pack$adapter,"kakuro")) {
    fn<-switch(operation,validate=.ra_kakuro_validate,outcome=.ra_kakuro_outcome,apply=.ra_kakuro_apply,legal=.ra_kakuro_legal);return(fn(...))
  }
  if(identical(pack$adapter,"nonogram")) {
    fn<-switch(operation,validate=.ra_nonogram_validate,outcome=.ra_nonogram_outcome,apply=.ra_nonogram_apply,legal=.ra_nonogram_legal);return(fn(...))
  }
  if(identical(pack$adapter,"boulder")) {
    fn<-switch(operation,validate=.ra_boulder_validate,outcome=.ra_boulder_outcome,apply=.ra_boulder_apply,legal=.ra_boulder_legal);return(fn(...))
  }
  if(!is.null(pack$ordered_race_rules)) {
    fn<-switch(operation,validate=.ra_or_validate,outcome=.ra_or_outcome,apply=.ra_or_apply,legal=.ra_or_legal);return(fn(...,config=pack$ordered_race_rules))
  }
  if(!is.null(pack$timed_heats_rules)) {
    fn<-switch(operation,validate=.ra_th_validate,outcome=.ra_th_outcome,apply=.ra_th_apply,legal=.ra_th_legal);return(fn(...,config=pack$timed_heats_rules))
  }
  if(identical(pack$adapter,"weightlifting")) {
    fn<-switch(operation,validate=.ra_wl_validate,outcome=.ra_wl_outcome,apply=.ra_wl_apply,legal=.ra_wl_legal);return(fn(...))
  }
  if(identical(pack$adapter,"powerlifting")) {
    fn<-switch(operation,validate=.ra_pl_validate,outcome=.ra_pl_outcome,apply=.ra_pl_apply,legal=.ra_pl_legal);return(fn(...))
  }
  if(identical(pack$adapter,"precision_shooting")) {
    fn<-switch(operation,validate=.ra_ps_validate,outcome=.ra_ps_outcome,apply=.ra_ps_apply,legal=.ra_ps_legal);return(fn(...))
  }
  if(identical(pack$adapter,"trap")) {
    fn<-switch(operation,validate=.ra_trap_validate,outcome=.ra_trap_outcome,apply=.ra_trap_apply,legal=.ra_trap_legal);return(fn(...))
  }
  if(identical(pack$adapter,"show_jumping")) {
    fn<-switch(operation,validate=.ra_sj_validate,outcome=.ra_sj_outcome,apply=.ra_sj_apply,legal=.ra_sj_legal);return(fn(...))
  }
  if(identical(pack$adapter,"agility")) {
    fn<-switch(operation,validate=.ra_ag_validate,outcome=.ra_ag_outcome,apply=.ra_ag_apply,legal=.ra_ag_legal);return(fn(...))
  }
  if(identical(pack$adapter,"belote")) {
    fn<-switch(operation,validate=.ra_be_validate,outcome=.ra_be_outcome,apply=.ra_be_apply,legal=.ra_be_legal);return(fn(...))
  }
  if(identical(pack$adapter,"jass")) {
    fn<-switch(operation,validate=.ra_ja_validate,outcome=.ra_ja_outcome,apply=.ra_ja_apply,legal=.ra_ja_legal);return(fn(...))
  }
  if(identical(pack$adapter,"diving")) {
    fn<-switch(operation,validate=.ra_di_validate,outcome=.ra_di_outcome,apply=.ra_di_apply,legal=.ra_di_legal);return(fn(...))
  }
  if(identical(pack$adapter,"dressage")) {
    fn<-switch(operation,validate=.ra_dr_validate,outcome=.ra_dr_outcome,apply=.ra_dr_apply,legal=.ra_dr_legal);return(fn(...))
  }
  if(identical(pack$adapter,"kendo")) {
    fn<-switch(operation,validate=.ra_ke_validate,outcome=.ra_ke_outcome,apply=.ra_ke_apply,legal=.ra_ke_legal);return(fn(...))
  }
  if(!is.null(pack$gymnastics_rules)) {
    fn<-switch(operation,validate=.ra_gy_validate,outcome=.ra_gy_outcome,apply=.ra_gy_apply,legal=.ra_gy_legal);return(fn(...,config=pack$gymnastics_rules))
  }
  if(identical(pack$adapter,"kubb")) {
    fn<-switch(operation,validate=.ra_ku_validate,outcome=.ra_ku_outcome,apply=.ra_ku_apply,legal=.ra_ku_legal);return(fn(...))
  }
  if(identical(pack$adapter,"shuffleboard")) {
    fn<-switch(operation,validate=.ra_sb_validate,outcome=.ra_sb_outcome,apply=.ra_sb_apply,legal=.ra_sb_legal);return(fn(...))
  }
  if(identical(pack$adapter,"golf")) {
    fn<-switch(operation,validate=.ra_golf_validate,outcome=.ra_golf_outcome,apply=.ra_golf_apply,legal=.ra_golf_legal);return(fn(...))
  }
  if(identical(pack$adapter,"disc_golf")) {
    fn<-switch(operation,validate=.ra_dg_validate,outcome=.ra_dg_outcome,apply=.ra_dg_apply,legal=.ra_dg_legal);return(fn(...))
  }
  if(identical(pack$adapter,"breaking")) {
    fn<-switch(operation,validate=.ra_break_validate,outcome=.ra_break_outcome,apply=.ra_break_apply,legal=.ra_break_legal);return(fn(...))
  }
  if(identical(pack$adapter,"parkour_speed")) {
    fn<-switch(operation,validate=.ra_parkour_validate,outcome=.ra_parkour_outcome,apply=.ra_parkour_apply,legal=.ra_parkour_legal);return(fn(...))
  }
  if(identical(pack$adapter,"sailing")) {
    fn<-switch(operation,validate=.ra_sailing_validate,outcome=.ra_sailing_outcome,apply=.ra_sailing_apply,legal=.ra_sailing_legal);return(fn(...))
  }
  if(identical(pack$adapter,"waterski")) {
    fn<-switch(operation,validate=.ra_waterski_validate,outcome=.ra_waterski_outcome,apply=.ra_waterski_apply,legal=.ra_waterski_legal);return(fn(...))
  }
  if(identical(pack$adapter,"cross_country")) {
    fn<-switch(operation,validate=.ra_cross_country_validate,outcome=.ra_cross_country_outcome,apply=.ra_cross_country_apply,legal=.ra_cross_country_legal);return(fn(...))
  }
  if(identical(pack$adapter,"biathlon")) {
    fn<-switch(operation,validate=.ra_biathlon_validate,outcome=.ra_biathlon_outcome,apply=.ra_biathlon_apply,legal=.ra_biathlon_legal);return(fn(...))
  }
  if(identical(pack$adapter,"ski_jumping")) {
    fn<-switch(operation,validate=.ra_ski_jump_validate,outcome=.ra_ski_jump_outcome,apply=.ra_ski_jump_apply,legal=.ra_ski_jump_legal);return(fn(...))
  }
  if(identical(pack$adapter,"nordic_combined")) {
    fn<-switch(operation,validate=.ra_nordic_validate,outcome=.ra_nordic_outcome,apply=.ra_nordic_apply,legal=.ra_nordic_legal);return(fn(...))
  }
  if(identical(pack$adapter,"rally")) {
    fn<-switch(operation,validate=.ra_rally_validate,outcome=.ra_rally_outcome,apply=.ra_rally_apply,legal=.ra_rally_legal);return(fn(...))
  }
  if(identical(pack$adapter,"schafkopf")) {
    fn<-switch(operation,validate=.ra_schafkopf_validate,outcome=.ra_schafkopf_outcome,apply=.ra_schafkopf_apply,legal=.ra_schafkopf_legal);return(fn(...))
  }
  if(identical(pack$adapter,"rope_skipping")) {
    fn<-switch(operation,validate=.ra_rope_skipping_validate,outcome=.ra_rope_skipping_outcome,apply=.ra_rope_skipping_apply,legal=.ra_rope_skipping_legal);return(fn(...))
  }
  if(!is.null(pack$course_race_rules)) {
    fn<-switch(operation,validate=.ra_course_validate,outcome=.ra_course_outcome,apply=.ra_course_apply,legal=.ra_course_legal);return(fn(...,config=pack$course_race_rules))
  }
  if(identical(pack$adapter,"skat")) {
    fn<-switch(operation,validate=.ra_skat_validate,outcome=.ra_skat_outcome,apply=.ra_skat_apply,legal=.ra_skat_legal);return(fn(...))
  }
  if(identical(pack$adapter,"doppelkopf")) {
    fn<-switch(operation,validate=.ra_doppelkopf_validate,outcome=.ra_doppelkopf_outcome,apply=.ra_doppelkopf_apply,legal=.ra_doppelkopf_legal);return(fn(...))
  }
  if(identical(pack$adapter,"boccia")) {
    fn<-switch(operation,validate=.ra_boccia_validate,outcome=.ra_boccia_outcome,apply=.ra_boccia_apply,legal=.ra_boccia_legal);return(fn(...))
  }
  if(identical(pack$adapter,"fencing")) {
    fn<-switch(operation,validate=.ra_fencing_validate,outcome=.ra_fencing_outcome,apply=.ra_fencing_apply,legal=.ra_fencing_legal);return(fn(...))
  }
  if(identical(pack$adapter,"aerials")) {
    fn<-switch(operation,validate=.ra_aerials_validate,outcome=.ra_aerials_outcome,apply=.ra_aerials_apply,legal=.ra_aerials_legal);return(fn(...))
  }
  if(identical(pack$adapter,"handball")) {
    fn<-switch(operation,validate=.ra_handball_validate,outcome=.ra_handball_outcome,apply=.ra_handball_apply,legal=.ra_handball_legal);return(fn(...))
  }
  if(!is.null(pack$panel_final_rules)) {
    fn<-switch(operation,validate=.ra_panel_final_validate,outcome=.ra_panel_final_outcome,apply=.ra_panel_final_apply,legal=.ra_panel_final_legal);return(fn(...,config=pack$panel_final_rules))
  }
  if(identical(pack$adapter,"dancesport")) {
    fn<-switch(operation,validate=.ra_dancesport_validate,outcome=.ra_dancesport_outcome,apply=.ra_dancesport_apply,legal=.ra_dancesport_legal);return(fn(...))
  }
  if(identical(pack$adapter,"figure_skating")) {
    fn<-switch(operation,validate=.ra_figure_skating_validate,outcome=.ra_figure_skating_outcome,apply=.ra_figure_skating_apply,legal=.ra_figure_skating_legal);return(fn(...))
  }
  if(identical(pack$adapter,"surfing")) {
    fn<-switch(operation,validate=.ra_surfing_validate,outcome=.ra_surfing_outcome,apply=.ra_surfing_apply,legal=.ra_surfing_legal);return(fn(...))
  }
  if(identical(pack$adapter,"field_hockey")) {
    fn<-switch(operation,validate=.ra_field_hockey_validate,outcome=.ra_field_hockey_outcome,apply=.ra_field_hockey_apply,legal=.ra_field_hockey_legal);return(fn(...))
  }
  if(identical(pack$adapter,"pelota")) {
    fn<-switch(operation,validate=.ra_pelota_validate,outcome=.ra_pelota_outcome,apply=.ra_pelota_apply,legal=.ra_pelota_legal);return(fn(...))
  }
  if(identical(pack$adapter,"tamburello")) {
    fn<-switch(operation,validate=.ra_tamburello_validate,outcome=.ra_tamburello_outcome,apply=.ra_tamburello_apply,legal=.ra_tamburello_legal);return(fn(...))
  }
  if(identical(pack$adapter,"real_tennis")) {
    fn<-switch(operation,validate=.ra_real_tennis_validate,outcome=.ra_real_tennis_outcome,apply=.ra_real_tennis_apply,legal=.ra_real_tennis_legal);return(fn(...))
  }
  if(identical(pack$adapter,"tchoukball")) {
    fn<-switch(operation,validate=.ra_tchoukball_validate,outcome=.ra_tchoukball_outcome,apply=.ra_tchoukball_apply,legal=.ra_tchoukball_legal);return(fn(...))
  }
  if(identical(pack$adapter,"sambo")) {
    fn<-switch(operation,validate=.ra_sambo_validate,outcome=.ra_sambo_outcome,apply=.ra_sambo_apply,legal=.ra_sambo_legal);return(fn(...))
  }
  if(identical(pack$adapter,"taekwondo")) {
    fn<-switch(operation,validate=.ra_taekwondo_validate,outcome=.ra_taekwondo_outcome,apply=.ra_taekwondo_apply,legal=.ra_taekwondo_legal);return(fn(...))
  }
  if(identical(pack$adapter,"sanda")) {
    fn<-switch(operation,validate=.ra_sanda_validate,outcome=.ra_sanda_outcome,apply=.ra_sanda_apply,legal=.ra_sanda_legal);return(fn(...))
  }
  if(identical(pack$adapter,"savate")) {
    fn<-switch(operation,validate=.ra_savate_validate,outcome=.ra_savate_outcome,apply=.ra_savate_apply,legal=.ra_savate_legal);return(fn(...))
  }
  if(identical(pack$adapter,"fistball")) {
    fn<-switch(operation,validate=.ra_fistball_validate,outcome=.ra_fistball_outcome,apply=.ra_fistball_apply,legal=.ra_fistball_legal);return(fn(...))
  }
  if(identical(pack$adapter,"korfball")) {
    fn<-switch(operation,validate=.ra_korfball_validate,outcome=.ra_korfball_outcome,apply=.ra_korfball_apply,legal=.ra_korfball_legal);return(fn(...))
  }
  if(identical(pack$adapter,"golf_croquet")) {
    fn<-switch(operation,validate=.ra_golf_croquet_validate,outcome=.ra_golf_croquet_outcome,apply=.ra_golf_croquet_apply,legal=.ra_golf_croquet_legal);return(fn(...))
  }
  if(pack$adapter %in% c("baseball","softball")) {
    fn<-switch(operation,validate=.ra_diamond_validate,outcome=.ra_diamond_outcome,apply=.ra_diamond_apply,legal=.ra_diamond_legal);return(fn(...))
  }
  if(identical(pack$adapter,"flag_football")) {
    fn<-switch(operation,validate=.ra_flag_validate,outcome=.ra_flag_outcome,apply=.ra_flag_apply,legal=.ra_flag_legal);return(fn(...))
  }
  if(identical(pack$adapter,"mahjong")) {
    fn<-switch(operation,validate=.ra_mahjong_validate,outcome=.ra_mahjong_outcome,apply=.ra_mahjong_apply,legal=.ra_mahjong_legal);return(fn(...))
  }
  if(identical(pack$adapter,"cricket")) {
    fn<-switch(operation,validate=.ra_cricket_validate,outcome=.ra_cricket_outcome,apply=.ra_cricket_apply,legal=.ra_cricket_legal);return(fn(...))
  }
  if(identical(pack$adapter,"tarock")) {
    fn<-switch(operation,validate=.ra_tarock_validate,outcome=.ra_tarock_outcome,apply=.ra_tarock_apply,legal=.ra_tarock_legal);return(fn(...))
  }
  if(identical(pack$adapter,"ice_hockey")) {
    fn<-switch(operation,validate=.ra_icehockey_validate,outcome=.ra_handball_outcome,apply=.ra_icehockey_apply,legal=.ra_icehockey_legal);return(fn(...))
  }
  if(identical(pack$adapter,"kho_kho")) {
    fn<-switch(operation,validate=.ra_khokho_validate,outcome=.ra_handball_outcome,apply=.ra_khokho_apply,legal=.ra_khokho_legal);return(fn(...))
  }
  if(identical(pack$adapter,"american_football")) {
    fn<-switch(operation,validate=.ra_amfootball_validate,outcome=.ra_flag_outcome,apply=.ra_amfootball_apply,legal=.ra_amfootball_legal);return(fn(...))
  }
  if(identical(pack$adapter,"water_polo")) {
    fn<-switch(operation,validate=.ra_waterpolo_validate,outcome=.ra_waterpolo_outcome,apply=.ra_waterpolo_apply,legal=.ra_waterpolo_legal);return(fn(...))
  }
  if(identical(pack$adapter,"polo")) {
    fn<-switch(operation,validate=.ra_polo_validate,outcome=.ra_polo_outcome,apply=.ra_polo_apply,legal=.ra_polo_legal);return(fn(...))
  }
  if(identical(pack$adapter,"underwater_hockey")) {
    fn<-switch(operation,validate=.ra_uwhockey_validate,outcome=.ra_uwhockey_outcome,apply=.ra_uwhockey_apply,legal=.ra_uwhockey_legal);return(fn(...))
  }
  if(identical(pack$adapter,"canoe_polo")) {
    fn<-switch(operation,validate=.ra_canoepolo_validate,outcome=.ra_canoepolo_outcome,apply=.ra_canoepolo_apply,legal=.ra_canoepolo_legal);return(fn(...))
  }
  if(identical(pack$adapter,"kabaddi")) {
    fn<-switch(operation,validate=.ra_kabaddi_validate,outcome=.ra_kabaddi_outcome,apply=.ra_kabaddi_apply,legal=.ra_kabaddi_legal);return(fn(...))
  }
  if(identical(pack$adapter,"sepaktakraw")) {
    fn<-switch(operation,validate=.ra_takraw_validate,outcome=.ra_takraw_outcome,apply=.ra_takraw_apply,legal=.ra_takraw_legal);return(fn(...))
  }
  if(identical(pack$adapter,"underwater_rugby")) {
    fn<-switch(operation,validate=.ra_uwrugby_validate,outcome=.ra_uwrugby_outcome,apply=.ra_uwrugby_apply,legal=.ra_uwrugby_legal);return(fn(...))
  }
  if(identical(pack$adapter,"floorball")) {
    fn<-switch(operation,validate=.ra_floorball_validate,outcome=.ra_floorball_outcome,apply=.ra_floorball_apply,legal=.ra_floorball_legal);return(fn(...))
  }
  if(identical(pack$adapter,"lacrosse_sixes")) {
    fn<-switch(operation,validate=.ra_lacrosse_validate,outcome=.ra_lacrosse_outcome,apply=.ra_lacrosse_apply,legal=.ra_lacrosse_legal);return(fn(...))
  }
  if(identical(pack$adapter,"trampoline")) {
    fn<-switch(operation,validate=.ra_trampoline_validate,outcome=.ra_trampoline_outcome,apply=.ra_trampoline_apply,legal=.ra_trampoline_legal);return(fn(...))
  }
  if(identical(pack$adapter,"bodybuilding")) {
    fn<-switch(operation,validate=.ra_bodybuilding_validate,outcome=.ra_bodybuilding_outcome,apply=.ra_bodybuilding_apply,legal=.ra_bodybuilding_legal);return(fn(...))
  }
  if(identical(pack$adapter,"triathlon")) {
    fn<-switch(operation,validate=.ra_triathlon_validate,outcome=.ra_triathlon_outcome,apply=.ra_triathlon_apply,legal=.ra_triathlon_legal);return(fn(...))
  }
  if(identical(pack$adapter,"futsal")) {
    fn<-switch(operation,validate=.ra_futsal_validate,outcome=.ra_futsal_outcome,apply=.ra_futsal_apply,legal=.ra_futsal_legal);return(fn(...))
  }
  if(identical(pack$adapter,"roundnet")) {
    fn<-switch(operation,validate=.ra_roundnet_validate,outcome=.ra_roundnet_outcome,apply=.ra_roundnet_apply,legal=.ra_roundnet_legal);return(fn(...))
  }
  if(identical(pack$adapter,"curling")) {
    fn<-switch(operation,validate=.ra_curling_validate,outcome=.ra_curling_outcome,apply=.ra_curling_apply,legal=.ra_curling_legal);return(fn(...))
  }
  if(identical(pack$adapter,"icestock")) {
    fn<-switch(operation,validate=.ra_icestock_validate,outcome=.ra_icestock_outcome,apply=.ra_icestock_apply,legal=.ra_icestock_legal);return(fn(...))
  }
  if(identical(pack$adapter,"dodgeball")) {
    fn<-switch(operation,validate=.ra_dodgeball_validate,outcome=.ra_dodgeball_outcome,apply=.ra_dodgeball_apply,legal=.ra_dodgeball_legal);return(fn(...))
  }
  if(identical(pack$adapter,"karate")) {
    fn<-switch(operation,validate=.ra_karate_validate,outcome=.ra_karate_outcome,apply=.ra_karate_apply,legal=.ra_karate_legal);return(fn(...))
  }
  if(identical(pack$adapter,"wrestling")) {
    fn<-switch(operation,validate=.ra_wrestling_validate,outcome=.ra_wrestling_outcome,apply=.ra_wrestling_apply,legal=.ra_wrestling_legal);return(fn(...))
  }
  if(!is.null(pack$board_racing_kind)) {
    fn<-switch(operation,validate=.ra_board_racing_validate,outcome=.ra_board_racing_outcome,apply=.ra_board_racing_apply,legal=.ra_board_racing_legal);return(fn(...,kind=pack$board_racing_kind))
  }
  if(!is.null(pack$lap_course_rules)) {
    fn<-switch(operation,validate=.ra_lap_course_validate,outcome=.ra_lap_course_outcome,apply=.ra_lap_course_apply,legal=.ra_lap_course_legal);return(fn(...,config=pack$lap_course_rules))
  }
  if(identical(pack$adapter,"canasta")) {
    fn<-switch(operation,validate=.ra_canasta_validate,outcome=.ra_canasta_outcome,apply=.ra_canasta_apply,legal=.ra_canasta_legal);return(fn(...))
  }
  if(identical(pack$adapter,"eventing")) {
    fn<-switch(operation,validate=.ra_eventing_validate,outcome=.ra_eventing_outcome,apply=.ra_eventing_apply,legal=.ra_eventing_legal);return(fn(...))
  }
  if(pack$adapter %in% c("motor_race_motorcycle","motor_race_motocross")) {
    fn<-switch(operation,validate=.ra_motor_validate,outcome=.ra_motor_outcome,apply=.ra_motor_apply,legal=.ra_motor_legal);return(fn(...,kind=sub("motor_race_","",pack$adapter,fixed=TRUE)))
  }
  if(pack$adapter %in% c("circuit_race_circuit","circuit_race_kart")) {
    fn<-switch(operation,validate=.ra_circuit_validate,outcome=.ra_circuit_outcome,apply=.ra_circuit_apply,legal=.ra_circuit_legal);return(fn(...,kind=sub("circuit_race_","",pack$adapter,fixed=TRUE)))
  }
  if(identical(pack$adapter,"decathlon")) {
    fn<-switch(operation,validate=.ra_decathlon_validate,outcome=.ra_decathlon_outcome,apply=.ra_decathlon_apply,legal=.ra_decathlon_legal);return(fn(...))
  }
  if(pack$adapter %in% c("gaelic_football","gaelic_hurling")) {
    fn<-switch(operation,validate=.ra_gaelic_validate,outcome=.ra_gaelic_outcome,apply=.ra_gaelic_apply,legal=.ra_gaelic_legal);return(fn(...,kind=sub("gaelic_","",pack$adapter,fixed=TRUE)))
  }
  if(identical(pack$adapter,"australian_football")) {
    fn<-switch(operation,validate=.ra_aussie_validate,outcome=.ra_aussie_outcome,apply=.ra_aussie_apply,legal=.ra_aussie_legal);return(fn(...))
  }
  if(pack$adapter %in% c("rugby_union","rugby_league")) {
    fn<-switch(operation,validate=.ra_rugby_validate,outcome=.ra_rugby_outcome,apply=.ra_rugby_apply,legal=.ra_rugby_legal);return(fn(...,code=sub("rugby_","",pack$adapter,fixed=TRUE)))
  }
  if(identical(pack$adapter,"snooker")) {
    fn<-switch(operation,validate=.ra_snooker_validate,outcome=.ra_snooker_outcome,apply=.ra_snooker_apply,legal=.ra_snooker_legal);return(fn(...))
  }
  if(identical(pack$adapter,"netball")) {
    fn<-switch(operation,validate=.ra_netball_validate,outcome=.ra_netball_outcome,apply=.ra_netball_apply,legal=.ra_netball_legal);return(fn(...))
  }
  if(identical(pack$adapter,"basketball")) {
    fn<-switch(operation,validate=.ra_basketball_validate,outcome=.ra_basketball_outcome,apply=.ra_basketball_apply,legal=.ra_basketball_legal);return(fn(...))
  }
  if(identical(pack$adapter,"ultimate")) {
    fn<-switch(operation,validate=.ra_ultimate_validate,outcome=.ra_ultimate_outcome,apply=.ra_ultimate_apply,legal=.ra_ultimate_legal);return(fn(...))
  }
  if(!is.null(pack$race_rules)) {
    fn<-switch(operation,validate=.ra_race_validate,outcome=.ra_race_outcome,apply=.ra_race_apply,legal=.ra_race_legal);return(fn(...,config=pack$race_rules))
  }
  if(identical(pack$adapter,"squash")) {
    fn<-switch(operation,validate=.ra_sq_validate,outcome=.ra_sq_outcome,apply=.ra_sq_apply,legal=.ra_sq_legal);return(fn(...))
  }
  if(identical(pack$adapter,"domino")) {
    fn<-switch(operation,validate=.ra_dom_validate,outcome=.ra_dom_outcome,apply=.ra_dom_apply,legal=.ra_dom_legal);return(fn(...))
  }
  if(identical(pack$adapter,"shut_box")) {
    fn<-switch(operation,validate=.ra_box_validate,outcome=.ra_box_outcome,apply=.ra_box_apply,legal=.ra_box_legal);return(fn(...))
  }
  if(identical(pack$adapter,"darts")) {
    fn<-switch(operation,validate=.ra_darts_validate,outcome=.ra_darts_outcome,apply=.ra_darts_apply,legal=.ra_darts_legal);return(fn(...))
  }
  if(identical(pack$adapter,"minesweeper")) {
    fn<-switch(operation,validate=.ra_mine_validate,outcome=.ra_mine_outcome,apply=.ra_mine_apply,legal=.ra_mine_legal);return(fn(...))
  }
  if(identical(pack$adapter,"hangman")) {
    fn<-switch(operation,validate=.ra_hm_validate,outcome=.ra_hm_outcome,apply=.ra_hm_apply,legal=.ra_hm_legal);return(fn(...))
  }
  if(identical(pack$adapter,"sudoku")) {
    fn<-switch(operation,validate=.ra_sudoku_validate,outcome=.ra_sudoku_outcome,apply=.ra_sudoku_apply,legal=.ra_sudoku_legal);return(fn(...))
  }
  if(!is.null(pack$connection_rules)) {
    fn<-switch(operation,validate=.ra_connection_validate,outcome=.ra_connection_outcome,apply=.ra_connection_apply,legal=.ra_connection_legal);return(fn(...,config=pack$connection_rules))
  }
  if(identical(pack$adapter,"football")) {
    fn<-switch(operation,validate=.ra_fb_validate,outcome=.ra_fb_outcome,apply=.ra_fb_apply,legal=.ra_fb_legal);return(fn(...))
  }
  if(!is.null(pack$scoring_awards)) {
    fn<-switch(operation,validate=.ra_scoring_validate,outcome=.ra_scoring_outcome,apply=.ra_scoring_apply,legal=.ra_scoring_legal);return(fn(...,awards=pack$scoring_awards))
  }
  if(identical(pack$adapter,"tic_tac_toe")) {
    fn<-switch(operation,validate=.ra_ttt_validate,outcome=.ra_ttt_outcome,apply=.ra_ttt_apply,legal=.ra_ttt_legal)
    return(fn(...))
  }
  if(identical(pack$adapter,"tennis")) {
    fn<-switch(operation,validate=.ra_tennis_validate,outcome=.ra_tennis_outcome,apply=.ra_tennis_apply,legal=.ra_tennis_legal)
    return(fn(...))
  }
  if(identical(pack$adapter,"reversi")) {
    fn<-switch(operation,validate=.ra_reversi_validate,outcome=.ra_reversi_outcome,apply=.ra_reversi_apply,legal=.ra_reversi_legal);return(fn(...))
  }
  if(pack$adapter %in% c("table_tennis","badminton")) {
    fn<-switch(operation,validate=.ra_racketscore_validate,outcome=.ra_racketscore_outcome,apply=.ra_racketscore_apply,legal=.ra_racketscore_legal);return(fn(...,sport=pack$adapter))
  }
  if(identical(pack$adapter,"rummy")) {
    fn<-switch(operation,validate=.ra_rummy_validate,outcome=.ra_rummy_outcome,apply=.ra_rummy_apply,legal=.ra_rummy_legal);return(fn(...))
  }
  if(identical(pack$adapter,"mau_mau")) {
    fn<-switch(operation,validate=.ra_mmau_validate,outcome=.ra_mmau_outcome,apply=.ra_mmau_apply,legal=.ra_mmau_legal);return(fn(...))
  }
  if(identical(pack$adapter,"thirty_one")) {
    fn<-switch(operation,validate=.ra_31_validate,outcome=.ra_31_outcome,apply=.ra_31_apply,legal=.ra_31_legal);return(fn(...))
  }
  if(identical(pack$adapter,"racquetball")) {
    fn<-switch(operation,validate=.ra_rb_validate,outcome=.ra_rb_outcome,apply=.ra_rb_apply,legal=.ra_rb_legal);return(fn(...))
  }
  if(identical(pack$adapter,"carom")) {
    fn<-switch(operation,validate=.ra_carom_validate,outcome=.ra_carom_outcome,apply=.ra_carom_apply,legal=.ra_carom_legal);return(fn(...))
  }
  if(identical(pack$adapter,"nine_ball")) {
    fn<-switch(operation,validate=.ra_9_validate,outcome=.ra_9_outcome,apply=.ra_9_apply,legal=.ra_9_legal);return(fn(...))
  }
  if(identical(pack$adapter,"petanque")) {
    fn<-switch(operation,validate=.ra_pet_validate,outcome=.ra_pet_outcome,apply=.ra_pet_apply,legal=.ra_pet_legal);return(fn(...))
  }
  if(identical(pack$adapter,"tafl")) {
    fn<-switch(operation,validate=.ra_tafl_validate,outcome=.ra_tafl_outcome,apply=.ra_tafl_apply,legal=.ra_tafl_legal);return(fn(...))
  }
  if(identical(pack$adapter,"xiangqi")) {
    fn<-switch(operation,validate=.ra_xq_validate,outcome=.ra_xq_outcome,apply=.ra_xq_apply,legal=.ra_xq_legal);return(fn(...))
  }
  if(identical(pack$adapter,"shogi")) {
    fn<-switch(operation,validate=.ra_sh_validate,outcome=.ra_sh_outcome,apply=.ra_sh_apply,legal=.ra_sh_legal);return(fn(...))
  }
  if(identical(pack$adapter,"backgammon")) {
    fn<-switch(operation,validate=.ra_bg_validate,outcome=.ra_bg_outcome,apply=.ra_bg_apply,legal=.ra_bg_legal);return(fn(...))
  }
  if(identical(pack$adapter,"janggi")) {
    fn<-switch(operation,validate=.ra_jg_validate,outcome=.ra_jg_outcome,apply=.ra_jg_apply,legal=.ra_jg_legal);return(fn(...))
  }
  if(identical(pack$adapter,"minigolf")) {
    fn<-switch(operation,validate=.ra_mg_validate,outcome=.ra_mg_outcome,apply=.ra_mg_apply,legal=.ra_mg_legal);return(fn(...))
  }
  if(identical(pack$adapter,"ninepin")) {
    fn<-switch(operation,validate=.ra_np_validate,outcome=.ra_np_outcome,apply=.ra_np_apply,legal=.ra_np_legal);return(fn(...))
  }
  if(identical(pack$adapter,"beach_tennis")) {
    fn<-switch(operation,validate=.ra_bt_validate,outcome=.ra_bt_outcome,apply=.ra_bt_apply,legal=.ra_bt_legal);return(fn(...))
  }
  if(identical(pack$adapter,"padel")) {
    fn<-switch(operation,validate=.ra_pd_validate,outcome=.ra_pd_outcome,apply=.ra_pd_apply,legal=.ra_pd_legal);return(fn(...))
  }
  if(identical(pack$adapter,"bridge")) {
    fn<-switch(operation,validate=.ra_br_validate,outcome=.ra_br_outcome,apply=.ra_br_apply,legal=.ra_br_legal);return(fn(...))
  }
  if(identical(pack$adapter,"judo")) {
    fn<-switch(operation,validate=.ra_ju_validate,outcome=.ra_ju_outcome,apply=.ra_ju_apply,legal=.ra_ju_legal);return(fn(...))
  }
  if(identical(pack$adapter,"bjj")) {
    fn<-switch(operation,validate=.ra_bjj_validate,outcome=.ra_bjj_outcome,apply=.ra_bjj_apply,legal=.ra_bjj_legal);return(fn(...))
  }
  if(identical(pack$adapter,"sixty_six")) {
    fn<-switch(operation,validate=.ra_66_validate,outcome=.ra_66_outcome,apply=.ra_66_apply,legal=.ra_66_legal);return(fn(...))
  }
  if(identical(pack$adapter,"trail_running")) {
    fn<-switch(operation,validate=.ra_tr_validate,outcome=.ra_tr_outcome,apply=.ra_tr_apply,legal=.ra_tr_legal);return(fn(...))
  }
  if(identical(pack$adapter,"canoe_slalom")) {
    fn<-switch(operation,validate=.ra_sl_validate,outcome=.ra_sl_outcome,apply=.ra_sl_apply,legal=.ra_sl_legal);return(fn(...))
  }
  if(identical(pack$adapter,"craps")) {
    fn<-switch(operation,validate=.ra_ca_validate,outcome=.ra_ca_outcome,apply=.ra_ca_apply,legal=.ra_ca_legal);return(fn(...))
  }
  if(identical(pack$adapter,"volleyball")) {
    fn<-switch(operation,validate=.ra_vb_validate,outcome=.ra_vb_outcome,apply=.ra_vb_apply,legal=.ra_vb_legal);return(fn(...))
  }
  if(identical(pack$adapter,"armwrestling")) {
    fn<-switch(operation,validate=.ra_aw_validate,outcome=.ra_aw_outcome,apply=.ra_aw_apply,legal=.ra_aw_legal);return(fn(...))
  }
  if(identical(pack$adapter,"cribbage")) {
    fn<-switch(operation,validate=.ra_cr_validate,outcome=.ra_cr_outcome,apply=.ra_cr_apply,legal=.ra_cr_legal);return(fn(...))
  }
  if(!is.null(pack$combat_rules)) {
    fn<-switch(operation,validate=.ra_jc_validate,outcome=.ra_jc_outcome,apply=.ra_jc_apply,legal=.ra_jc_legal);return(fn(...,config=pack$combat_rules))
  }
  if(identical(pack$adapter,"muay_thai")) {
    fn<-switch(operation,validate=.ra_mt_validate,outcome=.ra_mt_outcome,apply=.ra_mt_apply,legal=.ra_mt_legal);return(fn(...))
  }
  .ra_fail("Unknown native adapter",code="INTERNAL_ERROR")
}
