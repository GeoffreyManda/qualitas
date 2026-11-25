# Reproduce from manuscript/; reuse qualitas sources and its synthetic fixture.
pkgload::load_all("..", quiet = TRUE)
library("qualitas")
library("ggplot2")
library("dplyr")
set.seed(20261124)
options(width = 75)
article_options <- qualitas_options(as_of = as.Date("2026-04-01"))
intake <- data.frame(interview_id=c("I01","I02","I02","I04"),
                     age=c(28L,240L,41L,35L),district=c("North","South","South",NA))
intake_spec <- spec_complete(tibble::tibble(
  column=c("interview_id","age","district"),type=c("text","integer","category"),
  required=TRUE,unique=c(TRUE,FALSE,FALSE),min=c(NA,"18",NA),
  max=c(NA,"100",NA),allowed=c(NA,NA,"North;South")))
intake_checks <- c("CO02","UN02","VA02")
initial <- assess(intake,intake_spec,checks=intake_checks,options=article_options)
decisions <- cleaning_log(initial)
decisions$action <- "keep"
decisions$reason <- "Awaiting review of duplicate identifier and district"
selected <- decisions$check_id=="VA02" & decisions$row==2L
decisions$action[selected] <- "change"
decisions$new_value[selected] <- "24"
decisions$reason[selected] <- "Synthetic exercise specifies age 24"
decisions$decided_by[selected] <- "Example reviewer"
corrected <- suppressMessages(apply_log(intake,decisions))
reviewed <- assess(corrected,intake_spec,checks=intake_checks,options=article_options)
intake_measures <- measures(initial) |> select(check_id,unit,checked,flagged,score)
stopifnot(sum(is.na(intake))==1L,
  sum(duplicated(intake$interview_id)|duplicated(intake$interview_id,fromLast=TRUE))==2L,
  sum(intake$age<18|intake$age>100)==1L,isTRUE(all.equal(corrected$age,c(28,24,41,35))))
validation <- validate::confront(intake,validate::validator(age>=18,age<=100))
validation_matrix <- validate::values(validation)
stopifnot(sum(!validation_matrix)==measures(initial)$flagged[measures(initial)$check_id=="VA02"],
  identical(which(rowSums(!validation_matrix)>0),
            as.integer(issues(initial)$row[issues(initial)$check_id=="VA02"])))
ext <- function(file) system.file("extdata",file,package="qualitas")
survey <- readr::read_csv(ext("messy_survey.csv"),na="",show_col_types=FALSE)
survey_spec <- read_spec(ext("messy_survey_spec.csv"))
survey_rules <- read_rules(ext("messy_survey_rules.csv"))
article_checks <- checks_catalogue()$check_id
survey_result <- suppressWarnings(assess(survey,survey_spec,survey_rules,
                      options=article_options,checks=article_checks))
profile <- measures(survey_result)
stopifnot(!any(profile$status=="error"))

# Shared requirements use the same explicitly normalised input in every package.
comparison_inputs <- list(
  intake = data.frame(row_id = seq_len(nrow(intake)), id = intake$interview_id,
                      age = intake$age, district = intake$district),
  survey = data.frame(row_id = seq_len(nrow(survey)), id = survey$interview_id,
                      age = suppressWarnings(as.numeric(survey$age_years)), district = survey$district))
compare_requirements <- function(data, dataset) {
  data$district[is.na(data$district) | trimws(data$district) == ""] <- NA_character_
  spec <- spec_complete(tibble::tibble(
    column = c("id", "age", "district"), type = c("text", "number", "text"),
    required = c(FALSE, FALSE, TRUE), unique = c(TRUE, FALSE, FALSE),
    min = c(NA, "18", NA), max = c(NA, "100", NA)))
  q <- assess(data, spec, checks = c("VA02", "CO02", "UN02"))
  qrows <- lapply(c("VA02", "CO02", "UN02"), function(id)
    sort(unique(issues(q)$row[issues(q)$check_id == id])))
  v <- validate::values(validate::confront(data, validate::validator(
    bounds = age >= 18 & age <= 100, required = !is.na(district),
    key = is_unique(id))))
  vrows <- lapply(seq_len(ncol(v)), function(j) which(v[, j] %in% FALSE))
  agent <- pointblank::create_agent(data) |>
    pointblank::col_vals_between(columns = "age", left = 18, right = 100, na_pass = TRUE) |>
    pointblank::col_vals_not_null(columns = "district") |>
    pointblank::rows_distinct(columns = "id") |>
    pointblank::interrogate(progress = FALSE)
  all_extracts <- pointblank::get_data_extracts(agent)
  prows <- lapply(seq_len(3), function(i) {
    extracted <- all_extracts[[as.character(i)]]
    if (is.null(extracted)) integer() else sort(extracted$row_id)
  })
  meta <- data.frame(VAR_NAMES = "age", LABEL = "age", DATA_TYPE = "integer",
    HARD_LIMITS = "[18;100]", SCALE_LEVEL = "ratio", VARIABLE_ROLE = "primary",
    MISSING_LIST = NA_character_, JUMP_LIST = NA_character_)
  dq <- dataquieR::con_limit_deviations(resp_vars = "age", study_data = data["age"],
    meta_data = meta, label_col = "LABEL", return_flagged_study_data = TRUE)
  dqrows <- which(as.character(dq$FlaggedStudyData[["age_HARD_LIMITS"]]) %in% c("below", "above"))
  for (i in seq_len(3)) {
    stopifnot(identical(as.integer(qrows[[i]]), as.integer(vrows[[i]])),
              identical(as.integer(qrows[[i]]), as.integer(prows[[i]])))
  }
  stopifnot(identical(as.integer(qrows[[1]]), as.integer(dqrows)))
  expected <- list(which(!is.na(data$age) & (data$age < 18 | data$age > 100)),
                   which(is.na(data$district)),
                   which(duplicated(data$id) | duplicated(data$id, fromLast = TRUE)))
  stopifnot(identical(lapply(qrows, as.integer), lapply(expected, as.integer)))
  tibble::tibble(dataset = dataset, rows = nrow(data),
    requirement = c("Age bounds", "Required district", "Unique identifier"),
    qualitas = lengths(qrows), validate = lengths(vrows), pointblank = lengths(prows),
    dataquieR = c(length(dqrows), NA_integer_, NA_integer_),
    row_ids = vapply(qrows, function(x) paste(x, collapse = ", "), character(1)))
}
comparison <- purrr::imap_dfr(comparison_inputs, compare_requirements)
# Distinguish identical failed rows from different missing-value denominators.
boundary <- data.frame(age = c(28, 240, 41, NA_real_))
bspec <- spec_complete(tibble::tibble(column = "age", type = "number", min = "18", max = "100"))
bq <- measures(assess(boundary, bspec, checks = "VA02"))
bp <- pointblank::create_agent(boundary) |>
  pointblank::col_vals_between(columns = "age", left = 18, right = 100, na_pass = TRUE) |>
  pointblank::interrogate(progress = FALSE)
bx <- pointblank::get_agent_x_list(bp)
missing_policy <- list(qualitas = list(checked = bq$checked, flagged = bq$flagged, score = bq$score),
                      pointblank = list(checked = bx$n, flagged = bx$n_failed, score = bx$f_passed))
stopifnot(bq$checked == 3L, bq$flagged == 1L, isTRUE(all.equal(bq$score, 2/3)),
          bx$n == 4L, bx$n_failed == 1L, isTRUE(all.equal(bx$f_passed, 3/4)))

public_counts <- purrr::imap_dfr(list(pbc=survival::pbc,storms=dplyr::storms),function(data,name){
  m <- measures(suppressMessages(assess(data,checks="CO01",options=article_options)))
  stopifnot(m$checked==nrow(data)*ncol(data),m$flagged==sum(is.na(data)))
  tibble::tibble(rows=nrow(data),columns=ncol(data),data=name,checked=m$checked,
                 flagged=m$flagged,percent=100*m$flagged/m$checked)
})
timed_checks <- c("ST05","CO01","CO02","UN02","VA02","CS01")
timing_reference <- Sys.getenv("QUALITAS_REFERENCE_RESULTS",
  if (file.exists("data/original-results.json")) "data/original-results.json" else "measure")
if (timing_reference != "measure") stopifnot(file.exists(timing_reference))
timings <- if (timing_reference != "measure") {
  tibble::as_tibble(jsonlite::read_json(timing_reference,simplifyVector=TRUE)$timings)
} else purrr::map_dfr(c(100L,1000L,10000L,50000L),function(n){
  data <- survey[sample.int(nrow(survey),n,replace=TRUE),]
  run <- function() invisible(suppressWarnings(assess(data,survey_spec,survey_rules,
                                     options=article_options,checks=timed_checks)))
  run()
  tibble::tibble(rows=n,repetition=1:3,
    seconds=vapply(1:3,function(i) unname(system.time(run())[["elapsed"]]),numeric(1)))
})
timing_summary <- timings |> summarise(median=median(seconds),min=min(seconds),
                                       max=max(seconds),.by=rows)
ink <- "#1D1D1F"; blue <- "#0066CC"; muted <- "#6E6E73"; font <- "Helvetica"
article_theme <- theme_minimal(base_size=10,base_family=font)+
  theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),
    panel.grid.major.x=element_line(colour="#E5E5EA",linewidth=.25),
    axis.title=element_text(colour=ink),axis.text=element_text(colour=ink),
    plot.margin=margin(10,14,8,10),legend.position="bottom",legend.title=element_blank(),
    plot.tag=element_text(face="bold",colour=ink))
flow <- function(){
  grid::grid.newpage()
  box <- function(x,y,title,detail,w=.27,h=.16,accent=FALSE){
    grid::grid.roundrect(x,y,w,h,r=grid::unit(3,"pt"),
      gp=grid::gpar(fill="white",col=if(accent)blue else "#B9B9BE",lwd=.8))
    grid::grid.text(title,x,y+.026,gp=grid::gpar(fontfamily=font,fontsize=10,fontface="bold",col=ink))
    grid::grid.text(detail,x,y-.027,gp=grid::gpar(fontfamily=font,fontsize=8.6,col=ink))
  }
  arrow <- function(x0,y0,x1,y1) grid::grid.lines(c(x0,x1),c(y0,y1),
    arrow=grid::arrow(length=grid::unit(4,"pt"),type="closed"),
    gp=grid::gpar(col=blue,fill=blue,lwd=1))
  box(.17,.87,"Snapshot","Rows and columns")
  box(.5,.87,"Specification","Types, keys and bounds")
  box(.83,.87,"Rules","Valid combinations")
  arrow(.17,.79,.39,.70);arrow(.5,.79,.5,.70);arrow(.83,.79,.61,.70)
  box(.5,.62,"Assess","Catalogue and settings",w=.32,accent=TRUE)
  arrow(.5,.54,.5,.49)
  box(.5,.41,"Inspect","Findings, units and scores",w=.36)
  arrow(.34,.33,.17,.25);arrow(.66,.33,.83,.25)
  box(.17,.17,"Report","HTML, Word and tables")
  box(.83,.17,"Record decisions","Keep, change or remove")
  arrow(.83,.25,.83,.62)
  grid::grid.text("Apply log and reassess",.96,.43,rot=90,
                 gp=grid::gpar(fontfamily=font,fontsize=8.6,col=ink))
  arrow(.83,.62,.67,.62)
}
grDevices::cairo_pdf("figures/assessment-flow.pdf",width=5.5,height=3.8,family=font)
flow();grDevices::dev.off()
svglite::svglite("figures/assessment-flow.svg",width=5.5,height=3.8)
flow();grDevices::dev.off()
selected_measures <- profile |>
 filter(check_id %in% c("CO02","UN02","VA01","VA02","FA04","PI01","DO01")) |>
 mutate(label=paste0(check_id,"  ",unname(c(CO02="Required missing",UN02="Repeated keys",VA01="Invalid category",VA02="Out of range",FA04="Interview duration",PI01="Personal-data pattern",DO01="Missing description")[check_id])," [",unit,"]"),
        label=stringr::str_wrap(label,43),rate=100*flagged/checked,
        counts=paste0(flagged,"/",checked)) |> arrange(rate) |>
 mutate(label=factor(label,levels=label))
p1 <- ggplot(selected_measures,aes(rate,label))+
 geom_segment(aes(x=0,xend=rate,yend=label),colour="#B9D9F5",linewidth=1)+
 geom_point(colour=blue,size=2.3)+
 geom_text(aes(label=counts),hjust=-.2,size=2.6,family=font)+
 scale_x_continuous(limits=c(0,8),breaks=c(0,2,4,6),labels=function(x)paste0(x,"%"))+
 labs(x="Flagged assessed units",y=NULL,tag="A")+article_theme+
 theme(axis.text.y=element_text(size=8))
before_after <- bind_rows(mutate(measures(initial),review="Before review"),
                          mutate(measures(reviewed),review="After correction")) |>
 mutate(check=factor(check_id,levels=c("CO02","UN02","VA02"),
                     labels=c("Required\nvalues","Repeated\nkeys","Age range")))
p2 <- ggplot(before_after,aes(check,flagged,colour=review,shape=review,group=review))+
 geom_point(position=position_dodge(.45),size=2.8)+
 scale_colour_manual(values=c("After correction"=blue,"Before review"=muted))+
 scale_shape_manual(values=c("After correction"=16,"Before review"=1))+
 scale_y_continuous(limits=c(0,2.3),breaks=0:2)+
 labs(x=NULL,y="Flagged units",tag="B")+coord_flip()+article_theme+
 theme(axis.text.y=element_text(size=8),legend.text=element_text(size=8),
       legend.key.width=grid::unit(.6,"lines"))
p3 <- ggplot(timings,aes(rows,seconds))+geom_point(colour=blue,shape=1,size=2)+
 geom_line(data=timing_summary,aes(y=median),colour=blue,linewidth=.5)+
 scale_x_log10(breaks=c(100,1000,10000,50000),labels=expression(10^2,10^3,10^4,5%*%10^4))+
 labs(x="Rows (log scale)",y="Elapsed seconds",tag="C")+article_theme
quality <- patchwork::wrap_plots(
  p1, patchwork::wrap_plots(p2+theme(legend.position="none"),p3,nrow=1),
  ncol=1,heights=c(1.25,1))
ggsave("figures/assessment-results.pdf",quality,width=5.5,height=6.7,
       device=grDevices::cairo_pdf,family=font,bg="white")
ggsave("figures/assessment-results.svg",quality,width=5.5,height=6.7,
       device=svglite::svglite,bg="white")
evidence <- list(package_version=as.character(packageVersion("qualitas")),
 source_commit=system2("git",c("-C","..","rev-parse","HEAD"),stdout=TRUE),
 source_sha256=as.list(vapply(list.files("../R",full.names=TRUE),
   function(p) digest::digest(file=p,algo="sha256"),character(1))),as_of=as.character(article_options$as_of),
 catalogue_checks=nrow(checks_catalogue()),excluded_checks=character(),comparison=comparison,missing_policy=missing_policy,
 comparison_versions=as.list(vapply(c("validate","pointblank","dataquieR"),
   function(p) as.character(packageVersion(p)),character(1))),intake=intake_measures,
 after=measures(reviewed),survey_rows=nrow(survey),survey_columns=ncol(survey),
 survey_measures=profile,public_counts=public_counts,timings=timings,timing_summary=timing_summary,
 timing_source=if (timing_reference=="measure") "measured" else "archived observations",
 validate_version=as.character(packageVersion("validate")),system=as.list(Sys.info()),
 session=utils::capture.output(sessionInfo()))
jsonlite::write_json(evidence,"../outputs/qualitas-article-evidence.json",pretty=TRUE,
                     auto_unbox=TRUE,na="null")

utils::sessionInfo()
