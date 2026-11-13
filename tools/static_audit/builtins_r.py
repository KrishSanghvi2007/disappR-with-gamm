# Functions from base R, the recommended packages and the packages disappR imports or suggests. Names not defined in
# the package and not listed here are reported for manual review; the list does not need to be complete.
_BASE = """
abs all any anyNA anyDuplicated aperm append apply approx approxfun array arrayInd as.character as.complex as.Date
as.data.frame as.double as.environment as.factor as.formula as.function as.integer as.list as.logical as.matrix
as.name as.numeric as.numeric_version as.ordered as.POSIXct as.POSIXlt as.symbol as.table as.vector asS4 attr
attributes basename besselJ bitwAnd body bquote browser by bindingIsLocked c casefold cat cbind ceiling character
charmatch chartr chol chol2inv choose class col colMeans colnames colSums colSums complete.cases complex
conditionCall conditionMessage cos cosh crossprod cummax cummin cumprod cumsum cut data.frame data.matrix date
deparse det diag diff difftime digamma dim dimnames dir dir.create dirname do.call double dQuote droplevels
duplicated emptyenv endsWith environment environmentName eval evalq exists exp expand.grid expm1 expression
factor file file.exists file.path file.remove file.copy file.info file.size find findInterval floor for formals
format format.Date formatC function gamma gc get get0 getElement getOption gettextf gl globalenv gregexpr grepl
grep gsub identical identity ifelse integer interaction intersect inherits invisible is.array is.character
is.data.frame is.element is.environment is.factor is.finite is.function is.infinite is.integer is.list
is.logical is.matrix is.na is.name is.nan is.null is.numeric is.ordered is.primitive is.symbol is.vector isFALSE
isTRUE jitter julian kronecker lapply lbeta lchoose length letters LETTERS levels lfactorial lgamma library
list list.files list2env local log log10 log1p log2 logical lower.tri ls make.names make.unique mapply
match match.arg match.call Map mapply max mean median merge message methods min Mod mode months names nargs
nchar ncol NCOL Negate new.env NextMethod ngettext nlevels noquote norm normalizePath nrow NROW numeric
numeric_version nzchar objects oldClass on.exit order ordered outer packageVersion paste paste0 pmatch pmax pmin
polyroot pos.to.env pretty prettyNum print prod prop.table qr qr.solve quantile quarters quit range rank rapply
raw rawToChar Re read.csv read.table readline readLines readRDS Recall Reduce regexpr regmatches remove rep
rep_len rep.int replace requireNamespace rev rexp rm RNGkind round row row.names rownames rowsum rowSums rowMeans
sample sample.int sapply scale scan seq seq_along seq_len sequence setdiff setNames shQuote sign simpleCondition
simpleError simpleWarning sin sinh slice.index solve sort split sprintf sqrt sQuote standardGeneric startsWith
stop stopifnot storage.mode strsplit strtoi structure strtrim sub subset substr substring sum suppressMessages
suppressWarnings svd Sys.Date Sys.getenv Sys.setenv Sys.setlocale Sys.time system.file t table tabulate tail tan
tanh tempdir tempfile tolower toupper tracemem trimws trunc try tryCatch typeof union unique units unlink unlist
unname unsplit upper.tri UseMethod utf8ToInt vapply vector Vectorize warning which which.max which.min while
with within write writeLines xor xtfrm zapsmall nlevels rownames<- names<- levels<- attr<- dim<- class<-
is.environment environment<- body<- formals<- sys.call sys.function Sys.info R.version.string emptyenv
enc2utf8 iconv Encoding validUTF8 utf8ToInt intToUtf8 bitwOr regexec agrep mean.default rowsum trace
max.col det determinant backsolve forwardsolve isSymmetric matrix t.default mapply environmentName nlevels
sys.time Sys.sleep proc.time system.time interactive commandArgs file.show readChar writeChar gzfile url
close open connection textConnection sink capture.output dput dget deparse1 saveRDS load save rapply
array dimnames<- by aggregate ave cor cov var sd weighted.mean cumall is.primitive retracemem bitwXor
lengths nchar stopifnot trimws strtoi sprintf gettext bindtextdomain Sys.setenv Sys.unsetenv suppressPackageStartupMessages
tryInvokeRestart invokeRestart withCallingHandlers signalCondition conditionCall.default simpleMessage
rev.default sort.int order unsplit rle inverse.rle cumsum mapply outer tabulate findInterval cut.default
nchar substr<- regmatches<- is.element vapply array environment sys.frames parent.frame parent.env
sys.calls Reduce Filter Map Position Find do.call match.fun is.function formals args arity nargs
missing on.exit return invisible stop warning signalCondition tryCatch try stopifnot identity
isTRUE isFALSE xor any all ifelse which switch repeat
numeric_version R_system_version package_version compareVersion getNamespace asNamespace loadNamespace
isNamespaceLoaded attachNamespace requireNamespace getExportedValue
Sys.getlocale l10n_info file.access file.rename dir.exists list.dirs basename tools::file_ext
"""
_STATS = """
aggregate AIC anova approx as.dist as.formula ave binomial BIC coef complete.cases confint cor cov cutree
dbinom density deviance df.residual dist dnbinom dnorm dpois ecdf family fitted fitted.values formula gaussian
glm glm.fit Gamma inverse.gaussian IQR kmeans ks.test lm lm.fit loess logLik lowess mad median model.frame
model.matrix model.response na.omit na.exclude nlm nls nls.control nobs optim optimize optimise
p.adjust pbinom pchisq pf plogis pnorm poisson ppois predict qbinom qchisq qf qlogis qnorm qpois qt quantile
quasi quasibinomial quasipoisson rbinom rchisq reformulate relevel reorder residuals resid rexp rgamma rlogis
rnbinom rnorm rpois rt runif sd setNames shapiro.test simulate smooth.spline spline splinefun step t.test terms
uniroot update var vcov weighted.mean weights wilcox.test xtabs binom.test chisq.test fisher.test cor.test
pt qexp pexp dexp dgamma pgamma qgamma rbeta dbeta pbeta qbeta mahalanobis na.fail naprint offset poly
contrasts model.offset make.link sigma nobs dlogis plnorm dlnorm rlnorm rweibull dweibull pweibull
integrate D deriv numericDeriv lsfit prcomp cmdscale hclust as.hclust rstandard rstudent cooks.distance
hatvalues influence dfbetas median.default fivenum quantile.default embed filter lag window ts
"""
_UTILS = """
capture.output combn count.fields data head installed.packages modifyList object.size packageDescription
read.csv read.csv2 read.delim read.table str tail type.convert write.csv write.table zip unzip
packageVersion sessionInfo citation getFromNamespace flush.console txtProgressBar setTxtProgressBar
file_ext file_path_sans_ext toBibtex person bibentry globalVariables hasName tar untar URLencode
"""
_GRAPHICS = """
plot lines points abline par legend text hist barplot boxplot axis mtext title polygon rect segments arrows
image contour persp layout dev.off png pdf svg jpeg rgb col2rgb hcl hsv colorRampPalette grey gray
adjustcolor palette rainbow heat.colors dev.new dev.cur graphics.off
"""
_METHODS = "is new slot slotNames setClass setGeneric setMethod validObject isVirtualClass as"
_TOOLS = "file_ext file_path_sans_ext md5sum toTitleCase R_user_dir"
_SHINY = """
shinyApp runApp reactive reactiveVal reactiveValues eventReactive observe observeEvent isolate req validate need
renderUI renderPlot renderTable renderText renderPrint renderImage downloadHandler uiOutput plotOutput
tableOutput textOutput verbatimTextOutput htmlOutput imageOutput downloadButton downloadLink actionButton
actionLink selectInput selectizeInput numericInput textInput textAreaInput checkboxInput checkboxGroupInput
radioButtons sliderInput fileInput dateInput dateRangeInput passwordInput updateSelectInput updateSelectizeInput
updateNumericInput updateTextInput updateCheckboxInput updateCheckboxGroupInput updateRadioButtons
updateSliderInput updateActionButton updateTabsetPanel updateTabItems updateNavbarPage fluidPage fluidRow column
tabsetPanel tabPanel navbarPage sidebarLayout sidebarPanel mainPanel wellPanel conditionalPanel
showNotification removeNotification showModal removeModal modalDialog modalButton withProgress incProgress
setProgress Progress icon tags tagList div span p h1 h2 h3 h4 h5 h6 strong em br hr a img code pre HTML
includeCSS includeScript singleton addResourcePath shinyOptions getShinyOption invalidateLater debounce
throttle bindCache bindEvent outputOptions freezeReactiveValue reactiveValuesToList isRunning onStop
onSessionEnded moduleServer NS callModule insertUI removeUI helpText titlePanel absolutePanel fixedPanel
splitLayout verticalLayout flowLayout inputPanel exprToFunction installExprFunction markRenderFunction
getDefaultReactiveDomain safeError snapshotExclude testServer runExample shinyAppDir req silent_error
validateCssUnit restoreInput stopApp reactlog observeEvent withMathJax plotPNG tableOutput
"""
_SHINYDASH = """
dashboardPage dashboardHeader dashboardSidebar dashboardBody sidebarMenu menuItem menuSubItem tabItems tabItem
box valueBox valueBoxOutput renderValueBox infoBox infoBoxOutput renderInfoBox tabBox dropdownMenu
notificationItem messageItem taskItem sidebarMenuOutput renderMenu updateTabItems
"""
_HTMLTOOLS = "tags tagList div span p HTML htmlDependency tagAppendChild tagAppendAttributes css save_html browsable withTags"
_GGPLOT = """
ggplot aes aes_string geom_point geom_line geom_path geom_ribbon geom_smooth geom_histogram geom_bar geom_col
geom_boxplot geom_violin geom_errorbar geom_linerange geom_pointrange geom_hline geom_vline geom_abline
geom_text geom_label geom_tile geom_raster geom_rect geom_segment geom_area geom_density geom_step geom_jitter
geom_rug geom_crossbar geom_polygon geom_blank geom_count geom_dotplot geom_freqpoly stat_summary stat_function
facet_wrap facet_grid labs xlab ylab ggtitle theme theme_bw theme_minimal theme_classic theme_void theme_light
element_text element_line element_rect element_blank margin unit scale_x_continuous scale_y_continuous
scale_colour_manual scale_color_manual scale_fill_manual scale_colour_gradient scale_fill_gradient
scale_colour_gradient2 scale_fill_gradient2 scale_colour_gradientn scale_fill_gradientn scale_colour_viridis_c
scale_fill_viridis_c scale_colour_viridis_d scale_fill_viridis_d scale_linetype_manual scale_shape_manual
scale_alpha scale_alpha_continuous scale_size scale_size_continuous scale_x_log10 scale_y_log10 scale_x_discrete
scale_y_discrete scale_colour_identity scale_fill_identity scale_colour_brewer scale_fill_brewer
scale_color_gradient2 scale_color_gradient scale_colour_discrete scale_fill_discrete coord_cartesian coord_flip
coord_fixed expansion guides guide_legend guide_colourbar guide_colorbar guide_none annotate
position_dodge position_jitter position_identity position_nudge after_stat vars label_both label_value
labeller ggsave last_plot sec_axis dup_axis rel expand_limits lims xlim ylim alpha cut_width cut_number
scale_linewidth scale_linewidth_manual scale_x_date scale_shape_identity scale_linetype_identity
"""
_MODELS = """
lmer glmer lFormula glFormula isSingular VarCorr ranef fixef lmerControl glmerControl nlmer refit bootMer
getME ngrps sigma lmList findbars nobars subbars glmmTMB glmmTMBControl nbinom1 nbinom2 truncated_poisson
betabinomial beta_family tweedie genpois compois diagnose lme nlme gls lmeControl nlmeControl pdDiag pdSymm
pdIdent intervals corAR1 corCAR1 varIdent fromJSON toJSON unbox write_json read_json
simulateResiduals testDispersion testZeroInflation testUniformity check_collinearity check_model r2
contest ls_means
"""
_TESTTHAT = """
test_that expect_equal expect_identical expect_true expect_false expect_error expect_warning expect_message
expect_null expect_s3_class expect_s4_class expect_type expect_length expect_named expect_match expect_gt
expect_gte expect_lt expect_lte expect_setequal expect_silent expect_no_error expect_no_warning expect_output
expect_snapshot expect_vector expect_is expect_equivalent expect_invisible expect_visible expect_condition
expect_mapequal expect_contains expect_in expect_no_message expect_no_condition expect_reference
skip skip_if skip_if_not skip_on_cran skip_on_ci skip_if_not_installed skip_on_os skip_if_offline
test_path local_tempfile local_edition context fail succeed test_dir test_file test_check test_local
local_reproducible_output with_seed describe it teardown setup testthat_example
"""
KNOWN_EXTERNAL = set()
for block in (_BASE, _STATS, _UTILS, _GRAPHICS, _METHODS, _TOOLS, _SHINY, _SHINYDASH, _HTMLTOOLS, _GGPLOT, _MODELS, _TESTTHAT):
    KNOWN_EXTERNAL |= set(block.split())
