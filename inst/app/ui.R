
ui <- dashboardPage(
  title = "disappR \u00b7 selective disappearance and ageing",
  skin = "blue",
  dashboardHeader(
    title = tags$span(tags$img(src = paste0("disappR_logo.png?v=", DISAPPR_VERSION), height = "32px", style = "margin:-4px 8px 0 0;"), "disappR",
                      tags$span(class = "header-version", paste0("v", DISAPPR_VERSION))),
    titleWidth = 250
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs", selected = "overview",
      menuItem("Start here", tabName = "overview", icon = icon("compass")),
      menuItem("1 \u00b7 Data", tabName = "data", icon = icon("table")),
      menuItem("2 \u00b7 Visual diagnosis", tabName = "visual", icon = icon("eye")),
      menuItem("3 \u00b7 Missingness and proxies", tabName = "sampling", icon = icon("table-cells")),
      menuItem("4 \u00b7 Trajectories", tabName = "individual", icon = icon("chart-line")),
      menuItem("5 \u00b7 Modelling", tabName = "models", icon = icon("layer-group")),
      menuItem("6 \u00b7 Summary and report", tabName = "summary", icon = icon("square-check"))
    ),
    tags$hr(),
    uiOutput("current_data_box"),
    tags$hr(),
    div(class = "sidebar-cite", strong("Cite as"),
        tags$ol(
          tags$li("Sanghvi, K., Ivimey-Cook, E. R. 2026. disappR: a shiny app to model ageing and selective [dis]appearance."),
          tags$li("Sanghvi, K., Ivimey-Cook, E. R., Bouwhuis, S., Sepil, I. and van de Pol, M., 2026. A comparison of methods to assess selective disappearance and quantify ageing. ",
                  tags$em("EcoEvoRxiv"))),
        tags$details(class = "cite-more",
          tags$summary("Empirical datasets used"),
          tags$ol(class = "cite-list",
            tags$li("Allain J, Tissier M, Bergeron P, Garant D, R\u00e9ale D (2024) Age at first reproduction and senescence in a short-lived wild mammal. ", tags$em("Oikos"), ". doi:10.1111/oik.09944"),
            tags$li("Bichet C et al. (2022) Immunosenescence in the wild? A longitudinal study in a long-lived seabird. ", tags$em("Journal of Animal Ecology"), ". doi:10.1111/1365-2656.13642"),
            tags$li("Bichet C, R\u00e9gis C, Gilot-Fromont E, Cohas A (2022) Variations in immune parameters with age in a wild rodent population and links with survival. ", tags$em("Ecology and Evolution"), " 12: e9094. doi:10.1002/ece3.9094"),
            tags$li("Bouwhuis S, Sheldon BC, Verhulst S, Charmantier A (2009) Great tits growing old: selective disappearance and the partitioning of senescence to stages within the breeding cycle. ", tags$em("Proceedings of the Royal Society B"), " 276: 2769\u20132777. doi:10.1098/rspb.2009.0457"),
            tags$li("McKenna-Ell C, Ravindran S, Pilkington JG, Pemberton JM, Nussey DH, Froy H (2023) Trait-dependent associations between early- and late-life reproduction in a wild mammal. ", tags$em("Biology Letters"), " 19: 20230050. doi:10.1098/rsbl.2023.0050"),
            tags$li("Moullec H, Reichert S, Bize P (2023) Aging trajectories are trait- and sex-specific in the long-lived Alpine swift. ", tags$em("Frontiers in Ecology and Evolution"), " 11: 983266. doi:10.3389/fevo.2023.983266"),
            tags$li("P\u00e1sztor K et al. (2022) Phenotypic senescence in a natural insect population. ", tags$em("Ecology and Evolution"), " 12: e9668. doi:10.1002/ece3.9668"),
            tags$li("Sanghvi K, Iglesias-Carrasco M, Zajitschek F, Kruuk LEB, Head ML (2022) Effects of developmental and adult environments on ageing. ", tags$em("Evolution"), ". doi:10.1111/evo.14567"),
            tags$li("Sanghvi K, Gascoigne SJL, Todorova B, Vega-Trejo R, Pizzari T, Sepil I (2025) No evidence for paternal age effects on sons or daughters when accounting for paternal sperm storage. ", tags$em("The American Naturalist"), " 206: E29\u2013E46. doi:10.1086/736479"),
            tags$li("Szejner-Sigal A, Rinehart JP, Bowsher JH, Greenlee KJ (2025) Senescence and early-life performance as predictors of lifespan in a solitary bee. ", tags$em("Proceedings of the Royal Society B"), " 292: 20242637. doi:10.1098/rspb.2024.2637"),
            tags$li("Warner DA, Miller DAW, Bronikowski AM, Janzen FJ (2016) Decades of field data reveal that turtles senesce in the wild. ", tags$em("PNAS"), " 113: 6502\u20136507. doi:10.1073/pnas.1600035113"),
            tags$li("Wynn J, K\u00fcrten N, Moiron M, Bouwhuis S (2025) Selective disappearance based on navigational efficiency in a long-lived seabird. ", tags$em("Journal of Animal Ecology"), " 94: 535\u2013544. doi:10.1111/1365-2656.14231"))))
  ),
  dashboardBody(
    # A button that has just been clicked is disabled for 1.5 seconds, so repeated clicks on it cannot queue the same
    # work several times. (0.20.7-0.20.8 locked the whole page while R was busy; with that lock the Trajectories page
    # never finished drawing, so it was removed in 0.20.9.)
    tags$head(tags$script(HTML("document.addEventListener('DOMContentLoaded',function(){$(document).on('click','button.action-button',function(){var b=this;setTimeout(function(){b.disabled=true;},0);setTimeout(function(){b.disabled=false;},1500);});});"))),
    tags$head(tags$script(HTML("function disapprFlash(btn,ok){if(!btn){return;}if(!btn.dataset.label){btn.dataset.label=btn.innerHTML;}btn.innerHTML=ok?'Copied':'Could not copy';btn.title=ok?'':'Select the code and press Ctrl+C (Cmd+C on a Mac)';setTimeout(function(){btn.innerHTML=btn.dataset.label;btn.title='';},ok?1800:4000);}function disapprCopyFallback(txt){var ta=document.createElement('textarea');ta.value=txt;ta.setAttribute('readonly','');ta.style.position='fixed';ta.style.top='-1000px';document.body.appendChild(ta);ta.select();var ok=false;try{ok=document.execCommand('copy');}catch(e){ok=false;}document.body.removeChild(ta);return ok;}function disapprCopy(id,btn){var el=document.getElementById(id);var txt=el?(el.innerText||el.textContent||''):'';if(!txt.trim()){disapprFlash(btn,false);return;}if(navigator.clipboard&&window.isSecureContext){navigator.clipboard.writeText(txt).then(function(){disapprFlash(btn,true);},function(){disapprFlash(btn,disapprCopyFallback(txt));});}else{disapprFlash(btn,disapprCopyFallback(txt));}}")), tags$style(HTML("
 .section-code pre { max-height: 440px; overflow: auto; font-size: 11.5px; }
 .code-toolbar { display: flex; gap: 6px; margin-bottom: 6px; }
 /* --- guidance layer (signposting only; no analysis is affected) --- */
 .guide-box { background: #fbf5ec; border: 1px solid #e3d3bf; border-left: 5px solid #7b5a45; border-radius: 8px;
 padding: 12px 16px; margin: 0 0 14px 0; }
 .guide-head { font-weight: 700; color: #5a4033; font-size: 14px; margin-bottom: 4px; }
 .guide-goal { color: #3d342d; margin-bottom: 7px; }
 .guide-step { margin: 2px 0; color: #3d342d; }
 .guide-lab { display: inline-block; min-width: 74px; font-weight: 700; color: #7b5a45; }
 .guide-tip { margin-top: 7px; font-size: 12.5px; color: #6d5c4d; font-style: italic; }
 .guide-nav { margin-top: 10px; display: flex; gap: 18px; }
 .guide-link { font-weight: 600; color: #7b5a45; }
 .guide-link-next { margin-left: auto; }
 .look-for { background: #f6f1ea; border-left: 3px solid #b08968; padding: 6px 10px; margin: 8px 0 2px 0;
 font-size: 12.5px; color: #4a3f36; border-radius: 0 6px 6px 0; }
 .look-lab { font-weight: 700; color: #7b5a45; margin-right: 6px; }
 .dbadge { display: inline-block; font-size: 11px; font-weight: 700; padding: 1px 7px; border-radius: 10px;
 margin-left: 6px; vertical-align: middle; }
 .dbadge-recommended { background: #e4efe2; color: #2f6b34; border: 1px solid #b9d7b6; }
 .dbadge-optional { background: #eef0f6; color: #46527a; border: 1px solid #c4cbe0; }
 .dbadge-required { background: #fbeee2; color: #8a4b12; border: 1px solid #e5c9a9; }
 .dbadge-caution { background: #fdeaea; color: #98302b; border: 1px solid #edc2bf; }
 .dbadge-unavailable { background: #f0efed; color: #736c64; border: 1px solid #d8d3cc; }
 .plain-title .plain-q { display: block; font-size: 18px; font-weight: 700; color: #5a4033; }
 .plain-title .plain-t { display: block; font-size: 12.5px; font-weight: 400; color: #7d6d5e; }
 .section-divider { margin: 16px 0 8px 0; padding-bottom: 4px; border-bottom: 2px solid #e5d6c6; }
 .section-divider-label { font-size: 15px; font-weight: 700; color: #5a4033; }
 .section-divider-note { font-size: 12.5px; color: #8a7564; margin-left: 10px; }
 .progress-strip { display: flex; flex-wrap: wrap; gap: 6px; align-items: center; margin: 0 0 12px 0;
 font-size: 12.5px; }
 .pstep { padding: 3px 10px; border-radius: 12px; border: 1px solid #ddd0bf; background: #faf6f0; color: #7d6d5e; }
 .pstep-done { background: #e4efe2; border-color: #b9d7b6; color: #2f6b34; font-weight: 600; }
 .pstep-now { background: #7b5a45; border-color: #7b5a45; color: #fff; font-weight: 700; }
 .pstep-sep { color: #c4b6a5; }
 .journey { border: 1px solid #e3d3bf; border-radius: 10px; padding: 14px 16px; height: 100%; background: #fffdfa; }
 .journey-title { font-size: 16px; font-weight: 700; color: #5a4033; margin-bottom: 4px; }
 .journey-sub { font-size: 12.5px; color: #7d6d5e; min-height: 52px; }
      /* One semantic palette. Red means a real caution; everything else is neutral, and weight
         rather than hue marks what to read first. */
      .box.box-solid.box-primary > .box-header { background: #7b5a45 !important; border-color: #7b5a45 !important; }
      .box.box-primary { border-top-color: #7b5a45 !important; }
      .box.box-solid.box-warning > .box-header { background: #f6ede2 !important; color: #5a4033 !important; border-color: #e0cdb6 !important; }
      .box.box-warning { border-top-color: #e0cdb6 !important; }
      .box.box-solid.box-info > .box-header { background: #f3f1ed !important; color: #5a4033 !important; border-color: #ded9d1 !important; }
      .box.box-info { border-top-color: #ded9d1 !important; }
      .result-box { background: #fbf8f3; border: 1px solid #e3d3bf; border-left: 6px solid #7b5a45;
                    border-radius: 8px; padding: 14px 18px; margin-bottom: 14px; }
      .result-head { font-size: 17px; font-weight: 700; color: #3d2c22; margin-bottom: 6px; }
      .result-line { margin: 3px 0; color: #3d342d; }
      .result-mark { display: inline-block; width: 20px; font-weight: 700; }
      .mark-yes { color: #3f6b42; } .mark-warn { color: #9a6b1e; } .mark-no { color: #8a3b34; }
      .caution-box { background: #fdf6f5; border: 1px solid #edc2bf; border-left: 6px solid #A50026;
                     border-radius: 8px; padding: 14px 18px; margin-bottom: 14px; }
 .page-title { font-size: 21px; font-weight: 700; color: #3d2c22; margin: 2px 0 10px 0; line-height: 1.25; }
      .evidence-key { font-size: 12px; color: #7d6d5e; margin: 0 0 12px 0; }
      .ek { font-weight: 700; padding: 1px 7px; border-radius: 10px; margin-right: 3px; }
      .ek-primary { background: #7b5a45; color: #fff; }
      .ek-sens { background: #fbeee2; color: #8a4b12; border: 1px solid #e5c9a9; }
      .ek-support { background: #eef0f6; color: #46527a; border: 1px solid #c4cbe0; }
      .ek-tech { background: #f0efed; color: #736c64; border: 1px solid #d8d3cc; }
      .use-function-box { margin-top: 16px; border: 3px solid #1f1611; border-radius: 10px; padding: 14px; background: #f3e6d4;
                          box-shadow: 0 2px 6px rgba(31,22,17,0.25); }
      .shape-lead { font-size: 16px; color: #3d2c22; background: #fbf5ec; border-left: 5px solid #7b5a45;
                    padding: 8px 12px; border-radius: 0 6px 6px 0; margin-bottom: 10px; }
      .use-function-btn { background: #1f1611 !important; border: 2px solid #1f1611 !important; color: #ffffff !important;
                          font-weight: 800 !important; font-size: 17px !important; padding: 14px 16px !important; letter-spacing: 0.2px; }
      .use-function-btn:hover { background: #5a4033 !important; border-color: #5a4033 !important; color: #ffffff !important; }
      .nav-tabs-custom > .nav-tabs > li > a { font-size: 15px; font-weight: 700; color: #5a4033;
                                          border: 1.5px solid #d8c7b2; border-bottom: none; margin-right: 4px;
                                          border-radius: 6px 6px 0 0; padding: 8px 16px; }
      .nav-tabs-custom > .nav-tabs > li.active > a { background: #7b5a45; color: #ffffff; border-color: #7b5a45; }
      .header-version { font-size: 12px; font-weight: 400; opacity: 0.85; margin-left: 8px; }
      .evidence-card { background: #fbf8f3; border: 1px solid #e3d3bf; border-radius: 8px; padding: 14px 18px; margin: 12px 0 16px 0; }
      .evidence-headline { font-size: 16px; font-weight: 700; color: #3d2c22; margin: 4px 0 8px 0; line-height: 1.4; }
      .evidence-finding { font-size: 14.5px; margin: 6px 0; line-height: 1.45; }
      .evidence-explain { font-size: 14px; color: #8a3b34; margin: 6px 0 8px 0; }
      .evidence-table { border-collapse: collapse; margin: 8px 0; width: 100%; }
      .evidence-table td { padding: 5px 8px; border-bottom: 1px solid #efe4d6; vertical-align: top; }
      .evidence-table .ev-name { font-weight: 700; white-space: nowrap; color: #3d2c22; }
      .mark-na { color: #a39686; }
      .evidence-pending { background: #fdf6ee; border: 1px dashed #d8b98f; border-radius: 6px; padding: 8px 12px; margin-top: 10px; }
      .evidence-pending ol { margin: 6px 0 0 0; padding-left: 20px; }
      .perm-card { background: #fbf8f3; border: 1px solid #e3d3bf; border-radius: 8px; padding: 12px 16px; margin: 10px 0; }
      .perm-headline { font-size: 15px; font-weight: 700; color: #3d2c22; margin-bottom: 8px; line-height: 1.4; }
      .perm-row { margin: 6px 0; line-height: 1.45; }
      .perm-label { display: block; font-weight: 700; color: #7b5a45; font-size: 12.5px; text-transform: uppercase; letter-spacing: 0.3px; }
      .summary-fold > summary { cursor: pointer; list-style: none; outline: none; }
      .summary-fold > summary::-webkit-details-marker { display: none; }
      .summary-fold > summary .result-head::before { content: '\\25B8  '; color: #7b5a45; }
      .summary-fold[open] > summary .result-head::before { content: '\\25BE  '; }
      .summary-body { margin-top: 10px; border-top: 1px dashed #ddd0bf; padding-top: 8px; }
      .summary-body h5 { margin-top: 12px; color: #3d2c22; }
      .result-overall { font-size: 15px; color: #3d2c22; margin: 4px 0 8px 0; line-height: 1.45; }
      .cite-more { margin-top: 8px; }
      .cite-more > summary { cursor: pointer; font-weight: 700; font-size: 12px; outline: none; }
      .cite-list { font-size: 11px; padding-left: 16px; margin-top: 6px; }
      .cite-list li { margin-bottom: 4px; line-height: 1.3; }
      .info-more { margin-top: 12px; border-top: 1px dashed #ddd0bf; padding-top: 9px; }
      .info-more > summary { cursor: pointer; font-weight: 700; color: #7b5a45; font-size: 13px; outline: none;
                             border: 1.5px solid #7b5a45; border-radius: 5px; padding: 3px 10px; display: inline-block; }
      .info-more-body { margin-top: 9px; color: #4a3f36; font-size: 13.5px; }
      .advanced-block { margin-top: 10px; border-top: 1px dashed #ddd0bf; padding-top: 8px; }
 .advanced-block > summary { cursor: pointer; font-weight: 600; color: #7b5a45; font-size: 12.5px; outline: none;
                                  border: 1.5px solid #7b5a45; border-radius: 5px; padding: 3px 9px; display: inline-block; }
      /* collapse controls: bordered so the + and - are easy to see */
      .box-header .box-tools .btn-box-tool, .box-header .box-tools button {
        border: 1.5px solid #5a4033 !important; border-radius: 5px !important; color: #5a4033 !important;
        background: #ffffff !important; padding: 1px 7px !important; margin-left: 4px; font-weight: 700; line-height: 1.3; }
      .box-header .box-tools .btn-box-tool:hover { background: #f1e6d8 !important; color: #3d2c22 !important; }
      .box-solid > .box-header .box-tools .btn-box-tool { border: 1.5px solid #ffffff !important; color: #ffffff !important; background: transparent !important; }
      .box-solid > .box-header .box-tools .btn-box-tool:hover { background: rgba(255,255,255,0.25) !important; }
 .model-eq { font-family: 'Cambria Math', 'Times New Roman', serif; font-size: 15px; line-height: 1.8; margin: 4px 0 8px 0; overflow-x: auto; }
 .coef-eq td { vertical-align: top; }
 body, .content-wrapper, .right-side { font-family: Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }
 .content-wrapper, .right-side { background:#F2EADF !important; }
 .main-header .logo { background:#685243 !important; color:#FFF9F2 !important; font-weight:700; }
 .model-row { display:flex; align-items:center; gap:8px; border-bottom:1px solid #EFE6DA; padding:2px 0; }
 .model-row-check { flex:0 0 42%; } .model-row-check .checkbox { margin:4px 0; }
 .model-row-extra { flex:1 1 auto; } .model-row-extra .form-group { margin:3px 0; } .model-row-extra .selectize-control { margin:0; }
 .main-header .navbar { background:#91634E !important; }
 .skin-blue .main-sidebar { background:#493D34 !important; }
 .skin-blue .sidebar-menu>li>a { color:#EFE4D6 !important; border-left:3px solid transparent; }
 .skin-blue .sidebar-menu>li:hover>a, .skin-blue .sidebar-menu>li.active>a { background:#5B4B40 !important; color:#FFF9F2 !important; border-left-color:#D6A15E !important; }
 .sidebar-note { padding:2px 18px 18px; color:#DCCFC0; font-size:12px; line-height:1.55; }
 .sidebar-cite { padding:2px 18px 18px; color:#DCCFC0; font-size:11.5px; line-height:1.45; }
 .current-data { padding:2px 18px 4px; color:#DCCFC0; font-size:11.5px; line-height:1.45; white-space:normal; }
 .current-data .cd-head { font-weight:700; color:#F3EBE1; margin-bottom:3px; }
 .current-data .cd-row { margin:1px 0; }
 .current-data .cd-lab { color:#B9AC9D; }
 .current-data .cd-warn { color:#F0B27A; margin-top:3px; }
 .purpose .purpose-lead { color:#4D4036; font-size:14.5px; line-height:1.6; margin:0 0 8px; }
 .purpose ul { padding-left:18px; margin:4px 0 10px; }
 .purpose li { color:#5E4E42; font-size:13.5px; line-height:1.55; margin-bottom:6px; }
 .optional-pkgs td { font-size:12.5px; padding:3px 8px !important; }
 .prefit-box { font-size:12.5px; color:#5E4E42; border-left:3px solid #C9B8A6; padding:4px 10px; margin:6px 0 10px; }
 .prefit-box p { margin:0 0 4px; } .prefit-box ul { margin:0 0 4px; padding-left:18px; }
 .pkg-ok { color:#1B7837; font-weight:600; } .pkg-missing { color:#B35806; font-weight:600; }
 .sidebar-cite ol { padding-left:16px; margin:6px 0 0; }
 .sidebar-cite li { margin-bottom:7px; }
 .main-sidebar hr { border-color:rgba(255,255,255,.10); margin:14px 18px; }
 .box { border-radius:12px; border-top:0 !important; box-shadow:0 3px 14px rgba(79,60,44,.08); background:#FFFDF9; }
 .box.box-solid.box-primary>.box-header, .box.box-solid.box-info>.box-header, .box.box-solid.box-warning>.box-header { background:#E5D2BE !important; color:#463B33 !important; }
 .box-title { font-weight:700 !important; }
 .box-body { overflow-x:auto; }
 .metric-card { background:#FFFDF9; border:1px solid #E6D8C8; border-radius:12px; padding:13px 15px; min-height:96px; margin-bottom:12px; }
 .metric-label { color:#8A7564; font-size:11px; font-weight:700; text-transform:uppercase; letter-spacing:.06em; }
 .metric-value { color:#483D35; font-size:24px; font-weight:700; margin-top:4px; line-height:1.1; }
 .metric-sub { color:#78685B; font-size:12px; margin-top:5px; line-height:1.4; }
 .diagnosis-card { border-left:5px solid #B56C54; background:#F8EEE4; padding:13px 16px; border-radius:10px; margin-bottom:12px; }
 .diagnosis-title { font-size:16px; font-weight:700; color:#5A4033; }
 .diagnosis-detail { margin-top:6px; color:#66564A; line-height:1.5; }
 .truth-card { background:#EFE8D8; border:1px solid #D8C8AC; border-radius:10px; padding:12px 14px; color:#5B4C3F; line-height:1.5; margin:10px 0; }
 .truth-card ul { padding-left:18px; margin:6px 0 0; }
 .small-note { color:#7B6B5D; font-size:12.5px; line-height:1.5; }
 .section-lead { color:#66574B; font-size:14px; line-height:1.6; margin-bottom:12px; }
 .step-card { background:#FAF3EA; border:1px solid #E8DACB; border-radius:12px; padding:12px 14px; margin-bottom:10px; }
 .step-kicker { font-size:11px; text-transform:uppercase; letter-spacing:.08em; font-weight:700; color:#A26750; }
 .step-title { font-weight:700; color:#4D4036; margin:2px 0; }
 .step-text { color:#756458; font-size:12.5px; line-height:1.45; }
 .summary-block { background:#FFFDF9; border:1px solid #E5D6C6; border-radius:12px; padding:14px 16px; margin-bottom:12px; }
 .summary-block h4 { margin-top:0; color:#58463A; font-weight:700; }
 .btn-primary { background:#A8644E !important; border-color:#A8644E !important; color:white !important; }
 .btn-primary:hover { background:#8E5442 !important; }
 .btn-default { background:#F4E8DA !important; border-color:#D8C3AF !important; color:#5E4A3B !important; }
 .btn-block-space { margin:6px 0; width:100%; }
 table { color:#584B41; }
 th, td { white-space:normal !important; }
 pre.code-out { background:#2E2620; color:#F3E9DC; border-radius:10px; font-size:12px; max-height:420px; overflow:auto; }
 details summary { cursor:pointer; color:#7B5A45; font-weight:600; margin:8px 0; }
 .info-link { margin-left:7px; color:#A8644E !important; font-size:15px; }
 .info-link:hover { color:#6D3F31 !important; }
 .save-btn { margin-top:8px; }
 .saved-controls { background:#FAF3EA; border:1px solid #E8DACB; border-radius:10px; padding:10px 12px; margin-bottom:12px; }
 "))),

    tabItems(
      # ---------------------------------------------------------------- overview
      tabItem(tabName = "overview",
        fluidRow(
          box(width = 12, title = "Purpose", status = "primary", solidHeader = TRUE,
            div(class = "purpose",
              p(class = "purpose-lead",
                "Ecologists wish to evaluate the latent average within-individual ageing pattern, but only have access to ",
                "population-level data on individuals followed over time. These datasets are usually incomplete, and while several ",
                "analytical approaches exist to handle them, these were developed for simple scenarios, and how well they recover the latent average within-individual trajectory on complex data remains untested. Importantly, when individual phenotypes are associated with an ",
                "individual\u0027s entry into or removal from the sample \u2014 selective appearance and selective disappearance \u2014 ",
                "those approaches can return biased ageing patterns."),
              p(class = "purpose-lead", strong("disappR addresses the following gaps in ageing research:")),
              tags$ul(
                tags$li(strong("Pervasive, but the remedies are recent. "),
                        "Selective disappearance is found across traits and taxa, yet the methods that account for it were only developed in the last two decades."),
                tags$li(strong("Missing data can bias some methods. "),
                        "With incomplete sampling, mean age becomes a poor proxy of lifespan, and the survivor-restricted decomposition method can run short of individuals sampled at successive ages."),
                tags$li(strong("Age-dependent selection is widely overlooked. "),
                        "Lifespan, or age at first observation, can be linked to how fast or in what shape individuals age, not only to their average age-independent phenotype."),
                tags$li(strong("Many options, little guidance. "),
                        "Researchers often misfit models, or do not explore whether alternative methods might better explain their data, or if overlooked mechanisms and processes impact ageing patterns.")),
              p(class = "purpose-lead",
                "disappR diagnoses these problems in your data \u2014 visually first, then via a diagnosis of your dataset \u2014 ",
                "and fits and compares the models that address them."),
              p(class = "purpose-lead",
                "It is meant to be the first port of call for empiricists evaluating longitudinal data, especially when studying ageing: ",
                "comprehensive enough to carry out the mixed modelling and diagnostics behind a publishable analysis, ",
                "and guided enough that analysing a dataset and interpreting the biology feels more straightforward."))
          )
        ),
        fluidRow(
          box(width = 12, title = if (all(OPTIONAL_STATUS$Installed)) "Optional packages: all installed" else "Optional packages: some are not installed",
              status = if (all(OPTIONAL_STATUS$Installed)) "success" else "warning", solidHeader = TRUE, collapsible = TRUE,
              collapsed = all(OPTIONAL_STATUS$Installed),
              p(class = "small-note", "Missing packages remove the analyses listed; install them with install.packages()."),
              optional_packages_ui())
        ),
        fluidRow(
          box(width = 12, title = info_title("A visual-first workflow for selective disappearance and appearance", "overview"),
              status = "primary", solidHeader = TRUE,
            div(class = "section-lead",
                "Longitudinal ageing trajectories are biased when individuals that die (or enter) early differ from the others in their trait values or in how the trait changes with age. The tabs follow a diagnose-then-model workflow. Click the ", icon("circle-info"), " symbols for help on any section."),
            h4(strong("Choose how to start")),
            fluidRow(
              column(4, div(class = "journey",
                div(class = "journey-title", "Learn"),
                div(class = "journey-sub", "Use simulated data where the 'true' trajectory is known for comparison, so you can see what selective disappearance looks like before encountering it in real data. Generate a variety of datasets, with options to control data structure, missingness, the organism\u0027s biology, sample size, and the type and magnitude of the selective processes."),
                actionButton("journey_learn", "Start with simulated data", class = "btn-primary btn-block-space"))),
              column(4, div(class = "journey",
                div(class = "journey-title", "Explore"),
                div(class = "journey-sub", "Open one of 13 empirical datasets from 12 published studies and set the default options that most closely reproduce the analysis reported in its paper. Fit alternative models to these data to understand how the interpreted biological pattern would change. Useful for seeing how the workflow applies to real longitudinal data."),
                actionButton("journey_explore", "Open an empirical example", class = "btn-default btn-block-space"))),
              column(4, div(class = "journey",
                div(class = "journey-title", "Analyse"),
                div(class = "journey-sub", "Upload your own CSV of longitudinal data on individuals sampled repeatedly across ages or time steps."),
                actionButton("journey_analyse", "Upload my data", class = "btn-default btn-block-space")))
            ),
            tags$hr(),
            h4(strong("The six steps")),
            fluidRow(
              column(6,
                div(class = "step-card", div(class = "step-kicker", "1 \u00b7 Data"),
                    div(class = "step-title", "Load, map, subset and check your data"),
                    div(class = "step-text", "Use simulated data (known answer), one of the published empirical datasets, or your own CSV. Map continuous and categorical covariates and their interactions, and check that ages, lifespans, AFR (age at first observation) and censoring are read correctly.")),
                div(class = "step-card", div(class = "step-kicker", "2 \u00b7 Visual diagnosis"),
                    div(class = "step-title", "Diagnose selective disappearance and appearance visually"),
                    div(class = "step-text", "Qualitatively infer the type and magnitude of selective processes by visualising trait trajectories.")),
                div(class = "step-card", div(class = "step-kicker", "3 \u00b7 Missingness and proxies"),
                    div(class = "step-title", "How complete is sampling, and how does it affect the accuracy of different among-individual terms?"),
                    div(class = "step-text", "Sampling grid, missingness by age and against other variables, and agreement between ALR, mean age and lifespan. Guides the choice between ALR and mean age in the models."))
              ),
              column(6,
                div(class = "step-card", div(class = "step-kicker", "4 \u00b7 Individual and population trajectories"),
                    div(class = "step-title", "Diagnose ageing shapes visually, then compare functions"),
                    div(class = "step-text", "Fits a function (linear to exponential) to each individual to see how individuals age, reconstructs the parametric approximation of the average individual ageing curve, and compares ageing functions at the population level. Choose the ageing function for the mixed models here.")),
                div(class = "step-card", div(class = "step-kicker", "5 \u00b7 Modelling"),
                    div(class = "step-title", "Quantify selective disappearance and appearance"),
                    div(class = "step-text", "Fit ten models that account for different types of selective process; choose error families, random terms and covariates; compare model AICs and age predictions, with diagnostics of model fit.")),
                div(class = "step-card", div(class = "step-kicker", "6 \u00b7 Summary and report"),
                    div(class = "step-title", "Collect and export results"),
                    div(class = "step-text", "Every 'Save to summary' result is gathered here with an automatic, cautious overview, and exported as an HTML or text report."))
              )
            )
          )
        )
      ),

      # ---------------------------------------------------------------- data
      tabItem(tabName = "data",
        page_title("What data do I have, and are the columns mapped correctly?"),
        guide_box("data",
          goal = "Load a dataset and tell the app which columns are the individual, the age and the trait. Everything later depends on this mapping.",
          start_with = "pick a data source on the left, then map the variables to the respective columns. For simulated data, choose what biology and data structure you want to simulate. For empirical data, explore these using the default settings (which closely match the original study) or your own settings. For your own CSV, map the variables, covariates, interactions, random effects and so on.",
          then = "visualise the data distribution and check data integrity."
          ),
        fluidRow(
          box(width = 4, title = info_title("Data source", "data_source"), status = "primary", solidHeader = TRUE,
            radioButtons("data_source", NULL, choices = c("Simulated teaching data" = "toy", "Empirical examples (published data)" = "example", "Upload my CSV" = "upload"), selected = "toy"),
            conditionalPanel("input.data_source == 'toy'",
              selectInput("toy_trait", "Trait and distribution", choices = TOY_TRAITS, selected = "mass"),
              conditionalPanel("input.toy_trait != 'paper'",
                selectInput("toy_form", "Ageing form", choices = AGE_FUNCTIONS, selected = "Quadratic"),
                selectInput("toy_shape", "Shape within this form (biological scenario)", choices = toy_shape_choices("Quadratic"), selected = "default"),
                radioButtons("toy_strength", "Pattern strength", inline = TRUE,
                             choices = c("Dramatic (teaching)" = "dramatic", "Moderate" = "moderate"), selected = "dramatic"),
                sliderInput("toy_mean_ls", "Mean lifespan (number of sampling occasions)", min = 3, max = 30, value = 20, step = 1),
                radioButtons("toy_rate_var", "Individual differences in ageing rates", inline = TRUE,
                             choices = c("Small" = "low", "Large" = "high"), selected = "low")
              ),
              radioButtons("toy_sd_type", "Selective disappearance", choices = SELECTION_TYPES, selected = "both"),
              conditionalPanel("input.toy_sd_type != 'none'",
                radioButtons("toy_sd_dir", "Direction", inline = TRUE,
                             choices = c("Positive: longer-lived individuals have higher values" = "1",
                                         "Negative: lower values" = "-1"), selected = "1")),
              box_note("Age-independent: lifespan is linked to the level of the trajectory. Age-dependent: lifespan is linked to the rate of ageing."),
              selectInput("toy_missingness", "Sampling design", choices = c(
                "Complete sampling" = "complete", "Missing completely at random (MCAR, ~25% missing)" = "mcar",
                "Missing when old (MWO)" = "mwo", "Missing when young (MWY)" = "mwy",
                "Trait-dependent missingness" = "trait", "Condition-dependent missingness" = "condition"), selected = "complete"),
              radioButtons("toy_afr_mode", "Age at first observation (AFR)",
                           choices = c("Same for everyone (age 1)" = "same", "Individual-specific" = "individual"), selected = "same"),
              conditionalPanel("input.toy_afr_mode == 'individual'",
                radioButtons("toy_sa_type", "Selective appearance",
                             choices = c("None (AFR varies at random)" = "none", "Age-independent" = "independent",
                                         "Age-dependent" = "dependent", "Age-independent and age-dependent" = "both"),
                             selected = "none"),
                conditionalPanel("input.toy_sa_type != 'none'",
                  radioButtons("toy_sa_dir", "Direction", inline = TRUE,
                               choices = c("Positive: later starters have higher values" = "1",
                                           "Negative: lower values" = "-1"), selected = "1"))),
              checkboxInput("toy_diet", "Add a diet covariate (poor diet lowers the trait; no effect on lifespan)", FALSE),
              checkboxInput("toy_groups", "Add families (nested random effect: individuals within 25 families)", FALSE),
              fluidRow(
                column(6, numericInput("toy_n", "Individuals", value = 300, min = 30, max = 2000, step = 50)),
                column(6, numericInput("toy_seed", "Seed", value = 42, min = 1, step = 1))
              ),
              actionButton("toy_commit", "Simulate & use this dataset", class = "btn-primary btn-block-space", icon = icon("play")),
              uiOutput("toy_status"),
              downloadButton("download_toy", "Download simulated CSV", class = "btn-default btn-block-space")
            ),
            conditionalPanel("input.data_source == 'example'",
              selectInput("example_id", "Published dataset", choices = EXAMPLE_CHOICES, selected = "fly"),
              uiOutput("example_note"),
            div(class = "guide-box", style = "margin-top:10px;",
                div(class = "guide-head", "About the bundled empirical examples"),
                div(class = "guide-goal",
                    "Default settings match each paper\u0027s model specification as closely as the app and each study\u0027s method and analysis descriptions allow. Fitting the default settings generally reproduces the results these studies report. You can explore whether alternative models would have given a different fit and biological interpretation of the data. Their purpose is not to scrutinise the original analyses or to argue for a reanalysis, but only so that researchers can play with real datasets in different ways using the app\u0027s features."))
            ),
            conditionalPanel("input.data_source == 'upload'",
              fileInput("data_file", "CSV file (one row per individual \u00d7 age)", accept = ".csv"),
              box_note("Required: individual ID, age (numeric: whole numbers or decimals, not categories), trait. Optional: known lifespan, ALR, AFR, condition, covariates, grouping and random-effect variables, a censoring indicator. Missing values can be blank cells or NA (both are read as missing, as are '.', '-', 'NaN', 'N/A', '#N/A' and 'NULL').")
            ),
            tags$hr(),
            radioButtons("dup_action", info_title("Repeated ID \u00d7 age records", "duplicates"), inline = TRUE,
                         choices = c("Keep all" = "keep", "Collapse exact duplicates" = "mean"), selected = "keep")
          ),
          box(width = 8, title = info_title("Map columns", "mapping"), status = "primary", solidHeader = TRUE,
            uiOutput("mapping_ui"),
            box_note(strong("AFR"), " = age at first observation. ",
                     strong("ALR"), " = age at last record. ",
                     strong("LS"), " = known lifespan. ",
                     strong("Nesting"), " identifies individuals by group + ID: (1 | group) + (1 | group:ID). ",
                     strong("Additional random intercepts"), " enter every model as + (1 | X). ",
                     strong("Censored"), " individuals keep their records but their LS counts as unknown.")
          )
        ),
        fluidRow(
          box(width = 12, title = info_title("Subset the data (optional)", "subset"), status = "info", solidHeader = TRUE, collapsible = TRUE,
              uiOutput("subset_ui"))
        ),
        fluidRow(uiOutput("data_metrics")),
        fluidRow(
          box(width = 12, title = info_title("Distribution of a variable", "distribution"), status = "info", solidHeader = TRUE, collapsible = TRUE,
            fluidRow(
              column(3, uiOutput("dist_var_ui"),
                     radioButtons("dist_unit", "One value per", choices = c("Row" = "row", "Individual (mean or most common value)" = "individual"),
                                  selected = "row"),
                     save_button("save_distribution")),
              column(9, plotOutput("dist_plot", height = 400))
            ))
        ),
        fluidRow(
          box(width = 12, title = "Glossary of terms", status = "info", solidHeader = FALSE,
              collapsible = TRUE, collapsed = TRUE,
              tags$ul(class = "small-note",
                tags$li(strong("Selective disappearance"), " \u2014 individuals with particular trait values leave earlier, so the mix of individuals changes with age."),
                tags$li(strong("Selective appearance"), " \u2014 individuals with particular trait values enter later, so the mix changes at the start too."),
                tags$li(strong("AFR"), " \u2014 age at first observation."),
                tags$li(strong("ALR"), " \u2014 age at last record, used as a proxy for lifespan."),
                tags$li(strong("LS"), " \u2014 known lifespan, where you have it."),
                tags$li(strong("Among-individual term"), " \u2014 the ALR, LS, AFR or mean-age term that carries the correction for who is present."),
                tags$li(strong("Within-individual change"), " \u2014 how a trait changes as one individual ages, which is what you want to estimate."),
                tags$li(strong("AIC, \u0394AIC"), " \u2014 model fit penalised for complexity; lower is better, and models within 2 AIC of the best are equally supported."),
                tags$li(strong("Random slope"), " \u2014 lets each individual have its own ageing rate (or curvature)."),
                tags$li(strong("Error family"), " \u2014 the distribution of the trait around the model: Gaussian, Gamma, lognormal or beta for continuous traits; Poisson or negative binomial for counts; binomial for 0/1 or proportions."),
                tags$li(strong("Sampling interval"), " \u2014 the time between occasions; it defines expected and missed records."),
                tags$li(strong("SD, SE, CI"), " \u2014 standard deviation, standard error, confidence interval."),
                tags$li(strong("MCAR"), " \u2014 missing completely at random: records missed regardless of age or trait.")))
        ),
        fluidRow(box(width = 12, title = "Preview (first 10 rows)", status = "info", solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
                     tableOutput("data_preview"))),
        fluidRow(
          box(width = 12, title = info_title("Data integrity checks", "integrity"), status = "warning", solidHeader = TRUE,
              uiOutput("integrity_example_note"), tableOutput("integrity_table"), uiOutput("integrity_extra"), save_button("save_integrity"))
        ),
        section_code_box("data")
      ),

      # ---------------------------------------------------------------- visual
      tabItem(tabName = "visual",
        page_title("Is there visual evidence for selective disappearance, and of what type?"),
        guide_box("visual",
          goal = "Are there consistent differences in trait values between individuals with different AFR, ALR, lifespans or age classes? This might be indicative of selective processes. Visualise these qualitatively here.",
          start_with = "comparing how the trait changes with age across different bins of ALR, AFR or LS, or how the trait changes with these across different bins of age.",
          then = "evaluate terminal investment and the selection differentials that might mediate selective processes."
          ),
        fluidRow(
          box(width = 12, title = info_title("Set the number of bins and the selection term of interest", "visual_settings"), status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3, uiOutput("visual_proxy_ui")),
              column(3, sliderInput("n_bins", "Number of bins (lifespan groups; age bins)", min = 3, max = 6, value = 4, step = 1)),
              column(3, uiOutput("facet_ui")),
              column(3, radioButtons("trait_scale", "Trait scale", inline = TRUE, choices = c("Raw" = "raw", "log(trait + 1)" = "log1p")))
            ),
            uiOutput("visual_scale_advice"),
            uiOutput("toy_card_visual")
          )
        ),
        fluidRow(
          box(width = 6, title = info_title("Do long- and short-lived, or early- and late-entering, individuals differ in their phenotype and how it ages?", "a1"), status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4, radioButtons("bin_method", "Bin boundaries", inline = TRUE, choices = c("Quantiles" = "quantile", "Equal width" = "equal"), selected = "quantile")),
              column(3, checkboxInput("show_se", "Show \u00b11 SE", FALSE),
                     checkboxInput("show_bin_legend", "Show bin legend", TRUE),
                     checkboxInput("show_a1_obs", "Show observed means", TRUE)),
              column(5, uiOutput("a1_facet_ui"))
            ),
            plotOutput("a1_plot", height = 560),
              look_for("whether the lifespan groups stay roughly parallel but differ consistently (an age-independent selective process), overlap or differ only stochastically (no selective process apparent), or separate as age increases (an age-dependent selective process)."),
            save_button("save_a1")
          ),
          box(width = 6, title = info_title("How big is the gap between lifespan groups at each age?", "a1_diff"), status = "info", solidHeader = TRUE,
            radioButtons("diff_lines", "Trend lines", inline = TRUE, choices = c("One line per bin pair" = "pairs", "One line across all pairs" = "pooled"), selected = "pairs"),
            fluidRow(column(4, checkboxInput("show_legend_diff", "Show bin legend", TRUE)),
                     column(4, checkboxInput("weight_diff", "Inverse-variance weights", FALSE)),
                     column(4, checkboxInput("diff_points", "Show individual points", TRUE))),
            plotOutput("bin_diff_plot", height = 520),
            look_for("whether the line sits flat at zero (no selective process), stays at a constant non-zero value across age (an age-independent selective process), or becomes steeper or shallower with age (an age-dependent selective process)."),
            save_button("save_a1_diff"))
        ),
        fluidRow(
          box(width = 12, title = info_title("Does the trait track lifespan, and does that change with age?", "a2"), status = "primary", solidHeader = TRUE,
            fluidRow(column(6, checkboxInput("a2_points", "Show individual points (jittered)", TRUE)),
                     column(6, checkboxInput("show_legend_a2", "Show bin legend", TRUE))),
            plotOutput("a2_plot", height = 520),
            look_for("whether the slope is flat at zero (no selective process), constant and different from zero (an age-independent selective process), or changes in steepness across age bins (an age-dependent selective process). For count traits the slope can change through scale alone, so compare against the log scale."),
            save_button("save_a2"))
        ),
        fluidRow(
        section_divider("Advanced diagnoses")
        ),
        fluidRow(
          box(width = 6, title = info_title("Is there terminal investment?", "a5_terminal"), status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              plotOutput("a5_plot", height = 480), uiOutput("a5_note"), save_button("save_a5")),
          box(width = 6, title = info_title("Do survivors differ from those that disappear?", "a6_selection"), status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              radioButtons("a6_compare", "Compare survivors with", inline = TRUE, choices = c("All individuals present" = "population", "Individuals that disappear" = "contrast")),
              plotOutput("a6_plot", height = 440), uiOutput("a6_note"), save_button("save_a6"))
        ),
        fluidRow(
          box(width = 12, title = info_title("How likely is an individual to die at a specific age?: age-independent and age-specific", "a7_hazard"), status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              fluidRow(column(8, plotOutput("a7_plot", height = 460)), column(4, uiOutput("a7_note"))),
              save_button("save_a7"))
        ),
        section_code_box("visual")
      ),

      # ---------------------------------------------------------------- sampling
      tabItem(tabName = "sampling",
        page_title("Is my data complete, and how does that affect which among-individual term best accounts for selective processes?"),
        guide_box("sampling",
          goal = "Decide whether age at last record is a reasonable stand-in for lifespan, and whether gaps in the sampling could distort the analysis. The among-individual term often carries the statistical correction for selective processes. When sampling is complete and AFR does not vary, mean age and LS are perfect proxies of lifespan, so they correct among individuals just as lifespan would. But mean age is a consequence of sampling design and missingness: when AFR varies, sampling is heterogeneous, or data go missing, mean age generally becomes a worse proxy of lifespan than ALR, and so corrects among-individual processes less well.",
          start_with = "the sampling grid and the proxy guidance: they tell you how much of the expected record is missing and which proxy better tracks lifespan.",
          then = "the missingness figures, to see what type of missingness you have."
          ),
        fluidRow(column(12, uiOutput("sampling_caveat"))),
        fluidRow(uiOutput("sampling_metrics")),
        fluidRow(
          box(width = 8, title = info_title("When was each individual actually seen?", "heatmap"), status = "primary", solidHeader = TRUE,
              plotOutput("heatmap", height = "auto"), uiOutput("heatmap_note"), save_button("save_heatmap")),
          box(width = 4, title = info_title("Missingness settings", "sampling_guidance"), status = "info", solidHeader = TRUE,
              checkboxInput("age_step_auto", info_title("Infer the sampling interval", "age_step"), TRUE),
              conditionalPanel("!input.age_step_auto",
                numericInput("age_step_value", "Interval (age units)", value = NA, min = 0, step = 0.5)),
              uiOutput("age_step_note"),
              radioButtons("miss_start", info_title("Count missed occasions from", "afr_expression"),
                           choices = c("Age at first record (AFR)" = "afr",
                                       "Age at first trait expression (AFE)" = "afe"), selected = "afr"),
              conditionalPanel("input.miss_start == 'afe'",
                numericInput("afe_age", "Age at first trait expression (AFE)", value = NA)),
              box_note("Missed occasions are counted only from the age at first record (AFR), or from the age at first expression (AFE) if set, to the age at last record (ALR), both included."),
              sliderInput("heat_n", "Individuals to show in the grid", min = 20, max = 500, value = 150, step = 10),
              selectInput("heat_order", "Order individuals by", choices = c("ALR" = "alr", "AFR" = "afr", "Mean trait value" = "trait", "ID" = "id", "Random" = "random")),
              save_button("save_sampling"))
        ),
        fluidRow(
          box(width = 6, title = info_title("How much is missing, and where?", "missing_by_age"), status = "info", solidHeader = TRUE,
              plotOutput("missing_by_age", height = 440), save_button("save_missing_age")),
          box(width = 6, title = info_title("Is what is missing related to the trait or the individual?", "missing_vs_var"), status = "warning", solidHeader = TRUE, collapsible = TRUE,
              selectInput("miss_var", NULL, choices = c("ALR", "LS", "Condition", "Trait")),
              plotOutput("missing_vs_var", height = 380),
              h5(strong("Observed-data associations with missingness")), tableOutput("drivers_table"),
              uiOutput("missing_trait_note"),
              save_button("save_missing_var"))
        ),
        fluidRow(
          box(width = 12, title = plain_title("How well do different measures of among-individual and selective processes correlate?", "Is age at last record a reliable stand-in for lifespan?", "proxy_agreement"), status = "primary", solidHeader = TRUE,
              uiOutput("proxy_plots_ui"),
              h5(strong(info_title("Agreement between AFR and ALR", "afr_alr"))),
              fluidRow(column(7, plotOutput("afr_alr_plot", height = 420)),
                       column(5, tableOutput("afr_alr_table"))),
              save_button("save_proxies"))
        ),
        section_code_box("sampling")
      ),

      # ---------------------------------------------------------------- A3 individual fits
      tabItem(tabName = "individual",
        page_title("How do traits change at the individual level?"),
        guide_box("individual",
          goal = "Find out which function best fits individual-level data, how the functions compare, and what that function looks like when fitted to population-level data.",
          start_with = "'Which function best describes individual-level data?' \u2014 the functional shape can be chosen here for the modelling step.",
          then = "compare the functional forms, view the trajectory reconstructed by averaging individual functions, and compare the functions at population level."),
        fluidRow(
          box(width = 3, title = info_title("Which function best describes individual-level data?", "a3_settings"), status = "info", solidHeader = TRUE,
            selectInput("a3_function", "Individual ageing function", choices = A3_FUNCTIONS, selected = "Quadratic"),
            radioButtons("a3_scale", info_title("Scale of the individual fits", "a3_scale"), inline = TRUE,
                         choices = c("Automatic" = "auto", "Raw trait" = "identity", "Log (counts)" = "log"), selected = "auto"),
            numericInput("a3_min_records", "Minimum records per individual (0 = the function's own minimum)", value = 0, min = 0, step = 1),

            sliderInput("a3_n", "Individuals to draw", min = 5, max = 60, value = 20, step = 5),
            selectInput("a3_order", "Which individuals", choices = c("Random sample" = "random", "Longest ALR" = "longest", "Shortest ALR" = "shortest")),
            uiOutput("a3_metrics"),
            div(class = "use-function-box", uiOutput("use_a3_function_ui"))
          ),
          box(width = 9, title = info_title("How does each individual change with age?", "a3_fits"), status = "info", solidHeader = TRUE,
              uiOutput("a3_cap_warning"), plotOutput("a3_plot", height = "auto"))
        ),
        fluidRow(
          box(width = 7, title = info_title("What is the average within-individual trajectory?", "a3_mean"), status = "info", solidHeader = TRUE,
              uiOutput("truth_pending_a3"),
              checkboxInput("a3_show_fun", "Show the mean of individual curves (population average)", FALSE),
              plotOutput("a3_mean_plot", height = 520), save_button("save_a3")),
          box(width = 5, title = info_title("Which ageing function has most support at the individual level?", "a3_compare"), status = "primary", solidHeader = TRUE,
              actionButton("run_a3_compare", "Compare ageing functions across individuals", class = "btn-primary btn-block-space", icon = icon("play")),
              uiOutput("shape_headline"),
              tableOutput("a3_compare_table"),
              uiOutput("a3_common_warning"),
              box_note(strong("This comparison is inaccurate with small samples, few sampling time steps, or high missingness."), " \u0394AICc uses only individuals for which every function is estimable (N_common)."),
              save_button("save_a3_compare"))
        ),
        fluidRow(box(width = 12, title = info_title("Individual coefficient summary", "a3_coefs"), status = "info", solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
                     tableOutput("a3_coef_table"))),
        fluidRow(
          box(width = 4, title = info_title("How do different functions fit the population-level data?", "b2"), status = "info", solidHeader = TRUE,
              checkboxGroupInput("b2_functions", "Functions to compare", choices = MODEL_FUNCTIONS, selected = AGE_FUNCTIONS, inline = TRUE),
              actionButton("run_functions", "Compare ageing functions", class = "btn-primary btn-block-space", icon = icon("play")),
              tableOutput("b2_table"),
              save_button("save_b2")),
          box(width = 8, title = info_title("Fitted population trajectories", "b2_curves"), status = "info", solidHeader = TRUE,
              plotOutput("b2_plot", height = 600))
        ),
        section_code_box("individual")
      ),

      # ---------------------------------------------------------------- models
      tabItem(tabName = "models",
        page_title("What model best describes the data, and why?"),
        guide_box("models",
          goal = "Fit mixed-effects models that follow the form of Models 1 to 6 in our manuscript (Sanghvi et al. 2026), plus further models for selective appearance (Models 7\u201310). You set the error family, random terms and covariates, then compare AICs, predicted ageing trajectories, model output and diagnostics.",
          start_with = "choosing a functional form for the likely ageing trajectory, the error distribution and the models to compare, then press Fit models. View the model predictions and the model output.",
          then = "check the model assumptions and diagnostics, then explore the advanced settings: random slopes, higher-order among-individual terms, scaling, and terms added to a specific model.", further = "permute the data to test whether the lifespan effect reflects a real association (Advanced tab)."),
        tabBox(width = 12, id = "model_tabs",
          tabPanel("Fit", icon = icon("play"),
          fluidRow(
            box(width = 12, title = info_title("Model settings", "model_settings"), status = "primary", solidHeader = TRUE,
              fluidRow(
                column(4,
                  selectInput("model_family", "Error family", choices = MODEL_FAMILIES, selected = "gaussian"),
                  uiOutput("family_hint"),
                  conditionalPanel("input.model_family == 'zip' || input.model_family == 'zinb' || input.model_family == 'zinb1'", uiOutput("zi_ui")),
                  conditionalPanel("input.model_family == 'binomial' || input.model_family == 'betabinomial'", uiOutput("trials_ui")),
                  selectInput("model_age_function", "Ageing function", choices = c(AGE_FUNCTIONS, A3_NONLINEAR), selected = "Quadratic"),
                  uiOutput("function_default_note"),
                  selectInput("random_structure", "Random effects for individuals", choices = RANDOM_STRUCTURES, selected = "none"),
                  uiOutput("random_support_note"),
                  tags$details(class = "advanced-block",
                    tags$summary("Advanced options (defaults are fine for most analyses)"),
                    radioButtons("among_order", plain_title("How should differences between individuals enter the model?", "Higher-order among term", "among_order"),
                                 choices = c("Linear (default, as in the manuscript)" = "linear",
                                             "Higher order, consistent decomposition" = "consistent",
                                             "Higher order, powers of the mean" = "same"),
                                 selected = "linear"),
                    checkboxInput("standardise", "Standardise age and proxies (default)", TRUE),
                    p(class = "small-note", "Standardising changes the scale of the coefficients. It does not alter the fitted model when random intercepts and slopes are correlated (or absent). When they are uncorrelated, centring decides the age at which they are assumed uncorrelated, which changes the fit and AIC."),
                    radioButtons("disp_model", info_title("Is the variance in the data changing across age?", "disp"), choices = DISP_OPTIONS, selected = "constant"),
                    checkboxInput("include_invalid", "Include fits with invalid Hessians in the ranking (inspection only)", FALSE))
                ),
                column(8,
                  h5(strong("Models to compare"), span(class = "small-note", " \u2014 tick models; click ", icon("circle-info"), " for what a model tests and its exact specification; add terms to a model in its menu, or click ", icon("wrench"), " to build a term from chosen terms joined by + or \u00d7")),
                  div(class = "model-row",
                      div(class = "model-row-check", checkboxInput("use_M1", paste0("Model 1 \u00b7 ", MODEL_MEANING[["M1"]]$name), TRUE)),
                      actionLink("info_model_M1", icon("circle-info"), class = "info-link"),
                      div(class = "model-row-extra", selectizeInput("extra_M1", NULL, choices = EXTRA_TERM_KEYS, multiple = TRUE,
                                                                   options = list(placeholder = "add terms to this model (optional)"))),
                      actionLink("build_M1", icon("wrench"), class = "info-link", title = "Build a term: chosen terms joined by + or \u00d7 (up to three-way interactions)")),
                  div(class = "model-row",
                      div(class = "model-row-check", checkboxInput("use_M2", paste0("Model 2 \u00b7 ", MODEL_MEANING[["M2"]]$name), TRUE)),
                      actionLink("info_model_M2", icon("circle-info"), class = "info-link"),
                      div(class = "model-row-extra", selectizeInput("extra_M2", NULL, choices = EXTRA_TERM_KEYS, multiple = TRUE,
                                                                   options = list(placeholder = "add terms to this model (optional)"))),
                      actionLink("build_M2", icon("wrench"), class = "info-link", title = "Build a term: chosen terms joined by + or \u00d7 (up to three-way interactions)")),
                  div(class = "model-row",
                      div(class = "model-row-check", checkboxInput("use_M3", paste0("Model 3 \u00b7 ", MODEL_MEANING[["M3"]]$name), TRUE)),
                      actionLink("info_model_M3", icon("circle-info"), class = "info-link"),
                      div(class = "model-row-extra", selectizeInput("extra_M3", NULL, choices = EXTRA_TERM_KEYS, multiple = TRUE,
                                                                   options = list(placeholder = "add terms to this model (optional)"))),
                      actionLink("build_M3", icon("wrench"), class = "info-link", title = "Build a term: chosen terms joined by + or \u00d7 (up to three-way interactions)")),
                  div(class = "model-row",
                      div(class = "model-row-check", checkboxInput("use_M4", paste0("Model 4 \u00b7 ", MODEL_MEANING[["M4"]]$name), TRUE)),
                      actionLink("info_model_M4", icon("circle-info"), class = "info-link"),
                      div(class = "model-row-extra", selectizeInput("extra_M4", NULL, choices = EXTRA_TERM_KEYS, multiple = TRUE,
                                                                   options = list(placeholder = "add terms to this model (optional)"))),
                      actionLink("build_M4", icon("wrench"), class = "info-link", title = "Build a term: chosen terms joined by + or \u00d7 (up to three-way interactions)")),
                  div(class = "model-row",
                      div(class = "model-row-check", checkboxInput("use_M5", paste0("Model 5 \u00b7 ", MODEL_MEANING[["M5"]]$name), TRUE)),
                      actionLink("info_model_M5", icon("circle-info"), class = "info-link"),
                      div(class = "model-row-extra", selectizeInput("extra_M5", NULL, choices = EXTRA_TERM_KEYS, multiple = TRUE,
                                                                   options = list(placeholder = "add terms to this model (optional)"))),
                      actionLink("build_M5", icon("wrench"), class = "info-link", title = "Build a term: chosen terms joined by + or \u00d7 (up to three-way interactions)")),
                  div(class = "model-row",
                      div(class = "model-row-check", checkboxInput("use_M6", paste0("Model 6 \u00b7 ", MODEL_MEANING[["M6"]]$name), FALSE)),
                      actionLink("info_model_M6", icon("circle-info"), class = "info-link"),
                      div(class = "model-row-extra", selectizeInput("extra_M6", NULL, choices = EXTRA_TERM_KEYS, multiple = TRUE,
                                                                   options = list(placeholder = "add terms to this model (optional)"))),
                      actionLink("build_M6", icon("wrench"), class = "info-link", title = "Build a term: chosen terms joined by + or \u00d7 (up to three-way interactions)")),
                  div(class = "model-row",
                      div(class = "model-row-check", checkboxInput("use_M7", paste0("Model 7 \u00b7 ", MODEL_MEANING[["M7"]]$name), FALSE)),
                      actionLink("info_model_M7", icon("circle-info"), class = "info-link"),
                      div(class = "model-row-extra", selectizeInput("extra_M7", NULL, choices = EXTRA_TERM_KEYS, multiple = TRUE,
                                                                   options = list(placeholder = "add terms to this model (optional)"))),
                      actionLink("build_M7", icon("wrench"), class = "info-link", title = "Build a term: chosen terms joined by + or \u00d7 (up to three-way interactions)")),
                  div(class = "model-row",
                      div(class = "model-row-check", checkboxInput("use_M8", paste0("Model 8 \u00b7 ", MODEL_MEANING[["M8"]]$name), FALSE)),
                      actionLink("info_model_M8", icon("circle-info"), class = "info-link"),
                      div(class = "model-row-extra", selectizeInput("extra_M8", NULL, choices = EXTRA_TERM_KEYS, multiple = TRUE,
                                                                   options = list(placeholder = "add terms to this model (optional)"))),
                      actionLink("build_M8", icon("wrench"), class = "info-link", title = "Build a term: chosen terms joined by + or \u00d7 (up to three-way interactions)")),
                  div(class = "model-row",
                      div(class = "model-row-check", checkboxInput("use_M9", paste0("Model 9 \u00b7 ", MODEL_MEANING[["M9"]]$name), FALSE)),
                      actionLink("info_model_M9", icon("circle-info"), class = "info-link"),
                      div(class = "model-row-extra", selectizeInput("extra_M9", NULL, choices = EXTRA_TERM_KEYS, multiple = TRUE,
                                                                   options = list(placeholder = "add terms to this model (optional)"))),
                      actionLink("build_M9", icon("wrench"), class = "info-link", title = "Build a term: chosen terms joined by + or \u00d7 (up to three-way interactions)")),
                  div(class = "model-row",
                      div(class = "model-row-check", checkboxInput("use_M10", paste0("Model 10 \u00b7 ", MODEL_MEANING[["M10"]]$name), FALSE)),
                      actionLink("info_model_M10", icon("circle-info"), class = "info-link"),
                      div(class = "model-row-extra", selectizeInput("extra_M10", NULL, choices = EXTRA_TERM_KEYS, multiple = TRUE,
                                                                   options = list(placeholder = "add terms to this model (optional)"))),
                      actionLink("build_M10", icon("wrench"), class = "info-link", title = "Build a term: chosen terms joined by + or \u00d7 (up to three-way interactions)")),
                  uiOutput("structure_note"),
                  uiOutput("prefit_rows"),
                  actionButton("fit_models", "Fit models", class = "btn-primary btn-block-space", icon = icon("play"))
                )
              )
            )
          ),
          fluidRow(
            box(width = 8, title = info_title("Which selective processes best explain the data?", "model_support"), status = "info", solidHeader = TRUE,
              uiOutput("model_fit_note"),
              uiOutput("sensitivity_banner"),
              fluidRow(
                column(6, plotOutput("aic_plot", height = 400)),
                column(6, uiOutput("model_rowset_warning"), tableOutput("aic_table"))
              ),
              tags$details(class = "info-more", tags$summary(info_title("Nested likelihood-ratio tests", "lrt")),
                           div(class = "info-more-body", tableOutput("lrt_table"))),
              uiOutput("consistency_ui"),
              tags$details(tags$summary(info_title("Fitting status, convergence and dropped rows", "fit_status")), tableOutput("status_table"), uiOutput("drop_note")),
              save_button("save_models")
            ),
            box(width = 4, title = info_title("Compare models across fits", "model_store"), status = "info", solidHeader = TRUE,
              uiOutput("store_pick_ui"),
              actionButton("store_models", "Store selected models", class = "btn-default btn-block-space", icon = icon("plus")),
              uiOutput("store_note"),
              div(style = "overflow-x:auto;", tableOutput("store_table")),
              uiOutput("store_manage_ui"),
              save_button("save_store"))
          ),
          fluidRow(
            box(width = 12, title = info_title("Predicted trajectories of the stored models", "store_pred"), status = "info", solidHeader = TRUE,
              fluidRow(column(4, uiOutput("store_plot_pick_ui")), column(8, plotOutput("store_pred_plot", height = "380px"))))
          ),
          fluidRow(
            box(width = 7, title = plain_title("What ageing trajectory does each model and method predict?", "Model predictions trajectory", "predictions"), status = "primary", solidHeader = TRUE,
              fluidRow(
                column(4, checkboxInput("show_observed", "Observed means", TRUE)),
                column(4, checkboxInput("show_raw_points", "Individual records (jittered)", FALSE)),
                column(4, checkboxInput("show_decomp", "Decomposition", TRUE), info_title("", "decomposition"))
              ),
              fluidRow(
                column(6, checkboxInput("pred_as_lines", "Model predictions as lines, not functional smooths", FALSE)),
                column(6, checkboxInput("show_a3", "Reconstruction from individual fits", FALSE), info_title("", "reconstruction"))
              ),
              fluidRow(column(8, uiOutput("pred_models_ui")), column(4, uiOutput("pred_by_ui"))),
              uiOutput("truth_pending_pred"),
              plotOutput("pred_plot", height = 580),
              look_for("how the predicted age effect depends on the among-individual term used, on modelling selection as additive (age-independent) or as an interaction (age-dependent), and on accounting for entry- and/or removal-based selection. Do the models differ substantially from each other, and from the decomposition (Rebke et al. 2010)? Do they differ from the observed data? Does the reconstructed trajectory from step 4 agree with the best-fitting model?"),
              uiOutput("decomp_note"),
              box_note("Random effects excluded; numeric covariates at their mean; factor covariates marginalised over observed level combinations (weighted by individuals); ALR, LS, AFR and mean age at their individual-level means; response scale. Failed fits are not drawn; Caution fits are dashed."),
              save_button("save_predictions")),
            box(width = 5, title = info_title("How close is each model to the known truth?", "accuracy"), status = "info", solidHeader = TRUE,
              uiOutput("deviation_note"), tableOutput("deviation_table"))
          )
          ),
          tabPanel("Model output", icon = icon("table-list"),
          fluidRow(
            box(width = 12, title = info_title("What does each term in the model predict, and what is the estimated age effect?", "coefficients"), status = "primary", solidHeader = TRUE,
                uiOutput("coef_model_ui"), tableOutput("coef_table"),
                h5(strong("What the selection terms suggest")), uiOutput("coef_interpretation"),
                h5(info_title("Random effects", "varcomp")), tableOutput("coef_re_table"), uiOutput("coef_re_flags"),
                tags$details(class = "info-more", tags$summary("Scaling constants"),
                             div(class = "info-more-body", tableOutput("scaling_table"))),
                save_button("save_coefs"))
          ),
          fluidRow(
            box(width = 12, title = info_title("Peak age and onset of senescence in each model", "peak_onset"), status = "primary", solidHeader = TRUE,
                p(class = "small-note", "From each model's predicted trajectory (the model-predictions figure). 95% CIs: 1,000 draws of the fixed effects."),
                tableOutput("peak_table"),
                uiOutput("peak_sentences"),
                save_button("save_peak"))
          ),
          fluidRow(
            box(width = 12, title = info_title("Do individuals' model-predicted random effects covary with lifespan, ALR or AFR?", "blup_cov"), status = "info", solidHeader = TRUE,
                fluidRow(
                  column(4, uiOutput("blup_model_ui"), uiOutput("blup_note")),
                  column(8, tableOutput("blup_table"))
                ),
                plotOutput("blup_plot", height = 460),
                save_button("save_blup"))
          )
          ),
          tabPanel("Checks", icon = icon("stethoscope"),
          fluidRow(
            box(width = 6, title = info_title("Are my data overdispersed or underdispersed?", "dharma"), status = "warning", solidHeader = TRUE,
                uiOutput("dharma_model_ui"),
                actionButton("run_dharma", "Simulate residuals", class = "btn-default btn-block-space", icon = icon("stethoscope")),
                tableOutput("dharma_table"), plotOutput("dharma_plot", height = 420),
                save_button("save_dharma"))
          ),
          fluidRow(
            box(width = 12, title = info_title("Does my model violate assumptions such as homoscedasticity or normality of residuals?", "performance"), status = "warning", solidHeader = TRUE, collapsible = TRUE,
              fluidRow(
                column(3, uiOutput("performance_model_ui"),
                       actionButton("run_performance", "Run performance checks", class = "btn-default btn-block-space", icon = icon("clipboard-check")),
                       save_button("save_performance")),
                column(9, tableOutput("performance_table"), uiOutput("performance_note"), plotOutput("performance_plot", height = 860))
              ))
          ),
          fluidRow(
            box(width = 6, title = info_title("Error-family check", "family_check"), status = "warning", solidHeader = TRUE,
                selectInput("family_check_model", "Fit this model with each count family", choices = stats::setNames(MODEL_IDS, model_label(MODEL_IDS)), selected = "M4"),
                actionButton("run_family_check", "Compare error families", class = "btn-default btn-block-space", icon = icon("scale-balanced")),
                tableOutput("family_table"),
                save_button("save_family")),
            box(width = 6, title = info_title("Ageing-function check", "function_check"), status = "warning", solidHeader = TRUE,
                selectInput("function_check_model", "Fit this model with each ageing function",
                            choices = stats::setNames(MODEL_IDS, model_label(MODEL_IDS)), selected = "M1"),
                actionButton("run_function_check", "Compare ageing functions", class = "btn-default btn-block-space", icon = icon("chart-line")),
                tableOutput("function_check_table"),
                uiOutput("function_check_note"),
                save_button("save_function_check"))
          )
          ),
          tabPanel("Advanced", icon = icon("flask"),
          fluidRow(
            box(width = 12, title = "How large is the selective process's influence, and is it stronger than chance alone would produce?", status = "warning",
                solidHeader = TRUE,
                fluidRow(
                  column(6,
                    h5(info_title("Effect sizes", "effect_sizes")),
                    p(class = "small-note", "How much the correction changed the answer, on the trait's own scale."),
                    actionButton("run_effects", "Compute effect sizes", class = "btn-default btn-block-space", icon = icon("ruler")),
                    tableOutput("effect_table"),
                    save_button("save_effects")),
                  column(6,
                    h5(info_title("Null-model bootstrap", "permutation")),
                    p(class = "small-note", "Simulates data from a model with no lifespan term, keeping every individual's real ages, ALR and AFR, and refits, so the model's AIC advantage can be read against data with no link between lifespan and the trait."),
                    fluidRow(
                      column(6, uiOutput("perm_model_ui")),
                      column(6, numericInput("perm_n", "Simulated datasets", value = 39, min = 1, max = 999, step = 10))),
                    actionButton("run_perm", "Run bootstrap test", class = "btn-default btn-block-space", icon = icon("dice")),
                    uiOutput("perm_result"),
                    plotOutput("perm_plot", height = 340),
                    save_button("save_perm")))
            )
          )
          ),
          tabPanel("Code", icon = icon("code"),
          fluidRow(
            box(width = 12, title = info_title("Reproducible R code for this comparison", "rcode"), status = "info", solidHeader = TRUE, collapsible = TRUE,
                downloadButton("download_code", "Download R script", class = "btn-default"),
                downloadButton("download_analysis_data", "Analysis data (CSV)", class = "btn-default"),
                save_button("save_code", "Save code to summary"),
                uiOutput("code_ui"))
          )
          )
        )
      ),
      # ---------------------------------------------------------------- summary
      tabItem(tabName = "summary",
        page_title("What did I find, and do the pieces agree?"),
        guide_box("summary",
          goal = "Collect what you found into one place, see whether the pieces of evidence agree, and export the analysis.",
          start_with = "the Evidence summary below: it grades what your saved results suggest and lists the checks still to run.",
          then = "download the report, the R script and the reproducibility bundle.",
          tip = "Only results you saved are summarised. Use the 'Save to summary' buttons in steps 2 to 5."),
        fluidRow(
          box(width = 12, title = info_title("Saved results: manage and export", "saved"), status = "primary", solidHeader = TRUE,
            div(class = "saved-controls",
              fluidRow(
                column(5, uiOutput("saved_controls")),
                column(7,
                  downloadButton("download_report_html", "Download HTML report (with plots)", class = "btn-primary"),
                  downloadButton("download_bundle", "Reproducibility bundle (JSON)", class = "btn-default"),
                  downloadButton("download_report", "Download text report", class = "btn-default"),
                  downloadButton("download_code2", "Download R script", class = "btn-default"),
                  downloadButton("download_analysis_data2", "Analysis data (CSV)", class = "btn-default"))
              )
            ),
            div(class = "caution-box",
                div(class = "result-head", "The summary reads every saved result"),
                tags$ul(
                  tags$li("If an exploratory or poor result is saved, it is included in the interpretation. Remove any analysis you do not want counted."),
                  tags$li("If several models are saved, all of them are used. Remove the models you do not want interpreted.")))
          )
        ),
        fluidRow(
          box(width = 12, title = info_title("Evidence summary: what do the saved results suggest?", "evidence"),
              status = "primary", solidHeader = TRUE,
              uiOutput("evidence_summary"))
        ),
        fluidRow(
          box(width = 12, title = info_title("Auto-written methods", "methods_text"), status = "primary", solidHeader = TRUE,
              uiOutput("methods_pick_ui"),
              uiOutput("methods_conflicts"),
              uiOutput("methods_text_ui"),
              downloadButton("download_methods", "Download methods (text)", class = "btn-default"))
        ),
        fluidRow(
          box(width = 12, title = "Saved results (these are exported)", status = "info", solidHeader = TRUE,
              uiOutput("saved_ui"))
        )
      )
    )
  )
)
