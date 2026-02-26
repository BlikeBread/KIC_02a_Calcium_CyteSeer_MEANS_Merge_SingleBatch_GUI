###############################################################################
# KIC CALCIUM ANALYSIS PIPELINE — STEP 2a
# -----------------------------------------------------------------------------
#
# PURPOSE:
#   Interactive GUI tool to merge CyteSeer-derived WHOLE-IMAGE MEANS tables
#   from a single experiment/batch.
#
#   The script:
#     1. Prompts the user to select input/output folders (tk_choose.dir)
#     2. Recursively scans the input directory for *MEANS*.csv files
#        generated in Step 1 (/summary folder)
#     3. Reads and merges all MEANS tables into a unified batch-level dataset
#     4. Flags wells based on:
#          - Valid plate format (A01–H12)
#          - Num.Peaks within a user-defined 1 Hz range
#     5. Allows two workflows:
#          - Filtered mode (drops invalid wells and peaks out of range)
#          - Flag-only mode (retains all rows and adds logical flags)
#     6. Assigns experimental Group labels based on user-defined column rules
#     7. Optionally assigns RowGroup labels based on user-defined row rules
#     8. Appends Batch identifier to all rows
#     9. Exports a standardized merged.csv file for downstream analysis
#
# OUTPUT:
#   An output folder containing:
#     merged.csv   (single-batch merged MEANS dataset with flags and groups)
#
# NOTES:
#   - Uses tcltk (tk_choose.dir) for macOS folder pickers.
#   - Designed for interactive use (GUI folder selection + automated exports).
#   - Intended as Step 2a of a 3-step KIC calcium processing pipeline.
#   - Step 2b will merge multiple batch-level merged.csv files.
#
# AUTHORS:
#   Michele Buono
#   Talitha Spanjersberg
#   Nikki Scheen
#   Nina van der Wilt
#   Regenerative Medicine Center Utrecht (2026)
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

if (!requireNamespace("tcltk", quietly = TRUE)) {
  stop("Package 'tcltk' not available. On macOS install XQuartz, then restart R.")
}

# ----------------------------- GUI (ONE WINDOW) ------------------------------
kic_merge_settings_gui <- function(
    default_batch = format(Sys.Date(), "%y%m%d"),
    default_peaks_min = 8,
    default_peaks_max = 12,
    default_group_rules = "C1:1-3\nC2:4-6\nP1:7-9\nP2:10-12",
    default_row_rules = "D0:A,B\nD7:C,D\nD14:E,F\nD21:G,H"
) {
  bg <- "white"; fg <- "black"; font <- "Helvetica 13"
  
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "KIC — Merge MEANS (Step 1)")
  tcltk::tkconfigure(tt, background = bg)
  
  v_in_dir  <- tcltk::tclVar("")
  v_out_dir <- tcltk::tclVar("")
  v_batch   <- tcltk::tclVar(default_batch)
  v_pmin    <- tcltk::tclVar(as.character(default_peaks_min))
  v_pmax    <- tcltk::tclVar(as.character(default_peaks_max))
  
  v_workflow  <- tcltk::tclVar("filtered")   # filtered | flag
  v_plateMode <- tcltk::tclVar("group_only") # group_only | group_row
  
  frm <- tcltk::tkframe(tt, padx = 12, pady = 12, background = bg)
  tcltk::tkgrid(frm, sticky = "nsew")
  tcltk::tkgrid.columnconfigure(frm, 1, weight = 1)
  
  lab <- function(text) tcltk::tklabel(frm, text = text, anchor = "w", background = bg, foreground = fg, font = font)
  btn <- function(text, command) tcltk::tkbutton(frm, text = text, command = command, background = bg, foreground = fg, font = font)
  ent <- function(var, width=55) tcltk::tkentry(frm, textvariable = var, width = width, background = "white", foreground = "black", font = font)
  
  choose_dir <- function(var, caption) {
    d <- tcltk::tk_choose.dir(caption = caption)
    if (!is.na(d) && nzchar(d)) tcltk::tclvalue(var) <- d
  }
  
  row <- 0
  tcltk::tkgrid(lab("Input folder (MEANS CSVs; recursive):"), row=row, column=0, sticky="w", padx=4, pady=4)
  tcltk::tkgrid(ent(v_in_dir), row=row, column=1, sticky="we", padx=4, pady=4)
  tcltk::tkgrid(btn("Browse…", function() choose_dir(v_in_dir, "Select input folder")), row=row, column=2, sticky="e", padx=4, pady=4)
  
  row <- row + 1
  tcltk::tkgrid(lab("Output folder (writes merged.csv):"), row=row, column=0, sticky="w", padx=4, pady=4)
  tcltk::tkgrid(ent(v_out_dir), row=row, column=1, sticky="we", padx=4, pady=4)
  tcltk::tkgrid(btn("Browse…", function() choose_dir(v_out_dir, "Select output folder")), row=row, column=2, sticky="e", padx=4, pady=4)
  
  row <- row + 1
  tcltk::tkgrid(lab("Batch name:"), row=row, column=0, sticky="w", padx=4, pady=4)
  tcltk::tkgrid(ent(v_batch, width=18), row=row, column=1, sticky="w", padx=4, pady=4)
  
  row <- row + 1
  tcltk::tkgrid(lab("Num.Peaks keep range (1 Hz):"), row=row, column=0, sticky="w", padx=4, pady=4)
  f_thr <- tcltk::tkframe(frm, background = bg)
  tcltk::tkgrid(f_thr, row=row, column=1, sticky="w", padx=4, pady=4)
  tcltk::tkgrid(tcltk::tklabel(f_thr, text="min", background=bg, foreground=fg, font=font), row=0, column=0, padx=3)
  tcltk::tkgrid(tcltk::tkentry(f_thr, textvariable=v_pmin, width=6, background="white", foreground="black", font=font), row=0, column=1, padx=3)
  tcltk::tkgrid(tcltk::tklabel(f_thr, text="max", background=bg, foreground=fg, font=font), row=0, column=2, padx=3)
  tcltk::tkgrid(tcltk::tkentry(f_thr, textvariable=v_pmax, width=6, background="white", foreground="black", font=font), row=0, column=3, padx=3)
  
  row <- row + 1
  tcltk::tkgrid(lab("Workflow:"), row=row, column=0, sticky="nw", padx=4, pady=4)
  f_wf <- tcltk::tkframe(frm, background = bg)
  tcltk::tkgrid(f_wf, row=row, column=1, sticky="w", padx=4, pady=4)
  tcltk::tkgrid(tcltk::tkradiobutton(f_wf, text="Filtered (drop invalid wells + peaks out of range)",
                                     variable=v_workflow, value="filtered", background=bg, foreground=fg, font=font),
                row=0, column=0, sticky="w", padx=4)
  tcltk::tkgrid(tcltk::tkradiobutton(f_wf, text="Flag-only (keep all rows; add flags only)",
                                     variable=v_workflow, value="flag", background=bg, foreground=fg, font=font),
                row=1, column=0, sticky="w", padx=4)
  
  row <- row + 1
  tcltk::tkgrid(lab("Plate mode:"), row=row, column=0, sticky="nw", padx=4, pady=4)
  f_pm <- tcltk::tkframe(frm, background = bg)
  tcltk::tkgrid(f_pm, row=row, column=1, sticky="w", padx=4, pady=4)
  tcltk::tkgrid(tcltk::tkradiobutton(f_pm, text="Group only (columns)",
                                     variable=v_plateMode, value="group_only", background=bg, foreground=fg, font=font),
                row=0, column=0, sticky="w", padx=4)
  tcltk::tkgrid(tcltk::tkradiobutton(f_pm, text="Group + RowGroup (columns + rows)",
                                     variable=v_plateMode, value="group_row", background=bg, foreground=fg, font=font),
                row=1, column=0, sticky="w", padx=4)
  
  row <- row + 1
  tcltk::tkgrid(lab("GROUP_RULES (one per line, e.g. C1:1-3):"), row=row, column=0, sticky="nw", padx=4, pady=4)
  txt_groups <- tcltk::tktext(frm, height=5, width=55, background="white", foreground="black", font=font)
  tcltk::tkgrid(txt_groups, row=row, column=1, columnspan=2, sticky="we", padx=4, pady=4)
  tcltk::tkinsert(txt_groups, "1.0", default_group_rules)
  
  row <- row + 1
  tcltk::tkgrid(lab("ROW_RULES (optional; only if Group+RowGroup):"), row=row, column=0, sticky="nw", padx=4, pady=4)
  txt_rows <- tcltk::tktext(frm, height=5, width=55, background="white", foreground="black", font=font)
  tcltk::tkgrid(txt_rows, row=row, column=1, columnspan=2, sticky="we", padx=4, pady=4)
  tcltk::tkinsert(txt_rows, "1.0", default_row_rules)
  
  row <- row + 1
  v_status <- tcltk::tclVar("Ready.")
  tcltk::tkgrid(tcltk::tklabel(frm, textvariable=v_status, background=bg, foreground=fg, font=font),
                row=row, column=0, columnspan=3, sticky="w", padx=4, pady=6)
  
  res <- NULL
  validate_int <- function(x) suppressWarnings(!is.na(as.integer(trimws(x))))
  
  on_run <- function() {
    in_dir  <- trimws(tcltk::tclvalue(v_in_dir))
    out_dir <- trimws(tcltk::tclvalue(v_out_dir))
    batch   <- trimws(tcltk::tclvalue(v_batch))
    pmin_s  <- trimws(tcltk::tclvalue(v_pmin))
    pmax_s  <- trimws(tcltk::tclvalue(v_pmax))
    
    if (!nzchar(in_dir))  { tcltk::tclvalue(v_status) <- "ERROR: select input folder."; return() }
    if (!nzchar(out_dir)) { tcltk::tclvalue(v_status) <- "ERROR: select output folder."; return() }
    if (!nzchar(batch))   { tcltk::tclvalue(v_status) <- "ERROR: batch name empty."; return() }
    if (!validate_int(pmin_s) || !validate_int(pmax_s)) { tcltk::tclvalue(v_status) <- "ERROR: peaks min/max must be integers."; return() }
    
    pmin <- as.integer(pmin_s); pmax <- as.integer(pmax_s)
    if (pmin > pmax) { tcltk::tclvalue(v_status) <- "ERROR: min must be <= max."; return() }
    
    group_rules_txt <- as.character(tcltk::tkget(txt_groups, "1.0", "end-1c"))
    row_rules_txt   <- as.character(tcltk::tkget(txt_rows,   "1.0", "end-1c"))
    
    group_rules <- trimws(unlist(strsplit(group_rules_txt, "\n", fixed=TRUE)))
    group_rules <- group_rules[nzchar(group_rules)]
    if (!length(group_rules)) { tcltk::tclvalue(v_status) <- "ERROR: GROUP_RULES empty."; return() }
    
    row_rules <- trimws(unlist(strsplit(row_rules_txt, "\n", fixed=TRUE)))
    row_rules <- row_rules[nzchar(row_rules)]
    
    res <<- list(
      input_dir   = in_dir,
      output_dir  = out_dir,
      batch_name  = batch,
      peaks_min   = pmin,
      peaks_max   = pmax,
      workflow    = tcltk::tclvalue(v_workflow),
      plate_mode  = tcltk::tclvalue(v_plateMode),
      group_rules = group_rules,
      row_rules   = row_rules
    )
    tcltk::tclvalue(v_status) <- "Running…"
    tcltk::tkdestroy(tt)
  }
  
  on_cancel <- function() { res <<- NULL; tcltk::tkdestroy(tt) }
  
  row <- row + 1
  bf <- tcltk::tkframe(frm, background=bg)
  tcltk::tkgrid(bf, row=row, column=0, columnspan=3, sticky="e", padx=4, pady=6)
  tcltk::tkgrid(tcltk::tkbutton(bf, text="Cancel", command=on_cancel, background=bg, foreground=fg, font=font),
                row=0, column=0, padx=6)
  tcltk::tkgrid(tcltk::tkbutton(bf, text="Run", command=on_run, background=bg, foreground=fg, font=font),
                row=0, column=1, padx=6)
  
  tcltk::tkwait.window(tt)
  res
}

# ------------------------------ Core helpers --------------------------------
sanitize_well <- function(x) toupper(trimws(as.character(x)))

find_means_csvs <- function(root) {
  files_all <- list.files(root, pattern="\\.csv$", full.names=TRUE, recursive=TRUE, ignore.case=TRUE)
  files_all[grepl("MEANS", basename(files_all), ignore.case=TRUE)]
}

read_and_merge <- function(files) {
  dt_list <- lapply(files, function(f) {
    dt <- fread(f, sep="auto", showProgress=FALSE)
    dt[, source_file := tools::file_path_sans_ext(basename(f))]
    dt
  })
  rbindlist(dt_list, use.names=TRUE, fill=TRUE)
}

flag_rows <- function(df, peaks_min, peaks_max) {
  stopifnot("Num.Peaks" %in% names(df), "Well" %in% names(df))
  df$Well <- sanitize_well(df$Well)
  df$NumPeaks_num <- suppressWarnings(as.numeric(trimws(df[["Num.Peaks"]])))
  df$keep_1hz <- !is.na(df$NumPeaks_num) & df$NumPeaks_num >= peaks_min & df$NumPeaks_num <= peaks_max
  df$valid_well <- !is.na(df$Well) & grepl("^[A-H](0[1-9]|1[0-2])$", df$Well)
  df
}

filter_rows_strict <- function(df) df[df$keep_1hz & df$valid_well, , drop=FALSE]

parse_cols_spec <- function(spec) {
  parts <- unlist(strsplit(gsub("\\s+","",spec), ",", fixed=TRUE), use.names=FALSE)
  out <- integer(0)
  for (p in parts) {
    if (grepl("^[0-9]+-[0-9]+$", p)) {
      a <- as.integer(sub("-.*$","",p)); b <- as.integer(sub("^.*-","",p))
      out <- c(out, seq(min(a,b), max(a,b)))
    } else if (grepl("^[0-9]+$", p)) out <- c(out, as.integer(p))
  }
  unique(out)
}

parse_rules_vec <- function(vec) {
  nm  <- sub(":.*$", "", vec)
  rhs <- sub("^.*?:", "", vec)
  setNames(lapply(rhs, parse_cols_spec), nm)
}

assign_groups_by_cols <- function(df, rules_list) {
  w <- sanitize_well(df$Well)
  colnum <- suppressWarnings(as.integer(sub("^[A-Z]","",w)))
  
  grp <- rep(NA_character_, length(colnum))
  for (g in names(rules_list)) {
    grp[!is.na(colnum) & colnum %in% rules_list[[g]] & is.na(grp)] <- g
  }
  
  if (!"Group" %in% names(df)) {
    df$Group <- grp
  } else {
    g0 <- as.character(df$Group)
    if (length(g0) != nrow(df)) g0 <- rep(NA_character_, nrow(df))
    idx <- is.na(g0) | g0 == ""
    g0[idx] <- grp[idx]
    df$Group <- g0
  }
  df
}

parse_rows_spec <- function(spec) {
  parts <- unlist(strsplit(gsub("\\s+","",spec), ",", fixed=TRUE), use.names=FALSE)
  out <- character(0)
  for (p in parts) {
    p <- toupper(p)
    if (grepl("^[A-Z]-[A-Z]$", p)) {
      a <- substr(p,1,1); b <- substr(p,3,3)
      out <- c(out, LETTERS[seq(match(a,LETTERS), match(b,LETTERS))])
    } else if (grepl("^[A-Z]$", p)) out <- c(out, p)
  }
  unique(out)
}

parse_row_rules_vec <- function(vec) {
  nm  <- sub(":.*$", "", vec)
  rhs <- sub("^.*?:", "", vec)
  setNames(lapply(rhs, parse_rows_spec), nm)
}

assign_rowgroups_by_rows <- function(df, rules_list) {
  w <- sanitize_well(df$Well)
  row_letter <- sub("[0-9]+$","",w)
  
  rg <- rep(NA_character_, length(row_letter))
  for (g in names(rules_list)) {
    rg[!is.na(row_letter) & row_letter %in% rules_list[[g]] & is.na(rg)] <- g
  }
  
  if (!"RowGroup" %in% names(df)) {
    df$RowGroup <- rg
  } else {
    r0 <- as.character(df$RowGroup)
    if (length(r0) != nrow(df)) r0 <- rep(NA_character_, nrow(df))
    idx <- is.na(r0) | r0 == ""
    r0[idx] <- rg[idx]
    df$RowGroup <- r0
  }
  df
}

# ----------------------------------- RUN ------------------------------------
cfg <- kic_merge_settings_gui()
if (is.null(cfg)) stop("Cancelled by user.")

files <- find_means_csvs(cfg$input_dir)
if (!length(files)) stop("No MEANS CSVs found under: ", cfg$input_dir)

message("Found ", length(files), " MEANS file(s). Merging…")
merged <- read_and_merge(files)

if (!"Num.Peaks" %in% names(merged)) stop("Column 'Num.Peaks' not found.")
if (!"Well" %in% names(merged)) stop("Column 'Well' not found.")

merged <- flag_rows(merged, cfg$peaks_min, cfg$peaks_max)
if (cfg$workflow == "filtered") merged <- filter_rows_strict(merged)

merged <- assign_groups_by_cols(merged, parse_rules_vec(cfg$group_rules))

if (cfg$plate_mode == "group_row" && length(cfg$row_rules)) {
  merged <- assign_rowgroups_by_rows(merged, parse_row_rules_vec(cfg$row_rules))
} else {
  if (!"RowGroup" %in% names(merged)) merged$RowGroup <- NA_character_
}

merged$Batch <- cfg$batch_name

dir.create(cfg$output_dir, recursive=TRUE, showWarnings=FALSE)
outfile <- file.path(cfg$output_dir, "merged.csv")

ord <- c(
  intersect(c("Well","Group","RowGroup","Batch","Num.Peaks","NumPeaks_num","keep_1hz","valid_well","source_file"), names(merged)),
  setdiff(names(merged), c("Well","Group","RowGroup","Batch","Num.Peaks","NumPeaks_num","keep_1hz","valid_well","source_file"))
)

# ✅ FIX: data.table column selection via variable -> ..ord
data.table::fwrite(merged[, ..ord], outfile)

message("Saved merged.csv → ", outfile)