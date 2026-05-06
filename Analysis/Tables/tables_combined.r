# =============================================================================
# combine_tables.R
# Combines all HTML table files into:
#   1. A single combined HTML file
#   2. A Markdown (.md) file  
#   3. A PDF file
# =============================================================================

library(rvest)
library(xml2)
library(stringr)

# ── 0. Configuration ──────────────────────────────────────────────────────────

tables_dir  <- "/Users/alfredoverastegui/Endo_Exo_after_PD/Endo_Exo_Results/Tables"
output_dir  <- tables_dir

html_files <- c(
  "Table1_Patient_Preoperative_Characteristics.html",
  "Table2_Operative_Pathologic_Outcomes.html",
  "Table3_Endocrine_Exocrine_Dysfunction.html",
  "Table4A_Univariable_DM.html",
  "Table4B_Multivariable_DM.html",
  "Table5A_Univariable_Exocrine.html",
  "Table5B_Multivariable_Exocrine.html",
  "Table6_HbA1c_Trajectory.html",
  "Table7_LMM_HbA1c_Trajectory.html",
  "Table8_FineGray_DM.html",
  "Table9_FineGray_ExocrineInsufficiency.html"
)

table_labels <- str_replace_all(
  str_replace(html_files, "\\.html$", ""),
  "_", " "
)

# ── 1. Build combined HTML ────────────────────────────────────────────────────

css_style <- "
<style>
  body {
    font-family: 'Arial', sans-serif;
    font-size: 11pt;
    margin: 30px 40px;
    color: #1a1a1a;
  }
  h1.doc-title {
    font-size: 18pt;
    text-align: center;
    margin-bottom: 6px;
  }
  p.doc-subtitle {
    text-align: center;
    color: #555;
    margin-top: 0;
    font-size: 10pt;
  }
  h2.table-heading {
    font-size: 13pt;
    margin-top: 40px;
    margin-bottom: 8px;
    border-bottom: 2px solid #2c5f8a;
    padding-bottom: 4px;
    color: #2c5f8a;
  }
  .table-section {
    page-break-inside: avoid;
    margin-bottom: 30px;
  }
  table {
    border-collapse: collapse;
    width: 100%;
    font-size: 10pt;
  }
  th, td {
    border: 1px solid #ccc;
    padding: 5px 8px;
    text-align: left;
  }
  th {
    background-color: #e8f0f7;
    font-weight: bold;
  }
  tr:nth-child(even) td {
    background-color: #f9f9f9;
  }
  hr.page-sep {
    border: none;
    border-top: 1px dashed #aaa;
    margin: 30px 0;
  }
  @media print {
    .table-section { page-break-before: auto; }
    h2.table-heading { page-break-after: avoid; }
  }
</style>
"

today_str <- format(Sys.Date(), "%B %d, %Y")

html_header <- paste0(
  "<!DOCTYPE html>\n<html lang='en'>\n<head>\n",
  "<meta charset='UTF-8'>\n",
  "<title>Endo-Exo After PD - Combined Tables</title>\n",
  css_style,
  "</head>\n<body>\n",
  "<h1 class='doc-title'>Endocrine &amp; Exocrine Dysfunction After Pancreaticoduodenectomy</h1>\n",
  "<p class='doc-subtitle'>Combined Results Tables &mdash; Generated: ", today_str, "</p>\n",
  "<hr class='page-sep'>\n"
)

html_footer <- "\n</body>\n</html>"

sections <- character(length(html_files))

for (i in seq_along(html_files)) {
  fp <- file.path(tables_dir, html_files[i])
  
  if (!file.exists(fp)) {
    warning("File not found, skipping: ", fp)
    sections[i] <- paste0(
      "<div class='table-section'>",
      "<h2 class='table-heading'>", table_labels[i], "</h2>",
      "<p><em>File not found: ", html_files[i], "</em></p>",
      "</div>\n"
    )
    next
  }
  
  raw_html  <- paste(readLines(fp, warn = FALSE), collapse = "\n")
  parsed    <- tryCatch(read_html(raw_html), error = function(e) NULL)
  
  if (is.null(parsed)) {
    inner_html <- raw_html
  } else {
    body_node  <- html_node(parsed, "body")
    if (is.na(body_node)) {
      inner_html <- raw_html
    } else {
      inner_html <- as.character(body_node)
      inner_html <- str_replace(inner_html, "^<body[^>]*>", "")
      inner_html <- str_replace(inner_html, "</body>$", "")
    }
  }
  
  sections[i] <- paste0(
    "<div class='table-section'>\n",
    "<h2 class='table-heading'>", table_labels[i], "</h2>\n",
    inner_html, "\n",
    "</div>\n",
    if (i < length(html_files)) "<hr class='page-sep'>\n" else ""
  )
  
  cat(sprintf("  [%02d/%02d] Processed: %s\n", i, length(html_files), html_files[i]))
}

combined_html <- paste0(html_header, paste(sections, collapse = "\n"), html_footer)

# ── 2. Write combined HTML ────────────────────────────────────────────────────

out_html <- file.path(output_dir, "Combined_Tables.html")
writeLines(combined_html, out_html, useBytes = TRUE)
cat("\n✓ Combined HTML saved to:\n  ", out_html, "\n")

# ── 3. Write Markdown ─────────────────────────────────────────────────────────

html_to_md_table <- function(html_string) {
  tryCatch({
    doc    <- read_html(html_string)
    tables <- html_nodes(doc, "table")
    if (length(tables) == 0) return(html_string)
    
    md_tables <- lapply(tables, function(tbl) {
      rows <- html_nodes(tbl, "tr")
      if (length(rows) == 0) return("")
      
      header_cells <- html_nodes(rows[[1]], "th, td")
      header_text  <- trimws(html_text(header_cells, trim = TRUE))
      header_text  <- str_replace_all(header_text, "\\|", "\\\\|")
      
      md_header <- paste0("| ", paste(header_text, collapse = " | "), " |")
      md_sep    <- paste0("| ", paste(rep("---", length(header_text)), collapse = " | "), " |")
      
      md_rows <- character(0)
      if (length(rows) > 1) {
        for (r in rows[-1]) {
          cells     <- html_nodes(r, "td, th")
          cell_text <- trimws(html_text(cells, trim = TRUE))
          cell_text <- str_replace_all(cell_text, "\\|", "\\\\|")
          if (length(cell_text) < length(header_text)) {
            cell_text <- c(cell_text, rep("", length(header_text) - length(cell_text)))
          }
          md_rows <- c(md_rows, paste0("| ", paste(cell_text, collapse = " | "), " |"))
        }
      }
      
      paste(c(md_header, md_sep, md_rows), collapse = "\n")
    })
    
    paste(md_tables, collapse = "\n\n")
    
  }, error = function(e) {
    paste("*[Table rendering failed]*\n\n")
  })
}

md_lines <- c(
  "# Endocrine & Exocrine Dysfunction After Pancreaticoduodenectomy",
  "",
  paste0("**Generated:** ", today_str),
  "",
  "---",
  ""
)

for (i in seq_along(html_files)) {
  fp <- file.path(tables_dir, html_files[i])
  md_lines <- c(md_lines, paste0("## ", table_labels[i]), "")
  if (!file.exists(fp)) {
    md_lines <- c(md_lines, paste0("*File not found: ", html_files[i], "*"), "")
    next
  }
  raw_html   <- paste(readLines(fp, warn = FALSE), collapse = "\n")
  md_content <- html_to_md_table(raw_html)
  md_lines   <- c(md_lines, md_content, "", "---", "")
}

out_md <- file.path(output_dir, "Combined_Tables.md")
writeLines(md_lines, out_md)
cat("✓ Markdown saved to:\n  ", out_md, "\n")

# ── 4. Write PDF ──────────────────────────────────────────────────────────────

out_pdf     <- file.path(output_dir, "Combined_Tables.pdf")
pdf_success <- FALSE

# ── Method A: pagedown with explicit Chrome path detection ────────────────────
if (!pdf_success && requireNamespace("pagedown", quietly = TRUE)) {
  
  # Common Chrome/Chromium/Edge locations on macOS (Intel + Apple Silicon)
  chrome_candidates <- c(
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
    "/Applications/Arc.app/Contents/MacOS/Arc",
    path.expand("~/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"),
    path.expand("~/Applications/Chromium.app/Contents/MacOS/Chromium"),
    Sys.which("google-chrome"),
    Sys.which("chromium"),
    Sys.which("chromium-browser"),
    Sys.which("google-chrome-stable")
  )
  
  chrome_path <- NA
  for (cp in chrome_candidates) {
    if (!is.na(cp) && nchar(trimws(cp)) > 0 && file.exists(cp)) {
      chrome_path <- cp
      break
    }
  }
  
  if (!is.na(chrome_path)) {
    cat("\nAttempting PDF via pagedown using:\n  ", chrome_path, "\n")
    Sys.setenv(PAGEDOWN_CHROME = chrome_path)
    
    tryCatch({
      pagedown::chrome_print(
        input   = out_html,
        output  = out_pdf,
        browser = chrome_path,
        options = list(
          printBackground = TRUE,
          marginTop    = 0.5,
          marginBottom = 0.5,
          marginLeft   = 0.5,
          marginRight  = 0.5
        ),
        timeout = 120
      )
      pdf_success <- TRUE
      cat("✓ PDF saved to:\n  ", out_pdf, "\n")
    }, error = function(e) {
      cat("  pagedown failed:", conditionMessage(e), "\n")
    })
    
  } else {
    cat("\n  pagedown: could not locate Chrome/Chromium/Edge — trying next method.\n")
  }
}

# ── Method B: wkhtmltopdf ─────────────────────────────────────────────────────
if (!pdf_success) {
  wk_candidates <- c(
    Sys.which("wkhtmltopdf"),
    "/usr/local/bin/wkhtmltopdf",
    "/opt/homebrew/bin/wkhtmltopdf",   # Apple Silicon Homebrew
    "/usr/bin/wkhtmltopdf"
  )
  wk <- NA
  for (w in wk_candidates) {
    if (!is.na(w) && nchar(trimws(w)) > 0 && file.exists(w)) { wk <- w; break }
  }
  
  if (!is.na(wk)) {
    cat("\nAttempting PDF via wkhtmltopdf...\n")
    cmd <- sprintf(
      '"%s" --enable-local-file-access --page-size Letter --margin-top 15 --margin-bottom 15 --margin-left 15 --margin-right 15 "%s" "%s"',
      wk, out_html, out_pdf
    )
    ret <- system(cmd)
    if (ret == 0 && file.exists(out_pdf)) {
      pdf_success <- TRUE
      cat("✓ PDF saved to:\n  ", out_pdf, "\n")
    } else {
      cat("  wkhtmltopdf returned non-zero exit.\n")
    }
  }
}

# ── Method C: Direct Chrome CLI call (bypasses pagedown entirely) ─────────────
if (!pdf_success) {
  chrome_candidates <- c(
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"
  )
  chrome_path <- chrome_candidates[file.exists(chrome_candidates)][1]
  
  if (!is.na(chrome_path)) {
    cat("\nAttempting PDF via direct Chrome CLI...\n")
    cmd <- sprintf(
      '"%s" --headless --disable-gpu --no-sandbox --print-to-pdf="%s" --print-to-pdf-no-header "file://%s" 2>/dev/null',
      chrome_path, out_pdf, out_html
    )
    ret <- system(cmd)
    if (ret == 0 && file.exists(out_pdf)) {
      pdf_success <- TRUE
      cat("✓ PDF saved to:\n  ", out_pdf, "\n")
    } else {
      cat("  Chrome CLI failed.\n")
    }
  }
}

# ── Failure message ───────────────────────────────────────────────────────────
if (!pdf_success) {
  cat("\n⚠  PDF could not be generated automatically.\n\n")
  cat("── QUICKEST FIX: run in Terminal ──────────────────────\n")
  cat("  brew install wkhtmltopdf\n")
  cat("Then re-source this script.\n\n")
  cat("── OR: manual Chrome print ────────────────────────────\n")
  cat("  1. open -a 'Google Chrome' '", out_html, "'\n", sep = "")
  cat("  2. Cmd+P → 'Save as PDF' → save to the Tables folder\n\n")
  cat("── OR: tell pagedown exactly where Chrome is ──────────\n")
  cat("  Run these two lines in R console:\n")
  cat("  Sys.setenv(PAGEDOWN_CHROME = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome')\n")
  cat("  pagedown::chrome_print('", out_html, "',\n", sep = "")
  cat("    output = '", out_pdf, "')\n", sep = "")
}

# ── 5. Summary ────────────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════\n")
cat("Output files (in Tables directory):\n")
cat("  HTML →", basename(out_html), "\n")
cat("  MD   →", basename(out_md),   "\n")
if (pdf_success) cat("  PDF  →", basename(out_pdf), "\n")
cat("══════════════════════════════════════════\n")