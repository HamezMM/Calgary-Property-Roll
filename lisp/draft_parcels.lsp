;;; DRAFTPARCELS
;;; Drafts closed LWPOLYLINEs for Calgary tax roll parcels from a folder of
;;; pre-cleaned CSVs produced by scripts/clean_and_split.py.
;;;
;;; Run once per COMM_NAME folder, e.g. data/clean/Residential/DEER RUN/.
;;; Each CSV inside that folder is one LAND_USE_DESIGNATION -> becomes one
;;; layer (created if it doesn't exist); every row in the file is one closed
;;; ring, drafted on that layer with its ADDRESS attached as xdata under the
;;; "PDG_PARCEL" application (group 1000, string).
;;;
;;; CSV format (header row skipped): ADDRESS,POINTS
;;;   POINTS is "x1 y1|x2 y2|x3 y3|..." in local meters, city-center origin.
;;;   ADDRESS is guaranteed comma-free by the cleaning script.
;;;
;;; Usage: (load "draft_parcels.lsp") then run DRAFTPARCELS, pick the folder.

(vl-load-com)

(defun str-split (str delim / result pos)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos 1 (strlen delim))))
  )
  (append result (list str))
)

(defun parse-point (ptstr / parts)
  (setq parts (str-split ptstr " "))
  (list (atof (car parts)) (atof (cadr parts)))
)

(defun parse-points (ptsstr)
  (mapcar 'parse-point (str-split ptsstr "|"))
)

;; Curated ACI color indices, chosen to stay visually distinct from each
;; other and from black/white (7) and gray (8/9) backgrounds; cycled as
;; each new land-use layer is created so every layer gets its own color.
(setq *parcel-layer-palette*
  '(1 2 3 4 5 6 30 50 70 90 110 130 150 170 190 210 230 250
    14 24 34 44 54 64 74 84 94 104)
)
(setq *parcel-layer-color-index* 0)

(defun next-layer-color ( / color)
  (setq color
    (nth (rem *parcel-layer-color-index* (length *parcel-layer-palette*)) *parcel-layer-palette*)
  )
  (setq *parcel-layer-color-index* (1+ *parcel-layer-color-index*))
  color
)

(defun ensure-layer (name)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        (cons 2 name)
        '(70 . 0)
        (cons 62 (next-layer-color))
        '(6 . "Continuous")
      )
    )
  )
)

(defun draft-parcel-ring (layer address points)
  (entmake
    (append
      (list
        (cons 0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 8 layer)
        (cons 100 "AcDbPolyline")
        (cons 90 (length points))
        (cons 70 1) ; closed
      )
      (mapcar (function (lambda (p) (cons 10 p))) points)
      (list (list -3 (list "PDG_PARCEL" (cons 1000 address))))
    )
  )
)

(defun process-csv-file (path layer / f line comma-pos address points-str n)
  (setq f (open path "r"))
  (setq n 0)
  (read-line f) ; header
  (while (setq line (read-line f))
    (setq comma-pos (vl-string-search "," line))
    (if comma-pos
      (progn
        (setq address (substr line 1 comma-pos))
        (setq points-str (substr line (+ comma-pos 2)))
        (draft-parcel-ring layer address (parse-points points-str))
        (setq n (1+ n))
      )
    )
  )
  (close f)
  n
)

(defun browse-folder ( / shellapp folderobj path)
  (setq shellapp (vlax-create-object "Shell.Application"))
  (setq folderobj
    (vlax-invoke-method shellapp 'BrowseForFolder 0
      "Select folder of split parcel CSVs (one file per land use):" 0
    )
  )
  (setq path
    (if folderobj
      (vlax-get-property (vlax-get-property folderobj 'Self) 'Path)
      nil
    )
  )
  (vlax-release-object shellapp)
  path
)

(defun c:DRAFTPARCELS ( / folder csvfiles layer total total-parcels)
  (regapp "PDG_PARCEL")
  (setq *parcel-layer-color-index* 0)
  (setq folder (browse-folder))
  (if (not folder)
    (princ "\nNo folder selected. Aborted.")
    (progn
      (setq csvfiles (vl-directory-files folder "*.csv" 1))
      (if (not csvfiles)
        (princ (strcat "\nNo CSV files found in " folder))
        (progn
          (setq total-parcels 0)
          (foreach fname csvfiles
            (setq layer (vl-filename-base fname))
            (ensure-layer layer)
            (setq total (process-csv-file (strcat folder "\\" fname) layer))
            (setq total-parcels (+ total-parcels total))
            (princ (strcat "\n" layer ": " (itoa total) " parcel(s) drafted."))
          )
          (princ (strcat "\nDone. " (itoa total-parcels) " parcel(s) across " (itoa (length csvfiles)) " layer(s)."))
        )
      )
    )
  )
  (princ)
)

(princ "\nDRAFTPARCELS loaded. Type DRAFTPARCELS to run.")
(princ)
