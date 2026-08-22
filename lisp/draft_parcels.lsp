;;; DRAFTPARCELS
;;; Drafts closed LWPOLYLINEs for Calgary tax roll parcels from a folder of
;;; pre-cleaned CSVs produced by scripts/clean_and_split.py.
;;;
;;; Run once per top-level LAND_USE_DESIGNATION folder, e.g.
;;; Tax_CSVs_Clean/C-C1/. Underneath that, CSVs sit two folders deep:
;;;   <land use>/<Residential|Non-Residential|Farm Land>/<COMM_NAME>/*.csv
;;; Every CSV found anywhere under the selected folder is grouped by its
;;; immediate parent folder (COMM_NAME) -> one layer per neighbourhood
;;; (created if it doesn't exist), combining Residential and
;;; Non-Residential CSVs for the same community onto the same layer. Every
;;; row in every CSV is one closed ring, drafted on that layer with its
;;; ADDRESS attached as xdata under the "PDG_PARCEL" application (group
;;; 1000, string).
;;;
;;; CSV format (header row skipped): ADDRESS,POINTS
;;;   POINTS is "x1 y1|x2 y2|x3 y3|..." in local meters, city-center origin.
;;;   ADDRESS is guaranteed comma-free by the cleaning script.
;;;
;;; Usage: (load "draft_parcels.lsp") then run DRAFTPARCELS, pick the
;;; top-level land-use folder.

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
;; each new neighbourhood layer is created so every layer gets its own color.
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
      "Select top-level land-use folder (CSVs are grouped by neighbourhood):" 0
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

(defun path-leaf (path / i)
  (setq i (strlen path))
  (while (and (> i 0) (/= (substr path i 1) "\\"))
    (setq i (1- i))
  )
  (substr path (1+ i))
)

;; Recurses through folder, collecting every *.csv found anywhere below it
;; into groups keyed by each file's immediate parent folder name
;; (the neighbourhood/COMM_NAME folder). groups is an alist of
;; (neighbourhood-name . (full-csv-path ...)), accumulated and returned.
(defun collect-csv-groups (folder groups / csvfiles subdirs entry existing)
  (setq csvfiles (vl-directory-files folder "*.csv" 1))
  (if csvfiles
    (progn
      (setq entry (cons (path-leaf folder) (mapcar (function (lambda (f) (strcat folder "\\" f))) csvfiles)))
      (setq existing (assoc (car entry) groups))
      (setq groups
        (if existing
          (subst (cons (car existing) (append (cdr existing) (cdr entry))) existing groups)
          (append groups (list entry))
        )
      )
    )
  )
  (setq subdirs (vl-directory-files folder nil -1))
  (foreach d subdirs
    (if (and (/= d ".") (/= d ".."))
      (setq groups (collect-csv-groups (strcat folder "\\" d) groups))
    )
  )
  groups
)

(defun c:DRAFTPARCELS ( / folder groups grp name total total-parcels path)
  (regapp "PDG_PARCEL")
  (setq *parcel-layer-color-index* 0)
  (setq folder (browse-folder))
  (if (not folder)
    (princ "\nNo folder selected. Aborted.")
    (progn
      (setq groups (collect-csv-groups folder '()))
      (if (not groups)
        (princ (strcat "\nNo CSV files found under " folder))
        (progn
          (setq total-parcels 0)
          (foreach grp groups
            (setq name (car grp))
            (ensure-layer name)
            (setq total 0)
            (foreach path (cdr grp)
              (setq total (+ total (process-csv-file path name)))
            )
            (setq total-parcels (+ total-parcels total))
            (princ (strcat "\n" name ": " (itoa total) " parcel(s) drafted."))
          )
          (princ (strcat "\nDone. " (itoa total-parcels) " parcel(s) across " (itoa (length groups)) " neighbourhood layer(s)."))
        )
      )
    )
  )
  (princ)
)

(princ "\nDRAFTPARCELS loaded. Type DRAFTPARCELS to run.")
(princ)
