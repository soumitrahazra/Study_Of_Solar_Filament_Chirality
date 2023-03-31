;******************************************************************************
;***                                                                        ***
      function study_filaments, data, i_thresh, area_min, factor, $
                                CLUSTERS = clusters, $
                                U_THRESHOLD = u_thresh, $
                                DISK_CENTER = disk_center, $
                                SHOW_PIXELS=show_pixels, $
                                SHOW_BBOX=show_bbox, $
                                SHOW_BOUND=show_bound, $
                                SHOW_SPINE=show_spine, $
                                REQ_SLOPE =req_slope, $
                                SHOW_ALL_BRBS=show_all_brbs, $
                                MAX_DIST=max_dist, $
                                MAX_ANG=max_ang, $
                                VERBOSE=verbose
;***                                                                        ***
;******************************************************************************
;+
; PROJECT: Automated Solar Filament Detection and Characterization
;
; NAME:
;    STUDY_FILAMENTS
;
; PURPOSE:
;    Identify filaments as clusters of at least a minimum size and display
;    them. Also, find the spines, barbs, and chirality.
;    Write results to output file (catalog)
;
; CALLING SEQUENCE:
;    filam_data = study_filaments(data, i_thresh, area_min, factor [, $
;                      U_THRESHOLD = u_threshold, DISK_CENTER =
;                      disk_center, $ /SHOW_PIXELS, $
;                      /SHOW_BOUND, /SHOW_SPINE, /SHOW_BRB_BEAR, /VERBOSE])
;
; INPUTS:
;    data = array(X,Y): h-alpha image preprocessed. -1 if error occurred
;    i_thresh = float: threshold value below which filaments are
;                 identified.
;    area_min = int: minimum area that defines the smalles filament.
;    factor = display factor that says what kind of magnification or
;             reduction must be applyed to the data for the display.
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;    U_THRESHOLD = float: threshold value to which the filament is expanded.
;    DISK_CENTER = intarr(2): X & Y coordinates of sun center with respect
;                             to (0,0) coords of image
;    SHOW_PIXELS = flag: if set ighlight all pixels below the threshold in red
;    SHOW_BBOX   = flag: if set then draws the filament bounding box in red
;    SHOW_BOUND  = flag: if set draw the boundary of each filament in yellow
;    SHOW_SPINE  = flag: if set draw the spine of each filament in cyan
;    REQ_SLOPE   = float: minimum slope in degrees of barb spine for deciding
;                     the left/right. Default = 0.6
;    VERBOSE     = flag: if set then print some infos in the sdout
;
; OUTPUTS:
;    filam_data   = struct_array(nFilaments) with all the filaments parameters:
;          area   = intarr: area of the filament in pixels
;          coords = intarr(2): X & Y coords of filament location (in pixels)
;          bbox   = intarr(2,2): (X,Y) coordinates of lower left (*,0)
;                       and upper right (*,1) edges of filament bounding box
;          bnd    = uintarr(2,nbnd_p): array with (X,Y) coordinates of
;                       filament boundary. Max is 3000 points
;          nbnd_p = int: number of poins in the bnd array
;          spine  = intarr(2,nverts): array with the (X,Y) coordinates of
;                       the spine vertices. Max is 100 vertices
;          angle  = float: filament orientation angle in degrees
;          brb_coord = intarr(2,2,nbarbs): array with the (X,Y) coordinates
;                       of line defining a barb. Max # of barbs is 30
;          nbarbs = int: number of barbs in the filament
;          nRight = int: number of barbs bearing right
;          mLeft  = int: number of barbs bearing left
;          chir   = int: filament handyness: -1=right, 1=left, 0=unspecifyed
;
;     endpts = intarr(sz,10,4): the endpoints of all other filaments that are
;                      near the start of the filament, up to ten
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; COMON BLOCKS:
;    colortab: c_tab, grey_lim (if undefined => a true color device is assumed)
;
; PROCEDURES USED:
;    get_filaments, find_boundary, find_spine, filament_angle, find_barbs,
;    fsc_color, spine_center
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 4) then message,'Incorrect number of parameters!'

sz=size(data,/dim)
if (n_elements(sz) ne 2) then message,'input data must be a 2D array!'
width=sz(0)
height=sz(1)

if (n_elements(disk_center) eq 1) then $
  message,'disk_center parameter MUST be a 2-d array!' $
else if (n_elements(disk_center) le 0) then disk_center=[0,0]

if (n_elements(verbose) le 0) then verbose = 0

if (n_elements(u_thresh) le 0) then u_thresh=i_thresh

;---- Setting the maximum area allowed:
area_max = 25000

;--- Checks if the common block is defined. If not then checks what color
;    resolution it is. If present then display is NOT a true color
common colortab, c_tab, grey_lim, c_red, c_green, c_blue
if (n_elements(c_tab) le 0) then begin
    if (!d.n_colors le 256) then begin
        true_col = 0
        c_tab = make_colortab(grey_limit=grey_lim)
    endif else true_col = 1
endif else true_col = 0

;--- Sets the right color code
if (true_col) then begin
    ;--- This are the setting for a monitor with True Color 
    red_c = fsc_color('red')
    yellow_c = fsc_color('yellow')
    cyan_c = fsc_color('cyan')
endif else begin
    ;--- This are the setting for a standard monitor without True Color 
    red_c = ctb_color('red', c_tab)
    yellow_c = ctb_color('yellow', c_tab)
    cyan_c = ctb_color('cyan', c_tab)
endelse

;--- Identify potential filament pixels based on the threshold
mask = get_filaments(data, i_thresh, U_THRESH=u_thresh, /MORPH_FILTER, $
                     box = 11, VERBOSE=verbose)

;indices = where(mask eq 1, n)
indices = where(mask ge 1, n)
if verbose then print,$
  format='("Found ",I6," pixels at or below the intensity threshold.")',n

;--- exits if no filaments possible
if (n lt area_min) then begin
    if verbose then print,'No filaments analyzed!'
    return, -1
endif

if verbose then begin
    print,format='("     #  Area   A_rat   Coordinates'+$
      '   Angle",T45,"#brb #R",2X,"#L  Chir")'
    print,'    -----------------------------------------------------------'
endif

;--- Defines an array where to store all the filament data:
max_nfil   = 30    ;<- Initial guess of max # of filaments. If exeeded then
                   ;   increment it by steps of 10
fil={area:0, area_rat:0., coords: intarr(2), bbox:intarr(2,2), $
     bnd:intarr(2,3000), nbnd_p:0, spine:intarr(2,200), angle:0., length:0, $
     nverts:0, brb_coord:intarr(2,2,50), barbdir:intarr(50), $
     barblen:fltarr(50), nbarbs:0, nRight:0, nLeft:0, chir:0}
filam_data = replicate(fil, max_nfil)

;--- Group potential filaments into coherent clusters
clusters = label_region(mask)
h = histogram(clusters)

;--- Process each potential filament ------------------------------------------
; (index 0 is "not in a cluster", so start at i=1)
nFilaments = 0
for i=1, n_elements(h)-1 do begin
    filament  = where(clusters eq i)

    if (h[i] gt 50) then begin
        if verbose then print, format='(I6,I6,$)', nFilaments+1, h(i)

        if (h[i] gt area_max) then begin
            print,format='("   Is bigger than maximum area allowed or ",I5)',$
              area_max
            clusters(filament) = 0 ;<-- eliminates it from the clusters map
            continue            ;<-- goes back to top and checks next filament
        endif

        filamentx = (filament mod width)
        filamenty = (filament/width)

        ;--- Find the boundaries of the filament:
        bnd = find_boundary(filament, Xsize=width, Ysize=height)
        szbnd=size(bnd)

        ;--- Establish filament coordinates (the center of box 
        ;    enclosing it):
        mincrds=[min(bnd(0,*),max=maxx), min(bnd(1,*), max=maxy)]
        maxcrds=[maxx, maxy]
        xc = (mincrds(0)+maxcrds(0))/2.
        yc = (mincrds(1)+maxcrds(1))/2.
        mincrds = mincrds - 10
        maxcrds = maxcrds + 10
 
       ;--- Finds the distance from the center and the pixel along the
        ;    boundary farthest away. If the area ratio is too large then
        ;    the "filament" is a residual sunspot and is not analyzed:
        mx_d = max(distance(xc,yc,bnd(0,*),bnd(1,*)))
        area_rat = n_elements(filament)/mx_d^2/!pi
        if (area_rat gt 0.55) then begin
            ;--- This is identified as a residual spot:
            if verbose then $
              print,format='("   Is a sunspot!  Area ratio = ",f4.2)', area_rat
            clusters(filament) = 0 ;<-- eliminates it from the clusters map
            continue            ;<-- goes back to top and checks next filament
        endif

        ;--- Find the spine of the filament:
        weight = mask(filament)
        spine = find_spine(filamentx, filamenty, factor, weight=weight, $
                           mind=25, width=2, $
                           apex_move=36, bnd=bnd, NEW_VERT_CRIT = 0)
        szspine=size(spine)
        
        f_angle = filament_angle(spine, /DEGREES)

        ;--- Display options ----------------------------------------
        ;--- Highlight detected points in red
        if keyword_set(show_pixels) then begin
            sr = indgen(2,h[i])
            sr[0,*] = filamentx/factor
            sr[1,*] = filamenty/factor
            plots, sr, /Device, Color=red_c, psym=3
        endif

	;--- Draw filament bounding-box in red
        if keyword_set(show_bbox) then begin
            minx = mincrds(0)
            miny = mincrds(1)
            maxx = maxcrds(0)
            maxy = maxcrds(1)
            plots, [minx,maxx,maxx,minx,minx]/factor,$
              [miny,miny,maxy,maxy,miny]/factor, /Device, Color=red_c
        endif

	;--- Draw filament boundary in yellow
        if keyword_set(show_bound) then $
          plots, bnd/factor, /Device, Color=yellow_c, psym=3

        ;--- Draw filament spine in cyan
        if keyword_set(show_spine) then $
          plots, spine/factor, /Device, Color=cyan_c; ,psym=-1
        ;------------------------------------------------------------

        ;--- Identifyes barbs and determine their handyness:
        barbinfo = find_barbs(bnd, spine, factor, req_slope=req_slope, $
                              SHOW_ALL_BRBS=show_all_brbs)

        ;--- Refines the filament coordinates by finding the coord of the
        ;    mid point along the spine:
        spc = spine_center(spine, length = splen)
        if keyword_set(show_bound) then $
          plots, spc/factor, psym=2, /Device, Color=red_c
          ;plots, [xc,yc]/factor, psym=2, /Device, Color=red_c
        ; Convert them to be relative to the origin, at Sun center:
        spc = spc - disk_center

        ;--- Optional printout:
        case 1 of
            (barbinfo.chir eq -1): chirstr='L'
            (barbinfo.chir eq  1): chirstr='R'
            else: chirstr='?'
        endcase
        if verbose then begin
            print, format='(F7.2,"   (",I5,",",I5,")", F7.1,$)', $
              area_rat, xc, yc, f_angle
            print, format='(I5,I4,I4,"   ",A,F9.1)',$
              barbinfo.nbarbs,barbinfo.nRight,barbinfo.nLeft,chirstr,$
              total(barbinfo.barbdir*barbinfo.barblen)
        endif

        ;--- Printout on the image itself
        fs='(I2)'
        xyostr = string(nFilaments+1,format='(I3,": ")') + $
          chirstr + ' ('+string(barbinfo.nRight,format=fs) +'R,' + $
          string(barbinfo.nLeft,format=fs) + 'L)'
        xyouts, mincrds[0]/factor+5, mincrds[1]/factor-12, xyostr, $
          /device, color=1, charthick=2, charsize=1.3
       
        ;--- Filling the fil data structure:
        struct_assign,barbinfo,fil
        fil.area = h(i)
        fil.area_rat = area_rat
        fil.coords = spc
        fil.length = round(splen)
        fil.bbox(*,0) = mincrds
        fil.bbox(*,1) = maxcrds
        fil.bnd(*,0:szbnd(2)-1) = bnd
        fil.nbnd_p = szbnd(2)
        fil.spine(*,0:szspine(2)-1) = spine
        fil.angle = f_angle
        fil.nverts = szspine(2)

        ;--- Increases filam_data size if max num of filaments is exceeded
        if (nFilaments eq max_nfil) then begin
            max_nfil = max_nfil + 10
            tmp = filam_data
            filam_data = replicate(fil, max_nfil)
            filam_data(0:nFilaments-1) = tmp
        endif
        filam_data(nFilaments) = fil
        nFilaments = nFilaments + 1

    endif else begin
        clusters(filament) = 0  ;<-- eliminates it from the clusters map
    endelse
endfor

if verbose then $
  print, format='("    Total: ",I3," filaments detected.")', nFilaments

nFilam_orig = nFilaments

if (nFilaments le 0) then return,-1

filam_data = filam_data(0:nFilaments-1)

;---- Relabel the regions:
clusters = label_region(clusters)

;---- Connects filaments based on slopes at nearby endpoints
merge_filams,filam_data, disk_center, clusters, MAX_ANG=max_ang, $
  MAX_DIST=max_dist, verbose=verbose
nFilaments = n_elements(filam_data)

;---- Removing the filaments that are too small and too "round":
tmp = filam_data
tmp_sz = nFilaments
tmp_i  = 0
if verbose then begin
    print
    print,'Removing filaments that are too small or too "round" ...'
    print,format='("    Limits: area > ",I3,"  area ratio < 0.25")', area_min
endif
for i=0,nFilaments-1 do begin
    if ((filam_data(i).area lt area_min) and $
        (filam_data(i).area_rat gt 0.25) )then begin
        if verbose then print,format='(A,I3,A,I3,A,F4.2,A)',$
          '    Filament ',i+1,' has area ', filam_data(i).area,$
          ' and area ratio ', filam_data(i).area_rat,' ==> Removing'

        w = where(clusters eq tmp_i+1)
        clusters(w) = 0

        tmp_sz = tmp_sz-1
        if (tmp_i eq tmp_sz) then begin
            tmp = tmp(0:tmp_i-1)
        endif else begin
            tmp(tmp_i:tmp_sz-1) = filam_data(i+1:*)
            w = where(clusters gt tmp_i+1)
            clusters(w) = clusters(w)-1
      endelse
    endif else tmp_i = tmp_i +1
endfor
nFilaments = tmp_sz

filam_data = tmp(0:nFilaments-1)

if (verbose and (nFilaments lt nFilam_orig)) then begin
    print, format='("    Final total: ",I3," filaments accepted.")',nFilaments
    print
    print, 'Summary:'
    print,format='("     #  Area   A_rat   Coordinates'+$
      '   Angle",T45,"#brb #R",2X,"#L  Chir")'
    print,'    -----------------------------------------------------------'
    for i=0,nFilaments-1 do begin
        fp = filam_data(i)
        case 1 of
            (fp.chir eq -1): chirstr='L'
            (fp.chir eq  1): chirstr='R'
            else: chirstr='?'
        endcase
         print, format='(I6,I6,F7.2,"   (",I5,",",I5,")", F7.1,$)', $
           i+1, fp.area, fp.area_rat, fp.coords(0), fp.coords(1), fp.angle
         print, format='(I5,I4,I4,"   ",A)',$
           fp.nbarbs, fp.nRight, fp.nLeft, chirstr
    endfor
endif

if (nFilaments le 0) then return,-1

return, filam_data
end
;******************************************************************************
; MODIFICATION HISTORY:
; $Log: study_filaments.pro,v $
; Revision 3.10  2010/10/27 19:34:14  bernapn1
; Something
;
; Revision 3.9  2010/04/29 21:03:36  bernapn1
; Better handling of the merges especially when updating the clusters map.
; Improved vervbose output.
;
; Revision 3.8  2010/04/19 15:12:58  bernapn1
; Fixed a bug on how to count the remainig filaments after checking for their
; size.
;
; Revision 3.7  2010/04/13 21:03:30  bernapn1
; Improved merging of filaments (written new routine).
; Improved way to remove small detected filaments.  Now before removing one it
; checks its area ratio. If it is a skinny one then it keeps it.
;
; Revision 3.6  2010/03/10 20:17:46  bernapn1
; Added a 10 pixel extra padding around the original bounding box
;
; Revision 3.5  2010/03/10 20:02:36  bernapn1
; Now using the new routine spine_center to determine the coordinates of the
; mid point of the spine as well as the lenght of the spine
;
; Revision 3.4  2010/01/21 21:57:44  bernapn1
; Included in output strucutre the parameter bbox as per required output of the
; SDO FFT project
;
; Revision 3.3  2009/12/18 21:52:24  bernapn1
; Improved fainter filaments detection
; Improved connection of nearby filaments
; Added MAX_DIST paramenter in input file (adds more flexibility)
;
; Revision 3.2  2004/11/29 15:31:17  pietro
; Added a variable that limits the maximum area of a filament. This prevents
; the crashing of the code when a too big a filament is detected
;
; Revision 3.1  2004/11/22 21:41:23  pietro
; Removed the call to verbose in "get_nearby_endpts"
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.10  2004/10/25 18:53:11  pietro
; Fixed a wrap around in a format statement
;
; Revision 2.9  2004/07/21 17:55:54  pietro
; same as last change but for barbdir
;
; Revision 2.8  2004/07/21 17:53:18  pietro
; for fil structure now brb_coord is intarr(2,2,50) instead of intarr(2,2,30)
;
; Revision 2.7  2004/07/16 19:29:36  pietro
; In structure fil moved entry "length" form end of structure list to after
; "angle" entry
;
; Revision 2.6  2004/07/15 20:04:13  pietro
; Now it calculates the length of a filament
;
; Revision 2.4  2004/06/29 19:26:03  pietro
; Added keyword REQ_SLOPE
;
; Revision 2.3  2004/06/24 16:49:37  pietro
; Added an algorythm in the main loop that makes a final check to see if a
; 	filament in reality is a spot: It checks the ratio between area of
; 	a circle surrounding the entire "filament" and the total number of
; 	pixels in the filament. If that ratio is above 0.55 then it must
; 	be a residual sunspot
;
; Revision 2.2  2004/06/21 20:43:18  pietro
; *** empty log message ***
;
; Revision 2.1  2004/06/21 20:12:59  pietro
; Tested version with True color control
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.3  2004/06/16 18:20:08  pietro
; Added keyword SHOW_ALL_BRBS
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; 04/07/04 PNB: Added keyword U_THRESHOLD and software to handle it.
; Written by Pietro N. Bernasconi JHU/APL:
