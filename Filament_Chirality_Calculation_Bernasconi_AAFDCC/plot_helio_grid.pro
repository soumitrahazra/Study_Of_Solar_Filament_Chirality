;******************************************************************************
;***                                                                        ***
      pro plot_helio_grid, sunr_pix, B0_ANGLE=b0, P_ANGLE=pang, $
                           SUN_CENTER=sunc, GRID_SPACING=grid, $
                           NOLIMB = nolimb, NOERASE=noer, $
                           COLOR=color, LINESTYLE=line, THICK = thick
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    PLOT_HELIO_GRID
;
; PURPOSE:
;    Plots solar heliographic grid
;
; CALLING SEQUENCE:
;    plot_helio_grid, sunrad_pix [, B0_ANGLE=b0, P_ANGLE=pang, $
;                           SUN_CENTER=sunc, GRID_SPACING=grid, /NOERASE, $
;                           /NOLIMB, COLOR=color, LINESTYLE=line, $
;                           THICK = thick]
;
; INPUTS:
;    sunr_pix = int: radius of the sun on image in pixels.
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    B0_ANGLE   = float: Solar B0 angle in degrees. Default = 0.
;    P_ANGLE    = float: Solar P angle in degrees. Default = 0.
;    SUN_CENTER = float(2): [X,Y] position of sun center in image. Def = [0,0]
;    GRID_SPACING = float: spacing of the heliographic grid in deg. Def = 10.0
;    NOLIMB     = flag: if set then the limb contour is not plotted
;    NOERASE    = flag: if set then the grid is plotted above an image
;                       already existing in the dispaly window
;    COLOR      = int: color code for the grid. Default = 0
;    LINESTYLE  = int: line style for the grid. Default = 0
;    THICK      = float: Thickness of the lines. Default = 1.0
;
; OUTPUTS:
;    None
;
; ROUTINES CALLED:
;    hel2cart
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
;--- Handle the input:
if (n_params() le 0) then message,'Solar radius in pixels missing!'

if (n_elements(b0) le 0) then b0=0.
if (n_elements(pang) le 0) then pang=0.
if (n_elements(sunc) lt 2) then sunc=[0.,0.]
if (n_elements(grid) le 0) then grid=10.
if (n_elements(line) le 0) then line = 1

;--- Define the lat-long grid (in degrees) with the desired grid spacing:
llgr=-90.+grid*findgen(180./grid+1) & ng=n_elements(llgr)

;--- The following is the fine grid for plotting
finesp = 0.5
finegr=-100.+finesp*findgen(200./finesp+1) & nf=n_elements(finegr)

;--- Setup the display window with the appropriate grid scale:
old_ppos = !p.position
!p.position = [0,0,1,1]
plot,[0,1],/nodata,xs=5,ys=5,xr=[0,!d.x_size-1],yr=[0,!d.y_size-1],NOERASE=noer

;--- Plots the limb unles explicitly requested not to:
if (not keyword_set(nolimb)) then begin
    ang = findgen(361)/!radeg
    xlimb = sunr_pix * cos(ang) + sunc(0)
    ylimb = sunr_pix * sin(ang) + sunc(1)
    oplot, xlimb,ylimb, line = 0, color = color, THICK=thick
endif

;--- Plots the grid:
for ll = 0,1 do begin
    for grpt = 0, ng-1 do begin
        if (ll eq 0) then begin
            ;--- This is for parallels (latitudes) grid
            lat = replicate(llgr(grpt),nf)
            lng = finegr
        endif else begin
            ;--- This is for meridians (longitudes) grid
            lat = finegr
            lng = replicate(llgr(grpt),nf)
        endelse

        ;--- Getting the coordinates of the line to be plotted
        xycoor = hel2cart(lat, lng, isvis, B0=b0, P_ANG=pang, $
                          SUN_RADIUS = sunr_pix)
        w=where(isvis gt 0, nw)

        if (nw gt 0) then $
          oplot,xycoor(0,w(0):w(nw-1))+sunc(0),xycoor(1,w(0):w(nw-1))+sunc(1),$
          line=line, color=color, THICK=thick
    endfor
endfor

;--- Resetting the !p.position variable to the original before the call
!p.position = old_ppos

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: plot_helio_grid.pro,v $
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.4  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
;    Written by Pietro N. Bernasconi JHU/APL: 06/08/04-06/09/04 Inspired by
;        the Solar Soft routine plot_helio.
