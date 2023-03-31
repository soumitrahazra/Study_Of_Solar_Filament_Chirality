;******************************************************************************
;***                                                                        ***
        pro draw_image, fil_data, disp_img, sun_p, window, disp_magn, $
                        AS_IS = as_is, $
                        XOFFSET=xoff, YOFFSET=yoff, XSZ=xsz, YSZ=ysz, $
                        SHOW_BOUND = show_bound, SHOW_SPINE = show_spine, $
                        SHOW_ALL_BRBS=show_all_brbs, SHOW_BBOX=show_bbox
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    DRAW_IMAGE
;
; PURPOSE:
;    This procedure draw the solar image and plots the filaments
;    boundaries, spine, location, barbs and chirality:
;
; CALLING SEQUENCE:
;    draw_image, fil_data, disp_img, sun_p, window, disp_magn [, /AS_IS, $
;                        XOFFSET=xoff, YOFFSET=yoff, XSZ=xsz, YSZ=ysz, $
;                        /SHOW_BOUND, /SHOW_SPINE]
;
; INPUTS:
;    fil_data = structure_array(nFilaments): structure containing all the
;                    filaments data. See study_filaments for a detailed
;                    description of the structure content.
;    disp_img = array(X,Y): image for display
;    sun_p    = intarr(3): solar parameters for display:
;                    0) Sun radius; 1) X coord of center; 2) Y coord of center
;    disp_magn = float: magnification factor.
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    AS_IS   = flag: if set then it uses the input image "disp_img" as it is
;                    without cropping (it is useful if the input image is
;                    already a cropped version of the full disk image but you
;                    still want to display the coordinates grids correctly)
;    XOFFSET = int: X-coord of the lower left corner of box in image to be
;                    displayed. Default = 0
;    YOFFSET = int: Y-coord of the lower left corner of box in image to be
;                    displayed. Default = 0
;    XSZ = int: horizontal size of the box to be displayed. Def = X-XOFFSET-1
;    YSZ = int: vertical size of the box to be displayed. Def = Y-YOFFSET-1
;    SHOW_BBOX   = flag: if set then draws the filament bounding box in red
;    SHOW_BOUND  = flag: if set draw the boundary of each filament in yellow
;    SHOW_SPINE  = flag: if set draw the spine of each filament in cyan

;
; OUTPUTS:
;    None
;
; ROUTINES USED:
;    None
;
; COMMON BLOCKS:
;    colortab
;
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
szf = size(fil_data)
szd = size(disp_img)
z_buf_flag = 131072

if (n_elements(xoff) le 0) then xoff = 0
if (n_elements(yoff) le 0) then yoff = 0
if (n_elements(xsz) le 0) then xsz = szd(1)-xoff-1
if (n_elements(ysz) le 0) then ysz = szd(2)-yoff-1

xoff_h = xoff
yoff_h = yoff
if keyword_set(as_is) then begin
    xoff = 0
    yoff = 0
    xsz  = szd(1)-1
    ysz  = szd(2)-1
endif


;--- Checks is the common block is defined. If not then checks what color
;    resolution it is. If present then display is NOT a true color
common colortab, c_tab, grey_lim, c_red, c_green, c_blue
if (n_elements(c_tab) le 0) then begin
    if (!d.n_colors le 256) then begin
        true_color = 0
        c_tab = make_colortab(grey_limit=grey_lim, red=c_red, $
                              green=c_green, blue=c_blue)
    endif else true_color = 1
endif else true_color = 0

;--- Sets the right color code
if (true_color) then begin
    ;--- This are the setting for a monitor with True Color 
    red_c = fsc_color('red')
    green_c = fsc_color('green')
    yellow_c = fsc_color('yellow')
    cyan_c = fsc_color('cyan')
endif else begin
    ;--- This are the setting for a standard monitor without True Color 
    red_c = ctb_color('red',c_tab)
    green_c = ctb_color('green', c_tab)
    yellow_c = ctb_color('yellow', c_tab)
    cyan_c = ctb_color('cyan', c_tab)
endelse

if ((!d.flags and z_buf_flag) eq 0) then begin
    ;-- does this only if the device is NOT a z-buffer
    wset, window
    wshow, window
endif

tv,bytscl(congrid(disp_img(xoff:xoff+xsz-1,yoff:yoff+ysz-1), $
                  xsz*disp_magn, ysz*disp_magn, /int), $
          min=-1500, max=1500, top=grey_lim)

if (sun_p(0) gt 0) then begin
    plot_helio_grid, sun_p(0)*disp_magn, $
      sun_c=[sun_p(1)-xoff_h,sun_p(2)-yoff_h]*disp_magn, B0=sun_p(6), $
      /noer, color=green_c, line=2
endif

if (szf(0) le 0) then goto,at_end

for i=0, szf(1)-1 do begin
    ;--- Draw filament bounding-box in red
    if keyword_set(show_bbox) then begin
        minx = fil_data(i).bbox(0,0)
        miny = fil_data(i).bbox(1,0)
        maxx = fil_data(i).bbox(0,1)
        maxy = fil_data(i).bbox(1,1)
        plots, ([minx,maxx,maxx,minx,minx] - xoff)* disp_magn,$
          ([miny,miny,maxy,maxy,miny] - yoff)* disp_magn, /Device, Color=red_c
    endif

    ;--- Draw filament boundary in yellow
    if (keyword_set(show_bound)) then begin
        plots, (fil_data(i).bnd(0,0:fil_data(i).nbnd_p-1)-xoff) * disp_magn,$
          (fil_data(i).bnd(1,0:fil_data(i).nbnd_p-1) - yoff ) * disp_magn , $
          /Device, Color=yellow_c, psym=-3
        plots, ([fil_data(i).bnd(0,fil_data(i).nbnd_p-1),$
                 fil_data(i).bnd(0,0)] - xoff ) * disp_magn, $
               ([fil_data(i).bnd(1,fil_data(i).nbnd_p-1),$
                 fil_data(i).bnd(1,0)] - yoff ) * disp_magn , $
          /Device, Color=yellow_c, psym=-3        
    endif

    ;--- Draw filament spine in cyan
    if (keyword_set(show_spine)) then $
      plots, (fil_data(i).spine(0,0:fil_data(i).nverts-1)-xoff)*disp_magn,$
      (fil_data(i).spine(1,0:fil_data(i).nverts-1) - yoff)*disp_magn, $
      /Device, Color=cyan_c
    
    ;--- draw the single barbs:
    if (fil_data(i).nbarbs gt 0) then begin
        for b=0, fil_data(i).nbarbs-1 do begin
            plots, (fil_data(i).brb_coord(0,*,b) - xoff) * disp_magn, $
              (fil_data(i).brb_coord(1,*,b) - yoff) * disp_magn, $
              /Device, color=green_c

            case 1 of
                fil_data(i).barbdir(b) eq -1: d_str = ' R'
                fil_data(i).barbdir(b) eq  1: d_str = ' L'
                else : d_str = '?'
            endcase
            xyouts,(fil_data(i).brb_coord(0,0,b) - xoff) * disp_magn, $
              (fil_data(i).brb_coord(1,0,b) - yoff) * disp_magn, $
              d_str,/dev, color=cyan_c
        endfor
    endif

    ;--- marks the filament coordinates location with an asterisk
    plots,[sun_p(1) + fil_data(i).coords(0) - xoff_h, $
           sun_p(2) + fil_data(i).coords(1) - yoff_h] * disp_magn, psym=2, $
      /Device, Color=red_c

    ;--- Prints the filament number and the chirality statistics:
    case 1 of
        (fil_data(i).chir eq -1): chirstr = 'L'
        (fil_data(i).chir eq  1): chirstr = 'R'
        else: chirstr = '?'
    endcase
    fs='(I2)'
    xyostr = string(i+1,format='(I3,": ")') + $
      chirstr + ' ('+string(fil_data(i).nRight,format=fs) +'R,' + $
      string(fil_data(i).nLeft,format=fs) + 'L)'
    mincrds=[ min(fil_data(i).bnd(0,0:fil_data(i).nbnd_p-1)) - xoff , $
              min(fil_data(i).bnd(1,0:fil_data(i).nbnd_p-1)) - yoff ]
    xyouts, mincrds(0) * disp_magn + 5, mincrds(1)*disp_magn - 12, xyostr,$
      /device, color=1, charthick=2, charsize=1.3
endfor

at_end:
end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: draw_image.pro,v $
; Revision 3.6  2010/10/27 19:36:42  bernapn1
; Added the AS_IS keyword
;
; Revision 3.5  2010/03/29 21:11:18  bernapn1
; Added improvements:
;   1) Now the output file names consistently have YYYYMMDD instead of the
;      old YYYY_MM_DD
;   2) Now there is the possiblity to save in addition to the full disk image
;      also images of the individual filaments at 2x magnification
;
; Plus some bug fixes
;
; Revision 3.4  2010/03/26 17:35:10  bernapn1
; Now for boundary instead of only plotting dots it draws lines between the
; boundary points.  This is useful for when using magnification > 1 when
; displaying the image
;
; Revision 3.3  2010/01/21 21:49:27  bernapn1
; Added SHOW_BBOX keyword and added drawing of the filament bounding box
;
; Revision 3.2  2008/11/07 14:42:29  bernapn1
; Updated
;
; Revision 3.1  2005/01/06 22:37:57  pietro
; Now capable of handeling the z-buffer dispay device
;
; Revision 3.0  2004/10/27 14:38:17  pietro
; Given revision 3.0
;
; Revision 2.1  2004/07/13 15:33:16  pietro
; Created by Pietro Bernasconi JHU/APL
;
;
; Written by Pietro N. Bernasconi JHU/APL
