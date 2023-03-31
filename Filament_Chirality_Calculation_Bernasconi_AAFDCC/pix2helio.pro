;******************************************************************************
;***                                                                        ***
    function pix2helio, date, JULIAN_DATE = juldat, DATE_TIME = dt, $
                        sun_r, xy, XY_CENTER = xycent, $
                        SUN_RADIUS = semidiam, C_PATH=c_path
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    PIX2HELIO
;
; PURPOSE:
;    This function converts cartesian (x,y) coordinates into heliographic
;    (lat,long) coordinates
;
; CALLING SEQUENCE:
;    cartesian = pix2helio, date, sun_r, xy_coord, $
;                          [/JULIAN_DATE, /DATE_TIME, XY_CENTER=xycent, $
;                           C_PATH=c_path]
;
; INPUTS:
;    date = string,double,intarr(6): date for which do the conversion. The
;                 date can be expressed in either string format (default) as
;                 follows: "2002-05-18T15:38:27.000Z", or in julian date, or
;                 in a 6 element array as follows: [yyyy,MM,dd,hh,mm,ss]. To
;                 tell the routine if you are using julian date use the
;                 keyword /JULIAN_DATE. To tell it it is in the array format
;                 select the keyword /DATE_TIME.
;    sun_r = int or float: radius of Sun in pixels.
;    xy_coord = fltarr[2,n_points]: array with the coordinates of the points
;                 on the solar surface given in pixels
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    JULIAN_DATE = flag: if set then it tells the routine that the date
;                 parameter was given in julian date
;    DATE_TIME = flag: if set the it tells the routine that the date parameter
;                 is given as a 6 elements array [yyyy,MM,dd,hh,mm,ss].
;    XY_CENTER = fltarr(2): use this to enter the coordinates of the center
;                 if the cartesian coordiante system (if it is different
;                 from (0,0)
;    SUN_RADIUS= float: optional output of the Sun radius in arcsecs
;    C_PATH = string: Directory path where the c executables are located.
;                 Default is given by the call to the routine:
;                 filam_get_paths(/bin)
;
; OUTPUTS:
;    cartesian = fltarr(4, n_points): array with the coordinates converted in
;                 both pixels and arcseconds as follows (for each point):
;                 [X_coord_pix, Y_coord_pix, X_coord_arcs, Y_coord_arcs]
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    get_jdate, + the external C routine helio2pix needs to be in the c_path
;
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************

if not(keyword_set(juldat)) then begin
    if (keyword_set(dt)) then begin
        szd=size(date,/dim)
        if (szd(0) ne 6) then $
          message,'Incorrect input date. Must be a 6-elem array'
        date = make_datestring(date(0:2),sep='-')+'T'+$
          make_datestring(date(3:*),sep=':')+'.000Z'
    endif
    szd = size(date)
    if (szd(1) ne 7) then $
      message,'Incorrect input date. Must be in string format'
    juldat = get_jdate(date)
endif else begin
    szd = size(date)
    if ((szd(1) lt 2) and (szd(1) gt 5)) then $
      message,'Incorrect input date. Must be a int, float, or double'
    juldat = date
endelse

if (n_elements(xycent) le 0) then xycent = [0.,0.]

if (n_elements(c_path) le 0) then c_path = filam_get_paths(/bin)

tmpinf  = 'tmp_in.dat'
tmpoutf = 'tmp_out.dat'

;--- Writes a temporary file
openw, unit, tmpinf, /get_lun
printf, unit, form='(d15.6)', juldat
printf, unit, sun_r
printf, unit

xy=float(xy)
sz=size(xy)
if (sz(0) eq 1) then begin
    coords = fltarr(4,1)
    printf, unit, form='(F,F)',xy-xycent
endif else if (sz(0) eq 2) then begin
    coords = fltarr(4, sz(2))
    for i=0,sz(2)-1 do begin
        printf, unit, form='(F,F)',xy(*,i)-xycent
    endfor
endif else begin
    coords = fltarr(4, sz(2)*sz(3))
    for i=0,sz(2)-1 do begin
        for j=0,sz(3)-1 do begin
            printf, unit, form='(F,F)',xy(*,i,j)-xycent
        endfor
    endfor
endelse

close, unit

;--- Calls c program to calculate cartesian coords:
spawn,c_path+'pix2helio '+tmpinf+' '+tmpoutf+' -p'

;--- Reads the temporary outfile:
openr, unit, tmpoutf
readf, unit, semidiam
readf, unit, coords
close, unit
free_lun,unit

file_delete,[tmpinf,tmpoutf]

if (sz(0) le 2) then $
    out = reform(coords(0:1,*)) $
else begin
    out = fltarr(2,sz(2),sz(3))
    count = 0
    for i=0,sz(2)-1 do begin
        for j=0,sz(3) -1 do begin
            out(*,i,j) = coords(0:1,count)
            count = count + 1
        endfor
    endfor
endelse

return, out

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: pix2helio.pro,v $
; Revision 3.3  2010/04/29 20:55:46  bernapn1
; Added keyword output SUN_RADIUS
;
; Revision 3.2  2010/04/28 18:46:37  bernapn1
; Improved
;
; Revision 3.1  2010/04/20 17:16:07  bernapn1
; Created
;
; Written by Pietro N. Bernasconi JHU/APL
