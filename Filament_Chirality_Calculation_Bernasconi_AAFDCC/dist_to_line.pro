;******************************************************************************
;***                                                                        ***
      function dist_to_line, x1, y1, v1x, v1y, v2x, v2y, slope, intercept,$
                             TOLERANCE = tol, ENDSEGMENT=endseg, SHOW=show, $
                             M1_OUTSIDE = m1_out, PERP_INTERCEPT=perp_intrc
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    DIST_TO_LINE
;
; PURPOSE:
;    Calculate the distance from (x1,y1) to the line ((v1x,v1y)-(v2x,v2y)).
;    Optionally pass in slope and intercept to avoid recalculating them
;    all the time.
;
; CALLING SEQUENCE:
;    dist = dist_to_line(x1, y1, v1x, v1y, v2x, v2y [, slope, intercept, $
;                        TOLERANCE = tolerance, ENDSEGMENT=endseg, $
;                        M1_OUTSIDE, PERP_INTERCEPT=perp_intercept, /SHOW])
;
; INPUTS:
;    x1  = float: X coordinate of point
;    y1  = float: Y coordinate of point
;    v1x = float: X coordinate of first vertex of line
;    v1y = float: Y coordinate of first vertex of line
;    v2x = float: X coordinate of second vertex of line
;    v2y = float: Y coordinate of second vertex of line
;
; OPTIONAL INPUT PARAMETERS:
;    slope     = float: slope ( a ) of the segment according to equation
;                       y=a*x + b
;    intercept = float: y-intercept ( b ) of extension of segment:
;
; KEYWORD PARAMETERS:
;    TOLERANCE = intarr: pixels of tolerance around the line
;                        vertices. default = 0
;    ENDSEGMENT = int: This keyword is used only when determining the
;                 distance of a point to a spine segment: -1 if at start of
;                 a spine, 1 is at end, 0 elsewhere (the default).
;    M1_OUTSIDE = flag: if set then returns -1 if the point-line intercept lies
;                 outside the segment. Default is the negative of the
;                 distance.
;    PERP_INTERCEPT = fltarr(2) : [X,Y] coordinates of the intercept
;                 between the line and the perpendicular passing through
;                 the point.
;    SHOW = flag: If set then the line connecting the normal intercept with
;                 the segment and the point is plotted.
;
; OUTPUTS:
;    dist = double: distance between point and line.
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; PROCEDURES USED:
;    segment_parms, fsc_color
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_elements(tol) le 0) then tol=0
if (n_elements(endseg) le 0) then endseg=0
if (n_elements(m1_out) le 0) then m1_out = 0

npars=n_params()
if (npars lt 6) then message, 'Syntax: dist = dist_to_line(x1, y1, v1x, v1y, v2x, v2y [, slope, intercept, TOLERANCE=tol, ENDSEGMENT=endseg, /SHOW])'

if (npars eq 6) then begin
    slope = segment_parms(v1x, v1y, v2x, v2y)
    intercept = slope(1)
    slope = slope(0)
    npars = 8
endif
if (npars eq 7) then intercept = v1y - slope * v1x

;--- First, find the slope and y-intercept of a perpendicular line
;--- from the given line to (x1,y1)
if (slope eq 0) then begin
    ;--- Do this when line exactly horizontal:
    px = x1
    py = intercept
endif else if (v1x eq v2x) then begin
    ;--- Do this when line "exactly" vertical:
    px = v1x
    py = y1
endif else begin
    ;--- This is the most common and general case:
    perpslope = -1./double(slope)
    perpint = double(y1) - perpslope * x1

    ;--- Now, find the point where those lines intersect, (px,py)
    px = (perpint - intercept) / double(slope - perpslope)
    py = double(slope*px) + intercept
endelse
perp_intrc = [px, py]

;--- If (px,py) is on the segment, return the distance from (x1,y1) to (px,py)
;--- Otherwise, return -1.
d = distance(x1, y1, px, py)
if keyword_set(show) then begin
    ;--- Plots the line connecting (x1,y1) with (px,py) in pink
    line = intarr(2,2)
    line[0,0] = x1
    line[1,0] = y1
    line[0,1] = px
    line[1,1] = py
    plots, line*2, /Device, Color=FSC_Color('pink')
endif

if ((endseg eq 0) and $
    (((px lt v1x-tol) and (px lt v2x-tol)) or $
     ((px gt v1x+tol) and (px gt v2x+tol)) or $
     ((py lt v1y-tol) and (py lt v2y-tol)) or $
     ((py gt v1y+tol) and (py gt v2y+tol))))  then $
  if m1_out then return,-1 else return, -d
if ((endseg eq -1) and $
    (((v1x lt v2x) and (px gt v2x+tol)) or $
     ((v1x gt v2x) and (px lt v2x-tol)) or $
     ((v1y lt v2y) and (py gt v2y+tol)) or $
     ((v1y gt v2y) and (py lt v2y-tol)))) then $
  if m1_out then return,-1 else return, -d
if ((endseg eq 1) and $
    (((v1x lt v2x) and (px lt v1x-tol)) or $
     ((v1x gt v2x) and (px gt v1x+tol)) or $
     ((v1y lt v2y) and (py lt v1y-tol)) or $
     ((v1y gt v2y) and (py gt v1y+tol)))) then $
  if m1_out then return,-1 else return, -d

return, d

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: dist_to_line.pro,v $
; Revision 3.1  2010/10/27 19:35:34  bernapn1
; Something
;
; Revision 3.0  2004/10/27 14:38:17  pietro
; Given revision 3.0
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.3  2004/06/16 14:35:26  pietro
; Fixed a bug that if line is vertical or horizontal then the returned
;     perp_intercept parameter would be undefined.
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;    05/03/04 PNB: added keyword PERP_INTERCEPT
;    Written by Pietro N. Bernasconi JHU/APL: February 23, 2004
