;******************************************************************************
;***                                                                        ***
      function assign_points, spinei, xpoints, ypoints
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    ASSIGN_POINTS
;
; PURPOSE:
;    Finds whether a point in spine is closest to either of the vertices or to
;    the spine.
;
; CALLING SEQUENCE:
;    asgn = assignPoints(spine, xpoints, ypoints)
;
; INPUTS:
;    spine = fltarr(2,n_vertices): array with the coordinates of the
;                vertices along the spine.
;    xpoints = fltarr(n_points):X coordinates of the points in the filament
;    ypoints = fltarr(n_points):Y coordinates of the points in the filament
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;	None
;
; OUTPUTS:
;    asgn = intarr(n_points): array with for each point in spine a number
;                 that tells wheter it is closest to either the end points or
;                 otherwise.
;                 0 = point is closest to the first end point.
;                 1 = point is closest to the last end point.
;                 2 = point is closest to neither
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
; PROCEDUREs USED:
;    segment_parms, distance, dist_to_line
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************

x = 0 & y = 1

dims = size(spinei)
nvert= dims(2)

if (nvert gt 4) then begin
   ;-- this is to restrict the anaendis to only
   ;-- the start and end of the spine 
    spine = fltarr(2,4)
    spine(*,0:1) = spinei(*,0:1)
    spine(*,2:3) = spinei(*,nvert-2:nvert-1)
    dims = size(spine)
    nvert= dims(2)
endif else spine=spinei

n    = n_elements(xpoints)
asgn = intarr(n)

;--- computes slope and intercept for each spine segment:
spine_p = dblarr(2, dims(2)-1)
for i=0, dims(2)-2 do begin
    spine_p(*,i) = segment_parms(spine(x,i), spine(y,i),$
                                 spine(x,i+1), spine(y,i+1) )
endfor

;--- keep track of how many points are in each group
;--- first are all the vertices, then all of the segments
dists = dblarr(nvert + nvert-1)

for i=0, n-1 do begin
    ;--- vertices:
    for v=0, nvert-1 do $
      dists[v] = distance(xpoints[i], ypoints[i], spine[x,v], spine[y,v])

    ;--- segments:
    for v = nvert, nvert+nvert-2 do begin
        vind = v-nvert

        ;--- Check for vertical segments. If so, distance is horizontal dist.
        if (spine[x,vind] eq spine[x,vind+1]) then begin
            if ( ((spine[y,vind] le ypoints[i]) and $
                  (ypoints[i] le spine[y,vind+1])) or $
                 ((spine[y,vind+1] le ypoints[i]) and $
                  (ypoints[i] le spine[y,vind])) ) then $
              dists[v]=distance(xpoints[i],ypoints[i],$
                                spine[x,vind],ypoints[i]) $
            else dists[v] = -1

        ;--- Check for horizontal segments. If so, distance is vertical dist.
        endif else if (spine[y,vind+1] eq spine[y,vind]) then begin
            ;--- making sure that intercept falls ON the segment:
            if ( ((spine[x,vind] le xpoints[i]) and $
                  (xpoints[i] le spine[x,vind+1])) or $
                 ((spine[x,vind+1] le xpoints[i]) and $
                  (xpoints[i] le spine[x,vind])) ) then $
              dists[v]=distance(xpoints[i],ypoints[i],xpoints[i],$
                                spine[y,vind]) $
            else dists[v] = -1

        ;--- Otherwise calculates the distance from a point to a line:
        endif else begin
            dists[v] = dist_to_line(xpoints[i], ypoints[i], $
                                  spine[x,vind], spine[y,vind], $
                                  spine[x,vind+1], spine[y,vind+1], $
                                  spine_p(0,vind), spine_p(1,vind))
        endelse
    endfor

    ;--- Find the best match, based on the distances in 'dists'
    valid = where(dists ge 0)
    tmp = min(dists[valid], minind)
    best = valid[minind]

    if (best eq nvert-1) then $
      best = 1 $
    else if (best gt 0) then $
      best = 2

    asgn[i] = best
endfor

return, asgn

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: assign_points.pro,v $
; Revision 3.1  2010/10/27 19:33:09  bernapn1
; Not sure but something minor I am sure
;
; Revision 3.0  2004/10/27 14:38:17  pietro
; Given revision 3.0
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.3  2004/06/11 20:10:20  pietro
; Added some documentation in header
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
;	Written by Pietro N. Bernasconi JHU/APL: February 12, 2004 from
;       original written by Kiri Wagstaff
