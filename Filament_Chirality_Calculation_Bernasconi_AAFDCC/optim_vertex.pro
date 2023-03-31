;******************************************************************************
;***                                                                        ***
      function optim_vertex, spine, xpoints, ypoints, width = width,$
                             factor=factor, no_apex_optim = no_apex_optim,$
                             weight=weighti

;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    OPTIMIZE_VERTEX
;
; PURPOSE:
;    Optimize the position of the vertices fo the spine
;
; CALLING SEQUENCE:
;    result = optim_vertex(spine, xpoints, ypoints, asgn, weight=weight, $
;                          width = width,$
;                          factor=factor, no_apex_optim = no_apex_optim)
;
; INPUTS:
;    spine = fltarr(2,n_vertices): array with the coordinates of the
;                vertices along the spine.
;    xpoints = fltarr(n_pix):X coordinates of the points in the filament
;    ypoints = fltarr(n_pix):Y coordinates of the points in the filament
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    weight  = fltarr(n_pix): weight of each filament pixel. This is used to
;                 give for example more weight to the filament pixels in the
;                 core of the filament.
;    width = int: pixel width of the band perpendicular to a specific
;                  segment of the spine that is considered in order to
;                  optimize the position of next vertex.
;    factor = float: magnification factor for display purposes. Debugging tool
;    no_apex_optim = flag: if set then the positions of the two apices of
;                  spine are NOT optimized.
;
; OUTPUTS:
;    result = fltarr(2,n_vertices): New spine points with position optimized.
;
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
pltdb = 0   ;<-- plots some things for debugging purposes

x = 0 & y = 1

if (n_elements(width) le 0) then width = 5

numpts = n_elements(xpoints)

;--- Creates a mask with 1 where the filament exist
mxx = max(xpoints,min=mnx)
mxy = max(ypoints,min=mny)
im  = intarr(mxx-mnx+1,mxy-mny+1)
szim = size(im,/dim)
for i=0,numpts-1 do im(xpoints(i)-mnx,ypoints(i)-mny) = 1

if (n_elements(weighti) lt numpts) then begin
    weight = replicate(1,szim(x),szim(y))
endif else begin
    weight = intarr(szim(x),szim(y))
    for i=0,numpts-1 do weight(xpoints(i)-mnx,ypoints(i)-mny) = weighti(i)
endelse

dimsp = size(spine,/dim)
nvert = dimsp(1)

;--- move vertices if requested:
if not(keyword_set(no_apex_optim)) then begin
    ;--- Finds which poiend in spine are closest to end points
    asgn = assign_points(spine, xpoints, ypoints)

    ;--- do simple average for first and last vertex:
    for v=0,nvert-1,nvert-1 do begin
        if v eq 0 then $
          pts = where(asgn eq 0, numpts) $
        else $
          pts = where(asgn eq 1, numpts)
        if (numpts ne 0) then begin
            sumx = total(xpoints[pts])
            sumy = total(ypoints[pts])
            spine[x,v] = round(sumx/float(numpts))
            spine[y,v] = round(sumy/float(numpts))
        endif
    endfor
endif

for v=1,nvert-2 do begin
    ;--- Computes the average spine slope at the specified vertex
    alpha = 0.
    for i=0,1 do begin
        numer = double(spine[y,v+i]-spine[y,v+i-1])
        denom = double(spine[x,v+i]-spine[x,v+i-1])
        if (denom eq 0) then begin
            alpha1 = !pi/2.*sign_pb(numer)
        endif else begin
            alpha1 = atan(numer/denom)
            if (denom lt 0) then alpha1 = sign_pb(numer)*!pi + alpha1
        endelse
        ;print,v,numer,denom,alpha1
        alpha = alpha + alpha1
    endfor

    ;--- coputes the perpendicular to the average spine slope:
    alpha = (alpha+!pi)/2.      ;<- alpha/2. + !pi/2.
    ;print,'average perp alpha = ',v,alpha
    if (abs(alpha) eq !pi/2.) then slope = double(1./0.000000001) $
    else slope = tan(alpha)
    ;print
    ;intercept = spine[y,v] - slope * spine[x,v]

    ;--- creates a mask where the points are along the bisector:
    msk=intarr(szim(x),szim(y))
    p0 = spine(*,v) - [mnx, mny]
    if abs(slope) gt 1 then begin
        ;--- for looking above and below
        plt_color = 'blue'

        p0 = p0 - [width,0] 
        b0  = p0(y) - slope*p0(x)
        for yy = 0,szim(y)-1 do begin
            xx = round((yy - b0)/slope)+indgen(width*2+1)
            ww = where( ( (xx ge 0) and (xx lt szim(x)) ), nww)
            if (nww gt 0) then msk(xx(ww),yy) = 1
        endfor        
    endif else begin
        ;--- for looking left and right:
        plt_color = 'yellow'

        p0 = p0 - [0,width]
        b0  = p0(y) - slope*p0(x)
        for xx = 0,szim(x)-1 do begin
            yy = round(xx * slope + b0)+indgen(width*2+1)
            ww = where( ( (yy ge 0) and (yy lt szim(y)) ), nww)
            if (nww gt 0) then msk(xx,yy(ww)) = 1
        endfor
    endelse

    ww = where( (im+msk) eq 2, nww)
    if (nww gt 0) then begin

        if (pltdb) then begin
            xx = (ww mod szim(x)) + mnx
            yy = (ww /   szim(x)) + mny
            plots,xx/factor,yy/factor,$
              /dev,psym=3,color=fsc_color(plt_color)
        endif

        tmp = intarr(szim(x),szim(y))
        tmp(ww) = 1
        ll = label_region(tmp)
        n_c = max(ll)
        if (n_c le 1) then begin
            spine(*,v)=round([total( (ww mod szim(x)) * weight(ww) ), $
                              total( (ww  /  szim(x)) * weight(ww) )] / $
                             total(weight(ww)) ) + [mnx,mny]
        endif else begin
            c_coord = fltarr(2,n_c)
            tw = fltarr(n_c)
            dd = fltarr(n_c)
            ang= fltarr(n_c)
            for i=0,n_c-1 do begin
                ww = where(ll eq i+1)
                tw(i) = total(weight(ww))
                c_coord(*,i)= round([total( (ww mod szim(x)) * weight(ww) ), $
                                     total( (ww  /  szim(x)) * weight(ww) )]/ $
                                    tw(i) ) + [mnx,mny]
                dd(i) = distance(spine(x,v),   spine(y,v), $
                                 c_coord(x,i), c_coord(y,i) )
                if (dd(i) eq 0.) then dd(i) = 0.5

                if (nvert gt 3) then begin
                    ;-- getting the angle of deviation from the spine:
                    ang(i) = 0.
                    ang_count = 0.
                    for j=-2,2,4 do begin
                        ; do it for segment before and after current vertex
                        ss = v + j
                        ee = ss - (j > (-1) < 1) ; sign for 
                        if (ss gt 0) and (ss lt nvert-2) then begin
                            ss = reform(spine(*,ss))
                            ee = reform(spine(*,ee))
                            pp = reform(c_coord(*,i))
                            az_sp = segment_azimuth(ss(0),ss(1),ee(0),ee(1))

                            az_pp = segment_azimuth(ee(0),ee(1),pp(0),pp(1))
                            az_pp = (az_pp - az_sp) mod (!pi*2)

                            if (abs(az_pp) gt !pi) then $
                              az_pp = az_pp -!pi*2*sign_pb(az_pp)

                            ang(i) = ang(i) + !PI - abs(az_pp)
                            ang_count = ang_count+1.
                        endif
                    endfor
                    if (ang_count gt 0) then ang(i) = ang(i)/ang_count
                endif
                
            endfor

            twr = tw/total(tw)
            mxw = max(twr, take_this)

            if 0 then begin
                print
                print,'   twr',twr
                print,'    dd',dd
                print,'twr/dd',twr/dd
            endif
            if (mxw lt 0.9) then begin
                if (nvert gt 3) then begin
                    ; takes the option wiht the largest angle:
                    ;print,'   ang',ang*180./!PI
                    mxw = max(ang, take_this)
                endif else begin
                    ; takes the option with the largest weight weighted by the
                    ; distance:
                    if (mxw lt 0.7) then begin
                        twr = twr/dd
                        mxw = max(twr, take_this)                
                    endif
                endelse
            endif
            spine(*,v) = c_coord(*,take_this)

        endelse
        if (pltdb) then begin
            plots,spine(x,v)/factor,spine(y,v)/factor,/dev,psym=-2,$
              color=fsc_color('magenta')
        endif
    endif
endfor

return, spine

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: optim_vertex.pro,v $
; Revision 3.3  2010/10/27 19:30:41  bernapn1
; Radically changed the way the new vertex is determined. Now is a faster
; routime that does not call every time the assign_points subroutine.
; New algorithm that handles much better filaments that have large sub-branches.
; Now it does no longer allow a new vertex to actually be located in between the
; branches of a filament.
;
; Revision 3.2  2010/04/13 21:03:30  bernapn1
; Improved merging of filaments (written new routine).
; Improved way to remove small detected filaments.  Now before removing one it
; checks its area ratio. If it is a skinny one then it keeps it.
;
; Revision 3.1  2009/12/18 21:52:24  bernapn1
; Improved fainter filaments detection
; Improved connection of nearby filaments
; Added MAX_DIST paramenter in input file (adds more flexibility)
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.3  2004/06/11 20:21:19  pietro
; Added some documentation in header
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Written by Pietro N. Bernasconi JHU/APL:
