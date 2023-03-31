;******************************************************************************
;***                                                                        ***
      function find_barbs, bnd, spine, factor, BARB_DIST=barbdist, $
                           REQ_SLOPE=reqslope, $
                           MIN_BRB_LENGHT=min_brb_len,$
                           MIN_BRB_LW_RATIO=min_brb_lw_ratio,$
                           MIN_BRBH_LW_RATIO=min_brbh_lw_ratio,$
                           SHOW_ALL_BRBS=show_all_brbs,$
                           FILENAME_TEST=filen,$
                           VERBOSE=verbose
;***                                                                        ***
;******************************************************************************
;+
; Project: Automated Solar Filament Detection and Characterization
; Name:    find_barbs
; Purpose: Return information about the number and chirality of filament barbs.
; Inputs:
;    bnd      = filament boundary
;    spine    = filament spine
;    factor   = scaling factor for displaying results
;
; Output: barb_stat =  A structure containing the following fields:
;      coords   = intarr(2,2,50): coordinates of start and stop of barb.
;                      (*,0,i) = x,y coordinates of start of ith barb
;                      (*,1,i) = x,y coordinates of stop of ith barb
;      barbdir = intarr(50): direction of barb. 1 = bearing left,
;                      -1  = bearing right, 0 = undetermined 
;      nbarbs = int: total number of barbs
;      nRight = int: number of barbs bearing right
;      nleft  = int: number of barbs bearing left
;      chir   = int: filament handyness: -1=right, 1=left, 0=unspecifyed
;
; KEYWORDS:
;    BARB_DIST = int: minimum distance for apex of a barb from
;                     spine. Default is the median of the distances
;    REQ_SLOPE = float: minimum slope in degrees of barb spine for deciding
;                     the left/right. Default = 0.6
;    MIN_BRB_LENGHT = float: minimum length [pix] for a potential barb to be
;                     taken in consideration. Default=3 pix
;    MIN_BRB_LW_RATIO  = float: ratio barb_len/barb_width above wich a
;                     candidate is identified as a barb. Default=0.3
;    MIN_BRBh_LW_RATIO = float: ratio of halph_barb_len/halph_barb_width above
;                     wich a candidate is identified as a barb. Default=0.3
;    SHOW_ALL_BRBS = flag: if set then plots all the candidate barbs. For
;                     testing purposes.
;    FILENAME_TEST = string: name of an ASCII file where all the candidate
;                     barb parameter are printed. For testing purposes.
;    VERBOSE = flag: if set then some extra text is printed on
;                     terminal. For testing purposes.
;
; COMMON BLOCKS:
;    colortab: c_tab, grey_lim (if undefined => a true color device is assumed)
;
; ROUTINES USED:
;    segment_parms, dist_to_spine, diatance, plot_cross, barb_direction,
;    fsc_color
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
plot_dist = 0   ;<-- Flag for debugging purposes
printout  = 0   ;<-- Flag for debugging purposes

;--- Handle input:
if (n_params() lt 3) then message,'Input parameters missing!'
if (n_elements(reqslope) le 0) then reqslope = 0.6
if (n_elements(reqdiff) le 0) then  reqdiff = 2
;if (n_elements(min_brb_len) le 0) then min_brb_len=3.
if (n_elements(min_brb_len) le 0) then min_brb_len=4
if (n_elements(min_brb_lw_ratio) le 0) then min_brb_lw_ratio=0.3
if (n_elements(min_brbh_lw_ratio) le 0) then min_brbh_lw_ratio=0.3
if (not keyword_set(show_all_brbs)) then show_all_brbs = 0
if (n_elements(filen) le 0) then do_out = 0 else do_out = 1
if (not keyword_set(verbose)) then verbose = 0

;--- Setting some variables:
x=0 & y=1
tol = 0   ;<-- Tolerance for finding the ends of the barb
max_barbs = 40
barb_stat = {brb_coord:intarr(2,2,max_barbs), barbdir:intarr(max_barbs), $

             barblen:fltarr(max_barbs), nbarbs:0, nRight:0, nLeft:0, chir:0}
past_min = 10  ;<-- How many pixel past a minimum are checked
;print,format='("past_min =", I3)',past_min

;--- Checks is the common block is defined. If not then checks what color
;    resolution it is. If present then display is NOT a true color
common colortab, c_tab, grey_lim
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
    green_c = fsc_color('green')
    blue_c = fsc_color('blue')
    yellow_c = fsc_color('yellow')
    cyan_c = fsc_color('cyan')
    purple_c = fsc_color('Purple')
endif else begin
    ;--- This are the setting for a standard monitor without True Color 
    red_c = ctb_color('red', c_tab)
    green_c = ctb_color('green', c_tab)
    blue_c = ctb_color('blue', c_tab)
    yellow_c = ctb_color('yellow', c_tab)
    cyan_c = ctb_color('cyan', c_tab)
    purple_c = ctb_color('purple', c_tab)
endelse

;--- plot contour of filament and its spine, if requested:
if show_all_brbs then begin
    plots, bnd/factor, /Device, Color=yellow_c
    plots, spine/factor, /Device, Color=cyan_c,psym=-2
endif

;--- Calculate the spine segments parameters: slope & y-intercept
dims = size(spine)
spine_seg = dblarr(2, dims(2)-1)
for i=0, dims(2)-2 do begin
    spine_seg(*,i) = segment_parms(spine(x,i), spine(y,i),$
                                   spine(x,i+1), spine(y,i+1) )
endfor

;--- Calculate the distance from each point to the spine
stopit=0
d = dist_to_spine(bnd, spine, spine_seg, stopit=stopit)
;d = smooth(median(d,2),3,/ed)
d = smooth(median(d,2),5,/ed)
dd = d

if (n_elements(barbdist) le 0) then barbdist = median(d)*0.8
if verbose then $
  print,'FIND_BARBS: barbdist =',barbdist,'   min_brb_len = ',min_brb_len

if do_out then begin
    openw,unit,filen,/get_lun,width=200
    format='(i3,f7.2,2(f7.3),2("  --",2(f7.2),f7.3))'
    printf,unit,'   Barb parameters for '+filen
    printf,unit,'  #  d_max  d_end1 d_end2     brb_l  brb_w  brb_l'
    printf,unit,'            /d_max /d_max                   /brb_w'
    printf,unit,'---------------------------------------------------------'
endif

if plot_dist then begin
    window,3,xs=1250,ys=800
    plot,d,psym=-1,/xs
    oplot,[0,n_elements(d)],[barbdist,barbdist],line=1
    wset,0
endif

;--- Process the farthest points
maxdist = max(dd, maxind)
nels   = n_elements(bnd)/2   ;<-- it's a 2-D array
nBarbs = 0
nRight = 0
nLeft  = 0
count  = 0
r_str  = '  R'
l_str  = '  L'
unkn_str =  '  ?'
cnts = ''
while (maxdist gt barbdist) do begin
    count = count + 1
    if (do_out or show_all_brbs) then cnts = strcompress(string(count))

    ;------------------------------
    ; Walk up and down the array to find the ends of this barb.
    ; tolerance for finding local mins; how many pixels can be farther from
    ; the spine before we decide we've reached the base
    ;--- End 1: -------------------
    end1 = find_bottom(d, max_index=maxind, direction=-1, past_min=past_min)
    ;------------------------------

    ;--- End 2: -------------------
    end2 = find_bottom(d, max_index=maxind, direction=1, past_min=past_min)
    ;------------------------------

    ;--- The following is to have a more symmetric range around the maxind
    origend1=end1
    origend2=end2
    if (maxind lt end1) then end1 = end1-nels
    if (end2 lt maxind) then end2 = end2+nels

    sdlt = 1.2
    if (maxind-end1 gt fix((end2-maxind)*sdlt + 0.5) ) then $
      end1 = maxind - fix((end2-maxind)*sdlt + 0.5) $
    else if (end2-maxind gt fix((maxind-end1)*sdlt + 0.5) ) then $
      end2 = maxind + fix((maxind-end1)*sdlt + 0.5)

    if (end1 lt 0) then end1 = nels + end1
    if (end2 ge nels) then end2 = end2 - nels
    ;-----------------------------

    ;--- Calculate the barb's spine as a straight line from the peak
    ;    (maxind) to the midpoint of the end1 and end2
    barb = fltarr(2,2)
    barb(*,0) = bnd(*,maxind)   ;<-- point farthest from spine
    barb(*,1) = (bnd[*,end1] + bnd[*,end2])/2.0
    barblen   = distance(barb(x,0),barb(y,0),barb(x,1),barb(y,1))
    barbwidth = distance(bnd(x,end1),bnd(y,end1),bnd(x,end2),bnd(y,end2))

    if (barblen ge min_brb_len) then begin
        ;--- barb spine at half distance from maxind:
        if (maxind ge end1) then end1h=fix((end1+maxind)/2.+0.5) $
        else begin
            end1h=fix((end1+maxind+nels)/2.+0.5)
            if (end1h ge nels) then end1h = end1h - nels
        endelse
        if (maxind le end2) then end2h=fix((end2+maxind)/2.+0.5) $
        else begin
            end2h=fix((maxind+end2+nels)/2.+0.5)
            if (end2h ge nels) then end2h = end2h - nels
        endelse
        barbh = barb
        barbh(*,1) = (bnd[*,end1h] + bnd[*,end2h])/2.0
        barblenh   = distance(barbh(x,0),barbh(y,0),barbh(x,1),barbh(y,1))
        barbwidth_h = distance(bnd(x,end1h),bnd(y,end1h),$
                                   bnd(x,end2h),bnd(y,end2h))
    endif else begin
        barblenh = 0.
        barbwidth_h = 0.
        end1h = 0  &  end2h = 0
    endelse

    ;--- The following is only for test purposes: --------------------------
    if printout then begin
        print
        print,count,'  ',end1,maxind,end2,'  ',end1h,maxind,end2h
        print,'        ',d(end1), d(maxind), d(end2)
        print,'       ',barblen, barbwidth, barblen/barbwidth
        print,'        ',d(end1h), d(maxind), d(end2h)
        if (barbwidth_h eq 0.) then $
          print,'        ',barblenh, barbwidth_h $
        else $
          print,'        ',barblenh, barbwidth_h, barblenh/barbwidth_h
    endif
    if plot_dist then begin
        wset,3
        plots,[end1,end2],[d(end1),d(end2)],psym=2,color=red_c
        plots,maxind,d(maxind),psym=2,color=green_c
        plots,[end1h,end2h],[d(end1h),d(end2h)],psym=2,color=purple_c
        xyouts,end1,d(end1),'  E1'
        xyouts,maxind,d(maxind),'  '+strcompress(string(count),/rem)
        wset,0
    endif

    if do_out then $
      printf,unit,form=format,count,d(maxind),d(end1)/d(maxind),$
        d(end2)/d(maxind),$
        barblen, barbwidth, barblen / barbwidth,barblenh, barbwidth_h, $
        barblenh / barbwidth_h
    
    if show_all_brbs then begin
        plot_cross, bnd[0,maxind]/factor,bnd[1,maxind]/factor,$
          color=green_c,len=2./factor
        plot_cross, bnd[0,end1]/factor,bnd[1,end1]/factor,color=blue_c,$
          len=2./factor
        plot_cross, bnd[0,end2]/factor,bnd[1,end2]/factor,color=blue_c,$
          len=2./factor
    endif
    ;-----------------------------------------------------------------------

    ;--- Logic to find out wheter there is a barb or not: ----------
    isbarb = 0
    if (barblen ge min_brb_len) then $
      if (barblen/barbwidth ge min_brb_lw_ratio) then isbarb=1 $
    else if (barblenh/barbwidth_h ge min_brbh_lw_ratio) then isbarb=1
    ;----------------------------------------------------------------

    ;--- Do the following if feature identifyed as a barb:
    if isbarb then begin
        if nBarbs lt max_barbs then begin
            barb_stat.brb_coord(*,*,nBarbs) = barb
            barb_stat.barblen(nBarbs) = barblen
        endif else $
          print, format='("More than ",I2," barbs, only first ",I2,'+$
          '" will be shown and considered")',max_barbs,max_barbs

	;--- Show the barbs
        plots, barb/factor, /Device, Color=green_c
        if show_all_brbs then begin
            plot_cross, bnd[0,maxind]/factor, bnd[1,maxind]/factor, $
              color=red_c,len=2./factor
            plot_cross, bnd[0,end1]/factor,bnd[1,end1]/factor,color=purple_c,$
              len=2./factor
            plot_cross, bnd[0,end2]/factor,bnd[1,end2]/factor,color=purple_c,$
              len=2./factor
        endif

        ;--- Gets in which direction the barb is coming off:
        barb_dir = barb_direction(barb, spine, REQ_SLOPE=reqslope)
        case 1 of
            barb_dir eq -1: begin
                xyouts,bnd[x,maxind]/factor,bnd[y,maxind]/factor,$
                  r_str+cnts,/dev, color=cyan_c
                if nBarbs lt max_barbs then barb_stat.barbdir(nBarbs) = -1
                nRight = nRight + 1
            end
            barb_dir eq 1: begin
                xyouts,bnd[0,maxind]/factor,bnd[1,maxind]/factor,$
                  l_str+cnts,/dev, color=cyan_c
                if nBarbs lt max_barbs then barb_stat.barbdir(nBarbs) = 1
                nLeft = nLeft + 1
            end
            else :  xyouts,bnd[0,maxind]/factor,bnd[1,maxind]/factor,$
              unkn_str+cnts,/dev, color=cyan_c
        endcase

        nBarbs = nBarbs + 1
    endif else if show_all_brbs then begin
        ;--- This only if it is not a barb:
        plots, barb/factor, /Device, Color=red_c
        xyouts,bnd[0,maxind]/factor,bnd[1,maxind]/factor,$
          '  '+cnts, /dev, color=fsc_red_c
    endif
    ;-------------------------------------    
        
    ;--- 2b. reset distances for all barb points
    if (origend1 gt maxind) then begin
        dd[0:maxind] = 0 & dd[origend1:nels-1] = 0
    endif else dd[origend1:maxind] = 0
    if (origend2 lt maxind) then begin
        dd[maxind:nels-1] = 0 & dd[0:origend2] = 0
    endif else dd[maxind:origend2] = 0

    maxdist = max(dd, maxind)

endwhile

barb_stat.nbarbs = nBarbs
barb_stat.nright = nRight
barb_stat.nleft  = nLeft

;--- Now, establish the filament chirality:
;if (nRight ge (nLeft + reqdiff)) then begin
;    barb_stat.chir = -1
;endif else begin
;    if (nLeft ge (nRight + reqdiff)) then begin
;        barb_stat.chir = 1
;    endif else begin
;        barb_stat.chir = 0
;    endelse
;endelse
barb_stat.chir = get_fil_chir(barb_stat.barblen, barb_stat.barbdir)

if do_out then begin
    close,unit
    free_lun,unit
endif
;stop
return,barb_stat
end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: find_barbs.pro,v $
; Revision 3.3  2010/10/27 20:54:57  bernapn1
; Something
;
; Revision 3.2  2010/10/27 19:38:01  bernapn1
; New, simpler and more efficient method to find the barbs
;
; Revision 3.1  2010/04/13 21:03:30  bernapn1
; Improved merging of filaments (written new routine).
; Improved way to remove small detected filaments.  Now before removing one it
; checks its area ratio. If it is a skinny one then it keeps it.
;
; Revision 3.0  2004/10/27 14:38:17  pietro
; Given revision 3.0
;
; Revision 2.4  2004/08/24 17:49:17  pietro
; Removed some old, commented out, lines of code that are useless
;
; Revision 2.3  2004/07/26 14:25:36  hakimd
; Now checks if nBarbs lt 30 before using nBarbs as a reference value for any
; array.  Outputs a message if more than 30 barbs are found but just
; continues going without showing the barbs, everything is still calculated
; correctly.
;
; Revision 2.2  2004/06/21 20:43:42  pietro
; *** empty log message ***
;
; Revision 2.1  2004/06/21 20:15:04  pietro
; Tested version with true color control
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.5  2004/06/16 20:33:09  pietro
; don't know
;
; Revision 1.4  2004/06/16 18:18:52  pietro
; Added keywords MIN_BRB_LENGHT, MIN_BRB_LW_RATIO, MIN_BRBH_LW_RATIO,
;     SHOW_ALL_BRBS, VERBOSE. And added relative logic
;
; Revision 1.3  2004/06/11 20:33:02  pietro
; Added comment in header
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Version 2, October 14 2003, By Pietro Bernasconi JHU/APL:
;             changed distToSpine to get a much better handle of the distance.
; Version 1, October 2 2003.
; Written: Kiri Wagstaff, JHU/APL, May 2003.
