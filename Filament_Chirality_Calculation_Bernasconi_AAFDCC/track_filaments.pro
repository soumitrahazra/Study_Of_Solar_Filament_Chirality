;******************************************************************************
;***                                                                        ***
     function track_filaments, fil_pars, sun_p, DT_MAX=dtm, $
                                 RESULTS_PATH = res_path, $
                                 MAX_IMG_BACK = max_back, $
                                 VOE_PATH = voe_path, $
                                 VERBOSE=verbose, DISPLAY=display
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    TRACK_FILAMENTS
;
; PURPOSE:
;    This function tracks filaments in past images
;
; CALLING SEQUENCE:
;    track_str = track_filaments(fil_pars, sun_p, $
;                  [ DT_MAX=dt_max, $
;                    RESULTS_PATH = res_path, $
;                    MAX_IMG_BACK = max_back
;                    VOE_PATH = voe_path, $
;                    /VERBOSE, /DISPLAY ]  )
;
; INPUTS:
;    fil_pars = structure array(#filams): array with the information about the
;                  filaments in the current image. The structure looks like
;                  the following:
;              AREA            INT           6261
;              AREA_RAT        FLOAT          0.162968
;              COORDS          INT       Array[2]
;              BBOX            INT       Array[2, 2]
;              BND             INT       Array[2, 3000]
;              NBND_P          INT            664
;              SPINE           INT       Array[2, 200]
;              ANGLE           FLOAT           20.4917
;              LENGTH          INT            320
;              NVERTS          INT             22
;              BRB_COORD       INT       Array[2, 2, 50]
;              BARBDIR         INT       Array[50]
;              NBARBS          INT              7
;              NRIGHT          INT              2
;              NLEFT           INT              4
;              CHIR            INT              1
;
;    sun_p = dblearr(6): array with the information about the Sun in this
;                  image. Its elements are the following:
;              0: Sun radius in pixels in the current image
;              1: X coords of Sun center in the current image in pixels
;              2: Y coords of Sun center in the current image in pixels
;              3: Julian date
;              4: Sun radius in arcsecs
;              5: P0 angle in degrees
;              6: B0 angle in degrees
;
; OPTIONAL INPUT PARAMETERS:
;    None
;
; KEYWORD PARAMETERS:
;    DT_MAX = float: Maximum number of days to track in the past. Def = 4
;    MAX_IMG_BACK = int: How many images in the past to analyze at
;                        max. Default = 1
;    RESULTS_PATH = string: path root to where the results are stored
;    VOE_PATH = string: path root to where the VOEvents are stores
;    VERBOSE = flag: if set it creates a printout, mostly for debugging
;    DISPLAY = flat: if set it makes an image of what is doing for debugging
;
; OUTPUTS:
;    track_str = structure array(#filams) with the results as defined
;                  below.  Note the filaments numbering in track_str.XXX.num
;                  (XXX being one of FOLLOWS, MERGE(nn), SPLIT) starts
;                  from 1! That means that in the past images the first
;                  filament is labelend beginning from 1 and NOT from 0.
;          FOUND      INT    = if 1 then the filament was found
;          FOLLOWS    STRUCT = info on the filament it follows from
;          N_MERGE    INT    = # of past filaments is a merge of
;          MERGE  STRUCT Array[20] = info on filamentS if merges from
;          SPLIT  STRUCT = info on the filamen it is a split from
;
;                  Each of the substructures is as follows;
;          JDATE      DOUBLE = Julian date of the originating filament
;          DATE       INT Array[6] = date in [YYYY, MM, DD, hh, mm, ss] 
;          NUM        INT    = number of the originating filam (starts from 1!)
;          IVO        STRING = IVORN of the originating filament
;
; OPTIONAL OUTPUT PARAMETERS:
;    None
;
; ROUTINES USED:
;    make_datestring, js2ymds, jd2js, pix2helio, helio2pix, diff_rotate,
;    plot_helio_grid (BUT only if DISPLAY is set)
;
; COMMON BLOCKS:
; PROCEDURE:
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************

if (n_elements(dtm) le 0) then dtm = 4.
if (n_elements(max_back) le 0) then max_back = 1
if max_back lt 1 then max_back = 1
if (n_elements(res_pathi) le 0) then res_path = filam_get_paths(/results)
if (n_elements(voe_pathi) le 0) then voe_path = filam_get_paths(/voe)

if (n_elements(verbose) le 0) then verbose = 0
if (n_elements(display) le 0) then display = 0

;---- Generates date information for the current image:
jdate0 = sun_p(3)
js2ymds,jd2js(jdate0),y, m, d, h, mn, s
date0 = [y, m, d, h, mn, s]

;------------------------------------------------------------------------------
;---- Gathering the necessary past information: ------------------------------
past_info = {date:intarr(6), clusters:'', table:'', voe_list:''}
past_table = replicate(past_info, dtm*10)
count = 0
for dt=0,dtm do begin
    ;--- Generate past date info
    jdatep = jdate0-dt
    js2ymds,jd2js(jdatep),y, m, d, h, mn, s
    datep = [y, m, d, 0, 0, 0]

    ;--- Generating search pahs
    prefix = make_datestring(datep(0:2))

    year_month = strcompress(string(datep(0)),/rem) + '/'
    if (datep(1) lt 10) then year_month = year_month +'0'
    year_month = year_month + strcompress(string(datep(1)),/rem) + '/'

    ;--- Searching in the archives for past clusters tables:
    cluster_wld = res_path + year_month + prefix + '*_Ha_msk.idl'
    cluster_f = file_search(cluster_wld, count=nf)

    ;--- Skips to the next day if nothing found on this day
    if (nf le 0) then continue

    for i=nf-1,0,-1 do begin  ;<-- going backwards
        ;-- extract time information from file name:
        timep_s = strmid(cluster_f(i), strpos(cluster_f(i),prefix)+9, 6)
        for j=0,2 do datep(3+j)=fix(strmid(timep_s,j*2,2))

        ;-- Makes sure it is not "in the future":
        jdatep = julday(datep(1),datep(2),datep(0),datep(3),datep(4),datep(5))
        if (jdate0-jdatep le 0) then continue

        ;-- Makes double sure it is not the current file
        if (strpos(make_datestring(date0[3:*]),timep_s) eq 0) then continue

        ;-- Searching for other past info on this specific file:
        tab_wld = res_path + year_month + prefix+'_'+timep_s+'_Ha.tab'
        table_f = file_search(tab_wld, count=nf_t)

        voe_wld = voe_path + year_month + prefix+'_'+timep_s+'_VOE_list.txt'
        voe_f   = file_search(voe_wld, count=nf_v)

        ;-- Filling the past_info table:
        past_table(count).date = datep
        past_table(count).clusters = cluster_f(i)
        if (nf_t gt 0) then past_table(count).table = table_f(0)
        if (nf_v gt 0) then past_table(count).voe_list = voe_f(0)
        ;print,count
        count = count + 1
    endfor
endfor
if count gt 0 then $
  past_table = past_table(0:count-1) $
else begin
    if verbose then print,'    Past data required for tracking',$
      ' NOT found! ==> NO tracking'
    return, -1            ;<-- means that no images in the past were found
endelse
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

;---- Creates the tracking structure:
nfils = n_elements(fil_pars)
aa = {num:0, ivo:''}
bb = {found:0, jdate: 0.d, date:intarr(6), datestr:'', n_follow:0, follow:aa, $
      n_merge:0, merge:replicate(aa, 20), n_split:0, split:replicate(aa,20)}
tr_str = replicate(bb, nfils+1)

;---- Converts current spine date coords in heliocentric coords:
nverts  = fil_pars.nverts
maxnv   = max(nverts)
spine0  = float(fil_pars.spine(*,0:maxnv-1))
spine0h = pix2helio(jdate0, /julian, sun_p(0), spine0, xy_c = sun_p(1:2) )
spine1h = spine0h

;------------------------------------------------------------------------------
;---- Tracking the single filaments through time: -----------------------------
limit = min([count,max_back]) ;<-- To do track back to > than 1 img
for i = 0, limit-1 do begin
;for i = 0,0 do begin
    if (total(tr_str.found) eq nfils) then break

    ;--- Gets the past clusters tables and the sun_p parameters
    restore,past_table(i).clusters  ;<-- imports clusters1 and sun_p1
    szc = size(clusters1)
    hc  = histogram(clusters1,bin=1)
    szhc = size(hc)
    if (szhc(1) le 1) then break

    found1=replicate({found:0 , which:intarr(20)}, szhc(1))

    jdate1 = sun_p1(3)
    date1  = past_table(i).date

    if verbose then begin
        if i gt 0 then print
        print,'    Tracking from Ha image taken on ', $
          make_datestring(date1(0:2),sep='/'),' ',$
          make_datestring(date1(3:*),sep=':')
    endif

    ;--- If the VOEvents are present reads the IVORNS from them:
    have_voe = 0
    if (strlen(past_table(i).voe_list) gt 0) then begin
        have_voe = 1

        year_month = strcompress(string(date1(0)),/rem) + '/'
        if (date1(1) lt 10) then year_month = year_month +'0'
        year_month = year_month + strcompress(string(date1(1)),/rem) + '/'

        ivos = strarr(100)
        voe = ''
        openr,voe_unit,past_table(i).voe_list,/get_lun
        get_lun,voe_unit1
        n_ivos = 1
        while not(eof(voe_unit)) do begin
            readf,voe_unit,voe
            voe = voe_path + year_month + voe
            ivos(n_ivos) = get_voe_ivorn(voe)
            if (strlen(ivos(n_ivos)) gt 0) then n_ivos = n_ivos+1
        endwhile
        close,voe_unit
        free_lun,voe_unit
    endif

    ;--- Rotate back the current spine coords to the previous time:
    nverts1 = nverts
    for n=0,nfils-1 do begin   ;<-- do it only for the filaments not tracked
        nvr = nverts(n)
        nvr_n = 0
        for j=0,nvr-1 do begin
            ;-- Calculates the "new" longitude in the past.
            tmpl = diff_rotate(jdate1-jdate0, spine0h(0,j,n), $
                               spine0h(1,j,n), /aver )

            ;-- Uses it only if it is still withing the Sun
            if (tmpl ge -85) then begin
                spine1h(0,nvr_n,n) = spine0h(0,j,n)
                spine1h(1,nvr_n,n) = tmpl
                nvr_n = nvr_n+1
            endif
        endfor
        nverts1(n) = nvr_n
    endfor

    ;--- Converts heliographic coords back to pixel coords
    spine1 = helio2pix(jdate1, /julian, sun_p1(0), spine1h, xy_c=sun_p1(1:2) )

    ;--- Shows an image mostly for debugging: ---------------------------------
    if display then begin
        window,1,xs=990,ys=990
        f = 990./2032.
        www = where(clusters1 eq 0)
        tmp = clusters1
        tmp(www) = 1000
        tvscl,congrid(tmp,2032*f,2032*f)
        for n=1,szhc(1)-1 do begin
          ns = strcompress(string(n),/rem)
          w = where(clusters1 eq n)
          xyouts,(w(0) mod 2032)*f,(w(0) / 2032)*f,ns,/dev,align=0.2, $
                color=fsc_color('blue'),chars = 1.5,charth=2
        endfor
          
        plot_helio_grid,sun_p1(0)*f,b0=sun_p1(6),sun_c=sun_p1(1:2)*f,$
           /noer,color=fsc_color('olive'),line=2

        for n=0,nfils-1 do begin
          if (nverts1(n) ge 1) then begin
            ns = strcompress(string(n+1),/rem)
            nf = nverts(n)-1
            ;plots,spine0(0,0:nf,n)*f,spine0(1,0:nf,n)*f,/dev,$
            ;    psym=-1,color=fsc_color('red')
            ;xyouts,spine0(0,0,n)*f,spine0(1,0,n)*f,ns,/dev,align=0.9, $
            ;    color=fsc_color('yellow')
            nf = nverts1(n)-1
            plots,spine1(0,0:nf,n)*f,spine1(1,0:nf,n)*f,/dev,$
                psym=-1,color=fsc_color('red')
            xyouts,spine1(0,0,n)*f,spine1(1,0,n)*f,ns,/dev,align=0.9, $
                color=fsc_color('maroon'),chars=1.5,charth=2
          endif
        endfor
    endif
    ;--------------------------------------------------------------------------

    ;--- Cycles through all the filaments and checks for matches:
    for n=0,nfils-1 do begin
        cur = n+1

        if (tr_str(cur).found gt 0) then goto,at_end_match

        nv = nverts1(n)
        if (nv lt 2) then goto,at_end_match

        ;-- Makes a mask of the spine:
        sp_mask = spine_mask(spine1(*,0:nv-1,n), ll=ll)
        szsp = size(sp_mask)
        w = where(sp_mask gt 0, fil_sz)
        mask = intarr(szc(1),szc(2))
        mask(ll(0):ll(0)+szsp(1)-1, ll(1):ll(1)+szsp(2)-1) = sp_mask

        ;-- Gets the overlap with the clusters map and gets the histogram:
        h = histogram(clusters1*mask, bin=1)
        w = fix(where(h gt 0, nw))

        ;-- A Match was found! ==> analyzes it: *******************************
        if (nw gt 1) then begin
            tt = float(total(h(1:*)))  ;<-- total # pix overlap

            prev_sz  = hc(w)
            overl_sz = h(w)
            prc_tot  = overl_sz/tt*100.
            prc_prev = overl_sz/float(prev_sz)*100.
            prc_cur  = overl_sz/float(fil_sz)*100.

            if 0 then begin
                print,'Filament #', cur, fil_sz
                for ii = 1,nw-1 do print,w(ii),prc_tot(ii),$
                  prc_prev(ii),prc_cur(ii), hc(w(ii))
            endif

            ;- Logic to determine whether the match is good or not:
            prev = intarr(nw-1)
            cnt = 0
            for ii=1,nw-1 do begin
                handle_it = 0
                if (prc_prev(ii) gt 8) then $
                  handle_it = 1 $
                else if (prc_prev(ii) gt 0.8) then begin
                    if (prc_cur(ii) gt 1.5) then handle_it = 1
                endif

                if (handle_it) then begin
                    prev(cnt) = w(ii)
                    cnt = cnt+1
                endif
            endfor

            ;- Fills the track structure: -------------------------------------
            if (cnt gt 0) then tr_str(cur).found = 1

            if (cnt gt 1) then tr_str(cur).n_merge = cnt

            for f = 0,cnt-1 do begin
                tr_str(cur).jdate   = jdate1
                tr_str(cur).date    = date1
                tr_str(cur).datestr = make_datestring(date1(0:2),sep="-")+$
                  'T'+make_datestring(date1(3:5),sep=":")+'.000z'

                is_split = 0
                if (cnt gt 1) then begin
                    ;- Handles a MERGE:
                    tr_str(cur).merge(f).num   = prev(f)
                    if (have_voe) then $
                      tr_str(cur).merge(f).ivo = ivos(prev(f))

                    if (found1(prev(f)).found gt 0) then is_split = 1

                endif else begin
                    ;- Handles a FOLLOW:
                    if (found1(prev(f)).found gt 0) then begin
                        ; Is a split, handle it later
                        is_split = 1
                    endif else begin
                        tr_str(cur).n_follow = 1
                        tr_str(cur).follow.num   = prev(f)
                        if (have_voe) then $
                          tr_str(cur).follow.ivo = ivos(prev(f))
                    endelse
                endelse

                if (is_split eq 1) then begin
                    ;- Handles a SPLIT:
                    for ii = 1, found1(prev(f)).found do begin
                        ; cycles throug the ones previously found with same
                        which = found1(prev(f)).which(ii)
                        if (tr_str(which).n_follow gt 0.) then begin
                            ; Means that previously it was labeled as a 
                            ; follow ==> it must be changed into a split
                            spl_i = tr_str(which).n_split
                            tr_str(which).n_split = spl_i+1
                            tr_str(which).split(spl_i) = tr_str(which).follow
                            tr_str(which).n_follow = 0
                            tr_str(which).follow = aa
                        endif
                    endfor
                    spl_i = tr_str(cur).n_split
                    tr_str(cur).n_split = spl_i+1
                    tr_str(cur).split(spl_i).num   = prev(f)
                    if (have_voe) then $
                      tr_str(cur).split(spl_i).ivo = ivos(prev(f))
                endif

                found1(prev(f)).found = found1(prev(f)).found + 1
                found1(prev(f)).which(found1(prev(f)).found) = cur
            endfor
        endif ;----------------------------------------------------------------
        at_end_match:
        ;**********************************************************************
    endfor
endfor
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

;--- Prints the results if requested:
if verbose then begin

    print,'    Results:'

    if (total(tr_str.found) eq 0) then begin
        print,'    NO filament matches with previous data found'
    endif else begin
        for i=1,nfils do begin
            print,format='("    Filam #",I3,":",$)',i
            if (tr_str(i).found eq 0) then begin
                print,'  NEW'
                continue
            endif

            if (tr_str(i).n_follow gt 0) then begin
                print,format='("  Follows   ",I3)',tr_str(i).follow.num
                continue
            endif

            if (tr_str(i).n_merge gt 0) then begin
                print,format='("  Merge from",$)'
                for j=0,tr_str(i).n_merge-1 do $
                  print,format='(I3,$)',tr_str(i).merge(j).num
            endif

            if (tr_str(i).n_split gt 0) then begin
                if (tr_str(i).n_merge gt 0) then print,format='("  ",$)'
                print,format='("  Split from",$)'
                for j=0,tr_str(i).n_split-1 do $
                  print,format='(I3,$)',tr_str(i).split(j).num
            endif

            print
        endfor
    endelse
endif

return, tr_str(1:*)

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: track_filaments.pro,v $
; Revision 3.5  2010/10/27 20:54:57  bernapn1
; Something
;
; Revision 3.4  2010/05/06 14:33:50  bernapn1
; Several changes and improvements.  Now using a new technique to track
; filaments
;
; Revision 3.3  2010/05/03 18:13:44  bernapn1
; Now using get_voe_ivorn to get the AUTHORIVORN from an existing VOEvent
;
; Revision 3.2  2010/04/29 21:05:42  bernapn1
; Completely redone.  Now it does tracking using the clusters maps previously
; saved. It also only tracks in the past since the idea is that when yuou
; run the routine there is no knowledge of what will happen in the future.
;
; Written by Pietro N. Bernasconi JHU/APL
