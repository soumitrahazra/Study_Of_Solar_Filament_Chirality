;******************************************************************************
;***                                                                        ***
      function preprocess, infilei, threshold, startx, starty, width, height, $
                           SUN_PARMS = sun_p, FITS_HEADER = header, $
                           DATE_STRING = date, MASK_ABOVE = mask_above, $
                           BACKGROUND = backgr, DATA_UNMASKED=data_nmsk, $
                           SMOOTH=smth, MEDIAN_WIDTH=med_w, $
                           MEDIAN_LIMIT=med_l, EQUALIZE_LEVELS = equal_lev,$
                           FIT_SURFACE = dofit, CENTER = docenter, $
                           SUN_RADIUS = sun_rad, OUT_SIZE = out_size,$
                           C_PATH=c_path, VERBOSE=verbose
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    PREPROCESS
;
; PURPOSE:
;     Preprocess the data to:
;       1. mask out extra-solar pixels.
;       2. if requested apply solar disk background flattening.
;       3. adjust overly bright pixels so that the display is scaled reasonably
;          (does not affect the data that actually gets analyzed, later).
;       4. if specicyed it will return only a subset of interest.
;       5. extract some useful parameters from the image fits header.
;
; CALLING SEQUENCE:
;    data = preprocess( infile, threshold [startx, starty, width, height, $
;                       SUN_PARMS = disk_parms, FITS_HEADER = header, $
;                       DATE_STRING = date_srting, MASK_ABOVE = mask_above,$
;                       BACKGROUND = background, DATA_UNMASKED=data_nmsk,$
;                       SMOOTH=smth, MEDIAN_WIDTH=m_wdt, MEDIAN_LIMIT=limit, $
;                       /EQUALIZE_LEVELS, /FIT_SURFACE, /CENTER,$
;                       SUN_RADIUS=sun_rad, OUT_SIZE=out_size,$
;                       C_PATH=c_path, /VERBOSE])
;
; INPUTS:
;    infile    = string: name of input file
;    threshold = float: threshold value below which filaments are identified.
;
; OPTIONAL INPUT PARAMETERS:
;    startx = int: start X coordinate of a selected subframe
;    starty = int: start Y coordinate of a selected subframe
;    width  = int: with of a selected subframe
;    height = int: height of a selected subframe
;
; KEYWORD PARAMETERS:
;    SUN_PARMS = dblarr(7): Solar disk parameters: 0) Sun radius [pix]
;                                                  1) X coord of center [pix]
;                                                  2) Y coord of center [pix]
;                                                  3) Julian Date
;                                                  4) Sun radius [arcsec]
;                                                  5) Solar P  angle [deg]
;                                                  6) Solar B0 angle [deg]
;    DATE_STRING = string: date of observations as extracted from the
;                  fits header. Example: "2002-05-18T15:38:27.000Z"
;    FITS_HEADER = strarray(header_entries): string with all the fits
;                  header informations
;    DATA_UNMASKED = array(width,height): data not masked, used for display
;    MASK_ABOVE = float: solar polar degrees beyoud which filaments are
;                  masked out. Default = 75 degrees. 
;    BACKGROUND = int: value for the "sky". Default = threshold+10
;    SMOOTH = int: applies a smoothing to the data if requested
;    MEDIAN_WIDTH = int: width of the median box. If not given then NO
;                  median filter is applied
;    MEDIAN_LIMIT = float: limit above which the median value is
;                  repaced. Default = 0
;    EQUALIZE_LEVELS = flag: if set then the intensity levels are equalized
;                  With respect to image "bbso_halph_fr_20030816_153649"
;    FIT_SURFACE = flag: if set then a polynomial curve fit is applyed to
;                  the data to remove residual inhomogeneities from the
;                  background h-alpha.
;    CENTER = flag: if set then it moves the center of the Sun at the center
;             of the image
;    SUN_RADIUS = float: if set then the solar radius is forced to be this
;                 value in pixels. Suggest to set it to 975 pixels. WARNING:
;                 this may modify the image size and replaces the center of
;                 the Sun at the center of the image!
;    OUT_SIZE = int or intarr(xs,ys): if set then it forces a specific image
;               output size. !! This KEYWORD is inhibited if the optional
;               input parameters startx, starty, width, or height are given!
;               It can be given as a single value in which case
;               the same size will be ginven on boch X and Y axis: eg if given
;               1024 then the image size will be: [1024,1024]. Alternatively
;               if given as a 2 elements array it will impose different sizes
;               for the two dimentions. If nothing is given
;               then the image size will be the same as the one of the
;               original image. Warning! if the sun diameter is larger than
;               the given image size then there will be a clipping of the
;               Sun's image!!  If used I suggest to give [2048,2048].
;    C_PATH = string: Directory path where the c executables are located.
;                  Default is given by the call to the routine:
;                  filam_get_paths(/bin)
;    VERBOSE = flag: if set then some messages are printed
;
; OUTPUTS:
;    data = array(width,height):
;
; IDL PROCEDURES USED:
;    readfits (and a suite of fits reading programs), dist_circle,
;    fit_surface, remove_spots, find_disk_parms, mfilter, get_jdate,
;    filam_get_paths, sun_clv
;
; C PROCEDURES & SHELL SCRIPTS USED:
;    gunzip, gzip, sun_ephemeris
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 2) then message,'some required input parameter is missing!'

infile=infilei
if (not keyword_set(verbose)) then verbose = 0 else begin
    tmp_p = strpos(infile,'/',/reverse_se)
    print,'Preprocessing the data:'
endelse

if (n_elements(mask_above) le 0) then mask_above = 75.
mask_above = mask_above * !PI/180.  ;<-- converts it to radians

if (n_elements(backgr) le 0) then backgr = threshold + 10
if (backgr lt threshold) then backgr = threshold + 10

if (n_elements(equal_lev) le 0) then equal_lev = 0

if (n_elements(c_path) le 0) then c_path=filam_get_paths(/bin) $
else if (size(c_path, /type) ne 7) then begin
    print,'% PREPROCESS: C_PATH is not a string!'
    c_path=filam_get_paths(/bin)
    print,'                reset to directory '+c_path
endif

;--- Checks if infile exists:
tmp = file_search(infile, count = n_files)
if (n_files le 0) then begin
    print,'% PREPROCESS: input file "',infile,'" does NOT esist!'
    return,-1
endif

;------------------------------------------------------------------------------
;---- Reading the data: -------------------------------------------------------
if verbose then print,format='("    Reading file ",A," ...",$)',$
  strmid(infile,tmp_p+1)
data = readfits(infile, header)

;-- If an error was issued from readfits:
if (data(0) eq -1) then begin
    print,'% PREPROCESS: ERROR reading fits file !'
    return,-1
endif
if verbose then print,' DONE!'

;--- Getting the size & location of the window in which to do the analysis:
sz=size(data,/dim)
if (n_elements(startx) le 0) then startx = 0
if (n_elements(starty) le 0) then starty = 0
if (n_elements(width)  le 0) then width  = sz(0)
if (n_elements(height) le 0) then height = sz(1)
if (n_elements(smth)   le 0) then smth   = 0
;- This to make sure that the subframe falls within the image boundaries:
if ((width le 0) or (startx+width gt sz(0)) ) then width = sz(0)-startx
if ((height le 0) or (starty+height gt sz(1)) ) then height = sz(1)-starty


xy_off = [0,0]
;--- If requested then do the resizing of image:
if (n_elements(out_size) gt 0) then begin
    ;-- inhibits the use of out_size if a subframe was previously selected:
    ;if not((startx+starty eq 0) and (width eq sz(0)) and (height eq sz(1)) ) $
    ;  then goto,jump_out

    outs = out_size
    if (n_elements(outs) eq 1 ) then outs = [outs,outs]

    ;-- if image size is equal requested size then do nothing:
    if (outs(0) eq sz(0)) and (outs(1) eq sz(1)) then goto,jump_out

    if (verbose) then $
      print,format='("    Resizing image from  ",I4,"x",I4'+$
      ',"  to  ",I4,"x",I4)',sz(0),sz(0),outs(0),outs(1)

    xy_off = (sz - outs ) / 2
    ll = xy_off
    if ( (ll(0) ge 0) and (ll(1) ge 0) )  then begin
        ; case if new image completely enclosed in old image (!full clipping!)
        data = data(ll(0):ll(0)+outs(0)-1,ll(1):ll(1)+outs(1)-1)
    endif else begin
        data1 = replicate(data(0,0),outs(0),outs(1))
        if ( (ll(0) lt 0) and (ll(1) lt 0) ) then begin
            ; case if new image encloses completely old image (no clipping)
            ll = -ll
            data1(ll(0):ll(0)+sz(0)-1,ll(1):ll(1)+sz(1)-1) = data
        endif else if ((ll(0) lt 0) and (ll(1) ge 0)) then begin
            ; case where new image x sz > old imag x sz (Y clipping!)
            ll(0) = -ll(0)
            data1(ll(0):ll(0)+sz(0)-1,*) = data(*,ll(1):ll(1)+outs(1)-1)
        endif else if ((ll(0) ge 0) and (ll(1) lt 0)) then begin
            ; case where new image x sz > old imag x sz (X clipping!)
            ll(1) = -ll(1)
            data1(*,ll(1):ll(1)+sz(1)-1) = data(ll(0):ll(0)+outs(0)-1,*)
        endif
        data = data1
        sz=size(data,/dim)
    endelse
endif
jump_out:
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;---- Checking if the data makes sense: ---------------------------------------
;-- Getting the observatory from which the data originated:
observatory = sxpar(header,'ORIGIN')
case 1 of
    (strpos(observatory,'BBSO') ge 0): observatory='BBSO'
    (strpos(observatory,'KANZELHOEHE') ge 0): observatory='KANZ'
    else: begin
        print,'% PREPROCESS: can not process images from ',observatory
        return, -1
    endelse
endcase

;-- Check if the file is empty:
if (total(abs(data(sz(0)/2-10:sz(0)/2+10,sz(1)/2-10:sz(1)/2+10))) eq 0) $
  then begin
    print,'% PREPROCESS: ERROR reading fits file! It appears to be empty!'
    return,-1
endif

;-- Correcting for the proper pixel scale:
if (observatory eq 'KANZ') then begin
    if (strpos(sxpar(header,'BUNIT'), 'CONTRAST') ge 0) then data = data-100.
    if (strpos(sxpar(header,'BUNIT'), 'PERCENT') ge 0) then data = data*100.
endif

;-- If data read correctly but it is only garbage: 
minval= 100  &  maxval=1000
min_total = 4129024. / float(n_elements(data)) * 500000.
h =[histogram(data, bin=100, min=-maxval, max=-minval),$
    histogram(data, bin=100, min=minval, max=maxval)]
tt =  total(h)
if (tt lt min_total) then begin
    print,'% PREPROCESS: Detected incorrect intensity levels!'
    print,format='(14X,"Only ",I6," pixels between abs(100 and 1000)")', tt
    return, -1
endif
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;---- Determine date of the observation:
date = sxpar(header,'DATE_OBS') & szt=size(date,/type)
if (szt ne 7) then begin
    date = sxpar(header,'DATE-OBS') & szt=size(date,/type)
    if (szt eq 7) then date = date + 'T' + sxpar(header,'TIME-OBS') + $
      '.000Z'
endif
if (szt ne 7) then date = pars.image
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;---- Gets the Solar parameters: ----------------------------------------------
; sun_p(0) = sun radius (pixels)
; sun_p(1) = X coordinate of Sun center
; sun_p(2) = Y coordinate of Sun center
; sun_p(3) = Julian date of when the image was taken
; sun_p(4) = Sun radius [arcsec]
; sun_p(5) = P  angle [deg]
; sun_p(6) = B0 angle [deg]

sun_p = dblarr(7)  ;<-- Initialize sun_p

jul_dat = get_jdate(date) ;<-- calculate Julian Date
sun_p(3) = jul_dat

;--- Determine Sun radius (in pixels): ----
case 1 of
    (observatory eq 'BBSO'): begin
        ;-- the following is for BBSO (Sun Radius):
        if (sun_p(0) eq 0) then sun_p(0) = sxpar(header,'IMAGE_R0')
    end
    (observatory eq 'KANZ'): begin
        ;-- the following is for KANZELHOEHE (Sun Radius):
        sun_p(0)=sxpar(header,'SOLAR_R')
    end
    else : print,'UNKNOWN observatory'
endcase
if (sun_p(0) eq 0) then sun_p(0) = sxpar(header,'WIDT')/2.
;- Checking if we got the Sun diameter instead of the radius:
if (sun_p(0) gt 1700) then sun_p(0) = sun_p(0)/2.

;--- Determine coordinates of Sun center (pixels)
if (sun_p(0) eq 0) then begin
    ;-- Do this if Sun radius still undefined:
    if verbose then $
      print,'    Solar disk parameters not in fits file!'
    sun_p(0:2) = find_disk_parms(data, disk_thr = -1000, verbose=verbose, $
                            /force, /optim)
endif else begin
    ;--- Extract sun image parameters from fits header: ---
    if ((observatory eq 'BBSO') and (jul_dat lt 2454789.5)) then begin
        ;---                                    2454789.5 = 2008/11/19
        ;--- Before 2006/01/01  using CENX & CENY for the pixel
        ;    coordinates of the center of the Sun
        sun_p(1) = sxpar(header, 'CENX') ;<- x-coord of disk center 
        sun_p(2) = sxpar(header, 'CENY') ;<- y-coord of disk center

        ;This below is a cluge to patch a problem sometimes arising with
        ;BBSO fits files:
        if (sun_p(1) lt 800) then sun_p(1) = sxpar(header, 'CRPIX1')
        if (sun_p(2) lt 800) then sun_p(2) = sxpar(header, 'CRPIX2')
    endif else begin
        ;--- Afterwards both BBSO and KANZ are using CRPIX for Sun center
        sun_p(1) = sxpar(header, 'CRPIX1') ;<- x-coord of disk center 
        sun_p(2) = sxpar(header, 'CRPIX2') ;<- y-coord of disk center
    endelse

    ;-- Handles KANZELHOEHE data:
    ;-- A patch to account for an anomaly in KANZELHOEHE fits header
    if (observatory eq 'KANZ') then begin
        ;- Julian Date when Kanzelhoehe switched format (2003/10/06):
        jd_kanz_switch = 2452918.5

        ;- Accounting for a specular reverse in the Kanzelhoe data:
        if (jul_dat lt jd_kanz_switch) then begin
            data = reverse(data,1)
            sun_p(1) = sz(0) - 1 - sun_p(1)
            xy_off(0) = -xy_off(0)
        endif
    endif

    ;--adding offsets from the resizing (if it was performed):
    sun_p(1:2) = sun_p(1:2) - xy_off
endelse

;--- Determine solar radius, P and B0 angles (in degees):
date_img = strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)+$
  strmid(date,11,2)+strmid(date,14,2)+strmid(date,17,2)
spawn,c_path + 'sun_ephemeris '+date_img, answer
if (float(answer(0)) ne 0.) then begin
    sun_p(4) = float(answer(3))   ;<-- Sun radius [arcsec]
    sun_p(5) = float(answer(4))   ;<-- P  angle [deg]
    sun_p(6) = float(answer(5))   ;<-- B0 angle [deg]
endif else begin
    sun_p(4) = -1
    sun_p(5) = -100
    sun_p(6) = -100
endelse
;print,sun_p
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;---- For KANZELHOEHE data if it is in PERCENT CONTRAST format convert it in
;     only PERCENT format: ---
if ( (observatory eq 'KANZ') and $
     (strpos(sxpar(header,'BUNIT'), 'CONTRAST') ge 0) ) then begin
    reverse_ctr = sun_clv(xs=sz(0),ys=sz(1),rad=sun_p(0),$
                          xc=sun_p(1),yc=sun_p(2),parm=[0.84,0.098])
    data = data*reverse_ctr
endif
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;---- Forces the size of the Sun to a specific value and/or centers it: ---
if (n_elements(sun_rad) gt 0) then begin
    ;-- Resizes Sun and center it:

    if (sun_p(0) eq sun_rad) then begin
        docenter = 1
    endif else begin

        if (verbose) then print,$
          format='("    Resizing Sun size from radius of",'+$
          'I4," to ",I4," pixels")',sun_p(0),sun_rad

        xsn = sun_rad/sun_p(0)*sz(0)
        ysn = sun_rad/sun_p(0)*sz(1)
        xcn = sun_rad/sun_p(0)*sun_p(1)
        ycn = sun_rad/sun_p(0)*sun_p(2)

        sun_p(0) = sun_rad
        sun_p(1) = (sz(0) / 2 - 1 )
        sun_p(2) = (sz(1) / 2 - 1 )

        ;Creating a large array to dump the data in:
        szb = max([xsn,sz(0)])  ;<-- Get the lartest size
        szb = szb + fix(szb*0.5) ;<-- Increase it by 50%
        szb = szb + (szb mod 2) ;<-- make it even
        cnb = szb/2 - 1         ;<-- get the center
        big = replicate(data(0,0),szb,szb)

        ;Pluck in the data in its middle:
        ll = fix(cnb - xcn)
        bb = fix(cnb - ycn)
        big(ll:ll+xsn-1,bb:bb+ysn-1) = congrid(data,xsn,ysn,cub=-0.5,/center)

        ll = fix(cnb - sun_p(1))
        bb = fix(cnb - sun_p(2))
        data = big(ll:ll+sz(0)-1,bb:bb+sz(1)-1)
        docenter = 0
    endelse
endif

if (keyword_set(docenter)) then begin
    ;-- Center Sun
    xc = (sz(0) / 2 - 1 )
    dltx = round(xc - sun_p(1))

    yc = (sz(1) / 2 - 1 )
    dlty = round(yc - sun_p(2))

    sun_p(1) = xc
    sun_p(2) = yc

    if ( (dltx ne 0) or (dlty ne 0) ) then data = shift(data,dltx,dlty)
endif
;------------------------------------------------------------------------------

if (verbose) then print,$
  format='(4X,"Sun radius =",I5,4X,"Coord center = (",I5,",",I5,")")',$
  sun_p(0:2)

;---- Polynomial fit to the data for removal of residual inhomogeneities
if keyword_set(dofit) then begin
    cf = fit_surface(data, SUN_P=sun_p, /force, /adj_offs, verbose=verbose)
    data = data - cf
endif

;---- If not a Big Bear image the P angle is not accounted for => correct it:
if ( (observatory eq 'KANZ') and $
     (sun_p(4) ne -1) ) then begin
    pang = sun_p(5)
    if verbose then print,$
      form='("    Rotating the image to account to P angle offset ...",$)'

    data = rot(data, pang, 1., sun_p(1), sun_p(2), /pivot, /interp)

    if verbose then print,' DONE!'
endif

;--- Intensity levels equalization using gauss-histogram fit:
if (equal_lev) then begin
    ;-- Data from the reference for image "bbso_halph_fr_20030816_153649"
    n_el_ref = 102340L
    gf_ref = 1213.19

    ;-- Reference standard deviation from image:
    stdv_r = 165.0

    if (observatory eq 'KANZ') then begin
        ;- This for KANZELHOEHE:
        mx_limit_ratio    = 1.08 ;<- above this limit the equaliz is done with:
        mx_limit_ratio_eq = 1.08 ;<- THIS factor
    endif else begin
        ;- This for BBSO:
        mx_limit_ratio    = 1.3 ;<- above this limit the equaliz is done with:
        mx_limit_ratio_eq = 1.3 ;<- THIS factor
    endelse

    min_limit_ratio    = 0.7 ;<- below this limit the equaliz is done with:
    min_limit_ratio_eq = 0.7 ;<- THIS factor

    ;-- Getting the boxes from the poles:
    dxw  = fix(0.169* sun_p(0) +0.5)
    dyws = fix(0.76 * sun_p(0) +0.5)
    dywb = fix(0.95 * sun_p(0) +0.5)
    tmp1 = data(sun_p(1)-dxw:sun_p(1)+dxw, sun_p(2)-dywb:sun_p(2)-dyws)
    tmp2 = data(sun_p(1)-dxw:sun_p(1)+dxw, sun_p(2)+dyws:sun_p(2)+dywb)
    tmp=[tmp1,tmp2]

    ;-- Calculate the intensity distribution of image:
    h=histogram(tmp,bin=5)*102340L/float(n_elements(tmp))
    xx = findgen(n_elements(h))*5+min(tmp)
    w = where( (xx gt (-500)) and (xx lt 500) )
    ;-- Interpolate a gaussian and calculate the height ratio:
    gauss_ratio = max(gaussfit(xx(w),h(w)))/gf_ref

    ;-- Calcualting the image average contrast:
    w = where( (tmp1 ge (-500)) and (tmp1 le 500), nw)
    if (nw gt 0) then stdv1 = stdev(tmp1(w)) else  stdv1 = stdev(tmp1)
    w = where( (tmp2 ge (-500)) and (tmp2 le 500), nw)
    if (nw gt 0) then stdv2 = stdev(tmp2(w)) else  stdv2 = stdev(tmp)
    stdv_ratio = stdv_r / (stdv1+stdv2) * 2.

    ;-- average ratio:
    avg_ratio = avg([gauss_ratio,stdv_ratio])

    if (verbose) then begin
        print,format='(4X,"Gauss contrast normalization with ratio",f7.4)',$
          gauss_ratio
        print,format='(4X,"Stdeviation normalization with ratio   ",f7.4)',$
          stdv_ratio
        print,format='(4X,"Average ratio =",f7.4)', avg_ratio
        if (avg_ratio gt mx_limit_ratio) then print,$
          format='(8X,"above limit of ",f5.2,"  ==> equalization with'+$
          ' factor", f5.2)', mx_limit_ratio, mx_limit_ratio_eq
        if (avg_ratio lt min_limit_ratio) then print,$
          format='(8X,"below limit of ",f5.2,"  ==> equalization with'+$
          ' factor", f5.2)', min_limit_ratio, min_limit_ratio_eq
    endif

    case 1 of
        (avg_ratio gt mx_limit_ratio) : data = data * mx_limit_ratio_eq
        (avg_ratio lt min_limit_ratio): data = data * min_limit_ratio_eq
        else : data = data * avg_ratio
    endcase
endif

;--- Checks whether in the original image the pixels outside the Sun are
;    set as black (way below 0) or not:
data_nmsk = data
if (avg(data(2:6,2:6)) gt -1500) then begin
    dist_circle, radial_dist, sz(0), sun_p(1), sun_p(2)
    sun_mask = where(radial_dist gt sun_p(0))
    data_nmsk(sun_mask) = -2500.
endif

;--- Saves an unsaturated and unmasked data in variable for retrieval.
data_nmsk = data_nmsk[startx:startx+width-1,starty:starty+height-1]
saturated = where(data_nmsk gt 5000, numbright)
if (numbright gt 0) then begin
    data_nmsk[saturated] = 0
    brightest = max(data_nmsk)
    data_nmsk[saturated] = brightest
endif

;--- Mask out regions outside of the solar disk
; (otherwise, they are dark enough to seem like filaments)
; dist_circle calculates the distance, for each point, from the center
; of a square image of width 'sz(0)' and stores those distances in
; radial_dist
;radius = sun_p(0) * sin(!PI/3)    ;<-- mask out areas >60 deg from the center
radius = sun_p(0) * sin(mask_above);<-- mask out > mask_above deg from center
dist_circle, radial_dist, sz(0), sun_p(1), sun_p(2)
sun_mask = where(radial_dist gt radius)

;--- Reduces the effect of bright AR chromospheric emission (experimental)
if 1 then begin
    tmpdata = data
    tmpdata(sun_mask) = 1
    tmp = smooth(tmpdata,30,/ed)
    ww = where(tmp gt 900, nww)
    if (nww gt 0) then begin
        tmp=smooth(tmpdata,200,/ed)
        data(ww) = data(ww)-tmp(ww)
    endif
endif

;- guarantee that pixels outside the sun will not be filaments
maskeddata = data
maskeddata[sun_mask] = backgr

;--- Remove sunspots from the data:
maskeddata = remove_spots(maskeddata, sun_p, filament_thr=threshold,$
                          spot_thr = -3000, MAX_SPOT_AREA = 2000, $
                          FILL_VALUE=backgr)

; Now create a slightly different version, that has the extra-solar
; pixels brightened (using maskeddata), so that they won't show up as pixels.
small = maskeddata[startx:startx+width-1,starty:starty+height-1]

;--- Apply the median filter:
if (n_elements(med_w) gt 0) then begin
    if (n_elements(med_l) le 0) then med_l = 0
    if verbose then $
      print,format='(4X,"Median filter with box =",I2," and limit =",F8.2)',$
      med_w,med_l
    small = mfilter(small, med_w, med_l, /edges, /silent)
endif

;--- Apply the smoothing algorythm:
if (smth gt 0) then small = smooth(small, smth, /ed)

if verbose then print
return, small

end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: preprocess.pro,v $
; Revision 3.23  2012/04/26 18:21:50  bernapn1
; Added a call to sun_clv that is used for the new Kanzelhoe images that
; have a contrast enhancement towards the limb
;
; Revision 3.22  2011/03/23 19:26:30  bernapn1
; Fixed minor bugs after running code over 2000 to 2011 time span
;
; Revision 3.21  2010/11/17 20:18:06  bernapn1
; Modified algoritm to force Sun to have a given radius.
; Previous version could crash in some cases, and did not consider case with a
; radius larger than the original one.
;
; Revision 3.20  2010/10/27 19:44:58  bernapn1
; Added keywords SUN_RADIUS and OUT_SIZE
; This new version allows to preset a fixed sun radius and image size so that no
; matter what imput sun radius and image size the input image has, the out put
; will always be the same
;
; Revision 3.19  2010/04/29 20:57:16  bernapn1
; Not sure what changed probably something minor
;
; Revision 3.18  2010/04/02 19:11:06  bernapn1
; Removed absolute path in call to gzip
;
; Revision 3.17  2010/03/08 21:21:54  bernapn1
; Improved
;
; Revision 3.16  2010/02/04 19:55:27  bernapn1
; Updated the screen output messages
;
; Revision 3.15  2010/01/25 16:52:32  bernapn1
; run_analyze: Included keep_fits flag
; preprocess:  Now if beyond Sun is NOT dark makes it dark (mostly for KANZ)
; solar_filaments: Changes slightly the size of fonts in z-buffer
; analyze_image: updated
;
; Revision 3.14  2010/01/21 21:51:32  bernapn1
; Added MASK_ABOVE keyword which adds flexibility on how much of the disk to
; mask out for detection of the filaments
;
; Revision 3.13  2008/11/19 17:18:35  bernapn1
; Added some comments
;
; Revision 3.12  2008/11/18 18:56:44  bernapn1
; Fixed several issues with the latest KANZELHOEHE fits headers.
;
; Revision 3.11  2008/11/07 14:42:29  bernapn1
; Updated
;
; Revision 3.10  2004/12/30 22:03:17  pietro
; MAX_SPOT_AREA set to 2000
;
; Revision 3.9  2004/12/30 21:47:30  pietro
; Tweaked the way the normalization is done.
; Added MAX_SPOT_AREA to call to remove_spots.
;
; Revision 3.8  2004/12/30 19:24:21  pietro
; Now getting the default paths from filam_get_paths.
; The intensity equalization ratio is average between gauss and stdev.
;
; Revision 3.7  2004/12/29 16:07:22  pietro
; Introduced a new check of the integrity of the data right after the
; readfits.
;
; Revision 3.6  2004/12/23 20:41:50  pietro
; No actual change
;
; Revision 3.5  2004/12/23 19:29:05  pietro
; Just fuzzing with it ... nothing changed
;
; Revision 3.4  2004/12/23 16:28:26  pietro
; Changed the way the intensity normalization is done: now using a histogram
; distribution of the intensities at the poles.
;
; Revision 3.3  2004/11/23 22:08:44  pietro
; Fixed a problem when readfits detects an error reading and it returns a
; valid file anyway.
;
; Revision 3.2  2004/11/23 21:02:26  pietro
; Now spot_threshold is -3000 (instead of -3200)
;
; Revision 3.1  2004/11/23 20:45:34  pietro
; Changed he way the contrast of the image is determined
;
; Revision 3.0  2004/10/27 14:38:18  pietro
; Given revision 3.0
;
; Revision 2.6  2004/07/22 13:46:29  pietro
; MAde some changes ...
;
; Revision 2.5  2004/07/21 15:09:54  pietro
; Removed stop statement erroneously left from previous version
;
; Revision 2.4  2004/07/21 12:30:54  pietro
; Now processing only EXCLUSIVELY images from BBSO or KANZEHLHOEHE
;
; Revision 2.3  2004/07/16 18:25:40  pietro
; - Now readfits does the decompressing if it is a gzipped file
; - Checks the date and if image is a Kanzelhoehe and before October 6, 2003
;   it flips the image with respect to a vertical axis (to compensate for a
;   problem of the Kanzelhoehe data before that date)
;
; Revision 2.2  2004/07/15 19:57:02  pietro
; Now checks if the gunzip was executed without errors
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.5  2004/06/14 17:11:06  pietro
; Fixed a bug of the verbose variable
;
; Revision 1.4  2004/06/14 16:17:56  pietro
; Use sxpar to extract from fits header ORIGIN parameter
;
; Revision 1.3  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
;    06/08/04 PNB: Added Julian Date B0 and P0 angles output in SUN_PARMS
;        output. Added KEYWORD DATE_STRING.
;    05/12/04 PNB: Added standard deviation normalization. Added
;        rotation of image according to P angle if image is not a bbso
;    05/07/04 PNB: Now if input fits file is in compressed format it
;        uncompresses it before reading it. Recompresses after done.
;    04/30/04 PNB: Added median filter option with keywords MEDIAN_WIDTH
;        and MEDIAN_LIMIT
;    04/23/04 PNB: Now get sun parameters from fits file header and only
;        if the header does not provide them the automatic mode is used.
;    04/08/04 PNB: Added keyword BACKGROUND.
;    Pietro Bernasconi JHU/APL (3/11/04): Now the Sun radius and sun
;        center location is automaticly calculated. Added keyword
;        SUN_PARMS for output solar parameters
;    Pietro Bernasconi JHU/APL (3/2/04): added option to fit curve to
;        data. Removed the display of data, and converted the parameters
;        startx, starty, width, heigh into optional parameters.
;    Pietro Bernasconi JHU/APL: added SMOOTH keyword
;    Written by Kiri Wagstaff JHU/APL:
