;******************************************************************************
;**** PROJECT: Automated Solar Filament Detection and Characterization
;****
;**** NAME = run_analyze
;****
;**** IDL script to download and analyze Big Bear H_alpha images
;**** All what this script does is to set some high level variables and to
;**** launch the IDL routine "analyze_web_image" which is the one that
;**** actually does the entire job.
;**** Once that is done a shell script is lauched to remove older images,
;**** in order to save space in the hard drive.
;**** ! This script is not meant for launching the analysis from within an
;**** IDL session! To do that simply call the function `analyze_web_image'
;**** with the appropriate parameters.
;****
;**** USAGE:
;****    - From the LINUX prompt:
;****        idl < ./run_analyze.pro
;****        idl < ./run_analyze.pro >& log.log &
;****    - From the IDL prompt:
;****        .r run_analyze.pro
;****
;**** Written by Pietro Bernasconi JHU/APL
;******************************************************************************

;--- Setup some internal IDL variables
!quiet = 1
DEVICE,RETAIN=2
DEVICE,DECOMPOSED=1

;******************************************************************************
;**** VARIABLES SETUP *********************************************************

;--- Setting the date for which download the Halpha image. Un-comment
;       the line that provides the date that you whish.
;       date is a 3 element array with: [YYYY,MM,DD]
;start_date = bin_date()   ;<-- Use this to get the latest image
;start_date = [2012,03,29] ;<-- Use this to get an image for a specific date

tmpdate = getenv('START_DATE')
start_date = [strmid(tmpdate,0,4), strmid(tmpdate,4,2), strmid(tmpdate,6,2)]
end_date = start_date     ;<-- Use this to do a single date
;end_date = [2011,03,05]   ;<-- Use this for selecting a range of dates

;--- Set the following flat to 1 if first you want to see if images for the
;    specified date are already on thisk. If yes then it does not go off and
;    checks the remote H-alpha network.
check_disk = 0

;--- Set the following flag to 1 if you want to keep the fits file with the
;    original data downloaded from the remote H-alpha network.
keep_fits = 1

;--- Set the following flag to 1 if you want VoEvent metadata XML files being
;    generated for every filament identified. If yes then you also need to
;    make sure that the 'voevent_path' variable is correctly set 
sv_voevent = 1
allow_voe_update = 0  ;<-- set tp 1 to allow to update already generated VOEs

;--- Set the following flag to 1 if you want to generate an ascii table with
;    "minimal" information about each filament identified
sv_table = 1

;--- Set the following flag to 1 if you want to save a clusters map as well as
;    the current Sun parameters. This data is needed if you want to do
;    filaments tracking.
sv_mask = 1

;--- Set the following flag to 1 if you want to generate a png image
sv_png = 1

;--- Set the following flag to 1 if you want to generate png images for each
;       detected filament
sv_filam_png = 1

;--- Set the following flag to 1 if you want to track filaments from a series
;    of images taken at consecutive days. NOTE in order to do tracking you
;    need to AT LEAST save the past clusters map and Sun parameters
;    information. This is done by setting the do_mask parameter above to 1.
;    If you are analyzing only one Ha image and you do not care of tracking
;    then set it to 0.
do_track = 1

;--- Set the following flag to 1 if you want to merge daily tables into one
;    single large table/dataset.  If you are analyzing only one Ha
;    image and you do not care of merging then set it to 0.
do_merge = 0

;--- Set the following flag to 1 if you want to use the Z buffer. The Z buffer
;       is useful if program is run in background and you do not want to open
;       up a IDL display window.
use_z_buf = 1

;--- Set the following flag to 1 if you want to create an html output for a web
;       site. If yes then you also need to make sure that the 'web_path'
;       variable is correctly set 
do_web_page = 0
web_path = '/project/sbi/www/filaments/'  ;<-- set it if do_web_page = 1

;--- Set the following flag to 1 if you want to start an interactive window
;       where you can manually select the areas in the image to view in detail
;       This will automatically set the use_z_buf to 0
do_interactive = 0

;--- Set the following flag to exit the IDL session once the code is finished
;      running.  WARNING if you launch the code from the UNIX shell then this
;      flag MUST be set to 1. This is useful to be set to 0 basically only
;      when you launch this routine from within an IDL session and you do not
;      want IDL to quit once the routine is done.
do_exit = 1

;--- Location of the IDL programs:
;       Since this is started from the shell it IS NECESSARY! Make sure it is
;       correct!! I should be correctly set when running make
idl_path = '/home/bernapn/filaments/src/idl/'
!path  = expand_path('+'+idl_path) + ':' + !path

;--- Input variables (Uncomment these and change them if the default
;       ones are not good):
home     = filam_get_paths(/home)     ;<-- Home dir Path
img_path = filam_get_paths(/images)   ;<-- dir of images downloaded from ftp
res_path = filam_get_paths(/results)  ;<-- dir of most recent results
voevent_path = filam_get_paths(/voe)  ;<-- dir where VOEvents are stored
c_path   = filam_get_paths(/bin)      ;<-- dir of C and shell scripts


;******************************************************************************
;******************************************************************************
;---- No changes should be necessary below this line --------------------------

cd, home

;------------------------------------------------------------------------------
;-- Checking if IDL is lauched with SSW pachage. It needs to be with SSW if
;   sv_voevent is set to 1:
defsysv, '!SSW', exist=is_there
if (not(is_there) and sv_voevent)then begin &$
    print &$
    print &$
    print,'WARNING: IDL launched without SSW package but sv_voevent is 1!' &$
    print,'         Can not create VOEvent XML files without SSW!' &$
    print,'         SETTING sv_voevent to 0 !!!' &$
    sv_voevent = 0 &$
endif

;-- Set the display device as the z-buffer:
if ( (use_z_buf) and not(do_interactive)) then set_plot,'z'
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;-- Performing the download and analysis of images:
print,'**************************************************************'+$
  '*****************' &$
status = repeat_analysis( start_date, end_date, $
    /download, checkdisk=check_disk, /analyze, track=do_track, $
    filam_data = fil_pars, images_path = img_path, results_path = res_path, $
    do_voevent = sv_voevent, voevent_path = voevent_path, $
    allow_voe_update = allow_voe_update, $
    save_table = sv_table, save_mask = sv_mask, $
    save_png = sv_png, save_filam_png = sv_filam_png, $
    keep_fits = keep_fits, interactive = do_interactive, $
    c_path = c_path, /verbose)
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;---- Checking wether everything went OK or not: ------------------------------
if (status ne 0) then begin &$
  print &$
  print,'!!! NO analysis available!' &$
  print &$
endif else if (do_web_page) then begin &$
  print &$
  print,'Create web page at location: ',web_path  &$
  filam_web_prep,end_date(0:2),web_path  &$
endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

if (do_exit) then begin &$
  print &$
  exit &$
endif

end
;******************************************************************************
