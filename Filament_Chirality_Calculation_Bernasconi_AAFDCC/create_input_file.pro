;******************************************************************************
;***                                                                        ***
      pro create_input_file, filenam, image, IN_PATH=IN_PATH, $
        OUT_PATH=OUT_PATH, VOE_PATH=VOE_PATH, MASK_DEG = maks_deg,$
        I_THRSH = I_THRSH, U_THRSH = U_THRSH, $
        MIN_AREA=MIN_AREA, MAX_DIST=max_dist,$
        DISP_DIM=DISP_DIM, START_X=START_X, START_Y=START_Y, WIDTH=WIDTH, $
        HEIGHT=HEIGHT, SMTH_IMG=SMTH_IMG, BARB_ANG=BARB_ANG, $
        DO_VOE=DO_VOE, VOE_UPDT = ALLOW_VOE_UPDATE, $
        DO_TRACK=DO_TRACK, FIX_BGR=FIX_BGR, $
        SHOW_PIX=SHOW_PIX, SHOW_BBX=SHOW_BBX, $
        SHOW_BND=SHOW_BND, SHOW_SPN=SHOW_SPN, $
        SAVE_TAB = SAVE_TAB, SAVE_MSK = SAVE_MSK, $
        SAVE_PNG=SAVE_PNG, SAVE_FIL = SAVE_FIL, SAVE_DAT=SAVE_DAT
;***                                                                        ***
;******************************************************************************
;+
; NAME:
;    CRETAE_INPUT
;
; PURPOSE:
;    This procedure creates an ASCII file for the program solar_filaments.
;    It is particularely useful when is needed to perform multiple calls to
;    solar_filaments with the same value for most of the input parameters.
;
; CALLING SEQUENCE:
;    create_input_file, filename, image [, IN_PATH=in_path, $
;       OUT_PATH=out_path, VOE_PATH=VOE_PATH, MASK_DEG=MASK_DEG, $
;       I_THRSH = I_THRSH, U_THRSH = U_THRSH, $
;       MIN_AREA=MIN_AREA, MAX_DIST=max_dist, $
;       DISP_DIM=DISP_DIM, START_X=START_X, START_Y=START_Y, WIDTH=WIDTH, $
;       HEIGHT=HEIGHT, DO_VOE=DO_VOE, FIX_BGR=FIX_BGR, SHOW_PIX=SHOW_PIX, $
;       SHOW_BBX=SHOW_BBX, SHOW_BND=SHOW_BND, SHOW_SPN=SHOW_SPN, $
;       SAVE_PNG=SAVE_PNG, SAVE_FIL=SAVE_FIL, SAVE_TAB=SAVE_TAB ]
;
; INPUTS:
;    filename = string: Name of generated file with the parameters to feed
;                 to the program solar_filaments.
;    image    = string: Name of the fits file with the Halpha image to be
;                 processed
;
; OPTIONAL INPUT PARAMETERS:
;	None
;
; KEYWORD PARAMETERS:
;    IN_PATH = string: Path where the image is localed
;    OUT_PATH= string: Path where the result table will be posted
;    VOE_PATH= string: Path where the VOEvent XML files are posted
;    MASK_DEG= float : Mask out areas of Sun above mask_deg deg from center
;    I_THRSH = int   : Threshold below which the filament is identifyed
;    U_THRSH = int   : Upper threshol up to with the filament is grown 
;    MIN_AREA= int   : Minimum area of a filament, in pixels
;    DISP_DIM= float : Fraction the full image size, for display purposes
;    START_X = int   : x coord of start point for window-box to analyze
;    START_Y = int   : y coord of start point for window-box to analyze
;    WIDTH   = int   : Width of window-box to analyze
;    HEIGHT  = int   : Height of window-box to analyze
;    SMTH_IMG= int   : Smoothing of image in pixels
;    BARB_ANG= float : Minimum angle deviation from 90deg of barb to be ident.
;    MAX_DIST= int   : Maximum filam endpts distance to consider then the same
;    DO_VOE  = flag  : If set then generates VOEvent XML files
;    VOE_UPDT= flag  : If set it allows to updage VOEs already generated
;    DO_TRACK= flag  : If set then tracks filaments in the future
;    FIX_BGR = flag  : Fixes the image background to remove non uniformities
;    SHOW_PIX= flag  : Highlight all pixels below the threshold in red
;    SHOW_BBX= flag  : Draw the bounding box enclosing the filament in red
;    SHOW_BND= flag  : Draw the boundary of each filament in yellow
;    SHOW_SPN= flag  : Draw the spine of each filament in cyan
;    SAVE_TAB= flag  : Saves an ASCII table with each filament stats
;    SAVE_MSK= flag  : Saves the clusters map and the sun_p in idl save file
;    SAVE_PNG= flag  : Saves the window in as a pgn file
;    SAVE_FIL= flag  : Saves for each filament a separate png image
;
; OUTPUTS:
;    The file named <filename> with all the parameters for solar filaments
;
; OPTIONAL OUTPUT PARAMETERS:
;	None
;
;-
; MODIFICATION HISTORY: See at end of file
;
;******************************************************************************
if (n_params() lt 2) then $
  message,'missin one of the 2 required inut paremeters!'

openw,u,filenam,/get_lun

printf,u,';*** Input file for solar_filaments program'
printf,u
printf,u,'IMAGE   = '+image

;--- Add all the optional parameters if given: ---
if (n_elements(IN_PATH) gt 0) then $
  printf,u,'IN_PATH = '+IN_PATH

if (n_elements(OUT_PATH) gt 0) then $
  printf,u,'OUT_PATH= '+OUT_PATH

if (n_elements(VOE_PATH) gt 0) then $
  printf,u,'VOE_PATH= '+VOE_PATH

if (n_elements(MASK_DEG) gt 0) then $
  printf,u,'MASK_DEG=',MASK_DEG

if (n_elements(I_THRSH) gt 0) then $
  printf,u,'I_THRSH =',I_THRSH

if (n_elements(U_THRSH) gt 0) then $
  printf,u,'U_THRSH =',U_THRSH

if (n_elements(MIN_AREA) gt 0) then $
  printf,u,'MIN_AREA= ',MIN_AREA

if (n_elements(DISP_DIM) gt 0) then $
  printf,u,'DISP_DIM= ',DISP_DIM

if (n_elements(START_X) gt 0) then $
  printf,u,'START_X = ',START_X

if (n_elements(START_Y) gt 0) then $
  printf,u,'START_Y = ',START_Y

if (n_elements(WIDTH) gt 0) then $
  printf,u,'WIDTH   = ',WIDTH

if (n_elements(HEIGHT) gt 0) then $
  printf,u,'HEIGHT  = ',HEIGHT

if (n_elements(SMTH_IMG) gt 0) then $
  printf,u,'SMTH_IMG= ',SMTH_IMG

if (n_elements(BARB_ANG) gt 0) then $
  printf,u,'BARB_ANG= ',BARB_ANG

if (n_elements(MAX_DIST) gt 0) then $
  printf,u,'MAX_DIST= ',MAX_DIST

if (n_elements(FIX_BGR) gt 0) then $
  printf,u,'FIX_BGR = ',FIX_BGR

if (n_elements(SHOW_PIX) gt 0) then $
  printf,u,'SHOW_PIX= ',SHOW_PIX

if (n_elements(SHOW_BBX) gt 0) then $
  printf,u,'SHOW_BBX= ',SHOW_BBX

if (n_elements(SHOW_BND) gt 0) then $
  printf,u,'SHOW_BND= ',SHOW_BND

if (n_elements(SHOW_SPN) gt 0) then $
  printf,u,'SHOW_SPN= ',SHOW_SPN

if (n_elements(DO_VOE) gt 0) then $
  printf,u,'DO_VOE  = ',DO_VOE

if (n_elements(DO_VOE) gt 0) then $
  printf,u,'VOE_UPDT= ',ALLOW_VOE_UPDATE

if (n_elements(DO_TRACK) gt 0) then $
  printf,u,'DO_TRACK= ',DO_TRACK

if (n_elements(SAVE_TAB) gt 0) then $
  printf,u,'SAVE_TAB= ',SAVE_TAB

if (n_elements(SAVE_MSK) gt 0) then $
  printf,u,'SAVE_MSK= ',SAVE_MSK

if (n_elements(SAVE_PNG) gt 0) then $
  printf,u,'SAVE_PNG= ',SAVE_PNG

if (n_elements(SAVE_FIL) gt 0) then $
  printf,u,'SAVE_FIL= ',SAVE_FIL

if (n_elements(SAVE_DAT) gt 0) then $
  printf,u,'SAVE_DAT= ',SAVE_DAT

close,u
free_lun,u
end
;******************************************************************************

; MODIFICATION HISTORY:
; $Log: create_input_file.pro,v $
; Revision 3.5  2010/05/06 14:33:49  bernapn1
; Several changes and improvements.  Now using a new technique to track
; filaments
;
; Revision 3.4  2010/04/29 20:53:15  bernapn1
; Added keywords DO_TRACK and SAVE_MSK
;
; Revision 3.3  2010/03/26 19:07:36  bernapn1
; Added logic to handle new input parameter SAVE_FIL
;
; Revision 3.2  2010/01/21 21:46:27  bernapn1
; Added flexibility by introducing new input tags: MASK_DEG, DO_VOE, and
; VOE_PATH. Thi also adapts the code to allow output of VOEvents in XML format
;
; Revision 3.1  2009/12/18 21:52:24  bernapn1
; Improved fainter filaments detection
; Improved connection of nearby filaments
; Added MAX_DIST paramenter in input file (adds more flexibility)
;
; Revision 3.0  2004/10/27 14:38:17  pietro
; Given revision 3.0
;
; Revision 2.1  2004/07/02 13:22:44  pietro
; ADDED keyword BARB_ANG
;
; Revision 2.0  2004/06/16 21:18:30  pietro
; release 2.0
;
; Revision 1.2  2004/06/11 15:17:45  pietro
; Added log tag at end
;
;
; Written by Pietro N. Bernasconi JHU/APL: April 5, 2004
