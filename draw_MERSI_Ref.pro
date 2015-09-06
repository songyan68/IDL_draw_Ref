pro Mutil_Plots,EV_TIMEG,SDS_dataTELG,foutDegra=foutDegra,XRange,YRange,xtickname,xticks,position,flag,p1
    
    yticks=5 ;y轴刻度份数
    symsize=0.3
    Thick=2
    tick_size=0.5
   

   ;xstye=1 精确的坐标范围  xticklen 网格线    xticks刻度个数  xtick 线粗细   YTickLen=1,
   ; XTickLen=0.5,XGridStyle=1 网格线

   plot,EV_TIMEG, XRANGE=XRange,YRANGE=YRange, /YNOZERO,$; LINESTYLE=1, $
        XSTYLE=1, XTickLen=0.5,XGridStyle=1, XTICKS=xticks,  XCHARSIZE=tick_size, XTickName=xtickname,$
        YSTYLE=1, YTickLen=0.5,YGridStyle=1, YTICKS=yticks,  YCharSize=tick_size,$ ;YTickFormat='(F8.2)', $ YTITLE=ytitle,
        /NODATA, /NOERASE, POSITION=position, COLOR=1,xthick=3,ythick=3
        
    OPLOT,EV_TIMEG, SDS_dataTELG,PSYM =6,symsize=symsize, Color=4, THICK=Thick
    IF flag eq 0  THEN BEGIN 
      ; idx=()
       OPLOT, EV_TIMEG, foutDegra,LINESTYLE=0, Color=2, THICK=Thick 
    ENDIF 
    
END
FUNCTION TVREAD_ERROR_MESSAGE, theMessage, Traceback=traceback, $
   NoName=noName, _Extra=extra

On_Error, 2

   ; Check for presence and type of message.

IF N_Elements(theMessage) EQ 0 THEN theMessage = !Error_State.Msg
s = Size(theMessage)
messageType = s[s[0]+1]
IF messageType NE 7 THEN BEGIN
   Message, "The message parameter must be a string.", _Extra=extra
ENDIF

   ; Get the call stack and the calling routine's name.

Help, Calls=callStack
callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]

   ; Are widgets supported? Doesn't matter in IDL 5.3 and higher.

widgetsSupported = ((!D.Flags AND 65536L) NE 0) OR Float(!Version.Release) GE 5.3
IF widgetsSupported THEN BEGIN
   IF Keyword_Set(noName) THEN answer = Dialog_Message(theMessage, _Extra=extra) ELSE BEGIN
      IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN answer = Dialog_Message(theMessage, _Extra=extra) ELSE $
         answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + theMessage, _Extra=extra)
   ENDELSE
ENDIF ELSE BEGIN
      Message, theMessage, /Continue, /NoPrint, /NoName, /NoPrefix, _Extra=extra
      Print, '%' + callingRoutine + ': ' + theMessage
      answer = 'OK'
ENDELSE

   ; Provide traceback information if requested.

IF Keyword_Set(traceback) THEN BEGIN
   Help, /Last_Message, Output=traceback
   Print,''
   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
   Print, ''
   FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]
ENDIF

RETURN, answer
END ; ----------------------------------------------------------------------------

FUNCTION TVREAD, xstart, ystart, ncols, nrows, $
   BMP=bmp, $
   Cancel=cancel, $
   Colors=colors, $
   Cube=cube, $
   Dither=dither, $
   _Extra=extra, $
   Filename=filename, $
   GIF=gif, $
   JPEG=jpeg, $
   NoDialog=nodialog, $
   Order=order, $
   PICT=pict, $
   PNG=png, $
   TIFF=tiff, $
   True=true, $
   TYPE=type, $
   Quality=quality, $
   WID=wid

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = TVRead_Error_Message(Traceback=1, /Error)
   IF N_Elements(thisWindow) EQ 0 THEN RETURN, -1
   IF thisWindow GE 0 THEN WSet, thisWindow
   RETURN, -1
ENDIF

cancel = 0

   ; Check for availability of GIF files.

thisVersion = Float(!Version.Release)
IF (thisVersion LT 5.3) OR (thisVersion GE 6.1) THEN haveGif = 1 ELSE haveGIF = 0

   ; Go to correct window.

IF N_Elements(wid) EQ 0 THEN wid =!D.Window
thisWindow = !D.Window
IF (!D.Flags AND 256) NE 0 THEN WSet, wid

   ; Check keywords and parameters. Define values if necessary.

IF N_Elements(xstart) EQ 0 THEN xstart = 0
IF N_Elements(ystart) EQ 0 THEN ystart = 0
IF N_Elements(ncols) EQ 0 THEN ncols = !D.X_Size - xstart
IF N_Elements(nrows) EQ 0 THEN nrows = !D.Y_Size - ystart
IF N_Elements(order) EQ 0 THEN order = !Order
IF N_Elements(true) EQ 0 THEN true = 1
dialog = 1 - Keyword_Set(nodialog)

   ; Do you want to write an image file instead of
   ; capturing an image?
IF N_Elements(type) NE 0 THEN BEGIN
   CASE StrUpCase(type) OF
      'BMP': bmp = 1
      'GIF': gif = 1
      'JPEG': jpeg = 1
      'JPG': jpeg = 1
      'PICT': pict = 1
      'PNG': png = 1
      'TIFF': tiff = 1
      'TIF': tif = 1
      ELSE: Message, 'Cannot write a file of type: ' + StrUpCase(type) + '.'
   ENDCASE
ENDIF
writeImage = 0
fileType = ""
extention = ""
IF Keyword_Set(bmp)THEN BEGIN
   writeImage = 1
   fileType = 'BMP'
   extension = 'bmp'
ENDIF
IF Keyword_Set(gif) THEN BEGIN
   IF havegif THEN BEGIN
      writeImage = 1
      fileType = 'GIF'
      extension = 'gif'
    ENDIF ELSE BEGIN
       ok = Dialog_Message('GIF files not supported in this IDL version. Replacing with JPEG.')
       writeImage = 1
      fileType = 'JPEG'
      extension = 'jpg'
   ENDELSE
ENDIF
IF Keyword_Set(jpeg) THEN BEGIN
   writeImage = 1
   fileType = 'JPEG'
   extension = 'jpg'
ENDIF
IF Keyword_Set(PICT) THEN BEGIN
   writeImage = 1
   fileType = 'PICT'
   extension = 'pict'
ENDIF
IF Keyword_Set(png) THEN BEGIN
   writeImage = 1
   fileType = 'PNG'
   extension = 'PNG'
ENDIF
IF Keyword_Set(tiff) THEN BEGIN
   writeImage = 1
   fileType = 'TIFF'
   extension = 'tif'
ENDIF

IF N_Elements(colors) EQ 0 THEN colors = 256
IF N_Elements(quality) EQ 0 THEN quality = 75
dither = Keyword_Set(dither)

   ; On 24-bit displays, make sure color decomposition is ON.

IF (!D.Flags AND 256) NE 0 THEN BEGIN
   Device, Get_Decomposed=theDecomposedState, Get_Visual_Depth=theDepth
   IF theDepth GT 8 THEN BEGIN
      Device, Decomposed=1
      IF theDepth EQ 24 THEN truecolor = true ELSE truecolor = 0
   ENDIF ELSE truecolor = 0
   IF thisWindow LT 0 THEN $
      Message, 'No currently open windows. Returning.', /NoName
ENDIF ELSE BEGIN
   truecolor = 0
   theDepth = 8
ENDELSE

   ; Get the screen dump. 2D image on 8-bit displays. 3D image on 24-bit displays.

image = TVRD(xstart, ystart, ncols, nrows, True=truecolor, Order=order)

   ; Need to set color decomposition back?

IF theDepth GT 8 THEN Device, Decomposed=theDecomposedState

   ; If we need to write an image, do it here.

IF writeImage THEN BEGIN

      ; Get the name of the output file.

   IF N_Elements(filename) EQ 0 THEN BEGIN
      filename = 'idl.' + StrLowCase(extension)
   ENDIF ELSE BEGIN
      filename = filename + "." + StrLowCase(extension)
   ENDELSE
   IF dialog THEN filename = Dialog_Pickfile(/Write, File=filename)

   IF filename EQ "" THEN BEGIN
      cancel = 1
      RETURN, image
   ENDIF

      ; Write the file.

   CASE fileType OF

      'BMP': BEGIN
         IF truecolor THEN BEGIN
            ; BMP files assume blue, green, red planes.
            temp = image[0,*,*]
            image[0,*,*] = image[2,*,*]
            image[2,*,*] = temp
            Write_BMP, filename, image, _Extra=extra
         ENDIF ELSE BEGIN
            TVLCT, r, g, b, /Get
            Write_BMP, filename, image, r, g, b, _Extra=extra
         ENDELSE
         END

      'GIF': BEGIN
         IF truecolor THEN BEGIN
            CASE Keyword_Set(cube) OF
               0: image2D = Color_Quan(image, 1, r, g, b, Colors=colors, Dither=dither)
               1: image2D = Color_Quan(image, 1, r, g, b, Cube=2 > cube < 6)
            ENDCASE
         ENDIF ELSE BEGIN
            TVLCT, r, g, b, /Get
            image2D = image
         ENDELSE
         Write_GIF, filename, image2D, r, g, b, _Extra=extra
         END

      'JPEG': BEGIN
         IF truecolor THEN BEGIN
            image3D = image
         ENDIF ELSE BEGIN
            s = Size(image, /Dimensions)
            image3D = BytArr(3, s[0], s[1])
            TVLCT, r, g, b, /Get
            image3D[0,*,*] = r[image]
            image3D[1,*,*] = g[image]
            image3D[2,*,*] = b[image]
         ENDELSE
         Write_JPEG, filename, image3D, True=1, Quality=quality, _Extra=extra
         END

      'PICT': BEGIN
         IF truecolor THEN BEGIN
            CASE Keyword_Set(cube) OF
               0: image2D = Color_Quan(image, 1, r, g, b, Colors=colors, Dither=dither)
               1: image2D = Color_Quan(image, 1, r, g, b, Cube=2 > cube < 6)
            ENDCASE
         ENDIF ELSE BEGIN
            TVLCT, r, g, b, /Get
            image2D = image
         ENDELSE
         Write_PICT, filename, image2D, r, g, b
         END

      'PNG': BEGIN
         IF truecolor THEN BEGIN
            Write_PNG, filename, image, _Extra=extra
         ENDIF ELSE BEGIN
            TVLCT, r, g, b, /Get
            image2D = image
            Write_PNG, filename, Reverse(image2D,2), r, g, b, _Extra=extra
            Write_PNG, filename, image2D, r, g, b, _Extra=extra
         ENDELSE
         END

      'TIFF': BEGIN
         IF truecolor THEN BEGIN
            image3D = Reverse(image,3)
         ENDIF ELSE BEGIN
            s = Size(image, /Dimensions)
            image3D = BytArr(3, s[0], s[1])
            TVLCT, r, g, b, /Get
            image3D[0,*,*] = r[image]
            image3D[1,*,*] = g[image]
            image3D[2,*,*] = b[image]
            image3D = Reverse(Temporary(image3D), 3)
         ENDELSE
         Write_TIFF, filename, image3D, 1, _Extra=extra
         END
   ENDCASE
   RETURN, -1
ENDIF

   ; Return the screen dump image.

RETURN, image
END ;-------------------------------------------------------------------------------


PRO  CLOSE_PLOT_DEVICE, DeviceID, FileName=filename
  ;print,'DeviceID',DeviceID
  ; PS 
  IF ( DeviceID EQ 0 ) THEN BEGIN
    Device, /Close_File
  ENDIF
 
  ; Z-Buffer  注释 by songyan 10/12/2014
  IF ( DeviceID EQ 1 ) THEN BEGIN
    IF KEYWORD_SET(FileName) THEN BEGIN
      image = TVRead(Filename=filename, /NoDialog, /PNG)
    ENDIF ELSE BEGIN
      image = TVRead(Filename='ATOVS_ICVS_PLOT', /NoDialog, /PNG)
    ENDELSE
  ENDIF

END


pro Get_XTicks, XMIN, XMAX, xtickname, xticks  
     ;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
    ; Set the X-Tick name string array for all time plot
    xticks_all=10
    xtickname_all=REPLICATE(' ',xticks_all+1) 
  
    dday=(XMAX-XMIN)/xticks_all
    ;dday=ROUND(1.0*(XMAX-XMIN)/xticks_all)
    idday=[0,2,4,6,8,10]
    CalDat,XMIN+dday*idday,MM,DD,YY;,HH,MIN
  
    MM_str=StrTrim(MM,2)
    
    DD_str=StrTrim(DD,2)
    result=WHERE(MM LT 10, count)
    IF ( count GT 0 ) THEN MM_str[result]='0'+MM_str[result]
    result=WHERE(DD LT 10, count)
    IF ( count GT 0 ) THEN DD_str[result]='0'+DD_str[result]
    xtickname_all[idday] = MM_str+'/'+DD_str+'/'+StrTrim(YY,2)
    
    xtickname = xtickname_all
    xticks = xticks_all
    
end

PRO SET_PLOT_DEVICE, DeviceID, Fname_prefix, Title=title, SubTitle=subtitle, Z_RES=z_res, $
                     Tick_SCALE=tick_size, CHAR_SCALE=char_size, COLUMN=Column

  ; z-buffer output
  IF ( DeviceID EQ 1 ) THEN BEGIN
    fname_png=fname_prefix
    
    Set_Plot, 'Z'

    IF KEYWORD_SET(Z_RES) THEN BEGIN
      DEVICE, Set_Resolution=Z_RES
    ENDIF ELSE BEGIN
      DEVICE, Set_Resolution=[900,1200]
    ENDELSE

    TVLCT, 255, 255, 255,   0  ; White
    Erase, Color=0

    !P.CharSize=2.0

    !P.FONT=1             ; Use True Type for title
    title_size=1.5
;    IF KEYWORD_SET(Title) THEN BEGIN
;      XYOuts, 0.5, 0.95,title, /NORMAL, Alignment=0.5, CharSize=title_size, COLOR=1;, CharThick=1.5  
;    ENDIF
;    IF KEYWORD_SET(Subtitle) THEN BEGIN
;      XYOuts, 0.5, 0.02, Subtitle, /NORMAL, Alignment=0.5, CharSize=1.4, COLOR=1;, CharThick=1.5
;    ENDIF
 
    IF KEYWORD_SET(Column) THEN BEGIN
      tick_size=0.4
      char_size=1.0
    ENDIF ELSE BEGIN
      tick_size=0.4
      char_size=1.0
    ENDELSE

    !P.FONT=-1             ; Use Hershy vector for ticks
  
  ENDIF

  TVLCT,   0,   0,   0,   1  ; Black
  TVLCT, 255,   0,   0,   2  ; Red
  TVLCT,  34, 139,  34,   3  ; Green (dark)
  TVLCT,   0,   0, 255,   4  ; Blue
  TVLCT, 160,  92,  45,   5  ; Soil
  TVLCT, 255, 100, 255,   6  ; Light blue
  TVLCT,   0, 255,   0,   7  ; Green
;  TVLCT, 255, 100,   0,   8  ; Red
;  TVLCT, 100, 100,  34,   9  ; Green (dark)
;  TVLCT,   0, 255, 255,   10  ; Blue
;  TVLCT, 255, 255,  45,   11  ; Soil
;  TVLCT, 50, 200, 255,   12  ; Light blue
;TVLCT, 255, 255,  0,   7  ; Yellow
TVLCT,   0, 255, 255,   8  ; Aqua
TVLCT, 128,   0,   0,   9  ; Maroon
TVLCT, 128,   0, 128,  10  ; Purple
TVLCT,   0, 128, 128,  11  ; Teal
TVLCT, 128, 128,   0,  12  ; Olive
TVLCT,   0,   0, 128,  13  ; Navy
TVLCT, 128, 128, 128,  14  ; Grey
TVLCT, 255,   0, 255,  15  ; Fuchsia 
TVLCT, 255, 55, 105,  16  ; Soil
TVLCT, 50, 200, 255,   17  ; Light blue


  IF ( DeviceID NE 0 AND DeviceID NE 1 ) THEN BEGIN
    PRINT, 'Please check the device setting. EXIT !'
    STOP
  ENDIF

END


;2222222222222222222222222222222222222222222222222222222222222
    
pro draw,str_Output_Dir,outName,title,ytitle,EV_TIMEG,SDS_dataTELG,YMIN,YMAX,ch,flag,target,$
foutDegra=foutDegra,ftotal=ftotal,fmum=fmum,fvar=fvar,fA=fA,fk=fk;
           
          thisDevice=!D.Name
          thisFont=!P.Font
          DeviceID=1          ; Set PS output
          z_res = [ 1100, 500]
           
          index=ch
          if ch le 9 then begin
             ch='0'+strtrim(string(ch),2)
          endif else begin
             ch=strtrim(string(ch),2)
          endelse
          if flag eq 1 then begin
              FIGURE_fname = str_Output_Dir+outName+'_band'+ch+'_real'
          endif else begin
              FIGURE_fname = str_Output_Dir+outName+'_band'+ch
          endelse  
        
       ; subtitle='Scan UTC Date'  图像最下面的标题
        
        SET_PLOT_DEVICE, DeviceID, FIGURE_fname, Title=title, SubTitle=subtitle, Z_RES=z_res, $
                         Tick_Scale=tick_size, Char_Scale=char_size  ;Char_Scale=2 设置图例字号变大2倍
        
       ; XYOuts, 0.5, 0.955, update_utc, /NORMAL, Alignment=0.5, CharSize=char_size, Color=1
        Device,  Set_Font='Courier',  /TT_Font
       
        position = [0.15,0.1,0.85,0.9]
        XMAX = max(EV_TIMEG)
        XMIN = min(EV_TIMEG)
    
        XRANGE=[XMIN,XMAX]
        YRANGE=[YMIN,YMAX]
        Get_XTicks, XMIN, XMAX, xtickname, xticks 
        Mutil_Plots,EV_TIMEG,SDS_dataTELG,foutDegra=foutDegra,XRange,YRange,xtickname,xticks,position,flag,p1
        
        XYOuts, 0.5, 0.95,title, /NORMAL, Alignment=0.5, CharSize=1, COLOR=1, CharThick=1.5 
        XYOuts, 0.1, 0.5,ytitle, /NORMAL, Alignment=0.5, CharSize=1, COLOR=1, CharThick=1.5 ,Orientation=90
       
      ;图例绘制
     ; leg=legend(TARGET=[p1], POSITION=[0.1,0.1], /DATA)
    
       if flag eq 0 then begin
          box_y_coords = [0.75, 0.87, 0.87, 0.75,0.75]
          box_x_coords = [0.62, 0.62, 0.845, 0.845, 0.62] 
          PlotS, box_x_coords, box_y_coords, Color=1,/Normal
       endif else begin
          box_y_coords = [0.80, 0.87, 0.87, 0.80,0.80]
          box_x_coords = [0.62, 0.62, 0.75, 0.75, 0.62] 
       endelse
      ; box_x_coords = [0.62, 0.62, 0.84, 0.84, 0.62]      
       
    
       ; !P.FONT=1
       PlotS, 0.64, 0.83, Color=4, Psym=6,SymSize=0.5,/Normal ;Psym=3 点 PSYM =6,symsize=symsize
       XYOutS, 0.72, 0.83, target, Color=1, Size=char_size,Alignment = 0.5, /Normal;
    
    ;画衰减率的图例处理
      if flag eq 0 then begin
         
          count=0
          mflag=0
          for i=0,4 do begin
              strm=strtrim(string(fk[index-1]),2)
              p=strpos(strm,'.')
              l =strmid(strm,p+1,1)
              f =strmid(strm,p-1,1)
              if f ne 0 then begin
                 ENew=strmid(strm,strlen(strm)-5,5)
                 break
              endif else if (f eq 0 ) and  (l eq 0)then begin
                  fk[index-1]=fk[index-1]*10
                  count++
              endif else begin
                 mflag=1
                 fk[index-1]=fk[index-1]*10
                 count++
                 break
              endelse
          endfor
          
          strftotal=strtrim(string(ftotal[index-1]*100),2)
          strftotal=strmid(strftotal,0,5)          
          fmum=strtrim(string(fmum),2)
          fmum=strmid(fmum,0,5)          
          strfvar=strtrim(string(fvar[index-1]),2)
          strfvar=strmid(strfvar,0,5)
          strfA=strtrim(string(fA[index-1]),2)
          strfA=strmid(strfA,0,5)
          strfk=strtrim(string(fk[index-1]),2)
          strfk=strmid(strfk,0,5)
          
          leg1='Degradation='+strftotal+'%(num='+fmum+',var='+strfvar+')'
          
          if mflag eq 1 then begin
             leg2='y='+strfA+'*exp('+strfk+'e-00'+strtrim(string(count),2)+'*d)'
          endif else begin
            leg2='y='+strfA+'*exp('+strfk+ENew+'*d)'
          endelse
          XYOutS, 0.18, 0.83,leg1, COLOR=2, CharSize=char_size, /NORMAL;,Alignment = 0.5
          PlotS,  [0.63,0.66], [0.79,0.79], Color=2,/Normal         
          XYOutS, 0.75, 0.78, leg2, Color=1, Size=char_size,Alignment = 0.5, /Normal
      endif 
      
      CLOSE_PLOT_DEVICE, DeviceID, FileName=FIGURE_fname   
      print,'ch'+ch+' run ok!'  
          
end

pro degradation_rate2,xtime,yr,tk,tA,tcor,tvar
      
      x=-xtime;
    ;  yr=double(yr)
      yr=reform(yr,n_elements(yr))
      y=ALOG(yr);
      temp=TRANSPOSE([[x],[y]])
      R_cor = CORRELATE(temp, /DOUBLE)  ;[, /COVARIANCE] [, /DOUBLE]  ;      [R_cor,P_cor]=corrcoef(x,y);
     ;Result = POLY_FIT( X, Y, Degree) 
      p = POLY_FIT(x,y,1);[, /DOUBLE]  [p,s]=polyfit(x,y,1);直线拟合 
      tcor=R_cor(0,1)  ;cor=R_cor(2,1);
      lnA=p(0);lnA=p(2);
      tA=exp(lnA);A=exp(lnA);
      tk=p(1); k=p(1);
      ; Result = POLY(X, C)
      lnf= poly(x,p);     lnf= polyval(p,x)
      f=exp(lnf);f=exp(lnf);
      residual_y=abs(yr-f);residual_y=abs(yr-f)
      
      tvar=2.0*stddev(residual_y)/mean(f);var=2.0*std(residual_y)/mean(f);
end
function calDegradation,xtime,outRef,F_meas,dtotal_meas,var,A,k
     dim=size(xtime)
      d_meas=dblarr(20)
      A_meas=dblarr(20)
      Cor_meas=dblarr(20)
      var_meas=dblarr(20)
      F_meas=dblarr(20,dim[1])
      dtotal_meas=dblarr(20)
      k=fltarr(20)
      A=fltarr(20)
      R=fltarr(20)
      var=fltarr(20)
      timediff=xtime(dim[1]-1)-xtime(0)
      xtimearr=xtime-xtime(0)+1
    
      for iband=0,19 do begin
          if iband eq 4 then begin
              continue
          endif
          degradation_rate2,xtimearr,outRef(iband,*),tk,tA,tcor,tvar
       
          d_meas(iband)=tk;
          A_meas(iband)=tA;
          Cor_meas(iband)=tcor;  
          var_meas(iband)=tvar; 
          F_meas(iband,*)=tA*exp(-1*tk*xtimearr);    %拟合的反射率
          dtotal_meas(iband)=exp(-1*tk*timediff)-1; %仪器总衰减率  ??????  是不是二维数组
          
          k[iband]=tk
          A[iband]=tA
          R[iband]=tcor
          var[iband]=tvar
      endfor
  return,0
end

function areaFilter,sitestr,outDate,outRef,JulTime,outRefNew
   
   tmpDN_mean=fltarr(20)
   tmpDN_std=fltarr(20)
   timevas=fltarr(20)
   timevas_diff=fltarr(20)   
   outDateNew=''
   outRefNew=fltarr(20,1)
   dim=size(outRef)  
   if dim[2] le 20 then begin
      return,-1
   endif
  ; sitestr ='desert'
  ;沙漠和海洋目标使用目标中心10*10的区域
  ;冰川和白沙目标使用目标中心5*5的区域
   
   for i=0,dim[2]-1 do begin
       if (strcmp(sitestr,'desert') eq 0 or strcmp(sitestr,'ocean') eq 0 )then begin
            nearpixel=10;
        endif  else begin 
            nearpixel=5;
        endelse
        
        if (i le nearpixel) then begin;前 nearpixel条记录
            if i eq 0 then begin
              tmpDN=outRef[*,1:2*nearpixel];第一条 a[0,0:-1]表示整个矩阵
            endif else begin
              tmpDN=[ [outRef[*,0:(i-1)]] , [outRef[*,(i+1):(2*nearpixel)]] ]
            endelse
        endif else if (i gt dim[2]-1-nearpixel) then begin ;后 nearpixel条记录
           
            if i eq dim[2]-1 then begin
              tmpDN=outRef[*,(dim[2]-1-2*nearpixel):(dim[2]-2)];最后一条
            endif else begin
              tmpDN=[ [outRef[*,(dim[2]-1-2*nearpixel):(i-1)]] , [outRef[*,(i+1):(dim[2]-1)]] ];  
            endelse
            
        endif  else begin;中间记录
            tmpDN=[ [outRef[*,(i-nearpixel):(i-1)]] , [outRef[*,(i+1):(i+nearpixel)]] ];
        endelse
        
        
        for p=0,19 do begin
            tmpDN_mean[p]=mean(tmpDN[p,*]);求每列均值
            tmpDN_std[p]=stddev(tmpDN[p,*]);分母/(n-1),求每列标准差
            timevas[p]=abs(outRef[p,i]-tmpDN_mean[p]);
            timevas_diff[p]=timevas[p]-2.0*tmpDN_std[p];           
        endfor
        timevas_diff1=[timevas_diff(0:3),timevas_diff(5:19)];
       
        if (max(timevas_diff1) lt 0) then begin  
            outDateNew=[outDateNew,outDate[i]] 
            outRefNew=[[outRefNew],[outRef[*,i]]]
        endif  
   endfor 
  
   dims=size(outRefNew)
   outDateNew=outDateNew(1:dims[2]-1)  
   outRefNew=outRefNew[*,1:dims[2]-1] 
   
   y= strmid(outDateNew,0,4)           
   m= strmid(outDateNew,4,2)
   d= strmid(outDateNew,6,2)  
   JulTime =julday(m,d,y)
   
   return,0
end
function cloudFliter,Ref_std,Ref_mean,ymd,ref,outDate,outRef
         ;计算协方差CV值          
          dim=size(ref)
          CV=fltarr(20,dim[2])          
          outDate=''
          outRef=fltarr(20,1)
         
          for recNum=0,dim[2]-1 do begin
            for band=0,19 do begin
              if Ref_mean[band,recNum] eq 0 then begin
                CV[band,recNum]=0.0
              endif else  begin
                CV[band,recNum]=Ref_std[band,recNum]*1.0/Ref_mean[band,recNum]
              endelse              
            endfor

            if n_elements(ref[*,recNum]) NE 0 then begin           
              if max(CV[*,recNum]) lt 0.1  and min(ref[*,recNum]) ge 0 then begin ;                
                  outDate=[outDate,ymd[recNum]] 
                  outRef=[[outRef],[ref[*,recNum]]]
              endif              
            endif             
          endfor
          dims=size(outRef)
          outDate=outDate(1:dims[2]-1)  
          outRef=outRef[*,1:dims[2]-1] 
          return,0
end

FUNCTION ISLEAPYEAR,year
  ;判断是不是闰年
  result = 0;
  IF (year MOD 100 EQ 0) THEN BEGIN
    IF (year MOD 400 EQ 0) THEN BEGIN
      result = 1
    ENDIF ELSE BEGIN
      IF (year MOD 4 EQ 0) THEN BEGIN
        result = 1
      ENDIF ELSE BEGIN
      ENDELSE
    ENDELSE
  ENDIF ELSE BEGIN
  ENDELSE
  RETURN,result
END
FUNCTION JULIANDAY,year,month,day
  ;返回一个儒略日
  ;
  index =0
  months_days = [31,28,31,30,31,30,31,31,30,31,30,31]
  IF ISLEAPYEAR(year) EQ 1 THEN months_days[1]=29
  FOR i=0,month-2 DO BEGIN

    index = index + months_days[i]
  ENDFOR
  RETURN, index+day
END

;计算该天是一年中的哪一天    其他方法JULDAY(mon[i],day[i],year[i])  -  JULDAY(那一年的第一天)

function sun_ds2,y,m,d  
  dim=size(y) 
  DSOL=fltarr(dim[2])
  for i=0,dim[2]-1 do begin
    J=JULIANDAY(y[i],m[i],d[i])    
    OM=(0.9856*(J-4))*!pi/180.0                             
    DSOL[i]=1.0/((1.0-0.01673*cos(OM))^2)    
  endfor 
  return,DSOL  
end

function calRef,DN,k0,k1,k2,sz,dss,ref,flag
     
          ;由DN计算Ref
          dim=size(DN)
          ref=fltarr(20,dim[2])
         
          for i=0,19 do begin
            for j=0,dim[2]-1 do begin   
              if flag eq 0 then begin
                  ref[i,j]=(k2(i)*DN[i,j]*DN[i,j]+k1(i)*DN[i,j]+k0[i])/(cos(sz[j])*dss[j])
              endif else begin
                  ref[i,j]=(k2(i,j)*DN[i,j]*DN[i,j]+k1(i,j)*DN[i,j]+k0[i,j])/(cos(sz[j])*dss[j])
              endelse          
              
            endfor
          endfor 
         return,0
end

FUNCTION ReadText, infilename
    ;Get the number of lines
    nlines = FILE_LINES(infilename)   
    OPENR, lun1, infilename, /GET_LUN   
    ;Used to store a line
    tmp_str = ''   
    ;Get columns of the input file
    READF, lun1, tmp_str
    tmp = STRSPLIT(tmp_str, COUNT = col_count)
    POINT_LUN, lun1, 0   
    ;Allocate memory
    data = StrArr(col_count, nlines)   
    row_count = 0L
    WHILE ~EOF(lun1) DO BEGIN
        READF, lun1, tmp_str
        IF ~STRCMP(tmp_str, '') THEN BEGIN           
            data_line = STRSPLIT(tmp_str, /EXTRACT)            
            data[*, row_count] = data_line
            row_count = row_count + 1
        ENDIF       
    ENDWHILE
    FREE_LUN, lun1
    ;RETURN, data[*, 0 : (row_count - 1)]
    RETURN, data
END
function getTarget, txtname,outName,target,sitestr


       index1=strpos(txtname,'\', /REVERSE_SEARCH )
       index2=strpos(txtname,'.', /REVERSE_SEARCH )
       outName=strmid(txtname,index1+1,index2-index1-1)
       
       index3=strpos(txtname,'MERSI',/REVERSE_SEARCH )
       strtemp = strmid(txtname,index3+6)
       
       if stregex(strtemp,'\+') ne -1  then begin
           t1=fix(strpos(strtemp,'-' ))
           t2=fix(stregex(strtemp,'\+'))
           if t1 lt t2 and t1 ne -1 then begin
              idh=t1
           endif else begin
              idh=t2
           endelse       
       endif else if stregex(strtemp,'-' ) ne -1 then begin
           t1=fix(strpos(strtemp,'\+' ))
           t2=fix(stregex(strtemp,'-'))
           if t1 lt t2 and t1 ne -1 then begin
              idh=t1
           endif else begin
              idh=t2
           endelse       
       endif
       
       target=strmid(strtemp,0,idh-1)
       
       
        if  ( stregex(txtname,'Arabia1', /BOOLEAN) ne 0  or $
               stregex(txtname,'Libya1', /BOOLEAN) ne 0 or $          
               stregex(txtname,'Libya4', /BOOLEAN) ne 0 or $
               stregex(txtname,'Tinga', /BOOLEAN) ne 0 or $
               stregex(txtname,'dunhuang', /BOOLEAN) ne 0 or $
               stregex(txtname,'White', /BOOLEAN) ne 0 or $
               stregex(txtname,'Uyuni', /BOOLEAN) ne 0 or $
               stregex(txtname,'Mali', /BOOLEAN) ne 0 or $
               stregex(txtname,'Niger2', /BOOLEAN) ne 0 or $
               stregex(txtname,'Sonora', /BOOLEAN) ne 0 ) then begin  $
               sitestr='desert'   
         endif else if  (stregex(txtname,'Dome', /BOOLEAN) ne 0  or $
               stregex(txtname,'geladandong', /BOOLEAN) ne 0 or $          
               stregex(txtname,'Greenland', /BOOLEAN) ne 0 or $
                stregex(txtname,'puruobingchuan', /BOOLEAN) ne 0 or $
               stregex(txtname,'kunlunfeng', /BOOLEAN) ne 0 ) then begin  $
               sitestr='snow' 
;          
         endif else if  (stregex(txtname,'PacificN1', /BOOLEAN) ne 0  or $
               stregex(txtname,'qinghaihu', /BOOLEAN) ne 0 or $          
               stregex(txtname,'AtlanticN', /BOOLEAN) ne 0 or $
               stregex(txtname,'IndianOcean', /BOOLEAN) ne 0 or $
               stregex(txtname,'MobyLanai', /BOOLEAN) ne 0 ) then begin  $
               sitestr='ocean' 
         endif else begin
               sitestr='ocean' 
         endelse 
         return,0    
         
         
;          endif else if  (stregex(txtname,'Arabia1', /BOOLEAN) ne 0  or $
;               stregex(txtname,'Algeria3', /BOOLEAN) ne 0 or $          
;               stregex(txtname,'Algeria5', /BOOLEAN) ne 0 or $
;               stregex(txtname,'White_Sands', /BOOLEAN) ne 0 ) then begin  $
;               sitestr='cloud'     

end
pro draw_MERSI_Ref
;;;;;;;;;;;;;注意getTarget 函数中文件路径的/ Linux 和Windows的区别 记得修改
;        args=Command_Line_args(Count=c)
;        txtname=args[0]
;        str_Output_Dir=args[1]
;        flag=args[2]
;        
        txtname='D:\songyan\IDL\Libya1\TOA_Ref_FY3C_MERSI_Libya1_+024.420_+013.350_10X10.txt'
        str_Output_Dir = 'D:\songyan\IDL\Libya1\'
        flag=0
;       
        status0=getTarget( txtname,outName,target,sitestr)
        tempdata = ReadText(txtname)
     
        data = tempdata[*,1:*]
        length=size(data,/DIMENSIONS)
        nLines=length(1)
        nsamples=length(0)                         
        DN=fltarr(20,nLines) 
        Ref_mean=fltarr(20,nLines)
        Ref_std=fltarr(20,nLines)
        k0=fltarr(20,nLines)
        k1=fltarr(20,nLines)
        k2=fltarr(20,nLines)
        sz=fltarr(nLines)
        
        for i=0,19 do begin          
          DN[i,*]=data[2+i*10,*]
          k0[i,*]=data[8+i*10,*]
          k1[i,*]=data[9+i*10,*]
          k2[i,*]=data[10+i*10,*] 
          if flag eq 0 then begin
            Ref_mean[i,*]=data[4+i*10,*]
            Ref_std[i,*]=data[5+i*10,*]
          endif else begin
            Ref_mean[i,*]=data[6+i*10,*]
            Ref_std[i,*]=data[7+i*10,*]
          endelse         
        endfor 
       
     
        sz[*]=data(nsamples-1,*)/100.0
        sz=sz*(!pi/180)
       ;计算日地距离的平方
        ymd=data[0,*]
        year= strmid(ymd,0,4)           
        month= strmid(ymd,4,2)
        day= strmid(ymd,6,2)  
        dss=sun_ds2(year,month,day)
       
         
         if flag eq 0 then begin
            k0=k0[*,0]
            k1=k1[*,0]
            k2=k2[*,0]
         endif
         k0=k0/100.0
         k1=k1/100.0
         k2=k2/100.0
         
          ;(2)计算反射率Ref   由DN计算Ref
         status2=calRef(DN,k0,k1,k2,sz,dss,ref,flag) 
         ;(3)云去除算法(基于反射率的数据)
         status3=cloudFliter(Ref_std,Ref_mean,ymd,ref,outDate,outRef)
       ;  print,ref
         ;(4)利用相邻时间像元的值进行对比        
         status4=areaFilter(sitestr,outDate,outRef,JulTime,outRefNew)
         if status4 eq -1 then begin
         print,'data records are not enough!'
            return
         endif
         
         if flag eq 0 then begin
            ;(5)计算衰减率
             status5=calDegradation(JulTime,outRefNew,F_meas,dtotal_meas,var,A,k)            
         endif
         
         ;(5)Ref绘图
         ytitle='TOA Reflectlance'
         dim=size(outRefNew)
         ymin=0
         ymax=1
         for i=0,19 do begin
              if i eq 4 then begin
                  continue
              endif
              title='FY3C MERSI band'+strtrim(string(i+1),2)
              if flag eq 0 then begin
                  draw,str_Output_Dir,outName,title,ytitle,JulTime,outRefNew[i,*],YMIN,YMAX,i+1,flag,target,$
                  foutDegra=F_meas[i,*],ftotal=dtotal_meas,fmum=dim[2],fvar=var,fA=A,fk=k
              endif else begin
                  draw,str_Output_Dir,outName,title,ytitle,JulTime,outRefNew[i,*],YMIN,YMAX,i+1,flag,target,$
                  foutDegra=foutDegra,ftotal=ftotal,fmum=fmum,fvar=fvar,fA=fA,fk=fk
              endelse
           
         
         endfor
    
end