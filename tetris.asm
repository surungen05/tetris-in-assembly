.model small
.stack 64  
.data
    ; karelerin adreslemeleri ust sol kose (1,1)
    ; alt sag kose  (10, 16) olacak
   
    ; yandaki gelecek cisimlerin adreslemeleri de
    ; 1. icin ust sol kosesi (20, 5)
    ; 2. icin ust sol kosesi (20, 12) olacak
   
    ; satir, sutundan koordinat donusumu
    ;  yatay eksen icin (sutun-1)*12 + 100
    ;  dusey eksen icin (satir-1)*12 + 4
    ekran_baslangic_sutunu  dw   100
    ekran_baslangic_satiri  dw   4
    ekran_bitis_sutunu dw   220
    ekran_bitis_satiri dw   196    
   
    blok_baslangic_sutunu dw 208
    blok_baslangic_satiri dw 184
    blok_bitis_sutunu dw 220
    blok_bitis_satiri dw 196
    blok_yuksekligi   dw 12
    blok_genisligi    dw 12            

    eldeki_cisim dw ?
    bakilan_satir dw ?
    cisim_sutunu dw ?
    cisim_satiri dw ?
    yeri_bos_mu_sonuc db ?  
   
    gidecek_cisim dw ?
    cismin_gittigi_satir dw ?
    cismin_gittigi_sutun dw ?
   
    dolgu_satir dw ?
    dolgu_sutun dw ?
    dolgu_veri dw ?
   
    blok_sutunu dw ?
    blok_satiri dw ?
   
    ekran_sutunu dw ?
    ekran_satiri dw ?
    satir dw ?
    sutun dw ?
   
    alan dw ?  
   
    cisim dw ?
   
    aktif_cisim dw ?  
    ;        1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    16
    ekran dw 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024
 
    dolu_satirlar dw -1, -1, -1, -1 ; -1 silinecek satir yok anlaminda
    dolu_satir_adedi dw 0
    silinecek_satir dw ?
   
    cisim_rengi db 4H
    arkaplan_rengi db 8H
    gecici_renk db 4H
    smart_colour db ?
    cisim_sinir_rengi db ?  
    siradaki_1nci_cisim dw ?
    siradaki_2nci_cisim dw ?
    siradaki_1nci_cisim_rengi db ?
    siradaki_2nci_cisim_rengi db ?
    siradaki_1nci_cisim_rastgele_sayisi db ?
    siradaki_2nci_cisim_rastgele_sayisi db ?  
    pozisyon db 1H
    shift_counter dw 0H
    rastgele_cisim_sayisi db ?    
    saniyeler db 0
    yeni_saniye db 0
    eski_saniye db 0
    satir_dolu_mu db 0       
    bekleme_sayaci dw 0
    skor dw 0                  ;skor su anlik yok
    msg_skor db "skor:0000$"
   
.code
main proc far
    mov ax, @data
    mov ds, ax
    call ekrani_temizle
    call grafik_moda_gec
    call arkaplani_ciz
    call siniri_ciz    
    mov cisim_sinir_rengi, 0H
    mov bh, 1          
    call rastgele_sayi_uretim_basla
    call bekleme_2
    call rastgele_cisim_uret
             
      mov  ah, 2ch
      int  21h      ;saniyeler cinsinden dh icine dondurdu
      mov eski_saniye, dh

    oyun_dongusu:    
    call klavye_islemleri
;    call klavyeden_girdi_al
     mov  ah, 2ch
      int  21h      ;saniyeler cinsinden dh icine dondurdu
      cmp  dh, eski_saniye  ;Saniyeler degismemisse
      jne asagi_kaydir 
    jmp oyun_dongusu
     
    asagi_kaydir:
    mov eski_saniye, dh

    call sekli_asagi_kaydir
   
    jmp oyun_dongusu
   
    oyun_sonu:        
        mov cisim_sinir_rengi, 0H
        call siniri_ciz
    mov ax, 4C00h
    int 21h        

    ret
main endp
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bekleme proc  
beklemede:  
;Sistemden zamani al
  mov  ah, 2ch
  int  21h      ;saniyeler cinsinden dh icine dondurdu
;Saniyeleri dondurdugunu kontrol et
  cmp  dh, saniyeler  ;Saniyeler degismemisse
  je   beklemede     ;    ayni saniye icinde
  mov  saniyeler, dh  ;saniye degismis yeni saniyeyi sakla
  ret
bekleme endp

bekleme_2 proc
    push cx
   
    mov bekleme_sayaci, 1
bekleme_dongu1:
    mov cx, 0FFFFH
    inc bekleme_sayaci
bekleme_dongu2:
    loop bekleme_dongu2
    cmp bekleme_sayaci, 5
    jnz bekleme_dongu1
   
    pop cx
    ret
endp bekleme_2

klavyeden_girdi_al proc
    push cx
    mov bekleme_sayaci, 1
bekleme_dongu3:
    mov cx, 0FFFFH
    inc bekleme_sayaci
bekleme_dongu4:
    call klavye_islemleri
    loop bekleme_dongu4
    cmp bekleme_sayaci, 5
    jnz bekleme_dongu3
    pop cx
    ret
endp klavyeden_girdi_al  

; cisim 4x4luk ve bitwise
; ekran uzerindeki karesel bolgelerin bit duzeyindeki karsiligi (array)
; yapilmasi gereken bit duzeyinde birebir karsilastirma  
; basite indirgemek icin cisim degiskeni uzerinden satir satir ayristirip ekrandaki karsiliklari ile
; kontrol etmek
yeri_bos_mu proc
   
    push ax
    push bx
    push cx
    push dx
   
    push si
   
    mov cx, cismin_gittigi_sutun
    mov cismin_gittigi_sutun, cx
    dec cx  
    mov dx, cismin_gittigi_satir
    mov cismin_gittigi_satir, dx  
   
   
    push ax
    mov ax, cismin_gittigi_satir
    dec ax
    mov bx, 2
    mul bx
    mov si, ax
    pop ax

   
    ; x x x   1 + 2 + 4 + 32
    ;   x  
    ;
    ;
                         
    ;    1    2     4     8                
    ;   16   32    64   128
    ;  256  512  1024  2048
    ; 4096 8192 16384 32768
   
       
    birinci_satir:                                      
    mov bx, gidecek_cisim
    and bx, 15
    cmp bx, 0
    je ikinci_satir
    mov satir, bx  
    mov bx, satir
   

    mov dx, ekran[si]
    mov ekran_satiri, dx
     
    shr ekran_satiri, cl
    and bx, ekran_satiri
    cmp bx, 0
    jne dolu_cikis
   
    ikinci_satir:
    mov bx, gidecek_cisim
    shr bx, 4
    and bx, 15
    cmp bx, 0
    je ucuncu_satir
   
    mov satir, bx
   

    mov dx, ekran[si+2]
    mov ekran_satiri, dx
   
    shr ekran_satiri, cl
    and bx, ekran_satiri
    cmp bx, 0
    jne dolu_cikis
   
    ucuncu_satir:
    mov bx, gidecek_cisim
    shr bx, 8
    and bx, 15
    cmp bx, 0
    je dorduncu_satir
    mov satir, bx
   

    mov dx, ekran[si+4]
    mov ekran_satiri, dx
   
    shr ekran_satiri, cl
    and bx, ekran_satiri
    cmp bx, 0
    jne dolu_cikis
   
    dorduncu_satir:
    mov bx, gidecek_cisim
    shr bx, 12
    and bx, 15
    cmp bx, 0
    je bos_cikis
    mov satir, bx
   
 
    mov dx, ekran[si+6]
    mov ekran_satiri, dx  
     
    shr ekran_satiri, cl
    and bx, ekran_satiri
    cmp bx, 0
    jne dolu_cikis    
    jmp bos_cikis
   
    bos_cikis:
    mov yeri_bos_mu_sonuc, 1
    mov bh, yeri_bos_mu_sonuc
    jmp yeri_bos_mu_cikis
     
    dolu_cikis:
    mov yeri_bos_mu_sonuc, 0
    mov bh, yeri_bos_mu_sonuc
    jmp yeri_bos_mu_cikis
   
    yeri_bos_mu_cikis:
   
    pop si
   
    pop dx
    pop cx
    pop bx
    pop ax
    ret
endp yeri_bos_mu

cismi_ekrana_teslim_et proc    
     push ax
     push bx
     push cx
     push dx
     
     push si
     push di
     
     mov cisim_rengi, 3H
     mov al, cisim_rengi
     call sekil_ciz
     
     mov bx, aktif_cisim
     mov eldeki_cisim, bx
     mov dx, cisim_satiri
     mov bakilan_satir, dx
                           
     mov si, bakilan_satir
     add si, bakilan_satir ; word oldugu icin  
     
     sub si, 2
     
     mov cx, cisim_sutunu  
     dec cx
                             
     and bx, 15
     shl bx, cl          
     
     shr eldeki_cisim, 4  
     
     mov di, 0
               
     mov dolu_satir_adedi, 0
                 
teslime_devam:
     or bx, ekran[si]
     mov ekran[si], bx
     
     mov bx, eldeki_cisim
     and bx, 15
     shl bx, cl  

     shr eldeki_cisim, 4
     
     cmp ekran[si], 2047
     jne dolmamis
             
     mov dolu_satirlar[di], si
                 
               
     add di, 2
     mov dolu_satir_adedi, di
     
     dolmamis:              
     
     inc dx
     add si,2
       
     cmp bx, 0
     
     jne teslime_devam
     ; her parca tesliminde de satir kontrol edilirse ayrica kontrole gerek kalmaz
     ; ama kontrol edilen satirlari tutup sonra hepsini silmek lazim
     ;
     ; bunun icin dolmus_satirlar isimli 4 boyutlu bir degisken tanimlanabilir
     call dolu_satirlari_sil
       
             
     pop di
     pop si
     
     pop dx
     pop cx
     pop bx
     pop ax
     
     ret
endp cismi_ekrana_teslim_et                              

dolu_satirlari_sil proc
    push si
    push di
       
    mov di, 0
 
    cmp dolu_satir_adedi, 0
    je dolu_sat_sil_cik
   
satir_sil:
    mov si, dolu_satirlar[di]
    mov silinecek_satir, si
   
   
    call satiri_sil
   
    add di, 2
   
    cmp di, dolu_satir_adedi
    jl satir_sil
     
    mov dolu_satir_adedi, 0
                                       
    call ekrani_yeniden_ciz
   
dolu_sat_sil_cik:    
    pop di
    pop si

    ret
endp dolu_satirlari_sil
   
satiri_sil proc
    push di
    push bx

   mov di, silinecek_satir

sil_dongu:
   cmp di, 0
   je satiri_sil_cikis
   
   mov bx, ekran[di-2]
   mov ekran[di], bx
   
   sub di,2
   jmp sil_dongu

satiri_sil_cikis:
   mov ekran[0], 1024          
   
   pop bx      
   pop di
   
   ret
endp satiri_sil                                    


ekrani_temizle proc
    push ax
    push bx
    push cx
    push dx
   
    mov al, 06h   ; asagi kaydir    
    mov bh, 00h
    mov cx, 0000h    ;satir 00 | sutun 00
    mov dx, 184Fh  
   
    pop dx
    pop cx
    pop bx
    pop ax
    ret      
endp ekrani_temizle


grafik_moda_gec proc
    push ax
   
    mov ah, 00h
    mov al, 13h
    int 10h
   
    pop ax        
    ret  
endp grafik_moda_gec  

arkaplani_ciz proc
    push ax
    push cx
    push dx
   
    mov ah, 0ch
    mov al, arkaplan_rengi
    mov dx, ekran_baslangic_satiri  
loop1:
    mov cx, ekran_baslangic_sutunu
loop2:
    int 10h
    inc cx
    cmp cx, ekran_bitis_sutunu
    jnz loop2
    inc dx
    cmp dx, ekran_bitis_satiri
    jnz loop1
   
    pop dx
    pop cx
    pop ax    
    ret
endp arkaplani_ciz  

gelecek_paneli_temizle proc
    mov ah, 0ch
    mov al, 0h
    mov dx, 40  
gelecek_paneli_temizle_dongu1:
    mov cx, 256
gelecek_paneli_temizle_dongu2:
    int 10h
    inc cx
    cmp cx, 304
    jnz gelecek_paneli_temizle_dongu2
    inc dx
    cmp dx, 76
    jnz gelecek_paneli_temizle_dongu1
    mov ah, 0ch
    mov al, 0h
    mov dx, 136  
gelecek_paneli_temizle_dongu3:
    mov cx, 256
gelecek_paneli_temizle_dongu4:
    int 10h
    inc cx
    cmp cx, 304
    jnz gelecek_paneli_temizle_dongu4
    inc dx
    cmp dx, 172
    jnz gelecek_paneli_temizle_dongu3      
    ret  
endp gelecek_paneli_temizle  

proc ekrani_yeniden_ciz
    call arkaplani_ciz
    call siniri_ciz
   
    mov si, 0
    mov satir, 1
    mov cisim_rengi, 3h
    satir_dongusu:
        mov sutun, 1
 
        mov dx, ekran[si]      
        mov ekran_satiri, dx
       
        sutun_dongusu:  
            mov dx, ekran_satiri        
           
            and dx, 1
           
            shr ekran_satiri,1
           
            cmp dx, 0
           
            je sonraki_alan
                           
            mov bx, satir                
            mov blok_satiri, bx
            mov bx, sutun
            mov blok_sutunu, bx                          
            call tek_blok_ciz
           
           
            sonraki_alan:
           
            inc sutun
       
        cmp sutun, 11
        jl sutun_dongusu            
        inc satir
        add si, 2                  
    cmp satir, 17
    jl satir_dongusu

    ret    
endp ekrani_yeniden_ciz
;;;;;;;;;;;;;;;;;;
tek_blok_ciz proc
    call koordinat_donustur
   
    mov ah, 0ch
    mov al, cisim_rengi
             
    inc blok_baslangic_sutunu
    inc blok_baslangic_satiri
    dec blok_bitis_sutunu
    dec blok_bitis_satiri    

    mov dx, blok_baslangic_satiri
   
loop3:
    mov cx, blok_baslangic_sutunu
loop4:
    int 10h
    inc cx
    cmp cx, blok_bitis_sutunu
    jnz loop4
    inc dx
    cmp dx, blok_bitis_satiri
    jnz loop3
    dec blok_baslangic_sutunu
    dec blok_baslangic_satiri
    inc blok_bitis_sutunu
    inc blok_bitis_satiri
    ret  
endp tek_blok_ciz

;;;;;;;;;;;;;;;;;;

koordinat_donustur proc
;   blok_baslangic_sutunu = (blok_sutunu-1) * 12 + ekran_baslangic_sutunu
;   blok_baslangic_satiri = (blok_satiri-1) * 12 + ekran_baslangic_satiri
   
;   blok_bitis_sutunu = blok_baslangic_sutunu + 12
;   blok_bitis_satiri = blok_baslangic_satiri + 12
    push ax
    push bx
    push cx
    push dx


    mov cx, blok_sutunu
    dec cx
    mov bx, blok_genisligi
    push ax
    mov ax, bx
    mul cx
    mov bx, ax
    pop ax  
    mov cx, bx
   
 
    add cx, ekran_baslangic_sutunu
    mov blok_baslangic_sutunu, cx

    mov dx, blok_satiri
    dec dx
    mov bx, blok_yuksekligi
    push ax
    mov ax, dx
    mul bx
    mov dx, ax
    pop ax
   
    add dx, ekran_baslangic_satiri
    mov blok_baslangic_satiri, dx
 
    mov cx, blok_baslangic_sutunu
    add cx, blok_genisligi
    mov blok_bitis_sutunu, cx
   
    mov dx, blok_baslangic_satiri
    add dx, blok_yuksekligi
    mov blok_bitis_satiri, dx
   
    pop dx
    pop cx
    pop bx
    pop ax              
    ret        
endp koordinat_donustur


tek_blok_sil proc
   
    call koordinat_donustur
   
    mov ah, 0ch
    mov al, arkaplan_rengi
   
   
    inc blok_baslangic_sutunu
    inc blok_baslangic_satiri
    dec blok_bitis_sutunu
    dec blok_bitis_satiri    

    mov dx, blok_baslangic_satiri
   
loop3_sil:
    mov cx, blok_baslangic_sutunu
loop4_sil:
    int 10h
    inc cx
    cmp cx, blok_bitis_sutunu
    jnz loop4
    inc dx
    cmp dx, blok_bitis_satiri
    jnz loop3
    dec blok_baslangic_sutunu
    dec blok_baslangic_satiri
    inc blok_bitis_sutunu
    inc blok_bitis_satiri
    ret    
endp  tek_blok_sil

klavye_islemleri proc
    push ax
    push bx
   
    mov ah, 01h
    int 16h
    jz arrow_exit
    mov ah, 00h
    int 16h
    cmp al, 'd'
    je sekli_saga_kaydir_label
    cmp al, 'a'
    je  sekli_sola_kaydir_label
    cmp al, 'w'
    je sekli_dondur_label
    cmp al, 's'
    je sekli_asagi_kaydir_label
    cmp al, 'p'
    je ekrani_ciz_label

    jmp arrow_exit
   
sekli_saga_kaydir_label:
         
    call sekli_saga_kaydir
    jmp arrow_exit
sekli_sola_kaydir_label:

    call sekli_sola_kaydir
    jmp arrow_exit                        
sekli_dondur_label:          

    call sekli_dondur
    jmp arrow_exit
sekli_asagi_kaydir_label:
    call sekli_asagi_kaydir
    jmp arrow_exit
ekrani_ciz_label:
    call ekrani_yeniden_ciz
    jmp arrow_exit
   
arrow_exit:

    pop bx
    pop ax
    ret
endp klavye_islemleri    

rastgele_sayi_uretim_basla proc
   MOV AH, 00h  ; sistem zamanini almak icin interrupt        
   INT 1AH      ; CX:DX saat tiklerinin sayisini tutar      
   mov  ax, dx
   xor  dx, dx
   mov  cx, 7  
   div  cx       ;dx bolmeden kalanlari tutar
   mov siradaki_1nci_cisim_rastgele_sayisi, dl
   call bekleme_2
   MOV AH, 00h  ; sistem zamanini almak icin interrupt        
   INT 1AH      ; CX:DX saat tiklerinin sayisini tutar      
   mov  ax, dx
   xor  dx, dx
   mov  cx, 7    
   div  cx       ;dx bolmeden kalanlari tutar
   mov siradaki_2nci_cisim_rastgele_sayisi, dl
   ret
endp rastgele_sayi_uretim_basla

rastgele_cisim_uret proc
    push ax
    push bx
    push cx
    push dx
   
   mov bl, siradaki_1nci_cisim_rastgele_sayisi
   mov rastgele_cisim_sayisi, bl
   mov bl, siradaki_2nci_cisim_rastgele_sayisi
   mov siradaki_1nci_cisim_rastgele_sayisi, bl
   MOV AH, 00h  ; sistem zamani icin interrupt        
   INT 1AH      ; gece yarisindan beri saatin      
   mov  ax, dx
   xor  dx, dx
   mov  cx, 7    
   div  cx       ;dx bolmeden kalanlari tutar
   mov siradaki_2nci_cisim_rastgele_sayisi, dl  
   
   cmp rastgele_cisim_sayisi, 0
   je kare_blok_ciz_label
   cmp rastgele_cisim_sayisi, 1
   je dikdortgen_blok_ciz_label
   cmp rastgele_cisim_sayisi, 2
   je draw_L_block_label
   cmp rastgele_cisim_sayisi, 3  
   je draw_T_block_label
   cmp rastgele_cisim_sayisi, 4  
   je draw_Z_block_label
   cmp rastgele_cisim_sayisi, 5
   je draw_S_block_label
   cmp rastgele_cisim_sayisi, 6
   je draw_opposite_L_block_label
   
   mov bh, 1
   
   kare_blok_ciz_label:
   mov cisim_rengi, 0EH
   
   mov cisim_sutunu, 5
   mov cisim_satiri, 1
   
   
   mov aktif_cisim, 51 ;**
                       ;**
   mov bx, aktif_cisim      
   jmp sekli_ciz
   jmp rastgele_cisim_uret_exit
                       
   dikdortgen_blok_ciz_label:
   mov cisim_rengi, 9H        
   
   mov cisim_sutunu, 4
   mov cisim_satiri, 1
   
   mov aktif_cisim, 15 ;****
   mov bx, aktif_cisim
   jmp sekli_ciz
   jmp rastgele_cisim_uret_exit
   
   draw_L_block_label:
   mov cisim_rengi, 2H
   
   mov cisim_sutunu, 5
   mov cisim_satiri, 1  
   
   mov aktif_cisim, 71 ;***
                       ;  *
   mov bx, aktif_cisim
   jmp sekli_ciz
   jmp rastgele_cisim_uret_exit

   draw_T_block_label:
   mov cisim_rengi, 4H
   
   mov cisim_sutunu, 5
   mov cisim_satiri, 1    
   

   mov aktif_cisim, 39 ;***
                       ; *
   mov bx, aktif_cisim
   jmp sekli_ciz
   jmp rastgele_cisim_uret_exit

   draw_Z_block_label:
   mov cisim_rengi, 6H

   mov cisim_sutunu, 5
   mov cisim_satiri, 1
   
   mov aktif_cisim, 99 ;**
                       ; **
   mov bx, aktif_cisim          
   jmp sekli_ciz
   
   draw_S_block_label:
   mov cisim_rengi, 5H
   
   mov cisim_sutunu, 5
   mov cisim_satiri, 1
   
   
   mov aktif_cisim, 54
   
   mov bx, aktif_cisim  
   jmp sekli_ciz
   
   draw_opposite_L_block_label:
   
   mov cisim_rengi, 9H
   
   mov cisim_sutunu, 5
   mov cisim_satiri, 1
   
   mov aktif_cisim, 23
   
   mov bx, aktif_cisim  
   jmp sekli_ciz            
   
   sekli_ciz:
   
   mov bx, aktif_cisim
   mov gidecek_cisim, bx
   
   mov bx, cisim_sutunu
   mov cismin_gittigi_sutun, bx
   
   mov bx, cisim_satiri
   mov cismin_gittigi_satir, bx
   
   call yeri_bos_mu
   cmp yeri_bos_mu_sonuc, 0
   je oyun_sonu_cik
   
   call sekil_ciz
   
   rastgele_cisim_uret_exit:
   
   pop dx
   pop cx
   pop bx
   pop ax
   
   ret            
 
oyun_sonu_cik:
    mov cisim_sinir_rengi, 0H
    call siniri_ciz
    mov ax, 4C00h
    int 21h        
 
ret  
endp rastgele_cisim_uret

sekil_ciz proc
    push bx
    push cx
    push dx
     
    mov bx, aktif_cisim
    mov dx, bx ; *___
    and dx, 1  ; ____ (bit var mi kontrolu - yoksa 2. adima gecer)
    cmp dx, 1  ; ____
    jne adim2  ; ____  
    mov bx, dx                
   
    mov cx, cisim_sutunu
    mov blok_sutunu, cx
    mov dx, cisim_satiri  
    mov blok_satiri, dx
   
    call tek_blok_ciz  ;bit olan yere blok ciz
   
adim2:                
    mov bx, aktif_cisim
    mov dx, bx  ; _*__
    and dx, 2   ; ____
    cmp dx, 2   ; ____
    jne adim3   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    inc cx
    mov blok_sutunu, cx
    mov dx, cisim_satiri  
    mov blok_satiri, dx
   
    call tek_blok_ciz
   
adim3:                
    mov bx, aktif_cisim
    mov dx, bx  ; __*_
    and dx, 4   ; ____
    cmp dx, 4   ; ____
    jne adim4   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu  
    add cx, 2
    mov blok_sutunu, cx
    mov dx, cisim_satiri  
    mov blok_satiri, dx        
             
    call tek_blok_ciz
   
adim4:                
    mov bx, aktif_cisim
    mov dx, bx  ; ___*
    and dx, 8   ; ____
    cmp dx, 8   ; ____
    jne adim5   ; ____
    mov bx, dx
                       
   
    mov cx, cisim_sutunu
    add cx, 3
    mov blok_sutunu, cx
    mov dx, cisim_satiri  
    mov blok_satiri, dx
   
    call tek_blok_ciz
   
                     
adim5:                
    mov bx, aktif_cisim
    mov dx, bx  ; ____
    and dx, 16  ; *___
    cmp dx, 16  ; ____
    jne adim6   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 1
    mov blok_satiri, dx
   
    call tek_blok_ciz
   
adim6:                
    mov bx, aktif_cisim
    mov dx, bx  ; ____    ;**
    and dx, 32  ; _*__    ;** <- cisim satiri
    cmp dx, 32  ; ____
    jne adim7   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    inc cx
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 1  
    mov blok_satiri, dx
   
    call tek_blok_ciz
   
adim7:                
    mov bx, aktif_cisim
    mov dx, bx  ; ____
    and dx, 64  ; __*_
    cmp dx, 64  ; ____
    jne adim8   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    add cx, 2
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 1
    mov blok_satiri, dx
   
    call tek_blok_ciz

adim8:
    mov bx, aktif_cisim
    mov dx, bx  ; ____
    and dx, 128 ; ___*
    cmp dx, 128 ; ____
    jne adim9   ; ____
    mov bx, dx  
   
    mov cx, cisim_sutunu
    add cx, 3
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 1
    mov blok_satiri, dx
   
    call tek_blok_ciz
 
adim9:
    mov bx, aktif_cisim
    mov dx, bx   ; ____
    and dx, 256  ; ____
    cmp dx, 256  ; *___
    jne adim10   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 2  
    mov blok_satiri, dx
   
    call tek_blok_ciz

adim10:
    mov bx, aktif_cisim
    mov dx, bx   ; ____
    and dx, 512  ; ____
    cmp dx, 512  ; _*__
    jne adim11   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    inc cx
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 2
    mov blok_satiri, dx
   
    call tek_blok_ciz

adim11:
    mov bx, aktif_cisim
    mov dx, bx   ; ____
    and dx, 1024 ; ____
    cmp dx, 1024 ; __*_
    jne adim12   ; ____
    mov bx, dx  
   
    mov cx, cisim_sutunu
    inc cx
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 2
    mov blok_satiri, dx
   
    call tek_blok_ciz
                     
adim12:
    mov bx, aktif_cisim
    mov dx, bx   ; ____
    and dx, 2048 ; ____
    cmp dx, 2048 ; ___*
    jne adim13   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    add cx, 3
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 2  
    mov blok_satiri, dx
   
    call tek_blok_ciz
                     
adim13:
    mov bx, aktif_cisim
    mov dx, bx   ; ____
    and dx, 4096 ; ____
    cmp dx, 4096 ; ____
    jne adim14   ; *___
    mov bx, dx
   
    mov cx, cisim_sutunu
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 3  
    mov blok_satiri, dx
   
    call tek_blok_ciz    
   
adim14:
    mov bx, aktif_cisim
    mov dx, bx   ; ____
    and dx, 8192 ; ____
    cmp dx, 8192 ; ____
    jne adim15   ; _*__
    mov bx, dx
   
    mov cx, cisim_sutunu
    inc cx
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 3
    mov blok_satiri, dx
   
   
    call tek_blok_ciz
   
adim15:
    mov bx, aktif_cisim
    mov dx, bx    ; ____
    and dx, 16394 ; ____
    cmp dx, 16384 ; ____
    jne adim16    ; __*_
    mov bx, dx
   
    mov cx, cisim_sutunu
    add cx, 2
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx,3
    mov blok_satiri, dx
   
    call tek_blok_ciz
   
adim16:
    mov bx, aktif_cisim
    mov dx, bx    ; ____
    and dx, 32768 ; ____    (bit kontrolu - bit yoksa cik)
    cmp dx, 32768 ; ____
                  ; ___*
    jne sekil_ciz_exit    
    mov bx, dx
   
    mov cx, cisim_sutunu
    add cx, 3
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 3  
    mov blok_satiri, dx
   
    call tek_blok_ciz
   
sekil_ciz_exit:

    pop dx
    pop cx
    pop bx
    ret
endp sekil_ciz

sekil_sil proc  
    push bx
    push cx
    push dx
   
    mov bx, aktif_cisim
    mov dx, bx ; *___
    and dx, 1  ; ____ (bit var mi kontrolu - yoksa 2. adima gecer)
    cmp dx, 1  ; ____
    jne adim2_sil  ; ____  
    mov bx, dx                
   
    mov cx, cisim_sutunu
    mov blok_sutunu, cx
    mov dx, cisim_satiri  
    mov blok_satiri, dx
   
    call tek_blok_sil  ;bit olan yere blok ciz
   
adim2_sil:                
    mov bx, aktif_cisim
    mov dx, bx  ; _*__
    and dx, 2   ; ____
    cmp dx, 2   ; ____
    jne adim3_sil   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    inc cx
    mov blok_sutunu, cx
    mov dx, cisim_satiri  
    mov blok_satiri, dx
   
    call tek_blok_sil
   
adim3_sil:                
    mov bx, aktif_cisim
    mov dx, bx  ; __*_
    and dx, 4   ; ____
    cmp dx, 4   ; ____
    jne adim4_sil   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu  
    add cx, 2
    mov blok_sutunu, cx
    mov dx, cisim_satiri  
    mov blok_satiri, dx        
             
    call tek_blok_sil
   
adim4_sil:                
    mov bx, aktif_cisim
    mov dx, bx  ; ___*
    and dx, 8   ; ____
    cmp dx, 8   ; ____
    jne adim5_sil   ; ____
    mov bx, dx
                       
   
    mov cx, cisim_sutunu
    add cx, 3
    mov blok_sutunu, cx
    mov dx, cisim_satiri  
    mov blok_satiri, dx
   
    call tek_blok_sil
   
                     
adim5_sil:                
    mov bx, aktif_cisim
    mov dx, bx  ; ____
    and dx, 16  ; *___
    cmp dx, 16  ; ____
    jne adim6_sil   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    inc dx
    mov blok_satiri, dx
   
    call tek_blok_sil
   
adim6_sil:                
    mov bx, aktif_cisim
    mov dx, bx  ; ____
    and dx, 32  ; _*__
    cmp dx, 32  ; ____
    jne adim7_sil   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    inc cx
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    inc dx  
    mov blok_satiri, dx
   
    call tek_blok_sil
   
adim7_sil:                
    mov bx, aktif_cisim
    mov dx, bx  ; ____
    and dx, 64  ; __*_
    cmp dx, 64  ; ____
    jne adim8_sil   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    add cx, 2
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    inc dx
    mov blok_satiri, dx
   
    call tek_blok_sil

adim8_sil:
    mov bx, aktif_cisim
    mov dx, bx  ; ____
    and dx, 128 ; ___*
    cmp dx, 128 ; ____
    jne adim9_sil   ; ____
    mov bx, dx  
   
    mov cx, cisim_sutunu
    add cx, 3
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    inc dx
    mov blok_satiri, dx
   
    call tek_blok_sil
 
adim9_sil:
    mov bx, aktif_cisim
    mov dx, bx   ; ____
    and dx, 256  ; ____
    cmp dx, 256  ; *___
    jne adim10_sil   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 2  
    mov blok_satiri, dx
   
    call tek_blok_sil

adim10_sil:
    mov bx, aktif_cisim
    mov dx, bx   ; ____
    and dx, 512  ; ____
    cmp dx, 512  ; _*__
    jne adim11_sil   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    inc cx
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 2
    mov blok_satiri, dx
   
    call tek_blok_sil

adim11_sil:
    mov bx, aktif_cisim
    mov dx, bx   ; ____
    and dx, 1024 ; ____
    cmp dx, 1024 ; __*_
    jne adim12_sil  ; ____
    mov bx, dx  
   
    mov cx, cisim_sutunu
    inc cx
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 2
    mov blok_satiri, dx
   
    call tek_blok_sil
                     
adim12_sil:
    mov bx, aktif_cisim
    mov dx, bx   ; ____
    and dx, 2048 ; ____
    cmp dx, 2048 ; ___*
    jne adim13_sil   ; ____
    mov bx, dx
   
    mov cx, cisim_sutunu
    add cx, 3
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 2  
    mov blok_satiri, dx
   
    call tek_blok_sil
                     
adim13_sil:
    mov bx, aktif_cisim
    mov dx, bx   ; ____
    and dx, 4096 ; ____
    cmp dx, 4096 ; ____
    jne adim14_sil   ; *___
    mov bx, dx
   
    mov cx, cisim_sutunu
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 3  
    mov blok_satiri, dx
   
    call tek_blok_sil    
   
adim14_sil:
    mov bx, aktif_cisim
    mov dx, bx   ; ____
    and dx, 8192 ; ____
    cmp dx, 8192 ; ____
    jne adim15_sil   ; _*__
    mov bx, dx
   
    mov cx, cisim_sutunu
    inc cx
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 3
    mov blok_satiri, dx
   
   
    call tek_blok_sil
   
adim15_sil:
    mov bx, aktif_cisim
    mov dx, bx    ; ____
    and dx, 16394 ; ____
    cmp dx, 16384 ; ____
    jne adim16_sil    ; __*_
    mov bx, dx
   
    mov cx, cisim_sutunu
    add cx, 2
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx,3
    mov blok_satiri, dx
   
    call tek_blok_sil
   
adim16_sil:
    mov bx, aktif_cisim
    mov dx, bx    ; ____
    and dx, 32768 ; ____    (bit kontrolu - bit yoksa cik)
    cmp dx, 32768 ; ____
                  ; ___*
    jne sekil_sil_exit    
    mov bx, dx
   
    mov cx, cisim_sutunu
    add cx, 3
    mov blok_sutunu, cx
    mov dx, cisim_satiri
    add dx, 3  
    mov blok_satiri, dx
   
    call tek_blok_sil
   
sekil_sil_exit:

    pop dx
    pop cx
    pop bx
    ret
endp sekil_sil
           

sekli_saga_kaydir proc
    push bx
    push dx
     
     mov bx, aktif_cisim
     mov gidecek_cisim, bx
     mov bx, cisim_sutunu
     mov dx, cisim_satiri    
     mov cismin_gittigi_satir, dx
     inc bx
     mov cismin_gittigi_sutun, bx
     
     
     call yeri_bos_mu
     cmp yeri_bos_mu_sonuc, 1
     jne saga_kaydirma_cik
     cmp cisim_sutunu, 10
     je saga_kaydirma_cik
     
     call sekil_sil
     inc cisim_sutunu
     call sekil_ciz
     
     saga_kaydirma_cik:  
     
    pop dx
    pop bx
   
     ret
endp sekli_saga_kaydir

sekli_sola_kaydir proc
    push bx
    push dx
   
     mov bx, aktif_cisim
     mov gidecek_cisim, bx
   
     mov bx, cisim_sutunu
     mov dx, cisim_satiri
     dec bx
     mov cismin_gittigi_satir, dx
     mov cismin_gittigi_sutun, bx
     call yeri_bos_mu
     cmp yeri_bos_mu_sonuc, 1
     jne sola_kaydirma_cik
     cmp cismin_gittigi_sutun, 0
     je sola_kaydirma_cik

     call sekil_sil
     dec cisim_sutunu
     call sekil_ciz
     
     sola_kaydirma_cik:
    pop dx
    pop bx
     ret
endp sekli_sola_kaydir

sekli_asagi_kaydir proc
    push bx
    push cx
    push dx
   
    asagi_kaydirma:  
    mov dx, cisim_satiri
    mov bx, cisim_sutunu
    mov cisim_sutunu, bx
                     
    call sekil_sil
                       
    cmp aktif_cisim, 4095                                                
    jng uc_satir ; aktif_cisim 4095 den buyuk ise 4 satir demektir
    cmp cisim_satiri, 13
    je siradaki_sekil
   
    uc_satir:
    cmp aktif_cisim, 255
    jng iki_satir ; aktif_cisim 256 dan kucuk ise iki satir demektir 15 nci satira gidebilir
    cmp cisim_satiri, 14
    je siradaki_sekil
   
    iki_satir:                                                    
    cmp aktif_cisim, 15
    jng tek_satir ; aktif_cisim 16 dan kucuk ise iki satir demektir 15 nci satira gidebilir
    cmp cisim_satiri, 15
    je siradaki_sekil
 
    tek_satir:
    cmp cisim_satiri, 16
    je siradaki_sekil
   
    mov cx, aktif_cisim
    mov gidecek_cisim, cx

    mov dx, cisim_satiri                  
    mov cismin_gittigi_satir, dx  
    inc cismin_gittigi_satir    
    mov cismin_gittigi_sutun, bx
    mov bx, cismin_gittigi_sutun
   
    call yeri_bos_mu
 
    cmp yeri_bos_mu_sonuc, 0
    je siradaki_sekil

    inc cisim_satiri
    call sekil_ciz
   
   
    pop dx
    pop cx
    pop bx
   
    ret
   
    siradaki_sekil:
    call cismi_ekrana_teslim_et
             
    call siradaki_cismi_uret
         
    pop dx
    pop cx
    pop bx
    ret  
endp sekli_asagi_kaydir

siradaki_cismi_uret proc
    call rastgele_cisim_uret

    ret
endp siradaki_cismi_uret

sekli_dondur proc
    push bx
    push dx
   
    mov bx, aktif_cisim

    cmp bx, 51
    je kare
    cmp bx, 39
    je te
    cmp bx, 562
    je te1
    cmp bx, 114
    je te2
    cmp bx, 305
    je te3
    cmp bx, 23
    je le
    cmp bx, 547
    je le1
    cmp bx, 116
    je le2
    cmp bx, 785
    je le3
    cmp bx, 54
    je se    
    cmp bx, 561
    je se1
    cmp bx, 15
    je cubuk  
    cmp bx, 4369
    je cubuk1
    cmp bx, 71
    je ters_le
    cmp bx, 802
    je ters_le1
    cmp bx,113
    je ters_le2
    cmp bx, 275
    je ters_le3
    cmp bx, 99
    je ze      
    cmp bx, 306
    je ze1
   
    kare:
    mov gidecek_cisim, 51
    jmp rezerv_kontrol
    te:
    mov gidecek_cisim, 562
    jmp rezerv_kontrol
    te1:
    mov gidecek_cisim, 114
    jmp rezerv_kontrol
    te2:
    mov gidecek_cisim, 305
    jmp rezerv_kontrol
    te3:
    mov gidecek_cisim, 39
    jmp rezerv_kontrol
    le:
    mov gidecek_cisim, 547
    jmp rezerv_kontrol
    le1:
    mov gidecek_cisim, 116
    jmp rezerv_kontrol
    le2:
    mov gidecek_cisim, 785
    jmp rezerv_kontrol
    le3:
    mov gidecek_cisim, 23
    jmp rezerv_kontrol
    se:    
    mov gidecek_cisim, 561
    jmp rezerv_kontrol
    se1:
    mov gidecek_cisim, 54
    jmp rezerv_kontrol
    cubuk:  
    mov gidecek_cisim, 4369
    jmp rezerv_kontrol
    cubuk1:
    mov gidecek_cisim, 15
    jmp rezerv_kontrol
    ters_le:
    mov gidecek_cisim, 802
    jmp rezerv_kontrol
    ters_le1:
    mov gidecek_cisim,113
    jmp rezerv_kontrol
    ters_le2:
    mov gidecek_cisim, 275
    jmp rezerv_kontrol
    ters_le3:
    mov gidecek_cisim, 71
    jmp rezerv_kontrol
    ze:      
    mov gidecek_cisim, 306
    jmp rezerv_kontrol
    ze1:
    mov gidecek_cisim, 99
                       
    rezerv_kontrol:
    mov bx, cisim_sutunu
    mov dx, cisim_satiri
    mov cismin_gittigi_sutun, bx
    mov cismin_gittigi_satir, dx
   
    call yeri_bos_mu
   
    cmp yeri_bos_mu_sonuc, 1
    jne dondurmeden_cik
   
    call sekil_sil
    mov bx, gidecek_cisim
    mov aktif_cisim, bx
    call sekil_ciz
   
 dondurmeden_cik:  
 
 pop dx
 pop bx      
 ret  
endp sekli_dondur

siniri_ciz proc
    push bx
    mov bx,  ekran_baslangic_satiri
    mov blok_baslangic_satiri, bx
    add bx, 12
    mov blok_bitis_satiri, bx  
siniri_ciz_loop1:
    mov bx,  ekran_baslangic_sutunu
    mov blok_baslangic_sutunu, bx
    add bx, 12
    mov blok_bitis_sutunu, bx
siniri_ciz_loop2:
    call tek_blok_sinir_ciz
    add blok_baslangic_sutunu, 12
    add blok_bitis_sutunu, 12
    mov bx,  ekran_bitis_sutunu
    cmp bx, blok_baslangic_sutunu
    jnz siniri_ciz_loop2
    add blok_baslangic_satiri, 12
    add blok_bitis_satiri, 12
    mov bx, ekran_bitis_satiri
    cmp bx, blok_baslangic_satiri
    jnz siniri_ciz_loop1
    pop bx
    ret
endp siniri_ciz

tek_blok_sinir_ciz proc
    push ax
    push cx
    push dx
   
    mov ah, 0ch
    mov al, cisim_sinir_rengi
    dec blok_bitis_satiri
    mov dx, blok_baslangic_satiri    
    mov cx, blok_baslangic_sutunu
loop4_bb:
    int 10h
    inc cx
    cmp cx, blok_bitis_sutunu
    jnz loop4_bb
    inc dx
loop3_b:
    mov cx, blok_baslangic_sutunu
loop4_b:
    int 10h
    add cx, 11
    cmp cx, blok_bitis_sutunu
    jb loop4_b
    inc dx
    cmp dx, blok_bitis_satiri
    jnz loop3_b
    mov dx, blok_bitis_satiri    
    mov cx, blok_baslangic_sutunu
loop4_bbb:
    int 10h
    inc cx
    cmp cx, blok_bitis_sutunu
    jnz loop4_bbb
    inc blok_bitis_satiri
           
    pop dx
    pop cx      
    pop ax
    ret  
endp tek_blok_sinir_ciz    

proc skoru_goster 
    push ax          
    push bx
    
    mov ah, 02h
    mov bh, 00h ;cursoru ayarla
    mov dh, 04h
    mov dl, 01h
    int 10h
    mov ah, 09h ;stringi gosteren kisim 
    lea dx, msg_skor
    int 21h
    
    pop ax
    pop bx 
    ret
endp skoru_goster 

proc skoru_guncelle
    push ax
    push bx
    push dx
    push si
    
    xor ax, ax
    mov si, 9 
    mov ax, skor
    mov bx, 10
label:
    cmp si, 5
    je exit_label
    xor dx, dx
    div bx
    add dx, 30h
    mov [msg_skor+si], dl
    dec si
    jmp label
exit_label:
    call skoru_goster
    
    pop ax
    pop bx
    pop dx
    pop si
    ret
endp skoru_guncelle