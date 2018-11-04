.Model Small
draw_row Macro x
    Local l1
; draws a line in row x from col 10 to col 300
    MOV AH, 0CH
    MOV AL, 2
    MOV CX, 1
    MOV DX, x
L1: INT 10h
    INC CX
    CMP CX, 301
    JL L1
    EndM

draw_col Macro y
    Local l2
; draws a line col y from row 10 to row 189
    MOV AH, 0CH
    MOV AL, 2
    MOV CX, y
    MOV DX, 10
L2: INT 10h
    INC DX
    CMP DX, 190
    JL L2
    EndM

.Stack 100h
.Data
 
;FRONT PAGE
WELCOME_MESSAGE db "Welcome to the SHOOTERS GAME!!",0
INSTRUCTION_MESSAGE db 0DH,0AH,"INSTRUCTIONS",0AH,0DH,"YOU HAVE TO SHOOT THE TARGET IN THE BOARD",0AH,0DH,"THE GAME'LL BE OVER AFTER MISSING 5 TARGETS",0DH,0AH,0DH,0AH,"PRESS P FOR PAUSE AND PRESS R FOR RESUME",0DH,0AH,0DH,0AH,"PRESS ANY KEY TO START_$" 

;GAME PAGE   
START_CONDITION_OF_ARROW DB 0
GAME_OVER_MESSAGE DB "HIGHSCORE$"
abar_khela db "press a to play again$"
GAME_OVER_MESSAGE_LENGTH DB 5
MISSED_OF_PLAYER_MESSAGE DB "YOU MISSED:  TARGETS$"
MISSED_OF_PLAYER_LENGTH DB 19;11,12
 SCORE_MESSAGE DB "Score:  AAAA   $"
 SCORE_LENGTH DW ?
 g dw 0
 FILE_SCORE DB ?
 filename db "sco.txt",0
 MISSED_OF_PLAYER_SCORE DB -1
 second_time dw 0
 HANDLE DW ?
 SCORE_BOARD DB 10 dup(0)
 congo db "congratzzz!! new highscore unlocked$"
 STARTER_OF_ARROW DB 0
 MAIN_SCORE_OF_PLAYER DB 0
ARROW_ROW DW 60
RANDOM DW 63
TEN DB 10
GAMEOVER DB 0
HOHO DB 0;1 HOILE BARBE NA
;ARROW_COLUM DW 10
ARROW_COLUM DW 290
new_timer_vec   dw  ?,?
old_timer_vec   dw  ?,?
timer_flag  db  0
VEL_ARROW DW 10
POS DW 0;0 MEANS RESUME,1 MEANS POS
vel_x       dw  1
DIVIDEND_ROW DW 180
vel_y       dw  5
TARGET_POSITION DW ?
.Code

set_display_mode Proc
; sets display mode and draws boundary
    MOV AH, 0
    MOV AL, 04h; 320x200 4 color
    INT 10h
; select palette    
    MOV AH, 0BH
    MOV BH, 1
    MOV BL, 14
    INT 10h
; set bgd color
    MOV BH, 0
    ;MOV BL, 1; cyan
    MOV BL,2
    INT 10h
; draw boundary
    ;draw_row 10
    draw_row 12
    draw_row 189
    draw_col 10  
    draw_col 300
    
     draw_row 14
     draw_row 191
     draw_col 12
     draw_col 302
   
    
    RET
set_display_mode EndP

DISPLAY_ARROW Proc NEAR
; displays ball at col CX and row DX with color given in AL
; input: AL = color of ball
;    CX = col
;    DX = row
PUSH CX
PUSH DX
MOV CX,ARROW_COLUM
MOV DX,ARROW_ROW
    MOV AH, 0CH ; write pixel
    INT 10h
    ;  INC CX      ; pixel on next col
    ;INT 10h
    ;INC DX      ; down 1 row
    ;INT 10h
    ;DEC CX      ; prev col
    ;INT 10h
    PUSH BX
    MOV BX,20
    CHOL:
    INC CX
    INT 10H
    DEC BX
    JNE CHOL
    
    MOV DX,ARROW_ROW
    MOV CX,ARROW_COLUM
    ADD CX,20
    MOV BX,5
    DRAWING_UPPER_ARROW:
    INT 10H
    DEC CX
    DEC DX
    DEC BX
    JNE DRAWING_UPPER_ARROW
      MOV DX,ARROW_ROW
    MOV CX,ARROW_COLUM
    ADD CX,20
    MOV BX,5
    DRAWING_LOWER_ARROW:
    INT 10H
    DEC CX
    INC DX
    DEC BX
    JNE DRAWING_LOWER_ARROW
     POP BX
     POP DX
     POP CX
    
   
   
         ; restore dx
    RET 
    DISPLAY_ARROW EndP
display_ball Proc
; displays ball at col CX and row DX with color given in AL
; input: AL = color of ball
;    CX = col
;    DX = row
    MOV AH, 0CH ; write pixel
    INT 10h
    ;  INC CX      ; pixel on next col
    ;INT 10h
    ;INC DX      ; down 1 row
    ;INT 10h
    ;DEC CX      ; prev col
    ;INT 10h
    PUSH BX
    MOV BX,30
  
    CHOLO:
    INC DX
    INT 10H
    DEC BX
    JNE CHOLO
    SUB DX,30 
    DEC CX
    ADD DX,10
    MOV BX,10
    CHOL_2:
    INC DX
    INT 10H
    DEC BX
    JNE CHOL_2
    SUB DX,20
    ADD CX,1
    POP BX
         ; restore dx
    RET 
    display_ball EndP

timer_tick Proc
    PUSH DS
    PUSH AX
    
    MOV AX, Seg timer_flag
    MOV DS, AX
    MOV timer_flag, 1
  
    
    POP AX
    POP DS
    
    IRET
timer_tick EndP

move_ball Proc
; erase ball at current position and display at new position
; input: CX = col of ball position
;    DX = rwo of ball position
; erase ball
    CMP POS,1
    JE EXI
    MOV AL, 0
    CALL display_ball
    CALL DISPLAY_ARROW
; get new position
  ;  ADD CX, vel_x
    ADD DX, vel_y
    CMP START_CONDITION_OF_ARROW,1
    JE NEXT_
    PUSH AX
    MOV AX,VEL_ARROW
    ADD ARROW_COLUM,AX
     POP AX
; check boundary
   
    CALL CHECK_BOUNDARY_OR_TARGET_FOR_ARROW
NEXT_:
     CALL check_boundary
; wait for 1 timer tick to display ball
test_timer:
    CMP timer_flag, 1
    JNE test_timer
    
    MOV timer_flag, 0
    MOV AL, 3
    CALL display_ball
    CALL DISPLAY_ARROW
 EXI:
    RET 
move_ball EndP
 CHECK_BOUNDARY_OR_TARGET_FOR_ARROW PROC NEAR
 PUSH AX
 PUSH CX
 PUSH DX
 ADD ARROW_COLUM,10
 ;;;DEC CX
 SUB CX,5
 CMP ARROW_COLUM,CX
 JL CHECKING_BOUNDARY_
 ;; JNE CHECKING_BOUNDARY
 ;;;ADD DX,12
 CMP ARROW_ROW,DX
 JL CHECKING_BOUNDARY_
 ;;;ADD DX,4
 ADD DX,30
 CMP ARROW_ROW,DX
 JG CHECKING_BOUNDARY_
 INC MAIN_SCORE_OF_PLAYER
 MOV HOHO,1
 JMP CHECKING_BOUNDARY
 CHECKING_BOUNDARY_:

 ;SCORE UPDATE
 CHECKING_BOUNDARY:
 
 LP2_:    CMP ARROW_COLUM,280
    JG LP3_
    SUB ARROW_COLUM,10
    JMP DONE_
    LP3_:   
    MOV ARROW_COLUM,10
   ; PUSH AX
    
    PUSH AX
   PUSH DX
   CHANGING_ROW:
    MOV AX,RANDOM
   MOV DX,0
   ADD RANDOM,AX
   MOV AX,RANDOM
   DIV DIVIDEND_ROW
   MOV RANDOM,DX
   MOV AX,10
   CMP RANDOM,AX
   JL CHANGING_ROW
   MOV AX,RANDOM
   MOV ARROW_ROW,AX
   POP DX
   POP AX
  CMP HOHO,1;;1 HOILE BARBE NA
  JE RRR
    INC MISSED_OF_PLAYER_SCORE
    PUSH BX
    MOV BL,5
    CMP MISSED_OF_PLAYER_SCORE,BL
    JL RRR2
 
    MOV gameover,1
     MOV HOHO,0
     POP BX
    ;DEC MAIN_SCORE_OF_PLAYER
    MOV START_CONDITION_OF_ARROW,1
    JMP DONE_
    RRR2:
    POP BX
    RRR:
    MOV HOHO,0
    ;POP BX
    ;DEC MAIN_SCORE_OF_PLAYER
    MOV START_CONDITION_OF_ARROW,1
    JMP DONE_
    
    
DONE_:
    
    POP DX
    POP CX
    POP AX
   
    RET 
  CHECK_BOUNDARY_OR_TARGET_FOR_ARROW ENDP
check_boundary Proc

LP2:    CMP DX, 14
    JG LP3
    MOV DX, 14
    NEG vel_y
    JMP done
    LP3:    CMP DX, 150
    JL done
    MOV DX, 150
    NEG vel_y
done:
    RET 
check_boundary EndP

setup_int Proc

    MOV AH, 35h ; get vector
    INT 21h
    MOV [DI], BX    ; save offset
    MOV [DI+2], ES  ; save segment
; setup new vector
    MOV DX, [SI]    ; dx has offset
    PUSH DS     ; save ds
    MOV DS, [SI+2]  ; ds has the segment number
    MOV AH, 25h ; set vector
    INT 21h
    POP DS
    RET
setup_int EndP

main Proc

GO:
mov g,0
 mov ax, @data
    mov ds, ax 
    
    mov ax, 0b800H
    mov es, ax
    JMP OO
    yy:  
    JMP GO
    OO:
    
    ;clearing the screen
    mov ax, 0003H
    int 10H
    lea BX, WELCOME_MESSAGE
    mov dx,00
    call writestringat
    
     lea dx, INSTRUCTION_MESSAGE
    mov ah, 09H
    int 21h
    
    mov ah, 07h
    int 21h
    mov ax, 0003H
    int 10H
    
    
    MOV AX, @data
    MOV DS, AX
    
; set graphics display mode & draw border
    CALL set_display_mode
    cmp second_time,1
    je uu
; set up timer interrupt vector
    MOV new_timer_vec, offset timer_tick
    MOV new_timer_vec+2, CS
    MOV AL, 1CH; interrupt type
    LEA DI, old_timer_vec
    LEA SI, new_timer_vec
    CALL setup_int
    uu:
; start ball at col = 298, row = 100
; for the rest of the program CX = ball row, DX = ball col
    MOV CX, 290
    MOV DX, 10
    MOV AL, 3
    CALL display_ball
    CALL DISPLAY_ARROW
    jmp tt
    yyy:
    jmp yy
; wait for timer tick before moving the ball
tt:
    CMP timer_flag, 1
    JNE tt
    MOV timer_flag, 0
     PUSH AX
    MOV AL,1
    CMP GAMEOVER,AL
    JE QUIT
    POP AX
    CALL move_ball
    CALL PRINT_SCORE
    CALL PRINT_MISSED_OF_PLAYER_SCORE
   
    
    CMP START_CONDITION_OF_ARROW,1
    JNE tt2
    PUSH DX
    PUSH AX
    CALL CHECK_FOR_KEYBOARD
    POP AX
    POP DX
  
tt2:
    CMP timer_flag, 1
    JNE tt2
    MOV timer_flag, 0
    JMP tt

;POP AX 
  QUIT:
  POP AX
    ; displays ball at col CX and row DX with color given in AL
; input: AL = color of ball
;    CX = col
;    DX = row

MOV AL,5

    MOV AH, 0CH ; write pixel
   INT 10h
   
    MOV CX,0
    MOV DX,0
    MOV BX,65000
    KOOP:
    INT 10H
    INC CX
    CMP CX,BX
    JNE KOOP
    ; CALL WRITE_GAMEOVER_MESSAGE
    call write_a
    mov g,1
    CALL PRINT_SCORE
    call score_read_
    ;CALL PRINT_FILE_SCORE
    push ax
    mov al,MAIN_SCORE_OF_PLAYER
    cmp file_score,al
    jl kor
    call WRITE_GAMEOVER_MESSAGE
    call print_file_score
    jmp lasts
    kor:
    call congratulation_message
    
    lasts:
    pop ax
    ;call highscore_tester
    MOV AH,0
    INT 16H
    
    CMP AL,'a'
    jne yyyy
   mov second_time,1
   mov gameover,0
   mov MAIN_SCORE_OF_PLAYER,0
   mov  MISSED_OF_PLAYER_SCORE,-1
    jmp yyy
yyyy:
main EndP

;dl contains the ascii character if keypressed, else dl contains 0
;uses dx and ax, preserves other registers
READCHAR proc
    mov ah, 01H
    int 16H
    jnz KEYBDPRESSED
    xor dl, dl
    ret
    KEYBDPRESSED:
    ;extract the keystroke from the buffer
    mov ah, 00H
    int 16H
    mov dl,al
    ret


    READCHAR endp     
CHECK_FOR_KEYBOARD PROC NEAR
  CALL READCHAR
  CMP DL, 0
  JE NOT_PRESSED_ANY_KEY
    
    ;so a key was pressed, which key was pressed then solti?
    cmp DL,'a'
    JNE NOT_PRESSED_ANY_KEY
    ;MOV STARTER_OF_ARROW,1
    MOV START_CONDITION_OF_ARROW,0
    RET
NOT_PRESSED_ANY_KEY:
    CMP DL,'p'
    JE SET_POS
    CMP DL,'r'
    JE UNSET_POS
    JMP LLL
    SET_POS:
    MOV POS,1
    JMP LLL
    UNSET_POS:
    MOV POS,0
 LLL:
    RET
    CHECK_FOR_KEYBOARD ENDP
    
    
    ;dx contains row, col
;bx contains the offset of the string
writestringat proc
    push dx
    mov ax, dx
    and ax, 0FF00H
    shr ax,8
    push bx
    mov bh, 160
    mul bh
    
    pop bx
    and dx, 0FFH
    shl dx,1
    add ax, dx
    mov di, ax
loop_writestringat:
    
    mov al, [bx]
    test al, al
    jz exit_writestringat
    mov es:[di], al
    inc di
    inc di
    inc bx
    jmp loop_writestringat
    
    
exit_writestringat:
    pop dx
    ret
    
    
writestringat endp
PRINT_SCORE PROC NEAR
      PUSH AX
   PUSH BX
   PUSH DX
   PUSH CX
   cmp g,1
   je ssss
   MOV DL,30
   jmp exec
   ssss:
   mov dl,0
   exec:
   LEA SI,SCORE_MESSAGE
   DEC SI
CLD
MOV SCORE_LENGTH,8

CMP MAIN_SCORE_OF_PLAYER,0
JL NEGATIVE_SIGN
PUSH AX
MOV AH,0
MOV AL,MAIN_SCORE_OF_PLAYER
DIV TEN
ADD AL,30H
MOV [SI+7],AL
ADD AH,30H
MOV [SI+8],AH
POP AX
JMP SCORE_PRINT_LOOP
NEGATIVE_SIGN:
;NEG MAIN_SCORE_OF_PLAYER
PUSH BX
MOV BL,MAIN_SCORE_OF_PLAYER
MOV BH,0
NEG BL
ADD BL,30H
MOV [SI+8],BL
MOV [SI+7],'-'
POP BX
;NEG MAIN_SCORE_OF_PLAYER
SCORE_PRINT_LOOP:
INC SI
    MOV AH,02 ;set cursor
MOV BH,0 ;page 0
MOV DH,0 ;row 0

INT 10H
MOV AH,9 ;write char function


;INC SI
MOV  AL,[SI]
MOV  BL,2

MOV CX,1
INT 10H
INC DL
DEC SCORE_LENGTH
JNE SCORE_PRINT_LOOP
POP CX
POP DX
POP BX
POP AX
     RET
 PRINT_SCORE ENDP 
 PRINT_MISSED_OF_PLAYER_SCORE PROC NEAR



      PUSH AX
   PUSH BX
   PUSH DX
   PUSH CX
   MOV DL,0
   LEA SI,MISSED_OF_PLAYER_MESSAGE
   DEC SI
   MOV MISSED_OF_PLAYER_LENGTH,19
CLD
PUSH AX
MOV AH,0
MOV AL,MISSED_OF_PLAYER_SCORE
CMP AL,-1
JNE RR
MOV AL,0
RR:
DIV TEN
ADD AL,30H
MOV [SI+12],AL
ADD AH,30H
MOV [SI+13],AH
POP AX
SCORE_PRINT_LOOP_:
INC SI
    MOV AH,02 ;set cursor
MOV BH,0 ;page 0
MOV DH,0 ;row 0

INT 10H
MOV AH,9 ;write char function


;INC SI
MOV  AL,[SI]
MOV  BL,2

MOV CX,1
INT 10H
INC DL
DEC MISSED_OF_PLAYER_LENGTH
JNE SCORE_PRINT_LOOP_
POP CX
POP DX
POP BX
POP AX
     RET
     PRINT_MISSED_OF_PLAYER_SCORE ENDP 
    WRITE_GAMEOVER_MESSAGE PROC NEAR
      PUSH AX
   PUSH BX
   PUSH DX
   PUSH CX
   MOV DL,0
   LEA SI,GAME_OVER_MESSAGE
   DEC SI
   MOV GAME_OVER_MESSAGE_LENGTH,9
CLD

SCORE__PRINT_LOOP_:
INC SI
    MOV AH,02 ;set cursor
MOV BH,0 ;page 0
MOV DH,4 ;row 0

INT 10H
MOV AH,9 ;write char function


;INC SI
MOV  AL,[SI]
MOV  BL,2

MOV CX,1
INT 10H
INC DL
DEC GAME_OVER_MESSAGE_LENGTH
JNE SCORE__PRINT_LOOP_
POP CX
POP DX
POP BX
POP AX
     RET
     WRITE_GAMEOVER_MESSAGE ENDP 
       write_a PROC NEAR
      PUSH AX
   PUSH BX
   PUSH DX
   PUSH CX
   MOV DL,0
   LEA SI,abar_khela
   DEC SI
   MOV GAME_OVER_MESSAGE_LENGTH,21
CLD

SCORE__PRINT_LOOP___:
INC SI
    MOV AH,02 ;set cursor
MOV BH,0 ;page 0
MOV DH,10 ;row 0

INT 10H
MOV AH,9 ;write char function


;INC SI
MOV  AL,[SI]
MOV  BL,2

MOV CX,1
INT 10H
INC DL
DEC GAME_OVER_MESSAGE_LENGTH
JNE SCORE__PRINT_LOOP___
POP CX
POP DX
POP BX
POP AX
     RET
     write_a ENDP 
     score_read_ proc near
     PUSH AX
     PUSH DX
     PUSH CX
     PUSH BX
     mov ah,3DH
     MOV AL,0
     LEA DX,FILENAME
     INT 21H
     jc fuj
     MOV HANDLE,AX
     MOV AH,3FH
     MOV CX,512
     mov bx,handle
    
     LEA DX,SCORE_BOARD
     INT 21H
     MOV BX,HANDLE
     MOV AH,3EH
     INT 21H
     LEA SI,SCORE_BOARD
     MOV AL,BYTE PTR[SI]
     SUB AL,48
     MOV BL,10
     MUL BL
     INC SI
     MOV BL,BYTE PTR[SI]
     SUB BL,48
     ADD AL,BL
     
     MOV FILE_SCORE,AL
    jmp ao
    fuj:
    mov ah,4ch
    int 21h
    ao:
     POP BX
     POP CX
     POP DX
     POP AX
     RET
     
    score_read_ endp   
  PRINT_FILE_SCORE PROC NEAR
      PUSH AX
   PUSH BX
    PUSH DX
   PUSH CX
   MOV DL,0
   LEA SI,SCORE_BOARD
   DEC SI
   MOV GAME_OVER_MESSAGE_LENGTH,2
  CLD

  SC_ORE__PRINT_LOOP_:
  INC SI
    MOV AH,02 ;set cursor
  MOV BH,0 ;page 0
  MOV DH,5 ;row 0

  INT 10H
  MOV AH,9 ;write char function



  MOV  AL,[SI]
  MOV  BL,2

  MOV CX,1
  INT 10H
  INC DL
  DEC GAME_OVER_MESSAGE_LENGTH
  JNE SC_ORE__PRINT_LOOP_
  POP CX
  POP DX
  POP BX
  POP AX
    RET
    PRINT_FILE_SCORE ENDP 
     congratulation_message PROC NEAR
      PUSH AX
   PUSH BX
    PUSH DX
   PUSH CX
   MOV DL,0
   LEA SI,congo
   DEC SI
   MOV GAME_OVER_MESSAGE_LENGTH,35
  CLD

  S_C_ORE__PRINT_LOOP_:
  INC SI
    MOV AH,02 ;set cursor
  MOV BH,0 ;page 0
  MOV DH,15 ;row 0

  INT 10H
  MOV AH,9 ;write char function



  MOV  AL,[SI]
  MOV  BL,2

  MOV CX,1
  INT 10H
  INC DL
  DEC GAME_OVER_MESSAGE_LENGTH
  JNE S_C_ORE__PRINT_LOOP_
  lea si,score_board
  mov bl,10
  mov al,MAIN_SCORE_OF_PLAYER
  mov ah,0
  div bl
  add al,48
  add ah,48
  mov byte ptr[si],al
  mov byte ptr[si+1],ah
  
  
  
  mov ah,3dh
  mov al,1
  lea dx,filename
  int 21h
  
  mov bx,ax
  mov ah,40h
  mov cx,2
  lea dx,score_board
  int 21h
  mov bx,handle
  mov ah,3eh
  int 21h
  
  POP CX
  POP DX
  POP BX
  POP AX
    RET
    congratulation_message ENDP 
    END MAIN
